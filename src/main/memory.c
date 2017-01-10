/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011  The R Core Team.
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */


/* Memory management for pqR, using the SGGC (Segmented Generational
   Garbage Collector) module written by Radford M. Neal, found in 
   src/extra/sggc. */


#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/RS.h> /* for S4 allocation */

#define USE_FAST_PROTECT_MACROS   /* MUST use them in this module! */
#define USE_FAST_PROTECT_MACROS_DISABLED  /* ... even if disabled! */

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Print.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>

#include <helpers/helpers-app.h>
#include <sggc/sggc-app.h>


#undef NOT_LVALUE          /* Allow CAR, etc. on left of assignment here, */
#define NOT_LVALUE(x) (x)  /* since it's needed to implement SETCAR, etc. */


/* CONFIGURATION OPTIONS.  

   Any valid settings for the options below should work, with different effects
   on performance.  However, some combinations may not have been tested 
   recently (or at all). */

#define STRHASHINITSIZE (1<<16) /* Initial number of slots in string hash table
                                   (must be a power of two) */

#define STRHASHMAXSIZE (1<<21)  /* Maximum slots in the string hash table */

#define ENABLE_SHARED_CONSTANTS 1  /* Normally 1, to enable use of shared
                                      constants 0.0, 0L, etc. But doesn't affect
                                      sharing of logicals FALSE, TRUE, and NA,
                                      which is done in Rinlinedfuns.h */

/* DEBUGGING OPTIONS.

   The 'testvalgrind' function invoked with .Internal is always present.

   Options set externally:

   VALGRIND_LEVEL  

       Set by --with-valgrind-instrumentation=n configure option, where
       n (default 0) controls VALGRIND instrumentation.  Currently, any
       non-zero value enables all the extra instrumentation.

   NVALGRIND

       It may be necessary to define NVALGRIND for a non-gcc
       compiler on a supported architecture if it has different
       syntax for inline assembly language from gcc.

   PROTECTCHECK / TESTING_WRITE_BARRIER

       If defined, tries to detect unprotected SEXPs.  See below.

   Other debug options are set by the definitions below. */

#define DEBUG_GLOBAL_STRING_HASH 0

#define DEBUG_SHOW_CHARSXP_CACHE 0


/* VALGRIND declarations.

   For Win32, Valgrind is useful only if running under Wine. */

#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif

#ifndef NVALGRIND
# include "memcheck.h"
#endif

#ifndef VALGRIND_LEVEL
#define VALGRIND_LEVEL 0
#endif

#if defined(Win32) && defined(LEA_MALLOC)
/*#include <stddef.h> */
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif


/* Miscellaneous declarations for garbage collector. */

static void R_gc_internal(void);       /* The main GC procedure */

static SEXP R_PreciousList;            /* List of Persistent Objects */
static SEXP R_StringHash;              /* Global hash of CHARSXPs */

extern SEXP framenames;                /* in model.c */


/* Variables controlling when garbage collections are done. */

#define GC_INTERVAL 1000000

static int gc_countdown = GC_INTERVAL; /* Countdown to when to do next GC */
static long long int gc_count = 0;     /* Number of garbage collections done */
static int gc_last_level = 0;          /* Level of most recently done GC */
static int gc_next_level = 0;          /* Level currently planned for next GC */
static int gc_ran_finalizers;          /* Whether finalizers ran in last GC */
static int gc_reporting = 0;           /* Should message be printed on GC? */


/* Declarations relating to GC torture

   **** if the user specified a wait before starting to force
   **** collecitons it might make sense to also wait before starting
   **** to inhibit releases */

static int gc_force_wait = 0;
static int gc_force_gap = 0;
static Rboolean gc_inhibit_release = FALSE;
#define FORCE_GC (gc_force_wait > 0 ? \
  (--gc_force_wait > 0 ? 0 : (gc_force_wait = gc_force_gap, 1)) : 0)

#define GC_PROT(X) do { \
    int __wait__ = gc_force_wait; \
    int __gap__ = gc_force_gap;			   \
    Rboolean __release__ = gc_inhibit_release;	   \
    X;						   \
    gc_force_wait = __wait__;			   \
    gc_force_gap = __gap__;			   \
    gc_inhibit_release = __release__;		   \
}  while(0)


/* Declarations relating to Rprofmem */

static int R_IsMemReporting;
static int R_MemReportingToTerminal;
static int R_MemPagesReporting;
static int R_MemStackReporting;
static int R_MemDetailsReporting;
static FILE *R_MemReportingOutfile;
static R_size_t R_MemReportingThreshold;
static R_len_t R_MemReportingNElem;
static void R_ReportAllocation (R_size_t, SEXPTYPE, R_len_t);
static void R_ReportNewPage();


R_size_t attribute_hidden R_GetMaxVSize(void)
{
    return R_SIZE_T_MAX;
}

void attribute_hidden R_SetMaxVSize(R_size_t size)
{
}

R_size_t attribute_hidden R_GetMaxNSize(void)
{
    return R_SIZE_T_MAX;
}

void attribute_hidden R_SetMaxNSize(R_size_t size)
{
}

void R_SetPPSize(R_size_t size)
{
    R_PPStackSize = size;
}

static void mem_err_heap(R_size_t size)
{
    errorcall(R_NilValue, _("vector memory exhausted (limit reached?)"));
}

static void mem_err_cons(void)
{
    errorcall(R_NilValue, _("cons memory exhausted (limit reached?)"));
}

static void mem_err_malloc(R_size_t size)
{
    errorcall(R_NilValue, _("memory exhausted (limit reached?)"));
}


/* compute size in VEC units so result will fit in LENGTH field for FREESXPs */
static R_INLINE R_size_t getVecSizeInVEC(SEXP s)
{
    return ((int64_t) sggc_nchunks(TYPEOF(s),LENGTH(s)) * SGGC_CHUNK_SIZE
             - sizeof(SEXPREC_ALIGN)) / sizeof(VECREC);
}


#define CHECK_OLD_TO_NEW(x,y) \
    sggc_old_to_new_check(COMPRESSED_PTR(x),COMPRESSED_PTR(y))


/* Finalization and Weak References */

/* The design of this mechanism is very close to the one described in
   "Stretching the storage manager: weak pointers and stable names in
   Haskell" by Peyton Jones, Marlow, and Elliott (at
   www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz). --LT */

static SEXP R_weak_refs = R_NilValue;

#define READY_TO_FINALIZE_MASK 1

#define SET_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp |= READY_TO_FINALIZE_MASK)
#define CLEAR_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp &= ~READY_TO_FINALIZE_MASK)
#define IS_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp & READY_TO_FINALIZE_MASK)

#define FINALIZE_ON_EXIT_MASK 2

#define SET_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp |= FINALIZE_ON_EXIT_MASK)
#define CLEAR_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp &= ~FINALIZE_ON_EXIT_MASK)
#define FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp & FINALIZE_ON_EXIT_MASK)

#define WEAKREF_SIZE 4
#define WEAKREF_KEY(w) VECTOR_ELT(w, 0)
#define SET_WEAKREF_KEY(w, k) SET_VECTOR_ELT(w, 0, k)
#define WEAKREF_VALUE(w) VECTOR_ELT(w, 1)
#define SET_WEAKREF_VALUE(w, v) SET_VECTOR_ELT(w, 1, v)
#define WEAKREF_FINALIZER(w) VECTOR_ELT(w, 2)
#define SET_WEAKREF_FINALIZER(w, f) SET_VECTOR_ELT(w, 2, f)
#define WEAKREF_NEXT(w) VECTOR_ELT(w, 3)
#define SET_WEAKREF_NEXT(w, n) SET_VECTOR_ELT(w, 3, n)

static SEXP MakeCFinalizer(R_CFinalizer_t cfun);

static SEXP NewWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    SEXP w;

    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
	break;
    default: error(_("can only weakly reference/finalize reference objects"));
    }

    PROTECT2 (key, fin);
    PROTECT (val = NAMEDCNT_GT_0(val) ? duplicate(val) : val);

    w = allocVector(VECSXP, WEAKREF_SIZE);
    SET_TYPEOF(w, WEAKREFSXP);
    if (key != R_NilValue) {
	/* If the key is R_NilValue we don't register the weak reference.
	   This is used in loading saved images. */
	SET_WEAKREF_KEY(w, key);
	SET_WEAKREF_VALUE(w, val);
	SET_WEAKREF_FINALIZER(w, fin);
	SET_WEAKREF_NEXT(w, R_weak_refs);
	CLEAR_READY_TO_FINALIZE(w);
	if (onexit)
	    SET_FINALIZE_ON_EXIT(w);
	else
	    CLEAR_FINALIZE_ON_EXIT(w);
	R_weak_refs = w;
    }
    UNPROTECT(3);
    return w;
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    switch (TYPEOF(fin)) {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	break;
    default: error(_("finalizer must be a function or NULL"));
    }
    return NewWeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    SEXP w;
    PROTECT2 (key, val);
    w = NewWeakRef(key, val, MakeCFinalizer(fin), onexit);
    UNPROTECT(2);
    return w;
}

static void CheckFinalizers(void)
{
    SEXP s;
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s))
	if (sggc_not_marked(COMPRESSED_PTR(WEAKREF_KEY(s))) 
             && ! IS_READY_TO_FINALIZE(s))
	    SET_READY_TO_FINALIZE(s);
}

/* C finalizers are stored in a CHARSXP.  It would be nice if we could
   use EXTPTRSXP's but these only hold a void *, and function pointers
   are not guaranteed to be compatible with a void *.  There should be
   a cleaner way of doing this, but this will do for now. --LT */
/* Changed to RAWSXP in 2.8.0 */
static Rboolean isCFinalizer(SEXP fun)
{
    return TYPEOF(fun) == RAWSXP;
    /*return TYPEOF(fun) == EXTPTRSXP;*/
}

static SEXP MakeCFinalizer(R_CFinalizer_t cfun)
{
    SEXP s = allocVector(RAWSXP, sizeof(R_CFinalizer_t));
    *((R_CFinalizer_t *) RAW(s)) = cfun;
    return s;
    /*return R_MakeExternalPtr((void *) cfun, R_NilValue, R_NilValue);*/
}

static R_CFinalizer_t GetCFinalizer(SEXP fun)
{
    return *((R_CFinalizer_t *) RAW(fun));
    /*return (R_CFinalizer_t) R_ExternalPtrAddr(fun);*/
}

SEXP R_WeakRefKey(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    return WEAKREF_KEY(w);
}

SEXP R_WeakRefValue(SEXP w)
{
    SEXP v;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    v = WEAKREF_VALUE(w);
    if (v!=R_NilValue) 
        SET_NAMEDCNT_MAX(v);
    return v;
}

void R_RunWeakRefFinalizer(SEXP w)
{
    SEXP key, fun, e;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    key = WEAKREF_KEY(w);
    fun = WEAKREF_FINALIZER(w);
    SET_WEAKREF_KEY(w, R_NilValue);
    SET_WEAKREF_VALUE(w, R_NilValue);
    SET_WEAKREF_FINALIZER(w, R_NilValue);
    if (! IS_READY_TO_FINALIZE(w))
	SET_READY_TO_FINALIZE(w); /* insures removal from list on next gc */
    PROTECT2 (key, fun);
    if (isCFinalizer(fun)) {
	/* Must be a C finalizer. */
	R_CFinalizer_t cfun = GetCFinalizer(fun);
	cfun(key);
    }
    else if (fun != R_NilValue) {
	/* An R finalizer. */
	PROTECT(e = LCONS(fun, CONS(key, R_NilValue)));
	eval(e, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(2);
}

static Rboolean RunFinalizers(void)
{
    volatile SEXP s, last;
    volatile Rboolean finalizer_run = FALSE;

    for (s = R_weak_refs, last = R_NilValue; s != R_NilValue;) {
	SEXP next = WEAKREF_NEXT(s);
	if (IS_READY_TO_FINALIZE(s)) {
	    RCNTXT thiscontext;
	    RCNTXT * volatile saveToplevelContext;
	    volatile int savestack;
	    volatile SEXP topExp;

	    finalizer_run = TRUE;

	    /* A top level context is established for the finalizer to
	       insure that any errors that might occur do not spill
	       into the call that triggered the collection. */
	    begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    saveToplevelContext = R_ToplevelContext;
	    PROTECT(topExp = R_CurrentExpr);
	    savestack = R_PPStackTop;
	    if (! SETJMP(thiscontext.cjmpbuf)) {
		R_GlobalContext = R_ToplevelContext = &thiscontext;

		/* The entry in the weak reference list is removed
		   before running the finalizer.  This insures that a
		   finalizer is run only once, even if running it
		   raises an error. */
		if (last == R_NilValue)
		    R_weak_refs = next;
		else
		    SET_WEAKREF_NEXT(last, next);
		/* The value of 'next' is protected to make is safe
		   for thsis routine to be called recursively from a
		   gc triggered by a finalizer. */
		PROTECT(next);
		R_RunWeakRefFinalizer(s);
		UNPROTECT(1);
	    }
	    endcontext(&thiscontext);
	    R_ToplevelContext = saveToplevelContext;
	    R_PPStackTop = savestack;
	    R_CurrentExpr = topExp;
	    UNPROTECT(1);
	}
	else last = s;
	s = next;
    }
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    SEXP s;

    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s))
	if (FINALIZE_ON_EXIT(s))
	    SET_READY_TO_FINALIZE(s);
    RunFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

/* R interface function */

static SEXP do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	error(_("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	error(_("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if(onexit == NA_LOGICAL)
	error(_("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), onexit);
    return R_NilValue;
}


/* THE GENERATIONAL GARBAGE COLLECTOR. */

#ifndef sggc_find_object_ptrs
#include <sggc/sggc-find-ptrs.c>
#endif

#ifndef LOOK_AT
#define LOOK_AT(x) ((x) ? sggc_look_at(COMPRESSED_PTR(x)) : 1)
#endif

#define NOT_MARKED(x) sggc_not_marked(COMPRESSED_PTR(x))


void sggc_find_root_ptrs (void)
{
    int i;

    /* Start by scanning the symbol table.  We have to scan the symbol
       table specially, because it's linked by NEXTSYM_PTR, which
       sggc_find_object_ptrs doesn't know about, and because we need to 
       clear LASTSYMENV and LASTSYMENVNOTFOUND. */
 
    if (R_SymbolTable != NULL) { /* Symbol table nonexistent at startup */
        for (i = 0; i < HSIZE; i++) {
            for (SEXP s = R_SymbolTable[i]; s!=R_NilValue; s = NEXTSYM_PTR(s)) {
                LASTSYMENV(s) = R_NoObject;
                LASTSYMBINDING(s) = R_NoObject; /* not needed; just in case...*/
                LASTSYMENVNOTFOUND(s) = R_NoObject;
                LOOK_AT(s);
            }
        }
    }

    /* Clear fields in the few symbols not in the symbol table, though
       they shouldn't ever be set anyway.  Note that R_UnboundValue is
       a constant now, so its fields aren't going to change (if actually
       kept in read-only memory). */

    if (R_MissingArg) {
        LASTSYMENV(R_MissingArg) = R_NoObject;
        LASTSYMBINDING(R_MissingArg) = R_NoObject;
        LASTSYMENVNOTFOUND(R_MissingArg) = R_NoObject;
    }
    if (R_MissingUnder) {
        LASTSYMENV(R_MissingUnder) = R_NoObject;
        LASTSYMBINDING(R_MissingUnder) = R_NoObject;
        LASTSYMENVNOTFOUND(R_MissingUnder) = R_NoObject;
    }
    if (R_RestartToken) {
        LASTSYMENV(R_RestartToken) = R_NoObject;
        LASTSYMBINDING(R_RestartToken) = R_NoObject;
        LASTSYMENVNOTFOUND(R_RestartToken) = R_NoObject;
    }

    /* Forward other roots. */

    static SEXP *root_vars[] = { 
        &NA_STRING,	          /* Builtin constants */
        &R_BlankString,
	&R_BlankScalarString,
        &R_RestartToken,
        &R_MissingArg,
	&R_MissingUnder,

        &R_print.na_string,       /* Printing defaults - very kludgy! */
        &R_print.na_string_noquote,

        &R_GlobalEnv,	          /* Global environment */
        &R_BaseEnv,
        &R_Warnings,	          /* Warnings, if any */

        &R_HandlerStack,          /* Condition handler stack */
        &R_RestartStack,          /* Available restarts stack */
        &R_Srcref,                /* Current source reference */

        &R_PreciousList,
        0
    };

    for (i = 0; root_vars[i] != 0; i++)
        LOOK_AT(*root_vars[i]);

    if (R_VStack != R_NoObject)
        LOOK_AT(R_VStack);

    if (R_CurrentExpr != R_NoObject)	           /* Current expression */
	LOOK_AT(R_CurrentExpr);

    for (i = 0; i < R_MaxDevices; i++) {   /* Device display lists */
	pGEDevDesc gdd = GEgetDevice(i);
	if (gdd) {
	    if (gdd->displayList != R_NoObject)
                LOOK_AT(gdd->displayList);
	    if (gdd->savedSnapshot != R_NoObject)
                LOOK_AT(gdd->savedSnapshot);
	    if (gdd->dev != NULL && gdd->dev->eventEnv != R_NoObject)
	    	LOOK_AT(gdd->dev->eventEnv);
	}
    }

    RCNTXT *ctxt;
    for (ctxt = R_GlobalContext; ctxt != NULL; ctxt = ctxt->nextcontext) {
        SEXP *cntxt_ptrs[] = { /* using this run-time initialized table may be
                                  slower, but is certainly more compact */
	    &ctxt->conexit,       /* on.exit expressions */
	    &ctxt->promargs,	  /* promises supplied to closure */
	    &ctxt->callfun,       /* the closure called */
	    &ctxt->sysparent,     /* calling environment */
	    &ctxt->call,          /* the call */
	    &ctxt->cloenv,        /* the closure environment */
	    &ctxt->handlerstack,  /* the condition handler stack */
	    &ctxt->restartstack,  /* the available restarts stack */
	    &ctxt->srcref,	  /* the current source reference */
            0
        };
        for (i = 0; cntxt_ptrs[i] != 0; i++)
            LOOK_AT(*cntxt_ptrs[i]);
    }

    if (framenames != R_NoObject)	   /* used for interprocedure    */
        LOOK_AT(framenames);	   /*   communication in model.c */

    for (i = 0; i < R_PPStackTop; i++) {   /* Protected pointers */
        if (R_PPStack[i] != R_NoObject) 
            LOOK_AT(R_PPStack[i]);
    }

    /* Pointers from protected local SEXP variables. */

    for (const struct R_local_protect *p = R_local_protect_start;
           p != NULL; p = p->next) {
        for (i = 0; i < p->cnt; i++) 
            if (*p->Protected[i]) LOOK_AT(*p->Protected[i]);
    }

    /* Byte code stack */

    for (SEXP *sp = R_BCNodeStackBase; sp<R_BCNodeStackTop; sp++)
        LOOK_AT(*sp);
}

void sggc_after_marking (int level, int rep)
{
    int any;
    SEXP s; 
    int i;

    /* LOOK AT TASKS, THE FIRST TIME. */

    if (rep == 0) {

        /* Wait for all tasks whose output variable is no longer referenced
           (ie, not marked above) and is not in use by another task, to ensure
           they don't stay around for a long time.  (Such unreferenced outputs
           should rarely arise in real programs.) */
    
        for (SEXP *var_list = helpers_var_list(1); *var_list; var_list++) {
            SEXP v = *var_list;
            if (NOT_MARKED(v) && !helpers_is_in_use(v))
                helpers_wait_until_not_being_computed(v);
        }
    
        /* For a full collection, wait for tasks that have large variables
           as inputs or outputs that haven't already been marked above, so
           that we can then collect these variables. */
    
        if (level == 2) {
            for (SEXP *var_list = helpers_var_list(0); *var_list; var_list++) {
                SEXP v = *var_list;
                if (NOT_MARKED(v)) {
                    if (helpers_is_being_computed(v))
                        helpers_wait_until_not_being_computed(v);
                    if (helpers_is_in_use(v))
                        helpers_wait_until_not_in_use(v);
                }
            }
        }
    
        /* Look at all inputs and outputs of scheduled tasks. */
    
        any = 0;
        for (SEXP *var_list = helpers_var_list(0); *var_list; var_list++) {
            if (NOT_MARKED(*var_list)) {
                LOOK_AT(*var_list);
                any = 1;
            }
        }
     
        if (any) return;
    }

    /* IDENTIFY WEAKLY REACHABLE NODES */

    any = 0;
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
        if (!NOT_MARKED(WEAKREF_KEY(s))) {
            if (NOT_MARKED(WEAKREF_VALUE(s))) {
                LOOK_AT(WEAKREF_VALUE(s));
                any = 1;
            }
            if (NOT_MARKED(WEAKREF_FINALIZER(s))) {
                LOOK_AT(WEAKREF_FINALIZER(s));
                any = 1;
            }
        }
    }

    if (any) return;

    /* mark nodes ready for finalizing */

    CheckFinalizers();

    /* process the weak reference chain */

    any = 0;
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
        if (NOT_MARKED(s)) {
            LOOK_AT(s);
            any = 1;
        }
        if (NOT_MARKED(WEAKREF_KEY(s))) {
            LOOK_AT(WEAKREF_KEY(s));
            any = 1;
        }
        if (NOT_MARKED(WEAKREF_VALUE(s))) {
            LOOK_AT(WEAKREF_VALUE(s));
            any = 1;
        }
        if (NOT_MARKED(WEAKREF_FINALIZER(s))) {
            LOOK_AT(WEAKREF_FINALIZER(s));
            any = 1;
        }
    }

    if (any) return;

    /* PROCESS CHARSXP CACHE */

    if (R_StringHash != R_NoObject) /* in case of GC during initialization */
    {
        /* At this point, the hash table itself will not have been scanned.
           Some of the CHARSXP entries will be marked, either from being in 
           an older generation not being collected, or from a reference from
           a scanned node.  We need to remove unmarked entries here. */

	SEXP t;
	int nc = 0;
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    t = R_NilValue;
	    for (s = VECTOR_ELT(R_StringHash,i); s!=R_NilValue; s = ATTRIB(s)) {
                if (DEBUG_GLOBAL_STRING_HASH && TYPEOF(s)!=CHARSXP)
                   REprintf(
                     "R_StringHash table contains a non-CHARSXP (%d, gc)!\n",
                      TYPEOF(s));
		if (NOT_MARKED(s)) { 
                    /* remove unused CHARSXP */
		    if (t == R_NilValue) /* head of list */
                        /* Do NOT use SET_VECTOR_ELT - no old-to-new tracking */
			VECTOR_ELT(R_StringHash, i) = ATTRIB(s);
		    else
			ATTRIB(t) = ATTRIB(s);
		}
                else 
                    t = s;
	    }
	    if(VECTOR_ELT(R_StringHash, i) != R_NilValue) nc++;
	}
	SET_HASHSLOTSUSED (R_StringHash, nc);
        LOOK_AT(R_StringHash);
    }

#ifdef PROTECTCHECK
    for(i=0; i< NUM_SMALL_NODE_CLASSES;i++){
	s = NEXT_NODE(R_GenHeap[i].New);
	while (s != R_GenHeap[i].New) {
	    SEXP next = NEXT_NODE(s);
	    if (TYPEOF(s) != NEWSXP) {
		if (TYPEOF(s) != FREESXP) {
		    SETOLDTYPE(s, TYPEOF(s));
		    TYPEOF(s) = FREESXP;
		}
		if (gc_inhibit_release)
		    LOOK_AT(s);
	    }
	    s = next;
	}
    }
    s = NEXT_NODE(R_GenHeap[LARGE_NODE_CLASS].New);
    while (s != R_GenHeap[LARGE_NODE_CLASS].New) {
	SEXP next = NEXT_NODE(s);
	if (TYPEOF(s) != NEWSXP) {
	    if (TYPEOF(s) != FREESXP) {
		/**** could also leave this alone and restore the old
		      node type in ReleaseLargeFreeVectors before
		      calculating size */
		R_size_t size = getVecSizeInVEC(s);
		LENGTH(s) = size;
		SETOLDTYPE(s, TYPEOF(s));
		TYPEOF(s) = FREESXP;
	    }
	    if (gc_inhibit_release)
		LOOK_AT(s);
	}
	s = next;
    }
#endif
}


/* public interface for controlling GC torture settings */
void R_gc_torture(int gap, int wait, Rboolean inhibit)
{
    if (gap != NA_INTEGER && gap >= 0)
	gc_force_wait = gc_force_gap = gap;
    if (gap > 0) {
	if (wait != NA_INTEGER && wait > 0)
	    gc_force_wait = wait;
    }
#ifdef PROTECTCHECK
    if (gap > 0) {
	if (inhibit != NA_LOGICAL)
	    gc_inhibit_release = inhibit;
    }
    else gc_inhibit_release = FALSE;
#endif
}

static SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap;
    SEXP old = ScalarLogical(gc_force_wait > 0);

    checkArity(op, args);

    if (isLogical(CAR(args))) {
	int on = asLogical(CAR(args));
	if (on == NA_LOGICAL) gap = NA_INTEGER;
	else if (on) gap = 1;
	else gap = 0;
    }
    else gap = asInteger(CAR(args));

    R_gc_torture(gap, 0, FALSE);

    return old;
}

static SEXP do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int gap, wait;
    Rboolean inhibit;
    int old = gc_force_gap;

    checkArity(op, args);
    gap = asInteger(CAR(args));
    wait = asInteger(CADR(args));
    inhibit = asLogical(CADDR(args));
    R_gc_torture(gap, wait, inhibit);

    return ScalarInteger(old);
}

/* initialize gctorture settings from environment variables */
static void init_gctorture(void)
{
    char *arg = getenv("R_GCTORTURE");
    if (arg != NULL) {
	int gap = atoi(arg);
	if (gap > 0) {
	    gc_force_wait = gc_force_gap = gap;
	    arg = getenv("R_GCTORTURE_WAIT");
	    if (arg != NULL) {
		int wait = atoi(arg);
		if (wait > 0)
		    gc_force_wait = wait;
	    }
#ifdef PROTECTCHECK
	    arg = getenv("R_GCTORTURE_INHIBIT_RELEASE");
	    if (arg != NULL) {
		int inhibit = atoi(arg);
		if (inhibit > 0) gc_inhibit_release = TRUE;
		else gc_inhibit_release = FALSE;
	    }
#endif
	}
    }
}

static SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = ScalarLogical(gc_reporting);
    checkArity(op, args);
    i = asLogical(CAR(args));
    if (i != NA_LOGICAL)
	gc_reporting = i;
    return old;
}

/* reports memory use to profiler in eval.c */

void attribute_hidden get_current_mem(unsigned long *smallvsize,
				      unsigned long *largevsize,
				      unsigned long *nodes)
{
    *smallvsize = 0 /* R_SmallVallocSize */;
    *largevsize = 0;
    *nodes = 0;
    return;
}

static SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    int ogc, reset_max;
    double R_NMega;
    R_size_t onsize = R_NSize /* can change during collection */;

    checkArity(op, args);
    ogc = gc_reporting;
    gc_reporting = asLogical(CAR(args));
    reset_max = asLogical(CADR(args));
    gc_next_level = 2;

    R_gc();

    gc_reporting = ogc;
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 14));
    for (int i = 0; i < 14; i++) REAL(value)[i] = NA_REAL;
    UNPROTECT(1);
    return value;
}


/* InitMemory : Initialise the memory to be used in R. */

#define PP_REDZONE_SIZE 1000L
static R_size_t R_StandardPPStackSize, R_RealPPStackSize;

void attribute_hidden InitMemory()
{
    int gen, i;

#if VALGRIND_TEST
    valgrind_test();
#endif

    sggc_init(2000000);

    extern void Rf_constant_init(void);
    Rf_constant_init();

#   if 0
        extern SEXP R_inspect(SEXP);
        close(1); dup(2);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("R_NilValue:\n");
        R_inspect(R_NilValue);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("EmptyEnv:\n");
        R_inspect(R_EmptyEnv);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("UnboundValue:\n");
        R_inspect(R_UnboundValue);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("TRUE:\n");
        R_inspect(R_ScalarLogicalTRUE);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("3L:\n");
        R_inspect(R_ScalarInteger0To10(3));
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("1.0:\n");
        R_inspect(R_ScalarRealOne);
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
        REprintf("pairlist(NA):\n");
        R_inspect(MaybeConstList1(R_ScalarLogicalNA));
        REprintf("-----\n"); fflush(stdout); fflush(stderr);
#   endif

    init_gctorture();

    gc_reporting = R_Verbose;
    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    if (!(R_PPStack = (SEXP *) malloc(R_RealPPStackSize * sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;
#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_NOACCESS(R_PPStack+R_PPStackSize, PP_REDZONE_SIZE);
#endif

    R_BCNodeStackBase = (SEXP *) malloc(R_BCNODESTACKSIZE * sizeof(SEXP));
    if (R_BCNodeStackBase == NULL)
	R_Suicide("couldn't allocate node stack");
#ifdef BC_INT_STACK
    R_BCIntStackBase =
      (IStackval *) malloc(R_BCINTSTACKSIZE * sizeof(IStackval));
    if (R_BCIntStackBase == NULL)
	R_Suicide("couldn't allocate integer stack");
#endif
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
#ifdef BC_INT_STACK
    R_BCIntStackTop = R_BCIntStackBase;
    R_BCIntStackEnd = R_BCIntStackBase + R_BCINTSTACKSIZE;
#endif

    R_weak_refs = R_NilValue;  /* This is redundant: it's statically initialized
                                  above so it'll work in R_Suicide at startup */

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;
    
    /*  The current source line */
    R_Srcref = R_NilValue;
}


/* Allocate a vector object. */

static SEXP alloc_vec (SEXPTYPE type, R_len_t length)
{
    sggc_type_t sggctype = R_type_to_sggc_type[type];
    sggc_length_t sggclength = Rf_nchunks(type,length);

    if (gc_countdown-- == 0) {
        R_gc_internal(); 
        gc_countdown = GC_INTERVAL;
    }

    sggc_cptr_t cp = sggc_alloc (sggctype, sggclength);

    while (cp == SGGC_NO_OBJECT) {
        if (gc_countdown == GC_INTERVAL
             && gc_last_level < 2
             && gc_next_level < gc_last_level + 1) {
            gc_next_level = gc_last_level + 1;
        }
        R_gc_internal();
        gc_countdown = GC_INTERVAL;
        cp = sggc_alloc (sggctype, sggclength);
        if (cp == SGGC_NO_OBJECT && gc_last_level == 2 && !gc_ran_finalizers)
            R_Suicide("out of memory");
    }

    SEXP r = SEXP_PTR (cp);
    r->cptr = cp;
    TYPEOF(r) = type;
    ATTRIB(r) = R_NilValue;
    LENGTH(r) = length;

    return r;
}


/* Allocate a non-vector object. */

static SEXP alloc_nonvec (SEXPTYPE type)
{
    return alloc_vec (type, 1);
}


/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector.  Defined using the fast macros in Defn.h */
void *vmaxget(void)
{
    return VMAXGET();
}

void vmaxset(const void *ovmax)
{
    VMAXSET(ovmax);
}

char *R_alloc(size_t nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    double dsize = (double)nelem * eltsize;
    if (dsize > 0) { /* precaution against integer overflow */
	SEXP s;
#if SIZEOF_SIZE_T > 4
	/* In this case by allocating larger units we can get up to
	   size(double) * (2^31 - 1) bytes, approx 16Gb */
	if(dsize < R_LEN_T_MAX)
	    s = allocVector(RAWSXP, size + 1);
	else if(dsize < sizeof(double) * (R_LEN_T_MAX - 1))
	    s = allocVector(REALSXP, (int)(0.99+dsize/sizeof(double)));
	else {
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/1024.0/1024.0/1024.0);
	}
#else
	if(dsize > R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/1024.0/1024.0/1024.0);
	s = allocVector(RAWSXP, size + 1);
#endif
	ATTRIB(s) = R_VStack == NULL ? R_NilValue : R_VStack;
	R_VStack = s;
	return (char *)DATAPTR(s);
    }
    else return NULL;
}


/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    R_size_t size  = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);

    memset(p, 0, size);
    return p;
}


char *S_realloc(char *p, long new, long old, int size)
{
    size_t nold;
    char *q;
    /* shrinking is a no-op */
    if(new <= old) return p;
    q = R_alloc((size_t)new, size);
    nold = (size_t)old * size;
    memcpy(q, p, nold);
    memset(q + nold, 0, (size_t)new*size - nold);
    return q;
}

/* "allocSExp" allocate a SEXPREC */
/* call gc if necessary */

SEXP allocSExp(SEXPTYPE t)
{
    SEXP s = alloc_nonvec(t);
    CAR(s) = R_NilValue;
    CDR(s) = R_NilValue;
    TAG(s) = R_NilValue;
    return s;
}

/* Caller needn't protect arguments of cons. */

SEXP cons(SEXP car, SEXP cdr)
{
    PROTECT2(car,cdr);
    SEXP s = alloc_nonvec(LISTSXP);
    CAR(s) = Rf_chk_valid_SEXP(car);
    CDR(s) = Rf_chk_valid_SEXP(cdr);
    TAG(s) = R_NilValue;
/* REprintf("Did cons:\n");
extern SEXP R_inspect(SEXP);
R_inspect(s);
REprintf("-----\n"); */
    UNPROTECT(2);
    return s;
}

/* Version of cons that sets TAG too.  Caller needn't protect arguments. */

SEXP cons_with_tag(SEXP car, SEXP cdr, SEXP tag)
{
    PROTECT3(car,cdr,tag);
    SEXP s = alloc_nonvec(LISTSXP);
    CAR(s) = Rf_chk_valid_SEXP(car);
    CDR(s) = Rf_chk_valid_SEXP(cdr);
    TAG(s) = Rf_chk_valid_SEXP(tag);
    UNPROTECT(3);
    return s;
}

/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".  Note that "namelist" 
  can be shorter than "valuelist" if the rest of "valuelist" already 
  has tags. (In particular, "namelist" can be R_NilValue if all of
  "valuelist" already has tags.)

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame. */

SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    PROTECT3(namelist,valuelist,rho);
    SEXP newrho = alloc_nonvec(ENVSXP);
    SEXP v, n;

    FRAME(newrho) = valuelist;
    ENCLOS(newrho) = Rf_chk_valid_SEXP(rho);
    HASHTAB(newrho) = R_NilValue;

    v = Rf_chk_valid_SEXP(valuelist);
    n = Rf_chk_valid_SEXP(namelist);
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }
    UNPROTECT(3);
    return (newrho);
}

/* mkPROMISE protects its arguments.

   NAMEDCNT for the new promise is set to 1, and 'expr' has its NAMEDCNT
   set to the maximum. */

SEXP attribute_hidden mkPROMISE(SEXP expr, SEXP rho)
{
    PROTECT2(expr,rho);
    SEXP s = alloc_nonvec(PROMSXP);

    SET_NAMEDCNT_MAX(expr);
    /* SET_NAMEDCNT_1(s); */

    s->u.promsxp.value = R_UnboundValue;
    PRCODE(s) = Rf_chk_valid_SEXP(expr);
    PRENV(s) = Rf_chk_valid_SEXP(rho);
    PRSEEN(s) = 0;
    UNPROTECT(2);
    return s;
}


/*  mkPRIMSXP - return a builtin function      */
/*              either "builtin" or "special"  */

/*  The value produced is cached do avoid the need for GC protection
    in cases where a .Primitive is produced by unserializing or
    reconstructed after a package has clobbered the value assigned to
    a symbol in the base package.  Also relied on to protect op in "eval". */

SEXP attribute_hidden mkPRIMSXP(int offset, int eval)
{
    SEXP result;
    SEXPTYPE type = eval ? BUILTINSXP : SPECIALSXP;
    static SEXP PrimCache = NULL;
    static int FunTabSize = 0;
    
    if (PrimCache == NULL) {
	/* compute the number of entires in R_FunTab */
	while (R_FunTab[FunTabSize].name)
	    FunTabSize++;

	/* allocate and protect the cache */
	PrimCache = allocVector(VECSXP, FunTabSize);
	R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
	error("offset is out of R_FunTab range");

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue) {
	result = alloc_nonvec(type);
	SET_PRIMOFFSET(result, offset);
        SET_VECTOR_ELT (PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
	error("requested primitive type is not consistent with cached value");

    return result;
}


/* This is called by function() {}, where an invalid
   body should be impossible. When called from
   other places (eg do_asfunction) they
   should do this checking in advance */

/*  mkCLOSXP - return a closure with formals f,  */
/*             body b, and environment rho       */

SEXP attribute_hidden mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    PROTECT3(formals,body,rho);
    SEXP c = alloc_nonvec(CLOSXP);

#ifdef not_used_CheckFormals
    if(isList(formals))
	SET_FORMALS(c, formals);
    else
	error(_("invalid formal arguments for 'function'"));
#else
    SET_FORMALS(c, formals);
#endif
    switch (TYPEOF(body)) {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
	error(_("invalid body argument for 'function'"));
	break;
    default:
	SET_BODY(c, body);
	break;
    }

    if(rho == R_NilValue)
	SET_CLOENV(c, R_GlobalEnv);
    else
	SET_CLOENV(c, rho);
    UNPROTECT(3);
    return c;
}


/*  mkSYMSXP - return a symsxp with the string  */
/*             name inserted in the name field  */

static int isDDName(SEXP name)
{
    const char *buf;
    char *endp;

    buf = CHAR(name);
    if (buf[0]=='.' && buf[1]=='.' && buf[2]!=0) {
	(void) strtol(buf+2, &endp, 10);
        return *endp == 0;
    }
    return 0;
}

SEXP attribute_hidden mkSYMSXP(SEXP name, SEXP value)
{
    PROTECT2(name,value);
    SEXP c = alloc_nonvec(SYMSXP);
    PRINTNAME(c) = name;
    SYMVALUE(c) = value;
    INTERNAL(c) = R_NilValue;
    NEXTSYM_PTR(c) = R_NilValue;
    LASTSYMENV(c) = NULL;
    LASTSYMBINDING(c) = NULL;
    LASTSYMENVNOTFOUND(c) = NULL;
    SET_DDVAL(c, isDDName(name));
    UNPROTECT(2);
    return c;
}


/* Fast, specialize allocVector for vectors of length 1.  The type 
   passed must be RAWSXP, LGLSXP, INTSXP, or REALSXP, so a that
   there's no need to initialize a pointer in the data part. 

   The version with an argument is static.  Versions for each allowed
   type are defined below for use elsewhere in the interpreter, in which
   we hope the compiler will optimize the tail call to a simple jump. 
   (This avoids any need for an error check on "type" to guard against 
   mis-use.) */

static SEXP allocVector1 (SEXPTYPE type)
{
#if VALGRIND_LEVEL==0
    SEXP s = alloc_vec(type,1);
    if (R_IsMemReporting && !R_MemPagesReporting)
        R_ReportAllocation (
          sizeof(SEXPREC_ALIGN) + sizeof(VECREC), type, 1);
    return s;
#else
    return allocVector (type, 1);
#endif
}

SEXP allocVector1RAW(void)  { return allocVector1(RAWSXP); }
SEXP allocVector1LGL(void)  { return allocVector1(LGLSXP); }
SEXP allocVector1INT(void)  { return allocVector1(INTSXP); }
SEXP allocVector1REAL(void) { return allocVector1(REALSXP); }


/* These are kept for compatibility, though ScalarLogicalMaybeConst
   is preferred, unless attributes are to be attached later. */

SEXP mkTrue(void)
{
    SEXP s = allocVector1LGL();
    LOGICAL(s)[0] = 1;
    return s;
}

SEXP mkFalse(void)
{
    SEXP s = allocVector1LGL();
    LOGICAL(s)[0] = 0;
    return s;
}


/* Versions of functions for allocation of scalars that may return a 
   shared object.  ScalarLogicalMaybeConst is in Rinlinedfuns.h. */

SEXP ScalarIntegerMaybeConst(int x)
{
    if (ENABLE_SHARED_CONSTANTS) {
        if (x >=0 && x <= 10)
            return R_ScalarInteger0To10(x);
        if (x == NA_INTEGER)
            return R_ScalarIntegerNA;
    }

    return ScalarInteger(x);
}

SEXP ScalarRealMaybeConst(double x)
{
    if (ENABLE_SHARED_CONSTANTS) {

        /* Compare to pre-allocated values as 8-byte unsigned integers, not 
           as doubles, since double comparison doesn't work for NA or when 
           comparing -0 and +0 (which should be distinct). */

        uint64_t xv = *(uint64_t*) &x;

        if (xv == *(uint64_t*) &REAL(R_ScalarRealZero)[0])
            return R_ScalarRealZero;
        if (xv == *(uint64_t*) &REAL(R_ScalarRealOne)[0])
            return R_ScalarRealOne;
        if (xv == *(uint64_t*) &REAL(R_ScalarRealNA)[0])
            return R_ScalarRealNA;
    }

    return ScalarReal(x);
}

SEXP ScalarComplexMaybeConst(Rcomplex x)
{
    return ScalarComplex(x);
}

SEXP ScalarStringMaybeConst(SEXP x)
{
    return ScalarString(x);
}

SEXP ScalarRawMaybeConst(Rbyte x)
{
    return ScalarRaw(x);
}

/* Allocate a vector object (and also list-like objects).
   This ensures only validity of list-like (LISTSXP, VECSXP, EXPRSXP),
   STRSXP and CHARSXP types;  e.g., atomic types remain un-initialized
   and must be initialized upstream, e.g., in do_makevector().
*/

SEXP allocVector(SEXPTYPE type, R_len_t length)
{
    SEXP s;
    int i;

    if (length < 0 )
        errorcall(R_GlobalContext->call,
                  _("negative length vectors are not allowed"));

    /* Handle pairlists, which aren't actually vectors, but are nevertheless
       allowed types for allocVector. */

    switch (type) {
    case NILSXP:
        return R_NilValue;
    case LANGSXP:
        if (length == 0) return R_NilValue;
        s = allocList(length);
        TYPEOF(s) = LANGSXP;
        return s;
    case LISTSXP:
        return allocList(length);
    }

    s = alloc_vec(type,length);

    if (R_IsMemReporting) {
        if (!R_MemPagesReporting) {
            R_ReportAllocation (
                sggc_nchunks(type,length) * SGGC_CHUNK_SIZE,
                type, length);
        }
    }

#if VALGRIND_LEVEL>0
    VALGRIND_MAKE_MEM_UNDEFINED(DATAPTR(s), actual_size);
#endif

    /* Allocated space has been zeroed, which ensures CHARSXP has terminating
       null.  But still need to set STRSXPs to R_BlankString and VECSXP/EXPRSXPs
       to R_NilValue. */

    if (type == VECSXP || type == EXPRSXP) {
	for (i = 0; i < length; i++)
	    VECTOR_ELT(s,i) = R_NilValue;  /* no old-to-new check needed */
    }
    else if (type == STRSXP) {
	for (i = 0; i < length; i++)
	    STRING_ELT(s,i) = R_BlankString;  /* no old-to-new check needed */
    }

    return s;
}

/* For future hiding of allocVector(CHARSXP) */
SEXP attribute_hidden allocCharsxp(R_len_t len)
{
    return allocVector(CHARSXP, len);
}


SEXP allocList(int n)
{
    int i;
    SEXP result;
    result = R_NilValue;
    for (i = 0; i < n; i++)
	result = CONS(R_NilValue, result);
    return result;
}

SEXP allocS4Object(void)
{
   SEXP s = alloc_nonvec(S4SXP);
   SET_S4_OBJECT(s);
   TAG(s) = R_NilValue;
   return s;
}


/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    R_gc_internal();
}

extern double R_getClockIncrement(void);
extern void R_getProcTime(double *data);

static double gctimes[5], gcstarttimes[5];
static Rboolean gctime_enabled = FALSE;

/* this is primitive */
static SEXP do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (args == R_NilValue)
	gctime_enabled = TRUE;
    else {
	check1arg(args, call, "on");
	gctime_enabled = asLogical(CAR(args));
    }
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (gctime_enabled)
	R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (gctime_enabled) {
	double times[5];
	R_getProcTime(times);

	gctimes[0] += times[0] - gcstarttimes[0];
	gctimes[1] += times[1] - gcstarttimes[1];
	gctimes[2] += times[2] - gcstarttimes[2];
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}

#define R_MAX(a,b) ( (a) < (b) ? (b) : (a) )

static void R_gc_internal(void)
{
    gc_count++;

    BEGIN_SUSPEND_INTERRUPTS {
	gc_start_timing();
	sggc_collect(gc_next_level);
        gc_last_level = gc_next_level;
	gc_end_timing();
    } END_SUSPEND_INTERRUPTS;

    gc_next_level = ((gc_count&0x7)==0) + ((gc_count&0x1f)==0);

    gc_ran_finalizers = RunFinalizers();

    if (gc_reporting) {
        REprintf("Did garbage collection #%lld at level %d\n",
                  gc_count, gc_last_level);
    }
}

static SEXP do_memlimits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    double nsize, vsize;
    R_size_t tmp;

    checkArity(op, args);
    nsize = asReal(CAR(args));
    vsize = asReal(CADR(args));

    PROTECT(ans = allocVector(REALSXP, 2));
    REAL(ans)[0] = NA_REAL;
    REAL(ans)[1] = NA_REAL;
    UNPROTECT(1);
    return ans;
}

static SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    int i, tmp;

    PROTECT(ans = allocVector(INTSXP, 24));
    PROTECT(nms = allocVector(STRSXP, 24));
    for (i = 0; i < 24; i++) {
	INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str(i > LGLSXP? i+2 : i));
    }
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

/* "protect" pushes a single argument onto R_PPStack.

   In handling a stack overflow we have to be careful not to use
   PROTECT. error("protect(): stack overflow") would call deparse1,
   which uses PROTECT and segfaults.

   However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT 

   The PROTECT, UNPROTECT, PROTECT_WITH_INDEX, and REPROTECT macros at 
   the end of Defn.h do these things without procedure call overhead, and 
   are used here to define these functions, to keep the code in sync. 

   The procedure versions of protect, protect2, and protect3 do an
   error check.  Fiddling the condition for redefining PROTECT, etc.
   in Defn.h can enable use of these for debugging in the interpeter.
*/

static void reset_pp_stack(void *data)
{
    R_size_t *poldpps = data;
    R_PPStackSize =  *poldpps;
}

R_NORETURN void attribute_hidden Rf_protect_error (void)
{
    RCNTXT cntxt;
    R_size_t oldpps = R_PPStackSize;

    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
             R_NilValue, R_NilValue);
    cntxt.cend = &reset_pp_stack;
    cntxt.cenddata = &oldpps;

    if (R_PPStackSize < R_RealPPStackSize)
        R_PPStackSize = R_RealPPStackSize;
    errorcall(R_NilValue, _("protect(): protection stack overflow"));
}

SEXP protect(SEXP s)
{
    if (s != NULL && TYPEOF(s) == NILSXP && s != R_NilValue) abort();
    return PROTECT (Rf_chk_valid_SEXP(s));
}


/* Push 2 or 3 arguments onto protect stack.  BEWARE! All arguments will
   be evaluated (in the C sense) before any are protected. */

void Rf_protect2 (SEXP s1, SEXP s2)
{
    if (s1 != NULL && TYPEOF(s1) == NILSXP && s1 != R_NilValue
     || s2 != NULL && TYPEOF(s2) == NILSXP && s2 != R_NilValue) abort();

    PROTECT2 (Rf_chk_valid_SEXP(s1), Rf_chk_valid_SEXP(s2));
}

void Rf_protect3 (SEXP s1, SEXP s2, SEXP s3)
{
    if (s1 != NULL && TYPEOF(s1) == NILSXP && s1 != R_NilValue
     || s2 != NULL && TYPEOF(s2) == NILSXP && s2 != R_NilValue
     || s3 != NULL && TYPEOF(s3) == NILSXP && s3 != R_NilValue) abort();

    PROTECT3 (Rf_chk_valid_SEXP(s1), Rf_chk_valid_SEXP(s2), Rf_chk_valid_SEXP(s3));
}


/* "unprotect" pop argument list from top of R_PPStack */

R_NORETURN void attribute_hidden Rf_unprotect_error (void)
{
    error(_("unprotect(): only %d protected items"), R_PPStackTop);
}

void unprotect(int l)
{
    UNPROTECT(l);
}


/* "unprotect_ptr" remove pointer from somewhere in R_PPStack.  Don't
   try to combine use of this with use of ProtectWithIndex! */

void unprotect_ptr(SEXP s)
{
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
	if (i == 0)
	    error(_("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it, if any */

    while (++i < R_PPStackTop) R_PPStack[i - 1] = R_PPStack[i];

    R_PPStackTop--;
}

SEXP R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    return PROTECT_WITH_INDEX(Rf_chk_valid_SEXP(s),pi);
}

void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    REPROTECT(Rf_chk_valid_SEXP(s),i);
}

/* remove all objects from the protection stack from index i upwards
   and return them in a vector. The order in the vector is from new
   to old. */
SEXP R_CollectFromIndex(PROTECT_INDEX i)
{
    SEXP res;
    R_size_t top = R_PPStackTop, j = 0;
    if (i > top) i = top;
    res = protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    R_PPStackTop = top; /* this includes the protect we used above */
    return res;
}

/* "initStack" initialize environment stack */
void initStack(void)
{
    R_PPStackTop = 0;
}


/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p) /* problem here is that we don't have a format for size_t. */
	error(_("Calloc could not allocate memory (%.0f of %u bytes)"),
	      (double) nelem, elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if(ptr) p = realloc(ptr, size); else p = malloc(size);
    if(!p)
	error(_("Realloc could not re-allocate memory (%.0f bytes)"), 
	      (double) size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. Preserving/Releasing NULL is ignored. */

void R_PreserveObject(SEXP object)
{
    if (object != NULL)
        R_PreciousList = CONS(object, R_PreciousList);
}

static SEXP RecursiveRelease(SEXP object, SEXP list)
{
    if (!isNull(list)) {
	if (object == CAR(list))
	    return CDR(list);
	else
	    CDR(list) = RecursiveRelease(object, CDR(list));
    }
    return list;
}

void R_ReleaseObject(SEXP object)
{
    if (object != NULL)
        R_PreciousList = RecursiveRelease(object, R_PreciousList);
}


/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = alloc_nonvec(EXTPTRSXP);
    EXTPTR_PTR(s) = p;
    EXTPTR_PROT(s) = Rf_chk_valid_SEXP(prot);
    EXTPTR_TAG(s) = Rf_chk_valid_SEXP(tag);
    return s;
}

/* Work around casting issues: works where it is needed */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

/* used in package methods */
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    SEXP s = alloc_nonvec(EXTPTRSXP);
    tmp.fn = p;
    EXTPTR_PTR(s) = tmp.p;
    EXTPTR_PROT(s) = Rf_chk_valid_SEXP(prot);
    EXTPTR_TAG(s) = Rf_chk_valid_SEXP(tag);
    return s;
}


/* ------------------------------------------------------------------------ */

static SEXP do_pnamedcnt(SEXP call, SEXP op, SEXP args, SEXP rho)
{   SEXP a;
    int j;

    if (args == R_NilValue)
        error(_("too few arguments"));

    check1arg_x (args, call);

    for (a = CDR(args); a != R_NilValue; a = CDR(a))
        if (!isString(CAR(a)))
            error(_("invalid argument"));

    /* access nmcnt directly, so won't delay for possible task syncronization */
    Rprintf ("PNAMEDCNT:  %d  %x  %s", CAR(args)->sxpinfo.nmcnt, CAR(args),
                                       type2char(TYPEOF(CAR(args))));

    for (a = CDR(args); a != R_NilValue; a = CDR(a)) {
        Rprintf(" :");
        for (j = 0; j < LENGTH(CAR(a)); j++)
            Rprintf(" %s", CHAR(STRING_ELT(CAR(a),j)));
    }

    Rprintf("\n");

    return CAR(args);
}


/*******************************************/
/* Non-sampling memory use profiler reports vector allocations and/or
   calls to GetNewPage */
/*******************************************/

static void R_OutputStackTrace (void)
{
    RCNTXT *cptr;
    int newline;

    if (!R_MemStackReporting) goto print_newline;

    newline = R_MemReportingToTerminal | R_MemDetailsReporting;

    if (R_MemReportingOutfile != NULL) 
        fprintf (R_MemReportingOutfile, ":");
    if (R_MemReportingToTerminal) 
        REprintf (":");

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if (!newline) newline = 1;
	    if (R_MemReportingOutfile != NULL)
                fprintf (R_MemReportingOutfile, "\"%s\" ",
		         TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		         "<Anonymous>");
	    if (R_MemReportingToTerminal)
                REprintf ("\"%s\" ",
		          TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		          "<Anonymous>");
	}
    }

    if (!newline) return;

print_newline:
    if (R_MemReportingOutfile != NULL) 
        fprintf (R_MemReportingOutfile, "\n");
    if (R_MemReportingToTerminal) 
        REprintf ("\n");
}

static void R_ReportAllocation (R_size_t size, SEXPTYPE type, R_len_t length)
{
    if (size > R_MemReportingThreshold && length >= R_MemReportingNElem) {
        if (R_MemReportingOutfile != NULL) {
            if (R_MemDetailsReporting)
                fprintf (R_MemReportingOutfile, "%lu (%s %lu)",
                  (unsigned long) size, type2char(type), (unsigned long)length);
            else 
                fprintf (R_MemReportingOutfile, "%lu ",
                  (unsigned long) size);
        }
        if (R_MemReportingToTerminal) {
            if (R_MemDetailsReporting)
                REprintf ("RPROFMEM: %lu (%s %lu)",
                  (unsigned long) size, type2char(type), (unsigned long)length);
            else
                REprintf ("RPROFMEM: %lu ", 
                  (unsigned long) size);
        }
        R_OutputStackTrace();
    }
}

static void R_ReportNewPage(void)
{
    if (R_MemPagesReporting) {
        if (R_MemReportingOutfile != NULL)
            fprintf (R_MemReportingOutfile, "new page");
        if (R_MemReportingToTerminal)
            REprintf ("RPROFMEM: new page");
	R_OutputStackTrace();
    }
}

static void R_EndMemReporting(void)
{
    if(R_MemReportingOutfile != NULL) {
	fclose (R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    R_IsMemReporting = 0;
}

static void R_InitMemReporting(SEXP filename, int append)
{
    if (R_IsMemReporting)
        R_EndMemReporting();

    if (strlen(CHAR(filename)) > 0) {
        R_MemReportingOutfile = RC_fopen (filename, append ? "a" : "w", TRUE);
        if (R_MemReportingOutfile == NULL)
            error(_("Rprofmem: cannot open output file '%s'"), filename);
    }
    else
        R_MemReportingOutfile = NULL;

    R_IsMemReporting = 1;

    return;
}

static SEXP do_Rprofmem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP filename, ap;
    int append_mode;

    checkArity(op, args);

    ap = args;
    if (!isString(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "filename");
    filename = STRING_ELT(CAR(ap), 0);

    ap = CDR(ap);
    append_mode = asLogical(CAR(ap));

    ap = CDR(ap);
    if (!isReal(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "threshold");
    R_MemReportingThreshold = REAL(CAR(ap))[0];

    ap = CDR(ap);
    if (!isReal(CAR(ap)) || (LENGTH(CAR(ap))) != 1)
	error(_("invalid '%s' argument"), "nelem");
    R_MemReportingNElem = REAL(CAR(ap))[0];

    ap = CDR(ap);
    R_MemStackReporting = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemReportingToTerminal = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemPagesReporting = asLogical(CAR(ap));

    ap = CDR(ap);
    R_MemDetailsReporting = asLogical(CAR(ap));

    if (R_MemReportingToTerminal || strlen(CHAR(filename)) > 0)
	R_InitMemReporting(filename, append_mode);
    else
	R_EndMemReporting();

    return R_NilValue;
}


/* String cache routines, including string hashing - formerly in envir.c */

SEXP mkCharCE(const char *name, cetype_t enc)
{
    size_t len =  strlen(name);
    if (len > INT_MAX)
	error("R character strings are limited to 2^31-1 bytes");
    return mkCharLenCE(name, (int) len, enc);
}

/* no longer used in R but docuented in 2.7.x */
SEXP mkCharLen(const char *name, int len)
{
    return mkCharLenCE(name, len, CE_NATIVE);
}

SEXP mkChar(const char *name)
{
    size_t len =  strlen(name);
    if (len > INT_MAX)
	error("R character strings are limited to 2^31-1 bytes");
    return mkCharLenCE(name, (int) len, CE_NATIVE);
}

/* CHARSXP hashing follows the hash structure from envir.c, but need separate
   code for get/set of values since our keys are char* and not SEXP symbol types
   and the string hash table is treated specially in garbage collection.

   Experience has shown that it is better to use a different hash function,
   and a power of 2 for the hash size.
*/

/* char_hash_size MUST be a power of 2 and char_hash_mask == char_hash_size - 1
   in order for x & char_hash_mask to be equivalent to x % char_hash_size. */

static unsigned int char_hash_size = STRHASHINITSIZE;
static unsigned int char_hash_mask = STRHASHINITSIZE-1;

/* The hashing function is in util.c to stop the compiler from inlining it. */
unsigned int Rf_char_hash(const char *s, int len);

void attribute_hidden InitStringHash()
{
    R_StringHash = allocVector (VECSXP, char_hash_size);
    HASHSIZE(R_StringHash) = char_hash_size;
    HASHSLOTSUSED(R_StringHash) = 0;
}

/* Resize the global R_StringHash CHARSXP cache */
static void R_StringHash_resize(unsigned int newsize)
{
    SEXP old_table = R_StringHash;
    SEXP new_table, new_chain, val, next;
    unsigned int counter, new_hashcode, newmask;
#if DEBUG_GLOBAL_STRING_HASH
    unsigned int oldsize = HASHSIZE(R_StringHash);
    unsigned int oldslotsused = HASHSLOTSUSED(R_StringHash);
#endif

    /* Allocate the new hash table.  Chain moving is destructive and 
       does not involve allocation, so this is the only point where
       GC can occur.  The allocation could fail - ideally we would recover 
       from that and carry on with the original table, but we don't now. */

    new_table = allocVector (VECSXP, newsize);
    SET_HASHSLOTSUSED (new_table, 0);
    newmask = newsize - 1;

    /* transfer chains from old table to new table */
    for (counter = 0; counter < LENGTH(old_table); counter++) {
	val = VECTOR_ELT(old_table, counter);
	while (val != R_NilValue) {
	    next = ATTRIB(val);
#if DEBUG_GLOBAL_STRING_HASH
            if (TYPEOF(val)!=CHARSXP)
               REprintf("R_StringHash table contains a non-CHARSXP (%d, rs)!\n",
                        TYPEOF(val));
#endif
	    new_hashcode = Rf_char_hash (CHAR(val), LENGTH(val)) & newmask;
	    new_chain = VECTOR_ELT(new_table, new_hashcode);
	    /* If using a previously-unused slot then increase HASHSLOTSUSED */
	    if (new_chain == R_NilValue)
		SET_HASHSLOTSUSED(new_table, HASHSLOTSUSED(new_table) + 1);
	    /* Move the current chain link to the new chain.  This is a 
               destrictive modification, which does NOT do the old-to-new
               check, since table entries aren't supposed to be marked
               in the initial pass of the GC. */
	    ATTRIB(val) = new_chain;                   /* not SET_ATTRIB! */
	    VECTOR_ELT(new_table, new_hashcode) = val; /* not SET_VECTOR_ELT! */
	    val = next;
	}
    }
    R_StringHash = new_table;
    char_hash_size = newsize;
    char_hash_mask = newmask;
#if DEBUG_GLOBAL_STRING_HASH
    Rprintf ("Resized:  size %d => %d,  slotsused %d => %d\n",
      oldsize, HASHSIZE(new_table), oldslotsused, HASHSLOTSUSED(new_table));
#endif
}

/* mkCharCE - make a character (CHARSXP) variable and set its
   encoding bit.  If a CHARSXP with the same string already exists in
   the global CHARSXP cache, R_StringHash, it is returned.  Otherwise,
   a new CHARSXP is created, added to the cache and then returned. */


/* Because allocCharsxp allocates len+1 bytes and zeros the last,
   this will always zero-terminate */
SEXP mkCharLenCE(const char *name, int len, cetype_t enc)
{
    SEXP cval, val;
    unsigned int hashcode;
    int need_enc;
    Rboolean embedNul = FALSE, is_ascii = TRUE;

    switch(enc){
    case CE_NATIVE:
    case CE_UTF8:
    case CE_LATIN1:
    case CE_BYTES:
    case CE_SYMBOL:
    case CE_ANY:
	break;
    default:
	error(_("unknown encoding: %d"), enc);
    }
    for (int slen = 0; slen < len; slen++) {
	if ((unsigned int) name[slen] > 127) is_ascii = FALSE;
	if (!name[slen]) embedNul = TRUE;
    }
    if (embedNul) {
	SEXP c;
	/* This is tricky: we want to make a reasonable job of
	   representing this string, and EncodeString() is the most
	   comprehensive */
	c = allocCharsxp(len);
	memcpy(CHAR_RW(c), name, len);
	switch(enc) {
	case CE_UTF8: SET_UTF8(c); break;
	case CE_LATIN1: SET_LATIN1(c); break;
	case CE_BYTES: SET_BYTES(c); break;
	default: break;
	}
	if (is_ascii) SET_ASCII(c);
	error(_("embedded nul in string: '%s'"),
	      EncodeString(c, 0, 0, Rprt_adj_none));
    }

    if (enc && is_ascii) enc = CE_NATIVE;
    switch(enc) {
    case CE_UTF8: need_enc = UTF8_MASK; break;
    case CE_LATIN1: need_enc = LATIN1_MASK; break;
    case CE_BYTES: need_enc = BYTES_MASK; break;
    default: need_enc = 0;
    }

    hashcode = Rf_char_hash(name, len) & char_hash_mask;

    /* Search for a cached value */
    cval = R_NilValue;
    for (val = VECTOR_ELT(R_StringHash, hashcode); 
         val != R_NilValue; 
         val = ATTRIB(val)) {
	if (need_enc == (ENC_KNOWN(val) | IS_BYTES(val))) {
            if (LENGTH(val) == len) {
                if (len == 0 || *CHAR(val) == *name /* quick pretest */
                                   && memcmp(CHAR(val), name, len) == 0) {
                    cval = val;
                    break;
                }
            }
	}
    }
    if (cval == R_NilValue) {
	/* no cached value; need to allocate one and add to the cache */
	cval = allocCharsxp(len);
	memcpy(CHAR_RW(cval), name, len);
	switch(enc) {
	case 0:
	    break;          /* don't set encoding */
	case CE_UTF8:
	    SET_UTF8(cval);
	    break;
	case CE_LATIN1:
	    SET_LATIN1(cval);
	    break;
	case CE_BYTES:
	    SET_BYTES(cval);
	    break;
	default:
	    error("unknown encoding mask: %d", enc);
	}
	if (is_ascii) SET_ASCII(cval);
	SET_CACHED(cval);  /* Mark it */
	/* add the new value to the cache */
	val = VECTOR_ELT(R_StringHash, hashcode);
	if (val == R_NilValue)
	    SET_HASHSLOTSUSED(R_StringHash, HASHSLOTSUSED(R_StringHash) + 1);

        /* The modifications below should NOT do the old-to-new check, since
           the table should not be looked at in the initial GC scan. */
	ATTRIB(cval) = val;                         /* not SET_ATTRIB! */
	VECTOR_ELT(R_StringHash, hashcode) = cval;  /* not SET_VECTOR_ELT! */

	/* Resize the hash table if desirable and possible. */
	if (HASHSLOTSUSED(R_StringHash) > 0.85 * HASHSIZE(R_StringHash)
             && 2*char_hash_size <= STRHASHMAXSIZE) {
            /* NOTE!  Must protect cval here, since it is NOT protected by
               its presence in the hash table. */
            PROTECT(cval);
	    R_StringHash_resize (2*char_hash_size);
            UNPROTECT(1);
        }
    }

    return cval;
}


#if DEBUG_SHOW_CHARSXP_CACHE
/* Call this from gdb with

       call do_show_cache(10)

   for the first 10 cache chains in use. */
void do_show_cache(int n)
{
    int i, j;
    Rprintf("Cache size: %d\n", LENGTH(R_StringHash));
    Rprintf("Cache slots used:  %d\n", HASHSLOTSUSED(R_StringHash));
    for (i = 0, j = 0; j < n && i < LENGTH(R_StringHash); i++) {
	SEXP chain = VECTOR_ELT(R_StringHash, i);
	if (chain != R_NilValue) {
	    Rprintf("Line %d: ", i);
	    do {
		if (IS_UTF8(chain))
		    Rprintf("U");
		else if (IS_LATIN1(chain))
		    Rprintf("L");
		else if (IS_BYTES(chain))
		    Rprintf("B");
		Rprintf("|%s| ", CHAR(chain));
		chain = ATTRIB(chain);
	    } while (chain != R_NilValue);
	    Rprintf("\n");
	    j++;
	}
    }
}

void do_write_cache()
{
    int i;
    FILE *f = fopen("/tmp/CACHE", "w");
    if (f != NULL) {
	fprintf(f, "Cache size: %d\n", LENGTH(R_StringHash));
	fprintf(f, "Cache slots used:  %d\n", HASHSLOTSUSED(R_StringHash));
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    SEXP chain = VECTOR_ELT(R_StringHash, i);
	    if (chain != R_NilValue) {
		fprintf(f, "Line %d: ", i);
		do {
		    if (IS_UTF8(chain))
			fprintf(f, "U");
		    else if (IS_LATIN1(chain))
			fprintf(f, "L");
		    else if (IS_BYTES(chain))
			fprintf(f, "B");
		    fprintf(f, "|%s| ", CHAR(chain));
		    chain = ATTRIB(chain);
		} while (chain != R_NilValue);
		fprintf(f, "\n");
	    }
	}
	fclose(f);
    }
}
#endif /* DEBUG_SHOW_CHARSXP_CACHE */


/* RBufferUtils, moved from deparse.c */

#include "RBufferUtils.h"

/* Allocate at least blen+1 bytes, enough to hold a string of length blen. */

attribute_hidden void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, probably no longer needed */
    if(blen == (size_t)-1) {
	warning("R_AllocStringBuffer(-1) used: please report");
	R_FreeStringBufferL(buf);
	return NULL;
    }

    if (blen < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
	buf->data = (char *) malloc(blen);
        if (buf->data) buf->data[0] = 0;
    }
    else
	buf->data = (char *) realloc(buf->data, blen);

    if (!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error("could not allocate memory (%u Mb) in C function 'R_AllocStringBuffer'",
	      (unsigned int) blen/1024/1024);
    }

    buf->bufsize = blen;
    return buf->data;
}

void attribute_hidden
R_FreeStringBuffer(R_StringBuffer *buf)
{
    if (buf->data != NULL) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

void attribute_hidden
R_FreeStringBufferL(R_StringBuffer *buf)
{
    if (buf->bufsize > buf->defaultSize) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}


/* ======== These need direct access to gp field for efficiency ======== */

/* This has NA_STRING = NA_STRING.  Inlined version is SEQL. */
int Seql(SEXP a, SEXP b)
{
    /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
    if (a == b) return 1;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
	return 0;
    else {
    	SEXP vmax = R_VStack;
    	int result = !strcmp(translateCharUTF8(a), translateCharUTF8(b));
    	R_VStack = vmax; /* discard any memory used by translateCharUTF8 */
    	return result;
    }
}


/* A count of the memory used by an object.

   This is called from user-level, so only some types of objects are important.
   
   An object gets charged for all the space allocated on the heap and
   all the nodes specifically due to it (including padding to the size
   of its node class), but not for the space for its name, nor for
   .Internals it references, nor for unused padding in pages of nodes. 

   Sharing of CHARSXPs within a string (eg, in c("abc","abc")) is accounted
   for, but not other types of sharing (eg, in list("abc","abc")).

   Constant objects (in const-objs.c) are counted as being of zero size.
*/


SEXP csduplicated(SEXP x);  /* from unique.c */

static R_size_t objectsize(SEXP s)
{
    int i;
    SEXP tmp, dup;
    Rboolean isVec = FALSE;
    R_size_t cnt = 0;

    if (IS_CONSTANT(s)) 
       return 0;

    switch (TYPEOF(s)) {
    case NILSXP:
	return(0);
	break;
    case SYMSXP:
	break;
    case LISTSXP:
    case LANGSXP:
    case BCODESXP:
	cnt += objectsize(TAG(s));
	cnt += objectsize(CAR(s));
	cnt += objectsize(CDR(s));
	break;
    case CLOSXP:
	cnt += objectsize(FORMALS(s));
	cnt += objectsize(BODY(s));
	/* no charge for the environment */
	break;
    case ENVSXP:
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
	break;
    case RAWSXP:
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	isVec = TRUE;
	break;
    case STRSXP:
	dup = csduplicated(s);
	for (i = 0; i < LENGTH(s); i++) {
	    tmp = STRING_ELT(s, i);
	    if(tmp != NA_STRING && !LOGICAL(dup)[i])
		cnt += objectsize(tmp);
	}
	isVec = TRUE;
	break;
    case DOTSXP:
    case ANYSXP:
	/* we don't know about these */
	break;
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP:
	/* Generic Vector Objects */
	for (i = 0; i < length(s); i++)
	    cnt += objectsize(VECTOR_ELT(s, i));
	isVec = TRUE;
	break;
    case EXTPTRSXP:
#if 0  /* disabled - seems to make no sense */
	cnt += sizeof(void *);  /* the actual pointer */
#endif
	cnt += objectsize(EXTPTR_PROT(s));
	cnt += objectsize(EXTPTR_TAG(s));
	break;
    case S4SXP:
	/* Has TAG and ATTRIB but no CAR nor CDR */
	cnt += objectsize(TAG(s));
	break;
    default:
	UNIMPLEMENTED_TYPE("object.size", s);
    }

    if (!isVec)
        cnt += sizeof(SEXPREC);
    else
        cnt += sizeof(SEXPREC_ALIGN) + sizeof(VECREC) * getVecSizeInVEC(s);

    /* add in attributes, except for CHARSXP, where they are actually
       the links for the CHARSXP cache. */
    if(TYPEOF(s) != CHARSXP) cnt += objectsize(ATTRIB(s));

    return(cnt);
}


static SEXP do_objectsize(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarReal( (double) objectsize(CAR(args)) );
}


/* .Internal function for debugging the valgrind instrumentation code... */

volatile int R_valgrind_test_int;   /* places to store/access data */
volatile int R_valgrind_test_real;
volatile int R_valgrind_test_real2;

static SEXP do_testvalgrind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_len_t sizel = asInteger(CAR(args));

    if (sizel == NA_INTEGER) {
        REprintf(
          "Using malloc'd memory for testvalgrind (level %d)\n", 
           VALGRIND_LEVEL);
        int *p = malloc(2*sizeof(int));
        REprintf("Undefined read for 'if'\n");
        if (*p==0) R_valgrind_test_real = 987; else R_valgrind_test_real += 33;
        REprintf("OK write\n");
        *p = 7+R_valgrind_test_real2;
        REprintf("OK read for 'if'\n");
        if (*p==0) R_valgrind_test_real = 9876; else R_valgrind_test_real += 37;
        REprintf("OK write\n");
        *(p+1) = 8+R_valgrind_test_real2;
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_NOACCESS(p,2*sizeof(int));
#endif
        REprintf("Not OK write\n");
        *p = 9+R_valgrind_test_real2;
        REprintf("Not OK read\n");
        R_valgrind_test_real = *(p+1);
#if VALGRIND_LEVEL>0
        VALGRIND_MAKE_MEM_DEFINED(p+1,sizeof(int));
#endif
        REprintf("Not OK read\n");
        R_valgrind_test_real = *p;
        REprintf("OK read\n");
        R_valgrind_test_real = *(p+1);
        /* Note: p not freed */
    }
    else if (sizel<0) {
        sizel = -sizel;
        REprintf(
          "Allocating integer vector of size %d for testvalgrind (level %d)\n",
           sizel, VALGRIND_LEVEL);
        SEXP vec = allocVector(INTSXP,sizel);

        REprintf("Invalid read before start of object\n");
        R_valgrind_test_int = ((int*)vec)[-1];
        REprintf("Invalid read after end of object\n");
        R_valgrind_test_int = INTEGER(vec)[sizel];

        REprintf("Invalid read used for 'if' from beginning of vector\n");
        if (INTEGER(vec)[0]>1) R_valgrind_test_int = 123; 
        else R_valgrind_test_int += 456;
        REprintf("Invalid read used for 'if' from end of vector\n");
        if (INTEGER(vec)[sizel-1]>1) R_valgrind_test_int = 987; 
        else R_valgrind_test_int += 654;

        REprintf("Store at beginning of vector\n");
        INTEGER(vec)[0] = 1234;
        REprintf("Store at end of vector\n");
        INTEGER(vec)[sizel-1] = 5678;

        REprintf("Valid read used for 'if' from beginning of vector\n");
        if (INTEGER(vec)[0]>1) R_valgrind_test_int = 123; 
        else R_valgrind_test_int += 456;
        REprintf("Valid read used for 'if' from end of vector\n");
        if (INTEGER(vec)[sizel-1]>1) R_valgrind_test_int = 987; 
        else R_valgrind_test_int += 654;

        REprintf("Do a garbage collection\n");
        R_gc();

        REprintf("Invalid read at beginning of no-longer-existing vector\n");
        R_valgrind_test_int = INTEGER(vec)[0];
        REprintf("Invalid read at end of no-longer-existing vector\n");
        R_valgrind_test_int = INTEGER(vec)[sizel-1];
        REprintf("Done testvalgrind\n");
    }
    else {
        REprintf(
          "Allocating real vector of size %d for testvalgrind (level %d)\n",
           sizel, VALGRIND_LEVEL);
        SEXP vec = allocVector(REALSXP,sizel);

        REprintf("Invalid read before start of object\n");
        R_valgrind_test_real = ((double*)vec)[-1];
        REprintf("Invalid read after end of object\n");
        R_valgrind_test_real = REAL(vec)[sizel];

        REprintf("Invalid read used for 'if' from beginning of vector\n");
        if (REAL(vec)[0]>1) R_valgrind_test_real = 123; 
        else R_valgrind_test_real += 456;
        REprintf("Invalid read used for 'if' from end of vector\n");
        if (REAL(vec)[sizel-1]>1) R_valgrind_test_real = 987; 
        else R_valgrind_test_real += 654;

        REprintf("Store at beginning of vector\n");
        REAL(vec)[0] = 1234;
        REprintf("Store at end of vector\n");
        REAL(vec)[sizel-1] = 5678;

        REprintf("Valid read used for 'if' from beginning of vector\n");
        if (REAL(vec)[0]>1) R_valgrind_test_real = 123; 
        else R_valgrind_test_real += 456;
        REprintf("Valid read used for 'if' from end of vector\n");
        if (REAL(vec)[sizel-1]>1) R_valgrind_test_real = 987; 
        else R_valgrind_test_real += 654;

        REprintf("Do a garbage collection\n");
        R_gc();

        REprintf("Invalid read at beginning of no-longer-existing vector\n");
        R_valgrind_test_real = REAL(vec)[0];
        REprintf("Invalid read at end of no-longer-existing vector\n");
        R_valgrind_test_real = REAL(vec)[sizel-1];
        REprintf("Done testvalgrind\n");
    }

    return R_NilValue;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_memory[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"reg.finalizer",do_regFinaliz,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.limits",	do_memlimits,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"pnamedcnt",	do_pnamedcnt,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"Rprofmem",	do_Rprofmem,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"object.size",	do_objectsize,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"testvalgrind",do_testvalgrind,0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
