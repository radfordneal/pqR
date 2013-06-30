/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011  The R Development Core Team.
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

#ifndef DEFN_H_
#define DEFN_H_

/* seems unused */
#define COUNTING

#define BYTECODE

/* probably no longer needed */
#define NEW_CONDITION_HANDLING

/* To test the write barrier used by the generational collector,
   define TESTING_WRITE_BARRIER.  This makes the internal structure of
   SEXPRECs visible only inside of files that explicitly define
   USE_RINTERNALS, and all uses of SEXPREC fields that do not go
   through the appropriate functions or macros will become compilation
   errors.  Since this does impose a small but noticable performance
   penalty, code that includes Defn.h (or code that explicitly defines
   USE_RINTERNALS) can access a SEXPREC's fields directly. */

#ifndef TESTING_WRITE_BARRIER
# define USE_RINTERNALS
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_visible __attribute__ ((visibility ("default")))
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_visible
# define attribute_hidden
#endif

#ifdef __MAIN__
# define extern0 attribute_hidden
#else
# define extern0 extern
#endif

/* Define HELPERS_DISABLED if no helper support.  This has the effect of 
   making helpers.h define stubs for the helpers routines. */

#ifndef R_HELPER_THREADS
#define HELPERS_DISABLED
#endif

#define MAXELTSIZE 8192 /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */

#include <R_ext/Complex.h>
void Rf_CoercionWarning(int);/* warning code */
int Rf_LogicalFromInteger(int, int*);
int Rf_LogicalFromReal(double, int*);
int Rf_LogicalFromComplex(Rcomplex, int*);
int Rf_IntegerFromLogical(int, int*);
int Rf_IntegerFromReal(double, int*);
int Rf_IntegerFromComplex(Rcomplex, int*);
double Rf_RealFromLogical(int, int*);
double Rf_RealFromInteger(int, int*);
double Rf_RealFromComplex(Rcomplex, int*);
Rcomplex Rf_ComplexFromLogical(int, int*);
Rcomplex Rf_ComplexFromInteger(int, int*);
Rcomplex Rf_ComplexFromReal(double, int*);

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef eval
#define eval(e,rho) evalv(e,rho,0)  /* eval is a macro within interpreter */
#undef CALLED_FROM_DEFN_H

extern0 SEXP	R_CommentSymbol;    /* "comment" */
extern0 SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern0 SEXP	R_ExactSymbol;	    /* "exact" */
extern0 SEXP	R_RecursiveSymbol;  /* "recursive" */
extern0 SEXP	R_SrcfileSymbol;    /* "srcfile" */
extern0 SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern0 SEXP	R_SrcrefSymbol;     /* "srcref" */
extern0 SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern0 SEXP	R_UseNamesSymbol;   /* "use.names" */
extern0 SEXP	R_DoubleColonSymbol;   /* "::" */
extern0 SEXP	R_TripleColonSymbol;   /* ":::" */
extern0 SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern0 SEXP    R_DevicesSymbol;  /* ".Devices" */

extern0 SEXP    R_dot_Generic;  /* ".Generic" */
extern0 SEXP    R_dot_Methods;  /* ".Methods" */
extern0 SEXP    R_dot_Group;  /* ".Group" */
extern0 SEXP    R_dot_Class;  /* ".Class" */
extern0 SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern0 SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */

extern0 SEXP	R_StringHash;       /* Global hash of CHARSXPs */


 /* writable char access for R internal use only */
#define CHAR_RW(x)	((char *) CHAR(x))

/* CHARSXP charset bits */
#define BYTES_MASK (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK (1<<3)
/* (1<<4) is taken by S4_OBJECT_MASK */
#define CACHED_MASK (1<<5)
#define ASCII_MASK (1<<6)
#define HASHASH_MASK 1

/* Symbol and string hash table declarations. */
#define HASHMAXSIZE          1000000
#define HASHSIZE(x)	     LENGTH(x)
#define HASHSLOTSUSED(x)     TRUELENGTH(x)
#define HASHTABLEGROWTHRATE  1.27
#define HASHMINSIZE	     29
/* #define SET_HASHSIZE(x,v)    SETLENGTH(x,v) */
#define SET_HASHSLOTSUSED(x,v) SET_TRUELENGTH(x,v)
#define IS_HASHED(x)	     (HASHTAB(x) != R_NilValue)

#ifdef USE_RINTERNALS
# define IS_BYTES(x) ((x)->sxpinfo.gp & BYTES_MASK)
# define SET_BYTES(x) (((x)->sxpinfo.gp) |= BYTES_MASK)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define SET_LATIN1(x) (((x)->sxpinfo.gp) |= LATIN1_MASK)
# define IS_ASCII(x) ((x)->sxpinfo.gp & ASCII_MASK)
# define SET_ASCII(x) (((x)->sxpinfo.gp) |= ASCII_MASK)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define SET_UTF8(x) (((x)->sxpinfo.gp) |= UTF8_MASK)
# define ENC_KNOWN(x) ((x)->sxpinfo.gp & (LATIN1_MASK | UTF8_MASK))
# define SET_CACHED(x) (((x)->sxpinfo.gp) |= CACHED_MASK)
# define IS_CACHED(x) (((x)->sxpinfo.gp) & CACHED_MASK)
#else /* USE_RINTERNALS */
/* Needed only for write-barrier testing */
int IS_BYTES(SEXP x);
void SET_BYTES(SEXP x);
int IS_LATIN1(SEXP x);
void SET_LATIN1(SEXP x);
int IS_ASCII(SEXP x);
void SET_ASCII(SEXP x);
int IS_UTF8(SEXP x);
void SET_UTF8(SEXP x);
int ENC_KNOWN(SEXP x);
int SET_CACHED(SEXP x);
int IS_CACHED(SEXP x);
#endif /* USE_RINTERNALS */

/* macros and declarations for managing CHARSXP cache */
#define USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS
#ifdef USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS
# define CXHEAD(x) (x)
# define CXTAIL(x) ATTRIB(x)
#else
# define CXHEAD(x) CAR(x)
# define CXTAIL(x) CDR(x)
#endif /* USE_ATTRIB_FIELD_FOR_CHARSXP_CACHE_CHAINS */

#include "Internal.h"		/* do_FOO */

#include "Errormsg.h"

extern void R_ProcessEvents(void);

#ifdef R_USE_SIGNALS
#ifdef Win32
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif
#endif

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

/*  Heap and Pointer Protection Stack Sizes.  */

/* These are all required by C99 */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#if defined HAVE_DECL_SIZE_MAX && HAVE_DECL_SIZE_MAX
  typedef size_t R_size_t;
# define R_SIZE_T_MAX SIZE_MAX
#else
# error SIZE_MAX is required for C99
#endif


#define Mega 1048576. /* 1 Mega Byte := 2^20 (= 1048576) Bytes */
#define Giga 1073741824. /* 1 Giga Byte := 2^30 Bytes */

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in startup.c */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	50000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#ifndef R_VSIZE
#define	R_VSIZE		8000000L
#endif

/* some commonly needed headers */
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif


/* Maximal length in bytes of an entire path name.
   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris has 1024, Linux glibc has 4192.
   File names are limited to FILENAME_MAX bytes (usually the same as PATH_MAX)
   or NAME_MAX (often 255/256).
 */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* seems this is now defined by MinGW to be 259, whereas FILENAME_MAX
   and MAX_PATH are 260.  It is not clear that this really is in bytes,
   but might be chars for the Unicode interfaces.
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

#ifdef R_USE_SIGNALS
#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define SIGJMP_BUF jmp_buf
# define SIGSETJMP(x,s) setjmp(x)
# define SIGLONGJMP(x,i) longjmp(x,i)
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif
#endif

#define HSIZE	   4119	/* The size of the hash table for symbols */
#define MAXIDSIZE 10000	/* Largest symbol size, 
			   in bytes excluding terminator.
			   Was 256 prior to 2.13.0, now just a sanity check.
			*/

/* Types for the do_xxxx functions for SPECIAL and BUILTIN operations. */

typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);
typedef SEXP (*CCODEV)(SEXP, SEXP, SEXP, SEXP, int);  /* with variant info */

#define CALL_PRIMFUN(call,op,args,env,variant) \
  (PRIMVARIANT(op) ? PRIMFUNV(op)(call,op,args,env,variant) \
                   : PRIMFUN(op)(call,op,args,env))

/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID  =  0,
    PP_ASSIGN   =  1,
    PP_ASSIGN2  =  2,
    PP_BINARY   =  3,
    PP_BINARY2  =  4,
    PP_BREAK    =  5,
    PP_CURLY    =  6,
    PP_FOR      =  7,
    PP_FUNCALL  =  8,
    PP_FUNCTION =  9,
    PP_IF 	= 10,
    PP_NEXT 	= 11,
    PP_PAREN    = 12,
    PP_RETURN   = 13,
    PP_SUBASS   = 14,
    PP_SUBSET   = 15,
    PP_WHILE 	= 16,
    PP_UNARY 	= 17,
    PP_DOLLAR 	= 18,
    PP_FOREIGN 	= 19,
    PP_REPEAT 	= 20
} PPkind;

typedef enum {
    PREC_FN	 = 0,
    PREC_LEFT    = 1,
    PREC_EQ	 = 2,
    PREC_RIGHT	 = 3,
    PREC_TILDE	 = 4,
    PREC_OR	 = 5,
    PREC_AND	 = 6,
    PREC_NOT	 = 7,
    PREC_COMPARE = 8,
    PREC_SUM	 = 9,
    PREC_PROD	 = 10,
    PREC_PERCENT = 11,
    PREC_COLON	 = 12,
    PREC_SIGN	 = 13,
    PREC_POWER	 = 14,
    PREC_DOLLAR  = 15,
    PREC_NS	 = 16,
    PREC_SUBSET	 = 17
} PPprec;

typedef struct {
	PPkind kind; 	 /* deparse kind */
	PPprec precedence; /* operator precedence */
	unsigned int rightassoc;  /* right associative? */
} PPinfo;

/* The type definitions for the table of built-in functions. */
/* This table can be found in ../main/names.c */
typedef struct {
    char   *name;    /* print name */
    SEXP   (*cfun)();/* c-code address, function pointer */
    int	   code;     /* offset within c-code */
    int	   eval;     /* evaluate args? (and other info) */
    int	   arity;    /* function arity */
    PPinfo gram;     /* pretty-print info */
} FUNTAB;

#ifdef USE_RINTERNALS
/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Primitive Access Macros */

/* Set offset of primitive in table, and copy some of the information from
   the table into the primsxp structure (and misc) for fast access.  Note that 
   primsxp_fast_cfun will (possibly) be set by the slow function, not here. */

#define SET_PRIMOFFSET(x,v) do { \
    SEXP setprim_ptr = (x); \
    int setprim_value = (v); \
    setprim_ptr->sxpinfo.gp = setprim_value; \
    setprim_ptr->u.primsxp.primsxp_cfun = \
      (void *(*)()) R_FunTab[setprim_value].cfun; \
    setprim_ptr->u.primsxp.primsxp_fast_cfun = 0; \
    setprim_ptr->u.primsxp.primsxp_code   = R_FunTab[setprim_value].code; \
    setprim_ptr->u.primsxp.primsxp_arity  = R_FunTab[setprim_value].arity; \
    setprim_ptr->u.primsxp.primsxp_foreign \
        = R_FunTab[setprim_value].gram.kind==PP_FOREIGN; \
    setprim_ptr->u.primsxp.primsxp_print \
        = (R_FunTab[setprim_value].eval/100)%10; \
    setprim_ptr->u.primsxp.primsxp_variant \
        = (R_FunTab[setprim_value].eval/1000)&1; \
    setprim_ptr->u.primsxp.primsxp_internal \
        = (R_FunTab[setprim_value].eval/10)&1; \
    NONVEC_SXPINFO(setprim_ptr).pending_ok \
        = R_FunTab[setprim_value].eval/10000; \
} while (0)

#define PRIMOFFSET(x)	((x)->sxpinfo.gp)

#define PRIMFUN(x)	((CCODE)((x)->u.primsxp.primsxp_cfun))
#define PRIMFUNV(x)	((CCODEV)((x)->u.primsxp.primsxp_cfun))
#define SET_PRIMFUN(x,f) \
    ( (x)->u.primsxp.primsxp_cfun = \
        (void *(*)()) (R_FunTab[PRIMOFFSET(x)].cfun = (SEXP (*)()) (f)), \
      (x)->u.primsxp.primsxp_fast_cfun = 0 )
#define PRIMVAL(x)	((x)->u.primsxp.primsxp_code)
#define PRIMARITY(x)	((x)->u.primsxp.primsxp_arity)
#define PRIMPRINT(x)	((x)->u.primsxp.primsxp_print)
#define PRIMINTERNAL(x)	((x)->u.primsxp.primsxp_internal)
#define PRIMVARIANT(x)	((x)->u.primsxp.primsxp_variant)
#define PRIMFOREIGN(x)	((x)->u.primsxp.primsxp_foreign)
#define PRIMNAME(x)	(R_FunTab[PRIMOFFSET(x)].name)
#define PPINFO(x)	(R_FunTab[PRIMOFFSET(x)].gram)

#define PRIMFUN_PENDING_OK(x) (NONVEC_SXPINFO(x).pending_ok)

#define PRIMFUN_FAST(x)	((x)->u.primsxp.primsxp_fast_cfun)
#define PRIMFUN_DSPTCH1(x) ((x)->u.primsxp.primsxp_dsptch1)
#define PRIMFUN_DSPTCH2(x) ((x)->u.primsxp.primsxp_dsptch2)
#define PRIMFUN_ARG1VAR(x) (NONVEC_SXPINFO(x).var1)
#define PRIMFUN_ARG2VAR(x) (NONVEC_SXPINFO(x).var2)
#define PRIMFUN_UNI_TOO(x) ((x)->u.primsxp.primsxp_uni_too)

#define SET_PRIMFUN_FAST_UNARY(x,f,dsptch1,v1) do { \
    (x)->u.primsxp.primsxp_fast_cfun = (void *(*)()) (f); \
    (x)->u.primsxp.primsxp_dsptch1 = (dsptch1); \
    NONVEC_SXPINFO(x).var1 = (v1); \
} while (0)

#define SET_PRIMFUN_FAST_BINARY(x,f,dsptch1,dsptch2,v1,v2,uni_too) do { \
    (x)->u.primsxp.primsxp_fast_cfun = (void *(*)()) (f); \
    (x)->u.primsxp.primsxp_dsptch1 = (dsptch1); \
    (x)->u.primsxp.primsxp_dsptch2 = (dsptch2); \
    (x)->u.primsxp.primsxp_uni_too = (uni_too); \
    NONVEC_SXPINFO(x).var1 = (v1); \
    NONVEC_SXPINFO(x).var2 = (v2); \
} while (0)

/* Symbols for eval variants.  The symbols with values less than 16 may be
   OR'd with zero or more symbols with values 16, 32, 64, or 128.  The result
   will fit in one unsigned char.

   Return of a variant result is usually indicated by the attribute field 
   being R_VariantResult, but a VARIANT_NULL variant result can be just 
   R_NilValue, and results with VARIANT_PENDING_OK may be ordinary vectors
   marked as IS_BEING_COMPUTED_BY_TASK. */

#define VARIANT_NULL 1  /* May just return R_NilValue (but do side effects) */
                        /* (Should usually be OR'd with VARIANT_PENDING_OK) */

#define VARIANT_SEQ  2  /* May return a sequence spec, rather than a vector */
#define VARIANT_AND  3  /* May return AND of a logical vec rather than vec  */
#define VARIANT_OR   4  /* May return OR of a logical vec rather than vec   */
#define VARIANT_SUM  5  /* May return sum of vec elements rather than vec   */

#define VARIANT_KIND(v) (v&15) /* Isolate low four bits to compare with symbols
                                  defined above */

#define VARIANT_PENDING_OK 16  /* Computation may be deferred pending completion
                                  of a task (in a helper or the master) */

#ifdef R_HELPER_THREADS

/* Access to markers maintained with assistance of the helpers facility. */

#define IS_BEING_COMPUTED_BY_TASK(x)  ((x)->sxpinfo.being_computed)
#define IS_IN_USE_BY_TASK(x)          ((x)->sxpinfo.in_use)

/* Macros to wait until variables(s) computed. */

#ifndef HELPERS_DISABLED
#define helpers_wait_until_not_being_computed(v) \
  helpers_wait_until_not_being_computed2 ((v), (SEXP)0)
extern void helpers_wait_until_not_being_computed2 (SEXP, SEXP);
#else
#define helpers_wait_until_not_being_computed(v) 0
#define helpers_wait_until_not_being_computed2(u,v) 0
#endif

#define WAIT_UNTIL_COMPUTED(x) \
  ( ! IS_BEING_COMPUTED_BY_TASK(x) \
    ? (void) 0 \
    : helpers_wait_until_not_being_computed(x) )

#define WAIT_UNTIL_COMPUTED_2(x1,x2) \
  ( ! IS_BEING_COMPUTED_BY_TASK(x1) && ! IS_BEING_COMPUTED_BY_TASK(x2) \
    ? (void) 0 \
    : helpers_wait_until_not_being_computed2(x1,x2) ) \

#else 

#define IS_BEING_COMPUTED_BY_TASK(x) 0  /* Stub routines used when support */
#define IS_IN_USE_BY_TASK(x) 0          /*   for helpers is not enabled    */

#define WAIT_UNTIL_COMPUTED(x) 0
#define WAIT_UNTIL_COMPUTED_2(x1,x2) 0

#endif

/* Promise Access Macros */
#define PRVALUE(x) \
  (WAIT_UNTIL_COMPUTED((x)->u.promsxp.value), ((x)->u.promsxp.value))
#define PRVALUE_PENDING_OK(x) \
  ((x)->u.promsxp.value)
#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp & HASHASH_MASK)
#define HASHVALUE(x)    TRUELENGTH(x)
#define SET_HASHASH(x,v) ((v) ? (((x)->sxpinfo.gp) |= HASHASH_MASK) : \
			  (((x)->sxpinfo.gp) &= (~HASHASH_MASK)))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, v)

/* Vector Heap Structure */
typedef struct {
	union {
		SEXP		backpointer;
		double		align;
	} u;
} VECREC, *VECP;

/* Vector Heap Macros */
#define BACKPOINTER(v)	((v).u.backpointer)
#define BYTE2VEC(n)	(((n)>0)?(((n)-1)/sizeof(VECREC)+1):0)
#define INT2VEC(n)	(((n)>0)?(((n)*sizeof(int)-1)/sizeof(VECREC)+1):0)
#define FLOAT2VEC(n)	(((n)>0)?(((n)*sizeof(double)-1)/sizeof(VECREC)+1):0)
#define COMPLEX2VEC(n)	(((n)>0)?(((n)*sizeof(Rcomplex)-1)/sizeof(VECREC)+1):0)
#define PTR2VEC(n)	(((n)>0)?(((n)*sizeof(SEXP)-1)/sizeof(VECREC)+1):0)

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) ((b)->sxpinfo.gp |= BINDING_LOCK_MASK)
#define UNLOCK_BINDING(b) ((b)->sxpinfo.gp &= (~BINDING_LOCK_MASK))

#define check1arg_x(args,call) \
    do { \
        if (TAG(args) != R_NilValue && TAG(args) != R_xSymbol) \
            check1arg_error (args, call, "x"); \
    } while (0)

#else /* USE_RINTERNALS */

typedef struct VECREC *VECP;
int (PRIMOFFSET)(SEXP x);
void (SET_PRIMOFFSET)(SEXP x, int v);

#define PRIMFUN(x)	(R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x)	(R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x)	(R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x)	(R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x)	(R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x)	(((R_FunTab[PRIMOFFSET(x)].eval)/100)%10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval)%100)/10)


Rboolean (IS_ACTIVE_BINDING)(SEXP b);
Rboolean (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

#endif /* USE_RINTERNALS */

typedef SEXP R_bcstack_t;
#ifdef BC_INT_STACK
typedef union { void *p; int i; } IStackval;
#endif

#ifdef R_USE_SIGNALS
/* Stack entry for pending promises */
typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK *next;
} RPRSTACK;

/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT *nextcontext;	/* The next context up the chain */
    int cstacktop;		/* Top of the pointer protection stack */
    int evaldepth;	        /* evaluation depth at inception */
    SEXP callfun;		/* The closure called */
    void *vmax;		        /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    int callflag;		/* The context "type" */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    SEXP promargs;		/* Promises supplied to closure */
    SEXP sysparent;		/* environment the closure was called from */
    SEXP call;			/* The call that effected this context*/
    SEXP cloenv;		/* The environment */
    SEXP conexit;		/* Interpreted "on.exit" code */
    void (*cend)(void *);	/* C "on.exit" thunk */
    struct RPRSTACK *prstack;   /* stack of pending promises */
    SEXP srcref;	        /* The source line in effect */
    SEXP *nodestack;
#ifdef BC_INT_STACK
    IStackval *intstack;
#endif
    void *cenddata;		/* data for C "on.exit" thunk */
    JMP_BUF cjmpbuf;		/* C stack and register information */
} RCNTXT, *context;

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT	  = 1,
    CTXT_BREAK	  = 2,
    CTXT_LOOP	  = 3,	/* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE	  = 8,
    CTXT_RETURN	  = 12,
    CTXT_BROWSER  = 16,
    CTXT_GENERIC  = 20,
    CTXT_RESTART  = 32,
    CTXT_BUILTIN  = 64  /* used in profiling */
};

/*
TOP   0 0 0 0 0 0  = 0
NEX   1 0 0 0 0 0  = 1
BRE   0 1 0 0 0 0  = 2
LOO   1 1 0 0 0 0  = 3
FUN   0 0 1 0 0 0  = 4
CCO   0 0 0 1 0 0  = 8
BRO   0 0 0 0 1 0  = 16
RET   0 0 1 1 0 0  = 12
GEN   0 0 1 0 1 0  = 20
RES   0 0 0 0 0 0 1 = 32
BUI   0 0 0 0 0 0 0 1 = 64
*/

#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)
#endif

/* Miscellaneous Definitions */
#define streql(s, t)	(!strcmp((s), (t)))

/* Arithmetic and Relation Operators */
typedef enum {
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
} ARITHOP_TYPE;

typedef enum {
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
} RELOP_TYPE;

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1


/*--- Global Variables ---------------------------------------------------- */

/* Defined and initialized in names.c (not main.c) :*/
#ifndef __R_Names__
extern
#endif
FUNTAB	R_FunTab[];	    /* Built in functions */


#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif

LibExtern Rboolean R_interrupts_suspended INI_as(FALSE);
LibExtern int R_interrupts_pending INI_as(0);

/* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */

/* Memory Management */
extern0 R_size_t R_NSize  INI_as(R_NSIZE);/* Size of cons cell heap */
extern0 R_size_t R_VSize  INI_as(R_VSIZE);/* Size of the vector heap */
extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern0 R_size_t R_Collected;	    /* Number of free cons cells (after gc) */
LibExtern int	R_Is_Running;	    /* for Windows memory manager */

/* The Pointer Protection Stack */
extern0 int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
extern0 int	R_PPStackTop;	    /* The top of the stack */
extern0 SEXP*	R_PPStack;	    /* The pointer protection stack */

/* Evaluation Environment */
LibExtern SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */
extern0 SEXP*	R_SymbolTable;	    /* The symbol table */
#ifdef R_USE_SIGNALS
LibExtern RCNTXT R_Toplevel;	    /* Storage for the toplevel environment */
LibExtern RCNTXT* R_ToplevelContext;  /* The toplevel environment */
LibExtern RCNTXT* R_GlobalContext;    /* The global environment */
#endif
extern0 Rboolean R_Visible;	    /* Value visibility flag */
LibExtern int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser */

extern0 SEXP    R_VStack        INI_as(NULL);   /* R_alloc stack pointer */

extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
extern0 int	R_Expressions_keep INI_as(5000);	/* options(expressions) */
extern0 Rboolean R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 int	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);
extern0 uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern0 uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
extern0 int	R_CStackDir	INI_as(1);	/* C stack direction */
extern0 uintptr_t R_CStackThreshold;	/* Threshold for overflow detection */

#ifdef R_USE_SIGNALS
extern0 struct RPRSTACK *R_PendingPromises INI_as(NULL); /* Pending promise stack */
#endif

/* File Input/Output */
LibExtern Rboolean R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/
extern0 Rboolean R_Quiet	INI_as(FALSE);	/* Be as quiet as possible */
extern Rboolean  R_Slave	INI_as(FALSE);	/* Run as a slave process */
extern0 Rboolean R_Verbose	INI_as(FALSE);	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */
extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */
extern0 int	R_ErrorCon	INI_as(2);	/* Error connection */
LibExtern char *R_TempDir	INI_as(NULL);	/* Name of per-session dir */
extern0 char   *Sys_TempDir	INI_as(NULL);	/* Name of per-session dir
						   if set by R itself */
extern0 char	R_StdinEnc[31]  INI_as("");	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
extern0 int	R_ParseError	INI_as(0); /* Line where parse error occurred */
extern0 int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern0 SEXP	R_ParseErrorFile;   /* Source file where parse error was seen */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
extern0 char	R_ParseErrorMsg[PARSE_ERROR_SIZE] INI_as("");
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
extern0 char	R_ParseContext[PARSE_CONTEXT_SIZE] INI_as("");
extern0 int	R_ParseContextLast INI_as(0); /* last character in context buffer */
extern0 int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */

/* How should %*% operations be done - with C routines or BLAS routines?  
   For four kinds of operations: vec-dot, mat-vec, vec-mat, mat-mat. */
#define R_mat_mult_with_BLAS_len 4
extern0 Rboolean R_mat_mult_with_BLAS [R_mat_mult_with_BLAS_len] 
#ifdef __MAIN__
#ifdef R_MAT_MULT_WITH_BLAS_BY_DEFAULT
  = { TRUE, TRUE, TRUE, TRUE }
#else
  = { FALSE, FALSE, FALSE, FALSE }
#endif
#endif
;

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */
LibExtern int	R_HistorySize;	/* Size of the history file */
LibExtern int	R_RestoreHistory;	/* restore the history file? */
extern void 	R_setupHistory(void);

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
extern0 SEXP	R_Warnings;	    /* the warnings and their calls */
extern0 int	R_ShowErrorMessages INI_as(1);	/* show error messages? */
extern0 SEXP	R_HandlerStack;	/* Condition handler stack */
extern0 SEXP	R_RestartStack;	/* Stack of available restarts */
extern0 Rboolean R_warn_partial_match_args   INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_dollar INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_attr INI_as(FALSE);
extern0 Rboolean R_ShowWarnCalls INI_as(FALSE);
extern0 Rboolean R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

LibExtern Rboolean utf8locale  INI_as(FALSE);  /* is this a UTF-8 locale? */
LibExtern Rboolean mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */
extern0   Rboolean latin1locale INI_as(FALSE); /* is this a Latin-1 locale? */
#ifdef Win32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */
extern0   Rboolean WinUTF8out  INI_as(FALSE);  /* Use UTF-8 for output */
extern0   void WinCheckUTF8(void);
#endif

extern0 char OutDec	INI_as('.');  /* decimal point used for output */
extern0 Rboolean R_DisableNLinBrowser	INI_as(FALSE);

/* Initialization of the R environment when it is embedded */
extern int Rf_initEmbeddedR(int argc, char **argv);

/* GUI type */

extern char	*R_GUIType	INI_as("unknown");
extern Rboolean R_isForkedChild		INI_as(FALSE); /* was this forked? */

extern double cpuLimit			INI_as(-1.0);
extern double cpuLimit2			INI_as(-1.0);
extern double cpuLimitValue		INI_as(-1.0);
extern double elapsedLimit		INI_as(-1.0);
extern double elapsedLimit2		INI_as(-1.0);
extern double elapsedLimitValue		INI_as(-1.0);

void resetTimeLimits(void);

#define R_BCNODESTACKSIZE 100000
extern0 SEXP *R_BCNodeStackBase, *R_BCNodeStackTop, *R_BCNodeStackEnd;
#ifdef BC_INT_STACK
# define R_BCINTSTACKSIZE 10000
extern0 IStackval *R_BCIntStackBase, *R_BCIntStackTop, *R_BCIntStackEnd;
#endif
extern0 int R_jit_enabled INI_as(0);
extern0 int R_compile_pkgs INI_as(0);
extern SEXP R_cmpfun(SEXP);
extern void R_init_jit_enabled(void);

LibExtern int R_num_math_threads INI_as(1);
LibExtern int R_max_num_math_threads INI_as(1);

/* Pointer  type and utilities for dispatch in the methods package */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP); /* typedef */
R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef,
			SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.c, set in Init_R_Machine */
extern0 int R_dec_min_exponent		INI_as(-308);

/* structure for caching machine accuracy values */
typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;

LibExtern AccuracyInfo R_AccuracyInfo;

extern0 unsigned int max_contour_segments INI_as(25000);

extern0 Rboolean known_to_be_latin1 INI_as(FALSE);
extern0 Rboolean known_to_be_utf8 INI_as(FALSE);

#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

#define checkArity(a,b) Rf_checkArityCall(a,b,call)

/*--- FUNCTIONS ------------------------------------------------------ */

# define allocCharsxp		Rf_allocCharsxp
# define begincontext		Rf_begincontext
# define can_save_alloc		Rf_can_save_alloc
# define check_stack_balance	Rf_check_stack_balance
# define check1arg		Rf_check1arg
# define check1arg_error	Rf_check1arg_error
# define CheckFormals		Rf_CheckFormals
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyListMatrix		Rf_copyListMatrix
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define CustomPrintValue	Rf_CustomPrintValue
# define DataFrameClass		Rf_DataFrameClass
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define DispatchAnyOrEval      Rf_DispatchAnyOrEval
# define dynamicfindVar		Rf_dynamicfindVar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define endcontext		Rf_endcontext
# define envlength		Rf_envlength
# define ErrorMessage		Rf_ErrorMessage
# define evalList		Rf_evalList
# define evalListKeepMissing	Rf_evalListKeepMissing
# define evalListPendingOK	Rf_evalListPendingOK
# define factorsConform		Rf_factorsConform
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define forcePromise		Rf_forcePromise
# define forcePromisePendingOK	Rf_forcePromisePendingOK
# define FrameClassFix		Rf_FrameClassFix
# define framedepth		Rf_framedepth
# define frameSubscript		Rf_frameSubscript
# define get1index		Rf_get1index
# define getVar			Rf_getVar
# define getVarInFrame		Rf_getVarInFrame
# define InitArithmetic		Rf_InitArithmetic
# define InitColors		Rf_InitColors
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitBaseEnv		Rf_InitBaseEnv
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitTempDir		Rf_InitTempDir
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define levelsgets		Rf_levelsgets
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs		Rf_matchArgs
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkQUOTE		Rf_mkQUOTE
# define mkSYMSXP		Rf_mkSYMSXP
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define promiseArgsWithValues	Rf_promiseArgsWithValues
# define promiseArgsWith1Value	Rf_promiseArgsWith1Value
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define revisecontext          Rf_revisecontext
# define Seql			Rf_Seql
# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define StrToInternal		Rf_StrToInternal
# define strmat2intmat		Rf_strmat2intmat
# define substituteList		Rf_substituteList
# define tsConform		Rf_tsConform
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define vectorSubscript	Rf_vectorSubscript
# define wait_until_arguments_computed Rf_wait_until_arguments_computed
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
# define yychar			Rf_yychar
# define yylval			Rf_yylval
# define yynerrs		Rf_yynerrs
# define yyparse		Rf_yyparse

/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

/* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
#define CONSOLE_BUFFER_SIZE 4096
int	R_ReadConsole(const char *, unsigned char *, int, int);
void	R_WriteConsole(const char *, int); /* equivalent to R_WriteConsoleEx(a, b, 0) */
void	R_WriteConsoleEx(const char *, int, int);
void	R_ResetConsole(void);
void	R_FlushConsole(void);
void	R_ClearerrConsole(void);
void	R_Busy(int);
int	R_ShowFiles(int, const char **, const char **, const char *,
		    Rboolean, const char *);
int     R_EditFiles(int, const char **, const char **, const char *);
int	R_ChooseFile(int, char *, int);
char	*R_HomeDir(void);
Rboolean R_FileExists(const char *);
Rboolean R_HiddenFile(const char *);
double	R_FileMtime(const char *);

/* environment cell access */
typedef struct R_varloc_st *R_varloc_t;
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
Rboolean R_GetVarLocMISSING(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);

/* deparse option bits: change do_dump if more are added */

#define KEEPINTEGER 		1
#define QUOTEEXPRESSIONS 	2
#define SHOWATTRIBUTES 		4
#define USESOURCE 		8
#define WARNINCOMPLETE 		16
#define DELAYPROMISES 		32
#define KEEPNA			64
#define S_COMPAT       		128
/* common combinations of the above */
#define SIMPLEDEPARSE		0
#define DEFAULTDEPARSE		65 /* KEEPINTEGER | KEEPNA, used for calls */
#define FORSOURCING		95 /* not DELAYPROMISES, used in edit.c */

/* Coercion functions */
int Rf_LogicalFromString(SEXP, int*);
int Rf_IntegerFromString(SEXP, int*);
double Rf_RealFromString(SEXP, int*);
Rcomplex Rf_ComplexFromString(SEXP, int*);
SEXP Rf_StringFromLogical(int, int*);
SEXP Rf_StringFromInteger(int, int*);
SEXP Rf_StringFromReal(double, int*);
SEXP Rf_StringFromComplex(Rcomplex, int*);
SEXP Rf_EnsureString(SEXP);

/* Other Internally Used Functions */

SEXP Rf_allocCharsxp(R_len_t);
SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
void check1arg(SEXP, SEXP, const char *);
void check1arg_error(SEXP, SEXP, const char *);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
void CheckFormals(SEXP);
void R_check_locale(void);
void check_stack_balance(SEXP op, int save);
void CleanEd(void);
SEXP can_save_alloc (SEXP, SEXP, SEXPTYPE);
void copyListMatrix(SEXP, SEXP, Rboolean);
void copyMostAttribNoTs(SEXP, SEXP);
void CustomPrintValue(SEXP, SEXP);
void DataFrameClass(SEXP);
SEXP ddfindVar(SEXP, SEXP);
SEXP deparse1(SEXP,Rboolean,int);
SEXP deparse1line(SEXP,Rboolean);
SEXP deparse1s(SEXP call);
int DispatchAnyOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int DispatchGroup(const char *, SEXP,SEXP,SEXP,SEXP,SEXP*);
SEXP duplicated(SEXP, Rboolean);
SEXP duplicated3(SEXP, SEXP, Rboolean);
int any_duplicated(SEXP, Rboolean);
int any_duplicated3(SEXP, SEXP, Rboolean);
int envlength(SEXP);
SEXP evalList(SEXP, SEXP, SEXP);
SEXP evalListKeepMissing(SEXP, SEXP);
SEXP evalListPendingOK(SEXP, SEXP, SEXP);
int factorsConform(SEXP, SEXP);
void findcontext(int, SEXP, SEXP);
SEXP findVar1(SEXP, SEXP, SEXPTYPE, int);
SEXP forcePromise(SEXP);
SEXP forcePromisePendingOK(SEXP);
void FrameClassFix(SEXP);
SEXP frameSubscript(int, SEXP, SEXP);
int get1index(SEXP, SEXP, int, int, int, SEXP);
SEXP getVar(SEXP, SEXP);
SEXP getVarInFrame(SEXP, SEXP);
void InitArithmetic(void);
void InitColors(void);
void InitConnections(void);
void InitEd(void);
void InitFunctionHashing(void);
void InitBaseEnv(void);
void InitGlobalEnv(void);
Rboolean R_current_trace_state(void);
Rboolean R_has_methods(SEXP);
void R_InitialData(void);
SEXP R_possible_dispatch(SEXP, SEXP, SEXP, SEXP, Rboolean);
void InitGraphics(void);
void InitMemory(void);
void InitNames(void);
void InitOptions(void);
void InitStringHash(void);
void Init_R_Variables(SEXP);
void InitTempDir(void);
void initStack(void);
void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
Rboolean isMethodsDispatchOn(void);
int isValidName(const char *);
void jump_to_toplevel(void);
void KillAllDevices(void);
SEXP levelsgets(SEXP, SEXP);
void mainloop(void);
SEXP makeSubscript(SEXP, SEXP, int *, SEXP, int);
SEXP markKnown(const char *, SEXP);
SEXP mat2indsub(SEXP, SEXP, SEXP);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgExact(SEXP, SEXP*);
SEXP matchArgs(SEXP, char **, int, SEXP, SEXP);
SEXP matchPar(const char *, SEXP*);
void memtrace_report(void *, void *);
SEXP mkCLOSXP(SEXP, SEXP, SEXP);
SEXP mkFalse(void);
SEXP mkPRIMSXP (int, int);
SEXP mkPROMISE(SEXP, SEXP);
SEXP mkQUOTE(SEXP);
SEXP mkSYMSXP(SEXP, SEXP);
SEXP mkTrue(void);
SEXP NewEnvironment(SEXP, SEXP, SEXP);
void onintr(void);
RETSIGTYPE onsigusr1(int);
RETSIGTYPE onsigusr2(int);
int OneIndex(SEXP, SEXP, int, int, SEXP*, int, SEXP);
SEXP parse(FILE*, int);
void PrintDefaults(void);
void PrintGreeting(void);
void PrintValueEnv(SEXP, SEXP);
void PrintValueRec(SEXP, SEXP);
void PrintVersion(char *);
void PrintVersion_part_1(char *);
void PrintVersionString(char *);
void PrintWarnings(void);
void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);
SEXP promiseArgs(SEXP, SEXP);
SEXP promiseArgsWithValues(SEXP, SEXP, SEXP);
SEXP promiseArgsWith1Value(SEXP, SEXP, SEXP);
void Rcons_vprintf(const char *, va_list);
SEXP R_data_class(SEXP , Rboolean);
SEXP R_data_class2(SEXP);
char *R_LibraryFileName(const char *, char *, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, SEXP);
extern int R_Newhashpjw(const char *);
FILE* R_OpenLibraryFile(const char *);
SEXP R_Primitive(const char *);
void R_RestoreGlobalEnv(void);
void R_RestoreGlobalEnvFromFile(const char *, Rboolean);
void R_SaveGlobalEnv(void);
void R_SaveGlobalEnvToFile(const char *);
void R_SaveToFile(SEXP, FILE*, int);
void R_SaveToFileV(SEXP, FILE*, int, int);
Rboolean R_seemsOldStyleS4Object(SEXP object);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
void R_Suicide(const char *);
void R_getProcTime(double *data);
int R_isMissing(SEXP symbol, SEXP rho);
void sortVector(SEXP, Rboolean);
void SrcrefPrompt(const char *, SEXP);
void ssort(SEXP*,int);
int StrToInternal(const char *);
SEXP strmat2intmat(SEXP, SEXP, SEXP);
SEXP substituteList(SEXP, SEXP);
Rboolean tsConform(SEXP,SEXP);
SEXP tspgets(SEXP, SEXP);
SEXP type2symbol(SEXPTYPE);
void unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
SEXP R_LookupMethod(SEXP, SEXP, SEXP, SEXP);
int usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, int, SEXP*);
SEXP vectorIndex(SEXP, SEXP, int, int, int, SEXP);
SEXP Rf_vectorSubscript(int, SEXP, int*, SEXP (*)(SEXP,SEXP),
                        SEXP (*)(SEXP, int), SEXP, SEXP);
void Rf_wait_until_arguments_computed (SEXP);

#ifdef R_USE_SIGNALS
void begincontext(RCNTXT*, int, SEXP, SEXP, SEXP, SEXP, SEXP);
void revisecontext(SEXP, SEXP);
SEXP dynamicfindVar(SEXP, RCNTXT*);
void endcontext(RCNTXT*);
int framedepth(RCNTXT*);
void R_InsertRestartHandlers(RCNTXT *, Rboolean);
void R_JumpToContext(RCNTXT *, int, SEXP);
SEXP R_syscall(int,RCNTXT*);
int R_sysparent(int,RCNTXT*);
SEXP R_sysframe(int,RCNTXT*);
SEXP R_sysfunction(int,RCNTXT*);

void R_run_onexits(RCNTXT *);
void R_restore_globals(RCNTXT *);
#endif

/* ../main/bind.c */
SEXP ItemName(SEXP, int);

/* ../main/errors.c : */
void ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, R_WARNING, ...);
SEXP R_GetTraceback(int);

R_size_t R_GetMaxVSize(void);
void R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
void R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char *p, int *ierr);
void R_SetPPSize(R_size_t);

/* ../main/devices.c, used in memory.c, gnuwin32/extra.c */
#define R_MaxDevices 64

/* ../../main/printutils.c : */
typedef enum {
    Rprt_adj_left = 0,
    Rprt_adj_right = 1,
    Rprt_adj_centre = 2,
    Rprt_adj_none = 3
} Rprt_adj;

int	Rstrlen(SEXP, int);
const char *EncodeRaw(Rbyte);
const char *EncodeString(SEXP, int, int, Rprt_adj);
const char *EncodeReal2(double, int, int, int);


/* main/sort.c */
void orderVector1(int *indx, int n, SEXP key, Rboolean nalast,
		  Rboolean decreasing, SEXP rho);

/* main/subset.c */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP, SEXP);

/* main/subassign.c */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);

#include <wchar.h>

/* main/util.c */
void UNIMPLEMENTED_TYPE(const char *s, SEXP x);
void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
Rboolean Rf_strIsASCII(const char *str);
int utf8clen(char c);

typedef unsigned short ucs2_t;
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(ucs2_t *in, char *out);
size_t ucs2Mblen(ucs2_t *in); */
size_t utf8toucs(wchar_t *wc, const char *s);
size_t utf8towcs(wchar_t *wc, const char *s, size_t n);
size_t ucstomb(char *s, const unsigned int wc);
size_t ucstoutf8(char *s, const unsigned int wc);
size_t mbtoucs(unsigned int *wc, const char *s, size_t n);
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n);

const wchar_t *wtransChar(SEXP x); /* from sysutils.c */

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
Rboolean mbcsValid(const char *str);
Rboolean utf8Valid(const char *str);
char *Rf_strchr(const char *s, int c);
char *Rf_strrchr(const char *s, int c);

#ifdef Win32
void R_fixslash(char *s);
void R_fixbackslash(char *s);
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand);

#if defined(SUPPORT_UTF8_WIN32)
#define mbrtowc(a,b,c,d) Rmbrtowc(a,b)
#define wcrtomb(a,b,c) Rwcrtomb(a,b)
#define mbstowcs(a,b,c) Rmbstowcs(a,b,c)
#define wcstombs(a,b,c) Rwcstombs(a,b,c)
size_t Rmbrtowc(wchar_t *wc, const char *s);
size_t Rwcrtomb(char *s, const wchar_t wc);
size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n);
size_t Rwcstombs(char *s, const wchar_t *wc, size_t n);
#endif
#endif

FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand);
int Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA);
double R_strtod(const char *str, char **endptr);
double R_atof(const char *str);

/* unix/sys-std.c, main/options.c */
void set_rl_word_breaks(const char *str);

/* From localecharset.c */
extern char *locale2charset(const char *);

/* Localization */

#ifdef ENABLE_NLS
#include <libintl.h>
#ifdef Win32
#define _(String) libintl_gettext (String)
#undef gettext /* needed for graphapp */
#else
#define _(String) gettext (String)
#endif
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)
#else /* not NLS */
#define _(String) (String)
#define N_(String) String
#define ngettext(String, StringP, N) (N > 1 ? StringP: String)
#endif


/* Macros for suspending interrupts: also in GraphicsDevice.h */
#define BEGIN_SUSPEND_INTERRUPTS do { \
    Rboolean __oldsusp__ = R_interrupts_suspended; \
    R_interrupts_suspended = TRUE;
#define END_SUSPEND_INTERRUPTS R_interrupts_suspended = __oldsusp__; \
    if (R_interrupts_pending && ! R_interrupts_suspended) \
        onintr(); \
} while(0)


/* Macros for fast vmaxget and vmaxset */

#define VMAXGET() ((void *) R_VStack)
#define VMAXSET(ovmax) (R_VStack = (SEXP) (ovmax))


#ifdef __GNUC__
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
# if !HAVE_DECL_ALLOCA
extern void *alloca(size_t);
# endif
#endif


/* Macro for fast stack checking */

#define R_CHECKSTACK() do { \
    int dummy; \
    if (R_CStackDir > 0 ? (uintptr_t)&dummy < R_CStackThreshold \
                        : (uintptr_t)&dummy > R_CStackThreshold) \
        R_CheckStack(); \
} while (0)


/* Enable this by defining USE_FAST_PROTECT_MACROS before including Defn.h.

   Redefines PROTECT, UNPROTECT, PROTECT_WITH_INDEX, and REPROTECT for speed,
   as is possible because the required variables are defined above.  The
   macros below call procedure in memory.c for error handling.  PROTECT_PTR is 
   not redefined, since it contains a significant amount of code.

   Macros PROTECT2 and PROTECT3 for protecting 2 or 3 objects are also defined.

   Defining USE_FAST_PROTECT_MACROS in source files outside src/main may
   cause problems at link time. */

#ifdef USE_FAST_PROTECT_MACROS

extern void Rf_protect_error (void);
extern void Rf_unprotect_error (void);

#undef  PROTECT
#define PROTECT(s) \
( (R_PPStackTop >= R_PPStackSize ? Rf_protect_error() : (void)0), \
   R_PPStack[R_PPStackTop++] = (s) )

#undef  PROTECT2
#define PROTECT2(s1,s2) \
( (R_PPStackTop+1 >= R_PPStackSize ? Rf_protect_error() : (void)0), \
   R_PPStack[R_PPStackTop++] = (s1), \
   R_PPStack[R_PPStackTop++] = (s2) )

#undef  PROTECT3
#define PROTECT3(s1,s2,s3) \
( (R_PPStackTop+2 >= R_PPStackSize ? Rf_protect_error() : (void)0), \
   R_PPStack[R_PPStackTop++] = (s1), \
   R_PPStack[R_PPStackTop++] = (s2), \
   R_PPStack[R_PPStackTop++] = (s3) )

#undef  UNPROTECT
#define UNPROTECT(n) \
  ( R_PPStackTop >= (n) ? (void) (R_PPStackTop -= (n)) \
                        : Rf_unprotect_error() )

#undef  PROTECT_WITH_INDEX
#define PROTECT_WITH_INDEX(x,i) \
  ( (*(i) = R_PPStackTop), PROTECT(x) )

#undef  REPROTECT
#define REPROTECT(x,i) \
  ( (void) (R_PPStack[i] = x) )

#endif

#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
