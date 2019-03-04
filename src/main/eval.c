/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	The R Core Team.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Rinterface.h>
#include <Fileio.h>

#include "scalar-stack.h"
#include <Rmath.h>
#include "arithmetic.h"

#include <helpers/helpers-app.h>


#ifndef SCALAR_STACK_DEBUG    /* can be overridden by compiler option */
#define SCALAR_STACK_DEBUG 0
#endif


/* -------------------------------------------------------------------------- */
/*                CONTEXT PROCEDURES - others are in context.c                */

void beginbuiltincontext (RCNTXT * cptr, SEXP syscall)
{ begincontext (cptr, CTXT_BUILTIN, syscall, R_BaseEnv, 
                      R_BaseEnv, R_NilValue, R_NilValue);
}

/* begincontext - begin an execution context

   begincontext and endcontext are used in dataentry.c and modules. */

void begincontext(RCNTXT * cptr, int flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun)
{
    cptr->nextcontext = R_GlobalContext;  /* store in order in structure, */
    cptr->callflag = flags;               /*   since that may be faster   */
    cptr->cstacktop = R_PPStackTop;
    cptr->evaldepth = R_EvalDepth;
    cptr->promargs = promargs;
    cptr->callfun = callfun;
    cptr->sysparent = sysp;
    cptr->call = syscall;
    cptr->cloenv = env;
    cptr->conexit = R_NilValue;
    cptr->cend = NULL;
    cptr->vmax = VMAXGET();
    cptr->intsusp = R_interrupts_suspended;
    cptr->handlerstack = R_HandlerStack;
    cptr->restartstack = R_RestartStack;
    cptr->prstack = R_PendingPromises;
    cptr->nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    cptr->intstack = R_BCIntStackTop;
#endif
    cptr->srcref = R_Srcref;
    cptr->local_pr = R_local_protect_start;
    cptr->scalar_stack = R_scalar_stack;
    R_GlobalContext = cptr;
}


/* endcontext - end an execution context. */

static void attribute_noinline endcontext_onexit (RCNTXT *cptr)
{
    if (cptr->cloenv == R_NilValue)
        return;

    SEXP s = cptr->conexit;
    Rboolean savevis = R_Visible;
    cptr->conexit = R_NilValue; /* prevent recursion */
    PROTECT(s);
    eval(s, cptr->cloenv);
    UNPROTECT(1);
    R_Visible = savevis;
}

void endcontext(RCNTXT *cptr)
{
    R_HandlerStack = cptr->handlerstack;
    R_RestartStack = cptr->restartstack;
    if (cptr->conexit != R_NilValue)
        endcontext_onexit(cptr);  /* not inline, since not the common case */
    R_GlobalContext = cptr->nextcontext;
}


/* revisecontext - change environments in a context

   The revised context differs from the previous one only in env and sysp. */

static inline void revisecontext (SEXP env, SEXP sysp)
{
    R_GlobalContext->sysparent = sysp;
    R_GlobalContext->cloenv = env;
}


/* jumpfun - jump to the named context */

static R_NORETURN void jumpfun(RCNTXT * cptr, int mask, SEXP val)
{
    Rboolean savevis = R_Visible;

    if (ON_SCALAR_STACK(val))
        val = DUP_STACK_VALUE(val);

    /* run onexit/cend code for all contexts down to but not including
       the jump target */
    PROTECT(val);
    R_run_onexits(cptr);
    UNPROTECT(1);
    R_Visible = savevis;

    R_ReturnedValue = val;
    R_GlobalContext = cptr; /* this used to be set to
			       cptr->nextcontext for non-toplevel
			       jumps (with the context set back at the
			       SETJMP for restarts).  Changing this to
			       always using cptr as the new global
			       context should simplify some code and
			       perhaps allow loops to be handled with
			       fewer SETJMP's.  LT */
    R_restore_globals(R_GlobalContext);

    LONGJMP(cptr->cjmpbuf, mask);
}


/* findcontext - find the correct context */

void R_NORETURN attribute_hidden findcontext(int mask, SEXP env, SEXP val)
{
    RCNTXT *cptr;
    if (mask & CTXT_LOOP) {		/* break/next */
	for (cptr = R_GlobalContext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if ((cptr->callflag & CTXT_LOOP) && cptr->cloenv == env )
		jumpfun(cptr, mask, val);
	error(_("no loop for break/next, jumping to top level"));
    }
    else {				/* return; or browser */
	for (cptr = R_GlobalContext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if ((cptr->callflag & mask) && cptr->cloenv == env)
		jumpfun(cptr, mask, val);
	error(_("no function to return from, jumping to top level"));
    }
}

void R_NORETURN attribute_hidden R_JumpToContext (RCNTXT *target, int mask, 
                                                  SEXP val)
{
    RCNTXT *cptr;
    for (cptr = R_GlobalContext;
	 cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	 cptr = cptr->nextcontext)
	if (cptr == target)
	    jumpfun(cptr, mask, val);
    error(_("target context is not on the stack"));
}


/* -------------------------------------------------------------------------- */
/*              CORE EVAL PROCEDURES - KEEP TOGETHER FOR LOCALITY             */


/* Similar version used in bytecode.c */

extern void Rf_asLogicalNoNA_warning(SEXP s, SEXP call);
extern R_NORETURN void Rf_asLogicalNoNA_error(SEXP s, SEXP call);

/* Caller needn't protect the s arg below.  Value is popped off the
   scalar stack if it's there. */

static inline Rboolean asLogicalNoNA(SEXP s, SEXP call)
{
    int len, cond;

    /* Check the constants explicitly, since they should be the most
       common cases, and then no need to check for NA, or pop scalar stack. */

    if (s == R_ScalarLogicalTRUE)
        return TRUE;
    if (s == R_ScalarLogicalFALSE)
        return FALSE;

    switch(TYPEOF(s)) { /* common cases done here for efficiency */
    case INTSXP:  /* assume logical and integer are the same */
    case LGLSXP:
        len = LENGTH(s);
        if (len == 0) goto error;
        cond = LOGICAL(s)[0];
        break;
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
        len = LENGTH(s);
        if (len == 0) goto error;
        cond = asLogical(s);
        break;
    default:
        goto error;
    }

    if (cond == NA_LOGICAL) goto error;

    POP_IF_TOP_OF_STACK(s);

    if (len > 1) Rf_asLogicalNoNA_warning (s, call);

    return cond;

  error:
    Rf_asLogicalNoNA_error (s, call);
}


/* Inline version of findVarPendingOK, for speed when symbol is found
   from LASTSYMBINDING. */

static inline SEXP FIND_VAR_PENDING_OK (SEXP sym, SEXP rho)
{
    rho = SKIP_USING_SYMBITS (rho, sym);

    if (LASTSYMENV(sym) == SEXP32_FROM_SEXP(rho)) {
        SEXP b = CAR(LASTSYMBINDING(sym));
        if (b != R_UnboundValue) {
            R_binding_cell = LASTSYMBINDING(sym);
            return b;
        }
        LASTSYMENV(sym) = R_NoObject32;
    }

    return findVarPendingOK(sym,rho);
}


/* Inline version of findFun.  It's meant to be very fast when a
   function is found in the base environmet.  It can delegate uncommon
   cases such as traced functions to the general-case procedure.

   Note that primitive functions in the base environment are directly
   stored as the bound values, while closures (including internals)
   are referenced via promises (for lazy loading). */

static inline SEXP FINDFUN (SEXP symbol, SEXP rho)
{
    rho = SKIP_USING_SYMBITS (rho, symbol);

    if (rho == R_GlobalEnv && BASE_CACHE(symbol) || IS_BASE(rho)) {
        SEXP res = SYMVALUE(symbol);
        int type_etc = TYPE_ETC(res);
        if (type_etc == SPECIALSXP || type_etc == BUILTINSXP)
            return res;
        if (type_etc == PROMSXP + TYPE_ET_CETERA_VEC_DOTS_TR) { /* forced prom*/
            res = PRVALUE_PENDING_OK(res);
            type_etc = TYPE_ETC(res);
        }
        if (type_etc == CLOSXP)
            return res;
    }

    return findFun_nospecsym(symbol,rho);
}


#define CHECK_STACK_BALANCE(o,s) do { \
  if (s != R_PPStackTop) check_stack_balance(o,s); \
} while (0)


/* Wait until no value in an argument list is still being computed by a task.
   Macro version does preliminary check in-line for speed. */

#define WAIT_UNTIL_ARGUMENTS_COMPUTED(_args_) \
    do { \
        if (helpers_tasks > 0) { \
            SEXP _a_ = (_args_); \
            while (_a_ != R_NilValue) { \
                if (helpers_is_being_computed(CAR(_a_))) { \
                    wait_until_arguments_computed (_a_); \
                    break; \
                } \
                _a_ = CDR(_a_); \
            } \
        } \
    } while (0)

void attribute_hidden wait_until_arguments_computed (SEXP args)
{
    SEXP wait_for, a;

    if (helpers_tasks == 0) return;

    wait_for = R_NoObject;

    for (a = args; a != R_NilValue; a = CDR(a)) {
        SEXP this_arg = CAR(a);
        if (helpers_is_being_computed(this_arg)) {
            if (wait_for == R_NoObject)
                wait_for = this_arg;
            else {
                helpers_wait_until_not_being_computed2 (wait_for, this_arg);
                wait_for = R_NoObject;
            }
        }
    }

    if (wait_for != R_NoObject)
        helpers_wait_until_not_being_computed (wait_for);
}


/* Fast eval macros.  Do not set R_Visible properly, so should not be
   used if that is needed.  Do not check evaluation count, so should
   not be used if a loop without such a check might result.  Do not
   check expression depth or stack overflow, so should not be used if
   infinite recursion could result.  EVALV_NC is meant for use in
   contexts where a self-evaluating constant is not likely. */

static SEXP attribute_noinline evalv_sym   (SEXP, SEXP, int);
static SEXP attribute_noinline evalv_other (SEXP, SEXP, int);

#define EVALV(e, rho, variant) ( \
    R_variant_result = 0, \
    SELF_EVAL(TYPEOF(e)) ? \
       (NAMEDCNT_EQ_MAX(e) ? e \
         : (UPTR_FROM_SEXP(e)->sxpinfo.nmcnt |= MAX_NAMEDCNT, e)) \
    : TYPE_ETC(e) == SYMSXP /* not ..., ..1, etc */ ? \
       evalv_sym (e, rho, variant) \
    :  evalv_other (e, rho, variant) \
)

#define EVALV_NC(e, rho, variant) ( \
    R_variant_result = 0, \
    TYPE_ETC(e) == SYMSXP /* not ..., ..1, etc */ ? \
       evalv_sym (e, rho, variant) \
    :  evalv_other (e, rho, variant) \
)


/* The "evalv" function returns the value of "e" evaluated in "rho",
   with given variant.  The caller must ensure that both SEXP
   arguments are protected.  The "eval" function is just like "evalv"
   with 0 for the variant return argument. */

static SEXP attribute_noinline forcePromiseUnbound (SEXP e, int variant);
static SEXP attribute_noinline Rf_builtin_op_no_cntxt (SEXP op, SEXP e, 
                                                       SEXP rho, int variant);
SEXP attribute_hidden Rf_builtin_op (SEXP op, SEXP e, 
                                     SEXP rho, int variant);

#define evalcount R_high_frequency_globals.evalcount

SEXP eval (SEXP e, SEXP rho)
{
    return evalv (e, rho, 0);
}

SEXP evalv (SEXP e, SEXP rho, int variant)
{
    SEXP res;

    R_variant_result = 0;
    R_Visible = TRUE;
    
    /* Handle check for user interrupt. */

    if (--evalcount < 0) {
        R_CheckUserInterrupt();
        evalcount = 1000;
    }

    /* Quick return for self-evaluating constants. */

    if (SELF_EVAL(TYPEOF(e))) {
        SET_NAMEDCNT_MAX(e);
        return e;
    }

    /* Handle symbol lookup without stack overflow or expression depth check */

    if (TYPE_ETC(e) == SYMSXP /* symbol, but not ..., ..1, etc */) {
        res = evalv_sym (e, rho, variant);  /* may change R_Visible, but
                                               that seems to be desired... */
        return res;
    }

    /* Handle evaluations of other things (mostly language objects). */

    R_CHECKSTACK();  /* Check for stack overflow. */

    R_EvalDepth += 1;

    if (R_EvalDepth > R_Expressions)
        too_deep_error();

    res =  evalv_other (e, rho, variant);

    R_EvalDepth -= 1;

#   ifdef ENABLE_EVAL_DEBUG
    {
        sggc_cptr_t cptr = CPTR_FROM_SEXP(res);
        sggc_check_valid_cptr (cptr);
        if (SEXP_FROM_CPTR(cptr) != res) abort();
        if (res != R_NilValue && TYPEOF(res) == NILSXP) abort();
        if (TYPEOF(res) == FREESXP) abort();

#       ifdef ENABLE_SGGC_DEBUG
            if (sggc_trace_cptr_in_use) {
                sggc_check_valid_cptr (sggc_trace_cptr);
                SEXP trp = SEXP_FROM_CPTR (sggc_trace_cptr);
                if (trp != R_NilValue && TYPEOF(trp) == NILSXP) abort();
                if (TYPEOF(trp) == FREESXP) abort();
            }
#       endif
    }
#   endif

    return res;
}


static inline SEXP handle_symbol (SEXP res, SEXP sym, int variant)
{
    SEXP cell = R_binding_cell;

    if (TYPEOF(res) == PROMSXP) {
        cell = res;
        if (PRVALUE_PENDING_OK(res) == R_UnboundValue)
            res = forcePromiseUnbound(res,variant);
	else
	    res = PRVALUE_PENDING_OK(res);
    }
    else if (res == R_MissingArg) {
        if ( ! (variant & VARIANT_MISSING_OK))
            if (!DDVAL(sym))  /* revert bug fix for the moment */
		arg_missing_error(sym);
        return res;
    }

    /* A NAMEDCNT of 0 might arise from an inadverently missing increment
       somewhere, or from a save/load sequence (since loaded values in
       promises have NAMEDCNT of 0), so fix up here... */

    SET_NAMEDCNT_NOT_0(res);

    if (variant & (VARIANT_PENDING_OK | VARIANT_GRADIENT)) {
        if ( ! (variant & VARIANT_GRADIENT))
            return res;
        if (HAS_GRADIENT_IN_CELL(cell) 
             && (TYPEOF(cell) == LISTSXP || STORE_GRAD(cell))) {
            R_variant_result = VARIANT_GRADIENT_FLAG;
            R_gradient = GRADIENT_IN_CELL(cell);
        }
        if (variant & VARIANT_PENDING_OK)
            return res;
    }

    WAIT_UNTIL_COMPUTED(res);

    return res;
}


/* Evaluate an expression that is a symbol other than ..., ..1, ..2, etc. */

static SEXP attribute_noinline evalv_sym (SEXP sym, SEXP rho, int variant)
{
    SEXP res;

    SEXP32 lastsymenv = LASTSYMENV(sym);

    if (lastsymenv != SEXP32_FROM_SEXP(rho))
        goto slow_lookup;

  local:

    res = CAR(LASTSYMBINDING(sym));
    if (res == R_UnboundValue) {
	LASTSYMENV(sym) = lastsymenv = R_NoObject32;
        goto slow_lookup;
    }
    R_binding_cell = LASTSYMBINDING(sym);

  found:

    return handle_symbol (res, sym, variant);

  slow_lookup:

    rho = SKIP_USING_SYMBITS (rho, sym);
    if (lastsymenv == SEXP32_FROM_SEXP(rho))
        goto local;

    res = findVarPendingOK(sym,rho);
    if (res != R_UnboundValue)
        goto found;

    unbound_var_error(sym);
}


/* Evaluate an expression that is not a symbol (other than ..., ..1, ..2, etc.)
   such as language objects, promises, and self-evaluating expressions. 
   (Most often called with language objects.) */

static SEXP attribute_noinline evalv_other (SEXP e, SEXP rho, int variant)
{
    SEXP res;

    if (TYPE_ETC(e) == LANGSXP) {  /* parts other than type will be 0 */

        SEXP op;

#       if SCALAR_STACK_DEBUG
            SEXP sv_scalar_stack = R_scalar_stack;
#       endif

        SEXP fn = CAR(e);

        if (TYPE_ETC(fn) == SYMSXP)  /* symbol, and not ..., ..1, ..2, etc. */
            op = FINDFUN(fn,rho);
        else {
            op = eval(fn,rho);
            R_variant_result = 0;
        }

        int type_etc = TYPE_ETC(op);
        SEXP args = CDR(e);

      redo:  /* comes back here for traced functions, after clearing flag */

        if (type_etc == CLOSXP) {
            PROTECT(op);
            res = applyClosure_v (e, op, promiseArgs(args,rho,variant), 
                                  rho, NULL, variant);
            UNPROTECT(1);
        }
        else {
            int save = R_PPStackTop;
            const void *vmax = VMAXGET();

#           ifdef Win32
                /* Reset precision, rounding & exception modes of an ix86 fpu */
                __asm__ ( "fninit" );
#           endif

            /* Note: If called from evalv, R_Visible will've been set to TRUE */
            if (type_etc == SPECIALSXP) {
                /* Note:  Special primitives always take variant argument,
                   and are responsible for setting R_Visible as desired 
                   themselves, with default of TRUE. */
                res = PRIMFUNV(op) (e, op, args, rho, variant);
            }
            else if (type_etc == BUILTINSXP) {
                res = R_Profiling ? Rf_builtin_op(op, e, rho, variant)
                                  : Rf_builtin_op_no_cntxt(op, e, rho, variant);
                if (PRIMVISON(op))
                    R_Visible = TRUE;
            }
            else if ((type_etc & TYPE_ET_CETERA_VEC_DOTS_TR) 
                       && isFunction(op)) {
                PROTECT(op);
                R_trace_call(e,op);
                UNPROTECT(1);
                type_etc &= ~TYPE_ET_CETERA_VEC_DOTS_TR;
                goto redo;
            }
            else
                apply_non_function_error();

            CHECK_STACK_BALANCE(op, save);
            VMAXSET(vmax);
        }

#       if SCALAR_STACK_DEBUG
            if (variant & VARIANT_SCALAR_STACK_OK) {
                if (ON_SCALAR_STACK(res)) {
                    if (res != sv_scalar_stack) abort();
                    if (res != SCALAR_STACK_OFFSET(1)) abort();
                }
                else {
                    if (R_scalar_stack != sv_scalar_stack) abort();
                }
            }
            else {
                if (ON_SCALAR_STACK(res)) abort();
                if (R_scalar_stack != sv_scalar_stack) abort();
            }
#       endif

#       if SCALAR_STACK_DEBUG /* to get debug output, type SCALAR.STACK.DEBUG */
            if (installed_already("SCALAR.STACK.DEBUG") != R_NoObject) {
                if (ON_SCALAR_STACK(res)) {
                    REprintf(
                     "SCALAR STACK VALUE RETURNED: %llx %llx %llx %s %f\n",
                     (long long) R_scalar_stack_start,
                     (long long) res, 
                     (long long) R_scalar_stack,
                     TYPEOF(res)==INTSXP ? "int" : "real",
                     TYPEOF(res)==INTSXP ? (double)*INTEGER(res) : *REAL(res));
                }
#               if 0
                    REprintf("STACK:\n");
                    for (int i = 0; i < 6; i++) {
                        if (SCALAR_STACK_ENTRY(i)==R_scalar_stack)
                            REprintf("@@\n");
                        R_inspect(SCALAR_STACK_ENTRY(i));
                    }
                    REprintf("END\n");
#               endif
            }
#       endif

        return res;
    }

    if (TYPE_ETC(e) == PROMSXP + TYPE_ET_CETERA_VEC_DOTS_TR) {
        /* forced promise, no gradient */
        res = PRVALUE_PENDING_OK(e);
        if (variant & VARIANT_PENDING_OK)
            return res;
        goto wait_and_return;
    }

    if (TYPE_ETC(e) == PROMSXP) {
        /* unforced promise, force here */
        res = forcePromiseUnbound(e,variant);
        R_Visible = TRUE;
        if (variant & (VARIANT_PENDING_OK | VARIANT_GRADIENT)) {
            if ( ! (variant & VARIANT_GRADIENT))
                return res;
            if (HAS_GRADIENT_IN_CELL(e)) {
                R_variant_result = VARIANT_GRADIENT_FLAG;
                R_gradient = GRADIENT_IN_CELL(e);
            }
            if (variant & VARIANT_PENDING_OK)
                return res;
        }
        goto wait_and_return;
    }

    if (SELF_EVAL(TYPEOF(e))) {
        SET_NAMEDCNT_MAX(e);
        return e;
    }
 
    if (TYPE_ETC(e) == 
           PROMSXP + TYPE_ET_CETERA_VEC_DOTS_TR + TYPE_ET_CETERA_HAS_ATTR) {
        /* forced promise, with gradient */
        res = PRVALUE_PENDING_OK(e);
        if (variant & (VARIANT_PENDING_OK | VARIANT_GRADIENT)) {
            if ( ! (variant & VARIANT_GRADIENT))
                return res;
            R_variant_result = VARIANT_GRADIENT_FLAG;
            R_gradient = GRADIENT_IN_CELL(e);
            if (variant & VARIANT_PENDING_OK)
                return res;
        }
        goto wait_and_return;
    }

    if (TYPE_ETC(e) == SYMSXP+TYPE_ET_CETERA_VEC_DOTS_TR) /* ... or ..1, ..2 */
        return handle_symbol (ddfindVar(e,rho), e, variant);

    if (TYPE_ETC(e) == BCODESXP) {  /* parts other than type will be 0 */
        return bcEval(e, rho, TRUE);
    }

    if (TYPEOF(e) == DOTSXP)
        dotdotdot_error();
    else
        UNIMPLEMENTED_TYPE("eval", e);

  wait_and_return:
    WAIT_UNTIL_COMPUTED(res);
    return res;
}


/* e is protected here */
static SEXP attribute_noinline forcePromiseUnbound (SEXP e, int variant)
{
    SEXP val;

    val = PRCODE(e);

    if (SELF_EVAL(TYPEOF(val)) ) {

        /* Just copy code to value - avoids old-to-new check. */

        SET_PRVALUE_TO_PRCODE (e);

        SET_NAMEDCNT_MAX (val);  /* mimic what would happen if eval'd */
    }
    else {

        RPRSTACK prstack;

        if (PRSEEN(e) == 1) PRSEEN_error(e);

        /* Mark the promise as under evaluation and push it on a stack
           that can be used to unmark pending promises if a jump out
           of the evaluation occurs. */

        prstack.promise = e;
        prstack.next = R_PendingPromises;
        R_PendingPromises = &prstack;

        SET_PRSEEN (e, 1);

        PROTECT(e);

        int vrnt = variant & VARIANT_PENDING_OK;

        if (STORE_GRAD(e)) {
            vrnt |= VARIANT_MISSING_OK | VARIANT_GRADIENT;
            val = EVALV_NC (val, PRENV(e), vrnt);
            if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                SET_GRADIENT_IN_CELL (e, R_gradient);
                R_variant_result = 0;
            }
        }
        else {
            vrnt |= VARIANT_MISSING_OK;
            val = EVALV_NC (val, PRENV(e), vrnt);
        }

        /* Pop the stack, unmark the promise and set its value field. */

        R_PendingPromises = prstack.next;
        SET_PRSEEN (e, 0);

        SET_PRVALUE_MACRO (e, val);

        if (val == R_MissingArg || val == R_MissingUnder) {

            /* Attempt to mimic past behaviour... */
            if ( ! (variant & VARIANT_MISSING_OK) && TYPEOF(PRCODE(e)) == SYMSXP
                      && R_isMissing (PRCODE(e), PRENV(e)))
                arg_missing_error(PRCODE(e));

            UNPROTECT(1);
            return val;
        }

        INC_NAMEDCNT(val);
        UNPROTECT(1);
    }

    /* Set the environment to R_NilValue to allow GC to reclaim the
       promise environment (unless value is R_MissingArg or R_MissingUnder);
       this is also useful for fancy games with delayedAssign() */

    SET_PRENV_NIL(e);

    return val;
}

SEXP forcePromise (SEXP e) /* e protected here if necessary */
{
    if (PRVALUE_PENDING_OK(e) == R_UnboundValue)
        return forcePromiseUnbound(e,0);
    else
        return PRVALUE(e);
}

SEXP forcePromise_v (SEXP e, int variant) /* e protected here if necessary */
{
    if (PRVALUE_PENDING_OK(e) == R_UnboundValue)
        return forcePromiseUnbound(e,variant);
    else {
        SEXP r = PRVALUE_PENDING_OK(e);
        if (variant & (VARIANT_PENDING_OK | VARIANT_GRADIENT)) {
            if ( ! (variant & VARIANT_GRADIENT))
                return r;
            if (HAS_GRADIENT_IN_CELL(e)) {
                R_variant_result = VARIANT_GRADIENT_FLAG;
                R_gradient = GRADIENT_IN_CELL(e);
            }
            if (variant & VARIANT_PENDING_OK)
                return r;
        }
        WAIT_UNTIL_COMPUTED(r);
        return r;
    }
}


/* Like Rf_builtin_op (in builtin.c) except that no context is
   created.  Making this separate from Rf_builtin_op saves on stack
   space for the local context variable.  Since the somewhat
   time-consuming context creation is not done, there is no advantage
   to evaluating a single argument with pending OK. */

static SEXP attribute_noinline Rf_builtin_op_no_cntxt(SEXP op, SEXP e, SEXP rho,
                                                      int variant)
{
    SEXP args = CDR(e);
    SEXP arg1;
    SEXP res;

    /* See if this may be a fast primitive.  All fast primitives
       should be BUILTIN.  We do a fast call only if there is exactly
       one argument, with no tag, not missing or a ... argument; also
       must not be an object if the fast primitive dispatches, unless
       the argument was evaluated with VARIANT_UNCLASS and we got this
       variant result.  The argument is stored in arg1. */

    if (args!=R_NilValue) {
        if (PRIMFUN_FAST(op) 
              && TAG(args)==R_NilValue && CDR(args)==R_NilValue
              && (arg1 = CAR(args))!=R_DotsSymbol 
              && arg1!=R_MissingArg && arg1!=R_MissingUnder) {

            PROTECT(arg1 = EVALV (arg1, rho, PRIMFUN_ARG1VAR(op)));

            if (isObject(arg1) && PRIMFUN_DSPTCH1(op)) {
                if ((PRIMFUN_ARG1VAR (op) & VARIANT_UNCLASS)
                       && (R_variant_result & VARIANT_UNCLASS_FLAG)) {
                    R_variant_result &= ~VARIANT_UNCLASS_FLAG;
                }
                else {
                    UNPROTECT_PROTECT(args = CONS(arg1,R_NilValue));
                    goto not_fast;
                }
            }

            R_Visible = TRUE;
            res = ((SEXP(*)(SEXP,SEXP,SEXP,SEXP,int)) PRIMFUN_FAST(op)) 
                     (e, op, arg1, rho, variant);

            UNPROTECT(1); /* arg1 */
            return res;
        }

        args = evalList (args, rho);
    }

    PROTECT(args);

    /* Handle a non-fast op.  We may get here after starting to handle a
       fast op, but if so, args has been set to the evaluated argument list. */

  not_fast: 

    R_variant_result = 0;
    R_Visible = TRUE;
    res = CALL_PRIMFUN(e, op, args, rho, variant);

    UNPROTECT(1); /* args */
    return res;
}


static SEXP do_if (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Don't check arg count - missing are seen as R_NilValue, extra ignored. */

    SEXP Cond, Stmt;
    int absent_else = 0;

    Cond = CAR(args); args = CDR(args);
    Stmt = CAR(args); args = CDR(args);

    SEXP condval = EVALV_NC (Cond, rho, 
                             VARIANT_SCALAR_STACK_OK | VARIANT_ANY_ATTR);

    if ( ! asLogicalNoNA (condval, call)) {
        /* go to else part */
        if (args != R_NilValue)
            Stmt = CAR(args);
        else {
            absent_else = 1;
            Stmt = R_NilValue;
        }
    }

    if (RDEBUG(rho))
        start_browser (call, op, Stmt, rho);

    if (absent_else) {
        R_Visible = FALSE; /* case of no 'else' so return invisible NULL */
        return R_NilValue;
    }

    return evalv (Stmt, rho, VARIANT_PASS_ON(variant));
}


/* For statement.  Unevaluated arguments for different formats are as follows:

       for (i in v) body          i, v, body
       for (i down v) body        i, down=v, body
       for (i across v) body      i, across=v, body
       for (i along v) body       i, along=v, body     (ok for vec or for array)
       for (i, j along M) body    i, j, along=M, body     (requires correct dim)
       etc.

   Extra variables after i are ignored for 'in', 'down', and 'across'.

   Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK. */

static SEXP do_for (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and that might change between
       the setjmp and longjmp calls.  Theoretically this does not include
       n and some others, but gcc -O2 -Wclobbered warns about some, 
       so to be safe we declare them volatile as well. */

    volatile int i, n;
    volatile SEXP val, nval;
    volatile SEXP v, bcell;                /* for use with one 'for' variable */
    volatile SEXP indexes, ixvals, bcells; /* for use with >1 'for' variables */
    int dbg, val_type;
    SEXP a, syms, sym, body, dims;
    RCNTXT cntxt;
    PROTECT_INDEX vpi, bix;
    int is_seq, seq_start;
    int along = 0, across = 0, down = 0, in = 0;
    int nsyms;
    SEXP ret;
    SEXP s;
    int j;

#   if SCALAR_STACK_DEBUG
        SEXP sv_scalar_stack = R_scalar_stack;
#   endif

    int vrnt = VARIANT_NULL | VARIANT_PENDING_OK;
    if (variant & VARIANT_DIRECT_RETURN) 
        vrnt |= VARIANT_PASS_ON(variant);

    R_Visible = FALSE;

    /* Count how many variables there are before the argument after the "in",
       "across", "down", or "along" keyword.  Set 'a' to the cell for the 
       argument after these variables. */

    syms = args;
    nsyms = 0;
    a = args;
    do {
        if (!isSymbol(CAR(a))) errorcall(call, _("non-symbol loop variable"));
        a = CDR(a);
        nsyms += 1;
    } while (CDDR(a) != R_NilValue);

    if (TAG(a) == R_AlongSymbol)
        along = 1;
    else if (TAG(a) == R_AcrossSymbol)
        across = 1;
    else if (TAG(a) == R_DownSymbol)
        down = 1;
    else
        in = 1;  /* we treat any other (or no) tag as "in" */

    if (!along) nsyms = 1;  /* ignore extras when not 'along' */

    val = CAR(a);
    body = CADR(a);
    sym = CAR(syms);

    PROTECT2(args,rho);

    PROTECT(val = EVALV_NC (val, rho, 
                            in    ? VARIANT_SEQ | VARIANT_ANY_ATTR :
                            along ? VARIANT_UNCLASS | VARIANT_ANY_ATTR :
                                    VARIANT_UNCLASS | VARIANT_ANY_ATTR_EX_DIM));
    dims = R_NilValue;

    is_seq = 0;

    if (along) { /* "along" and therefore not seq variant */
        R_variant_result = 0;
        if (nsyms == 1) { /* go along vector/pairlist (may also be an array) */
            is_seq = 1;
            seq_start = 1;
            val_type = INTSXP;
        }
        else { /* "along" for array, with several dimensions */
            dims = getAttrib (val, R_DimSymbol);
            if (length(dims) != nsyms)
                errorcall (call, _("incorrect number of dimensions"));
            PROTECT(dims);
            INC_NAMEDCNT(dims);
            PROTECT(indexes = allocVector(INTSXP,nsyms));
            INTEGER(indexes)[0] = 0; /* so will be 1 after first increment */
            for (int j = 1; j < nsyms; j++) 
                INTEGER(indexes)[j] = 1;
            PROTECT(ixvals = allocVector(VECSXP,nsyms));
            PROTECT(bcells = allocVector(VECSXP,nsyms));
        }
        n = length(val);
    }
    else if (across || down) { /* "across" or "down", hence not seq variant*/
        R_variant_result = 0;
        is_seq = 1;
        seq_start = 1;
        val_type = INTSXP;
        dims = getAttrib (val, R_DimSymbol);
        if (TYPEOF(dims)!=INTSXP || LENGTH(dims)==0) /* no valid dim attribute*/
            n = length(val);
        else if (down)
            n = INTEGER(dims)[0];
        else /* across */
            n = LENGTH(dims) > 1 ? INTEGER(dims)[1] : INTEGER(dims)[0];
    }
    else if (R_variant_result) {  /* variant "in" value */
        R_variant_result = 0;
        is_seq = 1;
        seq_start = R_variant_seq_spec >= 0 ? R_variant_seq_spec>>32
            /* Note:  In C99, negative >> ... is undefined */
          : (int64_t)((uint64_t)R_variant_seq_spec>>32) - ((int64_t)1<<32);
        n = (R_variant_seq_spec & 0xffffffff) >> 1;
        val_type = INTSXP;
    }
    else { /* non-variant "in" value */

        INC_NAMEDCNT(val);  /* increment NAMEDCNT to avoid mods by loop code */
        nval = val;  /* for scanning pairlist */

        /* Deal with the case where we are iterating over a factor.
           We need to coerce to character, then iterate */

        if (inherits_CHAR (val, R_factor_CHARSXP)) {
            val = asCharacterFactor(val);
            UNPROTECT_PROTECT(val);
        }

        n = length(val);
        val_type = TYPEOF(val);
    }

    /* If no iterations, just set all variable(s) to R_NilValue or the 
       sizes of the dimensions and then return. */

    if (n == 0) {
        if (nsyms == 1)
            set_var_in_frame (sym, in ? R_NilValue : ScalarIntegerMaybeConst(0),
                              rho, TRUE, 3);
        else {
            int i;
            for (i = 0; i < nsyms; i++) {
                set_var_in_frame (CAR(syms), 
                                  ScalarIntegerMaybeConst(INTEGER(dims)[i]),
                                  rho, TRUE, 3);
                syms = CDR(syms);
            }
        }
        if (nsyms != 1)
            UNPROTECT(4);  /* dims, indexes, ixvals, bcells */
        if (in && !is_seq)
            DEC_NAMEDCNT(val);
        UNPROTECT(3);      /* args, rho, val */
        R_Visible = FALSE;
        return R_NilValue;
    }

    /* Initialize record of binding cells for variables. */

    if (nsyms == 1) { 
        PROTECT_WITH_INDEX (bcell = Rf_find_binding_in_frame (rho, sym, NULL),
                            &bix);
        PROTECT_WITH_INDEX (v = CAR(bcell), &vpi);
    }
    else { 
        for (j = 0, s = syms; j < nsyms; j++, s = CDR(s)) {
            bcell = Rf_find_binding_in_frame (rho, CAR(s), NULL);
            SET_VECTOR_ELT (bcells, j, bcell);
            SET_VECTOR_ELT (ixvals, j, CAR(bcell));
        }
    }

    dbg = RDEBUG(rho);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);

    switch (SETJMP(cntxt.cjmpbuf)) {
    case CTXT_BREAK: goto for_break;
    case CTXT_NEXT: goto for_next;
    }

    /* MAIN LOOP. */

    for (i = 0; i < n; i++) {

        /* Handle multi-dimensional "along". */

        if (nsyms > 1) {

            /* Increment to next combination of indexes. */

            for (j = 0; INTEGER(indexes)[j] == INTEGER(dims)[j]; j++) {
                if (j == nsyms-1) abort();
                INTEGER(indexes)[j] = 1;
            }
            INTEGER(indexes)[j] += 1;

            /* Make sure all 'for' variables are set to the right index,
               using records of the binding cells used for speed. */

            for (j = 0, s = syms; j < nsyms; j++, s = CDR(s)) {
                SEXP v = VECTOR_ELT(ixvals,j);
                if (TYPEOF(v) != INTSXP || LENGTH(v) != 1 || HAS_ATTRIB(v)
                                        || NAMEDCNT_GT_1(v)) {
                    v = allocVector(INTSXP,1);
                    SET_VECTOR_ELT(ixvals,j,v);
                }
                INTEGER(v)[0] = INTEGER(indexes)[j];
                SEXP bcell = VECTOR_ELT(bcells,j);
                if (bcell == R_NilValue || CAR(bcell) != v) {
                    set_var_in_frame (CAR(s), v, rho, TRUE, 3);
                    SET_VECTOR_ELT(bcells,j,R_binding_cell);
                }
            }
            
            goto do_iter;
        }

        /* Handle "in", "across", "down", and univariate "along". */

	switch (val_type) {

	case EXPRSXP:
	case VECSXP:
	    v = VECTOR_ELT(val, i);
	    SET_NAMEDCNT_MAX(v); /* maybe unnecessary? */
	    break;

	case LISTSXP:
	    v = CAR(nval);
	    nval = CDR(nval);
	    SET_NAMEDCNT_MAX(v);
	    break;

	default:

            /* Allocate new space for the loop variable value when the value has
               been assigned to another variable (NAMEDCNT(v) > 1), or when an
               attribute has been attached to it, etc. */

            if (TYPE_ETC(v) != val_type || NAMEDCNT_GT_1(v))
                REPROTECT(v = allocVector(val_type, 1), vpi);

            switch (val_type) {
            case LGLSXP:
                LOGICAL(v)[0] = LOGICAL(val)[i];
                break;
            case INTSXP:
                INTEGER(v)[0] = is_seq ? seq_start + i : INTEGER(val)[i];
                break;
            case REALSXP:
                REAL(v)[0] = REAL(val)[i];
                break;
            case CPLXSXP:
                COMPLEX(v)[0] = COMPLEX(val)[i];
                break;
            case STRSXP:
                SET_STRING_ELT(v, 0, STRING_ELT(val, i));
                break;
            case RAWSXP:
                RAW(v)[0] = RAW(val)[i];
                break;
            default:
                errorcall(call, _("invalid for() loop sequence"));
            }

            break;
        }

        if (bcell == R_NilValue || CAR(bcell) != v) {
            set_var_in_frame (sym, v, rho, TRUE, 3);
            REPROTECT(bcell = R_binding_cell, bix);
        }

    do_iter: ;

        if (RDEBUG(rho))
            start_browser (call, op, body, rho);

        SEXP r = evalv (body, rho, vrnt);

        if (R_variant_result & VARIANT_RTN_FLAG) {
            ret = r;
            goto for_return;
        }

        (void) POP_IF_TOP_OF_STACK(r);

#       if SCALAR_STACK_DEBUG
            if (R_scalar_stack != sv_scalar_stack) abort();
#       endif

    for_next: ;  /* semi-colon needed for attaching label */
    }

 for_break:
    R_Visible = FALSE;
    ret = R_NilValue;

 for_return:
    endcontext(&cntxt);
    if (in && !is_seq)
        DEC_NAMEDCNT(val);
    if (nsyms == 1)
        UNPROTECT(2);  /* v, bcell */
    else 
        UNPROTECT(4);  /* dims, indexes, ixvals, bcells */
    UNPROTECT(3);      /* val, rho, args */
    SET_RDEBUG(rho, dbg);

    return ret;
}


/* While statement.  Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK. */

static SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Don't check arg count - missing are seen as R_NilValue, extra ignored. */

    SEXP cond = CAR(args);
    SEXP body = CADR(args);
    RCNTXT cntxt;
    int dbg;

    int vrnt = VARIANT_NULL | VARIANT_PENDING_OK;
    if (variant & VARIANT_DIRECT_RETURN) 
        vrnt |= VARIANT_PASS_ON(variant);

    R_Visible = FALSE;

    dbg = RDEBUG(rho);

#   if SCALAR_STACK_DEBUG
        SEXP sv_scalar_stack = R_scalar_stack;
#   endif

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);

    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) { /* <- back here for "next" */
        for (;;) {
            SEXP condval = EVALV_NC (cond, rho, VARIANT_SCALAR_STACK_OK 
                                                    | VARIANT_ANY_ATTR);

            if ( ! asLogicalNoNA (condval, call))
                break;

            if (RDEBUG(rho))
                start_browser (call, op, body, rho);

            SEXP r = evalv (body, rho, vrnt);

            if (R_variant_result & VARIANT_RTN_FLAG) {
                endcontext(&cntxt);
                SET_RDEBUG (rho, dbg);
                return r;
            }

            (void) POP_IF_TOP_OF_STACK(r);

#           if SCALAR_STACK_DEBUG
                if (R_scalar_stack != sv_scalar_stack) abort();
#           endif
	}
    }

    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);

    R_Visible = FALSE;
    return R_NilValue;
}


/* Repeat statement.  Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK. */

static SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Don't check arg count - missing are seen as R_NilValue, extra ignored. */

    SEXP body = CAR(args);
    RCNTXT cntxt;
    int dbg;

    int vrnt = VARIANT_NULL | VARIANT_PENDING_OK;
    if (variant & VARIANT_DIRECT_RETURN) 
        vrnt |= VARIANT_PASS_ON(variant);

    R_Visible = FALSE;

    dbg = RDEBUG(rho);

#   if SCALAR_STACK_DEBUG
        SEXP sv_scalar_stack = R_scalar_stack;
#   endif

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);

    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) { /* <- back here for "next" */
	for (;;) {

            if (RDEBUG(rho))
                start_browser (call, op, body, rho);

            SEXP r = evalv (body, rho, vrnt);

            if (R_variant_result & VARIANT_RTN_FLAG) {
                endcontext(&cntxt);
                SET_RDEBUG (rho, dbg);
                return r;
            }

            (void) POP_IF_TOP_OF_STACK(r);

#           if SCALAR_STACK_DEBUG
                if (R_scalar_stack != sv_scalar_stack) abort();
#           endif
	}
    }

    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);

    R_Visible = FALSE;
    return R_NilValue;
}


/* "break" */

static R_NORETURN SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho, 
                                int variant)
{
    findcontext (CTXT_BREAK, rho, R_NilValue);
}


/* "next" */

static R_NORETURN SEXP do_next(SEXP call, SEXP op, SEXP args, SEXP rho,
                               int variant)
{
    findcontext (CTXT_NEXT, rho, R_NilValue);
}


/* Parens are SPECIAL in order to avoid overhead of creating an arg list. 
   Care is taken to allow (...), though it is debatable whether this should 
   be considered valid. 

   The eval variant requested is passed on to the inner expression. */

static SEXP do_paren (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Don't check arg count - missing are seen as R_NilValue, extra ignored. */

    SEXP res = EVALV_NC (CAR(args), rho, VARIANT_PASS_ON(variant));

    R_Visible = TRUE;
    return res;
}


/* Curly brackets.  Passes on the eval variant to the last expression.  For
   earlier expressions, passes either VARIANT_NULL | VARIANT_PENDING_OK or
   the variant passed OR'd with those, if the variant passed includes
   VARIANT_DIRECT_RETURN. */

static SEXP do_begin (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    if (args == R_NilValue)
        return R_NilValue;

    SEXP arg, s;
    SEXP savedsrcref = R_Srcref;
    int len;
    SEXP *srcrefs;

    getBlockSrcrefs(call,&srcrefs,&len);

    int vrnt = VARIANT_NULL | VARIANT_PENDING_OK;
    if (variant & VARIANT_DIRECT_RETURN) 
        vrnt |= VARIANT_PASS_ON(variant) & ~VARIANT_GRADIENT;

#   if SCALAR_STACK_DEBUG
        SEXP sv_scalar_stack = R_scalar_stack;
#   endif

    for (int i = 1; ; i++) {
        arg = CAR(args);
        args = CDR(args);
        R_Srcref = getSrcref (srcrefs, len, i);
        if (RDEBUG(rho))
            start_browser (call, op, arg, rho);
        if (args == R_NilValue)
            break;
        s = EVALV_NC (arg, rho, vrnt);
        if (R_variant_result & VARIANT_RTN_FLAG) {
            R_Srcref = savedsrcref;
            return s;
        }
        (void) POP_IF_TOP_OF_STACK(s);
#       if SCALAR_STACK_DEBUG
            if (R_scalar_stack != sv_scalar_stack) abort();
#       endif
    }

    s = evalv (arg, rho, variant);
    R_Srcref = savedsrcref;
    return s;
}


static SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP a = CAR(args);  /* if malformed, CAR(R_NilValue) will be R_NilValue. */

    if (CDR(args) != R_NilValue)
	errorcall(call, _("multi-argument returns are not permitted"));

    SEXP v;

    if (variant & VARIANT_DIRECT_RETURN) {
        int vrnt =  VARIANT_PASS_ON(variant) 
                     & ~ (VARIANT_NULL | VARIANT_SCALAR_STACK_OK);
        if (STORE_GRAD(rho))
            vrnt |= VARIANT_GRADIENT;
        v = evalv (a, rho, vrnt);
        R_variant_result |= VARIANT_RTN_FLAG;
        return v;
    }
    else {
        v = evalv (a, rho, 0);
        findcontext (CTXT_BROWSER | CTXT_FUNCTION, rho, v);
    }
}


/*  Assignment in its various forms. */

static attribute_noinline SEXP Rf_set_subassign 
  (SEXP call, SEXP lhs, SEXP rhs, SEXP rho, int variant, int opval);

static SEXP do_set (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    /* Don't check arg count - missing are seen as R_NilValue, extra ignored. */

    SEXP lhs = CAR(args), rhs = CADR(args);
    int opval = PRIMVAL(op);
    SEXP grad = R_NilValue;

    /* Swap operands for -> and ->>. */

    if (opval >= 10) {
        lhs = rhs;
        rhs = CAR(args);
        opval -= 10;
    }

  redo:  /* come back here after converting string as lhs to symbol */

    if (TYPE_ETC(lhs) == SYMSXP) {  /* don't allow ... or ..1, ..2, etc. */

        /* -- ASSIGNMENT TO A SIMPLE VARIABLE -- */

        /* Handle <<-, or assignment to the base environment, or a user 
           database (which has object bit set) without trying the 
           optimizations done below.  Does not track gradient. */

        if (opval || IS_BASE(rho) || OBJECT(rho)) {
            rhs = evalv (rhs, rho, VARIANT_PENDING_OK);
            if (opval) {
                set_var_nonlocal (lhs, rhs, ENCLOS(rho), 3);
                goto done;
            }
            else
                goto set_in_frame;
        }

        /* We decide whether we'll ask the right hand side evalutation to do
           the assignment, for statements like v<-exp(v), v<-v+1, or v<-2*v. */

        int varnt = (VARIANT_PENDING_OK | VARIANT_SCALAR_STACK_OK)
                      | (variant & VARIANT_GRADIENT);

        if (STORE_GRAD(rho) && !opval)
            varnt |= VARIANT_GRADIENT;
        if (TYPEOF(rhs) == LANGSXP) {
            if (CADR(rhs) == lhs) 
                varnt |= VARIANT_LOCAL_ASSIGN1;
            else if (CADDR(rhs) == lhs)
                varnt |= VARIANT_LOCAL_ASSIGN2;
        }

        /* Evaluate the right hand side, asking for it on the scalar stack. 
           May also get the gradient of the rhs, in which case we let it 
           say in R_gradient, with VARIANT_GRADIENT_FLAG in R_variant_result. */

        rhs = EVALV (rhs, rho, varnt);

        if (R_variant_result & 1) {  /* assignment done by the rhs operator. */
            R_variant_result = 0;
            goto done;
        }

        /* Look to see if the binding for the variable can be found
           from LASTSYMBINDING.  Set v to the bound value, or
           R_UnboundValue if not found, and R_binding_cell to the
           cell, if it was found. */

        SEXP v = R_UnboundValue;
        if (SEXP32_FROM_SEXP(rho) == LASTSYMENV(lhs) 
              && ! BINDING_IS_LOCKED((R_binding_cell = LASTSYMBINDING(lhs)))) {
            v = CAR(R_binding_cell);
        }
        else
            R_binding_cell = R_NilValue;

        /* Try to copy the value, not assign the object, if the rhs is
           scalar (no attributes, not being computed) and doesn't have
           zero NAMEDCNT (for which assignment would be free).  This
           will copy from the scalar stack, which must be replaced by
           a regular value if the copy can't be done.  If the copy
           can't be done, but a binding cell was found here, the
           assignment is done directly into the binding cell, avoiding
           overhead of calling set_var_in_frame.

           Avoid accessing NAMEDCNT in a way that will cause unnecessary
           waits for task completion.

           Note that the check below catches items on the scalar stack. 

           The copy is done with memcpy of 8 bytes, which is the minimum
           space allocated for data (required by alignment), which will
           not copy all of a complex item (hence CPLXSXP not handled here).
           (Note that since the size of the copy is constant, this should
           compile to a few inline instructions.) */

        int rhs_type_etc = TYPE_ETC(rhs);  /* type + vec + attr + b.c. */

        if (NAMEDCNT_GT_0(rhs)
         && (rhs_type_etc&~TYPE_ET_CETERA_TYPE)==0 /* scalar, no attr, n.b.c. */
         && (((NONPOINTER_VECTOR_TYPES & ~(1<<CPLXSXP)) >> rhs_type_etc) & 1)) {
            if (v == R_UnboundValue)
                v = findVarInFrame3_nolast (rho, lhs, 7);
            if (TYPE_ETC(v) == rhs_type_etc  /* won't be if R_UnboundValue */
                 && TRUELENGTH(v) == TRUELENGTH(rhs) && LEVELS(v) == LEVELS(rhs)
                 && !NAMEDCNT_GT_1(v)) {
                SET_NAMEDCNT_NOT_0(v);
                (void) POP_IF_TOP_OF_STACK(rhs);
                WAIT_UNTIL_NOT_IN_USE(v);  /* won't be being computed */
                memcpy(REAL(v),REAL(rhs),sizeof(double)); /* others no bigger */
                rhs = v; /* for return value */
                goto done;
            }
            if (POP_IF_TOP_OF_STACK(rhs))
                rhs = DUP_STACK_VALUE(rhs);
        }

        /* Assign rhs to lhs using the binding cell found above. */

        if (v != R_UnboundValue) {
            DEC_NAMEDCNT_AND_PRVALUE(v);
            SETCAR_MACRO(R_binding_cell, rhs);
            SET_MISSING(R_binding_cell,0);
            INC_NAMEDCNT(rhs);
            if (rho == R_GlobalEnv) 
                R_DirtyImage = 1;
            goto done;
        }

        /* Assign rhs object to lhs symbol the usual way. */

      set_in_frame:

        set_var_in_frame (lhs, rhs, rho, TRUE, 3);
    }

    else if (TYPE_ETC(lhs) == LANGSXP) {  /* other parts will be 0 */

        /* -- ASSIGNMENT TO A COMPLEX TARGET -- */

        rhs = Rf_set_subassign (call, lhs, rhs, rho, variant, opval);
        goto fini;
    }

    else if (TYPEOF(lhs) == STRSXP && LENGTH(lhs) == 1) {
        /* Convert lhs string to a symbol and try again */
        lhs = install_translated (STRING_ELT(lhs,0));
        goto redo;
    }

    else {
        invalid_assignment_error(call);
    }

  done:

    if (STORE_GRAD(rho)) {
        if (!opval && R_binding_cell != R_NilValue) {
            SET_GRADIENT_IN_CELL (R_binding_cell, 
                                  R_variant_result & VARIANT_GRADIENT_FLAG 
                                    ? R_gradient : R_NilValue);
        }
    }

  fini:

    R_Visible = FALSE;

    if (R_variant_result && !(variant & VARIANT_GRADIENT))
        R_variant_result = 0;

    if (variant & VARIANT_NULL)
        return R_NilValue;

    if ( ! (variant & VARIANT_PENDING_OK)) 
        WAIT_UNTIL_COMPUTED(rhs);
    
    return rhs;
}

#define FIND_SUBASSIGN_FUNC(fun)  \
    SUBASSIGN_FOLLOWS(fun) ? SUBASSIGN_THAT_FOLLOWS(fun) \
                           : find_subassign_func(fun)

static attribute_noinline SEXP find_subassign_func(SEXP fun)
{
    if (TYPE_ETC(fun) == SYMSXP) {  /* don't allow ... or ..1, ..2, etc. */

        if (SUBASSIGN_FOLLOWS(fun))
            return SUBASSIGN_THAT_FOLLOWS(fun);

        char buf[40];
        const char *fname = CHAR(PRINTNAME(fun));

        if (!copy_2_strings (buf, sizeof buf, fname, "<-"))
            error(_("overlong name in '%s'"), fname);

        return install(buf);
    }

    /* Handle foo::bar and foo:::bar. */

    if (TYPEOF(fun)==LANGSXP && length(fun)==3 && TYPEOF(CADDR(fun))==SYMSXP
      && (CAR(fun)==R_DoubleColonSymbol || CAR(fun)==R_TripleColonSymbol))
        return lang3 (CAR(fun), CADR(fun), find_subassign_func(CADDR(fun)));

    error(_("invalid function in complex assignment"));
}

/* arguments of replaceCall must be protected by the caller. */

static SEXP replaceCall(SEXP fun, SEXP varval, SEXP args, SEXP rhs)
{
    SEXP value, first;

    first = value = cons_with_tag (rhs, R_NilValue, R_ValueSymbol);

    if (args != R_NilValue) {

        first = cons_with_tag (CAR(args), value, TAG(args));

        SEXP p = CDR(args);
        if (p != R_NilValue) {
            PROTECT(first);
            SEXP q = first;
            do { 
                SETCDR (q, cons_with_tag (CAR(p), value, TAG(p)));
                q = CDR(q);
                p = CDR(p);
            } while (p != R_NilValue);
            UNPROTECT(1);
        }
    }

    first = LCONS (fun, CONS(varval, first));

    return first;
}

/* Complex assignment.  Made a separate, non-static, function in order
   to avoid possible overhead of a large function (eg, stack frame size)
   for the simple case. */

static attribute_noinline SEXP Rf_set_subassign_general 
  (SEXP call, SEXP lhs, SEXP rhs, SEXP rho, int depth, SEXP var, 
   SEXP varval, SEXP rhs_grad, SEXP var_grad,
   SEXP assgnfcn, SEXP rhsprom, SEXP rhs_uneval, int maybe_fast);

static SEXP attribute_noinline Rf_set_subassign 
  (SEXP call, SEXP lhs, SEXP rhs, SEXP rho, int variant, int opval)
{
    /* Find the assignment function symbol for the depth 1 assignment, and
       see if we maybe (tentatively) will be using the fast interface. */

    SEXP assgnfcn = FIND_SUBASSIGN_FUNC(CAR(lhs));

    int maybe_fast = MAYBE_FAST_SUBASSIGN(assgnfcn);

    /* We evaluate the right hand side now, asking for it on the
       scalar stack if we (tentatively) will be using the fast
       interface (unless value needed for return, and not allowed on
       scalar stack), and otherwise for pending computation. */

    SEXP rhs_uneval = rhs;  /* save unevaluated rhs */

    int rhs_variant = STORE_GRAD(rho) ? VARIANT_GRADIENT 
                                       : variant & VARIANT_GRADIENT;
    if (maybe_fast) {
        if (variant & (VARIANT_SCALAR_STACK_OK | VARIANT_NULL))
            rhs_variant |= VARIANT_SCALAR_STACK_OK;
    }
    else
        rhs_variant |= VARIANT_PENDING_OK;

    rhs = EVALV (rhs, rho, rhs_variant);

    SEXP rhs_grad = R_NilValue;
    if (R_variant_result & VARIANT_GRADIENT_FLAG) {
        rhs_grad = R_gradient;
        R_variant_result = 0;
    }

    PROTECT2(rhs,rhs_grad);

    /* Increment NAMEDCNT temporarily if rhs will be needed as the value,
       to protect it from being modified by the assignment, or otherwise. */

    if ( ! (variant & VARIANT_NULL))
        INC_NAMEDCNT(rhs);

    SEXP var, varval, newval, rhsprom;

    /* Find the variable ultimately assigned to, and its depth.
       The depth is 1 for a variable within one replacement function
       (eg, in names(a) <- ...). */

    int depth = 1;
    for (var = CADR(lhs); TYPEOF(var) != SYMSXP; var = CADR(var)) {
        if (TYPEOF(var) != LANGSXP) {
            if (TYPEOF(var) == STRSXP && LENGTH(var) == 1) {
                var = installChar (STRING_ELT(var,0));
                break;
            }
            invalid_assignment_error(call);
        }
        depth += 1;
    }

    /* Get the value of the variable assigned to, and ensure it is local
       (unless this is the <<- operator).  Save and protect the binding 
       cell used. */

    if (opval) /* <<- */
        varval = findVar (var, ENCLOS(rho));
    else {
        varval = findVarInFramePendingOK (rho, var);
        if (varval == R_UnboundValue && rho != R_EmptyEnv) {
            varval = findVar (var, ENCLOS(rho));
            if (varval != R_UnboundValue) {
                if (TYPEOF(varval) == PROMSXP)
                    varval = forcePromise(varval);
                set_var_in_frame (var, varval, rho, TRUE, 3);
            }
        }
    }

    SET_NAMEDCNT_NOT_0(varval); /* 0 may sometime happen - should mean 1 */

    SEXP bcell = R_binding_cell;
    SEXP var_grad = R_NilValue;
    if (HAS_GRADIENT_IN_CELL(bcell) && !opval)
        var_grad = GRADIENT_IN_CELL(bcell);
    PROTECT2(bcell,var_grad);

    if (TYPEOF(varval) == PROMSXP) {
        SEXP f = forcePromise(varval);
        if (HAS_GRADIENT_IN_CELL(varval) && !opval)
            UNPROTECT_PROTECT(var_grad = GRADIENT_IN_CELL(varval));
        varval = f;
    }

#if 0
if ((variant & VARIANT_GRADIENT) || STORE_GRAD(rho) && !opval) {
REprintf("==\n"); R_inspect(rhs);
REprintf("--\n"); R_inspect(rhs_grad);
REprintf("--\n"); R_inspect(varval);
REprintf("--\n"); R_inspect(var_grad);
REprintf("**\n");
}
#endif

    if (varval == R_UnboundValue)
        unbound_var_error(var);
    if (varval == R_MissingArg)
        arg_missing_error(var);

    /* We might be able to avoid this duplication sometimes (eg, in
       a <- b <- integer(10); a[1] <- 0.5), except that some packages 
       (eg, Matrix 1.0-6) assume (in C code) that the object in a 
       replacement function is not shared. */

    if (NAMEDCNT_GT_1(varval))
        varval = dup_top_level(varval);

    PROTECT(varval);

    SEXP res_grad = R_NilValue;

    /* Special code for depth of 1.  This is purely an optimization - the
       general code below should also work when depth is 1. */

    if (depth == 1) {
        SEXP lhsprom, fn, e;
        if (maybe_fast && !isObject(varval)
              && rhs_grad == R_NilValue && var_grad == R_NilValue  /* FOR NOW */
              && CADDR(lhs) != R_DotsSymbol
              && (fn = FINDFUN(assgnfcn,rho), 
                  TYPEOF(fn) == SPECIALSXP && PRIMFASTSUB(fn) && !RTRACE(fn))) {
            /* Use the fast interface.  No need to wait for rhs, since
               not evaluated with PENDING_OK */
            R_fast_sub_var = varval;
            R_fast_sub_replacement = rhs;
            R_variant_result = 0;
            newval = CALL_PRIMFUN (call, fn, CDDR(lhs), rho, 
                                   VARIANT_FAST_SUB);
        }
        else {
            if (POP_IF_TOP_OF_STACK(rhs))  /* might be on stack if maybe_fast */
                rhs = DUP_STACK_VALUE(rhs);
#if 0
REprintf("rr\n"); R_inspect(rhs_grad);
REprintf("vv\n"); R_inspect(var_grad);
#endif
            PROTECT (rhsprom = mkValuePROMISE(rhs_uneval, rhs));
            PROTECT (lhsprom = mkValuePROMISE(CADR(lhs), varval));
            PROTECT(e = replaceCall (assgnfcn, lhsprom, CDDR(lhs), rhsprom));
            if (rhs_grad != R_NilValue || var_grad != R_NilValue) {
                if (var_grad != R_NilValue) {
                    SET_GRADIENT_IN_CELL (lhsprom, var_grad);
                    SET_STORE_GRAD (lhsprom, 1);
                }
                if (rhs_grad != R_NilValue) {
                    SET_GRADIENT_IN_CELL (rhsprom, rhs_grad);
                    SET_STORE_GRAD (rhsprom, 1);
                }
                newval = evalv (e, rho, VARIANT_GRADIENT);
                if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                    res_grad = R_gradient;
                    R_variant_result = 0;
#if 0
REprintf("^^\n"); R_inspect(res_grad);
#endif
                }
            }
            else 
                newval = evalv (e, rho, 0);
            UNPROTECT(3);
        }
    }

    else {  

        /* The general case, for any depth, done in another function to
           possibly reduce overhead (eg, stack frame size) for simpler cases. */

        newval =  Rf_set_subassign_general (call, lhs, rhs, rho, depth, var, 
                                            varval, rhs_grad, var_grad,
                                            assgnfcn, rhsprom, rhs_uneval,
                                            maybe_fast);
        res_grad = R_gradient;
#if 0
if (res_grad != R_NilValue) { REprintf("oo\n"); R_inspect(res_grad); }
#endif
    }

    UNPROTECT(5);  /* rhs, rhs_grad, bcell, varval, var_grad */

    /* Assign the final result after the top level replacement.  We
       can sometimes avoid the cost of this by looking at the saved
       binding cell, if we have one. */

    if (bcell != R_NilValue && CAR(bcell) == newval) {
        SET_MISSING(bcell,0);
        /* The replacement function might have changed NAMEDCNT to 0. */
        SET_NAMEDCNT_NOT_0(varval);
    }
    else {
        if (opval) /* <<- */
            set_var_nonlocal (var, newval, ENCLOS(rho), 3);
        else
            set_var_in_frame (var, newval, rho, TRUE, 3);
        bcell = R_binding_cell;
    }

    if (bcell != R_NilValue)
        SET_GRADIENT_IN_CELL (bcell, res_grad);

    R_variant_result = 0;
    if (variant & VARIANT_NULL) {
        POP_IF_TOP_OF_STACK(rhs);
        return R_NilValue;
    }
    else {
        DEC_NAMEDCNT(rhs);
        if (rhs_grad != R_NilValue) {
            R_variant_result = VARIANT_GRADIENT_FLAG;
            R_gradient = rhs_grad;
        }
        return rhs;
    }
}

static SEXP attribute_noinline Rf_set_subassign_general 
  (SEXP call, SEXP lhs, SEXP rhs, SEXP rho, int depth, SEXP var, 
   SEXP varval, SEXP rhs_grad, SEXP var_grad,
   SEXP assgnfcn, SEXP rhsprom, SEXP rhs_uneval, int maybe_fast)
{
    SEXP v, b, e, op, prom, fn, fetch_args, newval, lhsprom;
    int d, fast;

    /* Structure recording information on expressions at all levels of 
       the lhs.  Level 'depth' is the ultimate variable; level 0 is the
       whole lhs expression. */

    struct { 
        SEXP expr;        /* Expression at this level */
        SEXP value;       /* Value of expr, may later change */
        SEXP grad;        /* Gradient of value, or R_NilValue */
        SEXP store_args;  /* Arg list for store; depth 0 special, else  */
                          /*   LISTSXP or NILSXP - pairlist of promises */
                          /*   PROMSXP - promise for single argument    */
                          /*   R_NoObject - one arg from CADDR(expr)    */
        int in_top;       /* 1 or 2 if value is an unshared part of the */
                          /*   value at top level, else 0               */
    } s[depth+1];         

    /* For each level from 1 to depth, store the lhs expression at that
       level. */

    s[0].expr = lhs;
    for (v = CADR(lhs), d = 1; d < depth; v = CADR(v), d++) {
        s[d].expr = v;
    }
    s[depth].expr = var;

    /* Note: In code below, promises with the value already filled
             in are used to 'quote' values passsed as arguments,
             so they will not be changed when the arguments are
             evaluated, and so deparsed error messages will have
             the source expression.  These promises should not be
             recycled, since they may be saved in warning messages
             stored for later display.  */

    /* For each level except the outermost, evaluate and save the
       value of the expression as it is before the assignment (and its
       gradient, if present).  Also, ask if it is an unshared subset
       of the next larger expression (and all larger ones).  If it is
       not known to be part of the larger expressions, we do a
       top-level duplicate of it.

       Also, for each level except the final variable and
       outermost level, which only does a store, save argument
       lists for the fetch/store functions that are built with
       shared promises, so that they are evaluated only once.  The
       store argument list has a "value" cell at the end to fill
       in the stored value.

       For efficiency, $ and [[ are handled with VARIANT_FAST_SUB,
       and for $, no promise is created for its argument. */

    s[depth].value = varval;
    s[depth].grad = var_grad;
    s[depth].in_top = 1;

    s[0].store_args = CDDR(lhs);  /* original args, no value cell */

    for (d = depth-1; d > 0; d--) {

        op = CAR(s[d].expr);

        fast = 0;
        if ((op == R_DollarSymbol || op == R_Bracket2Symbol)
             && s[d+1].grad == R_NilValue /* FOR NOW */ ) {
            fn = FINDFUN (op, rho);
            fast = TYPEOF(fn)==SPECIALSXP && PRIMFASTSUB(fn) && !RTRACE(fn);
        }

        if (fast && op == R_DollarSymbol 
                 && CDDR(s[d].expr) != R_NilValue 
                 && CDR(CDDR(s[d].expr)) == R_NilValue) {
            fetch_args = CDDR(s[d].expr);
            s[d].store_args = R_NoObject;
        }
        else {
            fetch_args = promiseArgs (CDDR(s[d].expr), rho, 0);
            if (CDR(fetch_args)==R_NilValue && TAG(fetch_args)==R_NilValue)
                s[d].store_args = CAR(fetch_args);
            else
                s[d].store_args = dup_arg_list (fetch_args);
        }

        PROTECT2 (s[d].store_args, fetch_args);

        /* We'll need this value for the subsequent replacement
           operation, so make sure it doesn't change.  Incrementing
           NAMEDCNT would be the obvious way, but if NAMEDCNT 
           was already non-zero, that leads to undesirable duplication
           later (even if the increment is later undone).  Making sure
           that NAMEDCNT isn't zero seems to be sufficient. */

        SET_NAMEDCNT_NOT_0(s[d+1].value);

        s[d].grad = R_NilValue;

        if (fast) {
            R_fast_sub_var = s[d+1].value;
            R_variant_result = 0;
            e = CALL_PRIMFUN (call, fn, fetch_args, rho, 
                  VARIANT_FAST_SUB /* implies QUERY_UNSHARED_SUBSET */);
            UNPROTECT(1);  /* fetch_args */
        }
        else {
            prom = mkValuePROMISE(s[d+1].expr,s[d+1].value);
            if (s[d+1].grad != R_NilValue) {
                SET_GRADIENT_IN_CELL (prom, s[d+1].grad);
                SET_STORE_GRAD (prom, 1);
                PROTECT (e = LCONS (op, CONS (prom, fetch_args)));
                R_variant_result = 0;
                e = evalv_other (e, rho, VARIANT_GRADIENT
                                           | VARIANT_QUERY_UNSHARED_SUBSET);
                if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                    s[d].grad = R_gradient;
                    R_variant_result &= ~VARIANT_GRADIENT_FLAG;
                }
            }
            else {
                PROTECT (e = LCONS (op, CONS (prom, fetch_args)));
                R_variant_result = 0;
                e = evalv_other (e, rho, VARIANT_QUERY_UNSHARED_SUBSET);
            }
            UNPROTECT(2);  /* e, fetch_args */
        }

        PROTECT(s[d].grad);
        s[d].in_top = 
          s[d+1].in_top == 1 ? R_variant_result : 0;  /* 0, 1, or 2 */
        R_variant_result = 0;
        if (s[d].in_top == 0)
            e = dup_top_level(e);
        s[d].value = e;
        PROTECT(e);
    }

    /* Call the replacement function at level 1, perhaps using the
       fast interface. */

    SEXP newgrad = R_NilValue;

    if (maybe_fast && !isObject(s[1].value) 
          && rhs_grad == R_NilValue && var_grad == R_NilValue  /* FOR NOW */
          && CAR(s[0].store_args) != R_DotsSymbol
          && (fn = FINDFUN(assgnfcn,rho), 
              TYPEOF(fn) == SPECIALSXP && PRIMFASTSUB(fn) && !RTRACE(fn))) {
        R_fast_sub_var = s[1].value;
        R_fast_sub_replacement = rhs;
        PROTECT3 (R_fast_sub_replacement, R_fast_sub_var, fn);
        newval = CALL_PRIMFUN (call, fn, s[0].store_args, rho, 
                               VARIANT_FAST_SUB);
        e = R_NilValue;
    }
    else {
        if (POP_IF_TOP_OF_STACK(rhs))  /* might be on stack if maybe_fast */
            rhs = DUP_STACK_VALUE(rhs);
        PROTECT (rhsprom = mkValuePROMISE(rhs_uneval, rhs));
        PROTECT (lhsprom = mkValuePROMISE(s[1].expr, s[1].value));
        /* original args, no value cell at end, assgnfcn set above*/
        PROTECT (e = replaceCall (assgnfcn, lhsprom, 
                                  s[0].store_args, rhsprom));
        if (rhs_grad != R_NilValue || s[1].grad != R_NilValue) {
            if (s[1].grad != R_NilValue) {
                SET_GRADIENT_IN_CELL (lhsprom, s[1].grad);
                SET_STORE_GRAD (lhsprom, 1);
            }
            if (rhs_grad != R_NilValue) {
                SET_GRADIENT_IN_CELL (rhsprom, rhs_grad);
                SET_STORE_GRAD (rhsprom, 1);
            }
            newval = evalv (e, rho, VARIANT_GRADIENT);
            if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                newgrad = R_gradient;
                R_variant_result = 0;
            }
        }
        else
            newval = evalv (e, rho, 0);
    }
#if 0
if (installed_already("DBGG")) {
  REprintf("repl %d %d\n",depth,0); 
  R_inspect(newval); REprintf("--\n");
  R_inspect(newgrad); REprintf("..\n");
}
#endif

    UNPROTECT(3);  /* e, lhsprom, rhsprom OR 3 from other branch of above if
                      Note: is e used later, but with no alloc before. */

    /* Call the replacement functions at levels 2 to depth, changing the
       values at each level, using the fetched value at that level 
       (was perhaps duplicated), and the new value after replacement at 
       the lower level.  Except we don't do that if it's not necessary
       because the new value is already part of the larger object. */
    
    for (d = 1; d < depth; d++) {

        /* If the replacement function returned a different object, 
           we have to replace, since that new object won't be part 
           of the object at the next level, even if the old one was. */

        if (s[d].in_top == 1 && s[d].value == newval
          && newgrad == R_NilValue && s[d+1].grad == R_NilValue /* FOR NOW */) {

            /* Don't need to do replacement. */

            newval = s[d+1].value;
        }
        else {

            /* Put value into the next-higher object. */

            PROTECT (newgrad);
            PROTECT (rhsprom = mkValuePROMISE (e, newval));
            if (newgrad != R_NilValue) {
                SET_STORE_GRAD (rhsprom, 1);
                SET_GRADIENT_IN_CELL (rhsprom, newgrad);
            }
            PROTECT (lhsprom = mkValuePROMISE (s[d+1].expr, s[d+1].value));
            if (s[d+1].grad != R_NilValue) {
                SET_STORE_GRAD (lhsprom, 1);
                SET_GRADIENT_IN_CELL (lhsprom, s[d+1].grad);
            }
            assgnfcn = FIND_SUBASSIGN_FUNC(CAR(s[d].expr));
            b = cons_with_tag (rhsprom, R_NilValue, R_ValueSymbol);
            if (s[d].store_args == R_NoObject)
                s[d].store_args = CONS (CADDR(s[d].expr), b);
            else if (s[d].store_args == R_NilValue)
                s[d].store_args = b;
            else if (TYPEOF(s[d].store_args) != LISTSXP) /* one arg */
                s[d].store_args = CONS (s[d].store_args, b);
            else {
                for (v = s[d].store_args; CDR(v)!=R_NilValue; v = CDR(v)) ;
                SETCDR(v, b);
            }
            PROTECT(e = LCONS (assgnfcn, CONS(lhsprom, s[d].store_args)));

            newval = newgrad != R_NilValue || s[d+1].grad != R_NilValue
                       ? evalv(e,rho,VARIANT_GRADIENT) : evalv(e,rho,0);

            if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                newgrad = R_gradient;
                R_variant_result = 0;
            }
            else
                newgrad = R_NilValue;
#if 0
if (installed_already("DBGG")) {
  REprintf("repl %d %d\n",depth,d); 
  R_inspect(newval); REprintf("--\n");
  R_inspect(newgrad); REprintf("..\n");
}
#endif

            UNPROTECT(4);  /* newgrad, rhsprom, lhsprom, e
                              (e is used later, but no alloc before) */
        }
    }

    UNPROTECT (3 * (depth-1));

    R_gradient = newgrad;
    return newval;
}


/* Evaluate each expression in "el" in the environment "rho".  
   The evaluation is done by calling evalv with the given variant.

   The MISSING gp field in the CONS cell for a missing argument is 
   set to the result of R_isMissing, which will allow identification 
   of missing arguments resulting from '_'.

   For all but the last argument, NAMEDCNT is temporarily incremented
   to prevent modification by evaluation of later arguments, with
   NAMEDCNT decremented again when all arguments have been evaluated.

   R_variant_result is set to 0 before return. */

SEXP attribute_hidden evalList_v (SEXP el, SEXP rho, int variant)
{
    SEXP ev, ev_el;

    /* Handle 0 or 1 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) { /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue) {
            R_variant_result = 0;
            return R_NilValue;
        }
        if (CAR(el) != R_DotsSymbol) {
            ev = cons_with_tag (EVALV (CAR(el), rho, variant), 
                                R_NilValue, TAG(el));
            R_variant_result = 0;
            return ev;
        }
    }

    /* The general case (except for el == R_NilValue, handed above). */

    int varpend = variant | VARIANT_PENDING_OK;

    BEGIN_PROTECT3 (head, tail, h);

    head = R_NilValue;
    tail = R_NilValue;

    do {  /* el won't be R_NilValue, so will loop at least once */

	if (CAR(el) == R_DotsSymbol) {
            /* If we have a ... symbol, we look to see what it is bound to.
               If its binding is Null (i.e. zero length) or missing we just
               ignore it and return the cdr with all its expressions evaluated.
               If it is bound to a ... list of promises, we force all the 
               promises and then splice the list of resulting values into
               the return value. Anything else bound to a ... symbol is an 
               error. */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP) {
		while (h != R_NilValue) {
                    INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
                    ev_el = evalv (CAR(h), rho, varpend);
                    ev = cons_with_tag (ev_el, R_NilValue, TAG(h));
                    if (head==R_NilValue)
                        head = ev;
                    else
                        SETCDR(tail, ev);
                    tail = ev;
                    if (CAR(ev) == R_MissingArg && isSymbol(CAR(h)))
                        SET_MISSING (ev, R_isMissing(CAR(h),rho));
		    h = CDR(h);
		}
	    }
	    else if (h != R_NilValue && h != R_MissingArg)
		dotdotdot_error();

	} else {
            if (CDR(el) == R_NilValue) 
                varpend = variant;  /* don't defer pointlessly for last one */
            INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
            ev_el = EVALV(CAR(el),rho,varpend);
            ev = cons_with_tag (ev_el, R_NilValue, TAG(el));
            if (head==R_NilValue)
                head = ev;
            else
                SETCDR_MACRO(tail, ev);
            tail = ev;
            if (ev_el == R_MissingArg && isSymbol(CAR(el)))
                SET_MISSING (ev, R_isMissing(CAR(el),rho));
	}

	el = CDR(el);

    } while (el != R_NilValue);

    for (el = head; CDR(el) != R_NilValue; el = CDR(el))
        DEC_NAMEDCNT(CAR(el));

    if (! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_ARGUMENTS_COMPUTED(head);

    R_variant_result = 0;
    RETURN_SEXP_INSIDE_PROTECT (head);
    END_PROTECT;

} /* evalList_v */


/* Version of evalList_v that also asks for gradients of some
   arguments, attaching them as attributes of the CONS cells holding
   the arguments.  If n and m are both zero, the last argument is
   evaluated asking for the gradient.  Otherwise, n must be non-zero,
   and gives the number of arguments that might be evaluated asking
   for their gradient; the m argument, always less than n, is the
   number of initial arguments for which the gradient is not requested.

   R_variant_result is set to 0 before return. */

SEXP attribute_hidden evalList_gradient (SEXP el, SEXP rho, int variant, 
                                         int n, int m)
{
    SEXP ev, ev_el;

    /* Handle 0 or 1 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) { /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue) {
            R_variant_result = 0;
            return R_NilValue;
        }
        if (CAR(el) != R_DotsSymbol) {
            ev  = cons_with_tag (EVALV (CAR(el), rho,
                                 m > 0 ? variant : variant | VARIANT_GRADIENT), 
                                 R_NilValue, TAG(el));
            if (R_variant_result & VARIANT_GRADIENT_FLAG)
                SET_GRADIENT_IN_CELL (ev, R_gradient);
            R_variant_result = 0;
            return ev;
        }
    }

    /* The general case (except for el == R_NilValue, handed above). */

    BEGIN_PROTECT3 (head, tail, h);

    int i = 0;

    head = R_NilValue;
    tail = R_NilValue;

    do {  /* el won't be R_NilValue, so will loop at least once */

	if (CAR(el) == R_DotsSymbol) {
            /* If we have a ... symbol, we look to see what it is bound to.
               If its binding is Null (i.e. zero length) or missing we just
               ignore it and return the cdr with all its expressions evaluated.
               If it is bound to a ... list of promises, we force all the 
               promises and then splice the list of resulting values into
               the return value. Anything else bound to a ... symbol is an 
               error. */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP) {
		while (h != R_NilValue) {
                    INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
                    int vr = variant;
                    if (CDR(h) == R_NilValue && CDR(el) == R_NilValue) {
                        if (n==0 || i<n && i>=m)
                            vr |= VARIANT_GRADIENT;
                    }
                    else {
                        vr |= i<n && i>=m ? VARIANT_GRADIENT|VARIANT_PENDING_OK
                                          : VARIANT_PENDING_OK;
                    }
                    ev_el = evalv (CAR(h), rho, vr);
                    i += 1;
                    ev = cons_with_tag (ev_el, R_NilValue, TAG(h));
                    if (R_variant_result & VARIANT_GRADIENT_FLAG)
                        SET_GRADIENT_IN_CELL (ev, R_gradient);
                    if (head==R_NilValue)
                        head = ev;
                    else
                        SETCDR(tail, ev);
                    tail = ev;
                    if (CAR(ev) == R_MissingArg && isSymbol(CAR(h)))
                        SET_MISSING (ev, R_isMissing(CAR(h),rho));
		    h = CDR(h);
		}
	    }
	    else if (h != R_NilValue && h != R_MissingArg)
		dotdotdot_error();

	} else {
            INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
            int vr = variant;
            if (CDR(el) == R_NilValue) {
                if (n==0 || i<n && i>=m)
                    vr |= VARIANT_GRADIENT;
            }
            else {
                vr |= i<n && i>=m ? VARIANT_GRADIENT|VARIANT_PENDING_OK
                                  : VARIANT_PENDING_OK;
            }
            ev_el = EVALV (CAR(el), rho, vr);
            i += 1;
            ev = cons_with_tag (ev_el, R_NilValue, TAG(el));
            if (R_variant_result & VARIANT_GRADIENT_FLAG)
                SET_GRADIENT_IN_CELL (ev, R_gradient);
            if (head==R_NilValue)
                head = ev;
            else
                SETCDR_MACRO(tail, ev);
            tail = ev;
            if (ev_el == R_MissingArg && isSymbol(CAR(el)))
                SET_MISSING (ev, R_isMissing(CAR(el),rho));
	}

	el = CDR(el);

    } while (el != R_NilValue);

    for (el = head; CDR(el) != R_NilValue; el = CDR(el))
        DEC_NAMEDCNT(CAR(el));

    if (! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_ARGUMENTS_COMPUTED(head);

    R_variant_result = 0;
    RETURN_SEXP_INSIDE_PROTECT (head);
    END_PROTECT;

} /* evalList_gradient */


/* evalListUnshared evaluates each expression in "el" in the
   environment "rho", ensuring that the values of variables evaluated
   are unshared, if they are atomic scalars without attributes, by
   assigning a duplicate to them if necessary.

   Used in .External (with .Call using eval_unshared directly) as a
   defensive measure against argument abuse.  evalListUnshared waits
   for arguments to be computed, and does not allow missing
   arguments. */

SEXP attribute_hidden eval_unshared (SEXP e, SEXP rho, int variant)
{
    SEXP res;

    if (!isSymbol(e) || e == R_DotsSymbol || DDVAL(e) 
                     || e == R_MissingArg || e == R_MissingUnder) {
        res = EVALV (e, rho, variant);
    }
    else {

        res = FIND_VAR_PENDING_OK (e, rho);

        if (res == R_UnboundValue)
            unbound_var_error(e);

        else if (res == R_MissingArg) {
            if ( ! (variant & VARIANT_MISSING_OK))
                if (!DDVAL(e))  /* revert bug fix for the moment */
                    arg_missing_error(e);
        }

        else if (TYPEOF(res) == PROMSXP) {
            SEXP prom = res;
            if (PRVALUE_PENDING_OK(prom) == R_UnboundValue)
                res = forcePromiseUnbound(prom,VARIANT_PENDING_OK);
            else
                res = PRVALUE_PENDING_OK(prom);
            if (isVectorAtomic(res) && LENGTH(res) == 1 && !HAS_ATTRIB(res)
                  && NAMEDCNT(res) > 2 /* will be 2 if unshared var passed */) {
                if (0) { /* Enable for debugging */
                    if (installed_already("UNSHARED.DEBUG") != R_NoObject)
                        Rprintf("Making %s unshared\n",CHAR(PRINTNAME(e)));
                }
                res = duplicate(res);
                SET_PRVALUE (prom, res);
            }
            SET_NAMEDCNT_NOT_0(res);
        }

        else {
            if (NAMEDCNT_GT_1(res) && R_binding_cell != R_NilValue
              && isVectorAtomic(res) && LENGTH(res) == 1 && !HAS_ATTRIB(res)) {
                if (0) { /* Enable for debugging */
                    if (installed_already("UNSHARED.DEBUG") != R_NoObject)
                        Rprintf("Making %s unshared\n",CHAR(PRINTNAME(e)));
                }
                res = duplicate(res);
                SETCAR (R_binding_cell, res);
                /* DON'T clear MISSING, though may not get here if it matters */
            }
            SET_NAMEDCNT_NOT_0(res);
        }
    }

    return res;
}

SEXP attribute_hidden evalListUnshared(SEXP el, SEXP rho)
{
    BEGIN_PROTECT3 (head, tail, h);
    int variant = VARIANT_PENDING_OK;
    SEXP ev, ev_el;

    head = R_NilValue;
    tail = R_NilValue;

    while (el != R_NilValue) {

        if (CDR(el) == R_NilValue)
            variant = 0;  /* would need to wait for last immediately anyway */

	if (CAR(el) == R_DotsSymbol) {
            /* If we have a ... symbol, we look to see what it is bound to.
               If its binding is Null (i.e. zero length) or missing we just
               ignore it and return the cdr with all its expressions evaluated.
               If it is bound to a ... list of promises, we force all the 
               promises and then splice the list of resulting values into
               the return value. Anything else bound to a ... symbol is an 
               error. */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP) {
		while (h != R_NilValue) {
                    INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
                    ev_el = eval_unshared (CAR(h), rho, variant);
                    ev = cons_with_tag (ev_el, R_NilValue, TAG(h));
                    if (head==R_NilValue)
                        head = ev;
                    else
                        SETCDR(tail, ev);
                    tail = ev;
                    if (CAR(ev) == R_MissingArg && isSymbol(CAR(h)))
                        SET_MISSING (ev, R_isMissing(CAR(h),rho));
		    h = CDR(h);
		}
	    }
	    else if (h != R_NilValue && h != R_MissingArg)
		dotdotdot_error();

	} else {
            INC_NAMEDCNT(CAR(tail));  /* OK when tail is R_NilValue */
            ev_el = eval_unshared (CAR(el), rho, variant);
            ev = cons_with_tag (ev_el, R_NilValue, TAG(el));
            if (head==R_NilValue)
                head = ev;
            else
                SETCDR(tail, ev);
            tail = ev;
            if (CAR(ev) == R_MissingArg && isSymbol(CAR(el)))
                SET_MISSING (ev, R_isMissing(CAR(el),rho));
	}

	el = CDR(el);
    }

    for (el = head; CDR(el) != R_NilValue; el = CDR(el))
        DEC_NAMEDCNT(CAR(el));

    WAIT_UNTIL_ARGUMENTS_COMPUTED (head);

    R_variant_result = 0;
    RETURN_SEXP_INSIDE_PROTECT (head);
    END_PROTECT;

} /* evalListUnshared() */

/* Evaluate argument list, waiting for any pending computations of arguments. */

SEXP attribute_hidden evalList(SEXP el, SEXP rho)
{
    return evalList_v (el, rho, 0);
}

/* Evaluate argument list, waiting for pending computations, and with no 
   error for missing arguments. */

SEXP attribute_hidden evalListKeepMissing(SEXP el, SEXP rho)
{ 
    return evalList_v (el, rho, VARIANT_MISSING_OK);
}


/* -------------------------------------------------------------------------- */
/*                          SCALAR STACK SUPPORT                              */

/* scalar_stack_eval2 is an inline function used by operators that can
   take operands on the scalar stack and can handle unclassed objects
   (VARIANT_UNCLASS_FLAG).

   Evaluates two arguments that may be put on the scalar stack.  These
   two arguments are returned in arg1 and arg2, and whether they are
   objects (accounting for VARIANT_UNCLASS_FLAG) in the two lowest
   bits of obj.  If one of the first two arguments is an object, a
   list of the evaluated arguments is returned as the value of the
   function, so that dispatch can be attempted (note that the
   argument count in this case may not be two).  If neither is an
   object, a list with the correct number of arguments is returned,
   but they may (or may not) be the unevaluated arguments.

   If an argument is an object, all arguments will have been computed
   before return from this function, but evaluation of arguments may 
   be pending if neither operand is an object.

   Note that if there are less than two arguments, the missing ones will
   appear here to be R_NilValue (since CAR(R_NilValue) is R_NilValue).

   The args and env arguments must be protected by the caller. 

   Two non-inlined routines below are used for the uncommon cases where
   operands are objects. */

static attribute_noinline SEXP scalar_stack_eval2_xobj 
 (SEXP x, SEXP args, SEXP env)  /* x protect at start, unprotected here */
{
    /* If first argument is an object, we evaluate the rest of the
       arguments (actually, at most one) normally. */

    SEXP argsevald;
    INC_NAMEDCNT(x);
    argsevald = evalList (CDR(args), env);
    argsevald = cons_with_tag (x, argsevald, TAG(args));
    DEC_NAMEDCNT(x);
    UNPROTECT(1); /* x */
    WAIT_UNTIL_COMPUTED(x);
    return argsevald;
}

static attribute_noinline SEXP scalar_stack_eval2_yobj 
 (SEXP x, SEXP y, SEXP args, SEXP env) /* x protect at start, unprotected here*/
{
    /* If the second argument is an object, we have to duplicate the
       first arg if it is on the scalar stack, or an unclassed object,
       and create the list of evaluated arguments. */

    if (ON_SCALAR_STACK(x) || isObject(x)) /* can't be both */ {
        UNPROTECT_PROTECT(y); /* unprotects x */
        if (ON_SCALAR_STACK(x)) {
            POP_SCALAR_STACK(x);
            x = duplicate(x);
        }
        else { /* isObject(x) */
            x = Rf_makeUnclassed(x);
        }
        UNPROTECT_PROTECT(x); /* unprotects y */
    }

    /* should not be any more arguments */
    SEXP argsevald;
    argsevald = cons_with_tag (y, R_NilValue, TAG(CDR(args)));
    argsevald = cons_with_tag (x, argsevald, TAG(args));
    UNPROTECT(1); /* x */
    WAIT_UNTIL_COMPUTED_2(x,y);
    return argsevald;
}

static inline SEXP scalar_stack_eval2 (SEXP args, SEXP *arg1, SEXP *arg2,
                                       int *obj, SEXP env)
{
    SEXP argsevald;
    SEXP x, y;
    int ob;

    x = CAR(args); 
    y = CADR(args);

    /* Evaluate by the general procedure if ... present, or more than
       two arguments, not trying to put arguments on the scalar stack. */

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        argsevald = evalList (args, env);
        x = CAR(argsevald);
        y = CADR(argsevald);
        ob = isObject(x) | (isObject(y)<<1);
        goto rtrn;
    }

    ob = 0;

    /* Otherwise, we try to put the first argument on the scalar stack,
       and evaluate with VARIANT_UNCLASS. */

    PROTECT(x = EVALV (x, env, 
             VARIANT_SCALAR_STACK_OK | VARIANT_UNCLASS | VARIANT_PENDING_OK));

    if (isObject(x)) {
        if (! (R_variant_result & VARIANT_UNCLASS_FLAG)) {
            argsevald = scalar_stack_eval2_xobj (x, args, env);
            y = CADR(argsevald);
            ob = 1;  /* x is an object, not unclassed */
            if (isObject(y)) ob |= 2;
            goto rtrn;
        }
    }

    /* If there's no second argument, we can return now. */

    if (y == R_NilValue) {
        UNPROTECT(1);
        argsevald = args;
        goto rtrn;
    }

    /* Now we evaluate the second argument, also allowing it to be on the
       scalar stack, again with VARIANT_UNCLASS. */

    if (SELF_EVAL(TYPEOF(y))) {
        R_variant_result = 0;
        SET_NAMEDCNT_MAX(y);
    }
    else {
        INC_NAMEDCNT(x);
        y = EVALV_NC (y, env, 
              VARIANT_UNCLASS | VARIANT_SCALAR_STACK_OK | VARIANT_PENDING_OK);
        DEC_NAMEDCNT(x);
    }

    if (isObject(y)) {
        if (! (R_variant_result & VARIANT_UNCLASS_FLAG)) {
            argsevald = scalar_stack_eval2_yobj (x, y, args, env);
            x = CAR(argsevald);
            y = CADR(argsevald);
            ob = 2;  /* y is an object, not unclassed */
            goto rtrn;
        }
    }

    /* If neither of the first two arguments are an object (or have
       been unclassed), we don't look at any possible remaining
       arguments.  The caller is responsible for reporting an error if
       any are present, but we assist by returning the unevaluated
       arguments, which in this case (no ...) number the same as the
       actual arguments. */

    UNPROTECT(1); /* x */
    argsevald = args;

  rtrn:

    R_variant_result = 0;

    *arg1 = x;
    *arg2 = y;

    *obj = ob;

    return argsevald;
}


/* -------------------------------------------------------------------------- */
/*                        ARITHMETIC OPERATORS.                               */
/*                                                                            */
/* All but simple cases are handled in R_unary and R_binary in arithmetic.c.  */
/* do_arith1 handles binary and unary + and -.  do_arith2 handles the binary  */
/* *, /, ^, %%, and %/% operators.                                            */

static SEXP do_arith1 (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int opcode = PRIMVAL(op);

    SEXP argsevald, ans, arg1, arg2, grad1, grad2;
    SEXP sv_scalar_stack = 0;
    int obj;

    if (variant & VARIANT_GRADIENT) {

        /* FOR NOW:  Evaluate by evalList_gradient if gradients are
           desired, with any gradients being attached as attributes of
           the CONS cells of the argument list. */

        argsevald = evalList_gradient (args, env, VARIANT_PENDING_OK, 2, 0);
        arg1 = CAR(argsevald);
        arg2 = CADR(argsevald);
        obj = isObject(arg1) | (isObject(arg2)<<1);
    }
    else {

        /* Evaluate arguments, maybe putting them on the scalar stack. */

        sv_scalar_stack = R_scalar_stack;
        argsevald = scalar_stack_eval2(args, &arg1, &arg2, &obj, env);
    }

    PROTECT3(argsevald,arg1,arg2);

    /* Check for dispatch on S3 or S4 objects. */

    if (obj) { /* one or other or both operands are objects */
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans, variant)) {
            UNPROTECT(3);
            R_Visible = TRUE;
            return ans;
        }
    }

    /* Check for argument count error (not before dispatch, since other
       methods may have different requirements).  Only check for more than
       two at this point - check for simple cases will fail for other
       argument count errors, so do those checks later. */

    if (CDDR(argsevald) != R_NilValue) goto arg_count_err;


    /* FOR NOW:  Handle gradients with the general-case R_unary and R_binary
       procedures. */

    if (variant & VARIANT_GRADIENT) {
        grad1 = GRADIENT_IN_CELL (argsevald);
        grad2 = GRADIENT_IN_CELL (CDR(argsevald));
        goto gradient;
    }

    /* Arguments are now in arg1 and arg2, and are protected. They may
       be on the scalar stack, but if so, are removed now, though they
       may still be referenced.  Note that result might be on top of
       one of them - OK since after storing into it, the args won't be
       accessed again.

       Below same as POP_IF_TOP_OF_STACK(arg2); POP_IF_TOP_OF_STACK(arg1);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    /* We quickly do real arithmetic and integer plus/minus/times on
       scalars with no attributes (as will be the case for scalar
       stack values), or attributes that will be ignored.  We don't
       bother trying local assignment, since returning the result on
       the scalar stack should be about as fast. */

    char typeplus1 = TYPE_ETC(arg1);
    char typeplus2 = TYPE_ETC(arg2);

    double a1, a2;  /* the two operands, if real */
    int i1;         /* the first operand, if integer */

    if (typeplus2 == NILSXP && CDR(argsevald) == R_NilValue) { /* Unary op */

        /* Test if arg1 is scalar numeric, computation not pending, attr OK */

      retry_unary:

        if (typeplus1 == REALSXP) {

            double val = opcode == PLUSOP ? *REAL(arg1) : -*REAL(arg1);

            ans = NAMEDCNT_EQ_0(arg1) ?           (*REAL(arg1) = val, arg1)
                : CAN_USE_SCALAR_STACK(variant) ? PUSH_SCALAR_REAL(val)
                :                                 ScalarReal(val);

            goto ret;
        }

        if (typeplus1 == INTSXP || typeplus1 == LGLSXP) {

            i1 = *INTEGER(arg1);
            int val = i1 == NA_INTEGER ? NA_INTEGER : opcode==PLUSOP ? i1 : -i1;

            ans = NAMEDCNT_EQ_0(arg1) ?           (*INTEGER(arg1) = val, arg1)
                : CAN_USE_SCALAR_STACK(variant) ? PUSH_SCALAR_INTEGER(val)
                :                                 ScalarInteger(val);

            goto ret;
        }

        if ((variant & VARIANT_ANY_ATTR)
              && (typeplus1 & TYPE_ET_CETERA_HAS_ATTR)) {
            typeplus1 &= ~TYPE_ET_CETERA_HAS_ATTR;
            goto retry_unary;
        }

        goto general;
    }

  retry_binary:

    if (typeplus2 == REALSXP) {
        a2 = *REAL(arg2);
        if (typeplus1 == REALSXP) {
            a1 = *REAL(arg1);
        }
        else if (typeplus1 == INTSXP || typeplus1 == LGLSXP) {
            i1 = *INTEGER(arg1);
            if (i1 == NA_INTEGER) {
                ans = R_ScalarRealNA;
                goto ret;
            }
            a1 = (double) i1;
        }
        else
            goto general;
    }
    else if (typeplus2 == INTSXP || typeplus2 == LGLSXP) {
        int i2 = *INTEGER(arg2);
        if (typeplus1 == REALSXP) {
            if (i2 == NA_INTEGER) {
                ans = R_ScalarRealNA;
                goto ret;
            }
            a1 = *REAL(arg1);
            a2 = (double) i2;
        }
        else if (typeplus1 == INTSXP || typeplus1 == LGLSXP) {

            /* neither operand is real */

            i1 = *INTEGER(arg1);
            if (i1 == NA_INTEGER || i2 == NA_INTEGER) {
                ans = R_ScalarIntegerNA;
                goto ret;
            }

            int_fast64_t val;

            if (opcode==PLUSOP)
                val = (int_fast64_t)i1 + (int_fast64_t)i2;
            else
                val = (int_fast64_t)i1 - (int_fast64_t)i2;

            if (val < R_INT_MIN || val > R_INT_MAX) {
                val = NA_INTEGER;
                warningcall (call, _("NAs produced by integer overflow"));
            }

            int ival = (int) val;

            ans = NAMEDCNT_EQ_0(arg2) && typeplus2 == INTSXP ? 
                                                   (*INTEGER(arg2) = ival, arg2)
                : NAMEDCNT_EQ_0(arg1) && typeplus1 == INTSXP ?
                                                   (*INTEGER(arg1) = ival, arg1)
                : CAN_USE_SCALAR_STACK(variant) ?  PUSH_SCALAR_INTEGER(ival) 
                :                                  ScalarInteger(ival);
  
            goto ret;
        }
        else
            goto general;
    }
    else if ((variant & VARIANT_ANY_ATTR)
               && ((typeplus1 | typeplus1) & TYPE_ET_CETERA_HAS_ATTR)) {
        typeplus1 &= ~TYPE_ET_CETERA_HAS_ATTR;
        typeplus2 &= ~TYPE_ET_CETERA_HAS_ATTR;
        goto retry_binary;
    }
    else
        goto general;

    /* Do the operation on scalar reals. */
    
    double val;

    if (opcode == PLUSOP)
        val = a1 + a2;
    else
        val = a1 - a2;

    ans = NAMEDCNT_EQ_0(arg2) && typeplus2==REALSXP ? (*REAL(arg2) = val, arg2)
        : NAMEDCNT_EQ_0(arg1) && typeplus1==REALSXP ? (*REAL(arg1) = val, arg1)
        : CAN_USE_SCALAR_STACK(variant) ?             PUSH_SCALAR_REAL(val)
        :                                             ScalarReal(val);

    goto ret;

    /* Handle the general case. */

  general:

    grad1 = grad2 = R_NilValue;

  gradient:

    if (CDR(argsevald) != R_NilValue)
        ans = R_binary (call, opcode, arg1, arg2, obj&1, obj>>1, 
                        grad1, grad2, env, variant);
    else {
        if (argsevald == R_NilValue) goto arg_count_err;
        ans = R_unary (call, opcode, arg1, obj, grad1, env, variant);
    }

  ret:
    R_Visible = TRUE;
    UNPROTECT(3);
    return ans;

  arg_count_err:
    errorcall(call,_("operator needs one or two arguments"));
}

static SEXP do_arith2 (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int opcode = PRIMVAL(op);

    SEXP argsevald, ans, arg1, arg2, grad1, grad2;
    SEXP sv_scalar_stack = 0;
    int obj;

    if (variant & VARIANT_GRADIENT) {

        /* FOR NOW:  Evaluate by evalList_gradient if gradients are
           desired, with any gradients being attached as attributes of
           the CONS cells of the argument list. */

        argsevald = evalList_gradient (args, env, VARIANT_PENDING_OK, 2, 0);
        arg1 = CAR(argsevald);
        arg2 = CADR(argsevald);
        obj = isObject(arg1) | (isObject(arg2)<<1);
    }
    else {

        /* Evaluate arguments, maybe putting them on the scalar stack. */

        sv_scalar_stack = R_scalar_stack;
        argsevald = scalar_stack_eval2(args, &arg1, &arg2, &obj, env);
    }

    PROTECT3(argsevald,arg1,arg2);

    /* Check for dispatch on S3 or S4 objects. */

    if (obj) { /* one or other or both operands are objects */
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans, variant)) {
            UNPROTECT(3);
            R_Visible = TRUE;
            return ans;
        }
    }

    /* Check for argument count error (not before dispatch, since other
       methods may have different requirements).  Only check for more than
       two at this point - check for simple cases will fail for other
       argument count errors, so do those checks later. */

    if (CDDR(argsevald) != R_NilValue) goto arg_count_err;

    /* FOR NOW:  Handle gradients with the general-case R_binary procedure. */

    if (variant & VARIANT_GRADIENT) {
        grad1 = GRADIENT_IN_CELL (argsevald);
        grad2 = GRADIENT_IN_CELL (CDR(argsevald));
        goto gradient;
    }

    /* Arguments are now in arg1 and arg2, and are protected. They may
       be on the scalar stack, but if so, are removed now, though they
       may still be referenced.  Note that result might be on top of
       one of them - OK since after storing into it, the args won't be
       accessed again.

       Below same as POP_IF_TOP_OF_STACK(arg2); POP_IF_TOP_OF_STACK(arg1);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    /* We quickly do real arithmetic and integer plus/minus/times on
       scalars with no attributes (as will be the case for scalar
       stack values), or attributes that will be ignored.  We don't
       bother trying local assignment, since returning the result on
       the scalar stack should be about as fast. */

    char typeplus1 = TYPE_ETC(arg1);
    char typeplus2 = TYPE_ETC(arg2);

    double a1, a2;  /* the two operands, if real */
    int i1;         /* the first operand, if integer */

  retry:

    if (typeplus2 == REALSXP) {
        a2 = *REAL(arg2);
        if (typeplus1 == REALSXP) {
            a1 = *REAL(arg1);
        }
        else if (typeplus1 == INTSXP || typeplus1 == LGLSXP) {
            i1 = *INTEGER(arg1);
            if (i1 == NA_INTEGER) {
                ans = R_ScalarRealNA;
                goto ret;
            }
            a1 = (double) i1;
        }
        else
            goto general;
    }
    else if (typeplus2 == INTSXP || typeplus2 == LGLSXP) {
        if (typeplus1 == REALSXP) {
            if (*INTEGER(arg2) == NA_INTEGER) {
                ans = R_ScalarRealNA;
                goto ret;
            }
            a1 = *REAL(arg1);
            a2 = (double) *INTEGER(arg2);
        }
        else
            goto general;
    }
    else if ((variant & VARIANT_ANY_ATTR)
               && ((typeplus1 | typeplus1) & TYPE_ET_CETERA_HAS_ATTR)) {
        typeplus1 &= ~TYPE_ET_CETERA_HAS_ATTR;
        typeplus2 &= ~TYPE_ET_CETERA_HAS_ATTR;
        goto retry;
    }
    else
        goto general;

    /* Do the operation on scalar reals. */
    
    double val;

    switch (opcode) {
    case TIMESOP:
        val = a1 * a2;
        break;
    case DIVOP:
        val = a1 / a2;
        break;
    case POWOP:
        if (a2 == 2.0)       val = a1 * a1;
        else if (a2 == 1.0)  val = a1;
        else if (a2 == 0.0)  val = 1.0;
        else if (a2 == -1.0) val = 1.0 / a1;
        else                 val = R_pow(a1,a2);
        break;
    case MODOP:
        val = myfmod(a1,a2);
        break;
    case IDIVOP:
        val = myfloor(a1,a2);
        break;
    default: abort();
    }

    ans = NAMEDCNT_EQ_0(arg2) && typeplus2==REALSXP ? (*REAL(arg2) = val, arg2)
        : NAMEDCNT_EQ_0(arg1) && typeplus1==REALSXP ? (*REAL(arg1) = val, arg1)
        : CAN_USE_SCALAR_STACK(variant) ?             PUSH_SCALAR_REAL(val)
        :                                             ScalarReal(val);

    goto ret;

    /* Handle the general case. */

  general:

    grad1 = grad2 = R_NilValue;

  gradient:

    if (CDR(argsevald) == R_NilValue) goto arg_count_err;

    ans = R_binary (call, opcode, arg1, arg2, obj&1, obj>>1, 
                    grad1, grad2, env, variant);

  ret:
    R_Visible = TRUE;
    UNPROTECT(3);
    return ans;

  arg_count_err:
    errorcall(call,_("operator needs one or two arguments"));
}


/* -------------------------------------------------------------------------- */
/*                       RELATIONAL OPERATORS.                                */
/*                                                                            */
/* Main work (except for simple scalar reals) is done in R_relop, in relop.c. */

static SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP argsevald, ans, x, y;
    int obj;

    /* Evaluate arguments, maybe putting them on the scalar stack. */

    SEXP sv_scalar_stack = R_scalar_stack;

    argsevald = scalar_stack_eval2 (args, &x, &y, &obj, env);
    PROTECT3(argsevald,x,y);

    /* Check for dispatch on S3 or S4 objects. */

    if (obj) {
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans, variant)) {
            R_Visible = TRUE;
            UNPROTECT(3);
            return ans;
        }
    }

    /* Check argument count after dispatch, since other methods may allow
       other argument count.  Only check for too many now, since check
       for simple cases will fail on too few. */

    if (CDDR(argsevald) != R_NilValue) goto arg_count_err;

    int opcode = PRIMVAL(op);

    R_Visible = TRUE;

    /* Arguments are now in x and y, and are protected.  They may be on
       the scalar stack, but if so are popped off here (but retain their
       values if eval is not called). */

    /* Below does same as POP_IF_TOP_OF_STACK(y); POP_IF_TOP_OF_STACK(x);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    /* Handle numeric scalars specially for speed. */

    int typeplusx = TYPE_ETC(x);
    int typeplusy = TYPE_ETC(y);

    double xv, yv;  /* the two operands */

  retry_x:

    if (typeplusx == REALSXP)
        xv = *REAL(x);
    else if ((typeplusx == INTSXP || typeplusx == LGLSXP)
               && *INTEGER(x) != NA_INTEGER)
        xv = (double) *INTEGER(x);
    else if ((variant & VARIANT_ANY_ATTR) 
               && (typeplusx & TYPE_ET_CETERA_HAS_ATTR)) {
        typeplusx &= ~TYPE_ET_CETERA_HAS_ATTR;
        goto retry_x;
    }
    else
        goto general;

  retry_y:

    if (typeplusy == REALSXP)
        yv = *REAL(y);
    else if ((typeplusy == INTSXP || typeplusy == LGLSXP)
               && *INTEGER(y) != NA_INTEGER)
        yv = (double) *INTEGER(y);
    else if ((variant & VARIANT_ANY_ATTR) 
               && (typeplusy & TYPE_ET_CETERA_HAS_ATTR)) {
        typeplusy &= ~TYPE_ET_CETERA_HAS_ATTR;
        goto retry_y;
    }
    else
        goto general;

    /* Do the comparison on scalar reals (possibly converted from int) */

    int res;

    if (MAY_BE_NAN2(xv,yv) && (ISNAN(xv) || ISNAN(yv)))
        res = NA_LOGICAL;
    else {
        switch (opcode) {
        case EQOP: res = xv == yv; break;
        case NEOP: res = xv != yv; break;
        case LTOP: res = xv < yv; break;
        case GTOP: res = xv > yv; break;
        case LEOP: res = xv <= yv; break;
        case GEOP: res = xv >= yv; break;
        }
    }
    ans = ScalarLogicalMaybeConst (res);
    goto ret;

  general:

    if (CDR(argsevald) == R_NilValue) goto arg_count_err;

    ans = R_relop (call, opcode, x, y, obj&1, obj>>1, env, variant);

  ret:
    UNPROTECT(3);
    return ans;

  arg_count_err:
    checkArity(op,argsevald);  /* will report the error, not return */
    return R_NoObject;         /* never executed */
}


/* -------------------------------------------------------------------------- */
/*                         LOGICAL OPERATORS                                  */

#define SWAP12(s1,n1,s2,n2) \
 do { SEXP st = s1; s1 = s2; s2 = st; int nt = n1; n1 = n2; n2 = nt; } while (0)

static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);


void task_and_or (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int * restrict lans = LOGICAL(ans);

    int n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = LENGTH(ans);

    if (n2 < n1) SWAP12(s1,n1,s2,n2);

    switch (code) {
    case 1:  /* & operator */
        if (n1 == n2) {
            for (R_len_t i = 0; i<n; i++) {
                uint32_t u1 = LOGICAL(s1)[i];
                uint32_t u2 = LOGICAL(s2)[i];
                uint32_t u = u1 | u2;
                lans[i] = (u1 & u2) | (u & (u << 31));
            }
        }
        else {  /* n1 < n2 */
            mod_iterate_1(n1,n2,i1,i2) {
                uint32_t u1 = LOGICAL(s1)[i1];
                uint32_t u2 = LOGICAL(s2)[i2];
                uint32_t u = u1 | u2;
                lans[i] = (u1 & u2) | (u & (u << 31));
            }
        }
        break;
    case 2:  /* | operator */
        if (n1 == n2) {
            for (R_len_t i = 0; i<n; i++) {
                uint32_t u = LOGICAL(s1)[i] | LOGICAL(s2)[i];
                lans[i] = u & ~ (u << 31);
            }
        }
        else {  /* n1 < n2 */
            mod_iterate_1(n1,n2,i1,i2) {
                uint32_t u = LOGICAL(s1)[i1] | LOGICAL(s2)[i2];
                lans[i] = u & ~ (u << 31);
            }
        }
        break;
    }
}


/* Handles the & and | operators. */

#define T_and_or THRESHOLD_ADJUST(100)

SEXP attribute_hidden do_andor(SEXP call, SEXP op, SEXP args, SEXP env, 
                               int variant)
{
    PROTECT_INDEX xpi, ypi;
    SEXP ans, x, y;
    int args_evald;

    /* Evaluate arguments, setting x to first argument and y to
       second argument.  The whole argument list is in args, already 
       evaluated if args_evald is 1. */

    x = CAR(args); 
    y = CADR(args);

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        args = evalList (args, env);
        x = CAR(args); 
        y = CADR(args);
        PROTECT_WITH_INDEX(x, &xpi);
        PROTECT_WITH_INDEX(y, &ypi);
        args_evald = 1;
    }
    else {
        x = EVALV_NC (x, env, VARIANT_PENDING_OK);
        PROTECT_WITH_INDEX(x, &xpi);
        INC_NAMEDCNT(x);
        y = EVALV_NC (y, env, VARIANT_PENDING_OK);
        PROTECT_WITH_INDEX(y, &ypi);
        DEC_NAMEDCNT(x);
        args_evald = 0;
    }

    /* Check for dispatch on S3 or S4 objects.  Takes care to match length
       of "args" to length of original (number of args in "call"). */

    if (isObject(x) || isObject(y)) {
        if (!args_evald) {
            args = CDR(args)!=R_NilValue ? CONS(x,CONS(y,R_NilValue)) 
                                         : CONS(x,R_NilValue);
            WAIT_UNTIL_COMPUTED_2(x,y);
        }
        PROTECT(args);
        if (DispatchGroup("Ops", call, op, args, env, &ans, variant)) {
            UNPROTECT(3);
            R_Visible = TRUE;
            return ans;
        }
        UNPROTECT(1);
    }

    /* Check argument count now (after dispatch, since other methods may allow
       other argument count). */

    if (CDR(args) == R_NilValue || CDDR(args) != R_NilValue)
        checkArity(op,args);  /* to report the error */

    /* Arguments are now in x and y, and are protected.  The value 
       in args may not be protected, and is not used below. */

    R_Visible = TRUE;

    SEXP dims, tsp, klass, xnames, ynames;
    int xarray, yarray, xts, yts;

    if (! (isRaw(x) && isRaw(y)) && ! (isNumber(x) && isNumber(y)))
        errorcall (call,
       _("operations are possible only for numeric, logical or complex types"));

    R_len_t nx = LENGTH(x);
    R_len_t ny = LENGTH(y);

    tsp = R_NilValue;		/* -Wall */
    klass = R_NilValue;		/* -Wall */
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);

    /* If either x or y is a matrix with length 1 and the other is a
       vector, we want to coerce the matrix to be a vector. */

    if (xarray != yarray) {
        if (xarray && nx==1 && ny!=1) {
            REPROTECT(x = duplicate(x), xpi);
            setAttrib(x, R_DimSymbol, R_NilValue);
            xarray = FALSE;
        }
        if (yarray && ny==1 && nx!=1) {
            REPROTECT(y = duplicate(y), ypi);
            setAttrib(y, R_DimSymbol, R_NilValue);
            yarray = FALSE;
        }
    }

    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		error(_("non-conformable arrays"));
	    dims = getDimAttrib(x);
	}
	else if (xarray) {
	    dims = getDimAttrib(x);
	}
	else /*(yarray)*/ {
	    dims = getDimAttrib(y);
	}
        PROTECT(dims);
	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }

    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, _("non-conformable time series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getClassAttrib(x));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getClassAttrib(x));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getClassAttrib(y));
	}
    }

    if (nx > 0 && ny > 0 && (nx > ny ? nx % ny : ny % nx))
        warningcall(call,
         _("longer object length is not a multiple of shorter object length"));

    R_len_t n = nx == 0 || ny == 0 ? 0 : nx > ny ? nx : ny;

    if (isRaw(x) && isRaw(y)) {
        WAIT_UNTIL_COMPUTED_2(x,y);
        PROTECT(ans = binaryLogic2(PRIMVAL(op), x, y));
    }
    else if (n == 0) {
        PROTECT (ans = allocVector (LGLSXP, 0));
    }
    else {
        PROTECT(ans = allocVector (LGLSXP, n));

        if (!isLogical(x) || !isLogical(y)) {
            WAIT_UNTIL_COMPUTED_2(x,y);
            PROTECT(x = coerceVector(x, LGLSXP));
            y = coerceVector(y, LGLSXP);
            UNPROTECT(1);
        }

        DO_NOW_OR_LATER2 (variant, n >= T_and_or, 
                          0, task_and_or, PRIMVAL(op), ans, x, y);
    }

    /* With zero-length result, dims needn't match */
    if (dims != R_NilValue && n == 0) {
        int i;
        for (i = 0; i < LENGTH(dims); i++) 
            if (INTEGER(dims)[i] == 0) break;
        if (i == LENGTH(dims)) /* none zero */
            dims = R_NilValue;
    }

    if (dims != R_NilValue) {
	setAttrib (ans, R_DimSymbol, dims);
	if (xnames != R_NilValue)
	    setAttrib(ans, R_DimNamesSymbol, xnames);
	else if (ynames != R_NilValue)
	    setAttrib(ans, R_DimNamesSymbol, ynames);
    }
    else {
	if (LENGTH(ans) == length(xnames))
	    setAttrib (ans, R_NamesSymbol, xnames);
	else if (LENGTH(ans) == length(ynames))
	    setAttrib (ans, R_NamesSymbol, ynames);
    }

    if (xts || yts) {
	setAttrib(ans, R_TspSymbol, tsp);
	setAttrib(ans, R_ClassSymbol, klass);
	UNPROTECT(2);
    }

    UNPROTECT(6);
    return ans;
}

void task_not (helpers_op_t code, SEXP x, SEXP arg, SEXP unused)
{
    int len = LENGTH(arg);
    int i;

    switch(TYPEOF(arg)) {
    case LGLSXP:
        for (i = 0; i < len; i++) {
            uint32_t u = LOGICAL(arg)[i];
            LOGICAL(x)[i] = u ^ 1 ^ (u >> 31);
        }
        break;
    case INTSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (INTEGER(arg)[i] == NA_INTEGER) ? NA_LOGICAL 
                          : INTEGER(arg)[i] == 0;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = ISNAN(REAL(arg)[i]) ? NA_LOGICAL 
                          : REAL(arg)[i] == 0;
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = ISNAN(COMPLEX(arg)[i].r) || ISNAN(COMPLEX(arg)[i].i)
              ? NA_LOGICAL : (COMPLEX(arg)[i].r == 0 && COMPLEX(arg)[i].i == 0);
	break;
    case RAWSXP:
	for (i = 0; i < len; i++)
	    RAW(x)[i] = ~ RAW(arg)[i];
	break;
    }
}


/* Handles the ! operator.  When unary, it's "not", when binary, it's paste0. */

#define T_not THRESHOLD_ADJUST(100)

static SEXP do_fast_not(SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    /* Quickly do scalar operation on logical with no attributes. */

    if (TYPE_ETC(arg) == LGLSXP) {
        int v = LOGICAL(arg)[0];
        return ScalarLogicalMaybeConst (v==NA_LOGICAL ? NA_LOGICAL : !v);
    }

    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (length(arg)==0) 
            return allocVector(LGLSXP, 0);
	else
            errorcall(call, _("invalid argument type"));
    }

    R_len_t len = LENGTH(arg);

    SEXP x, dim, dimnames, names;

    /* The general case... */

    if (TYPEOF(arg) != LGLSXP && TYPEOF(arg) != RAWSXP)
        x = allocVector(LGLSXP,len);
    else if (isObject(arg) || NAMEDCNT_GT_0(arg))
        x = duplicate(arg);
    else
        x = arg;

    if (!isVectorAtomic(arg) || TYPEOF(arg) == STRSXP)
	UNIMPLEMENTED_TYPE("do_fast_not", arg);

    DO_NOW_OR_LATER1 (variant, len >= T_not, 0, task_not, 0, x, arg);

    if (TYPEOF(arg) != LGLSXP && TYPEOF(arg) != RAWSXP) {
        if (!NO_ATTRIBUTES_OK(variant,arg)) {
            PROTECT(x);
            PROTECT (names    = getAttrib (arg, R_NamesSymbol));
            PROTECT (dim      = getDimAttrib(arg));
            PROTECT (dimnames = getAttrib (arg, R_DimNamesSymbol));
            if (names    != R_NilValue) setAttrib(x,R_NamesSymbol,    names);
            if (dim      != R_NilValue) setAttrib(x,R_DimSymbol,      dim);
            if (dimnames != R_NilValue) setAttrib(x,R_DimNamesSymbol, dimnames);
            UNPROTECT(4);
        }
    }

    return x;
}

SEXP attribute_hidden do_not (SEXP call, SEXP op, SEXP args, SEXP env, 
                              int variant)
{
    if (CDR(args) != R_NilValue)  /* more than one arg, so paste0, not not */
        return do_paste_bang (call, op, args, env);

    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans, variant)) {
	return ans;
    }

    if (args == R_NilValue)
        checkArity(op,args);  /* to report the error */

    return do_fast_not (call, op, CAR(args), env, variant);
}


/* Handles the && (op 1) and || (op 2) operators. */

SEXP attribute_hidden do_andor2(SEXP call, SEXP op, SEXP args, SEXP env,
                                int variant)
{
    int ov = PRIMVAL(op);

    SEXP s1, s2;
    int x1, x2;

    if (args==R_NilValue || CDR(args)==R_NilValue || CDDR(args)!=R_NilValue)
	error(_("'%s' operator requires 2 arguments"), ov == 1 ? "&&" : "||");

    s1 = EVALV_NC (CAR(args), env, VARIANT_ANY_ATTR);
    if (!isNumber(s1))
	errorcall(call, _("invalid 'x' type in 'x %s y'"), ov==1 ? "&&" : "||");

    x1 = TYPEOF(s1) == LGLSXP && LENGTH(s1) == 1 ? *LOGICAL(s1) : asLogical(s1);

    if (ov==1 && x1==FALSE)  /* FALSE && ... */
        return ScalarLogicalMaybeConst(FALSE);

    if (ov==2 && x1==TRUE)   /* TRUE || ... */
        return ScalarLogicalMaybeConst(TRUE);
    
    s2  = EVALV_NC (CADR(args), env, VARIANT_ANY_ATTR);
    if (!isNumber(s2))	
        errorcall(call, _("invalid 'y' type in 'x %s y'"), ov==1 ? "&&" : "||");

    x2 = TYPEOF(s2) == LGLSXP && LENGTH(s2) == 1 ? *LOGICAL(s2) : asLogical(s2);

    R_Visible = TRUE;

    if (ov==1) /* ... && ... */
        return ScalarLogicalMaybeConst (x2==FALSE ? FALSE
                                  : x1==TRUE && x2==TRUE ? TRUE
                                  : NA_LOGICAL);
    else /* ... || ... */
        return ScalarLogicalMaybeConst (x2==TRUE ? TRUE
                                  : x1==FALSE && x2==FALSE ? FALSE
                                  : NA_LOGICAL);
}

static SEXP binaryLogic2(int code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(RAWSXP, 0);
	return ans;
    }
    ans = allocVector(RAWSXP, n);

    if (n2 < n1) SWAP12(s1,n1,s2,n2);

    switch (code) {
    case 1:  /* & : AND */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] & RAW(s2)[i];
        }
        else {  /* n1 < n2 */
            mod_iterate_1(n1,n2,i1,i2)
                RAW(ans)[i] = RAW(s1)[i1] & RAW(s2)[i2];
        }
	break;
    case 2:  /* | : OR */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] | RAW(s2)[i];
        }
        else {  /* n1 < n2 */
            mod_iterate_1(n1,n2,i1,i2)
                RAW(ans)[i] = RAW(s1)[i1] | RAW(s2)[i2];
        }
	break;
    }
    return ans;
}

#define OP_ALL 1
#define OP_ANY 2

static int any_all_check (int op, int na_rm, int *x, int n)
{
    int i = 0;

    if (op == OP_ANY) {
        unsigned na = 0;
        if (n & 1) {
            na |= x[i];
            if (na & 1) 
                return TRUE;
            i += 1;
        }
        if (n & 2) {
            na |= x[i];
            na |= x[i+1];
            if (na & 1) 
                return TRUE;
            i += 2;
        }
        if (n & 4) {
            na |= x[i];
            na |= x[i+1];
            na |= x[i+2];
            na |= x[i+3];
            if (na & 1) 
                return TRUE;
            i += 4;
        }
        while (i < n) {
            na |= x[i];
            na |= x[i+1];
            na |= x[i+2];
            na |= x[i+3];
            na |= x[i+4];
            na |= x[i+5];
            na |= x[i+6];
            na |= x[i+7];
            if (na & 1) 
                return TRUE;
            i += 8;
        }

        return na_rm || (na>>31) == 0 ? FALSE : NA_LOGICAL;
    }
    else { /* OP_ALL */
        uint64_t na = 0;
        if (n & 1) {
            if (x[i] == FALSE)
                return FALSE;
            na |= x[i];
            i += 1;
        }
        if (n & 2) {
            if (x[i] == FALSE)
                return FALSE;
            na |= x[i];
            if (x[i+1] == FALSE)
                return FALSE;
            na |= x[i+1];
            i += 2;
        }
        if (n & 4) {
            uint64_t s;
            s = (unsigned) x[i];
            s += (unsigned) x[i+1];
            s += (unsigned) x[i+2];
            s += (unsigned) x[i+3];
            na |= s;
            if ((uint8_t)(s + (s>>31)) != 4)
                return FALSE;
            i += 4;
        }
        while (i < n) {
            uint64_t s;
            s = (unsigned) x[i];
            s += (unsigned) x[i+1];
            s += (unsigned) x[i+2];
            s += (unsigned) x[i+3];
            s += (unsigned) x[i+4];
            s += (unsigned) x[i+5];
            s += (unsigned) x[i+6];
            s += (unsigned) x[i+7];
            na |= s;
            if ((uint8_t)(s + (s>>31)) != 8)
                return FALSE;
            i += 8;
        }

        return na_rm || (na>>31) == 0 ? TRUE : NA_LOGICAL;
    }
}


/* fast version handles only one unnamed argument, so narm is FALSE. */

static SEXP do_fast_allany (SEXP call, SEXP op, SEXP arg, SEXP env, 
                            int variant)
{
    int val;

    if (length(arg) == 0)
        /* Avoid memory waste from coercing empty inputs, and also
           avoid warnings with empty lists coming from sapply */
        val = PRIMVAL(op) == OP_ALL ? TRUE : FALSE;

    else {
	if (TYPEOF(arg) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if (TYPEOF(arg) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(arg)));
	    arg = coerceVector(arg, LGLSXP);
	}
        if (LENGTH(arg) == 1) /* includes variant return of AND or OR of vec */
            val = LOGICAL(arg)[0];
        else
            val = any_all_check (PRIMVAL(op), FALSE, LOGICAL(arg), LENGTH(arg));
    }

    return ScalarLogicalMaybeConst(val);
}

static SEXP do_allany(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    int val = PRIMVAL(op) == OP_ALL ? TRUE : FALSE;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = LCONS(CAR(call),args));

    if (DispatchGroup("Summary", call2, op, args, env, &ans, 0)) {
	UNPROTECT(2);
	return(ans);
    }

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(length(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(t)));
	    t = coerceVector(t, LGLSXP);
	}
	val = any_all_check (PRIMVAL(op), narm, LOGICAL(t), LENGTH(t));
        if (val == NA_LOGICAL)
            has_na = 1;
        else {
            if (PRIMVAL(op) == OP_ANY && val || PRIMVAL(op) == OP_ALL && !val) {
                has_na = 0;
                break;
            }
        } 
    }
    UNPROTECT(2);
    return ScalarLogicalMaybeConst (has_na ? NA_LOGICAL : val);
}


/* -------------------------------------------------------------------------- */
/*                           SUBSET OPERATORS.                                */

static SEXP do_subset(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    int argsevald = 0;
    SEXP array_grad = R_NilValue;
    SEXP array, ans;

    /* If we can easily determine that this will be handled by
       subset_dflt and has one or two index arguments in total, try to
       evaluate the first index with VARIANT_SEQ, so it may come as a
       range rather than a vector.  Also, evaluate the array with
       VARIANT_UNCLASS and VARIANT_PENDING_OK, and perhaps evaluate
       indexes with VARIANT_SCALAR_STACK (should be safe, since there
       will be no later call of eval). */

    if (CAR(args) != R_DotsSymbol) {

        SEXP ixlist = CDR(args);

        array = EVALV_NC (CAR(args), rho, variant & VARIANT_GRADIENT 
                   ? VARIANT_UNCLASS | VARIANT_PENDING_OK | VARIANT_GRADIENT
                   : VARIANT_UNCLASS | VARIANT_PENDING_OK);

        if (R_variant_result & VARIANT_GRADIENT_FLAG) {
            array_grad = R_gradient;
            R_variant_result &= ~VARIANT_GRADIENT_FLAG;
        }

        int obj = isObject(array);
        if (R_variant_result & VARIANT_UNCLASS_FLAG) {
            obj = 0;
            R_variant_result = 0;
        }

        if (obj) {
            args = CONS(array,ixlist);
            if (array_grad != R_NilValue)
                SET_GRADIENT_IN_CELL (args, array_grad);
            argsevald = -1;
            /* handle with general code below */
        }
        else if (ixlist == R_NilValue || TAG(ixlist) != R_NilValue 
                                      || CAR(ixlist) == R_DotsSymbol) {
            PROTECT2 (array, array_grad);
            args = evalListKeepMissing(ixlist,rho);
            UNPROTECT(2);
            return do_subset_dflt_seq (call, op, array, array_grad,
                                       R_NoObject, R_NoObject,
                                       args, rho, variant, 0);
        }
        else {

            SEXP sv_scalar_stack = R_scalar_stack;
            SEXP ixlist2 = CDR(ixlist);
            SEXP r;

            BEGIN_PROTECT3 (sb1, sb2, remargs);
            ALSO_PROTECT2 (array, array_grad);

            sb1 = EVALV (CAR(ixlist), rho, 
                         ixlist2 == R_NilValue ?  /* only 1 argument */
                           VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                           VARIANT_MISSING_OK
                         : CDR(ixlist2)==R_NilValue ?  /* at most 2 arguments */
                           VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                           VARIANT_MISSING_OK | VARIANT_PENDING_OK
                         : /* more than 2 arguments */
                           VARIANT_SCALAR_STACK_OK | 
                           VARIANT_MISSING_OK | VARIANT_PENDING_OK);

            int64_t seq = 0;
            sb2 = R_NoObject;
            remargs = ixlist2;
            if (R_variant_result) {
                seq = R_variant_seq_spec;
                R_variant_result = 0;
            }

            if (ixlist2 == R_NilValue) {
                if (sb1 == R_MissingArg && isSymbol(CAR(ixlist))) {
                    remargs = CONS(sb1,R_NilValue);
                    SET_MISSING (remargs, R_isMissing(CAR(ixlist),rho));
                    sb1 = R_NoObject;
                }
                else if (!seq && isVectorAtomic(array) && !HAS_ATTRIB(array)
                           && array_grad == R_NilValue) {

                    /* Do simplest cases here */

                    R_len_t len = LENGTH(array);
                    R_len_t ix = 0;
                    if (TYPE_ETC(sb1) == INTSXP && *INTEGER(sb1) >= 1
                                                && *INTEGER(sb1) <= len)
                        ix = *INTEGER(sb1);
                    else if (TYPE_ETC(sb1) == REALSXP && *REAL(sb1) >= 1 
                                                      && *REAL(sb1) <= len)
                        ix = (int) *REAL(sb1);
                    if (ix != 0) {
                        ix -= 1;
                        WAIT_UNTIL_COMPUTED(array);
                        switch (TYPEOF(array)) {
                        case LGLSXP:
                            r = ScalarLogicalMaybeConst (LOGICAL(array)[ix]);
                            break;
                        case INTSXP:
                            if (CAN_USE_SCALAR_STACK(variant))
                                r = PUSH_SCALAR_INTEGER(INTEGER(array)[ix]);
                            else
                                r = ScalarIntegerMaybeConst(INTEGER(array)[ix]);
                            break;
                        case REALSXP:
                            if (CAN_USE_SCALAR_STACK(variant))
                                r = PUSH_SCALAR_REAL(REAL(array)[ix]);
                            else
                                r = ScalarRealMaybeConst(REAL(array)[ix]);
                            break;
                        case RAWSXP:
                            r = ScalarRawMaybeConst (RAW(array)[ix]);
                            break;
                        case STRSXP:
                            r = ScalarStringMaybeConst (STRING_ELT(array,ix));
                            break;
                        case CPLXSXP:
                            r = ScalarComplexMaybeConst (COMPLEX(array)[ix]);
                            break;
                        default: abort();
                        }
                        goto done;
                    }
                }
            }
            else {
                if (TAG(ixlist2)==R_NilValue && CAR(ixlist2) != R_DotsSymbol) {
                    INC_NAMEDCNT(sb1);
                    sb2 = EVALV (CAR(ixlist2), rho,
                                 VARIANT_SCALAR_STACK_OK | VARIANT_MISSING_OK);
                    DEC_NAMEDCNT(sb1);
                    remargs = CDR(ixlist2);
                }
                if (remargs != R_NilValue) {
                    INC_NAMEDCNT(sb1);
                    if (sb2 != R_NoObject) INC_NAMEDCNT(sb2);
                    remargs = evalList_v(remargs, rho, VARIANT_SCALAR_STACK_OK |
                                VARIANT_PENDING_OK | VARIANT_MISSING_OK);
                    DEC_NAMEDCNT(sb1);
                    if (sb2 != R_NoObject) DEC_NAMEDCNT(sb2);
                }
                if (sb2 == R_MissingArg && isSymbol(CAR(ixlist2))) {
                    remargs = CONS(sb2,remargs);
                    SET_MISSING (remargs, R_isMissing(CAR(ixlist2),rho));
                    sb2 = R_NoObject;
                }
                if (sb1 == R_MissingArg && isSymbol(CAR(ixlist))) {
                    if (sb2 != R_NoObject) {
                        remargs = CONS(sb2,remargs);
                        sb2 = R_NoObject;
                    }
                    remargs = CONS(sb1,remargs);
                    SET_MISSING (remargs, R_isMissing(CAR(ixlist),rho));
                    sb1 = R_NoObject;
                }
                if (remargs != R_NilValue) 
                    wait_until_arguments_computed(remargs);
            }

            if (sb1 != R_NoObject) WAIT_UNTIL_COMPUTED(sb1);
            r = do_subset_dflt_seq (call, op, array, array_grad, sb1, sb2,
                                    remargs, rho, variant, seq);

          done:
            R_scalar_stack = sv_scalar_stack;
            END_PROTECT;

            R_Visible = TRUE;
            return ON_SCALAR_STACK(r) ? PUSH_SCALAR(r) : r;
        }
    }

    /* If the first argument is an object and there is an appropriate
       method, we dispatch to that method, otherwise we evaluate the
       arguments and fall through to the generic code below.  Note
       that evaluation retains any missing argument indicators. */

    if (DispatchOrEval (call, op, "[", args, rho, &ans, 
                        2 /* ask for gradient of 1st argument */, 
                        argsevald, variant)) {
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);    /* IS THIS NECESSARY? */
        R_Visible = TRUE;
	return ans;
    }

    /* Method dispatch has failed, we now run the generic internal code. */

    array = CAR(ans);

    if (HAS_GRADIENT_IN_CELL(ans))
        array_grad = GRADIENT_IN_CELL(ans);

    args = CDR(ans);

    if (args == R_NilValue || TAG(args) != R_NilValue)
        return do_subset_dflt_seq (call, op, array, array_grad,
                                   R_NoObject, R_NoObject, args,
                                   rho, variant, 0);
    else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
        return do_subset_dflt_seq (call, op, array, array_grad,
                                   CAR(args), R_NoObject, CDR(args),
                                   rho, variant, 0);
    else
        return do_subset_dflt_seq (call, op, array, array_grad, 
                                   CAR(args), CADR(args), CDDR(args),
                                   rho, variant, 0);
}

static SEXP do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    int argsevald = 0;
    int fast_sub = VARIANT_KIND(variant) == VARIANT_FAST_SUB;
    SEXP array_grad = R_NilValue;
    SEXP ans;
        
    /* If we can easily determine that this will be handled by
       subset2_dflt_x, evaluate the array with VARIANT_UNCLASS and
       VARIANT_PENDING_OK, and perhaps evaluate indexes with
       VARIANT_SCALAR_STACK_OK (should be safe, since there will be
       no later call of eval). 

       For now, don't do this if we need the gradient. */

    if ((fast_sub || args != R_NilValue && CAR(args) != R_DotsSymbol)
          && ! (variant & VARIANT_GRADIENT)) {

        SEXP array, ixlist;
        int obj;

        if (fast_sub) {
            ixlist = args;
            array = R_fast_sub_var;
            obj = isObject(array);
        }
        else {
            ixlist = CDR(args);
            array = 
              EVALV_NC (CAR(args), rho, VARIANT_UNCLASS | VARIANT_PENDING_OK);
            obj = isObject(array);
            if (R_variant_result) {
                obj = 0;
                R_variant_result = 0;
            }
        }

        if (obj) {
            args = CONS(array,ixlist);
            argsevald = -1;
            /* go on to general-purpose code below */
        }
        else if (ixlist == R_NilValue || TAG(ixlist) != R_NilValue 
                                      || CAR(ixlist) == R_DotsSymbol) {
            PROTECT(array);
            args = evalListKeepMissing(ixlist,rho);
            UNPROTECT(1);  /* array */
            return do_subset2_dflt_x (call, op, array, R_NoObject, R_NoObject,
                                      R_NilValue, args, rho, variant);
        }
        else {
            SEXP r;
            PROTECT(array);
            BEGIN_PROTECT3 (sb1, sb2, remargs);
            SEXP sv_scalar_stack = R_scalar_stack;
            SEXP ixlist2 = CDR(ixlist);
            sb1 = EVALV (CAR(ixlist), rho, VARIANT_SCALAR_STACK_OK | 
                         VARIANT_MISSING_OK | VARIANT_PENDING_OK);
            remargs = ixlist2;
            sb2 = R_NoObject;
            if (sb1 == R_MissingArg && isSymbol(CAR(ixlist))) {
                remargs = CONS(sb1,remargs);
                SET_MISSING (remargs, R_isMissing(CAR(ixlist),rho));
                sb1 = R_NoObject;
            }
            else if (ixlist2 != R_NilValue && TAG(ixlist2) == R_NilValue 
                                      && CAR(ixlist2) != R_DotsSymbol) {
                INC_NAMEDCNT(sb1);
                sb2 = EVALV (CAR(ixlist2), rho,
                             VARIANT_SCALAR_STACK_OK | VARIANT_MISSING_OK);
                DEC_NAMEDCNT(sb1);
                remargs = CDR(ixlist2);
            }
            if (remargs != R_NilValue) {
                if (sb1 != R_NoObject) INC_NAMEDCNT(sb1);
                if (sb2 != R_NoObject) INC_NAMEDCNT(sb2);
                remargs = evalList_v (remargs, rho, VARIANT_SCALAR_STACK_OK |
                                      VARIANT_PENDING_OK | VARIANT_MISSING_OK);
                if (sb1 != R_NoObject) DEC_NAMEDCNT(sb1);
                if (sb2 != R_NoObject) DEC_NAMEDCNT(sb2);
            }
            if (sb2 == R_MissingArg && isSymbol(CAR(ixlist2))) {
                remargs = CONS(sb2,remargs);
                SET_MISSING (remargs, R_isMissing(CAR(ixlist2),rho));
                sb2 = R_NoObject;
            }
            if (sb1 != R_NoObject)
                WAIT_UNTIL_COMPUTED(sb1);
            wait_until_arguments_computed(remargs);
            r = do_subset2_dflt_x (call, op, array, sb1, sb2,
                                   R_NilValue, remargs, rho, variant);
            R_scalar_stack = sv_scalar_stack;
            END_PROTECT;
            UNPROTECT(1);  /* array */
            R_Visible = TRUE;
            return ON_SCALAR_STACK(r) ? PUSH_SCALAR(r) : r;
        }
    }
    else {
        if (fast_sub) {
            args = CONS (R_fast_sub_var, args);
            argsevald = -1;
        }
    }

    PROTECT(args);

    /* If the first argument is an object and there is an appropriate
       method, we dispatch to that method, otherwise we evaluate the
       arguments and fall through to the generic code below.  Note
       that evaluation retains any missing argument indicators. */

    if (DispatchOrEval (call, op, "[[", args, rho, &ans, 
                        2 /* ask for gradient of 1st arg */,
                        argsevald, variant)) {
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);    /* IS THIS NECESSARY? */
    }
    else {

        /* Method dispatch has failed, we now run the generic internal code. */

        UNPROTECT_PROTECT(ans);

        SEXP x = CAR(ans);

        if (HAS_GRADIENT_IN_CELL(ans))
            PROTECT (array_grad = GRADIENT_IN_CELL(ans));

        args = CDR(ans);

        if (args == R_NilValue || TAG(args) != R_NilValue)
            ans = do_subset2_dflt_x (call, op, x, R_NoObject, R_NoObject, 
                                     array_grad, args, rho, variant);
        else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
            ans = do_subset2_dflt_x (call, op, x, CAR(args), R_NoObject,
                                     array_grad, CDR(args), rho, variant);
        else
            ans = do_subset2_dflt_x (call, op, x, CAR(args), CADR(args),
                                     array_grad, CDDR(args), rho, variant);
    }

    UNPROTECT (1 + (array_grad != R_NilValue));
    R_Visible = TRUE;
    return ans;
}

/* Below is used to implement 'lengths'. */
SEXP attribute_hidden dispatch_subset2(SEXP x, R_xlen_t i, SEXP call, SEXP rho)
{
    static SEXP bracket_op = R_NoObject;
    SEXP args, x_elt;
    if (isObject(x)) {
	if (bracket_op == R_NoObject)
            bracket_op = R_Primitive("[[");
        PROTECT(args = list2(x, ScalarReal(i + 1)));
        x_elt = do_subset2(call, bracket_op, args, rho, 0);
        UNPROTECT(1);
    } else {
      // FIXME: throw error if not a list
	x_elt = VECTOR_ELT(x, i);
    }
    return(x_elt);
}

static SEXP do_subset3(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP from, what, ans, input, ncall;

    SEXP string = R_NilValue;
    SEXP name = R_NilValue;
    SEXP grad = R_NilValue;

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {

        /* Fast interface: object to be subsetted comes already evaluated. */

        what = CAR(args);
        from = R_fast_sub_var;
    }

    else {

        /* regular SPECIAL interface */

        checkArity(op, args);
        what = CADR(args);
        from = EVALV_NC (CAR(args), env, VARIANT_ONE_NAMED | VARIANT_UNCLASS
                                          | (variant & VARIANT_GRADIENT));
        if (R_variant_result & VARIANT_GRADIENT_FLAG)
            grad = R_gradient;
    }

    if (TYPEOF(what) == PROMSXP)
        what = PRCODE(what);
    if (isSymbol(what))
        name = what;
    else if (isString(what) && LENGTH(what) > 0) 
        string = STRING_ELT(what,0);
    else
	errorcall(call, _("invalid subscript type '%s'"), 
                        type2char(TYPEOF(what)));

    if (!isObject(from) || (R_variant_result & VARIANT_UNCLASS_FLAG)) {
        R_variant_result = 0;
        return R_subset3_dflt (from, string, name, call, grad, variant);
    }

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */

    PROTECT(from);

    input = allocVector(STRSXP,1);

    if (name!=R_NilValue)
	SET_STRING_ELT(input, 0, PRINTNAME(name));
    else
	SET_STRING_ELT(input, 0, string);

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when "from" was */
    /* ..., as in PR#8718 */
    PROTECT(args = CONS(from, CONS(input, R_NilValue)));
    if (grad != R_NilValue)
        SET_GRADIENT_IN_CELL (args, grad);

    /* Change call used too, for compatibility with
       R-2.15.0:  It's accessible using "substitute", 
       and was a string in R-2.15.0. */
    PROTECT(ncall = LCONS(CAR(call), CONS(CADR(call), CONS(input,R_NilValue))));

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    R_variant_result = 0;

    if(DispatchOrEval(ncall, op, "$", args, env, &ans, 0, 1, variant)) {
        UNPROTECT(3);
	if (NAMEDCNT_GT_0(ans))         /* IS THIS NECESSARY? */
	    SET_NAMEDCNT_MAX(ans);
        R_Visible = TRUE;
	return ans;
    }

    ans = R_subset3_dflt(CAR(ans), string, name, call, grad, variant);
    UNPROTECT(3);
    return ans;
}

/* -------------------------------------------------------------------------- */
/*                         SUBASSIGN OPERATORS.                               */

static SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP sv_scalar_stack = R_scalar_stack;

    SEXP ans, r, x, sb1, sb2, subs, y;
    int argsevald = 0;
    int64_t seq = 0;

    /* See if we are using the fast interface. */

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {

        /* Fast interface: object assigned into (x) comes already
           evaluated.  Evaluate indexes using VARIANT_SCALAR_STACK_OK,
           and evaluate a single index with VARIANT_SEQ so it may come
           as a range rather than a vector.  */

        y = R_fast_sub_replacement;  /* may be on scalar stack */
        x = R_fast_sub_var;
        sb1 = CAR(args);
        subs = CDR(args);

        if (subs == R_NilValue) {
            sb1 = EVALV (sb1, rho, VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                                                 VARIANT_MISSING_OK);
            if (R_variant_result) {
                seq = R_variant_seq_spec;
                R_variant_result = 0;
            }

            else {

                /* Do the very simplest cases here. */

                if (isVectorAtomic(x) && TYPEOF(x) == TYPE_ETC(y)) {
                    R_len_t len = LENGTH(x);
                    R_len_t ix = 0;
                    if (TYPE_ETC(sb1) == INTSXP && *INTEGER(sb1) >= 1
                                                && *INTEGER(sb1) <= len)
                        ix = *INTEGER(sb1);
                    else if (TYPE_ETC(sb1) == REALSXP && *REAL(sb1) >= 1 
                                                      && *REAL(sb1) <= len)
                        ix = (int) *REAL(sb1);
                    if (ix != 0) {
                        ix -= 1;
                        WAIT_UNTIL_COMPUTED(x);
                        switch (TYPEOF(x)) {
                            case LGLSXP:
                            case INTSXP:
                                INTEGER(x)[ix] = *INTEGER(y);
                                break;
                            case REALSXP:
                                REAL(x)[ix] = *REAL(y);
                                break;
                            case CPLXSXP:
                                COMPLEX(x)[ix] = *COMPLEX(y);
                                break;
                            case STRSXP:
                                SET_STRING_ELT (x, ix, STRING_ELT(y,0));
                                break;
                            case RAWSXP:
                                RAW(x)[ix] = *RAW(y);
                                break;
                        }
                        SET_NAMEDCNT_0(x);
                        r = x;
                        goto ret;
                    }
                }
            }
            sb2 = R_NoObject;
        }
        else {
            sb1 = EVALV (sb1, rho, VARIANT_SCALAR_STACK_OK |
                                   VARIANT_MISSING_OK);
            PROTECT(sb1);
            if (TAG(subs) != R_NilValue || CAR(subs) == R_DotsSymbol)
                sb2 = R_NoObject;
            else {
                INC_NAMEDCNT(sb1);
                sb2 = EVALV (CAR(subs), rho, VARIANT_SCALAR_STACK_OK |
                                             VARIANT_MISSING_OK);
                DEC_NAMEDCNT(sb1);
                subs = CDR(subs);
            }
            if (subs != R_NilValue) {
                INC_NAMEDCNT(sb1);
                if (sb2 != R_NoObject) {
                    PROTECT(sb2);
                    INC_NAMEDCNT(sb2);
                }
                subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                              VARIANT_MISSING_OK);
                DEC_NAMEDCNT(sb1);
                if (sb2 != R_NoObject) {
                    UNPROTECT(1);
                    DEC_NAMEDCNT(sb2);
                }
            }
            UNPROTECT(1); /* sb1 */
        }

        goto dflt_seq;
    }

    y = R_NoObject;  /* found later after other arguments */
    x = CAR(args);   /* args are (x, indexes..., y) */
    sb1 = R_NoObject;
    sb2 = R_NoObject;
    subs = CDR(args);

    if (x != R_DotsSymbol && !(variant & VARIANT_GRADIENT)) {

        /* Mostly called from do_set, with first arg an evaluated promise. */

        PROTECT (x = TYPEOF(x) == PROMSXP 
                      && PRVALUE_PENDING_OK(x) != R_UnboundValue
                        ? PRVALUE(x) : eval(x,rho));
        if (isObject(x)) {
            args = CONS(x,subs);
            UNPROTECT(1);
            argsevald = -1;
        }
        else if (TYPEOF(CAR(subs)) != LANGSXP || CDR(subs) != R_NilValue) {
            /* in particular, CAR(subs) might be missing or ... */
            subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                          VARIANT_MISSING_OK);
            UNPROTECT(1);
            goto dflt_seq;
        }
        else {
            PROTECT(sb1 = EVALV (CAR(subs), rho, VARIANT_SEQ |
                            VARIANT_SCALAR_STACK_OK | VARIANT_MISSING_OK));
            if (R_variant_result) {
                seq = R_variant_seq_spec;
                R_variant_result = 0;
            }
            subs = R_NilValue;
            UNPROTECT(2);
            goto dflt_seq;
        }
    }

    if (DispatchOrEval (call, op, "[<-", args, rho, &ans, 
          !(variant & VARIANT_GRADIENT) ? 0 : 3 /* eval 1st and last arg */, 
          argsevald, variant)) {
        R_Visible = TRUE;
        return ans;
    }

    return do_subassign_dflt_seq
       (call, CAR(ans), 
        HAS_GRADIENT_IN_CELL(ans) ? GRADIENT_IN_CELL(ans) : R_NilValue, 
        R_NoObject, R_NoObject, CDR(ans), rho, R_NoObject, 0);

    /* ... path that bypasses DispatchOrEval ... */

  dflt_seq:
    r = do_subassign_dflt_seq(call, x, R_NilValue, sb1, sb2, subs, rho, y, seq);

  ret:
    R_scalar_stack = sv_scalar_stack;
    return r;
}

static SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {

        SEXP scalar_stack_sv = R_scalar_stack;
        SEXP y = R_fast_sub_replacement; /* may be on the scalar stack */
        SEXP x = R_fast_sub_var;
        SEXP sb1, sb2, subs;

        sb1 = EVALV (CAR(args), rho, VARIANT_SCALAR_STACK_OK | 
                                     VARIANT_MISSING_OK);
        subs = CDR(args);

        PROTECT(sb1);
        if (subs == R_NilValue || TAG(subs) != R_NilValue 
                               || CAR(subs) == R_DotsSymbol)
            sb2 = R_NoObject;
        else {
            INC_NAMEDCNT(sb1);
            sb2 = EVALV (CAR(subs), rho, VARIANT_SCALAR_STACK_OK |
                                         VARIANT_MISSING_OK);
            DEC_NAMEDCNT(sb1);
            subs = CDR(subs);
        }
        if (subs != R_NilValue) {
            INC_NAMEDCNT(sb1);
            if (sb2 != R_NoObject) {
                PROTECT(sb2);
                INC_NAMEDCNT(sb2);
            }
            subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                          VARIANT_MISSING_OK);
            DEC_NAMEDCNT(sb1);
            if (sb2 != R_NoObject) {
                UNPROTECT(1);
                DEC_NAMEDCNT(sb2);
            }
        }
        UNPROTECT(1); /* sb1 */

        SEXP r = do_subassign2_dflt_int
                  (call, x, sb1, sb2, subs, rho, y, R_NilValue);
        R_scalar_stack = scalar_stack_sv;
        return r;
    }

    SEXP ans;

    if (DispatchOrEval (call, op, "[[<-", args, rho, &ans, 
         ! (variant & VARIANT_GRADIENT) ? 0 : 3 /* grad for 1st and last arg */,
         0, variant)) {
        R_Visible = TRUE;
        return ans;
    }

    return do_subassign2_dflt_int 
            (call, CAR(ans), R_NoObject, R_NoObject, CDR(ans), rho, R_NoObject,
             HAS_GRADIENT_IN_CELL(ans) ? GRADIENT_IN_CELL(ans) : R_NilValue);
}

static SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP into, what, value, ans, string, ncall;

    SEXP value_grad = R_NilValue;
    SEXP into_grad = R_NilValue;
    SEXP schar = R_NilValue;
    SEXP name = R_NilValue;

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {
        value = R_fast_sub_replacement; /* may be on scalar stack */
        into = R_fast_sub_var;
        what = CAR(args);
        if (args == R_NilValue || CDR(args) != R_NilValue)
            errorcall (call, _("%d arguments passed to '%s' which requires %d"),
                             length(args)+2, PRIMNAME(op), 3);
    }
    else {
        into = CAR(args);
        what = CADR(args);
        value = CADDR(args);
        if (CDDR(args) == R_NilValue || CDR(CDDR(args)) != R_NilValue)
            errorcall (call, _("%d arguments passed to '%s' which requires %d"),
                             length(args), PRIMNAME(op), 3);
    }

    if (TYPEOF(what) == PROMSXP)
        what = PRCODE(what);

    if (isSymbol(what)) {
        name = what;
        schar = PRINTNAME(name);
    }
    else if (isString(what) && LENGTH(what) > 0)
        schar = STRING_ELT(what,0);
    else
        errorcall(call, _("invalid subscript type '%s'"), 
                        type2char(TYPEOF(what)));

    /* Handle the fast case, for which 'into' and 'value' have been evaluated,
       and 'into' is not an object. */

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {
        if (name == R_NilValue) name = install_translated(schar);
        return R_subassign3_dflt (call, into, name, value, 
                                  into_grad, value_grad);
    }

    /* Handle usual case not into an object quickly, without overhead
       of allocation and calling of DispatchOrEval. */

    /* Note: mostly called from do_set, with first arg an evaluated promise */

    if (TYPEOF(into) == PROMSXP && PRVALUE_PENDING_OK(into) != R_UnboundValue) {
        if (HAS_GRADIENT_IN_CELL(into))
            into_grad = GRADIENT_IN_CELL(into);
        into = PRVALUE(into);
    }
    else {
        into = evalv (into, env, variant & VARIANT_GRADIENT);
        if (R_variant_result & VARIANT_GRADIENT_FLAG) {
            into_grad = R_gradient;
            R_variant_result = 0;
        }
    }

    PROTECT2(into,into_grad);

    if (!isObject(into)) {
        if (name == R_NilValue) name = install_translated(schar);
        value = EVALV (value, env, variant & VARIANT_GRADIENT);
        if (R_variant_result & VARIANT_GRADIENT_FLAG) {
            value_grad = R_gradient;
            R_variant_result = 0;
        }
        UNPROTECT(2);
        return R_subassign3_dflt (call, into, name, value, 
                                  into_grad, value_grad);
    }

    /* First translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly.
       We also change the call used, as in do_subset3, since the
       destructive change in R-2.15.0 has this side effect. */

    string = allocVector(STRSXP,1);
    SET_STRING_ELT (string, 0, schar);
    PROTECT(args = CONS(into, CONS(string, CDDR(args))));
    PROTECT(ncall = 
      LCONS(CAR(call),CONS(CADR(call),CONS(string,CDR(CDDR(call))))));

    if (DispatchOrEval (ncall, op, "$<-", args, env, &ans, 
         ! (variant & VARIANT_GRADIENT) ? 0 : 1 /* grad for last arg */,
         -1, variant)) {
        UNPROTECT(4);
        R_Visible = TRUE;
        return ans;
    }

    PROTECT(ans);
    if (name == R_NilValue) name = install_translated(schar);
    UNPROTECT(5);

    if ((variant & VARIANT_GRADIENT) && HAS_GRADIENT_IN_CELL(CDDR(ans)))
        value_grad = GRADIENT_IN_CELL(CDDR(ans));

    return R_subassign3_dflt (call, CAR(ans), name, CADDR(ans),
                              into_grad, value_grad);
}


/* -------------------------------------------------------------------------- */
/*                      FUNCTION APPLY PROCEDURES                             */

/* Apply a "closure" to an argument list.  Keep in sync with execMethod, etc.
   below.

   'supplied' is an array of SEXP values, first a set of pairs of tag and
   value, then a pairlist of tagged values (or R_NilValue).  If NULL, no
   extras supplied. */

SEXP attribute_hidden applyClosure_v(SEXP call, SEXP op, SEXP arglist, SEXP rho,
                                     SEXP *supplied, int variant)
{
    int vrnt = VARIANT_PENDING_OK | VARIANT_DIRECT_RETURN | VARIANT_WHOLE_BODY
                 | VARIANT_PASS_ON(variant);

    if (variant & VARIANT_NOT_WHOLE_BODY)
        vrnt &= ~VARIANT_WHOLE_BODY;

    SEXP actuals, savedsrcref, newrho;
    RCNTXT cntxt;

    /*  Set up a context with the call in it for use if an error occurs below
        in matchArgs or from running out of memory (eg, in NewEnvironment). 
        Note that this also protects call, rho, arglist, and op. */

    begincontext(&cntxt, CTXT_RETURN, call, CLOENV(op), rho, arglist, op);

    savedsrcref = R_Srcref;  /* saved in context for longjmp, and protection */

    /*  Build a list which matches the actual (unevaluated) arguments
        to the formal paramters.  Build a new environment which
        contains the matched pairs.  Note that actuals is protected via
        newrho. */

    actuals = matchArgs_pairlist (FORMALS(op), arglist, call);
    PROTECT(newrho = NewEnvironment(R_NilValue, actuals, CLOENV(op)));
        /* no longer passes formals, since matchArg now puts tags in actuals */

    if (variant & VARIANT_GRADIENT)
        SET_STORE_GRAD (newrho, STORE_GRAD(rho));

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    R_symbits_t bits = 0;

    SEXP f = FORMALS(op);
    SEXP a = actuals;
    while (a != R_NilValue) {
        SEXP t = TAG(a);
        bits |= SYMBITS(t);
        if (MISSING(a)) {
            if (CAR(f) != R_MissingArg) {
                SETCAR(a, mkPROMISE (CAR(f), newrho));
                if (variant & VARIANT_GRADIENT) SET_STORE_GRAD(CAR(a),1);
                SET_MISSING(a,2);
            }
        }
        else { 
            /* optimize assuming non-missing arguments are usually referenced */
            LASTSYMENV(t) = SEXP32_FROM_SEXP(newrho);
            LASTSYMBINDING(t) = a;
        }
        a = CDR(a);
        f = CDR(f);
    }

    SET_ENVSYMBITS (newrho, bits);

    /* set_symbits_in_env (newrho); */  /* now done in loop above */

    /*  Fix up any extras that were supplied by usemethod. */

    if (supplied != NULL) {
        while (TYPEOF(*supplied) == SYMSXP) {
            set_var_in_frame (*supplied, *(supplied+1), newrho, TRUE, 3);
            supplied += 2;
        }
        for (SEXP t = *supplied; t != R_NilValue; t = CDR(t)) {
            for (a = actuals; a != R_NilValue; a = CDR(a))
                if (TAG(a) == TAG(t))
                    break;
            if (a == R_NilValue)
                set_var_in_frame (TAG(t), CAR(t), newrho, TRUE, 3);
        }
    }

    UNPROTECT(1); /* newrho, which will be protected below via revised context*/

    /*  Change the previously-set-up context to have the correct environment.

        If we have a generic function we need to use the sysparent of
        the generic as the sysparent of the method because the method
        is a straight substitution of the generic. */

    if (R_GlobalContext->nextcontext->callflag == CTXT_GENERIC)
        revisecontext (newrho, R_GlobalContext->nextcontext->sysparent);
    else
        revisecontext (newrho, rho);

    /* Get the srcref record from the closure object */
    
    R_Srcref = getAttrib00(op, R_SrcrefSymbol);

    SEXP body = BODY(op);

    /* Debugging */

    if (RDEBUG(op) | RSTEP(op))
        body = Rf_apply_debug_setup (call, op, rho, body, newrho);

    /* Set a longjmp target which will catch any explicit returns from the
       function body that are not instead handled by VARIANT_DIRECT_RETURN.  */

    SEXP res;

    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue != R_RestartToken) {
            res = R_ReturnedValue;
            goto evald;
        }
        cntxt.callflag = CTXT_RETURN;  /* turn restart off */
        R_ReturnedValue = R_NilValue;  /* remove restart token */
    }

    res = evalv (body, newrho, vrnt);

  evald: 
    PROTECT(res);

    R_variant_result &= ~VARIANT_RTN_FLAG;

    R_Srcref = savedsrcref;
    endcontext(&cntxt);

    if ( ! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_COMPUTED(res);

    if (RDEBUG(op))
        Rf_apply_debug_finish (call, rho);

    UNPROTECT(1); /* res */
    return res;
}

SEXP applyClosure (SEXP call, SEXP op, SEXP arglist, SEXP rho, 
                   SEXP *supplied)
{
    if (supplied != NULL) error("Last argument to applyClosure must be NULL");
    return applyClosure_v (call, op, arglist, rho, NULL, 0);
}

/* Create a promise to evaluate each argument.	If the argument is itself
   a promise, it is used unchanged, except that it has its NAMEDCNT
   incremented, and the NAMEDCNT of its value (if not unbound) incremented
   unless it is zero, and it has STORE_GRAD set if 'variant' has the 
   VARIANT_GRADIENT bit set ('variant' is otherwise unused).  

   See inside for handling of ... */

#define MAKE_PROMISE(a,rho,variant) do { \
    if (a != R_MissingArg && a != R_MissingUnder) { \
        if (TYPEOF(a) == PROMSXP) { \
            INC_NAMEDCNT(a); \
            SEXP p = PRVALUE_PENDING_OK(a); \
            if (p != R_UnboundValue && NAMEDCNT_GT_0(p)) \
                INC_NAMEDCNT(p); \
        } \
        else \
            a = mkPROMISE (a, rho); \
        if (variant) SET_STORE_GRAD (a, 1); \
    } \
} while (0)

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho, int variant)
{
    variant &= VARIANT_GRADIENT;

    /* Handle 0, 1, or 2 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) {  /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue)
            return el;
        SEXP a = CAR(el);
        if (a != R_DotsSymbol) {
            MAKE_PROMISE(a,rho,variant);
            return cons_with_tag (a, R_NilValue, TAG(el));
        }
    }
    else if (CDDR(el) == R_NilValue) {
        SEXP a1 = CAR(el);
        SEXP a2 = CADR(el);
        if (a1 != R_DotsSymbol && a2 != R_DotsSymbol) {
            SEXP r;
            MAKE_PROMISE(a2,rho,variant);
            PROTECT (r = cons_with_tag (a2, R_NilValue, TAG(CDR(el))));
            MAKE_PROMISE(a1,rho,variant);
            r = cons_with_tag (a1, r, TAG(el));
            UNPROTECT(1);
            return r;
        }
    }

    /* Handle the general case (except for el being R_NilValue, done above). */

    BEGIN_PROTECT4 (head, tail, ev, h);

    head = R_NilValue;

    do {  /* el == R_NilValue is handled above, so always loop at least once */

        SEXP a = CAR(el);

	/* If we have a ... symbol, we look to see what it is bound to.
	   If its binding is R_NilValue we just ignore it.  If it is bound
           to a list, promises in the list (typical case) are re-used with
           NAMEDCNT incremented, and non-promises have promises created for
           them; the promise is then spliced into the list that is returned.
           Anything else bound to a ... symbol is an error. */

	if (a == R_DotsSymbol) {
	    h = findVar(a, rho);
            if (h == R_NilValue) {
                /* nothing */
            }
	    else if (TYPEOF(h) == DOTSXP) {
		while (h != R_NilValue) {
                    a = CAR(h);
                    MAKE_PROMISE(a,rho,variant);
                    ev = cons_with_tag (a, R_NilValue, TAG(h));
                    if (head==R_NilValue)
                        head = ev;
                    else
                        SETCDR(tail,ev);
                    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		dotdotdot_error();
	}
        else {
            MAKE_PROMISE(a,rho,variant);
            ev = cons_with_tag (a, R_NilValue, TAG(el));
            if (head == R_NilValue)
                head = ev;
            else
                SETCDR_MACRO(tail, ev);
            tail = ev;
        }
	el = CDR(el);

    } while (el != R_NilValue);

    RETURN_SEXP_INSIDE_PROTECT (head);
    END_PROTECT;
}
 
/* Create promises for arguments, with values for promises filled in.  
   Values for arguments that don't become promises are silently ignored.  
   This is used in method dispatch, hence the text of the error message 
   (which should never occur).  Copies gradient info from 'values' to
   promises in result. */
 
SEXP attribute_hidden promiseArgsWithValues (SEXP el, SEXP rho, SEXP values,
                                             int variant)
{
    SEXP s, a, b;

    PROTECT (s = promiseArgs (el, rho, 0));  /* variant arg 0 since gradients
                                                are handled here below */
    for (a = values, b = s; 
         a != R_NilValue && b != R_NilValue;
         a = CDR(a), b = CDR(b)) {
        if (TYPEOF (CAR(b)) == PROMSXP) {
            SET_PRVALUE (CAR(b), CAR(a));
            if (HAS_GRADIENT_IN_CELL(a)) {
                SET_STORE_GRAD (CAR(b), 1);
                SET_GRADIENT_IN_CELL (CAR(b), GRADIENT_IN_CELL(a));
            }
            INC_NAMEDCNT (CAR(a));
        }
    }

    if (a == R_NilValue && b == R_NilValue) {
        UNPROTECT(1);
        return s;
    }

    error(_("dispatch error"));
}

/* Like promiseArgsWithValues except it sets only the first value.  So it
   needs the variant for creating promises for the other arguments (which
   might require gradient). */

SEXP attribute_hidden promiseArgsWith1Value (SEXP el, SEXP rho, SEXP value,
                                             SEXP value_grad, int variant)
{
    SEXP s, p;
    PROTECT (s = promiseArgs (el, rho, variant));
    if (s == R_NilValue) error(_("dispatch error"));
    p = CAR(s);
    if (TYPEOF(p) == PROMSXP) {
        SET_PRVALUE (p, value);
        if (value_grad != R_NilValue) {
            SET_STORE_GRAD (p, 1);
            SET_GRADIENT_IN_CELL (p, value_grad);
        }
        INC_NAMEDCNT (value);
    }
    UNPROTECT(1);
    return s;
}

/* Declared with a variable number of args in names.c */
static SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP rval, srcref;

    /* The following is as in 2.15.0, but it's not clear how it can happen. */
    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMEDCNT_MAX(op);
    }

    CheckFormals(CAR(args));
    rval = mkCLOSXP(CAR(args), CADR(args), rho);
    srcref = CADDR(args);
    if (srcref != R_NilValue) 
        setAttrib(rval, R_SrcrefSymbol, srcref);

    R_Visible = TRUE;
    return rval;
}

/* Check that each formal is a symbol.  Also used in coerce.c */

void attribute_hidden CheckFormals(SEXP ls)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		goto err;
	return;
    }
 err:
    error(_("invalid formal argument list for \"function\""));
}


/* -------------------------------------------------------------------------- */
/*                         METHOD DISPATCH FOR PRIMITIVES                     */


/* DispatchOrEval is used in internal functions which dispatch to
   object methods (e.g. "[" or "[[").  The code either builds promises
   and dispatches to the appropriate method, or it evaluates the
   arguments it comes in with (if argsevald is 0) and returns them so that
   the generic built-in C code can continue.  Note that CDR(call) is
   used to obtain the unevaluated arguments when creating promises, even
   when argsevald is 1 (so args is the evaluated arguments).  If argsevald 
   is -1, only the first argument will have been evaluated.

   If first_last_arg_grad & 2 is non-zero, the gradient will be
   requested when evaluating the first argument.  If first_last_arg_grad & 1 
   is non-zero, the gradient will be requested when evaluating the
   last argument, if there is more than one argument; first_last_arg_grad
   is ignored if argsevald is 1.  If argsevald is non-zero, any gradient
   for the first (evaluated) argument will be preserved for S3 dispatching,
   and in the returned 'ans' if dispatch did not occur.

   The 'variant' argument is passed on to an S3 method, and is also used
   (when argsevald is not 1) to determine whether promises passed to the
   S3 method have STORE_GRAD set.
  
   The arg list is protected by this function, and needn't be by the caller.
 */
attribute_hidden
int DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, SEXP *ans, int first_last_arg_grad, int argsevald,
                   int variant)
{
  /* DispatchOrEval is called very frequently, most often in cases where
     no dispatching is needed and the isObject or the string-based
     pre-test fail.  To avoid degrading performance it is therefore
     necessary to avoid creating promises in these cases.  The pre-test
     does require that we look at the first argument, so that needs to
     be evaluated.  The complicating factor is that the first argument
     might come in with a "..." and that there might be other arguments
     in the "..." as well.  LT */

    BEGIN_PROTECT2 (x, x_grad);
    ALSO_PROTECT1 (args);

    int dots = FALSE;

    if (argsevald != 0) {
        x = CAR(args);
        if (HAS_GRADIENT_IN_CELL(args)) 
            x_grad = GRADIENT_IN_CELL(args);
    }
    else {
        /* Find the object to dispatch on, dropping any leading
           ... arguments with missing or empty values.  If there are no
           arguments, R_NilValue is used. */
        int vr = first_last_arg_grad & 2 ? VARIANT_GRADIENT : 0;
        x = R_NilValue;
        for (; args != R_NilValue; args = CDR(args)) {
            if (CAR(args) == R_DotsSymbol) {
                SEXP h = findVar(R_DotsSymbol, rho);
                if (TYPEOF(h) == DOTSXP) {
                    dots = TRUE;
                    x = evalv (CAR(h), rho, vr);
                    if (R_variant_result) x_grad = R_gradient;
                    break;
                }
                else if (h != R_NilValue && h != R_MissingArg)
                    dotdotdot_error();
            }
            else {
                x = evalv (CAR(args), rho, vr);
                if (R_variant_result) x_grad = R_gradient;
                break;
            }
        }
    }

    first_last_arg_grad &= ~2;

    if (isObject(x)) { /* try to dispatch on the object */

        /* Try for formal method. */

        if (IS_S4_OBJECT(x) && R_has_methods(op)) {

            /* Create promises to pass down to applyClosure  */

            if (argsevald < 0)
                args = promiseArgsWith1Value (CDR(call), rho, x, 
                                              x_grad, variant);
            else if (argsevald == 0)
                args = promiseArgsWith1Value (args, rho, x, 
                                              x_grad, variant);

            if (first_last_arg_grad && CDR(args) != R_NilValue) {
                SEXP p = CDR(args);
                while (CDR(p) != R_NilValue) p = CDR(p);
                SET_STORE_GRAD(p,1);
            }

            /* This means S4 dispatch */
            SEXP value = R_possible_dispatch(call, op, args, rho, argsevald<=0);
            if (value != R_NoObject) {
                *ans = value;
                RETURN_OUTSIDE_PROTECT (1);
            }

            if (argsevald <= 0) {

                /* re-evaluates first argument, but this should be OK since
                   it's in a forced promise, so not really re-evaluated. */

                args = first_last_arg_grad 
                       ? evalList_gradient (args, rho, VARIANT_MISSING_OK, 0, 0)
                       : evalList_v (args, rho, VARIANT_MISSING_OK);

                argsevald = 1;
            }
        }

        char *pt;
        if (TYPEOF(CAR(call)) == SYMSXP)
            pt = Rf_strrchr(CHAR(PRINTNAME(CAR(call))), '.');
        else
            pt = NULL;

        if (pt == NULL || strcmp(pt,".default") != 0) {

            SEXP pargs, rho1;
            RCNTXT cntxt;

            if (argsevald > 0)  /* handle as in R_possible_dispatch */
                pargs = promiseArgsWithValues (CDR(call), rho, args, variant);
            else {
                pargs = promiseArgsWith1Value (args, rho, x, x_grad, variant); 
                if (first_last_arg_grad && CDR(pargs) != R_NilValue) {
                    SEXP p = CDR(pargs);
                    while (CDR(p) != R_NilValue) p = CDR(p);
                    SET_STORE_GRAD(p,1);
                }
            }

            /* The context set up here is needed because of the way
               usemethod() is written.  DispatchGroup() repeats some
               internal usemethod() code and avoids the need for a
               context; perhaps the usemethod() code should be
               refactored so the contexts around the usemethod() calls
               in this file can be removed.

               Using rho for current and calling environment can be
               confusing for things like sys.parent() calls captured
               in promises (Gabor G had an example of this).  Also,
               since the context is established without a SETJMP using
               an R-accessible environment allows a segfault to be
               triggered (by something very obscure, but still).
               Hence here and in the other usemethod() uses below a
               new environment rho1 is created and used.  LT */

            PROTECT(pargs);
            rho1 = NewEnvironment(R_NilValue, R_NilValue, rho);
            UNPROTECT(1);

            begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);

            SET_STORE_GRAD (rho1, STORE_GRAD(rho));
            if (usemethod (generic, x, call, pargs, rho1, rho, 
                           R_BaseEnv, variant, ans)) {
                endcontext(&cntxt);
                RETURN_OUTSIDE_PROTECT (1);
            }

            endcontext(&cntxt);
        }
    }

    if (argsevald <= 0) {

        if (dots) {

            /* The first call argument was ... and may contain more than the
               object, so it needs to be evaluated here.  The object should be
               in a promise, or be self-evaluating, so evaluating it again 
               should be no problem. */

            args = first_last_arg_grad 
                    ? evalList_gradient (args, rho, VARIANT_MISSING_OK, 0, 0)
                    : evalList_v (args, rho, VARIANT_MISSING_OK);
        }
        else {
            args = cons_with_tag (x, 
              first_last_arg_grad 
                ? evalList_gradient (CDR(args), rho, VARIANT_MISSING_OK, 0, 0)
                : evalList_v (CDR(args), rho, VARIANT_MISSING_OK),
              TAG(args));
        }

        if (x_grad != R_NilValue)
            SET_GRADIENT_IN_CELL (args, x_grad);
    }

    *ans = args;
    END_PROTECT;
    return 0;
}


/* gr needs to be protected on return from this function. */
static void findmethod(SEXP Class, const char *group, const char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       SEXP rho)
{
    int len, whichclass;
    char buf[512];

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	if (!copy_3_strings (buf, sizeof buf, generic, ".", ss))
	    error(_("class name too long in '%s'"), generic);
	*meth = install(buf);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = R_BlankScalarString;
	    break;
	}
        if (!copy_3_strings (buf, sizeof buf, group, ".", ss))
	    error(_("class name too long in '%s'"), group);
	*meth = install(buf);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString(group);
	    break;
	}
    }
    *which = whichclass;
}

attribute_hidden
int DispatchGroup(const char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		  SEXP *ans, int variant)
{
    int nargs, lwhich, rwhich, set;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr;
    SEXP rclass, rmeth, rgr, rsxp, value;
    char *generic;
    Rboolean useS4 = TRUE, isOps = FALSE;

    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (args != R_NilValue && ! isObject(CAR(args)) &&
	(CDR(args) == R_NilValue || ! isObject(CADR(args))))
	return 0;

    isOps = strcmp(group, "Ops") == 0;

    /* try for formal method */
    if(length(args) == 1 && !IS_S4_OBJECT(CAR(args))) useS4 = FALSE;
    if(length(args) == 2 &&
       !IS_S4_OBJECT(CAR(args)) && !IS_S4_OBJECT(CADR(args))) useS4 = FALSE;
    if(useS4) {
	/* Remove argument names to ensure positional matching */
	if(isOps)
	    for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG_NIL(s);
	if(R_has_methods(op)) {
	    value = R_possible_dispatch(call, op, args, rho, FALSE);
            if (value != R_NoObject) {
	        *ans = value;
	        return 1;
            }
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if ( isSymbol(CAR(call)) ) {
        const char *pt;
        pt = CHAR(PRINTNAME(CAR(call)));
        while (*pt == '.') pt += 1;   /* duplicate previous behaviour exactly */
        while (*pt != 0 && *pt != '.') pt += 1;
        if (*pt != 0) {
            while (*pt == '.') pt += 1;
            if (strcmp(pt,"default") == 0)
                return 0;
        }
    }

    if(isOps)
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    if(!isObject(CAR(args)) && !isObject(CADR(args)))
	return 0;

    generic = PRIMNAME(op);

    lclass = IS_S4_OBJECT(CAR(args)) ? R_data_class2(CAR(args))
              : getClassAttrib(CAR(args));
    PROTECT(lclass);

    if( nargs == 2 )
	rclass = IS_S4_OBJECT(CADR(args)) ? R_data_class2(CADR(args))
                  : getClassAttrib(CADR(args));
    else
	rclass = R_NilValue;
    PROTECT(rclass);

    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;

    findmethod(lclass, group, generic, &lsxp, &lgr, &lmeth, &lwhich, rho);
    PROTECT(lgr);
    if(isFunction(lsxp) && IS_S4_OBJECT(CAR(args)) && lwhich > 0
       && isBasicClass(translateChar(STRING_ELT(lclass, lwhich)))) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
        value = CAR(args);
	if (NAMEDCNT_GT_0(value)) SET_NAMEDCNT_MAX(value);
	value = R_getS4DataSlot(value, S4SXP); /* the .S3Class obj. or NULL*/
	if(value != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(args, value);
    }

    if( nargs == 2 )
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth, &rwhich, rho);
    else
	rwhich = 0;

    if(isFunction(rsxp) && IS_S4_OBJECT(CADR(args)) && rwhich > 0
       && isBasicClass(translateChar(STRING_ELT(rclass, rwhich)))) {
        value = CADR(args);
	if (NAMEDCNT_GT_0(value)) SET_NAMEDCNT_MAX(value);
	value = R_getS4DataSlot(value, S4SXP);
	if(value != R_NilValue) SETCADR(args, value);
    }

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(4);
	return 0; /* no generic or group method so use default*/
    }

    if( lsxp != rsxp ) {
	if ( isFunction(lsxp) && isFunction(rsxp) ) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if( streql(rname, "Ops.difftime") && 
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")) )
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") && 
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;
	    else {
		warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			lname, rname, generic);
		UNPROTECT(4);
		return 0;
	    }
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
	}
    }

    /* we either have a group method or a class method */

    int i, j;

    PROTECT(m = allocVector(STRSXP,nargs));
    s = args;
    for (i = 0 ; i < nargs ; i++) {
	t = IS_S4_OBJECT(CAR(s)) ? R_data_class2(CAR(s))
	  : getClassAttrib(CAR(s));
	set = 0;
	if (isString(t)) {
	    for (j = 0 ; j < LENGTH(t) ; j++) {
		if (!strcmp(translateChar(STRING_ELT(t, j)),
			    translateChar(STRING_ELT(lclass, lwhich)))) {
		    SET_STRING_ELT(m, i, PRINTNAME(lmeth));
		    set = 1;
		    break;
		}
	    }
	}
	if( !set )
	    SET_STRING_ELT_BLANK(m, i);
	s = CDR(s);
    }

    SEXP genstr = PROTECT(mkString(generic));

    set = length(lclass) - lwhich;
    PROTECT(t = allocVector(STRSXP, set));
    copy_string_elements (t, 0, lclass, lwhich, set);

    SEXP supplied[13];
    supplied[0] = R_NilValue;

    i = 0;

    supplied[i++] = R_dot_Class;          supplied[i++] = t;
    supplied[i++] = R_dot_Generic;        supplied[i++] = genstr;
    supplied[i++] = R_dot_Method;         supplied[i++] = m;
    supplied[i++] = R_dot_GenericCallEnv; supplied[i++] = rho;
    supplied[i++] = R_dot_GenericDefEnv;  supplied[i++] = R_BaseEnv;
    supplied[i++] = R_dot_Group;          supplied[i++] = lgr;


    supplied[i] = R_NilValue;

    PROTECT(t = LCONS(lmeth, CDR(call)));

    /* The arguments have been evaluated; since we are passing them
       out to a closure we need to wrap them in promises so that they
       get duplicated and things like missing/substitute work.  */

    PROTECT(s = promiseArgsWithValues(CDR(call), rho, args, variant));
    if (isOps) {
        /* ensure positional matching for operators */
        for (m = s; m != R_NilValue; m = CDR(m))
            SET_TAG_NIL(m);
    }

    *ans = applyClosure_v (t, lsxp, s, rho, supplied, variant);

    UNPROTECT(9);
    return 1;
}


/* -------------------------------------------------------------------------- */
/*                     VERSIONS OF "APPLY" FOR METHODS                        */

/* **** FIXME: This code is factored out of applyClosure.  If we keep
   **** it we should change applyClosure to run through this routine
   **** to avoid code drift. */
static SEXP R_execClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			  SEXP newrho)
{
    volatile SEXP body;
    SEXP savedsrcref;
    RCNTXT cntxt;

    body = BODY(op);

    begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);
    savedsrcref = R_Srcref;  /* saved in context for longjmp, and protection */

    /* Get the srcref record from the closure object.  Disable for now
       at least, since it's not clear that it's needed. */
    
    R_Srcref = R_NilValue;  /* was: getAttrib(op, R_SrcrefSymbol); */

    /* Debugging */

    if (RDEBUG(op) | RSTEP(op))
        body =  Rf_apply_debug_setup (call, op, rho, body, newrho);

    /*  Set a longjmp target which will catch any explicit returns
        from the function body.  */

    SEXP res;

    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue != R_RestartToken) {
            res = R_ReturnedValue;
            goto evald;
        }
        cntxt.callflag = CTXT_RETURN;  /* turn restart off */
        R_ReturnedValue = R_NilValue;  /* remove restart token */
    }

    res = evalv(body, newrho, 0);

  evald:
    PROTECT(res);

    R_Srcref = savedsrcref;
    endcontext(&cntxt);

    if (RDEBUG(op))
        Rf_apply_debug_finish (call, rho);

    UNPROTECT(1);  /* res */
    return res;
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    SEXP call, arglist, callerenv, newrho, next, val;
    RCNTXT *cptr;

    /* create a new environment frame enclosed by the lexical
       environment of the method */
    PROTECT(newrho = Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op)));

    /* copy the bindings for the formal environment from the top frame
       of the internal environment of the generic call to the new
       frame.  need to make sure missingness information is preserved
       and the environments for any default expression promises are
       set to the new environment.  should move this to envir.c where
       it can be done more efficiently. */
    for (next = FORMALS(op); next != R_NilValue; next = CDR(next)) {
        SEXP symbol =  TAG(next);
	R_varloc_t loc;
	int missing;
	loc = R_findVarLocInFrame(rho,symbol);
	if (loc == R_NoObject)
	    error(_("could not find symbol \"%s\" in environment of the generic function"),
		  CHAR(PRINTNAME(symbol)));
	missing = R_GetVarLocMISSING(loc);
	val = R_GetVarLocValue(loc);
	SET_FRAME(newrho, CONS(val, FRAME(newrho)));
	SET_TAG(FRAME(newrho), symbol);
	if (missing) {
	    SET_MISSING(FRAME(newrho), missing);
	    if (TYPEOF(val) == PROMSXP && PRENV(val) == rho) {
		SEXP deflt;
		SET_PRENV(val, newrho);
		/* find the symbol in the method, copy its expression
		   to the promise */
		for(deflt = CAR(op); deflt != R_NilValue; deflt = CDR(deflt)) {
		    if(TAG(deflt) == symbol)
			break;
		}
		if(deflt == R_NilValue)
		    error(_("symbol \"%s\" not in environment of method"),
			  CHAR(PRINTNAME(symbol)));
		SET_PRCODE(val, CAR(deflt));
	    }
	}
    }

    /* copy the bindings of the spacial dispatch variables in the top
       frame of the generic call to the new frame */
    defineVar(R_dot_defined, findVarInFrame(rho, R_dot_defined), newrho);
    defineVar(R_dot_Method, findVarInFrame(rho, R_dot_Method), newrho);
    defineVar(R_dot_target, findVarInFrame(rho, R_dot_target), newrho);

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    defineVar(R_dot_Generic, findVar(R_dot_Generic, rho), newrho);
    defineVar(R_dot_Methods, findVar(R_dot_Methods, rho), newrho);

    /* Find the calling context.  Should be R_GlobalContext unless
       profiling has inserted a CTXT_BUILTIN frame. */
    cptr = R_GlobalContext;
    if (cptr->callflag & CTXT_BUILTIN)
	cptr = cptr->nextcontext;

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    callerenv = cptr->sysparent; /* or rho? */

    /* get the rest of the stuff we need from the current context,
       execute the method, and return the result */
    call = cptr->call;
    arglist = cptr->promargs;
    val = R_execClosure(call, op, arglist, callerenv, newrho);
    UNPROTECT(1);
    return val;
}


/* -------------------------------------------------------------------------- */
/*                          R-LEVEL "EVAL" FUNCTIONS                          */

static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len, len_x = length(x);

    PROTECT(x);
    PROTECT(xnames = getAttrib(x, R_NamesSymbol)); 
                       /* isn't this protected via x?  Or could be concocted? */

    len = 0;
    if (xnames != R_NilValue) {
	for (i = 0; i < len_x; i++)
	    if (CHAR(STRING_ELT(xnames,i))[0] != 0) len += 1;
    }

    PROTECT(xnew = allocList(len));

    if (len > 0) {
	xptr = xnew;
	for (i = 0; i < len_x; i++) {
	    if (CHAR(STRING_ELT(xnames,i))[0] != 0) {
		SETCAR (xptr, VECTOR_ELT(x,i));
		SET_TAG (xptr, install_translated (STRING_ELT(xnames,i)));
		xptr = CDR(xptr);
	    }
	}
    } 

    UNPROTECT(3);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)

/* "eval" and "eval.with.vis" : Evaluate the first argument */
/* in the environment specified by the second argument. */

static SEXP do_eval (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP encl, x, xptr;
    volatile SEXP expr, env, tmp;

    int frame;
    RCNTXT cntxt;

    checkArity(op, args);

    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    if (isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !isEnvironment(encl) &&
		!isEnvironment((encl = simple_as_environment(encl))) )
	error(_("invalid '%s' argument"), "enclos");
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* so eval(expr, NULL, encl) works */
        break;
    case ENVSXP:
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = NewEnvironment(R_NilValue, duplicate(CADR(args)), encl);
        set_symbits_in_env(env);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(CADR(args));
	for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	    SET_NAMEDCNT_MAX(CAR(xptr));
	env = NewEnvironment(R_NilValue, x, encl);
        set_symbits_in_env(env);
	break;
    case INTSXP:
    case REALSXP:
	if (length(env) != 1)
	    error(_("numeric 'envir' arg not of length one"));
	frame = asInteger(env);
	if (frame == NA_INTEGER)
	    error(_("invalid '%s' argument"), "envir");
	env = R_sysframe(frame, R_GlobalContext);
	break;
    default:
	error(_("invalid '%s' argument"), "envir");
    }

    PROTECT(env); /* may no longer be what was passed in arg */

    /* isLanguage includes NILSXP, and that does not need to be evaluated,
       so don't use isLanguage(expr) || isSymbol(expr) || isByteCode(expr) */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP || isByteCode(expr)) {
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args, op);
	if (!SETJMP(cntxt.cjmpbuf))
	    expr = evalv (expr, env, VARIANT_PASS_ON(variant));
	else {
	    expr = R_ReturnedValue;
	    if (expr == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
            if ( ! (variant & VARIANT_PENDING_OK))
                WAIT_UNTIL_COMPUTED(R_ReturnedValue);
	}
	UNPROTECT_PROTECT(expr);
	endcontext(&cntxt);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	int i, n;
        int len;
        SEXP *srcrefs;
        getBlockSrcrefs(expr,&srcrefs,&len);
	n = LENGTH(expr);
	tmp = R_NilValue;
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args, op);
        SEXP savedsrcref = R_Srcref;
	if (!SETJMP(cntxt.cjmpbuf)) {
	    for (i = 0 ; i < n ; i++) {
                R_Srcref = getSrcref (srcrefs, len, i); 
		tmp = evalv (VECTOR_ELT(expr, i), env, 
                        i==n-1 ? VARIANT_PASS_ON(variant) 
                               : VARIANT_NULL | VARIANT_PENDING_OK);
            }
        }
	else {
	    tmp = R_ReturnedValue;
	    if (tmp == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
            if ( ! (variant & VARIANT_PENDING_OK))
                WAIT_UNTIL_COMPUTED(R_ReturnedValue);
	}
	UNPROTECT_PROTECT(tmp);
        R_Srcref = savedsrcref;
	endcontext(&cntxt);
	expr = tmp;
    }
    else if( TYPEOF(expr) == PROMSXP ) {
	expr = forcePromise(expr);
    } 
    else 
        ; /* expr is returned unchanged */

    if (PRIMVAL(op)) { /* eval.with.vis(*) : */
	PROTECT(expr);
	PROTECT(env = allocVector(VECSXP, 2));
	PROTECT(encl = allocVector(STRSXP, 2));
	SET_STRING_ELT(encl, 0, mkChar("value"));
	SET_STRING_ELT(encl, 1, mkChar("visible"));
	SET_VECTOR_ELT(env, 0, expr);
	SET_VECTOR_ELT(env, 1, ScalarLogicalMaybeConst(R_Visible));
	setAttrib(env, R_NamesSymbol, encl);
	expr = env;
	UNPROTECT(3);
    }

    UNPROTECT(1);
    return expr;
}

/* This is a special .Internal */
static SEXP do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    checkArity(op, args);
    x = CAR(args);
    x = eval(x, rho);
    PROTECT(x);
    PROTECT(ret = allocVector(VECSXP, 2));
    PROTECT(nm = allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("value"));
    SET_STRING_ELT(nm, 1, mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, ScalarLogicalMaybeConst(R_Visible));
    setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
static SEXP do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    SEXP s, ans ;
    cptr = R_GlobalContext;
    /* get the args supplied */
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr != NULL) {
	args = cptr->promargs;
    }
    /* get the env recall was called from */
    s = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == s)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error(_("'Recall' called from outside a closure"));

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->callfun != R_NilValue)
	PROTECT(s = cptr->callfun);
    else if( TYPEOF(CAR(cptr->call)) == SYMSXP)
	PROTECT(s = findFun(CAR(cptr->call), cptr->sysparent));
    else
	PROTECT(s = eval(CAR(cptr->call), cptr->sysparent));
    if (TYPEOF(s) != CLOSXP) 
    	error(_("'Recall' called from outside a closure"));
    ans = applyClosure_v(cptr->call, s, args, cptr->sysparent, NULL, 0);
    UNPROTECT(1);
    return ans;
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_eval[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"if",		do_if,		0,	1200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"for",		do_for,		0,	1100,	-1,	{PP_FOR,     PREC_FN,	  0}},
{"while",	do_while,	0,	1100,	-1,	{PP_WHILE,   PREC_FN,	  0}},
{"repeat",	do_repeat,	0,	1100,	-1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break,	0,	1000,	-1,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_next,	0,	1000,	-1,	{PP_NEXT,    PREC_FN,	  0}},
{"(",		do_paren,	0,	1000,	1,	{PP_PAREN,   PREC_FN,	  0}},
{"{",		do_begin,	0,	1200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"return",	do_return,	0,	1200,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	0,	1000,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		0,	1100,	2,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		0,	1100,	2,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		1,	1100,	2,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"->",		do_set,		10,	1100,	2,	{PP_ASSIGN,  PREC_RIGHT,	  1}},
{"->>",		do_set,		11,	1100,	2,	{PP_ASSIGN2, PREC_RIGHT,	  1}},
{"eval",	do_eval,	0,	1211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"eval.with.vis",do_eval,	1,	1211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

{"withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Logical Operators, all primitives */
/* these are group generic and so need to eval args (as builtin or themselves)*/

{"&",		do_andor,	1,	1000,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_andor,	2,	1000,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_not,		1,	1001,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_andor2,	1,	1000,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_andor2,	2,	1000,	2,	{PP_BINARY,  PREC_OR,	  0}},

/* these are group generic and so need to eval args */
{"all",		do_allany,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_allany,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* Arithmetic Operators, all primitives, now special, though always eval args */

{"+",		do_arith1,	PLUSOP,	1000,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"-",		do_arith1,	MINUSOP,1000,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"*",		do_arith2,	TIMESOP,1000,	2,	{PP_BINARY,  PREC_PROD,	  0}},
{"/",		do_arith2,	DIVOP,	1000,	2,	{PP_BINARY2, PREC_PROD,	  0}},
{"^",		do_arith2,	POWOP,	1000,	2,	{PP_BINARY2, PREC_POWER,  1}},
{"%%",		do_arith2,	MODOP,	1000,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%/%",		do_arith2,	IDIVOP,	1000,	2,	{PP_BINARY2, PREC_PERCENT,0}},

/* Relational Operators, all primitives */
/* these are group generic and so need to eval args (inside, as special) */

{"==",		do_relop,	EQOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop,	NEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop,	LTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop,	LEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop,	GEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop,	GTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},

/* Subset operators. */

{"[",		do_subset,	1,	1000,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	2,	101000,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{".el.methods",	do_subset2,	0,	101000,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	3,	101000,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},

/* Subassign operators.  The 100000 part of the flag is for the fast subassign
   interface; keep in sync with SetupSubsetSubassign. */

{"[<-",		do_subassign,	0,	101000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	101000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	30101000,3,	{PP_SUBASS,  PREC_LEFT,	  1}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}},
};

/* Fast built-in functions in this file. See names.c for documentation */

attribute_hidden FASTFUNTAB R_FastFunTab_eval[] = {
/*slow func	fast func,     code or -1   dsptch  variant */
{ do_not,	do_fast_not,	-1,		1,  VARIANT_PENDING_OK },
{ do_allany,	do_fast_allany,	OP_ALL,		1,  VARIANT_AND },
{ do_allany,	do_fast_allany,	OP_ANY,		1,  VARIANT_OR },
{ 0,		0,		0,		0,  0 }
};
