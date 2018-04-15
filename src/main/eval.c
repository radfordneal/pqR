/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
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


#define SCALAR_STACK_DEBUG 0


/* Inline version of findVarPendingOK, for speed when symbol is found
   from LASTSYMBINDING.  Doesn't necessarily set R_binding_cell. */

static inline SEXP FIND_VAR_PENDING_OK (SEXP sym, SEXP rho)
{
    rho = SKIP_USING_SYMBITS (rho, sym);

    if (LASTSYMENV(sym) == SEXP32_FROM_SEXP(rho)) {
        SEXP b = CAR(LASTSYMBINDING(sym));
        if (b != R_UnboundValue)
            return b;
        LASTSYMENV(sym) = R_NoObject32;
    }

    return findVarPendingOK(sym,rho);
}


/* Inline version of findFun, meant to be fast when a special symbol is found 
   in the base environmet. */

static inline SEXP FINDFUN (SEXP symbol, SEXP rho)
{
    rho = SKIP_USING_SYMBITS (rho, symbol);

    if (rho == R_GlobalEnv && BASE_CACHE(symbol)) {
        SEXP res = SYMVALUE(symbol);
        if (TYPEOF(res) == PROMSXP)
            res = PRVALUE_PENDING_OK(res);
        if (isFunction(res))
            return res;
    }

    return findFun_nospecsym(symbol,rho);
}


#define ARGUSED(x) LEVELS(x)

static SEXP Rf_builtin_op_no_cntxt (SEXP op, SEXP e, SEXP rho, int variant);

/*#define BC_PROFILING*/
#ifdef BC_PROFILING
static Rboolean bc_profiling = FALSE;
#endif

#define R_Profiling R_high_frequency_globals.Profiling

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <signal.h>
#endif /* not Win32 */

static FILE *R_ProfileOutfile = NULL;
static int R_Mem_Profiling=0;
extern void get_current_mem(unsigned long *,unsigned long *,unsigned long *); /* in memory.c */
extern unsigned long get_duplicate_counter(void);  /* in duplicate.c */
extern void reset_duplicate_counter(void);         /* in duplicate.c */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;

static void doprof(void)
{
    RCNTXT *cptr;
    char buf[1100];
    unsigned long bigv, smallv, nodes;
    int len;

    buf[0] = '\0';
    SuspendThread(MainThread);
    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < 1000) {
		sprintf(buf+len, ":%ld:%ld:%ld:%ld:", smallv, bigv,
		     nodes, get_duplicate_counter());
	    }
	    reset_duplicate_counter();
    }
    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < 1000) {
		strcat(buf, TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		       "<Anonymous>");
		strcat(buf, " ");
	    }
	}
    }
    ResumeThread(MainThread);
    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);
}

/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof();
    }
}
#else /* not Win32 */
static void doprof(int sig)
{
    RCNTXT *cptr;
    int newline = 0;
    unsigned long bigv, smallv, nodes;
    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if (!newline) newline = 1;
	    fprintf(R_ProfileOutfile, ":%ld:%ld:%ld:%ld:", smallv, bigv,
		     nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }
    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if (!newline) newline = 1;
	    fprintf(R_ProfileOutfile, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    if (newline) fprintf(R_ProfileOutfile, "\n");
    signal(SIGPROF, doprof);
}

static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else /* not Win32 */
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, doprof_null);
#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
    R_Profiling = 0;
}

static void R_InitProfiling(SEXP filename, int append, double dinterval, int mem_profiling)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

    interval = 1e6 * dinterval + 0.5;
    if(R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == NULL)
	error(_("Rprof: cannot open profile file '%s'"),
	      translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: sample.interval=%d\n", interval);
    else
	fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */
    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    R_Profiling = 1;
}

static SEXP do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP filename;
    int append_mode, mem_profiling;
    double dinterval;

#ifdef BC_PROFILING
    if (bc_profiling) {
	warning(_("can't use R profiling while byte code profiling"));
	return R_NilValue;
    }
#endif
    checkArity(op, args);
    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	error(_("invalid '%s' argument"), "filename");
    append_mode = asLogical(CADR(args));
    dinterval = asReal(CADDR(args));
    mem_profiling = asLogical(CADDDR(args));
    filename = STRING_ELT(CAR(args), 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
static SEXP do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("R profiling is not available on this system"));
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

#define CHECK_STACK_BALANCE(o,s) do { \
  if (s != R_PPStackTop) check_stack_balance(o,s); \
} while (0)

void attribute_hidden check_stack_balance(SEXP op, int save)
{
    if(save == R_PPStackTop) return;
    REprintf("Warning: stack imbalance in '%s', %d then %d\n",
	     PRIMNAME(op), save, R_PPStackTop);
}


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


/* Fast eval macro.  Does not set R_Visible, so should not be used if
   that is needed.  Does not check evaluation count, so should not be
   used if a loop without such a check might result.  Does not check
   expression depth or stack overflow, so should not be used if
   infinite recursion could result. */

static SEXP evalv_lang  (SEXP, SEXP, int);
static SEXP evalv_sym   (SEXP, SEXP, int);
static SEXP evalv_other (SEXP, SEXP, int);

#define EVALV(e, rho, variant) ( \
    R_variant_result = 0, \
    SELF_EVAL(TYPEOF(e)) ? \
       (UPTR_FROM_SEXP(e)->sxpinfo.nmcnt == MAX_NAMEDCNT ? e \
         : (UPTR_FROM_SEXP(e)->sxpinfo.nmcnt = MAX_NAMEDCNT, e)) \
     : SYM_NO_DOTS(e)       ? Rf_evalv_sym   (e, rho, variant) \
     :                        Rf_evalv_other (e, rho, variant) \
)


/* The "evalv" function returns the value of "e" evaluated in "rho",
   with given variant.  The caller must ensure that both SEXP
   arguments are protected.  The "eval" function is just like "evalv"
   with 0 for the variant return argument. */

SEXP Rf_builtin_op (SEXP op, SEXP e, SEXP rho, int variant);

#define evalcount R_high_frequency_globals.evalcount


SEXP eval (SEXP e, SEXP rho)
{
    return evalv (e, rho, 0);
}

SEXP evalv (SEXP e, SEXP rho, int variant)
{
    R_Visible = TRUE;
    
    /* Handle check for user interrupt. */

    if (--evalcount < 0) {
        R_CheckUserInterrupt();
        evalcount = 1000;
    }

    /* Check for stack overflow. */

    R_CHECKSTACK();

    /* Handle check for too deep expression nesting. */

    R_EvalDepth += 1;

    if (R_EvalDepth > R_Expressions) {
        R_Expressions = R_Expressions_keep + 500;
        errorcall (R_NilValue /* avoids deparsing call in the error handler */,
         _("evaluation nested too deeply: infinite recursion / options(expressions=)?"));
    }

    /* Do evaluation as for fast eval macro. */

    SEXP res = EVALV (e, rho, variant);

    R_EvalDepth -= 1;

#   if SCALAR_STACK_DEBUG /* Get debug output after typing SCALAR.STACK.DEBUG */
        if (installed_already("SCALAR.STACK.DEBUG") != R_NoObject) {
            if (ON_SCALAR_STACK(res)) {
                REprintf("SCALAR STACK VALUE RETURNED: %llx %llx %llx %s %f\n",
                 (long long) R_scalar_stack_start,
                 (long long) res, 
                 (long long) R_scalar_stack,
                 TYPEOF(res)==INTSXP ? "int" : "real",
                 TYPEOF(res)==INTSXP ? (double)*INTEGER(res) : *REAL(res));
            }
#           if 0
                REprintf("STACK:\n");
                for (int i = 0; i < 6; i++) {
                    if (SCALAR_STACK_ENTRY(i)==R_scalar_stack) REprintf("@@\n");
                    R_inspect(SCALAR_STACK_ENTRY(i));
                }
                REprintf("END\n");
#           endif
        }
#   endif

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

/* Evaluate an expression that is a symbol other than ..., ..1, ..2, etc. */

SEXP attribute_hidden Rf_evalv_sym (SEXP e, SEXP rho, int variant)
{
    SEXP res;

    res = FIND_VAR_PENDING_OK (e, rho);

    if (TYPEOF(res) == PROMSXP) {
        if (PRVALUE_PENDING_OK(res) == R_UnboundValue)
            res = forcePromiseUnbound(res,variant);
        else
            res = PRVALUE_PENDING_OK(res);
    }
    else if (TYPEOF(res) == SYMSXP) {
        if (res == R_MissingArg) {
            if ( ! (variant & VARIANT_MISSING_OK))
                if (!DDVAL(e))  /* revert bug fix for the moment */
                    arg_missing_error(e);
        }
        else if (res == R_UnboundValue)
            unbound_var_error(e);
    }

    /* A NAMEDCNT of 0 might arise from an inadverently missing increment
       somewhere, or from a save/load sequence (since loaded values in
       promises have NAMEDCNT of 0), so fix up here... */

    SET_NAMEDCNT_NOT_0(res);

    if ( ! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_COMPUTED(res);

    return res;
}

/* Evaluate an expression that is not self-evaluating and not a symbol
   (other than ..., ..1, ..2, etc.). */

SEXP attribute_hidden Rf_evalv_other (SEXP e, SEXP rho, int variant)
{
    SEXP op, res;

    if (TYPEOF(e) == LANGSXP) {

#       ifdef Win32
            /* Reset precision, rounding and exception modes of an ix86 fpu. */
            __asm__ ( "fninit" );
#       endif

#       if SCALAR_STACK_DEBUG
            SEXP sv_stack = R_scalar_stack;
#       endif

        SEXP fn = CAR(e), args = CDR(e);

        if (SYM_NO_DOTS(fn))
            op = FINDFUN(fn,rho);
        else
            op = eval(fn,rho);

        if (RTRACE(op)) R_trace_call(e,op);

        if (TYPEOF(op) == CLOSXP) {
            PROTECT(op);
            res = applyClosure_v (e, op, promiseArgs(args,rho), rho, 
                                  NULL, variant);
            UNPROTECT(1);
        }
        else {
            int save = R_PPStackTop;
            const void *vmax = VMAXGET();

            R_Visible = TRUE;

            if (TYPEOF(op) == SPECIALSXP)
                res = CALL_PRIMFUN (e, op, args, rho, variant);
            else if (TYPEOF(op) == BUILTINSXP)
                res = R_Profiling ? Rf_builtin_op(op, e, rho, variant)
                                  : Rf_builtin_op_no_cntxt(op, e, rho, variant);
            else
                apply_non_function_error();

            if (!R_Visible && PRIMPRINT(op) == 0)
                R_Visible = TRUE;

            CHECK_STACK_BALANCE(op, save);
            VMAXSET(vmax);
        }

#       if SCALAR_STACK_DEBUG
            if (variant & VARIANT_SCALAR_STACK_OK) {
                if (R_scalar_stack != sv_stack && (res != sv_stack 
                      || SCALAR_STACK_OFFSET(1) != sv_stack)) abort();
            }
            else {
                if (R_scalar_stack != sv_stack) abort();
            }
#       endif
    }

    else if (TYPEOF(e) == SYMSXP) {  /* Must be ... or ..1, ..2, etc. */

        if (e == R_DotsSymbol)
            dotdotdot_error();

        R_Visible = TRUE;  /* May be set FALSE by active binding / lazy eval */

        res = ddfindVar(e,rho);

        if (TYPEOF(res) == PROMSXP) {
            if (PRVALUE_PENDING_OK(res) == R_UnboundValue)
                res = forcePromiseUnbound(res,variant);
            else
                res = PRVALUE_PENDING_OK(res);
        }
        else if (TYPEOF(res) == SYMSXP) {
            if (res == R_MissingArg) {
                if ( ! (variant & VARIANT_MISSING_OK))
                    if (!DDVAL(e))  /* revert bug fix for the moment */
                        arg_missing_error(e);
            }
            else if (res == R_UnboundValue)
                unbound_var_error(e);
        }

        /* A NAMEDCNT of 0 might arise from an inadverently missing increment
           somewhere, or from a save/load sequence (since loaded values in
           promises have NAMEDCNT of 0), so fix up here... */

        SET_NAMEDCNT_NOT_0(res);

        if ( ! (variant & VARIANT_PENDING_OK))
            WAIT_UNTIL_COMPUTED(res);
    }

    else if (TYPEOF(e) == PROMSXP) {

        if (PRVALUE_PENDING_OK(e) == R_UnboundValue)
            res = forcePromiseUnbound(e,variant);
        else
            res = PRVALUE_PENDING_OK(e);

        if ( ! (variant & VARIANT_PENDING_OK))
            WAIT_UNTIL_COMPUTED(res);

        R_Visible = TRUE;
    }

    else if (TYPEOF(e) == BCODESXP) {

        res = bcEval(e, rho, TRUE);
    }

    else if (TYPEOF(e) == DOTSXP)
        dotdotdot_error();

    else
        UNIMPLEMENTED_TYPE("eval", e);

    return res;
}

/* e is protected here */
SEXP attribute_hidden forcePromiseUnbound (SEXP e, int variant)
{
    SEXP val;

    val = PRCODE(e);

    if ( ! SELF_EVAL(TYPEOF(val)) ) {

        RPRSTACK prstack;

        if (PRSEEN(e) == 1) PRSEEN_error(e);

        /* Mark the promise as under evaluation and push it on a stack
           that can be used to unmark pending promises if a jump out
           of the evaluation occurs. */

        prstack.promise = e;
        prstack.next = R_PendingPromises;
        R_PendingPromises = &prstack;

        SET_PRSEEN(e, 1);

        PROTECT(e);

        val = EVALV (val, PRENV(e), 
                     (variant & VARIANT_PENDING_OK) | VARIANT_MISSING_OK);

        UNPROTECT(1);

        /* Pop the stack, unmark the promise and set its value field. */

        R_PendingPromises = prstack.next;
        SET_PRSEEN(e, 0);
    }

    SET_PRVALUE_MACRO(e, val);
    
    if (val == R_MissingArg) {  /* Attempt to mimic past behaviour... */
        if ( ! (variant & VARIANT_MISSING_OK) && TYPEOF(PRCODE(e)) == SYMSXP
                  && R_isMissing (PRCODE(e), PRENV(e)))
            arg_missing_error(PRCODE(e));
    }
    else {

        /* Set the environment to R_NilValue to allow GC to reclaim the
           promise environment (unless value is R_MissingArg); this is
           also useful for fancy games with delayedAssign() */

        SET_PRENV_NIL(e);
    }

    INC_NAMEDCNT(val);

    return val;
}

SEXP forcePromise (SEXP e) /* e protected here if necessary */
{
    if (PRVALUE(e) == R_UnboundValue) {
        return forcePromiseUnbound(e,0);
    }
    else
        return PRVALUE(e);
}


/* Like Rf_builtin_op (defined in builtin.c) except that no context is
   created.  Making this separate from Rf_builtin_op saves on stack
   space for the local context variable.  Since the somewhat
   time-consuming context creation is not done, there is no advantage
   to evaluating a single argument with pending OK. */

static SEXP Rf_builtin_op_no_cntxt (SEXP op, SEXP e, SEXP rho, int variant)
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
                    UNPROTECT(1);
                    PROTECT(args = CONS(arg1,R_NilValue));
                    goto not_fast;
                }
            }

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

    res = CALL_PRIMFUN(e, op, args, rho, variant);

    UNPROTECT(1); /* args */
    return res;
}


attribute_hidden
void SrcrefPrompt(const char * prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != R_NilValue) {
        if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = getAttrib00(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = findVar(install("filename"), srcfile);
	    if (isString(filename) && length(filename)) {
	    	Rprintf(_("%s at %s#%d: "), prefix, CHAR(STRING_ELT(filename, 0)), 
	                                    asInteger(srcref));
	        return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}


/* This function gets the srcref attribute from a statement block, 
   and confirms it's in the expected format */
   
static inline void getBlockSrcrefs(SEXP call, SEXP **refs, int *len)
{
    SEXP srcrefs = getAttrib00(call, R_SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) {
        *refs = (SEXP *) DATAPTR(srcrefs);
        *len = LENGTH(srcrefs);
    }
    else
    {   *len = 0;
    }
}


/* This function extracts one srcref, and confirms the format.  It is 
   passed an index and the array and length from getBlockSrcrefs. */

static inline SEXP getSrcref(SEXP *refs, int len, int ind)
{
    if (ind < len) {
        SEXP result = refs[ind];
        if (TYPEOF(result) == INTSXP && LENGTH(result) >= 6)
            return result;
    }

    return R_NilValue;
}

static void printcall (SEXP call, SEXP rho)
{
    int old_bl = R_BrowseLines;
    int blines = asInteger(GetOption1(install("deparse.max.lines")));
    if (blines != NA_INTEGER && blines > 0) R_BrowseLines = blines;
    PrintValueRec(call,rho);
    R_BrowseLines = old_bl;
}

static void start_browser (SEXP call, SEXP op, SEXP stmt, SEXP env)
{
    SrcrefPrompt("debug", R_Srcref);
    PrintValue(stmt);
    do_browser(call, op, R_NilValue, env);
}

/* 'supplied' is an array of SEXP values, first a set of pairs of tag and
   value, then a pairlist of tagged values (or R_NilValue).  If NULL, no
   extras supplied. */

SEXP attribute_hidden applyClosure_v(SEXP call, SEXP op, SEXP arglist, SEXP rho,
                                     SEXP *supplied, int variant)
{
    int vrnt = VARIANT_PENDING_OK | VARIANT_DIRECT_RETURN | VARIANT_WHOLE_BODY
                 | VARIANT_PASS_ON(variant);

    if (variant & VARIANT_NOT_WHOLE_BODY)
        vrnt &= ~VARIANT_WHOLE_BODY;

    SEXP formals, actuals, savedrho, savedsrcref;
    volatile SEXP body, newrho;
    SEXP f, a, res;
    RCNTXT cntxt;

    PROTECT2(op,arglist);

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    /*  Set up a context with the call in it for use if an error occurs below
        in matchArgs or from running out of memory (eg, in NewEnvironment). */

    begincontext(&cntxt, CTXT_RETURN, call, savedrho, rho, arglist, op);
    savedsrcref = R_Srcref;  /* saved in context for longjmp, and protection */

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Note that actuals is protected via
        newrho. */

    actuals = matchArgs(formals, NULL, 0, arglist, call);
    PROTECT(newrho = NewEnvironment(R_NilValue, actuals, savedrho));
        /* no longer passes formals, since matchArg now puts tags in actuals */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (MISSING(a) && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    set_symbits_in_env (newrho);

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

    /* Debugging */

    if (RDEBUG(op) | RSTEP(op)) {
        SET_RDEBUG(newrho, 1);
        if (RSTEP(op)) SET_RSTEP(op, 0);
	SEXP savesrcref; SEXP *srcrefs; int len;
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
        printcall(call,rho);
	savesrcref = R_Srcref;
	getBlockSrcrefs(body,&srcrefs,&len);
	PROTECT(R_Srcref = getSrcref(srcrefs,len,0));
        start_browser (call, op, body, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(res = evalv (body, newrho, vrnt));
	}
	else {
	    PROTECT(res = R_ReturnedValue);
        }
    }
    else {
	PROTECT(res = evalv (body, newrho, vrnt));
    }

    R_variant_result &= ~VARIANT_RTN_FLAG;

    R_Srcref = savedsrcref;
    endcontext(&cntxt);

    if ( ! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_COMPUTED(res);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
        printcall(call,rho);
    }

    UNPROTECT(3); /* op, arglist, res */
    return res;
}

SEXP applyClosure (SEXP call, SEXP op, SEXP arglist, SEXP rho, 
                   SEXP *supplied)
{
    if (supplied != NULL) error("Last argument to applyClosure must be NULL");
    return applyClosure_v (call, op, arglist, rho, NULL, 0);
}

/* **** FIXME: This code is factored out of applyClosure.  If we keep
   **** it we should change applyClosure to run through this routine
   **** to avoid code drift. */
static SEXP R_execClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			  SEXP newrho)
{
    volatile SEXP body;
    SEXP res, savedsrcref;
    RCNTXT cntxt;

    PROTECT2(op,arglist);

    body = BODY(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);
    savedsrcref = R_Srcref;  /* saved in context for longjmp, and protection */

    /* Get the srcref record from the closure object.  Disable for now
       at least, since it's not clear that it's needed. */
    
    R_Srcref = R_NilValue;  /* was: getAttrib(op, R_SrcrefSymbol); */

    /* Debugging */

    if (RDEBUG(op) | RSTEP(op)) {
        SET_RDEBUG(newrho, 1);
        if (RSTEP(op)) SET_RSTEP(op, 0);
        SEXP savesrcref; SEXP *srcrefs; int len;
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	printcall (call, rho);
	savesrcref = R_Srcref;
	getBlockSrcrefs(body,&srcrefs,&len);
	PROTECT(R_Srcref = getSrcref(srcrefs,len,0));
        start_browser (call, op, body, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(res = evalv(body, newrho, VARIANT_NOT_WHOLE_BODY));
	}
	else {
	    PROTECT(res = R_ReturnedValue);
            WAIT_UNTIL_COMPUTED(res);
        }
    }
    else {
	PROTECT(res = eval(body, newrho));
    }

    R_Srcref = savedsrcref;
    endcontext(&cntxt);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
	printcall (call, rho);
    }

    UNPROTECT(3);  /* op, arglist, res */
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
		 * to the promise */
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


#define BodyHasBraces(body) \
    (isLanguage(body) && CAR(body) == R_BraceSymbol)


static SEXP do_if (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP Cond, Stmt;
    int absent_else = 0;

    Cond = CAR(args); args = CDR(args);
    Stmt = CAR(args); args = CDR(args);

    SEXP condval = EVALV (Cond, rho, VARIANT_SCALAR_STACK_OK);
    int condlogical = asLogicalNoNA (condval, call);
    POP_IF_TOP_OF_STACK(condval);

    if (!condlogical) {
        /* go to else part */
        if (args != R_NilValue)
            Stmt = CAR(args);
        else {
            absent_else = 1;
            Stmt = R_NilValue;
        }
    }

    if (RDEBUG(rho) && Stmt!=R_NilValue && !BodyHasBraces(Stmt))
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

   Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK.
 */

#define DO_LOOP_RDEBUG(call, op, body, rho, bgn) do { \
        if (!bgn && RDEBUG(rho)) start_browser (call, op, body, rho); \
    } while (0)

static SEXP do_for (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and that might change between
       the setjmp and longjmp calls.  Theoretically this does not include
       n, bgn, and some others, but gcc -O2 -Wclobbered warns about some, 
       so to be safe we declare them volatile as well. */

    volatile int i, n, bgn;
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
    SEXP s;
    int j;

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

    PROTECT(val = EVALV (val, rho, in    ? VARIANT_SEQ | VARIANT_ANY_ATTR :
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
        seq_start = R_variant_seq_spec >> 32;
        n = (R_variant_seq_spec >> 1) & 0x7fffffff;
        val_type = INTSXP;
    }
    else { /* non-variant "in" value */

        INC_NAMEDCNT(val);  /* increment NAMEDCNT to avoid mods by loop code */
        nval = val;  /* for scanning pairlist */

        /* Deal with the case where we are iterating over a factor.
           We need to coerce to character, then iterate */

        if (inherits_CHAR (val, R_factor_CHARSXP)) {
            val = asCharacterFactor(val);
            UNPROTECT(1);
            PROTECT(val);
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
    bgn = BodyHasBraces(body);

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

            if (TYPEOF(v) != val_type || LENGTH(v) != 1 || HAS_ATTRIB(v)
                                      || NAMEDCNT_GT_1(v))
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

        DO_LOOP_RDEBUG(call, op, body, rho, bgn);

        evalv (body, rho, VARIANT_NULL | VARIANT_PENDING_OK);

    for_next: ;  /* semi-colon needed for attaching label */
    }

 for_break:
    endcontext(&cntxt);
    if (in && !is_seq)
        DEC_NAMEDCNT(val);
    if (nsyms == 1)
        UNPROTECT(2);  /* v, bcell */
    else 
        UNPROTECT(4);  /* dims, indexes, ixvals, bcells */
    UNPROTECT(3);      /* val, rho, args */
    SET_RDEBUG(rho, dbg);

    R_Visible = FALSE;
    return R_NilValue;
}


/* While statement.  Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK. */

static SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP body;
    RCNTXT cntxt;

    R_Visible = FALSE;

    dbg = RDEBUG(rho);
    body = CADR(args);
    bgn = BodyHasBraces(body);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);

    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) { /* <- back here for "next" */
        for (;;) {
            SEXP condval = EVALV (CAR(args), rho, VARIANT_SCALAR_STACK_OK);
            int condlogical = asLogicalNoNA (condval, call);
            POP_IF_TOP_OF_STACK(condval);
            if (!condlogical) 
                break;
	    DO_LOOP_RDEBUG(call, op, body, rho, bgn);
	    evalv (body, rho, VARIANT_NULL | VARIANT_PENDING_OK);
	}
    }

    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);

    R_Visible = FALSE;
    return R_NilValue;
}


/* Repeat statement.  Evaluates body with VARIANT_NULL | VARIANT_PENDING_OK. */

static SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP body;
    RCNTXT cntxt;

    R_Visible = FALSE;

    dbg = RDEBUG(rho);
    body = CAR(args);
    bgn = BodyHasBraces(body);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);

    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) { /* <- back here for "next" */
	for (;;) {
	    DO_LOOP_RDEBUG(call, op, body, rho, bgn);
	    evalv (body, rho, VARIANT_NULL | VARIANT_PENDING_OK);
	}
    }

    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);

    R_Visible = FALSE;
    return R_NilValue;
}


static R_NORETURN SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    findcontext(PRIMVAL(op), rho, R_NilValue);
}

/* Parens are now a SPECIAL, to avoid overhead of creating an arg list. 
   Also avoids overhead of calling checkArity when there is no error.  
   Care is taken to allow (...) when ... is bound to exactly one argument, 
   though it is debatable whether this should be considered valid. 

   The eval variant requested is passed on to the inner expression. */

static SEXP do_paren (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    if (args!=R_NilValue && CAR(args)==R_DotsSymbol && CDR(args)==R_NilValue) {
        args = findVar(R_DotsSymbol, rho);
        if (TYPEOF(args) != DOTSXP)
            args = R_NilValue;
    }

    if (args == R_NilValue || CDR(args) != R_NilValue)
        checkArity(op, args);

    return evalv (CAR(args), rho, VARIANT_PASS_ON(variant));
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
    variant = VARIANT_PASS_ON(variant);
    if (variant & VARIANT_DIRECT_RETURN) 
        vrnt |= variant;

    for (int i = 1; ; i++) {
        arg = CAR(args);
        args = CDR(args);
        R_Srcref = getSrcref (srcrefs, len, i);
        if (RDEBUG(rho))
            start_browser (call, op, arg, rho);
        if (args == R_NilValue)
            break;
        s = EVALV (arg, rho, vrnt);
        if (R_variant_result & VARIANT_RTN_FLAG) {
            R_Srcref = savedsrcref;
            return s;
        }
    }

    s = evalv (arg, rho, variant);
    R_Srcref = savedsrcref;
    return s;
}


static SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP v;

    if (args == R_NilValue) /* zero arguments provided */
	v = R_NilValue;
    else if (CDR(args) == R_NilValue) /* one argument */
	v = evalv (CAR(args), rho, ! (variant & VARIANT_DIRECT_RETURN) ? 0
                    : VARIANT_PASS_ON(variant) & ~ VARIANT_NULL);
    else
	errorcall(call, _("multi-argument returns are not permitted"));

    if (variant & VARIANT_DIRECT_RETURN) {
        R_variant_result |= VARIANT_RTN_FLAG;
        return v;
    }

    findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, v);
}

/* Declared with a variable number of args in names.c */
static SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    return rval;
}

#define ASSIGNBUFSIZ 32
static SEXP installAssignFcnName(SEXP fun)
{
    /* Handle "[", "[[", and "$" specially for speed. */

    if (fun == R_BracketSymbol)
       return R_SubAssignSymbol;

    if (fun == R_Bracket2Symbol)
        return R_SubSubAssignSymbol;

    if (fun == R_DollarSymbol)
        return R_DollarAssignSymbol;

    /* The general case for a symbol */

    if (TYPEOF(fun) == SYMSXP) {

        char buf[ASSIGNBUFSIZ];
        const char *fname = CHAR(PRINTNAME(fun));

        if (!copy_2_strings (buf, sizeof buf, fname, "<-"))
            error(_("overlong name in '%s'"), fname);

        return install(buf);
    }

    /* Handle foo::bar and foo:::bar. */

    if (TYPEOF(fun)==LANGSXP && length(fun)==3 && TYPEOF(CADDR(fun))==SYMSXP
      && (CAR(fun)==R_DoubleColonSymbol || CAR(fun)==R_TripleColonSymbol))
        return lang3 (CAR(fun), CADR(fun), installAssignFcnName(CADDR(fun)));

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


/* Macro used in promiseArgs and promiseArgsTwo. */

#define MAKE_PROMISE(a,rho) do { \
    if (TYPEOF(a) == PROMSXP) { \
        INC_NAMEDCNT(a); \
        SEXP p = PRVALUE_PENDING_OK(a); \
        if (p != R_UnboundValue && NAMEDCNT_GT_0(p)) \
            INC_NAMEDCNT(p); \
    } \
    else if (a != R_MissingArg && a != R_MissingUnder) \
        a = mkPROMISE (a, rho); \
} while (0)


/* Create two lists of promises to evaluate each argument, with promises
   shared.  When an argument is evaluated in a call using one argument list,
   its value is then known without re-evaluation in a second call using 
   the second argument list.  The argument lists are terminated with the
   initial values of *a1 and *a2. */

static void promiseArgsTwo (SEXP el, SEXP rho, SEXP *a1, SEXP *a2)
{
    /* Handle 0 or 1 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) {  /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue)
            return;
        SEXP a = CAR(el);
        SEXP t = TAG(el);
        if (a != R_DotsSymbol) {
            MAKE_PROMISE(a,rho);
            PROTECT (*a1 = cons_with_tag (a, *a1, t));
            *a2 = cons_with_tag (a, *a2, t);
            UNPROTECT(1);
            return;
        }
    }

    /* The general case (except el == R_NilValue handled above). */

    BEGIN_PROTECT6 (head1, tail1, head2, tail2, ev, h);

    head1 = head2 = R_NilValue;

    do {  /* el won't be R_NilValue, so we loop at least once */

        SEXP a = CAR(el);

	/* If we have a ... symbol, we look to see what it is bound to.
	   If its binding is R_NilValue we just ignore it.  If it is bound
           to a ... list of promises, we repromise all the promises and 
           then splice the list of resulting values into the return value.
	   Anything else bound to a ... symbol is an error. */

	if (a == R_DotsSymbol) {
	    h = findVar(a, rho);
            if (h == R_NilValue) {
                /* nothing */
            }
	    else if (TYPEOF(h) == DOTSXP) {
		while (h != R_NilValue) {
                    a = CAR(h);
                    MAKE_PROMISE(a,rho);
                    INC_NAMEDCNT(a);
                    ev = cons_with_tag (a, R_NilValue, TAG(h));
                    if (head1==R_NilValue)
                        head1 = ev;
                    else
                        SETCDR(tail1,ev);
                    tail1 = ev;
                    ev = cons_with_tag (a, R_NilValue, TAG(h));
                    if (head2==R_NilValue)
                        head2 = ev;
                    else
                        SETCDR(tail2,ev);
                    tail2 = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		dotdotdot_error();
	}
        else {
            MAKE_PROMISE(a,rho);
            INC_NAMEDCNT(a);
            ev = cons_with_tag (a, R_NilValue, TAG(el));
            if (head1 == R_NilValue)
                head1 = ev;
            else
                SETCDR(tail1, ev);
            tail1 = ev;
            ev = cons_with_tag (a, R_NilValue, TAG(el));
            if (head2 == R_NilValue)
                head2 = ev;
            else
                SETCDR(tail2, ev);
            tail2 = ev;
        }

	el = CDR(el);

    } while (el != R_NilValue);

    if (head1 != R_NilValue) {
        if (*a1 != R_NilValue)
            SETCDR(tail1,*a1);
        *a1 = head1;
        if (*a2 != R_NilValue)
            SETCDR(tail2,*a2);
        *a2 = head2;
    }

    END_PROTECT;
}


/*  Assignment in its various forms. */

SEXP Rf_set_subassign (SEXP call, SEXP lhs, SEXP rhs, SEXP rho, 
                       int variant, int opval);

static SEXP do_set (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP a;

    if ((a = CDR(args)) == R_NilValue /* includes case of args == R_NilValue */
          || CDR(a) != R_NilValue)
        checkArity(op,args);

    SEXP lhs = CAR(args), rhs = CAR(a);
    int opval = PRIMVAL(op);

    /* Swap operands for -> and ->>. */

    if (opval >= 10) {
        rhs = lhs;
        lhs = CAR(a);
        opval -= 10;
    }

    /* Convert lhs string to a symbol. */

    if (TYPEOF(lhs) == STRSXP) {
        lhs = install(translateChar(STRING_ELT(lhs, 0)));
    }

    if (TYPEOF(lhs) == SYMSXP) {

        /* -- ASSIGNMENT TO A SIMPLE VARIABLE -- */

        /* Handle <<- without trying the optimizations done below. */

        if (opval == 2) {
            rhs = EVALV (rhs, rho, VARIANT_PENDING_OK);
            set_var_nonlocal (lhs, rhs, ENCLOS(rho), 3);
            goto done;
        }

        /* Handle assignment into base and user database environments without
           any special optimizations. */

        if (IS_BASE(rho) || IS_USER_DATABASE(rho)) {
            rhs = EVALV (rhs, rho, VARIANT_PENDING_OK);
            set_var_in_frame (lhs, rhs, rho, TRUE, 3);
            goto done;
        }

        /* We decide whether we'll ask the right hand side evalutation to do
           the assignment, for statements like v<-exp(v), v<-v+1, or v<-2*v. */

        int local_assign = 0;

        if (TYPEOF(rhs) == LANGSXP) {
            if (CADR(rhs) == lhs) 
                local_assign = VARIANT_LOCAL_ASSIGN1;
            else if (CADDR(rhs) == lhs)
                local_assign = VARIANT_LOCAL_ASSIGN2;
        }

        /* Evaluate the right hand side, asking for it on the scalar stack. */

        rhs = EVALV (rhs, rho, 
                local_assign | VARIANT_PENDING_OK | VARIANT_SCALAR_STACK_OK);

        /* See if the assignment was done by the rhs operator. */

        if (R_variant_result) {
            R_variant_result = 0;
            goto done;
        }

        /* Try to copy the value, not assign the object, if the rhs is
           scalar and doesn't have zero NAMEDCNT (for which assignment
           would be free).  This will copy from the scalar stack,
           which must be replaced by a regular value if the copy can't
           be done.  If the copy can't be done, but a binding cell was
           found here, the assignment is done directly into the binding 
           cell, avoiding overhead of calling set_var_in_frame.

           Avoid accessing NAMEDCNT in a way that will cause unnecessary waits
           for task completion. */

        if (isVectorNonpointer(rhs) && LENGTH(rhs) == 1 && NAMEDCNT_GT_0(rhs)) {
            SEXPTYPE rhs_type = TYPEOF(rhs);
            SEXP v;
            if (SEXP32_FROM_SEXP(rho) != LASTSYMENV(lhs) 
                  || BINDING_IS_LOCKED((R_binding_cell = LASTSYMBINDING(lhs)))
                  || (v = CAR(R_binding_cell)) == R_UnboundValue)
                v = findVarInFrame3_nolast (rho, lhs, 7);
            if (v != R_UnboundValue && TYPEOF(v) == rhs_type && LENGTH(v) == 1
                 && ATTRIB(v) == ATTRIB(rhs) && TRUELENGTH(v) == TRUELENGTH(rhs)
                 && LEVELS(v) == LEVELS(rhs) && !NAMEDCNT_GT_1(v)) {
                SET_NAMEDCNT_NOT_0(v);
                POP_IF_TOP_OF_STACK(rhs);
                helpers_wait_until_not_in_use(v);
                WAIT_UNTIL_COMPUTED(v);
                switch (rhs_type) {
                case LGLSXP:  *LOGICAL(v) = *LOGICAL(rhs); break;
                case INTSXP:  *INTEGER(v) = *INTEGER(rhs); break;
                case REALSXP: *REAL(v)    = *REAL(rhs);    break;
                case CPLXSXP: *COMPLEX(v) = *COMPLEX(rhs); break;
                case RAWSXP:  *RAW(v)     = *RAW(rhs);     break;
                }
                rhs = v; /* for return value */
                goto done;
            }
            if (POP_IF_TOP_OF_STACK(rhs))
                rhs = DUP_STACK_VALUE(rhs);
            if (R_binding_cell != R_NilValue) {
                DEC_NAMEDCNT_AND_PRVALUE(v);
                SETCAR(R_binding_cell, rhs);
                SET_MISSING(R_binding_cell,0);
                INC_NAMEDCNT(rhs);
                if (rho == R_GlobalEnv) 
                    R_DirtyImage = 1;
                goto done;
            }
        }

        /* Assign rhs object to lhs symbol the usual way. */

        set_var_in_frame (lhs, rhs, rho, TRUE, 3);
    }

    else if (TYPEOF(lhs) == LANGSXP) {

        /* -- ASSIGNMENT TO A COMPLEX TARGET -- */

        rhs = Rf_set_subassign (call, lhs, rhs, rho, variant, opval);
    }

    else {
        errorcall (call, _("invalid assignment left-hand side"));
    }

  done:

    R_Visible = FALSE;

    if (variant & VARIANT_NULL)
        return R_NilValue;

    if ( ! (variant & VARIANT_PENDING_OK)) 
        WAIT_UNTIL_COMPUTED(rhs);
    
    return rhs;
}

/* Complex assignment.  Made a separate, non-static, function in order
   to avoid possible overhead of a large function (eg, stack frame size)
   for the simple case. */

SEXP attribute_hidden Rf_set_subassign (SEXP call, SEXP lhs, SEXP rhs, SEXP rho,
                                        int variant, int opval)
{
    SEXP var, varval, newval, rhsprom, lhsprom, e, fn;

    /* Find the variable ultimately assigned to, and its depth.
       The depth is 1 for a variable within one replacement function
       (eg, in names(a) <- ...). */

    int depth = 1;
    for (var = CADR(lhs); TYPEOF(var) != SYMSXP; var = CADR(var)) {
        if (TYPEOF(var) != LANGSXP) {
            if (TYPEOF(var) == STRSXP && LENGTH(var) == 1) {
                var = install (CHAR (STRING_ELT(var,0)));
                break;
            }
            errorcall (call, _("invalid assignment left-hand side"));
        }
        depth += 1;
    }

    /* Find the assignment function symbol for the depth 1 assignment, and
       see if we maybe (tentatively) will be using the fast interface. */

    SEXP assgnfcn = installAssignFcnName(CAR(lhs));

    int maybe_fast = assgnfcn == R_SubAssignSymbol ||
                     assgnfcn == R_DollarAssignSymbol ||
                     assgnfcn == R_SubSubAssignSymbol;

    /* We evaluate the right hand side now, asking for it on the
       scalar stack if we (tentatively) will be using the fast
       interface (unless value needed for return, and not allowed on
       scalar stack), and otherwise for pending computation. */

    SEXP rhs_uneval = rhs;  /* save unevaluated rhs */

    if (maybe_fast) {
        PROTECT(rhs = EVALV (rhs, rho, 
                       (variant & (VARIANT_SCALAR_STACK_OK | VARIANT_NULL)) ? 
                         VARIANT_SCALAR_STACK_OK : 0));
    }
    else
        PROTECT(rhs = EVALV (rhs, rho, VARIANT_PENDING_OK));

    /* Increment NAMEDCNT temporarily if rhs will be needed as the value,
       to protect it from being modified by the assignment, or otherwise. */

    if ( ! (variant & VARIANT_NULL))
        INC_NAMEDCNT(rhs);

    /* Get the value of the variable assigned to, and ensure it is local
       (unless this is the <<- operator).  Save and protect the binding 
       cell used. */

    if (opval == 2) /* <<- */
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
    PROTECT(bcell);

    if (TYPEOF(varval) == PROMSXP)
        varval = forcePromise(varval);
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

    /* Special code for depth of 1.  This is purely an optimization - the
       general code below should also work when depth is 1. */

    if (depth == 1) {
        if (maybe_fast && !isObject(varval)
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
            UNPROTECT(3);
        }
        else {
            if (POP_IF_TOP_OF_STACK(rhs)) 
                rhs = DUP_STACK_VALUE(rhs);
            PROTECT (rhsprom = mkValuePROMISE(rhs_uneval, rhs));
            PROTECT (lhsprom = mkValuePROMISE(CADR(lhs), varval));
            PROTECT(e = replaceCall (assgnfcn, lhsprom, CDDR(lhs), rhsprom));
            newval = eval(e,rho);
            UNPROTECT(6);
        }
    }

    else {  /* the general case, for any depth */

        SEXP v, b, op, prom, fetch_args;
        int d, fast;

        /* Structure recording information on expressions at all levels of 
           the lhs.  Level 'depth' is the ultimate variable; level 0 is the
           whole lhs expression. */

        struct { 
            SEXP expr;        /* Expression at this level */
            SEXP value;       /* Value of expr, may later change */
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
           value of the expression as it is before the assignment.
           Also, ask if it is an unshared subset of the next larger
           expression (and all larger ones).  If it is not known to be
           part of the larger expressions, we do a top-level duplicate
           of it.

           Also, for each level except the final variable and
           outermost level, which only does a store, save argument
           lists for the fetch/store functions that are built with
           shared promises, so that they are evaluated only once.  The
           store argument list has a "value" cell at the end to fill
           in the stored value.

           For efficiency, $ and [[ are handled with VARIANT_FAST_SUB,
           and for $, no promise is created for its argument. */

        s[depth].value = varval;
        s[depth].in_top = 1;

        s[0].store_args = CDDR(lhs);  /* original args, no value cell */

        for (d = depth-1; d > 0; d--) {

            op = CAR(s[d].expr);

            fast = 0;
            if (op == R_DollarSymbol || op == R_Bracket2Symbol) {
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
                fetch_args = promiseArgs (CDDR(s[d].expr), rho);
                if (CDR(fetch_args)==R_NilValue && TAG(fetch_args)==R_NilValue)
                    s[d].store_args = CAR(fetch_args);
                else
                    s[d].store_args = dup_arg_list (fetch_args);
            }

            PROTECT(s[d].store_args);
            PROTECT(fetch_args);

            /* We'll need this value for the subsequent replacement
               operation, so make sure it doesn't change.  Incrementing
               NAMEDCNT would be the obvious way, but if NAMEDCNT 
               was already non-zero, that leads to undesirable duplication
               later (even if the increment is later undone).  Making sure
               that NAMEDCNT isn't zero seems to be sufficient. */

            SET_NAMEDCNT_NOT_0(s[d+1].value);

            if (fast) {
                R_fast_sub_var = s[d+1].value;
                R_variant_result = 0;
                e = CALL_PRIMFUN (call, fn, fetch_args, rho, 
                      VARIANT_FAST_SUB /* implies QUERY_UNSHARED_SUBSET */);
                UNPROTECT(1);  /* fetch_args */
            }
            else {
                prom = mkValuePROMISE(s[d+1].expr,s[d+1].value);
                PROTECT (e = LCONS (op, CONS (prom, fetch_args)));
                R_variant_result = 0;
                e = Rf_evalv_other (e, rho, VARIANT_QUERY_UNSHARED_SUBSET);
                UNPROTECT(2);  /* e, fetch_args */
            }

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

        if (maybe_fast && !isObject(s[1].value) 
              && CAR(s[0].store_args) != R_DotsSymbol
              && (fn = FINDFUN(assgnfcn,rho), 
                  TYPEOF(fn) == SPECIALSXP && PRIMFASTSUB(fn) && !RTRACE(fn))) {
            R_fast_sub_var = s[1].value;
            R_fast_sub_replacement = rhs;
            PROTECT3(R_fast_sub_replacement,R_fast_sub_var,fn);
            newval = CALL_PRIMFUN (call, fn, s[0].store_args, rho, 
                                   VARIANT_FAST_SUB);
            e = R_NilValue;
        }
        else {
            if (POP_IF_TOP_OF_STACK(rhs)) 
                rhs = DUP_STACK_VALUE(rhs);
            PROTECT(rhsprom = mkValuePROMISE(rhs_uneval, rhs));
            PROTECT (lhsprom = mkValuePROMISE(s[1].expr, s[1].value));
            /* original args, no value cell at end, assgnfcn set above*/
            PROTECT(e = replaceCall (assgnfcn, lhsprom, 
                                     s[0].store_args, rhsprom));
            newval = eval(e,rho);
        }

        /* Unprotect e, lhsprom, rhsprom, and s[1].value from the
           previous loop, which went from depth-1 to 1 in the 
           opposite order as this one (plus unprotect one more from
           before that).  Note: e used later, but no alloc before. */

        UNPROTECT(4);

        /* Call the replacement functions at levels 2 to depth, changing the
           values at each level, using the fetched value at that level 
           (was perhaps duplicated), and the new value after replacement at 
           the lower level.  Except we don't do that if it's not necessary
           because the new value is already part of the larger object. */
        
        for (d = 1; d < depth; d++) {

            /* If the replacement function returned a different object, 
               we have to replace, since that new object won't be part 
               of the object at the next level, even if the old one was. */

            if (s[d].in_top == 1 && s[d].value == newval) { 

                /* Don't need to do replacement. */

                newval = s[d+1].value;
                UNPROTECT(1);  /* s[d+1].value protected in previous loop */
            }
            else {

                /* Put value into the next-higher object. */

                PROTECT (rhsprom = mkValuePROMISE (e, newval));
                PROTECT (lhsprom = mkValuePROMISE (s[d+1].expr, s[d+1].value));
                assgnfcn = installAssignFcnName(CAR(s[d].expr));
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

                newval = eval(e,rho);

                /* Unprotect e, lhsprom, rhsprom, and s[d+1].value from the
                   previous loop, which went from depth-1 to 1 in the 
                   opposite order as this one (plus unprotect one more from
                   before that).  Note: e used later, but no alloc before. */

                UNPROTECT(4);
            }
        }

        UNPROTECT(depth-1+2);  /* store_args + two more */
    }

    /* Assign the final result after the top level replacement.  We
       can sometimes avoid the cost of this by looking at the saved
       binding cell, if we have one. */

    if (bcell != R_NilValue && CAR(bcell) == newval) {
        SET_MISSING(bcell,0);
        /* The replacement function might have changed NAMEDCNT to 0. */
        SET_NAMEDCNT_NOT_0(varval);
    }
    else {
        if (opval == 2) /* <<- */
            set_var_nonlocal (var, newval, ENCLOS(rho), 3);
        else
            set_var_in_frame (var, newval, rho, TRUE, 3);
    }

    if (variant & VARIANT_NULL) {
        POP_IF_TOP_OF_STACK(rhs);
        return R_NilValue;
    }
    else {
        DEC_NAMEDCNT(rhs);
        return rhs;
    }
}


/* Evaluate each expression in "el" in the environment "rho".  
   The evaluation is done by calling evalv with the given variant.

   The MISSING gp field in the CONS cell for a missing argument is 
   set to the result of R_isMissing, which will allow identification 
   of missing arguments resulting from '_'.

   Used in eval and applyMethod (object.c) for builtin primitives,
   do_internal (names.c) for builtin .Internals and in evalArgs. */

SEXP attribute_hidden evalList_v (SEXP el, SEXP rho, int variant)
{
    /* Handle 0 or 1 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) { /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue)
            return R_NilValue;
        if (CAR(el) != R_DotsSymbol)
            return cons_with_tag (EVALV (CAR(el), rho, variant), 
                                  R_NilValue, TAG(el));
    }

    /* The general case (except for el == R_NilValue, handed above). */

    int varpend = variant | VARIANT_PENDING_OK;

    BEGIN_PROTECT4 (head, tail, ev, h);

    head = R_NilValue;

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
                    ev = cons_with_tag (EVALV (CAR(h), rho, varpend),
                                        R_NilValue, TAG(h));
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
            ev = cons_with_tag(EVALV(CAR(el),rho,varpend), R_NilValue, TAG(el));
            if (head==R_NilValue)
                head = ev;
            else
                SETCDR_MACRO(tail, ev);
            tail = ev;
            if (CAR(ev) == R_MissingArg && isSymbol(CAR(el)))
                SET_MISSING (ev, R_isMissing(CAR(el),rho));
	}

	el = CDR(el);

    } while (el != R_NilValue);

    if (! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_ARGUMENTS_COMPUTED(head);

    RETURN_SEXP_INSIDE_PROTECT (head);
    END_PROTECT;

} /* evalList_v */


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

    if (!isSymbol(e) || e == R_DotsSymbol || DDVAL(e)) {
        res = EVALV (e, rho, variant);
    }
    else {

        res = findVarPendingOK (e, rho);

        if (res == R_UnboundValue)
            unbound_var_error(e);
        else if (res == R_MissingArg) {
            if ( ! (variant & VARIANT_MISSING_OK))
                if (!DDVAL(e))  /* revert bug fix for the moment */
                    arg_missing_error(e);
        }
        else if (TYPEOF(res) == PROMSXP) {
            if (PRVALUE_PENDING_OK(res) == R_UnboundValue)
                res = forcePromiseUnbound(res,VARIANT_PENDING_OK);
            else
                res = PRVALUE_PENDING_OK(res);
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
    BEGIN_PROTECT4 (head, tail, ev, h);

    int variant = VARIANT_PENDING_OK;

    head = R_NilValue;

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
                    ev = cons_with_tag (eval_unshared (CAR(h), rho, variant),
                                        R_NilValue, TAG(h));
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
            ev = cons_with_tag (eval_unshared (CAR(el), rho, variant), 
                                R_NilValue, TAG(el));
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

    WAIT_UNTIL_ARGUMENTS_COMPUTED (head);

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


/* Create a promise to evaluate each argument.	If the argument is itself
   a promise, it is used unchanged, except that it has its NAMEDCNT
   incremented, and the NAMEDCNT of its value (if not unbound) incremented
   unless it is zero.  See inside for handling of ... */

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho)
{
    /* Handle 0 or 1 arguments (not ...) specially, for speed. */

    if (CDR(el) == R_NilValue) {  /* Note that CDR(R_NilValue) == R_NilValue */
        if (el == R_NilValue)
            return el;
        SEXP a = CAR(el);
        if (a != R_DotsSymbol) {
            MAKE_PROMISE(a,rho);
            return cons_with_tag (a, R_NilValue, TAG(el));
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
                    MAKE_PROMISE(a,rho);
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
            MAKE_PROMISE(a,rho);
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
   (which should never occur). */
 
SEXP attribute_hidden promiseArgsWithValues(SEXP el, SEXP rho, SEXP values)
{
    SEXP s, a, b;
    PROTECT(s = promiseArgs(el, rho));
    if (length(s) != length(values)) error(_("dispatch error"));
    for (a = values, b = s; a != R_NilValue; a = CDR(a), b = CDR(b))
        if (TYPEOF(CAR(b)) == PROMSXP) {
            SET_PRVALUE(CAR(b), CAR(a));
            INC_NAMEDCNT(CAR(a));
        }
    UNPROTECT(1);
    return s;
}

/* Like promiseArgsWithValues except it sets only the first value. */

SEXP attribute_hidden promiseArgsWith1Value(SEXP el, SEXP rho, SEXP value)
{
    SEXP s;
    PROTECT(s = promiseArgs(el, rho));
    if (s == R_NilValue) error(_("dispatch error"));
    if (TYPEOF(CAR(s)) == PROMSXP) {
        SET_PRVALUE(CAR(s), value);
        INC_NAMEDCNT(value);
    }
    UNPROTECT(1);
    return s;
}


/* Check that each formal is a symbol */

/* used in coerce.c */
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
		SET_TAG (xptr, install (translateChar (STRING_ELT(xnames,i))));
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
	UNPROTECT(1);
	PROTECT(expr);
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
	UNPROTECT(1);
	PROTECT(tmp);
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


static SEXP evalArgs(SEXP el, SEXP rho, int dropmissing)
{
    return dropmissing ? evalList(el,rho) : evalListKeepMissing(el,rho);
}


/* A version of DispatchOrEval that checks for possible S4 methods for
 * any argument, not just the first.  Used in the code for `[` in
 * do_subset.  Differs in that all arguments are evaluated
 * immediately, rather than after the call to R_possible_dispatch.
 * NOT ACTUALLY USED AT PRESENT.
 */
attribute_hidden
int DispatchAnyOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		      SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
    if(R_has_methods(op)) {
        SEXP argValue, el,  value; 
	/* Rboolean hasS4 = FALSE; */ 
	int nprotect = 0, dispatch;
	if(!argsevald) {
            PROTECT(argValue = evalArgs(args, rho, dropmissing));
	    nprotect++;
	    argsevald = TRUE;
	}
	else argValue = args;
	for(el = argValue; el != R_NilValue; el = CDR(el)) {
	    if(IS_S4_OBJECT(CAR(el))) {
	        value = R_possible_dispatch(call, op, argValue, rho, TRUE);
	        if (value != R_NoObject) {
		    *ans = value;
		    UNPROTECT(nprotect);
		    return 1;
	        }
		else break;
	    }
	}
	 /* else, use the regular DispatchOrEval, but now with evaluated args */
	dispatch = DispatchOrEval(call, op, generic, argValue, rho, ans, dropmissing, argsevald);
	UNPROTECT(nprotect);
	return dispatch;
    }
    return DispatchOrEval(call, op, generic, args, rho, ans, dropmissing, argsevald);
}


/* DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * arguments it comes in with (if argsevald is 0) and returns them so that
 * the generic built-in C code can continue.  Note that CDR(call) is
 * used to obtain the unevaluated arguments when creating promises, even
 * when argsevald is 1 (so args is the evaluated arguments).  If argsevald 
 * is -1, only the first argument will have been evaluated.
 *
 * The arg list is protected by this function, and needn't be by the caller.
 */
attribute_hidden
int DispatchOrEval(SEXP call, SEXP op, const char *generic, SEXP args,
		   SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    BEGIN_PROTECT1 (x);
    ALSO_PROTECT1 (args);

    int dots = FALSE;

    if (argsevald != 0)
	x = CAR(args);
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
        x = R_NilValue;
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error(_("value in '...' is not a promise"));
#endif
		    dots = TRUE;
		    x = eval(CAR(h), rho);
                    break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    dotdotdot_error();
	    }
	    else {
                dots = FALSE;
                x = eval(CAR(args), rho);
                break;
	    }
	}
    }

    if (isObject(x)) { /* try to dispatch on the object */
	char *pt;
	/* Try for formal method. */
	if(IS_S4_OBJECT(x) && R_has_methods(op)) {

	    BEGIN_INNER_PROTECT2 (value, argValue);

	    /* create a promise to pass down to applyClosure  */
	    if (argsevald < 0)
                argValue = promiseArgsWith1Value(CDR(call), rho, x);
            else if (argsevald == 0)
		argValue = promiseArgsWith1Value(args, rho, x);
	    else 
                argValue = args;
	    /* This means S4 dispatch */
	    value = R_possible_dispatch (call, op, argValue, rho, argsevald<=0);
	    if (value != R_NoObject) {
		*ans = value;
		RETURN_OUTSIDE_PROTECT (1);
	    }
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    argValue = evalArgs(argValue, rho, dropmissing);
		else {
		    argValue = CONS(x, evalArgs(CDR(argValue),rho,dropmissing));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		args = argValue; 
		argsevald = 1;
	    }

            END_INNER_PROTECT;
	}
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = Rf_strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || strcmp(pt,".default")) {

	    BEGIN_INNER_PROTECT2 (pargs, rho1);
	    RCNTXT cntxt;

            if (argsevald > 0) {  /* handle as in R_possible_dispatch */
                pargs = promiseArgsWithValues(CDR(call), rho, args);
            }
            else
                pargs = promiseArgsWith1Value(args, rho, x); 

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
	    rho1 = NewEnvironment(R_NilValue, R_NilValue, rho);
	    begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);
	    if(usemethod(generic, x, call, pargs, rho1, rho, R_BaseEnv, 0, ans))
	    {   endcontext(&cntxt);
		RETURN_OUTSIDE_PROTECT (1);
	    }
	    endcontext(&cntxt);

            END_INNER_PROTECT;
	}
    }

    if (argsevald <= 0) {
	if (dots)
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    args = evalArgs(args, rho, dropmissing);
	else {
	    args = cons_with_tag (x, evalArgs(CDR(args), rho, dropmissing),
                                  TAG(args));
	}
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
		  SEXP *ans)
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
	    for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
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
	    SET_STRING_ELT(m, i, R_BlankString);
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

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */

    PROTECT(s = promiseArgsWithValues(CDR(call), rho, args));
    if (isOps) {
        /* ensure positional matching for operators */
        for (m = s; m != R_NilValue; m = CDR(m))
            SET_TAG(m, R_NilValue);
    }

    *ans = applyClosure_v (t, lsxp, s, rho, supplied, 0);

    UNPROTECT(9);
    return 1;
}

static SEXP do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP symbol, i;

    checkArity(op, args);
    symbol = CAR(args);

    if (!isSymbol(symbol))
	errorcall(call, _("invalid symbol"));

    if ((i = INTERNAL(symbol)) != R_NilValue && TYPEOF(i) == BUILTINSXP)
	return R_ScalarLogicalTRUE;
    else
	return R_ScalarLogicalFALSE;
}

static SEXP do_loadfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file, s;
    FILE *fp;

    checkArity(op, args);

    PROTECT(file = coerceVector(CAR(args), STRSXP));

    if (! isValidStringF(file))
	errorcall(call, _("bad file name"));

    fp = RC_fopen(STRING_ELT(file, 0), "rb", TRUE);
    if (!fp)
	errorcall(call, _("unable to open 'file'"));
    s = R_LoadFromFile(fp, 0);
    fclose(fp);

    UNPROTECT(1);
    return s;
}

static SEXP do_savefile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    FILE *fp;

    checkArity(op, args);

    if (!isValidStringF(CADR(args)))
	errorcall(call, _("'file' must be non-empty string"));
    if (TYPEOF(CADDR(args)) != LGLSXP)
	errorcall(call, _("'ascii' must be logical"));

    fp = RC_fopen(STRING_ELT(CADR(args), 0), "wb", TRUE);
    if (!fp)
	errorcall(call, _("unable to open 'file'"));

    R_SaveToFileV(CAR(args), fp, INTEGER(CADDR(args))[0], 0);

    fclose(fp);
    return R_NilValue;
}



/* --------------------------------------------------------------------------

   Inline function used by operators that can take operands on the
   scalar stack and can handle unclassed objects (VARIANT_UNCLASS_FLAG).

   Evaluates two arguments that may be put on the scalar stack.  These
   two arguments are returned in arg1 and arg2, and whether they are
   objects (accounting for VARIANT_UNCLASS_FLAG) in obj1 and obj2.  
   If one of the first two arguments is an object, a list of the evaluated
   arguments is returned as the value of the function, so that dispatch
   must be attempted (note that the argument count in this case may
   not be two).  If neither is an object, a list with the correct
   number of arguments is returned, but they may (or may not) be the
   unevaluated arguments.

   If an argument is an object, all arguments will have been computed
   before return from this function, but evaluation of arguments may 
   be pending if neither operand is an object.

   Note that if there are less than two arguments, the missing ones will
   appear here to be R_NilValue (since CAR(R_NilValue) is R_NilValue).

   The args and env arguments must be protected by the caller. */

static SEXP scalar_stack_eval2 (SEXP args, SEXP *arg1, SEXP *arg2,
                                int *obj1, int *obj2, SEXP env)
{
    SEXP argsevald;
    SEXP x, y;
    int o1, o2;

    o1 = o2 = 0;

    x = CAR(args); 
    y = CADR(args);

    /* We evaluate by the general procedure if ... present or more than
       two arguments, not trying to put arguments on the scalar stack. */

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        argsevald = evalList (args, env);
        x = CAR(argsevald);
        y = CADR(argsevald);
        o1 = isObject(x);
        o2 = isObject(y);
        goto rtrn;
    }

    /* Otherwise, we try to put the first argument on the scalar stack,
       and evaluate with VARIANT_UNCLASS. */

    PROTECT(x = EVALV (x, env, 
             VARIANT_SCALAR_STACK_OK | VARIANT_UNCLASS | VARIANT_PENDING_OK));

    if (isObject(x)) {
        if (R_variant_result & VARIANT_UNCLASS_FLAG)
            R_variant_result = 0;
        else
            o1 = 1;
    }

    /* If first argument is an object, we evaluate the rest of the arguments
       normally. */

    if (o1) {
        argsevald = evalList (CDR(args), env);
        y = CAR(argsevald);
        argsevald = cons_with_tag (x, argsevald, TAG(args));
        UNPROTECT(1); /* x */
        WAIT_UNTIL_COMPUTED(x);
        goto rtrn;
    }

    /* If there's no second argument, we can return now. */

    if (y == R_NilValue) {
        UNPROTECT(1);
        argsevald = args;
        goto rtrn;
    }

    /* Now we evaluate the second argument, also allowing it to be on the
       scalar stack, again with VARIANT_UNCLASS. */

    y = EVALV (y, env, 
                VARIANT_UNCLASS | VARIANT_SCALAR_STACK_OK | VARIANT_PENDING_OK);

    if (isObject(y)) {
        if (R_variant_result & VARIANT_UNCLASS_FLAG)
            R_variant_result = 0;
        else
            o2 = 1;
    }

    /* If the second argument is an object, we have to duplicate the first
       arg if it is on the scalar stack, or an unclassed object, and create 
       the list of evaluated arguments. */

    if (o2) {

        if (ON_SCALAR_STACK(x) || isObject(x)) /* can't be both */ {
            UNPROTECT(1); /* x */
            PROTECT(y);
            if (ON_SCALAR_STACK(x)) {
                POP_SCALAR_STACK(x);
                PROTECT(x = duplicate(x));
            }
            else { /* isObject(x) */
                PROTECT(x = Rf_makeUnclassed(x));
                o1 = 0;
            }
        }
        else
            PROTECT(y);

        argsevald = evalList (CDDR(args), env);
        argsevald = cons_with_tag (y, argsevald, TAG(CDR(args)));
        argsevald = cons_with_tag (x, argsevald, TAG(args));
        UNPROTECT(2); /* x & y */
        WAIT_UNTIL_COMPUTED_2(x,y);
        goto rtrn;
    }

    /* If neither of the first two arguments are an object, we
       don't look at any possible remaining arguments.  The caller
       is responsible for reporting an error if any are present,
       but we assist by returning the unevaluated arguments, which
       in this case (no ...) number the same as the actual arguments. */

    UNPROTECT(1); /* x */
    argsevald = args;

  rtrn:
    *arg1 = x;
    *arg2 = y;
    *obj1 = o1;
    *obj2 = o2;

    return argsevald;
}


/* -------------------------------------------------------------------------- */
/*                         LOGICAL OPERATORS                                  */

static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

void task_and_or (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int * restrict lans = LOGICAL(ans);

    int i, i1, i2, n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = LENGTH(ans);

    switch (code) {
    case 1:  /* & : AND */
        if (n1 == n2) {
            for (i = 0; i<n; i++) {
                uint32_t u1 = LOGICAL(s1)[i];
                uint32_t u2 = LOGICAL(s2)[i];
                lans[i] = (u1 & u2) | (u1 & (u2<<31)) | (u2 & (u1<<31));
            }
        }
        else {
            mod_iterate(n1,n2,i1,i2) {
                uint32_t u1 = LOGICAL(s1)[i1];
                uint32_t u2 = LOGICAL(s2)[i2];
                lans[i] = (u1 & u2) | (u1 & (u2<<31)) | (u2 & (u1<<31));
            }
        }
        break;
    case 2:  /* | : OR */
        if (n1 == n2) {
            for (i = 0; i<n; i++) {
                uint32_t u = LOGICAL(s1)[i] | LOGICAL(s2)[i];
                lans[i] = u & ~ (u << 31);
            }
        }
        else {
            mod_iterate(n1,n2,i1,i2) {
                uint32_t u = LOGICAL(s1)[i1] | LOGICAL(s2)[i2];
                lans[i] = u & ~ (u << 31);
            }
        }
        break;
    }
}


/* & | */

#define T_and_or THRESHOLD_ADJUST(25)

SEXP attribute_hidden do_andor(SEXP call, SEXP op, SEXP args, SEXP env, 
                               int variant)
{
    SEXP ans, x, y;
    int args_evald;

    /* Evaluate arguments, setting x to first argument and y to
       second argument.  The whole argument list is in args, already 
       evaluated if args_evald is 1. */

    x = CAR(args); 
    y = CADR(args);

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        args = evalList (args, env);
        PROTECT(x = CAR(args)); 
        PROTECT(y = CADR(args));
        args_evald = 1;
    }
    else {
        PROTECT(x = EVALV (x, env, VARIANT_PENDING_OK));
        PROTECT(y = EVALV (y, env, VARIANT_PENDING_OK));
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
        if (DispatchGroup("Ops", call, op, args, env, &ans)) {
            UNPROTECT(3);
            return ans;
        }
        UNPROTECT(1);
    }

    /* Check argument count now (after dispatch, since other methods may allow
       other argument count). */

    checkArity(op,args);

    /* Arguments are now in x and y, and are protected.  The value 
       in args may not be protected, and is not used below. */

    SEXP dims, tsp, klass, xnames, ynames;
    int xarray, yarray, xts, yts;

    if (! (isRaw(x) && isRaw(y)) && ! (isNumber(x) && isNumber(y)))
        errorcall (call,
       _("operations are possible only for numeric, logical or complex types"));

    tsp = R_NilValue;		/* -Wall */
    klass = R_NilValue;		/* -Wall */
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		error(_("binary operation on non-conformable arrays"));
	    PROTECT(dims = getDimAttrib(x));
	}
	else if (xarray) {
	    PROTECT(dims = getDimAttrib(x));
	}
	else /*(yarray)*/ {
	    PROTECT(dims = getDimAttrib(y));
	}
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

    R_len_t nx = LENGTH(x);
    R_len_t ny = LENGTH(y);

    if (nx > 0 && ny > 0 && (nx > ny ? nx % ny : ny % nx))
        warningcall(call,
         _("longer object length is not a multiple of shorter object length"));

    if (isRaw(x) && isRaw(y)) {
        WAIT_UNTIL_COMPUTED_2(x,y);
        PROTECT(ans = binaryLogic2(PRIMVAL(op), x, y));
    }
    else {

        if (nx == 0 || ny == 0) {
            ans = allocVector (LGLSXP, 0);
            UNPROTECT(5);
            return ans;
        }

        R_len_t n = (nx > ny) ? nx : ny;
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

/* Handles the ! operator. */

#define T_not THRESHOLD_ADJUST(40)

static SEXP do_fast_not(SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    SEXP x, dim, dimnames, names;
    int len;

    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (length(arg)==0) 
            return allocVector(LGLSXP, 0);
	else
            errorcall(call, _("invalid argument type"));
    }
    len = LENGTH(arg);

    /* Quickly do scalar operation on logical with no attributes. */

    if (len==1 && isLogical(arg) && !HAS_ATTRIB(arg)) {
        int v = LOGICAL(arg)[0];
        return ScalarLogicalMaybeConst (v==NA_LOGICAL ? v : !v);
    }

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

/* ! */

SEXP attribute_hidden do_not(SEXP call, SEXP op, SEXP args, SEXP env, 
                             int variant)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    checkArity (op, args);

    return do_fast_not (call, op, CAR(args), env, variant);
}

/* Does && (op 1) and || (op 2). */

SEXP attribute_hidden do_andor2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int ov = PRIMVAL(op);

    SEXP s1, s2;
    int x1, x2;

    if (args==R_NilValue || CDR(args)==R_NilValue || CDDR(args)!=R_NilValue)
	error(_("'%s' operator requires 2 arguments"), ov == 1 ? "&&" : "||");

    s1 = EVALV (CAR(args), env, 0);
    if (!isNumber(s1))
	errorcall(call, _("invalid 'x' type in 'x %s y'"), ov==1 ? "&&" : "||");

    x1 = TYPEOF(s1) == LGLSXP && LENGTH(s1) == 1 ? *LOGICAL(s1) : asLogical(s1);

    if (ov==1 && x1==FALSE)  /* FALSE && ... */
        return ScalarLogicalMaybeConst(FALSE);

    if (ov==2 && x1==TRUE)   /* TRUE || ... */
        return ScalarLogicalMaybeConst(TRUE);
    
    s2  = EVALV (CADR(args), env, 0);
    if (!isNumber(s2))	
        errorcall(call, _("invalid 'y' type in 'x %s y'"), ov==1 ? "&&" : "||");

    x2 = TYPEOF(s2) == LGLSXP && LENGTH(s2) == 1 ? *LOGICAL(s2) : asLogical(s2);

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

    switch (code) {
    case 1:  /* & : AND */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] & RAW(s2)[i];
        }
        else {
            mod_iterate(n1,n2,i1,i2)
                RAW(ans)[i] = RAW(s1)[i1] & RAW(s2)[i2];
        }
	break;
    case 2:  /* | : OR */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] | RAW(s2)[i];
        }
        else {
            mod_iterate(n1,n2,i1,i2)
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
    if (na_rm) {

        if (op == OP_ANY) {
            unsigned res = 0;
            for (int i = 0; i<n; i++) {
                res |= x[i];
                if (res & 1)
                    return TRUE;
            }
            return FALSE;
        }
        else { /* OP_ALL */
            unsigned res = 1;
            for (int i = 0; i<n; i++) {
                res &= x[i] | (x[i]>>31);
                if (! (res & 1))
                    return FALSE;
            }
            return TRUE;
        }

    }
    else { /* !na_rm */

        if (op == OP_ANY) {
            unsigned res = 0;
            for (int i = 0; i<n; i++) {
                res |= x[i];
                if (res & 1)
                    return TRUE;
            }
            return res>>31 ? NA_LOGICAL : FALSE;
        }
        else { /* OP_ALL */
            unsigned res = 1;
            unsigned na = 0;
            for (int i = 0; i<n; i++) {
                res &= x[i] | (x[i]>>31);
                if (! (res & 1))
                    return FALSE;
                na |= x[i];
            }
            return na>>31 ? NA_LOGICAL : TRUE;
        }

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

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
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
/*                        ARITHMETIC OPERATORS.                               */
/*                                                                            */
/* All but simple cases are handled in R_unary and R_binary in arithmetic.c.  */

static SEXP do_arith (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int opcode = PRIMVAL(op), obj1, obj2;
    SEXP argsevald, ans, arg1, arg2;

    /* Evaluate arguments, maybe putting them on the scalar stack. */

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT (argsevald = 
               scalar_stack_eval2(args, &arg1, &arg2, &obj1, &obj2, env));
    PROTECT2(arg1,arg2);

    /* Check for dispatch on S3 or S4 objects. */

    if (obj1 || obj2) {
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans)) {
            UNPROTECT(3);
            return ans;
        }
    }

    /* Check for argument count error (not before dispatch, since other
       methods may have different requirements). */

    if (argsevald==R_NilValue || CDDR(argsevald)!=R_NilValue)
	errorcall(call,_("operator needs one or two arguments"));

    if (CDR(argsevald)==R_NilValue && opcode!=MINUSOP && opcode!=PLUSOP)
        errorcall(call, _("%d argument passed to '%s' which requires %d"),
                        1, PRIMNAME(op), 2);

    /* Arguments are now in arg1 and arg2, and are protected. They may
       be on the scalar stack, but if so, are removed now, though they
       may still be referenced.  Note that result might be on top of
       one of them - OK since after storing into it, the args won't be
       accessed again.

       Below same as POP_IF_TOP_OF_STACK(arg2); POP_IF_TOP_OF_STACK(arg1);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    /* We quickly do real arithmetic and integer plus/minus/times on scalars 
       with no attributes (as will be the case for scalar stack values).  We
       don't bother trying local assignment, since returning the result on the
       scalar stack should be about as fast. */

    int type1 = TYPEOF(arg1);

    if ((type1==REALSXP || type1==INTSXP) && LENGTH(arg1) == 1
                                          && NO_ATTRIBUTES_OK (variant, arg1)) {

        if (CDR(argsevald)==R_NilValue) { /* Unary operation */
            WAIT_UNTIL_COMPUTED(arg1);
            if (type1==REALSXP) {
                double val = opcode == PLUSOP ? *REAL(arg1) : -*REAL(arg1);
                ans = NAMEDCNT_EQ_0(arg1) ? (*REAL(arg1) = val, arg1)
                    : CAN_USE_SCALAR_STACK(variant) ? PUSH_SCALAR_REAL(val)
                    :   ScalarReal(val);
            }
            else { /* INTSXP */
                int val  = *INTEGER(arg1)==NA_INTEGER ? NA_INTEGER
                         : opcode == PLUSOP ? *INTEGER(arg1) : -*INTEGER(arg1);
                ans = NAMEDCNT_EQ_0(arg1) ? (*INTEGER(arg1) = val, arg1)
                    : CAN_USE_SCALAR_STACK(variant) ? PUSH_SCALAR_INTEGER(val)
                    :   ScalarInteger(val);
            }
            goto ret;
        }

        int type2 = TYPEOF(arg2);

        if ((type2 == REALSXP || type2 == INTSXP) && LENGTH(arg2) == 1 
                                       && NO_ATTRIBUTES_OK (variant, arg2)) {

            if (type1 == INTSXP && type2 == INTSXP) {

                if (opcode==PLUSOP || opcode==MINUSOP || opcode==TIMESOP) {

                    WAIT_UNTIL_COMPUTED_2(arg1,arg2);
    
                    int a1 = *INTEGER(arg1), a2 = *INTEGER(arg2);
                    int_fast64_t val;
    
                    if (a1==NA_INTEGER || a2==NA_INTEGER)
                        val = NA_INTEGER;
                    else {
                        val = 
                         opcode==PLUSOP  ? (int_fast64_t)a1 + (int_fast64_t)a2 :
                         opcode==MINUSOP ? (int_fast64_t)a1 - (int_fast64_t)a2 :
                                           (int_fast64_t)a1 * (int_fast64_t)a2;
    
                          if (val < R_INT_MIN || val > R_INT_MAX) {
                              val = NA_INTEGER;
                              warningcall (call, 
                                         _("NAs produced by integer overflow"));
                          }
                    }
    
                    int ival = (int) val;

                    ans = NAMEDCNT_EQ_0(arg2) ?
                            (*INTEGER(arg2) = ival, arg2)
                        : NAMEDCNT_EQ_0(arg1) ?
                            (*INTEGER(arg1) = ival, arg1)
                        : CAN_USE_SCALAR_STACK(variant) ? 
                            PUSH_SCALAR_INTEGER(ival)
                        :   ScalarInteger(ival);
  
                    goto ret;
                }
                else {
                    /* fall through to general code below */
                }
            }

            else { /* not both INTSXP, so at least one is REALSXP */

                double a1, a2, val;
    
                WAIT_UNTIL_COMPUTED_2(arg1,arg2);

                if (type1 == INTSXP) {
                    a1 = (double) *INTEGER(arg1);
                    a2 = *REAL(arg2);
                }
                else if (type2 == INTSXP) {
                    a1 = *REAL(arg1);
                    a2 = (double) *INTEGER(arg2);
                }
                else {
                    a1 = *REAL(arg1);
                    a2 = *REAL(arg2);
                }
            
                switch (opcode) {
                case PLUSOP:
                    val = a1 + a2;
                    break;
                case MINUSOP:
                    val = a1 - a2;
                    break;
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

                ans = NAMEDCNT_EQ_0(arg2) && type2 == REALSXP ?
                        (*REAL(arg2) = val, arg2)
                    : NAMEDCNT_EQ_0(arg1) && type1 == REALSXP ?
                        (*REAL(arg1) = val, arg1)
                    : CAN_USE_SCALAR_STACK(variant) ? 
                        PUSH_SCALAR_REAL(val)
                    :   ScalarReal(val);

                goto ret;
            }
        }
    }

    /* Otherwise, handle the general case. */

    ans = CDR(argsevald)==R_NilValue 
           ? R_unary (call, op, arg1, obj1, env, variant) 
           : R_binary (call, op, arg1, arg2, obj1, obj2, env, variant);

  ret:
    UNPROTECT(3);
    return ans;
}

/* -------------------------------------------------------------------------- */
/*                       RELATIONAL OPERATORS.                                */
/*                                                                            */
/* Main work is done in R_relop, in relop.c.                                  */

static SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP argsevald, ans, x, y;
    int objx, objy;

    /* Evaluate arguments, maybe putting them on the scalar stack. */

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT(argsevald = 
              scalar_stack_eval2 (args, &x, &y, &objx, &objy, env));
    PROTECT2(x,y);

    /* Check for dispatch on S3 or S4 objects. */

    if (objx || objy) {
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans)) {
            UNPROTECT(3);
            return ans;
        }
    }

    /* Check argument count now (after dispatch, since other methods may allow
       other argument count). */

    checkArity(op,argsevald);

    /* Arguments are now in x and y, and are protected.  They may be on
       the scalar stack, but if so are popped off here (but retain their
       values if eval is not called). */

    /* Below does same as POP_IF_TOP_OF_STACK(y); POP_IF_TOP_OF_STACK(x);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    ans = R_relop (call, op, x, y, objx, objy, env, variant);

    UNPROTECT(3);
    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_eval[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"if",		do_if,		0,	1200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"for",		do_for,		0,	100,	-1,	{PP_FOR,     PREC_FN,	  0}},
{"while",	do_while,	0,	100,	-1,	{PP_WHILE,   PREC_FN,	  0}},
{"repeat",	do_repeat,	0,	100,	-1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break, CTXT_BREAK,	0,	-1,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break, CTXT_NEXT,	0,	-1,	{PP_NEXT,    PREC_FN,	  0}},
{"(",		do_paren,	0,	1000,	1,	{PP_PAREN,   PREC_FN,	  0}},
{"{",		do_begin,	0,	1200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"return",	do_return,	0,	1200,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		1,	1100,	2,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		3,	1100,	2,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		2,	1100,	2,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"->",		do_set,		11,	1100,	2,	{PP_ASSIGN,  PREC_RIGHT,	  1}},
{"->>",		do_set,		12,	1100,	2,	{PP_ASSIGN2, PREC_RIGHT,	  1}},
{"eval",	do_eval,	0,	1211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"eval.with.vis",do_eval,	1,	1211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

{"Rprof",	do_Rprof,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Logical Operators, all primitives */
/* these are group generic and so need to eval args (as builtin or themselves)*/

{"&",		do_andor,	1,	1000,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_andor,	2,	1000,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_not,		1,	1001,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_andor2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_andor2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},

/* these are group generic and so need to eval args */
{"all",		do_allany,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_allany,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* Arithmetic Operators, all primitives, now special, though always eval args */

{"+",		do_arith,	PLUSOP,	1000,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"-",		do_arith,	MINUSOP,1000,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"*",		do_arith,	TIMESOP,1000,	2,	{PP_BINARY,  PREC_PROD,	  0}},
{"/",		do_arith,	DIVOP,	1000,	2,	{PP_BINARY2, PREC_PROD,	  0}},
{"^",		do_arith,	POWOP,	1000,	2,	{PP_BINARY2, PREC_POWER,  1}},
{"%%",		do_arith,	MODOP,	1000,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%/%",		do_arith,	IDIVOP,	1000,	2,	{PP_BINARY2, PREC_PERCENT,0}},

/* Relational Operators, all primitives */
/* these are group generic and so need to eval args (inside, as special) */

{"==",		do_relop,	EQOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop,	NEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop,	LTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop,	LEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop,	GEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop,	GTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},

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
