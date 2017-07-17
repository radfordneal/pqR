/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Core Team.
 *  Copyright (C) 2003-4	    The R Foundation
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


/* These macros and inline functions manage the stack of scalar values
   that can be used to quickly return a numeric scalar value (INTSXP
   or REALSXP) without the overhead of heap allocation, if the caller
   is prepared to handle this (in particular, remove it from the
   stack), as signaled by calling evalv with VARIANT_SCALAR_STACK_OK.

   These values may be referenced as SEXPs, but must never be put in a
   data structure (eg, a list) that will survive as a long-term
   reference.  It is allowed to call eval while keeping values on the
   stack, since further uses will be stacked.  The stack is restored
   back when an error exit occurs.  Overflow of the stack is handled
   by stopping using it (as if VARIANT_SCALAR_STACK_OK was not set). */

/* Macros to get the n'th entry on the scalar stack (0 = bottom), or
   the n'th from the top (1 = top).  For compressed pointers, uses the
   fact that the stack consists of consecutive compressed pointers (in
   consecutive segments, if more than one used), which relies on an
   entry occupying only one chunk.  For uncompressed pointers, uses
   the locations allocated in const-objs.c. */

#if USE_COMPRESSED_POINTERS
#   define SCALAR_STACK_ENTRY(n) (R_scalar_stack_start+(n))
#   define SCALAR_STACK_OFFSET(n) (R_scalar_stack-(n))
#else
#   define SCALAR_STACK_ENTRY(n) ((SEXP) &R_scalar_stack_start[n])
#   define SCALAR_STACK_OFFSET(n) \
     ((SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)-(n)))
#endif

/* Test if object is on the scalar stack (set up in const-objs.c). */

#define ON_SCALAR_STACK(x) \
  (SGGC_SEGMENT_INDEX(CPTR_FROM_SEXP(x)) == R_SGGC_SCALAR_STACK_INDEX)

/* See if a value can be put on the scalar stack, based on variant, and on
   whether there is space. */

#define CAN_USE_SCALAR_STACK(v) \
  (((v) & VARIANT_SCALAR_STACK_OK) && SCALAR_STACK_HAS_SPACE())

#define SCALAR_STACK_HAS_SPACE() \
  (R_scalar_stack <= SCALAR_STACK_ENTRY(SCALAR_STACK_SIZE-1))

/* Macros to push, pop, and slide. */

#if USE_COMPRESSED_POINTERS
#   define POP_IF_TOP_OF_STACK(x) \
     ((x) != SCALAR_STACK_OFFSET(1) ? 0 : \
      ((R_scalar_stack -= 1), 1))
#   define POP_SCALAR_STACK(x) \
     (/* SCALAR_STACK_OFFSET(1) != (x) ? (void) abort() : */ \
      (void) (R_scalar_stack -= 1))
#   define PUSH_SCALAR_INTEGER(v) \
     ((TYPEOF(R_scalar_stack) = INTSXP), \
      (*INTEGER(R_scalar_stack) = (v)), \
      (R_scalar_stack += 1), \
      SCALAR_STACK_OFFSET(1))
#   define PUSH_SCALAR_REAL(v) \
     ((TYPEOF(R_scalar_stack) = REALSXP), \
      (*REAL(R_scalar_stack) = (v)), \
      (R_scalar_stack += 1), \
      SCALAR_STACK_OFFSET(1))
#else
#   define POP_IF_TOP_OF_STACK(x) \
     ((x) != SCALAR_STACK_OFFSET(1) ? 0 : \
      (/* REprintf("POP TOP %llx %s %d\n", \
                 (long long)(x),__FILE__,__LINE__), */ \
       (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)-1)), 1))
#   define POP_SCALAR_STACK(x) \
     (/* REprintf("POP %llx %s %d\n", \
                   (long long)(x),__FILE__,__LINE__), */ \
      SCALAR_STACK_OFFSET(1) != (x) ? (void) abort() : \
      (void) (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)-1)))
#   define PUSH_SCALAR_INTEGER(v) \
     (/* REprintf("PUSH INTEGER %llx %d %s %d\n", \
                (long long)R_scalar_stack,v,__FILE__,__LINE__), */ \
      (TYPEOF(R_scalar_stack) = INTSXP), \
      (*INTEGER(R_scalar_stack) = (v)), \
      (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)+1)), \
      SCALAR_STACK_OFFSET(1))
#   define PUSH_SCALAR_REAL(v) \
     (/* REprintf("PUSH REAL %llx %f %s %d\n", \
                (long long)R_scalar_stack,v,__FILE__,__LINE__), */ \
      (TYPEOF(R_scalar_stack) = REALSXP), \
      (*REAL(R_scalar_stack) = (v)), \
      (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)+1)), \
      SCALAR_STACK_OFFSET(1))
#endif

#define DUP_STACK_VALUE(x) \
  (TYPEOF(x) == INTSXP ? ScalarInteger(*INTEGER(x)) : ScalarReal(*REAL(x)))


/* Inline function used by operators that can take operands on the
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

static inline SEXP scalar_stack_eval2 (SEXP args, SEXP *arg1, SEXP *arg2,
                     int *obj1, int *obj2, SEXP env, SEXP call, int variant)
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
