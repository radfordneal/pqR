/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015, 2016, 2017, 2018 by Radford M. Neal
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
#   define SCALAR_STACK_ENTRY(n) ((SEXP) &R_scalar_stack_space[n])
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

/* Macros to push and pop. */

#if USE_COMPRESSED_POINTERS
#   define POP_IF_TOP_OF_STACK(x) \
     ((x) != SCALAR_STACK_OFFSET(1) ? 0 : \
      ((R_scalar_stack -= 1), 1))
#   define POP_SCALAR_STACK(x) \
     (/* SCALAR_STACK_OFFSET(1) != (x) ? (void) abort() : */ \
      (void) (R_scalar_stack -= 1))
#   define PUSH_SCALAR_INTEGER(v) \
     (SET_TYPEOF0(R_scalar_stack,INTSXP), \
      (*INTEGER(R_scalar_stack) = (v)), \
      (R_scalar_stack += 1), \
      SCALAR_STACK_OFFSET(1))
#   define PUSH_SCALAR_REAL(v) \
     (SET_TYPEOF0(R_scalar_stack,REALSXP), \
      (*REAL(R_scalar_stack) = (v)), \
      (R_scalar_stack += 1), \
      SCALAR_STACK_OFFSET(1))
#else
#   define POP_IF_TOP_OF_STACK(x) \
     ((x) != SCALAR_STACK_OFFSET(1) ? 0 : \
      (/* REprintf("POP TOP %llx %s %d\n", */ \
       /*           (long long)(x),__FILE__,__LINE__), */ \
       (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)-1)), 1))
#   define POP_SCALAR_STACK(x) \
     (/* REprintf("POP %llx %s %d\n", */ \
      /*           (long long)(x),__FILE__,__LINE__), */ \
      SCALAR_STACK_OFFSET(1) != (x) ? (void) abort() : \
      (void) (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)-1)))
#   define PUSH_SCALAR_INTEGER(v) \
     (/* REprintf("PUSH INTEGER %llx %d %s %d\n", */ \
      /*           (long long)R_scalar_stack,v,__FILE__,__LINE__), */ \
      SET_TYPEOF0(R_scalar_stack,INTSXP), \
      (*INTEGER(R_scalar_stack) = (v)), \
      (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)+1)), \
      SCALAR_STACK_OFFSET(1))
#   define PUSH_SCALAR_REAL(v) \
     (/* REprintf("PUSH REAL %llx %f %s %d\n", */ \
      /*           (long long)R_scalar_stack,v,__FILE__,__LINE__), */ \
      SET_TYPEOF0(R_scalar_stack,REALSXP), \
      (*REAL(R_scalar_stack) = (v)), \
      (R_scalar_stack = (SEXP)(((VECTOR_SEXPREC_C*)R_scalar_stack)+1)), \
      SCALAR_STACK_OFFSET(1))
#endif

#define PUSH_SCALAR(v) \
  (TYPEOF(v) == INTSXP ? PUSH_SCALAR_INTEGER(*INTEGER(v)) \
                       : PUSH_SCALAR_REAL(*REAL(v)))


/* Macro to duplicate top of stack as a regular object. */

#if USE_COMPRESSED_POINTERS
#    define DUP_STACK_VALUE(x) \
      (TYPEOF(x)==INTSXP \
       ? ScalarInteger (R_scalar_stack_space[(x)-R_scalar_stack_start].data.i) \
       : ScalarReal (R_scalar_stack_space[(x)-R_scalar_stack_start].data.d))
#else
#    define DUP_STACK_VALUE(x) \
      (TYPEOF(x) == INTSXP ? ScalarInteger(*INTEGER(x)) : ScalarReal(*REAL(x)))
#endif


/* Inline function to handle positive scalar real and integer
   subscripts specially, putting them on the scalar stack, and
   otherwise call internalArraySubscript. */

static inline SEXP array_sub (SEXP sb, SEXP dim, int i, SEXP x, int *hasna)
{
    if ( (((1<<INTSXP) + (1<<REALSXP)) >> TYPEOF(sb)) & 1 ) {
        if (LENGTH(sb) == 1) {
            R_len_t dm, ix;
            dm = INTEGER(dim)[i];
            if (TYPEOF(sb) == REALSXP) {
                if (ISNAN(*REAL(sb)) || *REAL(sb) < 1 || *REAL(sb) > dm)
                    goto fallback;
                ix = (R_len_t) *REAL(sb);
            }
            else {
                ix = *INTEGER(sb);
                if (ix < 1 || ix > dm)
                    goto fallback;
            }
            *hasna = 0;
            return SCALAR_STACK_HAS_SPACE() ? PUSH_SCALAR_INTEGER(ix)
                                            : ScalarInteger(ix);
        }
    }

  fallback:
    return internalArraySubscript (i, sb, dim, x, hasna);
}
