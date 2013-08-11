/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Development Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif
#define USE_FAST_PROTECT_MACROS

#include <complex.h>
#include "Defn.h"		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("Non-numeric argument to mathematical function")

#include <Rmath.h>
extern double Rf_gamma_cody(double);

#include "arithmetic.h"

#include <errno.h>

#include <helpers/helpers-app.h>


extern double (*R_math1_func_table[44])(double);
extern char R_math1_err_table[44];
extern int R_naflag;

extern helpers_task_proc task_unary_minus, task_math1;


/* Codes for binary (and unary minus with 0 op) arithmetic operations.  Note
   that PLUS, MINUS, and TIMES are not assumed to be commutative, since it's
   not clear that they are when one or both operands are NaN or NA.  We want
   exactly the same result as is obtained without merging operations.  A scalar
   to a vector power is not handled, just to reduce the number of operations.  
   Squaring is handled specially for speed. */

#define ARITH_OP_C_PLUS_V  0
#define ARITH_OP_V_PLUS_C  1
#define ARITH_OP_C_MINUS_V 2
#define ARITH_OP_V_MINUS_C 3
#define ARITH_OP_C_TIMES_V 4
#define ARITH_OP_V_TIMES_C 5
#define ARITH_OP_C_DIV_V   6
#define ARITH_OP_V_DIV_C   7
#define ARITH_OP_V_POW_C   8
#define ARITH_OP_V_SQUARED 9

#define N_ARITH_OPS 10


/* Task for performing a set of merged arithmetic/math1 operations. */

/* Slow version for testing. */

void task_merged_arith_math1 (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int flags = code & 0x7f;
    int which = code & 0x80;
    helpers_op_t ops = code >> 8;

    SEXP vec, scalars;

    if (which) {
        vec = s2;
        scalars = s1;
    }
    else {
        vec = s1;
        scalars = s2;
    }

    static double zero = 0.0;
    double *scp = scalars==0 ? &zero : REAL(scalars);
    R_len_t n = LENGTH(vec);
    R_len_t i = 0;
    R_len_t a;

    HELPERS_SETUP_OUT(5);

    while (i < n) {
        HELPERS_WAIT_IN1 (a, i, n);
        do {
            double v = REAL(vec)[i];
            int ai = 0;
            int j = 0;

            for (int f = flags; f != 1; f >>= 1) j += 1;

            while (j > 0) {

                j -= 1;

                int op = (ops >> (j<<3)) & 0xff;

                if ((flags>>j) & 1) { /* arithmetic operation */
                    double c = scp[ai];
                    switch (op) {
                    case ARITH_OP_C_PLUS_V:   v = c + v;       break;
                    case ARITH_OP_V_PLUS_C:   v = v + c;       break;
                    case ARITH_OP_C_MINUS_V:  v = c - v;       break;
                    case ARITH_OP_V_MINUS_C:  v = v - c;       break;
                    case ARITH_OP_C_TIMES_V:  v = c * v;       break;
                    case ARITH_OP_V_TIMES_C:  v = v * c;       break;
                    case ARITH_OP_C_DIV_V:    v = c / v;       break;
                    case ARITH_OP_V_DIV_C:    v = v / c;       break;
                    case ARITH_OP_V_POW_C:    v = R_pow(v,c);  break;
                    case ARITH_OP_V_SQUARED:  v = v * v;       break;
                    }
                    ai += 1;
                }
                else { /* math1 operation */
                    if (!ISNAN(v)) {
                        v = R_math1_func_table[op](v);
                        if (R_math1_err_table[op] != 0) {
                            if (ISNAN(v)) {
                                R_naflag = 1; /* only done in master thread */
                            }
                        }
                    }
                }
            }

            REAL(ans)[i] = v;
            HELPERS_NEXT_OUT(i);
        } while (i < a);
    }
}


/* Find what arithmetic operation is to be merged. */

static int merged_arith_op (helpers_task_proc *proc, helpers_op_t op,
                            helpers_var_ptr in1, helpers_var_ptr in2)
{
    if (proc == task_unary_minus) {
        return ARITH_OP_C_MINUS_V;
    }
    else if (LENGTH(in1) == 1) {
        switch (op) {
        case PLUSOP:  return ARITH_OP_C_PLUS_V; 
        case MINUSOP: return ARITH_OP_C_MINUS_V;
        case TIMESOP: return ARITH_OP_C_TIMES_V;
        case DIVOP:   return ARITH_OP_C_DIV_V;  
        }
    }
    else {
        switch (op) {
        case PLUSOP:  return ARITH_OP_V_PLUS_C; 
        case MINUSOP: return ARITH_OP_V_MINUS_C;
        case TIMESOP: return ARITH_OP_V_TIMES_C;
        case DIVOP:   return ARITH_OP_V_DIV_C;  
        case POWOP:   return *REAL(in2)==2.0 ? ARITH_OP_V_SQUARED 
                                             : ARITH_OP_V_POW_C;
        }
    }
}


/* Procedure for merging arithmetic/math1 operations. */

void helpers_merge_proc ( /* helpers_var_ptr out, */
  helpers_task_proc *proc_A, helpers_op_t op_A, 
  helpers_var_ptr in1_A, helpers_var_ptr in2_A,
  helpers_task_proc **proc_B, helpers_op_t *op_B, 
  helpers_var_ptr *in1_B, helpers_var_ptr *in2_B)
{
    int flags, which;
    helpers_op_t ops;
    SEXP sv;
  
    /* Set flags, which, ops, and sv according to operations other than 
       operation A. */
  
    if (*proc_B == task_merged_arith_math1) {
        flags = *op_B & 0x7f;
        which = *op_B & 0x80;
        ops = *op_B >> 8;
        sv = which ? *in1_B : *in2_B; 
    }
    else { /* create "merge" of just operation B */
        if (*proc_B == task_math1) {
            flags = 0x02;
            which = 0x00;
            ops = *op_B;
            sv = 0;
        }
        else { /* binary or unary arithmetic operation */
            ops = merged_arith_op (*proc_B, *op_B, *in1_B, *in2_B);
            flags = 0x03;
            if (LENGTH(*in2_B) == 1) {
                which = 0x00;
                sv = *in2_B;
            }
            else {
                which = 0x80;
                sv = in1_A;
            }
        }
        *proc_B = task_merged_arith_math1;
    }
  
    /* Merge operation A into other operations. */
  
    if (proc_A == task_math1) {
        ops = (ops << 8) | op_A;
        flags = (flags << 1);
    }
    else { /* binary or unary arithmetic operation */
        SEXP scalar = LENGTH(in1_A) == 1 ? in1_A : in2_A;
        double *p;
        if (sv == 0) {
            sv = scalar;
            *in2_B = sv;
        }
        else if (LENGTH(sv) == 1) {
            sv = allocVector (REALSXP, MAX_OPS_MERGED);
            * (which ? in1_B : in2_B) = sv;
            REAL(sv)[1] = *REAL(scalar);
        }
        else {
            p = REAL(sv);
            for (int f = flags; f != 1; f >>= 1) p += f&1;
            *p = *REAL(scalar);
        }
        ops = (ops << 8) | merged_arith_op (proc_A, op_A, in1_A, in2_A);
        flags = (flags << 1) | 1;
    }
  
    /* Store the new operation specification in *op_B (*proc_B and *in1_B or
       *in2_B may have been updated above). */
  
    *op_B = (ops<<8) | which | flags;
}
