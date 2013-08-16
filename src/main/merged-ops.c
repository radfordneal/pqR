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


/* Codes for merged arithmetic operations and math1 functions.  Note that
   PLUS, MINUS, and TIMES are not assumed to be commutative, since they
   aren't always when one or both operands are NaN or NA, and we want exactly 
   the same result as is obtained without merging operations.  

   Relies on PLUSOP ... POWOP being the integers from 1 to 5.  Codes from
   0x80 to 0xff are math1 codes plus 0x80.  Code 0 is for empty slots.
   Additional operation code are created within the fast routine. */

#define MERGED_OP_NULL      0   /* No operation, either created or empty slot */

#define MERGED_OP_C_PLUS_V  (2*PLUSOP - 1)
#define MERGED_OP_V_PLUS_C  (2*PLUSOP)
#define MERGED_OP_C_MINUS_V (2*MINUSOP - 1)
#define MERGED_OP_V_MINUS_C (2*MINUSOP)
#define MERGED_OP_C_TIMES_V (2*TIMESOP - 1)
#define MERGED_OP_V_TIMES_C (2*TIMESOP)
#define MERGED_OP_C_DIV_V   (2*DIVOP - 1)
#define MERGED_OP_V_DIV_C   (2*DIVOP)
#define MERGED_OP_C_POW_V   (2*POWOP - 1)
#define MERGED_OP_V_POW_C   (2*POWOP)

#define MERGED_OP_V_SQUARED 11  /* Created for V_POW_C when power is 2 */
#define MERGED_OP_CONSTANT 12   /* Created for V_POW_C when power is 0 */
#define MERGED_OP_MATH1 13      /* Created for any math1 op */

#define N_MERGED_OPS 14         /* Number of operation codes above */

/* Task for performing a set of merged arithmetic/math1 operations. */

#if USE_SLOW_MERGED_OP

/* Slow version for testing.  Doesn't treat powers of -1, 0, 1, and 2 
   specially.  Inefficiently does switch inside loop. */

void task_merged_arith_math1 (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int which = code & 1;

    int nops = 0;
    for (helpers_op_t o = code>>8; o != 0; o >>= 8) nops += 1;

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
            helpers_op_t ops = code; 
            int ai = 0;
            int op;

            for (int shft = 8*nops; shft != 0; shft -= 8) {

                op = (ops >> shft) & 0xff;

                if (op & 0x80) { /* math1 operation */
                    op &= ~0x80;
                    if (!ISNAN(v)) {
                        v = R_math1_func_table[op](v);
                        if (R_math1_err_table[op] != 0) {
                            if (ISNAN(v)) {
                                R_naflag = 1; /* only done in master thread */
                            }
                        }
                    }
                }
                else { /* arithmetic operation */
                    double c = scp[ai++];
                    switch (op) {
                    case MERGED_OP_C_PLUS_V:   v = c + v;       break;
                    case MERGED_OP_V_PLUS_C:   v = v + c;       break;
                    case MERGED_OP_C_MINUS_V:  v = c - v;       break;
                    case MERGED_OP_V_MINUS_C:  v = v - c;       break;
                    case MERGED_OP_C_TIMES_V:  v = c * v;       break;
                    case MERGED_OP_V_TIMES_C:  v = v * c;       break;
                    case MERGED_OP_C_DIV_V:    v = c / v;       break;
                    case MERGED_OP_V_DIV_C:    v = v / c;       break;
                    case MERGED_OP_C_POW_V:    v = R_pow(c,v);  break;
                    case MERGED_OP_V_POW_C:    v = R_pow(v,c);  break;
                    }
                }
            }

            REAL(ans)[i] = v;
            HELPERS_NEXT_OUT(i);
        } while (i < a);
    }
}

#else 

/* Fast version. Treats powers of -1, 0, 1, and 2 specially, replacing
   the MERGED_OP_V_POW_C code with another.  Uses a big switch over all
   combinations of operations.  

   Works only when MAX_OPS_MERGED is 3. */

#if MAX_OPS_MERGED != 3
#error Fast merged operations are implemented only when MAX_OPS_MERGED is 3
#endif

void task_merged_arith_math1 (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    /* Get vector to operate on and pointer to scalar operands (if any). */

    double *ansp, *vecp, *scp;

    ansp = REAL(ans);

    if (code & 1) {
        vecp = REAL(s2);
        scp = REAL(s1);
    }
    else {
        vecp = REAL(s1);
        if (s2 != NULL) scp = REAL(s2);
    }

    /* Set up switch value encoding all operations and the functions and
       scalar constants used by all operations. */

    int ops = code >> 8;

    double (*f1)(double), (*f2)(double), (*f3)(double);
    double c1, c2, c3;
    int e3;

    int switch_value;
    int op;

#   define POW_SPECIAL(op,c) do { \
        if (op == MERGED_OP_V_POW_C) { \
            if (c == -1.0)     { op = MERGED_OP_C_DIV_V; c = 1.0; } \
            else if (c == 0.0) { op = MERGED_OP_CONSTANT; c = 1.0; } \
            else if (c == 1.0) { op = MERGED_OP_NULL; } \
            else if (c == 2.0) { op = MERGED_OP_V_SQUARED; } \
        } \
    } while (0)

    if ((ops & 0xff0000) == 0) {
        switch_value = MERGED_OP_NULL * (N_MERGED_OPS*N_MERGED_OPS);
    }
    else {
        op = ops >> 16;
        ops &= 0xffff;
        if (op & 0x80) {
            op &= 0x7f;
            switch_value = MERGED_OP_MATH1 * (N_MERGED_OPS*N_MERGED_OPS);
            f1 = R_math1_func_table[op];
        }
        else {
            c1 = *scp++;
            POW_SPECIAL(op,c1);
            switch_value = op * (N_MERGED_OPS*N_MERGED_OPS);
        }
    }

    op = ops >> 8;
    if (op & 0x80) {
        op &= 0x7f;
        switch_value += MERGED_OP_MATH1 * N_MERGED_OPS;
        f2 = R_math1_func_table[op];
    }
    else {
        c2 = *scp++;
        POW_SPECIAL(op,c2);
        switch_value += op * N_MERGED_OPS;
    }

    op = ops & 0xff;
    if (op & 0x80) {
        op &= 0x7f;
        switch_value += MERGED_OP_MATH1;
        f3 = R_math1_func_table[op];
        e3 = R_math1_err_table[op];
    }
    else {
        c3 = *scp;
        POW_SPECIAL(op,c3);
        switch_value += op;
    }

    /* Do the operations. */

    R_len_t n = LENGTH(ans);
    R_len_t i = 0;
    R_len_t a;
    double v;

    HELPERS_SETUP_OUT(6);

#   define SWITCH_CASE(o1,S1,o2,S2,o3,S3) \
        case o1*N_MERGED_OPS*N_MERGED_OPS + o2*N_MERGED_OPS + o3: \
            do { \
                R_len_t u = HELPERS_UP_TO(i,a); \
                do { v = vecp[i]; S1; S2; S3; ansp[i] = v; } while (++i <= u); \
                helpers_amount_out(i); \
            } while (i < a); \
            break;

#   define SWITCH_CASES2(o1,S1,o2,S2) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_NULL, ;) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_C_PLUS_V,  v = c3 + v) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_PLUS_C,  v = v + c3) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_C_MINUS_V, v = c3 - v) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_MINUS_C, v = v - c3) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_C_TIMES_V, v = c3 * v) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_TIMES_C, v = v * c3) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_C_DIV_V,   v = c3 / v) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_DIV_C,   v = v / c3) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_C_POW_V,   v = R_pow(c3,v)) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_POW_C,   v = R_pow(v,c3)) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_V_SQUARED, v = v * v) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_CONSTANT,  v = c3) \
        SWITCH_CASE(o1,S1,o2,S2, MERGED_OP_MATH1, \
            if (!ISNAN(v)) v = f3(v); if (e3 && ISNAN(v)) R_naflag = 1 )

#   define SWITCH_CASES1(o1,S1) \
        SWITCH_CASES2(o1,S1, MERGED_OP_NULL, ;) \
        SWITCH_CASES2(o1,S1, MERGED_OP_C_PLUS_V,  v = c2 + v) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_PLUS_C,  v = v + c2) \
        SWITCH_CASES2(o1,S1, MERGED_OP_C_MINUS_V, v = c2 - v) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_MINUS_C, v = v - c2) \
        SWITCH_CASES2(o1,S1, MERGED_OP_C_TIMES_V, v = c2 * v) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_TIMES_C, v = v * c2) \
        SWITCH_CASES2(o1,S1, MERGED_OP_C_DIV_V,   v = c2 / v) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_DIV_C,   v = v / c2) \
        SWITCH_CASES2(o1,S1, MERGED_OP_C_POW_V,   v = R_pow(c2,v)) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_POW_C,   v = R_pow(v,c2)) \
        SWITCH_CASES2(o1,S1, MERGED_OP_V_SQUARED, v = v * v) \
        SWITCH_CASES2(o1,S1, MERGED_OP_CONSTANT,  v = c2) \
        SWITCH_CASES2(o1,S1, MERGED_OP_MATH1,     if (!ISNAN(v)) v = f2(v) )

    while (i < n) {
        HELPERS_WAIT_IN1 (a, i, n);
        switch (switch_value) {
            SWITCH_CASES1(MERGED_OP_NULL, ;)
            SWITCH_CASES1(MERGED_OP_C_PLUS_V,  v = c1 + v)
            SWITCH_CASES1(MERGED_OP_V_PLUS_C,  v = v + c1)
            SWITCH_CASES1(MERGED_OP_C_MINUS_V, v = c1 - v)
            SWITCH_CASES1(MERGED_OP_V_MINUS_C, v = v - c1)
            SWITCH_CASES1(MERGED_OP_C_TIMES_V, v = c1 * v)
            SWITCH_CASES1(MERGED_OP_V_TIMES_C, v = v * c1)
            SWITCH_CASES1(MERGED_OP_C_DIV_V,   v = c1 / v)
            SWITCH_CASES1(MERGED_OP_V_DIV_C,   v = v / c1)
            SWITCH_CASES1(MERGED_OP_C_POW_V,   v = R_pow(c1,v))
            SWITCH_CASES1(MERGED_OP_V_POW_C,   v = R_pow(v,c1))
            SWITCH_CASES1(MERGED_OP_V_SQUARED, v = v * v)
            SWITCH_CASES1(MERGED_OP_CONSTANT,  v = c1)
            SWITCH_CASES1(MERGED_OP_MATH1,     if (!ISNAN(v)) v = f1(v) )
        }
    }
}

#endif


/* Procedure for merging arithmetic/math1 operations. */

#define MERGED_ARITH_OP(proc,op,in1,in2) \
 ((proc)==task_unary_minus ? MERGED_OP_C_MINUS_V \
          : LENGTH(in1)==1 ? 2*(op) - 1 : 2*(op))

void helpers_merge_proc ( /* helpers_var_ptr out, */
  helpers_task_proc *proc_A, helpers_op_t op_A, 
  helpers_var_ptr in1_A, helpers_var_ptr in2_A,
  helpers_task_proc **proc_B, helpers_op_t *op_B, 
  helpers_var_ptr *in1_B, helpers_var_ptr *in2_B)
{
    helpers_op_t ops;
    int which;
    SEXP sv;
  
    /* Set flags, which, ops, and sv according to operations other than 
       operation A. */
  
    if (*proc_B == task_merged_arith_math1) {
        which = *op_B & 1;
        ops = *op_B >> 8;
        sv = which ? *in1_B : *in2_B; 
    }
    else { /* create "merge" of just operation B */
        if (*proc_B == task_math1) {
            which = 0;
            ops = *op_B + 0x80;
            sv = 0;
        }
        else { /* binary or unary arithmetic operation */
            ops = MERGED_ARITH_OP (*proc_B, *op_B, *in1_B, *in2_B);
            if (*in2_B == 0) { /* unary minus */
                which = 0;
                sv = *in2_B = R_ScalarRealZero;
            }
            else if (LENGTH(*in2_B) == 1) {
                which = 0;
                sv = *in2_B;
            }
            else {
                which = 1;
                sv = *in1_B;
            }
        }
        *proc_B = task_merged_arith_math1;
    }
  
    /* Merge operation A into other operations. */

    helpers_op_t newop;

    if (proc_A == task_math1) {
        newop = op_A | 0x80;
    }
    else { /* binary or unary arithmetic operation */
        SEXP scalar = in2_A == 0 ? R_ScalarRealZero /* for unary minus */
                    : LENGTH(in2_A) == 1 ? in2_A : in1_A;
        double *p;
        if (sv == 0) { /* will also have which == 0 */
            sv = scalar;
            *in2_B = sv;
            helpers_mark_in_use(sv);
        }
        else if (LENGTH(sv) == 1) {
            double tmp = *REAL(sv);
            sv = allocVector (REALSXP, MAX_OPS_MERGED);
            * (which ? in1_B : in2_B) = sv;
            REAL(sv)[0] = tmp;
            REAL(sv)[1] = *REAL(scalar);
        }
        else {
            p = REAL(sv);
#           if MAX_OPS_MERGED==3
                if ((ops&0x80) == 0) p += 1;
                if ((ops>>8) != 0 && (ops&0x8000) == 0) p += 1;
#           else
                for (helpers_op_t o = ops; o != 0; o >>= 8) {
                    if (! (o&0x80)) p += 1;
                }
#           endif
            *p = *REAL(scalar);
        }
        newop = MERGED_ARITH_OP (proc_A, op_A, in1_A, in2_A);
    }

    ops = (ops << 8) | newop;
  
    /* Store the new operation specification in *op_B (*proc_B and *in1_B or
       *in2_B may have been updated above). */
  
    *op_B = (ops << 8) | which;
}
