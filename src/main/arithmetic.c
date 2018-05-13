/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
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

#include "scalar-stack.h"

/*** NOTE: do_arith itself is in eval.c, calling R_unary and R_binary here. ***/

static inline void maybe_dup_attributes (SEXP to, SEXP from, int variant)
{
    if (to == from) {
        /* nothing to do */
    }
    else if (isObject(from)) {
        DUPLICATE_ATTRIB (to, from);
    }
    else if (variant & VARIANT_ANY_ATTR) {
        /* needn't do anything */
    }
    else if (variant & VARIANT_ANY_ATTR_EX_DIM) {
        SEXP dim = getAttrib (from, R_DimSymbol);
        if (dim != R_NilValue) 
            setAttrib (to, R_DimSymbol, dim);
    }
    else {
        DUPLICATE_ATTRIB(to,from);
    }
}

static R_NORETURN void non_numeric_errorcall (SEXP call)
{
    errorcall (call, _("Non-numeric argument to mathematical function"));
}

static void NaN_warning (void)
{
    warning (_("NaNs produced"));
}

static void NaN_warningcall (SEXP call)
{
    warningcall (call, _("NaNs produced"));
}

/* Macro to do attribute duplication only if they're not the same already.
   Using parens in (DUPLICATE_ATTRIB) gets us the function defined in memory.c, 
   not this macro. */

#define DUPLICATE_ATTRIB(_to_,_from_) do { \
    if (ATTRIB((_to_))!=ATTRIB((_from_)) || OBJECT((_from_))) \
        (DUPLICATE_ATTRIB)((_to_),(_from_)); \
} while (0)

#include <Rmath.h>
extern double Rf_gamma_cody(double);

#include "arithmetic.h"

#include <errno.h>

#include <helpers/helpers-app.h>

#ifdef HAVE_MATHERR

/* Override the SVID matherr function:
   the main difference here is not to print warnings.
 */
#ifndef __cplusplus
int matherr(struct exception *exc)
{
    switch (exc->type) {
    case DOMAIN:
    case SING:
	errno = EDOM;
	break;
    case OVERFLOW:
	errno = ERANGE;
	break;
    case UNDERFLOW:
	exc->retval = 0.0;
	break;
	/*
	   There are cases TLOSS and PLOSS which are ignored here.
	   According to the Solaris man page, there are for
	   trigonometric algorithms and not needed for good ones.
	 */
    }
    return 1;
}
#endif
#endif

/* Hack to avoid possibly-incorrect constant folding. */
volatile double R_Zero_Hack = 0.0;


static double R_ValueOfNA(void)
{
    return REAL(R_ScalarRealNA)[0];
}

int R_IsNA(double x)
{
    return ISNA(x);
}

int R_IsNaN(double x)
{
    return ISNAN_NOT_NA(x);
}

int R_finite(double x)
{
    return R_FINITE(x);
}

/* Used to define ISNAN for C++ in Arith.h */

int R_isnancpp(double x)
{
   return ISNAN(x);
}


/* Arithmetic Initialization */

void attribute_hidden InitArithmetic()
{
#ifdef Win32
    /* This resets the precision, rounding and exception modes of a ix86 fpu. */
    __asm__ ( "fninit" );
#endif

    R_NaInt = INT_MIN; /* now mostly unused: NA_INTEGER defined as INT_MIN */
    R_NaN = 0.0/R_Zero_Hack;
    R_NaReal = R_ValueOfNA();
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
    R_NaN_cast_to_int = (int) R_NaN;
}

/* some systems get this wrong, possibly depend on what libs are loaded */
static R_INLINE double R_log(double x) {
    return x > 0 ? log(x) : x < 0 ? R_NaN : R_NegInf;
}

/* Macro handling powers 1 and 2 quickly, and otherwise using R_pow.  
   First argument should be double, second may be double or int. */

#define R_POW(x,y) ((y) == 2 ? (x)*(x) : (y) == 1 ? (x) : R_pow((x),(y)))

double R_pow(double x, double y) /* = x ^ y */
{
    /* Don't optimize for power of 2, since we assume most calls are
       via R_POW, which already does that, or from some other place making
       a similar check. */

    if(x == 1. || y == 0.)
	return(1.);
    if(x == 0.) {
	if(y > 0.) return(0.);
	else if(y < 0) return(R_PosInf);
	else return(y); /* NA or NaN, we assert */
    }

    if (R_FINITE(x) && R_FINITE(y)) {
        if (y == 1.)
            return x;
        else if (y == 0.5)
            return sqrt(x);
        else
            return pow(x, y);
    }

    if (MAY_BE_NAN2(x,y) && (ISNAN(x) || ISNAN(y)))
	return(x + y);
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return (y < 0.)? 0. : R_PosInf;
	else {			/* (-Inf) ^ y */
	    if (R_FINITE(y) && y == floor(y)) { /* (-Inf) ^ n */
                if (y < 0.)
                    return 0.;
                /* Return x (= -Inf) if power is odd, -x if power is even. */
                /* Note that all the really, really big integers are even. */
                if (y < (((int_fast64_t)1)<<62) && (((int_fast64_t)y) & 1))
                    return x;
                else
                    return -x;
            }
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return (x >= 1) ? R_PosInf : 0.;
	    else		/* y == -Inf */
		return (x < 1) ? R_PosInf : 0.;
	}
    }
    return(R_NaN);		/* all other cases: (-Inf)^{+-Inf,
				   non-int}; (neg)^{+-Inf} */
}

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;

    if (n != 0) {
	if (!R_FINITE(x)) return R_POW(x, n);

	Rboolean is_neg = (n < 0);
	if(is_neg) n = -n;
	for(;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
        if(is_neg) xn = 1. / xn;
    }
    return xn;
}


/* General Base Logarithms */

/* Note that the behaviour of log(0) required is not necessarily that
   mandated by C99 (-HUGE_VAL), and the behaviour of log(x < 0) is
   optional in C99.  Some systems return -Inf for log(x < 0), e.g.
   libsunmath on Solaris.
*/
static double logbase(double x, double base)
{
#ifdef HAVE_LOG10
    if(base == 10) return x > 0 ? log10(x) : x < 0 ? R_NaN : R_NegInf;
#endif
#ifdef HAVE_LOG2
    if(base == 2) return x > 0 ? log2(x) : x < 0 ? R_NaN : R_NegInf;
#endif
    return R_log(x) / R_log(base);
}


#define add_func(a,b) ((a)+(b))
#define sub_func(a,b) ((a)-(b))
#define mul_func(a,b) ((a)*(b))
#define div_func(a,b) ((a)/(b))

#define add_func_mm(a,b) _mm256_add_pd((a),(b))
#define sub_func_mm(a,b) _mm256_sub_pd((a),(b))
#define mul_func_mm(a,b) _mm256_mul_pd((a),(b))
#define div_func_mm(a,b) _mm256_div_pd((a),(b))

/* Macro for pipelined arithmetic computation.  Arguments are as follows:

       type     type of the result
       func     function or macro for the arithmetic operation
       result   address of array where results are stored
       n        length of the result
       fetch1   macro to fetch an element of the first operand
       s1       address of first operand
       n1       length of the first operand
       fetch2   macro to fetch an element of the second operand
       s2       address of second operand
       n2       length of the second operand

   Both n1 and n2 must be non-zero and no bigger than n, and at least one 
   of n1 and n2 must be equal to n.  An operand of length one or with length 
   less than n is assumed to already be available with no waiting.
*/

#define PIPEARITH(type,func,result,n,fetch1,s1,n1,fetch2,s2,n2) \
    do { \
        R_len_t i, i1, i2, a, a1, a2; \
        i = 0; \
        if (n2 == 1) { \
            type tmp = fetch2(s2,0); \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    while (i <= u-3) { \
                        type op10 = fetch1(s1,i); \
                        type op11 = fetch1(s1,i+1); \
                        type op12 = fetch1(s1,i+2); \
                        type op13 = fetch1(s1,i+3); \
                        result[i] = func(op10,tmp); \
                        result[i+1] = func(op11,tmp); \
                        result[i+2] = func(op12,tmp); \
                        result[i+3] = func(op13,tmp); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(fetch1(s1,i),tmp); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else if (n1 == 1) { \
            type tmp = fetch1(s1,0); \
            while (i<n) { \
                HELPERS_WAIT_IN2 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    while (i <= u-3) { \
                        type op20 = fetch2(s2,i); \
                        type op21 = fetch2(s2,i+1); \
                        type op22 = fetch2(s2,i+2); \
                        type op23 = fetch2(s2,i+3); \
                        result[i] = func(tmp,op20); \
                        result[i+1] = func(tmp,op21); \
                        result[i+2] = func(tmp,op22); \
                        result[i+3] = func(tmp,op23); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(tmp,fetch2(s2,i)); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else if (n1 == n2) { \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a1, i, n); \
                HELPERS_WAIT_IN2 (a2, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO2(i,a1,a2); \
                    while (i <= u-3) { \
                        type op10 = fetch1(s1,i), op20 = fetch2(s2,i); \
                        type op11 = fetch1(s1,i+1), op21 = fetch2(s2,i+1); \
                        type op12 = fetch1(s1,i+2), op22 = fetch2(s2,i+2); \
                        type op13 = fetch1(s1,i+3), op23 = fetch2(s2,i+3); \
                        result[i] = func(op10,op20); \
                        result[i+1] = func(op11,op21); \
                        result[i+2] = func(op12,op22); \
                        result[i+3] = func(op13,op23); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(fetch1(s1,i),fetch2(s2,i)); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a1 && i<a2); \
            } \
        } \
        else if (n1 > n2) { \
            i2 = 0; \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    do { \
                        result[i] = func(fetch1(s1,i),fetch2(s2,i2)); \
                        if (++i2 == n2) i2 = 0; \
                        i += 1; \
                    } while (i<=u); \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else { /* n1 < n2 */ \
            i1 = 0; \
            while (i<n) { \
                HELPERS_WAIT_IN2 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    do { \
                        result[i] = func(fetch1(s1,i1),fetch2(s2,i)); \
                        if (++i1 == n1) i1 = 0; \
                        i += 1; \
                    } while (i<=u); \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
    } while (0)

/* A version of PIPEARITH using AVX intrinsics (for x86 platforms),
   which may be used.  The function name passed must have a version
   with _mm appended that does the AVX256 operation.  Alignment and
   offset is assumed to be the same for both operands and the result
   (except that scalars can have any alignment).

   Note that here we can take advantage of knowledge that operations
   are done element-by-element, with any possible aliasing of s1, s2,
   and result being irrelevant, as long as only final values are
   stored in result. */

#if !__AVX__ || defined(DISABLE_AVX_CODE)

#define MM_PIPEARITH(func,result,n,s1,n1,s2,n2) \
          PIPEARITH(double,func,result,n,RFETCH,s1,n1,RFETCH,s2,n2)

#else

#include <immintrin.h>

#define MM_PIPEARITH(func,result,n,s1,n1,s2,n2) \
    do { \
        R_len_t i, i1, i2, a, a1, a2; \
        i = 0; \
        if (n2 == 1) { \
            double tmp = REAL(s2)[0]; \
            __m256d tmp_pd = _mm256_set_pd(tmp,tmp,tmp,tmp); \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    /* do ops individually until result+i is 32-byte aligned */\
                    while (((uintptr_t)(result+i) & 0x1f) != 0 && i <= u) { \
                        result[i] = func(REAL(s1)[i],tmp); \
                        i += 1; \
                    } \
                    while (i <= u-3) { \
                        __m256d res_pd; \
                        res_pd = _mm256_load_pd (REAL(s1)+i); \
                        res_pd = func ## _mm (res_pd, tmp_pd); \
                        _mm256_store_pd (result+i, res_pd); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(REAL(s1)[i],tmp); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else if (n1 == 1) { \
            double tmp = REAL(s1)[0]; \
            __m256d tmp_pd = _mm256_set_pd(tmp,tmp,tmp,tmp); \
            while (i<n) { \
                HELPERS_WAIT_IN2 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    /* do ops individually until result+i is 32-byte aligned */\
                    while (((uintptr_t)(result+i) & 0x1f) != 0 && i <= u) { \
                        result[i] = func(tmp,REAL(s2)[i]); \
                        i += 1; \
                    } \
                    while (i <= u-3) { \
                        __m256d res_pd; \
                        res_pd = _mm256_load_pd (REAL(s2)+i); \
                        res_pd = func ## _mm (tmp_pd, res_pd); \
                        _mm256_store_pd (result+i, res_pd); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(tmp,REAL(s2)[i]); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else if (n1 == n2) { \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a1, i, n); \
                HELPERS_WAIT_IN2 (a2, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO2(i,a1,a2); \
                    /* do ops individually until result+i is 32-byte aligned */\
                    while (((uintptr_t)(result+i) & 0x1f) != 0 && i <= u) { \
                        result[i] = func(REAL(s1)[i],REAL(s2)[i]); \
                        i += 1; \
                    } \
                    while (i <= u-3) { \
                        __m256d res_pd, op2_pd; \
                        res_pd = _mm256_load_pd (REAL(s1)+i); \
                        op2_pd = _mm256_load_pd (REAL(s2)+i); \
                        res_pd = func ## _mm (res_pd, op2_pd); \
                        _mm256_store_pd (result+i, res_pd); \
                        i += 4; \
                    } \
                    while (i <= u) { \
                        result[i] = func(REAL(s1)[i],REAL(s2)[i]); \
                        i += 1; \
                    } \
                    helpers_amount_out(i); \
                } while (i<a1 && i<a2); \
            } \
        } \
        else if (n1 > n2) { \
            i2 = 0; \
            while (i<n) { \
                HELPERS_WAIT_IN1 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    do { \
                        result[i] = func(REAL(s1)[i],REAL(s2)[i2]); \
                        if (++i2 == n2) i2 = 0; \
                        i += 1; \
                    } while (i<=u); \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
        else { /* n1 < n2 */ \
            i1 = 0; \
            while (i<n) { \
                HELPERS_WAIT_IN2 (a, i, n); \
                do { \
                    R_len_t u = HELPERS_UP_TO(i,a); \
                    do { \
                        result[i] = func(REAL(s1)[i1],REAL(s2)[i]); \
                        if (++i1 == n1) i1 = 0; \
                        i += 1; \
                    } while (i<=u); \
                    helpers_amount_out(i); \
                } while (i<a); \
            } \
        } \
    } while (0)

#endif

#define RFETCH(_s_,_i_) (REAL(_s_)[_i_])
#define RIFETCH(_s_,_i_) \
   ((double) (INTEGER(_s_)[_i_] == NA_INTEGER ? NA_REAL : INTEGER(_s_)[_i_]))

static int integer_overflow;  /* Set by task_integer_arithmetic on overflow
                                 (only in a master-now task or a direct call) */

void task_integer_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int *ians = INTEGER(ans);

    R_len_t i1, i2, n, n1, n2;
    int x1, x2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    switch (code) {
    case PLUSOP:
        if (n1 > n2) { SEXP t = s1; s1 = s2; s2 = t; n1 = n2; n2 = n; }
        mod_iterate_1 (n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                ians[i] = NA_INTEGER;
            else {
                int_fast64_t val = (int_fast64_t) x1 + (int_fast64_t) x2;
                if (val >= R_INT_MIN && val <= R_INT_MAX)
                    ians[i] = val;
                else {
                    integer_overflow = TRUE;
                    ians[i] = NA_INTEGER;
                }
            }
        }
        break;
    case MINUSOP:
        if (n1 <= n2) {
            mod_iterate_1 (n1, n2, i1, i2) {
                x1 = INTEGER(s1)[i1];
                x2 = INTEGER(s2)[i2];
                if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                    ians[i] = NA_INTEGER;
                else {
                    int_fast64_t val = (int_fast64_t) x1 - (int_fast64_t) x2;
                    if (val >= R_INT_MIN && val <= R_INT_MAX)
                        ians[i] = val;
                    else {
                        integer_overflow = TRUE;
                        ians[i] = NA_INTEGER;
                    }
                }
            }
        }
        else {
            mod_iterate_2 (n1, n2, i1, i2) {
                x1 = INTEGER(s1)[i1];
                x2 = INTEGER(s2)[i2];
                if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                    ians[i] = NA_INTEGER;
                else {
                    int_fast64_t val = (int_fast64_t) x1 - (int_fast64_t) x2;
                    if (val >= R_INT_MIN && val <= R_INT_MAX)
                        ians[i] = val;
                    else {
                        integer_overflow = TRUE;
                        ians[i] = NA_INTEGER;
                    }
                }
            }
        }
        break;
    case TIMESOP:
        if (n1 > n2) { SEXP t = s1; s1 = s2; s2 = t; n1 = n2; n2 = n; }
        mod_iterate_1 (n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                ians[i] = NA_INTEGER;
            else {
                int_fast64_t val = (int_fast64_t) x1 * (int_fast64_t) x2;
                if (val >= R_INT_MIN && val <= R_INT_MAX)
                    ians[i] = val;
                else {
                    integer_overflow = TRUE;
                    ians[i] = NA_INTEGER;
                }
            }
        }
        break;
    case DIVOP:
        mod_iterate (n, n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                    REAL(ans)[i] = NA_REAL;
                else
                    REAL(ans)[i] = (double) x1 / (double) x2;
        }
        break;
    case POWOP:
        mod_iterate (n, n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == 1 || x2 == 0)
                REAL(ans)[i] = 1.0;
            else if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                REAL(ans)[i] = NA_REAL;
            else {
                REAL(ans)[i] = R_POW((double) x1, x2);
            }
        }
        break;
    case MODOP:
        mod_iterate (n, n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
                ians[i] = NA_INTEGER;
            else {
                ians[i] = /* till 0.63.2: x1 % x2 */
                    (int)myfmod((double)x1,(double)x2);
            }
        }
        break;
    case IDIVOP:
        mod_iterate (n, n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            /* This had x %/% 0 == 0 prior to 2.14.1, but
               it seems conventionally to be undefined */
            if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
                ians[i] = NA_INTEGER;
            else
                ians[i] = floor((double)x1 / (double)x2);
        }
        break;
    }
}

void task_real_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    double *rans = REAL(ans);
    R_len_t n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    HELPERS_SETUP_OUT (7);

    switch (code) {
    case PLUSOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,add_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,add_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            MM_PIPEARITH(add_func,rans,n,s1,n1,s2,n2);
        break;
    case MINUSOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,sub_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,sub_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            MM_PIPEARITH(sub_func,rans,n,s1,n1,s2,n2);
        break;
    case TIMESOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,mul_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,mul_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            MM_PIPEARITH(mul_func,rans,n,s1,n1,s2,n2);
        break;
    case DIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,div_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,div_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            MM_PIPEARITH(div_func,rans,n,s1,n1,s2,n2);
        break;
    case POWOP:
        if (TYPEOF(s1) == REALSXP && n2 == 1) {
            double tmp = TYPEOF(s2) == REALSXP ? RFETCH(s2,0) : RIFETCH(s2,0);
            R_len_t i, a;
            i = 0;
            if (tmp == 2.0)
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
#                       if __AVX__ && !defined(DISABLE_AVX_CODE)
                        /* do individual ops until rans+i is 32-byte aligned */
                        while (((uintptr_t)(rans+i) & 0x1f) != 0 && i <= u) {
                            double op = RFETCH(s1,i);
                            rans[i] = op * op;
                            i += 1;
                        }
                        while (i <= u-3) {
                            __m256d res_pd;
                            res_pd = _mm256_load_pd (&RFETCH(s1,i));
                            res_pd = _mm256_mul_pd (res_pd, res_pd);
                            _mm256_store_pd (rans+i, res_pd);
                            i += 4;
                        }
#                       else
                        while (i <= u-3) {
                            double op0 = RFETCH(s1,i);
                            double op1 = RFETCH(s1,i+1);
                            double op2 = RFETCH(s1,i+2);
                            double op3 = RFETCH(s1,i+3);
                            rans[i] = op0 * op0;
                            rans[i+1] = op1 * op1;
                            rans[i+2] = op2 * op2;
                            rans[i+3] = op3 * op3;
                            i += 4;
                        }
#                       endif
                        while (i <= u) {
                            double op = RFETCH(s1,i);
                            rans[i] = op * op;
                            i += 1;
                        }
                        helpers_amount_out(i);
                    } while (i<a);
                }
            else if (tmp == 1.0)
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
                        do {
                            rans[i] = RFETCH(s1,i);
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
            else if (tmp == 0.0)
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
                        do {
                            rans[i] = 1.0;
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
            else if (tmp == -1.0)
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
                        do {
                            rans[i] = 1.0 / RFETCH(s1,i);
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
            else
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
                        do {
                            rans[i] = R_pow (RFETCH(s1,i), tmp);
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
        }
        else if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,R_POW,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,R_POW,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,R_POW,rans,n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case MODOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfmod,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfmod,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,myfmod,rans,n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case IDIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfloor,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfloor,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,myfloor,rans,n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    }
}

extern double complex R_cpow (double complex, double complex);

void task_complex_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    R_len_t i1, i2, n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    if (TYPEOF(s1) == REALSXP) { /* s2 must be complex */
        switch (code) {
        case PLUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x2 = COMPLEX(s2)[i2];
                COMPLEX(ans)[i].r = REAL(s1)[i1] + x2.r;
                COMPLEX(ans)[i].i = x2.i;
            }
            break;
        case MINUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x2 = COMPLEX(s2)[i2];
                COMPLEX(ans)[i].r = REAL(s1)[i1] - x2.r;
                COMPLEX(ans)[i].i = -x2.i;
            }
            break;
        case TIMESOP:
            mod_iterate (n, n1, n2, i1, i2) {
                double x1 = REAL(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                COMPLEX(ans)[i].r = x1 * x2.r;
                COMPLEX(ans)[i].i = x1 * x2.i;
            }
            break;
        case DIVOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1;
                x1.r = REAL(s1)[i1];
                x1.i = 0;
                R_from_C99_complex (COMPLEX(ans)+i,
                                    C99_from_R_complex(&x1) 
                                     / C99_from_R_complex(COMPLEX(s2)+i2));
            }
            break;
        case POWOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1;
                x1.r = REAL(s1)[i1];
                x1.i = 0;
                R_from_C99_complex (COMPLEX(ans)+i,
                                    R_cpow(C99_from_R_complex(&x1),
                                           C99_from_R_complex(COMPLEX(s2)+i2)));
            }
            break;
        }
    }
    else if (TYPEOF(s2) == REALSXP) { /* s1 must be complex */
        switch (code) {
        case PLUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                COMPLEX(ans)[i].r = x1.r + REAL(s2)[i2];
                COMPLEX(ans)[i].i = x1.i;
            }
            break;
        case MINUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                COMPLEX(ans)[i].r = x1.r - REAL(s2)[i2];
                COMPLEX(ans)[i].i = x1.i;
            }
            break;
        case TIMESOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                double x2 = REAL(s2)[i2];
                COMPLEX(ans)[i].r = x1.r * x2;
                COMPLEX(ans)[i].i = x1.i * x2;
            }
            break;
        case DIVOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                double x2 = REAL(s2)[i2];
                COMPLEX(ans)[i].r = x1.r / x2;
                COMPLEX(ans)[i].i = x1.i / x2;
            }
            break;
        case POWOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x2;
                x2.r = REAL(s2)[i2];
                x2.i = 0;
                R_from_C99_complex (COMPLEX(ans)+i,
                                    R_cpow(C99_from_R_complex(COMPLEX(s1)+i1),
                                           C99_from_R_complex(&x2)));
            }
            break;
        }
    }
    else if (TYPEOF(s1) == INTSXP || TYPEOF(s1) == LGLSXP) { /* s2 complex */
        switch (code) {
        case PLUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                int x1 = INTEGER(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                if (x1 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1 + x2.r;
                    COMPLEX(ans)[i].i = x2.i;
                }
            }
            break;
        case MINUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                int x1 = INTEGER(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                if (x1 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1 - x2.r;
                    COMPLEX(ans)[i].i = -x2.i;
                }
            }
            break;
        case TIMESOP:
            mod_iterate (n, n1, n2, i1, i2) {
                int x1 = INTEGER(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                if (x1 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1 * x2.r;
                    COMPLEX(ans)[i].i = x1 * x2.i;
                }
            }
            break;
        case DIVOP:
            mod_iterate (n, n1, n2, i1, i2) {
                int x1 = INTEGER(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                if (x1 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    Rcomplex x1c;
                    x1c.r = x1;
                    x1c.i = 0;
                    R_from_C99_complex (COMPLEX(ans)+i,
                                        C99_from_R_complex(&x1c) 
                                          / C99_from_R_complex(&x2));
                }
            }
            break;
        case POWOP:
            mod_iterate (n, n1, n2, i1, i2) {
                int x1 = INTEGER(s1)[i1];
                Rcomplex x2 = COMPLEX(s2)[i2];
                if (x1 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    Rcomplex x1c;
                    x1c.r = x1;
                    x1c.i = 0;
                    R_from_C99_complex (COMPLEX(ans)+i,
                                        R_cpow(C99_from_R_complex(&x1c),
                                               C99_from_R_complex(&x2)));
                }
            }
            break;
        }
    }
    else if (TYPEOF(s2) == INTSXP || TYPEOF(s2) == LGLSXP) { /* s1 complex */
        switch (code) {
        case PLUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                int x2 = INTEGER(s2)[i2];
                if (x2 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1.r + x2;
                    COMPLEX(ans)[i].i = x1.i;
                }
            }
            break;
        case MINUSOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                int x2 = INTEGER(s2)[i2];
                if (x2 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1.r - x2;
                    COMPLEX(ans)[i].i = x1.i;
                }
            }
            break;
        case TIMESOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                int x2 = INTEGER(s2)[i2];
                if (x2 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1.r * x2;
                    COMPLEX(ans)[i].i = x1.i * x2;
                }
            }
            break;
        case DIVOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                int x2 = INTEGER(s2)[i2];
                if (x2 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    COMPLEX(ans)[i].r = x1.r / x2;
                    COMPLEX(ans)[i].i = x1.i / x2;
                }
            }
            break;
        case POWOP:
            mod_iterate (n, n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1];
                int x2 = INTEGER(s2)[i2];
                if (x2 == NA_INTEGER)
                    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
                else {
                    Rcomplex x2c;
                    x2c.r = x2;
                    x2c.i = 0;
                    R_from_C99_complex (COMPLEX(ans)+i,
                                        R_cpow(C99_from_R_complex(&x1),
                                               C99_from_R_complex(&x2c)));
                }
            }
            break;
        }
    }
    else { /* both complex */
        switch (code) {
        case PLUSOP:
            if (n1 > n2) { SEXP t = s1; s1 = s2; s2 = t; n1 = n2; n2 = n; }
            mod_iterate_1 (n1, n2, i1, i2) {
                Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
                COMPLEX(ans)[i].r = x1.r + x2.r;
                COMPLEX(ans)[i].i = x1.i + x2.i;
            }
            break;
        case MINUSOP:
            if (n1 <= n2) {
                mod_iterate_1 (n1, n2, i1, i2) {
                    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
                    COMPLEX(ans)[i].r = x1.r - x2.r;
                    COMPLEX(ans)[i].i = x1.i - x2.i;
                }
            }
            else {
                mod_iterate_2 (n1, n2, i1, i2) {
                    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
                    COMPLEX(ans)[i].r = x1.r - x2.r;
                    COMPLEX(ans)[i].i = x1.i - x2.i;
                }
            }
            break;
        case TIMESOP:
            if (n1 > n2) { SEXP t = s1; s1 = s2; s2 = t; n1 = n2; n2 = n; }
            mod_iterate_1 (n1, n2, i1, i2) {
                R_from_C99_complex (COMPLEX(ans)+i,
                                    C99_from_R_complex(COMPLEX(s1)+i1) 
                                     * C99_from_R_complex(COMPLEX(s2)+i2));
            }
            break;
        case DIVOP:
            mod_iterate (n, n1, n2, i1, i2) {
                R_from_C99_complex (COMPLEX(ans)+i,
                                    C99_from_R_complex(COMPLEX(s1)+i1) 
                                     / C99_from_R_complex(COMPLEX(s2)+i2));
            }
            break;
        case POWOP:
            mod_iterate (n, n1, n2, i1, i2) {
                R_from_C99_complex (COMPLEX(ans)+i,
                                    R_cpow(C99_from_R_complex(COMPLEX(s1)+i1),
                                           C99_from_R_complex(COMPLEX(s2)+i2)));
            }
            break;
        }
    }
}

#define T_arithmetic THRESHOLD_ADJUST(24)  /* >= 8, further adjusted below */

SEXP attribute_hidden R_binary (SEXP call, SEXP op, SEXP x, SEXP y, 
                                int objx, int objy, SEXP env, int variant)
{
    helpers_task_proc *task;
    SEXP klass, dims, tsp, xnames, ynames, ans;
    int mismatch = 0, nx, ny, n, xarray, yarray, xts, yts, xS4 = 0, yS4 = 0;
    int xattr, yattr;
    PROTECT_INDEX xpi, ypi;
    ARITHOP_TYPE oper = (ARITHOP_TYPE) PRIMVAL(op);
    int threshold, flags, nprotect;

    if (x == R_NilValue) x = allocVector(REALSXP,0);
    PROTECT_WITH_INDEX(x, &xpi);
    if (y == R_NilValue) y = allocVector(REALSXP,0);
    PROTECT_WITH_INDEX(y, &ypi);
    nprotect = 2;

    if (!isNumberOrFactor(x) || !isNumberOrFactor(y))
        errorcall(call, _("non-numeric argument to binary operator"));

    nx = LENGTH(x);
    if (HAS_ATTRIB(x)) {
        xattr = TRUE;
        xarray = isArray(x);
        xts = isTs(x);
        xS4 = objx && isS4(x);
    }
    else xarray = xts = xattr = FALSE;
    ny = LENGTH(y);
    if (HAS_ATTRIB(y)) {
        yattr = TRUE;
        yarray = isArray(y);
        yts = isTs(y);
        yS4 = objy && isS4(y);
    }
    else yarray = yts = yattr = FALSE;

    /* If either x or y is a matrix with length 1 and the other is a
       vector, we want to coerce the matrix to be a vector. */

    if (xarray != yarray) {
        if (xarray && nx==1 && ny!=1) {
            REPROTECT(x = duplicate(x), xpi);
            setAttrib(x, R_DimSymbol, R_NilValue);
        }
        if (yarray && ny==1 && nx!=1) {
            REPROTECT(y = duplicate(y), ypi);
            setAttrib(y, R_DimSymbol, R_NilValue);
        }
    }

    if (xarray || yarray) {
        if (xarray && yarray && !conformable(x,y))
                errorcall(call, _("non-conformable arrays"));
        PROTECT(dims = getAttrib (xarray ? x : y, R_DimSymbol));
        nprotect++;
        if (xattr) {
            PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
            nprotect++;
        }
        else xnames = R_NilValue;
        if (yattr) {
            PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
            nprotect++;
        }
        else ynames = R_NilValue;
    }
    else {
        dims = R_NilValue;
        if (xattr) {
            PROTECT(xnames = getAttrib(x, R_NamesSymbol));
            nprotect++;
        }
        else xnames = R_NilValue;
        if (yattr) {
            PROTECT(ynames = getAttrib(y, R_NamesSymbol));
            nprotect++;
        }
        else ynames = R_NilValue;
    }
    if (nx == ny || nx == 1 || ny == 1) mismatch = 0;
    else if (nx > 0 && ny > 0) {
        if (nx > ny) mismatch = nx % ny;
        else mismatch = ny % nx;
    }

    if (xts || yts) {
        if (xts && yts) {
            if (!tsConform(x, y))
                errorcall(call, _("non-conformable time-series"));
            PROTECT(tsp = getAttrib(x, R_TspSymbol));
            PROTECT(klass = !objx ? R_NilValue : getClassAttrib(x));
        }
        else if (xts) {
            if (nx < ny)
                ErrorMessage(call, ERROR_TSVEC_MISMATCH);
            PROTECT(tsp = getAttrib(x, R_TspSymbol));
            PROTECT(klass = !objx ? R_NilValue : getClassAttrib(x));
        }
        else {                        /* (yts) */
            if (ny < nx)
                ErrorMessage(call, ERROR_TSVEC_MISMATCH);
            PROTECT(tsp = getAttrib(y, R_TspSymbol));
            PROTECT(klass = !objy ? R_NilValue : getClassAttrib(y));
        }
        nprotect += 2;
    }
    else klass = tsp = R_NoObject; /* -Wall */

    if (mismatch)
        warningcall (call,
          _("longer object length is not a multiple of shorter object length"));

    /* S4-compatibility change: if nx or ny is 0, result is of length 0 */

    n = nx==0 || ny==0 ? 0 : nx>ny ? nx : ny;

    int local_assign1 = 0, local_assign2 = 0;

    if (VARIANT_KIND(variant) == VARIANT_LOCAL_ASSIGN1) {
        if (n == nx && !NAMEDCNT_GT_1(x)
          && x == findVarInFrame3 (env, CADR(call), 7))
            local_assign1 = 1;
    }
    else if (VARIANT_KIND(variant) == VARIANT_LOCAL_ASSIGN2) {
        if (n == ny && !NAMEDCNT_GT_1(y)
          && y == findVarInFrame3 (env, CADDR(call), 7))
            local_assign2 = 1;
    }
    
    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
        if (oper==IDIVOP || oper==MODOP)
            errorcall(call,_("unimplemented complex operation"));
        ans = alloc_or_reuse (x, y, CPLXSXP, n, local_assign1, local_assign2);
        task = task_complex_arithmetic;
        flags = 0;  /* Not bothering with pipelining yet. */
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
         /* task_real_arithmetic takes REAL, INT, and LOGICAL operands, 
            and assumes INT and LOGICAL are really the same. */
        ans = alloc_or_reuse (x, y, REALSXP, n, local_assign1, local_assign2);
        task = task_real_arithmetic;
        flags = HELPERS_PIPE_IN0_OUT;
        if (oper <= POWOP) { /* this is +, -, *, /, and ^ operators */
            if (n > 1 && (nx == 1 || ny == 1)
                  && TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP) {
                flags = HELPERS_PIPE_IN0_OUT | HELPERS_MERGE_IN_OUT;
            }
        }
        if (n>1) {
            if (nx==n) flags |= HELPERS_PIPE_IN1;
            if (ny==n) flags |= HELPERS_PIPE_IN2;
        }
    }
    else {
        /* task_integer_arithmetic is assumed to work for LOGICAL too, though
           this won't be true if they aren't really the same */
        if (oper == DIVOP || oper == POWOP)
            ans = allocVector(REALSXP, n);
        else
            ans = alloc_or_reuse(x, y, INTSXP, n, local_assign1, local_assign2);
        task = task_integer_arithmetic;

       /* Only ^, /, %/%, and %% can be done in helpers at present - others
          must be in the master because of possible integer overflow.
          Not bothering with pipelining yet. */

        flags = oper==POWOP || oper==DIVOP || oper==IDIVOP || oper==MODOP ? 0 
              : HELPERS_MASTER_NOW;
    }

    if (isObject(ans) && !objx && !objy) ans = allocVector (TYPEOF(ans), n);

    if (ans != x) local_assign1 = 0;
    if (ans != y) local_assign2 = 0;

    PROTECT(ans);
    nprotect++;

    /* Do the actual operation. */

    if (n!=0) {
        integer_overflow = 0;

        threshold = T_arithmetic;
        if (TYPEOF(ans)==CPLXSXP) threshold >>= 1;
        if (oper>TIMESOP) threshold >>= 1;

        if (n >= threshold && (variant & VARIANT_PENDING_OK)) {
            if (ON_SCALAR_STACK(x) && ON_SCALAR_STACK(y)) {
                PROTECT(x = duplicate(x));
                y = duplicate(y);
                UNPROTECT(1);
            }
            else if (ON_SCALAR_STACK(x)) x = duplicate(x);
            else if (ON_SCALAR_STACK(y)) y = duplicate(y);
        }
        DO_NOW_OR_LATER2 (variant, n>=threshold, flags, task, oper, ans, x, y);

        if (integer_overflow)
            warningcall(call, _("NAs produced by integer overflow"));
    }

    /* Copy attributes from arguments as needed. */

    if ((xattr || yattr) && (objx || objy || !(variant & VARIANT_ANY_ATTR))) {

        if (yattr && ny==n && ans!=y)
            objy ? copyMostAttrib(y, ans) : copyMostAttribNoClass(y, ans);
        if (xattr && nx==n && ans!=x) /* Done 2nd so x's attrs overwrite y's */
            objx ? copyMostAttrib(x, ans) : copyMostAttribNoClass(x, ans);
    
        /* Don't set the dims if one argument is an array of size 0 and the
           other isn't of size zero, cos they're wrong */
        /* Not if the other argument is a scalar (PR#1979) */
        if (dims != R_NilValue) {
            if (!((xarray && (nx == 0) && (ny > 1)) ||
                  (yarray && (ny == 0) && (nx > 1)))){
                setAttrib(ans, R_DimSymbol, dims);
                if (xnames != R_NilValue)
                    setAttrib(ans, R_DimNamesSymbol, xnames);
                else if (ynames != R_NilValue)
                    setAttrib(ans, R_DimNamesSymbol, ynames);
            }
        }
        else {
            if (LENGTH(ans) == length(xnames))
                setAttrib(ans, R_NamesSymbol, xnames);
            else if (LENGTH(ans) == length(ynames))
                setAttrib(ans, R_NamesSymbol, ynames);
        }
    
        if (xts || yts) {                /* must set *after* dims! */
            setAttrib(ans, R_TspSymbol, tsp);
            setAttrib(ans, R_ClassSymbol, klass);
        }
    
        if(xS4 || yS4) {   /* Only set the bit:  no method defined! */
            ans = asS4(ans, TRUE, TRUE);
        }
    }

    R_variant_result = local_assign1 | local_assign2;
    UNPROTECT(nprotect);
    return ans;
}

void task_unary_minus (helpers_op_t op, SEXP ans, SEXP s1, SEXP ignored)
{
    R_len_t n = LENGTH(s1);
    R_len_t i = 0;
    R_len_t a;

    HELPERS_SETUP_OUT (9);

    switch (TYPEOF(s1)) {

    case LGLSXP:
        /* Assume LGLSXP is really the same as INTSXP... */
    case INTSXP:
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                R_len_t u = HELPERS_UP_TO(i,a);
                do {
                    int x = INTEGER(s1)[i];
                    INTEGER(ans)[i] = x==NA_INTEGER ? NA_INTEGER : -x;
                    i += 1;
                } while (i<=u);
                helpers_amount_out(i);
            } while (i < a);
        }
        break;

    case REALSXP:
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                R_len_t u = HELPERS_UP_TO(i,a);
                do {
                    REAL(ans)[i] = -REAL(s1)[i];
                    i += 1;
                } while (i<=u);
                helpers_amount_out(i);
            } while (i < a);
        }
        break;

    case CPLXSXP:
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                R_len_t u = HELPERS_UP_TO(i,a);
                do {
                    COMPLEX(ans)[i].r = -COMPLEX(s1)[i].r;
                    COMPLEX(ans)[i].i = -COMPLEX(s1)[i].i;
                    i += 1;
                } while (i<=u);
                helpers_amount_out(i);
            } while (i < a);
        }
        break;
    }
}

#define T_unary_minus THRESHOLD_ADJUST(20)

SEXP attribute_hidden R_unary (SEXP call, SEXP op, SEXP s1, int obj1,
                               SEXP env, int variant)
{
    ARITHOP_TYPE operation = (ARITHOP_TYPE) PRIMVAL(op);
    int type = TYPEOF(s1);
    int local_assign = 0;
    SEXP ans;
    int n;

    if ( ! ((NUMBER_TYPES >> type) & 1))
        errorcall(call, _("invalid argument to unary operator"));

    n = LENGTH(s1);

    if (type == LGLSXP) {
        SEXP dim, dimnames, names;
        ans = allocVector(LGLSXP,n);
        PROTECT (names    = getAttrib (s1, R_NamesSymbol));
        PROTECT (dim      = getDimAttrib(s1));
        PROTECT (dimnames = getAttrib (s1, R_DimNamesSymbol));
        if (names    != R_NilValue) setAttrib(ans,R_NamesSymbol,    names);
        if (dim      != R_NilValue) setAttrib(ans,R_DimSymbol,      dim);
        if (dimnames != R_NilValue) setAttrib(ans,R_DimNamesSymbol, dimnames);
        UNPROTECT(3);
    }

    if (operation==PLUSOP) {
        if (type != LGLSXP)
            ans = isObject(s1) && !obj1 ? Rf_makeUnclassed(s1) : s1;
        else {
            WAIT_UNTIL_COMPUTED(s1);
            for (int i = 0; i<LENGTH(s1); i++) INTEGER(ans)[i] = LOGICAL(s1)[i];
        }
    }
    else if (operation==MINUSOP) {
        if (type == LGLSXP) 
            ; /* allocated above */
        else if (isObject(s1) && !obj1)
            ans = Rf_makeUnclassed(s1);
        else {
            if (VARIANT_KIND(variant) == VARIANT_LOCAL_ASSIGN1
              && !NAMEDCNT_GT_1(s1) && s1 == findVarInFrame3(env,CADR(call),7))
                local_assign = 1;
            ans = local_assign || NAMEDCNT_EQ_0(s1) ? s1 : duplicate(s1);
        }
        DO_NOW_OR_LATER1 (variant, n >= T_unary_minus,
          TYPEOF(s1)==REALSXP ? HELPERS_PIPE_IN01_OUT | HELPERS_MERGE_IN_OUT
                              : HELPERS_PIPE_IN01_OUT,
          task_unary_minus, 0, ans, s1);
    }
    else
        errorcall(call, _("invalid argument to unary operator"));

    R_variant_result = local_assign;  /* do at end, just in case */
    return ans;
}


/* MATHEMATICAL FUNCTIONS OF ONE ARGUMENT.  Implements a variant return
   of the sum of the vector result, rather than the vector itself. */

/* Table to map math1 operation code to function.  The entries for trunc
   and R_log are not called via do_math1 like the others, but from special
   primitives.  The entry for fabs is currently not used, since the compiler
   may be able to inline fabs when it's called directly from task_abs. */

double (* const R_math1_func_table[44])(double) = {
        /*      0       1       2       3       4       5       6 7 8 9 */
/* 00 */        fabs,   floor,  ceil,   sqrt,   sign,   trunc,  0,0,0,0,
/* 10 */        exp,    expm1,  log1p,  R_log,  0,      0,      0,0,0,0,
/* 20 */        cos,    sin,    tan,    acos,   asin,   atan,   0,0,0,0,
/* 30 */        cosh,   sinh,   tanh,   acosh,  asinh,  atanh,  0,0,0,0,
/* 40 */      lgammafn, gammafn,digamma,trigamma
};

/* Table of flags saying when an operation may produce NA/NaN or warning:

       0:  NA/NaN only when argument is NA/NaN
       1:  NA/NaN only when argument is NA/Nan or +-Inf
      -1:  NA/NaN possible for finite argument as well as NA/NaN or +- Inf
      -2:  may produce warning message, not just NA/NaN

   Entries correspond to those in R_math1_func_table above. */

const char R_math1_err_table[44] = {
        /*      0       1       2       3       4       5       6 7 8 9 */
/* 00 */        0,      0,      0,      -1,     0,      0,      0,0,0,0,
/* 10 */        0,      0,      -1,     -1,     0,      0,      0,0,0,0,
/* 20 */        1,      1,      1,      -1,     -1,     0,      0,0,0,0,
/* 30 */        0,      0,      0,      -1,     0,      -1,     0,0,0,0,
/* 40 */        -2,     -2,     -1,     -1
};

int R_naflag;  /* Set to one (in master) for the "NAs produced" warning */

void task_math1 (helpers_op_t opcode, SEXP sy, SEXP sa, SEXP ignored)
{
    double *ra = REAL(sa);
    double *ry = REAL(sy);
    R_len_t n = LENGTH(sa);
    R_len_t i = 0;
    R_len_t a;

    double (*f)(double) = R_math1_func_table[opcode];

    HELPERS_SETUP_OUT(5);

    if (R_math1_err_table[opcode]==0) {
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else
                    ry[i] = f(ra[i]);
                HELPERS_NEXT_OUT(i);
            } while (i < a);
        }
    }
    else {
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else {
                    ry[i] = f(ra[i]);
                    if (ISNAN(ry[i])) {
                        R_naflag = 1; /* only done in master thread */
                    }
                }
                HELPERS_NEXT_OUT(i);
            } while (i < a);
        }
    }
}

void task_sum_math1 (helpers_op_t opcode, SEXP sy, SEXP sa, SEXP ignored)
{
    double *ra = REAL(sa);
    long double s = 0.0;
    R_len_t n = LENGTH(sa);
    R_len_t i = 0;
    R_len_t a;

    double (*f)(double) = R_math1_func_table[opcode];

    if (R_math1_err_table[opcode]==0) {
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    s += ra[i];
                else
                    s += f(ra[i]);
                i += 1;
            } while (i < a);
        }
    }
    else {
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    s += ra[i];
                else {
                    double t = f(ra[i]);
                    if (ISNAN(t)) {
                        R_naflag = 1; /* only done in master thread */
                    }
                    s += t;
                }
                i += 1;
            } while (i < a);
        }
    }

    REAL(sy)[0] = (double) s;
}

#define T_math1 THRESHOLD_ADJUST(5)

static SEXP math1(SEXP sa, unsigned opcode, SEXP call, SEXP env, int variant)
                  /* Note:  sa may be on the scalar stack. */
{
    if (opcode == 10003) /* horrible kludge for log */
        opcode = 13;
    else if (opcode >= 44)
        errorcall(call, _("unimplemented real function of 1 argument"));

    if (!isNumeric(sa)) non_numeric_errorcall(call);

    int local_assign = 0;
    int n = LENGTH(sa);
    SEXP sa0 = sa;

    if (TYPEOF(sa) != REALSXP)
        sa = coerceVector(sa, REALSXP); /* coercion can lose the object bit */
    else if (VARIANT_KIND(variant)==VARIANT_LOCAL_ASSIGN1 && !NAMEDCNT_GT_1(sa)
              && sa == findVarInFrame3 (env, CADR(call), 7))
        local_assign = 1;

    PROTECT(sa);

    SEXP sy;

    if (LENGTH(sa) == 1) { /* scalar operation, including on scalar stack. */

        WAIT_UNTIL_COMPUTED(sa);

        double opr = REAL(sa)[0];
        double res;

        if (ISNAN(opr))
            res = opr;
        else {
            res = R_math1_func_table[opcode] (opr);
            if (R_math1_err_table[opcode] && ISNAN(res))
                NaN_warningcall(call);
        }

        POP_IF_TOP_OF_STACK(sa0);

        if (local_assign || NAMEDCNT_EQ_0(sa)) {
            sy = sa;
            *REAL(sy) = res;
        }
        else if (CAN_USE_SCALAR_STACK(variant) && NO_ATTRIBUTES_OK(variant,sa))
            sy = PUSH_SCALAR_REAL(res);
        else {
            PROTECT(sy = ScalarReal(res));
            maybe_dup_attributes (sy, sa, variant);
            UNPROTECT(1);
        }

        UNPROTECT(1);
    }

    else { /* not scalar */

        /* Note: need to protect sy below because some ops may produce a warning
           and attributes may be duplicated. */

        R_naflag = 0;

        if (VARIANT_KIND(variant) == VARIANT_SUM) { /* just need the sum */

            PROTECT(sy = allocVector1REAL());
            DO_NOW_OR_LATER1 (variant, 
                        LENGTH(sa) >= T_math1 && R_math1_err_table[opcode] == 0,
                        HELPERS_PIPE_IN1, task_sum_math1, opcode, sy, sa);
        }

        else { /* not scalar, not just sum */

            PROTECT(sy = local_assign || NAMEDCNT_EQ_0(sa) 
                           ? sa : allocVector(REALSXP, n));

            DO_NOW_OR_LATER1 (variant,
                        LENGTH(sa) >= T_math1 && R_math1_err_table[opcode] == 0,
                        HELPERS_PIPE_IN01_OUT,
                        task_math1, opcode, sy, sa);

            maybe_dup_attributes (sy, sa, variant);
        }

        if (R_naflag)
            NaN_warningcall(call);
        UNPROTECT(2);
    }

    R_variant_result = local_assign;  /* defer setting to just before return */

    return sy;
}

static SEXP do_fast_math1(SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    if (isComplex(arg)) {
        /* for the moment, keep the interface to complex_math1 the same */
        SEXP tmp;
        PROTECT(tmp = CONS(arg,R_NilValue));
        WAIT_UNTIL_COMPUTED(arg);
        tmp = complex_math1(call, op, tmp, env);
        UNPROTECT(1);
        return tmp;
    }

    return math1 (arg, PRIMVAL(op), call, env, variant);
}


SEXP attribute_hidden do_math1(SEXP call, SEXP op, SEXP args, SEXP env, 
                               int variant)
{
    SEXP s;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    return do_fast_math1 (call, op, CAR(args), env, variant);
}

/* Methods for trunc are allowed to have more than one arg */

static SEXP do_fast_trunc (SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    return math1(arg, 5, call, env, variant);
}

SEXP do_trunc(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP s;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    check1arg_x (args, call);

    return do_fast_trunc (call, op, CAR(args), env, variant);
}

/* Note that abs is slightly different from the do_math1 set, both
   for integer/logical inputs and what it dispatches to for complex ones. 
   Also, the compiler is more likely to generate inline code for fabs
   than for other math functions, so treating it specially may be good. */

void task_abs (helpers_op_t op, SEXP s, SEXP x, SEXP ignored)
{
    R_len_t n = LENGTH(x);
    R_len_t i = 0;
    R_len_t a;

    HELPERS_SETUP_OUT(9);
    while (i < n) {
        HELPERS_WAIT_IN1 (a, i, n);
        while (i < a-1) {
            REAL(s)[i] = fabs(REAL(x)[i]);
            HELPERS_NEXT_OUT(i);
            REAL(s)[i] = fabs(REAL(x)[i]);
            HELPERS_NEXT_OUT(i);
        }
        if (i < a) {
            REAL(s)[i] = fabs(REAL(x)[i]);
            HELPERS_NEXT_OUT(i);
        }
    }
}

void task_sum_abs (helpers_op_t op, SEXP s, SEXP x, SEXP ignored)
{
    R_len_t n = LENGTH(x);
    R_len_t i = 0;
    R_len_t a;
    long double r = 0.0;

    while (i < n) {
        HELPERS_WAIT_IN1 (a, i, n);
        do {
            r += fabs(REAL(x)[i]);
            i += 1;
        } while (i < a);
    }
    REAL(s)[0] = (double) r;
}

#define T_abs THRESHOLD_ADJUST(10)

static SEXP do_fast_abs (SEXP call, SEXP op, SEXP x, SEXP env, int variant)
{   
    SEXP s;

    POP_IF_TOP_OF_STACK(x);

    if (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
	/* integer or logical ==> return integer,
	   factor was covered by Math.factor. */
        int n = LENGTH(x);
        if (n == 1) {
            WAIT_UNTIL_COMPUTED(x);
            int v;
            v = *INTEGER(x);
            v = v==NA_INTEGER ? NA_INTEGER : v<0 ? -v : v;
            s = NAMEDCNT_EQ_0(x) && TYPEOF(x) == INTSXP ? 
                  (*INTEGER(x) = v, x)
              : CAN_USE_SCALAR_STACK(variant) && NO_ATTRIBUTES_OK(variant,x) ?
                  PUSH_SCALAR_INTEGER(v)
              :   ScalarInteger(v);
        }
        else {
            s = NAMEDCNT_EQ_0(x) && TYPEOF(x) == INTSXP ? x 
              :   allocVector(INTSXP,n);
            WAIT_UNTIL_COMPUTED(x);
            /* Note: relying on INTEGER(.) === LOGICAL(.) : */
            for (int i = 0 ; i < n ; i++) {
                int v = INTEGER(x)[i];
                INTEGER(s)[i] = v==NA_INTEGER ? NA_INTEGER : v<0 ? -v : v;
            }
        }
    } 

    else if (TYPEOF(x) == REALSXP) {
        int n = LENGTH(x);
        if (VARIANT_KIND(variant) == VARIANT_SUM) {
            s = allocVector1REAL();
            DO_NOW_OR_LATER1 (variant, n >= T_abs,
                              HELPERS_PIPE_IN1, task_sum_abs, 0, s, x);
            return s;
        }
        else if (n == 1) {
            WAIT_UNTIL_COMPUTED(x);
            double v = fabs(*REAL(x));
            s = NAMEDCNT_EQ_0(x) && TYPEOF(x) == REALSXP ? 
                  (*REAL(x) = v, x)
              : CAN_USE_SCALAR_STACK(variant) && NO_ATTRIBUTES_OK(variant,x) ?
                  PUSH_SCALAR_REAL(v)
              :   ScalarReal(v);
        }
        else { /* x won't be on scalar stack, since n != 1 */
            s = NAMEDCNT_EQ_0(x) ? x : allocVector(REALSXP, n);
            DO_NOW_OR_LATER1 (variant, n >= T_abs,
                              HELPERS_PIPE_IN01_OUT | HELPERS_MERGE_IN_OUT,
                              task_abs, 0, s, x);
        }

    } else if (isComplex(x)) {
        WAIT_UNTIL_COMPUTED(x);
        s = do_fast_cmathfuns (call, op, x, env, variant);

    } else
        non_numeric_errorcall(call);

    PROTECT(s);
    maybe_dup_attributes (s, x, variant);
    UNPROTECT(1);

    return s;
}

SEXP do_abs(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP s;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    return do_fast_abs (call, op, CAR(args), env, variant);
}

/* Mathematical Functions of Two Numeric Arguments (plus 0, 1, or 2 integers) */

static void setup_Math2 
    (SEXP *sa, SEXP *sb, SEXP *sy, int na, int nb, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb))
	non_numeric_errorcall(lcall);

    if (na == 0 || nb == 0) {
	PROTECT(*sy = allocVector(REALSXP, 0));
        /* for 0-length a we want the attributes of a, not those of b
           as no recycling will occur */
	if (na == 0) DUPLICATE_ATTRIB(*sy, *sa);
	UNPROTECT(1);
        return;
    }

    PROTECT(*sa = coerceVector (*sa, REALSXP));
    PROTECT(*sb = coerceVector (*sb, REALSXP));
    PROTECT(*sy = allocVector (REALSXP, na < nb ? nb : na));
}

#define DO_MATH2(y,a,b,n,na,nb,fncall) do { \
    int naflag = 0; \
    double ai, bi; \
    R_len_t ia, ib; \
    mod_iterate (n, na, nb, ia, ib) { \
        ai = a[ia]; \
        bi = b[ib]; \
        if (MAY_BE_NAN2(ai,bi)) { \
            if (ISNA(ai) || ISNA(bi)) { \
                y[i] = NA_REAL; \
                continue; \
            } \
            if (ISNAN(ai) || ISNAN(bi)) { \
                y[i] = R_NaN; \
                continue; \
            } \
        } \
        y[i] = fncall; \
        if (ISNAN(y[i])) naflag = 1; \
    } \
    if (naflag) NaN_warning(); \
    SEXP frm = n==na ? sa : sb; \
    DUPLICATE_ATTRIB(sy, frm); \
    UNPROTECT(3); \
} while (0)


static SEXP math2(SEXP sa, SEXP sb, double (*f)(double, double),
		  SEXP lcall)
{
    double *a, *b, *y;
    int n, na, nb;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb);
    setup_Math2 (&sa, &sb, &sy, na, nb, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); y = REAL(sy);

    DO_MATH2(y,a,b,n,na,nb, f(ai,bi));

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI,
		    double (*f)(double, double, int), SEXP lcall)
{
    double *a, *b, *y;
    int n, na, nb;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb);
    setup_Math2 (&sa, &sb, &sy, na, nb, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); y = REAL(sy);

    int m_opt = asInteger(sI);

    DO_MATH2(y,a,b,n,na,nb, f(ai,bi,m_opt));

    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int), SEXP lcall)
{
    double *a, *b, *y;
    int n, na, nb;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb);
    setup_Math2 (&sa, &sb, &sy, na, nb, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); y = REAL(sy);

    int i_1 = asInteger(sI1);
    int i_2 = asInteger(sI2);

    DO_MATH2(y,a,b,n,na,nb, f(ai,bi,i_1,i_2));

    return sy;
} /* math2_2() */

static SEXP math2B(SEXP sa, SEXP sb, double (*f)(double, double, double *),
		   SEXP lcall)
{
    double *a, *b, *y;
    int n, na, nb;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb);
    setup_Math2 (&sa, &sb, &sy, na, nb, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); y = REAL(sy);

    /* allocate work array for BesselJ, BesselY large enough for all
       arguments */

    double amax, *work;
    long nw;
    int i;

    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (av > amax) amax = av;
    }
    nw = 1 + (long)floor(amax);
    work = (double *) R_alloc((size_t) nw, sizeof(double));

    DO_MATH2(y,a,b,n,na,nb, f(ai,bi,work));

    return sy;
} /* math2B() */

#define Math2(A, FUN)	  math2(CAR(A), CADR(A), FUN, call)
#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN, call)
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call)
#define Math2B(A, FUN)	  math2B(CAR(A), CADR(A), FUN, call)

SEXP do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if (isComplex(CAR(args)) ||
	(PRIMVAL(op) == 0 && isComplex(CADR(args))))
	return complex_math2(call, op, args, env);

    switch (PRIMVAL(op)) {

    case  0: return Math2(args, atan2);

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case  6: return Math2_1(args, dchisq);
    case  7: return Math2_2(args, pchisq);
    case  8: return Math2_2(args, qchisq);

    case  9: return Math2_1(args, dexp);
    case 10: return Math2_2(args, pexp);
    case 11: return Math2_2(args, qexp);

    case 12: return Math2_1(args, dgeom);
    case 13: return Math2_2(args, pgeom);
    case 14: return Math2_2(args, qgeom);

    case 15: return Math2_1(args, dpois);
    case 16: return Math2_2(args, ppois);
    case 17: return Math2_2(args, qpois);

    case 18: return Math2_1(args, dt);
    case 19: return Math2_2(args, pt);
    case 20: return Math2_2(args, qt);

    case 21: return Math2_1(args, dsignrank);
    case 22: return Math2_2(args, psignrank);
    case 23: return Math2_2(args, qsignrank);

    case 24: return Math2B(args, bessel_j_ex);
    case 25: return Math2B(args, bessel_y_ex);
    case 26: return Math2(args, psigamma);

    default: 
        /* Put 10001 and 10004 here so switch table won't be sparse. */
        if (PRIMVAL(op) == 10001)
            return Math2(args, fround); /* round(), src/nmath/fround.c */
        if (PRIMVAL(op) == 10004)
            return Math2(args, fprec);  /* signif(), src/nmath/fprec.c */

	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 2);
    }
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2;

    args = evalListKeepMissing(args, env);
    int n = length(args);

    if (n >= 2 && CADR(args) == R_MissingArg) {
        /* we ignore arguments after the second - not sure why... */
        PROTECT(args);
	SEXP digits = ScalarRealMaybeConst (PRIMVAL(op) == 10004 ? 6.0 : 0.0);
        args = list2 (CAR(args), digits);
        UNPROTECT(1);
        n = 2;
    }

    if (n != 1 && n != 2)
	error(_("%d arguments passed to '%s' which requires 1 or 2"),
	      n, PRIMNAME(op));

    PROTECT(call2 = LCONS (CAR(call), args));
    int nprotect = 1;

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	if(n == 1) {
	    double digits = 0.0;
            check1arg_x (args, call);
	    if(PRIMVAL(op) == 10004) digits = 6.0;
	    SETCDR(args, CONS(ScalarRealMaybeConst(digits), R_NilValue));
	} else {
	    /* If named, do argument matching by name */
	    if (TAG(args) != R_NilValue || TAG(CDR(args)) != R_NilValue) {
                static char *ap[2] = { "x", "digits" };
		PROTECT(args = matchArgs(R_NilValue, ap, 2, args, call));
		nprotect += 1;
	    }
	    if (length(CADR(args)) == 0)
		errorcall(call, _("invalid second argument of length 0"));
	}
	res = do_math2(call, op, args, env);
    }

    UNPROTECT(nprotect);
    R_Visible = TRUE;
    return res;
}

/* log{2,10} are builtins */
SEXP do_log1arg(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2, args2, tmp = R_NilValue /* -Wall */;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &res)) return res;

    SEXP sLog = install("log");
    if(PRIMVAL(op) == 10) tmp = ScalarRealMaybeConst(10.0);
    if(PRIMVAL(op) == 2)  tmp = ScalarRealMaybeConst(2.0);

    PROTECT(call2 = lang3(sLog, CAR(args), tmp));
    PROTECT(args2 = lang2(CAR(args), tmp));
    if (! DispatchGroup("Math", call2, op, args2, env, &res)) {
	if (isComplex(CAR(args)))
	    res = complex_math2(call2, op, args2, env);
	else
	    res = math2(CAR(args), tmp, logbase, call);
    }
    UNPROTECT(2);
    return res;
}


/* This is a primitive SPECIALSXP with internal argument matching */
SEXP do_log (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    /* Do the common case of one un-tagged, non-object, argument quickly. */

    if (!isNull(args) && isNull(CDR(args)) && isNull(TAG(args)) 
          && CAR(args) != R_DotsSymbol && CAR(args) != R_MissingArg) {

        SEXP arg, ans;
        arg = evalv (CAR(args), env, 
                     VARIANT_PENDING_OK | VARIANT_SCALAR_STACK_OK);
        if (isObject(arg)) {
            WAIT_UNTIL_COMPUTED(arg);
            args = CONS(arg, R_NilValue);
        }
        else {
            PROTECT(arg);
            ans = do_fast_math1 (call, op, arg, env, variant);
            UNPROTECT(1);
            R_Visible = TRUE;
            return ans;
        }
    }
    else {

        args = evalListKeepMissing(args, env);

        /* This seems like some sort of horrible kludge that can't possibly
           be right in general (it ignores the argument names, and silently
           discards arguments after the first two). */
        if (CDR(args) != R_NilValue && CADR(args) == R_MissingArg) {
#ifdef M_E
	    double e = M_E;
#else
	    double e = exp(1.);
#endif
            PROTECT(args);
	    args = list2(CAR(args), ScalarReal(e)); 
            UNPROTECT(1);
        }
    }

    SEXP res, call2;
    PROTECT(call2 = LCONS (CAR(call), args));

    int n = length(args);

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	switch (n) {
	case 1:
            check1arg_x (args, call);
	    if (isComplex(CAR(args)))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(CAR(args), 13, call, env, variant);
	    break;
	case 2:
	{
	    /* match argument names if supplied */
            static char *ap[2] = { "x", "base" };
	    PROTECT(args = matchArgs(R_NilValue, ap, 2, args, call));
	    if (length(CADR(args)) == 0)
		errorcall(call, _("invalid argument 'base' of length 0"));
	    if (isComplex(CAR(args)) || isComplex(CADR(args)))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(CAR(args), CADR(args), logbase, call);
            UNPROTECT(1); /* args */
	    break;
	}
	default:
	    error(_("%d arguments passed to 'log' which requires 1 or 2"), n);
	}
    }

    UNPROTECT(1); /* call2 */
    R_Visible = TRUE;
    return res;
}


/* Mathematical Functions of Three (Real) Arguments */

static void setup_Math3
    (SEXP *sa, SEXP *sb, SEXP *sc, SEXP *sy, int na, int nb, int nc, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb) || !isNumeric(*sc))
	non_numeric_errorcall(lcall);

    if (na == 0 || nb == 0 || nc == 0) {
	*sy = allocVector(REALSXP,0);
        return;
    }

    PROTECT(*sa = coerceVector (*sa, REALSXP));
    PROTECT(*sb = coerceVector (*sb, REALSXP));
    PROTECT(*sc = coerceVector (*sc, REALSXP));

    int n = na;
    if (n < nb) n = nb;
    if (n < nc) n = nc;
    PROTECT(*sy = allocVector (REALSXP, n));
}

#define DO_MATH3(y,a,b,c,n,na,nb,nc,fncall) do { \
    int naflag = 0; \
    double ai, bi, ci; \
    int i, ia, ib, ic; \
    for (i = ia = ib = ic = 0; i < n; \
         ia = (++ia==na) ? 0 : ia, \
         ib = (++ib==nb) ? 0 : ib, \
         ic = (++ic==nc) ? 0 : ic, i++) { \
        ai = a[ia]; \
        bi = b[ib]; \
        ci = c[ic]; \
        if (MAY_BE_NAN3(ai,bi,ci)) { \
            if (ISNA(ai) || ISNA(bi) || ISNA(ci)) { \
                y[i] = NA_REAL; \
                continue; \
            } \
            if (ISNAN(ai) || ISNAN(bi) || ISNAN(ci)) { \
                y[i] = R_NaN; \
                continue; \
            } \
        } \
        y[i] = fncall; \
        if (ISNAN(y[i])) naflag = 1; \
    } \
    if (naflag) NaN_warning(); \
    SEXP frm = n==na ? sa : n==nb ? sb : sc; \
    DUPLICATE_ATTRIB(sy, frm); \
    UNPROTECT(4); \
} while (0)

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int), SEXP lcall)
{
    double *a, *b, *c, *y;
    int n, na, nb, nc;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc);
    setup_Math3 (&sa, &sb, &sc, &sy, na, nb, nc, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); y = REAL(sy);

    int i_1 = asInteger(sI);

    DO_MATH3(y,a,b,c,n,na,nb,nc, f(ai,bi,ci,i_1));

    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int), SEXP lcall)
{
    double *a, *b, *c, *y;
    int n, na, nb, nc;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc);
    setup_Math3 (&sa, &sb, &sc, &sy, na, nb, nc, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); y = REAL(sy);

    int i_1 = asInteger(sI);
    int i_2 = asInteger(sJ);

    DO_MATH3(y,a,b,c,n,na,nb,nc, f(ai,bi,ci,i_1,i_2));

    return sy;
} /* math3_2 */

static SEXP math3B(SEXP sa, SEXP sb, SEXP sc,
		   double (*f)(double, double, double, double *), SEXP lcall)
{
    double *a, *b, *c, *y;
    int n, na, nb, nc;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc);
    setup_Math3 (&sa, &sb, &sc, &sy, na, nb, nc, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); y = REAL(sy);

    /* allocate work array for BesselI, BesselK large enough for all
       arguments */

    double amax, *work;
    long nw;
    int i;

    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (av > amax) amax = av;
    }
    nw = 1 + (long)floor(amax);
    work = (double *) R_alloc((size_t) nw, sizeof(double));

    DO_MATH3(y,a,b,c,n,na,nb,nc, f(ai,bi,ci,work));

    return sy;
} /* math3B */

#define Math3_1(A, FUN)	math3_1(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call)
#define Math3_2(A, FUN) math3_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN, call)
#define Math3B(A, FUN)  math3B (CAR(A), CADR(A), CADDR(A), FUN, call)

SEXP do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    switch (PRIMVAL(op)) {

    case  1:  return Math3_1(args, dbeta);
    case  2:  return Math3_2(args, pbeta);
    case  3:  return Math3_2(args, qbeta);

    case  4:  return Math3_1(args, dbinom);
    case  5:  return Math3_2(args, pbinom);
    case  6:  return Math3_2(args, qbinom);

    case  7:  return Math3_1(args, dcauchy);
    case  8:  return Math3_2(args, pcauchy);
    case  9:  return Math3_2(args, qcauchy);

    case 10:  return Math3_1(args, df);
    case 11:  return Math3_2(args, pf);
    case 12:  return Math3_2(args, qf);

    case 13:  return Math3_1(args, dgamma);
    case 14:  return Math3_2(args, pgamma);
    case 15:  return Math3_2(args, qgamma);

    case 16:  return Math3_1(args, dlnorm);
    case 17:  return Math3_2(args, plnorm);
    case 18:  return Math3_2(args, qlnorm);

    case 19:  return Math3_1(args, dlogis);
    case 20:  return Math3_2(args, plogis);
    case 21:  return Math3_2(args, qlogis);

    case 22:  return Math3_1(args, dnbinom);
    case 23:  return Math3_2(args, pnbinom);
    case 24:  return Math3_2(args, qnbinom);

    case 25:  return Math3_1(args, dnorm);
    case 26:  return Math3_2(args, pnorm);
    case 27:  return Math3_2(args, qnorm);

    case 28:  return Math3_1(args, dunif);
    case 29:  return Math3_2(args, punif);
    case 30:  return Math3_2(args, qunif);

    case 31:  return Math3_1(args, dweibull);
    case 32:  return Math3_2(args, pweibull);
    case 33:  return Math3_2(args, qweibull);

    case 34:  return Math3_1(args, dnchisq);
    case 35:  return Math3_2(args, pnchisq);
    case 36:  return Math3_2(args, qnchisq);

    case 37:  return Math3_1(args, dnt);
    case 38:  return Math3_2(args, pnt);
    case 39:  return Math3_2(args, qnt);

    case 40:  return Math3_1(args, dwilcox);
    case 41:  return Math3_2(args, pwilcox);
    case 42:  return Math3_2(args, qwilcox);

    case 43:  return Math3B(args, bessel_i_ex);
    case 44:  return Math3B(args, bessel_k_ex);

    case 45:  return Math3_1(args, dnbinom_mu);
    case 46:  return Math3_2(args, pnbinom_mu);
    case 47:  return Math3_2(args, qnbinom_mu);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 3);
    }
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

static void setup_Math4 (SEXP *sa, SEXP *sb, SEXP *sc, SEXP *sd, SEXP *sy, 
                         int na, int nb, int nc, int nd, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb) || !isNumeric(*sc) || !isNumeric(*sd))
	non_numeric_errorcall(lcall);

    if (na == 0 || nb == 0 || nc == 0 || nd == 0) {
	*sy = allocVector(REALSXP,0);
        return;
    }

    PROTECT(*sa = coerceVector (*sa, REALSXP));
    PROTECT(*sb = coerceVector (*sb, REALSXP));
    PROTECT(*sc = coerceVector (*sc, REALSXP));
    PROTECT(*sd = coerceVector (*sd, REALSXP));

    int n = na;
    if (n < nb) n = nb;
    if (n < nc) n = nc;
    if (n < nd) n = nd;
    PROTECT(*sy = allocVector (REALSXP, n));
}

#define DO_MATH4(y,a,b,c,d,n,na,nb,nc,nd,fncall) do { \
    int naflag = 0; \
    double ai, bi, ci, di; \
    int i, ia, ib, ic, id; \
    for (i = ia = ib = ic = id = 0; i < n; \
         ia = (++ia==na) ? 0 : ia, \
         ib = (++ib==nb) ? 0 : ib, \
         ic = (++ic==nc) ? 0 : ic, \
         id = (++id==nd) ? 0 : id, i++) { \
        ai = a[ia]; \
        bi = b[ib]; \
        ci = c[ic]; \
        di = d[id]; \
        if (MAY_BE_NAN4(ai,bi,ci,di)) { \
            if (ISNA(ai) || ISNA(bi) || ISNA(ci) || ISNA(di)) { \
                y[i] = NA_REAL; \
                continue; \
            } \
            if (ISNAN(ai) || ISNAN(bi) || ISNAN(ci) || ISNAN(di)) { \
                y[i] = R_NaN; \
                continue; \
            } \
        } \
        y[i] = fncall; \
        if (ISNAN(y[i])) naflag = 1; \
    } \
    if (naflag) NaN_warning(); \
    SEXP frm = n==na ? sa : n==nb ? sb : n==nc ? sc : sd; \
    DUPLICATE_ATTRIB(sy, frm); \
    UNPROTECT(5); \
} while (0)


static SEXP math4 (SEXP sa, SEXP sb, SEXP sc, SEXP sd,
              double (*f)(double, double, double, double), SEXP lcall)
{
    double *a, *b, *c, *d, *y;
    int n, na, nb, nc, nd;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc); nd = LENGTH(sd);
    setup_Math4 (&sa, &sb, &sc, &sd, &sy, na, nb, nc, nd, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); d = REAL(sd); y = REAL(sy);

    DO_MATH4(y,a,b,c,d,n,na,nb,nc,nd, f(ai,bi,ci,di));

    return sy;
} /* math4() */

static SEXP math4_1 (SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, 
              double (*f)(double, double, double, double, int), SEXP lcall)
{
    double *a, *b, *c, *d, *y;
    int n, na, nb, nc, nd;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc); nd = LENGTH(sd);
    setup_Math4 (&sa, &sb, &sc, &sd, &sy, na, nb, nc, nd, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); d = REAL(sd); y = REAL(sy);

    int i_1 = asInteger(sI);

    DO_MATH4(y,a,b,c,d,n,na,nb,nc,nd, f(ai,bi,ci,di,i_1));

    return sy;
} /* math4_1() */

static SEXP math4_2 (SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
              double (*f)(double, double, double, double, int, int), SEXP lcall)
{
    double *a, *b, *c, *d, *y;
    int n, na, nb, nc, nd;
    SEXP sy;

    na = LENGTH(sa); nb = LENGTH(sb); nc = LENGTH(sc); nd = LENGTH(sd);
    setup_Math4 (&sa, &sb, &sc, &sd, &sy, na, nb, nc, nd, lcall);
    if ((n = LENGTH(sy)) == 0) return sy;
    a = REAL(sa); b = REAL(sb); c = REAL(sc); d = REAL(sd); y = REAL(sy);

    int i_1 = asInteger(sI);
    int i_2 = asInteger(sJ);

    DO_MATH4(y,a,b,c,d,n,na,nb,nc,nd, f(ai,bi,ci,di,i_1,i_2));

    return sy;
} /* math4_2() */


#define CAD3R	CADDDR
/* This is not (yet) in Rinternals.h : */
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))

#define Math4(A, FUN)   math4  (CAR(A), CADR(A), CADDR(A), CAD3R(A), FUN, call)
#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN, call)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN, call)


SEXP do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- math4() at all! : */
    case -1: return Math4(args, (double (*)(double, double, double, double))NULL);

    case  1: return Math4_1(args, dhyper);
    case  2: return Math4_2(args, phyper);
    case  3: return Math4_2(args, qhyper);

    case  4: return Math4_1(args, dnbeta);
    case  5: return Math4_2(args, pnbeta);
    case  6: return Math4_2(args, qnbeta);
    case  7: return Math4_1(args, dnf);
    case  8: return Math4_2(args, pnf);
    case  9: return Math4_2(args, qnf);
#ifdef UNIMP
    case 10: return Math4_1(args, dtukey);
#endif
    case 11: return Math4_2(args, ptukey);
    case 12: return Math4_2(args, qtukey);
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 4);
    }
}

#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if     (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e))	\
		y = NA_REAL;						\
	else if(ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

#define mod_iterate5(n1,n2,n3,n4,n5, i1,i2,i3,i4,i5)	\
 for (i=i1=i2=i3=i4=i5=0; i<n;				\
	i1 = (++i1==n1) ? 0 : i1,			\
	i2 = (++i2==n2) ? 0 : i2,			\
	i3 = (++i3==n3) ? 0 : i3,			\
	i4 = (++i4==n4) ? 0 : i4,			\
	i5 = (++i5==n5) ? 0 : i5,			\
	++i)

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	non_numeric_errorcall(lcall);				\
									\
    na = LENGTH(sa);							\
    nb = LENGTH(sb);							\
    nc = LENGTH(sc);							\
    nd = LENGTH(sd);							\
    ne = LENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))	\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    e = REAL(se);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math5;

    mod_iterate5 (na, nb, nc, nd, ne,
		  ia, ib, ic, id, ie) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = f(ai, bi, ci, di, ei);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math5				\
    if(naflag) NaN_warning();			\
						\
    SEXP frm = n==na ? sa : n==nb ? sb : n==nc ? sc : n==nd ? sd : se; \
    DUPLICATE_ATTRIB(sy, frm); \
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 5);
    }
} /* do_math5() */

#endif /* Math5 is there */


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_arithmetic[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"signif",	do_Math2,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log",		do_log,		10003,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log10",	do_log1arg,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log2",	do_log1arg,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"abs",		do_abs,		6,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"floor",	do_math1,	1,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ceiling",	do_math1,	2,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sqrt",	do_math1,	3,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sign",	do_math1,	4,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trunc",	do_trunc,	5,	1001,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"exp",		do_math1,	10,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expm1",	do_math1,	11,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log1p",	do_math1,	12,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cos",		do_math1,	20,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sin",		do_math1,	21,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tan",		do_math1,	22,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acos",	do_math1,	23,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asin",	do_math1,	24,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atan",	do_math1,	25,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cosh",	do_math1,	30,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinh",	do_math1,	31,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanh",	do_math1,	32,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acosh",	do_math1,	33,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asinh",	do_math1,	34,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atanh",	do_math1,	35,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lgamma",	do_math1,	40,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gamma",	do_math1,	41,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"digamma",	do_math1,	42,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trigamma",	do_math1,	43,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},
/* see "psigamma" below !*/

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	0,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	2,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	3,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	4,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	5,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"dchisq",	do_math2,	6,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2,	7,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2,	8,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2,	9,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2,	10,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2,	11,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2,	12,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2,	13,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2,	14,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2,	15,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2,	16,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2,	17,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2,	18,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2,	19,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2,	20,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2,	21,   1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2,	22,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2,	23,   1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2,	24,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	25,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	26,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	1,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3,	2,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3,	3,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3,	4,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3,	5,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3,	6,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3,	7,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3,	8,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3,	9,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3,	10,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3,	11,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3,	12,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3,	13,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3,	14,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3,	15,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3,	16,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3,	17,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3,	18,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3,	19,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3,	20,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3,	21,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3,	22,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3,	23,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3,	24,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3,	25,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3,	26,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3,	27,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3,	28,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3,	29,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3,	30,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3,	31,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3,	32,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3,	33,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnchisq",	do_math3,	34,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnchisq",	do_math3,	35,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnchisq",	do_math3,	36,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnt",		do_math3,	37,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnt",		do_math3,	38,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnt",		do_math3,	39,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dwilcox",	do_math3,	40,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pwilcox",	do_math3,	41,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qwilcox",	do_math3,	42,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselI",	do_math3,	43,   1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3,	44,   1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom_mu",	do_math3,	45,   1000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3,	46,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom_mu",	do_math3,	47,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4,	1,   1000011,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"phyper",	do_math4,	2,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qhyper",	do_math4,	3,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbeta",	do_math4,	4,   1000011,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbeta",	do_math4,	5,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbeta",	do_math4,	6,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnf",		do_math4,	7,   1000011,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnf",		do_math4,	8,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnf",		do_math4,	9,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dtukey",	do_math4,	10,   1000011,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ptukey",	do_math4,	11,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qtukey",	do_math4,	12,   1000011,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};

/* Fast built-in functions in this file. See names.c for documentation */

attribute_hidden FASTFUNTAB R_FastFunTab_arithmetic[] = {
/*slow func	fast func,     code or -1   dsptch  variant */
{ do_math1,	do_fast_math1,	-1,             1,  VARIANT_SCALAR_STACK_OK|VARIANT_PENDING_OK },
{ do_trunc,	do_fast_trunc,	-1,		1,  VARIANT_SCALAR_STACK_OK|VARIANT_PENDING_OK },
{ do_abs,	do_fast_abs,	-1,		1,  VARIANT_SCALAR_STACK_OK|VARIANT_PENDING_OK },
{ 0,		0,		0,		0,  0 }
};
