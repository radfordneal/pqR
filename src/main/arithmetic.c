/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013-2020 by Radford M. Neal
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
    if (ATTRIB_W((_to_))!=ATTRIB_W((_from_)) || OBJECT((_from_))) \
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


/* Support for arithmetic operations on lists. */

static SEXP unary_listop (SEXP s1, int obj1, SEXP grad1, SEXP env, int variant,
                          const char *listopname)
{
    SEXP call, ans;
    s1 = isObject(s1) && !obj1 ? Rf_makeUnclassed(s1) : s1;
    PROTECT (call = LCONS (install(listopname),
                           CONS (mkPROMISE(s1,R_EmptyEnv), R_NilValue)));
    if (variant & VARIANT_GRADIENT)
    { /* .... */
    }
    ans = eval(call,env);
    UNPROTECT(1);
    return ans;
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
    if (base == 10)
        return x > 0 ? log10(x) : x < 0 ? R_NaN : R_NegInf;

    if (base == 2)
        return x > 0 ? log2(x) : x < 0 ? R_NaN : R_NegInf;

    return R_log(x) / R_log(base);
}

static double R_log2 (double x) 
{
    return x > 0 ? log2(x) : x < 0 ? R_NaN : R_NegInf;
}

static double R_log10 (double x) 
{
    return x > 0 ? log10(x) : x < 0 ? R_NaN : R_NegInf;
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
       swp      if 1, ops have been swapped so that n1 <= n2.

   Both n1 and n2 must be non-zero and no bigger than n, and at least one 
   of n1 and n2 must be equal to n.  An operand of length one or with length 
   less than n is assumed to already be available with no waiting.
*/

#define PIPEARITH(type,func,result,n,fetch1,s1,n1,fetch2,s2,n2,swp) \
    do { \
        R_len_t i, i1, i2, a, a1, a2; \
        i = 0; \
        if (!swp && n2 == 1) { \
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
        else if (!swp && n1 > n2) { \
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

#define MM_PIPEARITH(func,result,n,s1,n1,s2,n2,swp) \
          PIPEARITH(double,func,result,n,RFETCH,s1,n1,RFETCH,s2,n2,swp)

#else

#include <immintrin.h>

#define MM_PIPEARITH(func,result,n,s1,n1,s2,n2,swp) \
    do { \
        R_len_t i, i1, i2, a, a1, a2; \
        i = 0; \
        if (!swp && n2 == 1) { \
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
                    while (i <= u-7) { \
                        __m256d res_pd_1; \
                        res_pd_1 = _mm256_load_pd (REAL(s1)+i); \
                        res_pd_1 = func ## _mm (res_pd_1, tmp_pd); \
                        _mm256_store_pd (result+i, res_pd_1); \
                        __m256d res_pd_2; \
                        res_pd_2 = _mm256_load_pd (REAL(s1)+i+4); \
                        res_pd_2 = func ## _mm (res_pd_2, tmp_pd); \
                        _mm256_store_pd (result+i+4, res_pd_2); \
                        i += 8; \
                    } \
                    if (i <= u-3) { \
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
                    while (i <= u-8) { \
                        __m256d res_pd_1; \
                        res_pd_1 = _mm256_load_pd (REAL(s2)+i); \
                        res_pd_1 = func ## _mm (tmp_pd, res_pd_1); \
                        _mm256_store_pd (result+i, res_pd_1); \
                        __m256d res_pd_2; \
                        res_pd_2 = _mm256_load_pd (REAL(s2)+i+4); \
                        res_pd_2 = func ## _mm (tmp_pd, res_pd_2); \
                        _mm256_store_pd (result+i+4, res_pd_2); \
                        i += 8; \
                    } \
                    if (i <= u-3) { \
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
                    while (i <= u-7) { \
                        __m256d res_pd_1, op2_pd_1; \
                        res_pd_1 = _mm256_load_pd (REAL(s1)+i); \
                        op2_pd_1 = _mm256_load_pd (REAL(s2)+i); \
                        res_pd_1 = func ## _mm (res_pd_1, op2_pd_1); \
                        _mm256_store_pd (result+i, res_pd_1); \
                        __m256d res_pd_2, op2_pd_2; \
                        res_pd_2 = _mm256_load_pd (REAL(s1)+i+4); \
                        op2_pd_2 = _mm256_load_pd (REAL(s2)+i+4); \
                        res_pd_2 = func ## _mm (res_pd_2, op2_pd_2); \
                        _mm256_store_pd (result+i+4, res_pd_2); \
                        i += 8; \
                    } \
                    if (i <= u-3) { \
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
        else if (!swp && n1 > n2) { \
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

    R_len_t n, n1, n2;
    int x1, x2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    switch (code) {
    case PLUSOP:
        if (n1 > n2) abort();
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
        if (n1 > n2) abort();
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
                ians[i] = 0 /* this is slower */ && x1 >= 0 && x2 >= 0 ? x1 % x2
                        : (int) (x1 - x2 * floor ((double)x1 / (double)x2));
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
                ians[i] = 0 /* this is slower */ && x1 >= 0 && x2 >= 0 ? x1 / x2
                        : (int) floor ((double)x1 / (double)x2);
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
            PIPEARITH(double,add_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,1);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,add_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,1);
        else
            MM_PIPEARITH(add_func,rans,n,s1,n1,s2,n2,1);
        break;
    case MINUSOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,sub_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,0);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,sub_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,0);
        else
            MM_PIPEARITH(sub_func,rans,n,s1,n1,s2,n2,0);
        break;
    case TIMESOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,mul_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,1);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,mul_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,1);
        else
            MM_PIPEARITH(mul_func,rans,n,s1,n1,s2,n2,1);
        break;
    case DIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,div_func,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,0);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,div_func,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,0);
        else
            MM_PIPEARITH(div_func,rans,n,s1,n1,s2,n2,0);
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
            PIPEARITH(double,R_POW,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,0);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,R_POW,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,0);
        else
            PIPEARITH(double,R_POW,rans,n,RFETCH,s1,n1,RFETCH,s2,n2,0);
        break;
    case MODOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfmod,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,0);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfmod,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,0);
        else
            PIPEARITH(double,myfmod,rans,n,RFETCH,s1,n1,RFETCH,s2,n2,0);
        break;
    case IDIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfloor,rans,n,RIFETCH,s1,n1,RFETCH,s2,n2,0);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfloor,rans,n,RFETCH,s1,n1,RIFETCH,s2,n2,0);
        else
            PIPEARITH(double,myfloor,rans,n,RFETCH,s1,n1,RFETCH,s2,n2,0);
        break;
    }
}

extern double complex R_cpow (double complex, double complex);

void task_complex_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    R_len_t n, n1, n2;

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

#define T_arithmetic THRESHOLD_ADJUST(500)  /* >= 16, further adjusted below */

SEXP attribute_hidden R_binary (SEXP call, int opcode, SEXP x, SEXP y, 
                                int objx, int objy, SEXP grad1, SEXP grad2,
                                SEXP env, int variant)
{
    helpers_task_proc *task;
    SEXP klass, dims, tsp, xnames, ynames, ans;
    int mismatch = 0, nx, ny, n, xarray, yarray, xts, yts, xS4 = 0, yS4 = 0;
    int xattr, yattr;
    PROTECT_INDEX xpi, ypi;
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
        xS4 = objx && IS_S4_OBJECT(x);
    }
    else xarray = xts = xattr = FALSE;
    ny = LENGTH(y);
    if (HAS_ATTRIB(y)) {
        yattr = TRUE;
        yarray = isArray(y);
        yts = isTs(y);
        yS4 = objy && IS_S4_OBJECT(y);
    }
    else yarray = yts = yattr = FALSE;

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

    if (grad1 == R_NilValue && grad2 == R_NilValue) {
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
    }
    
    if (nx == 1) WAIT_UNTIL_COMPUTED(x);
    if (ny == 1) WAIT_UNTIL_COMPUTED(y);

    int swap_ops = FALSE;  /* TRUE if ops will be swapped so first is shorter */

    int replace_by_half = FALSE;  /* TRUE if x/2 will be replaced by 0.5*x;
                                     if TRUE, swap_ops will be TRUE as well */

    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {

        if (opcode==IDIVOP || opcode==MODOP)
            errorcall(call,_("unimplemented complex operation"));
        ans = alloc_or_reuse (x, y, CPLXSXP, n, local_assign1, local_assign2);
        task = task_complex_arithmetic;
        flags = 0;  /* Not bothering with pipelining yet. */
    }

    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {

         /* task_real_arithmetic takes REAL, INTEGER, and LOGICAL operands, 
            and assumes INTEGER and LOGICAL are really the same. */

        if ((grad1 != R_NilValue || grad2 != R_NilValue)
               && (opcode != PLUSOP && opcode != MINUSOP)) 
            /* can't reuse operand space because needed for gradient calc. */
            ans = allocVector (REALSXP, n);
        else
            ans = alloc_or_reuse (x, y, REALSXP, n, 
                                  local_assign1, local_assign2);
        task = task_real_arithmetic;
        flags = HELPERS_PIPE_IN0_OUT;

        if (n > 1) {

            if (opcode == DIVOP && ny == 1 && (TYPEOF(y)==REALSXP ? REAL(y)[0]
                                                      : INTEGER(y)[0]) == 2.0) {
                opcode = TIMESOP;
                replace_by_half = TRUE;
            }

            if (TYPEOF(x)==REALSXP && TYPEOF(y)==REALSXP) { /*see if may merge*/

                if (nx==1 || ny==1) {  /* operation on scalar and vector */
                    if (opcode < DIVOP  /* this is the +, -, and * operators */
                     || opcode == POWOP && ny==1 && REAL(y)[0]==2.0 /* square*/)
                        flags = HELPERS_PIPE_IN0_OUT | HELPERS_MERGE_IN_OUT 
                                                     | HELPERS_HOLD;
                    else if (opcode == DIVOP) /* only allowed as last in merge*/
                        flags = HELPERS_PIPE_IN0_OUT | HELPERS_MERGE_IN;
                }

                if (nx == ny && opcode <= TIMESOP) {  /* op on eq-len vectors */
                    /* need first op to not be being computed for merging */
                    if (!helpers_is_being_computed(x))
                        flags = HELPERS_PIPE_IN02_OUT | HELPERS_MERGE_OUT 
                                                      | HELPERS_HOLD;
                    else if (!helpers_is_being_computed(y)
                               && opcode != MINUSOP) {
                        flags = HELPERS_PIPE_IN02_OUT | HELPERS_MERGE_OUT
                                                      | HELPERS_HOLD;
                        swap_ops = TRUE;
                    }
                }
            }

            if (ny<nx && (opcode == PLUSOP || opcode == TIMESOP)) {
                swap_ops = TRUE;
                flags |= HELPERS_PIPE_IN2;
            }
            else {
                if (nx==n) flags |= HELPERS_PIPE_IN1;
                if (ny==n) flags |= HELPERS_PIPE_IN2;
            }
        }
    }
    else {

        /* task_integer_arithmetic is assumed to work for LOGICAL too, though
           this won't be true if they aren't really the same */

        if (opcode == DIVOP || opcode == POWOP)
            ans = allocVector(REALSXP, n);
        else
            ans = alloc_or_reuse(x, y, INTSXP, n, local_assign1, local_assign2);
        task = task_integer_arithmetic;

       /* Only ^, /, %/%, and %% can be done in helpers at present - others
          must be in the master because of possible integer overflow.
          Not bothering with pipelining yet. */

        flags = 
          opcode==POWOP || opcode==DIVOP || opcode==IDIVOP || opcode==MODOP ? 0 
            : HELPERS_MASTER_NOW;

        if (ny<nx && (opcode == PLUSOP || opcode == TIMESOP))
            swap_ops = TRUE;
    }

    if (isObject(ans) && !objx && !objy) 
        ans = allocVector (TYPEOF(ans), n);

    if (ans != x) local_assign1 = 0;
    if (ans != y) local_assign2 = 0;

    PROTECT(ans);
    nprotect++;

    /* Do the actual operation. */

    double xval1, yval1;  /* may be needed for gradient */

    if (n!=0) {

        xval1 = TYPEOF(x) == REALSXP ? *REAL(x) : *INTEGER(x);
        yval1 = TYPEOF(y) == REALSXP ? *REAL(y) : *INTEGER(y);

        threshold = T_arithmetic;
        if (TYPEOF(ans) == CPLXSXP)
            threshold >>= 2;
        if (opcode > TIMESOP)
            threshold >>= 2;

        SEXP xx = x, yy = y;

        if (swap_ops) { 
            xx = y; yy = x;
            if (replace_by_half)
                xx = R_ScalarRealHalf;
        }

        if (n >= threshold && (variant & VARIANT_PENDING_OK)) {
            if (ON_SCALAR_STACK(xx) && ON_SCALAR_STACK(yy)) {
                PROTECT(xx = duplicate(xx));
                yy = duplicate(yy);
                UNPROTECT(1);
            }
            else if (ON_SCALAR_STACK(xx)) xx = duplicate(xx);
            else if (ON_SCALAR_STACK(yy)) yy = duplicate(yy);
        }

        integer_overflow = 0;

        DO_NOW_OR_LATER2 (variant, n>=threshold, flags, 
                          task, opcode, ans, xx, yy);

        if (integer_overflow)
            warningcall(call, _("NAs produced by integer overflow"));
    }

    /* Copy attributes from arguments as needed. */

    if ((xattr || yattr) && (objx || objy || !(variant & VARIANT_ANY_ATTR))) {

        if (yattr && ny==n && ans!=y)
            objy ? copyMostAttrib(y, ans) : copyMostAttribNoClass(y, ans);
        if (xattr && nx==n && ans!=x) /* Done 2nd so x's attrs overwrite y's */
            objx ? copyMostAttrib(x, ans) : copyMostAttribNoClass(x, ans);
    
        if (dims != R_NilValue) {
            /* With zero-length result, dims needn't match */
            if (dims != R_NilValue && n == 0) {
                int i;
                for (i = 0; i < LENGTH(dims); i++) 
                    if (INTEGER(dims)[i] == 0) break;
                if (i == LENGTH(dims)) /* none zero */
                    dims = R_NilValue;
            }
            setAttrib(ans, R_DimSymbol, dims);
            if (xnames != R_NilValue)
                setAttrib(ans, R_DimNamesSymbol, xnames);
            else if (ynames != R_NilValue)
                setAttrib(ans, R_DimNamesSymbol, ynames);
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

    /* Handle gradients. */

    if (TYPEOF(ans)==REALSXP && (grad1 != R_NilValue || grad2 != R_NilValue)) {

        GRADIENT_TRACE(call);
        WAIT_UNTIL_COMPUTED_2(x,y);

        SEXP res_grad = R_NilValue;

        if (n == 1) {

            switch (opcode) {
            case PLUSOP: 
                if (grad1 == R_NilValue)
                    res_grad = grad2;
                else if (grad2 == R_NilValue)
                    res_grad = grad1;
                else
                    res_grad = add_scaled_gradients (grad1, grad2, 1.0, 1);
                break;
            case MINUSOP: 
                if (grad1 == R_NilValue)
                    res_grad = copy_scaled_gradients (grad2, -1.0, 1);
                else if (grad2 == R_NilValue)
                    res_grad = grad1;
                else
                    res_grad = add_scaled_gradients (grad1, grad2, -1.0, 1);
                break;
            case TIMESOP: 
                if (grad1 == R_NilValue)
                    res_grad = copy_scaled_gradients (grad2, xval1, 1);
                else if (grad2 == R_NilValue)
                    res_grad = copy_scaled_gradients (grad1, yval1, 1);
                else
                    res_grad = add_scaled_gradients (
                                   copy_scaled_gradients (grad1, yval1, 1),
                                   grad2, xval1, 1);
                break;
            case DIVOP: 
                if (grad1 == R_NilValue)
                    res_grad = copy_scaled_gradients
                                 (grad2, -xval1/(yval1*yval1), 1);
                else if (grad2 == R_NilValue)
                    res_grad = copy_scaled_gradients (grad1, 1/yval1, 1);
                else
                    res_grad = add_scaled_gradients (
                                   copy_scaled_gradients (grad1, 1/yval1, 1),
                                   grad2, -xval1/(yval1*yval1), 1);
                break;
            case POWOP: ;
                double aval1 = *REAL(ans);
                if (aval1 != 0) {
                    if (grad1 != R_NilValue)
                        res_grad = copy_scaled_gradients 
                                    (grad1, aval1*yval1/xval1, 1);
                    if (grad2 != R_NilValue && xval1 > 0)
                        res_grad = add_scaled_gradients 
                                    (res_grad, grad2, aval1*log(xval1), 1);
                }
                break;
            }
        }

        else {

            R_len_t i;
            SEXP f;

            switch (opcode) {
            case PLUSOP: 
                if (grad1 == R_NilValue)
                    res_grad = grad2;
                else if (grad2 == R_NilValue)
                    res_grad = grad1;
                else
                    res_grad = add_scaled_gradients (grad1, grad2, 1.0, n);
                break;
            case MINUSOP: 
                if (grad1 == R_NilValue)
                    res_grad = copy_scaled_gradients (grad2, -1.0, n);
                else if (grad2 == R_NilValue)
                    res_grad = grad1;
                else
                    res_grad = add_scaled_gradients (grad1, grad2, -1.0, n);
                break;
            case TIMESOP: 
                if (grad1 == R_NilValue) {
                    if (TYPEOF(x) != REALSXP) {
                        f = allocVector (REALSXP, n);
                        for (i = 0; i < n; i++) REAL(f)[i] = INTEGER(x)[i%nx];
                    }
                    else
                        f = x;
                    PROTECT(f);
                    res_grad = copy_scaled_gradients_vec (grad2, f, n);
                    UNPROTECT(1);
                }
                else if (grad2 == R_NilValue) {
                    if (TYPEOF(y) != REALSXP) {
                        f = allocVector (REALSXP, n);
                        for (i = 0; i < n; i++) REAL(f)[i] = INTEGER(y)[i%ny];
                    }
                    else
                        f = y;
                    PROTECT(f);
                    res_grad = copy_scaled_gradients_vec (grad1, f, n);
                    UNPROTECT(1);
                }
                else {
                    if (TYPEOF(y) != REALSXP) {
                        f = allocVector (REALSXP, n);
                        for (i = 0; i < n; i++) REAL(f)[i] = INTEGER(y)[i%ny];
                    }
                    else
                        f = y;
                    PROTECT(f);
                    res_grad = copy_scaled_gradients_vec (grad1, f, n);
                    UNPROTECT(1);
                    if (TYPEOF(x) != REALSXP) {
                        if (f == y) f = allocVector (REALSXP, n);
                        for (i = 0; i < n; i++) REAL(f)[i] = INTEGER(x)[i%nx];
                    }
                    else 
                        f = x;
                    PROTECT(f);
                    res_grad = add_scaled_gradients_vec (res_grad, grad2, f, n);
                    UNPROTECT(1);
                }
                break;
            case DIVOP: 
                PROTECT (f = allocVector (REALSXP, n));
                if (grad1 == R_NilValue) {
                    for (i = 0; i < n; i++) {
                        double tx = TYPEOF(x) == REALSXP ? REAL(x)[i%nx]
                                                         : INTEGER(x)[i%nx];
                        double ty = TYPEOF(y) == REALSXP ? REAL(y)[i%ny]
                                                         : INTEGER(y)[i%ny];
                        REAL(f)[i] = -tx / (ty*ty);
                    }
                    res_grad = copy_scaled_gradients_vec (grad2, f, n);
                }
                else if (grad2 == R_NilValue) {
                    for (i = 0; i < n; i++) {
                        double ty = TYPEOF(y) == REALSXP ? REAL(y)[i%ny]
                                                         : INTEGER(y)[i%ny];
                        REAL(f)[i] = 1 / ty;
                    }
                    res_grad = copy_scaled_gradients_vec (grad1, f, n);
                }
                else {
                    for (i = 0; i < n; i++) {
                        double ty = TYPEOF(y) == REALSXP ? REAL(y)[i%ny]
                                                         : INTEGER(y)[i%ny];
                        REAL(f)[i] = 1 / REAL(y)[i%ny];
                    }
                    res_grad = copy_scaled_gradients_vec (grad1, f, n);
                    for (i = 0; i < n; i++) {
                        double tx = TYPEOF(x) == REALSXP ? REAL(x)[i%nx]
                                                         : INTEGER(x)[i%nx];
                        double ty = TYPEOF(y) == REALSXP ? REAL(y)[i%ny]
                                                         : INTEGER(y)[i%ny];
                        REAL(f)[i] = -tx / (ty*ty);
                    }
                    res_grad = add_scaled_gradients_vec(res_grad, grad2, f, n);
                }
                UNPROTECT(1);
                break;
            case POWOP: ;
                PROTECT (f = allocVector (REALSXP, n));
                WAIT_UNTIL_COMPUTED(ans);
                if (grad1 != R_NilValue) {
                    for (i = 0; i < n; i++) {
                        double tx = TYPEOF(x) == REALSXP ? REAL(x)[i%nx]
                                                         : INTEGER(x)[i%nx];
                        double ty = TYPEOF(y) == REALSXP ? REAL(y)[i%ny]
                                                         : INTEGER(y)[i%ny];
                        REAL(f)[i] = REAL(ans)[i] * ty / tx;
                    }
                    res_grad = copy_scaled_gradients_vec (grad1, f, n);
                }
                if (grad2 != R_NilValue) {
                    for (i = 0; i < n; i++) {
                        double tx = TYPEOF(x) == REALSXP ? REAL(x)[i%nx]
                                                         : INTEGER(x)[i%nx];
                        REAL(f)[i] = tx > 0 ? REAL(ans)[i] * log(tx) : 0;
                    }
                    res_grad = add_scaled_gradients_vec (res_grad, grad2, f, n);
                }
                UNPROTECT(1);
                break;
            }
        }

        if (res_grad != R_NilValue) {
            R_gradient = res_grad;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }
    }

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

#define T_unary_minus THRESHOLD_ADJUST(500)

SEXP attribute_hidden R_unary (SEXP call, int opcode, SEXP s1, int obj1,
                               SEXP grad1, SEXP env, int variant)
{
    int type = TYPEOF(s1);
    int local_assign = 0;
    SEXP ans;
    int n;

    if (opcode != MINUSOP && opcode != PLUSOP)
        errorcall(call, _("invalid unary operator"));

    if ( ! ((NUMBER_TYPES >> type) & 1)) {
        if (type == VECSXP)
            return unary_listop (s1, obj1, grad1, env, variant,
                                 opcode==PLUSOP ? "+.list" : "-.list");
        errorcall(call, _("invalid argument to unary operator"));
    }

    n = LENGTH(s1);

    if (type == LGLSXP) {
        SEXP dim, dimnames, names;
        ans = allocVector(INTSXP,n);
        PROTECT (names    = getAttrib (s1, R_NamesSymbol));
        PROTECT (dim      = getDimAttrib(s1));
        PROTECT (dimnames = getAttrib (s1, R_DimNamesSymbol));
        if (names    != R_NilValue) setAttrib(ans,R_NamesSymbol,    names);
        if (dim      != R_NilValue) setAttrib(ans,R_DimSymbol,      dim);
        if (dimnames != R_NilValue) setAttrib(ans,R_DimNamesSymbol, dimnames);
        UNPROTECT(3);
    }

    if (opcode==PLUSOP) {
        if (type != LGLSXP)
            ans = isObject(s1) && !obj1 ? Rf_makeUnclassed(s1) : s1;
        else {
            WAIT_UNTIL_COMPUTED(s1);
            for (int i = 0; i<LENGTH(s1); i++) INTEGER(ans)[i] = LOGICAL(s1)[i];
        }
    }
    else { /* opcode==MINUSOP */
        if (type == LGLSXP) 
            ; /* allocated above */
        else if (isObject(s1) && !obj1)
            ans = Rf_makeUnclassed(s1);
        else {
            if (VARIANT_KIND(variant) == VARIANT_LOCAL_ASSIGN1
              && !NAMEDCNT_GT_1(s1) && grad1 == R_NilValue
              && s1 == findVarInFrame3(env,CADR(call),7))
                local_assign = 1;
            ans = local_assign || NAMEDCNT_EQ_0(s1) ? s1 : duplicate(s1);
        }
        DO_NOW_OR_LATER1 (variant, n >= T_unary_minus,
          TYPEOF(s1)==REALSXP ? HELPERS_PIPE_IN01_OUT | HELPERS_MERGE_IN_OUT
                              : HELPERS_PIPE_IN01_OUT,
          task_unary_minus, 0, ans, s1);
    }

    R_variant_result = local_assign;  /* do at end, just in case */

    if (grad1 != R_NilValue) {

        GRADIENT_TRACE(call);

        double d = opcode == MINUSOP ? -1 : 1;
        SEXP res_grad = R_NilValue;

        if (n == 1) {
            if (!ISNAN(*REAL(ans))) {
                res_grad = copy_scaled_gradients (grad1, d, 1);
            }
        }
        else {
            res_grad = copy_scaled_gradients (grad1, d, n);
        }

        if (res_grad != R_NilValue) {
            R_gradient = res_grad;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }
    }

    return ans;
}


/* MATHEMATICAL FUNCTIONS OF ONE ARGUMENT.  Implements a variant return
   of the sum of the vector result, rather than the vector itself. */

/* Table to map math1 operation code to function.  The entries for trunc,
   R_log, R_log2, and R_log10 are not called via do_math1 like the others, 
   but from special primitives.  The entry for fabs is currently not used, 
   since the compiler may be able to inline fabs when it's called directly 
   from task_abs. */

double (* const R_math1_func_table[44])(double) = {
        /*      0       1       2       3       4       5       6 7 8 9 */
/* 00 */        fabs,   floor,  ceil,   sqrt,   sign,   trunc,  0,0,0,0,
/* 10 */        exp,    expm1,  log1p,  R_log,  R_log2, R_log10,0,0,0,0,
/* 20 */        cos,    sin,    tan,    acos,   asin,   atan,   0,0,0,0,
/* 30 */        cosh,   sinh,   tanh,   acosh,  asinh,  atanh,  0,0,0,0,
/* 40 */      lgammafn, gammafn,digamma,trigamma
};

/* Table of flags saying when an operation may produce NA/NaN or warning:

       0:  NA/NaN only when argument is NA/NaN
       1:  NA/NaN only when argument is NA/Nan or +-Inf
       2:  NA/NaN possible for finite argument as well as NA/NaN or +- Inf
       3:  may produce warning message, not just NA/NaN

   Entries correspond to those in R_math1_func_table above. */

const char R_math1_err_table[44] = {
        /*      0       1       2       3       4       5       6 7 8 9 */
/* 00 */        0,      0,      0,      2,      0,      0,      0,0,0,0,
/* 10 */        0,      0,      2,      2,      2,      2,      0,0,0,0,
/* 20 */        1,      1,      1,      2,      2,      0,      0,0,0,0,
/* 30 */        0,      0,      0,      2,      0,      2,      0,0,0,0,
/* 40 */        3,      3,      2,      2
};

/* Table of list methods for math1 operations.

   Entries correspond to those in R_math1_func_table above. */

static const char *math1_list_method[44] = {
/*         0            1            2            3            4            5 */
 "abs.list", "floor.list","ceiling.list","sqrt.list","sign.list","trunc.list",
 0,0,0,0,
 "exp.list","expm1.list","lop1p.list",  "log.list", "log2.list","log10.list",
 0,0,0,0,
 "cos.list",  "sin.list",  "tan.list", "acos.list", "asin.list", "atan.list",
 0,0,0,0,
"cosh.list", "sinh.list", "tanh.list","acosh.list","asinh.list","atanh.list",
 0,0,0,0,
"lgamma.list","gamma.list","digamma.list","trigamma.list"
/*         0            1            2            3            4            5 */
};

int R_naflag;  /* Set to one (in master) for the "NAs produced" warning */

static double Dfabs (double x, double y)
{ 
    return x>=0 ? 1 : -1;
}

static double Dsqrt (double x, double y)
{ 
    return 0.5/y;
}

static double Dexp (double x, double y)
{ 
    return y;
}

static double Dexpm1 (double x, double y)
{ 
    return y+1;
}

static double Dlog1p (double x, double y)
{ 
    return 1+x < 0 ? NA_REAL : 1/(1+x);
}

static double Dlog (double x, double y)
{ 
    return x < 0 ? NA_REAL : 1/x;
}

static double Dlog2 (double x, double y)
{ 
    return x < 0 ? NA_REAL : 1.4426950408889634073599246810 / x;
}

static double Dlog10 (double x, double y)
{ 
    return x < 0 ? NA_REAL : 0.4342944819032518276511289189 / x;
}

static double Dcos (double x, double y)
{ 
    return -sin(x);
}

static double Dsin (double x, double y)
{ 
    return cos(x);
}

static double Dtan (double x, double y)
{ 
    double a = cos(x);
    return 1 / (a*a);
}

static double Dacos (double x, double y)
{ 
    return (-1) / sqrt (1 - x*x);
}

static double Dasin (double x, double y)
{ 
    return 1 / sqrt (1 - x*x);
}

static double Datan (double x, double y)
{ 
    return 1 / (1 + x*x);
}

static double Dcosh (double x, double y)
{ 
    return sinh(x);
}

static double Dsinh (double x, double y)
{ 
    return cosh(x);
}

static double Dtanh (double x, double y)
{ 
    double a = cosh(x);
    return 1 / (a*a);
}

static double Dacosh (double x, double y)
{ 
    return 1 / sqrt (x*x - 1);
}

static double Dasinh (double x, double y)
{ 
    return 1 / sqrt (x*x + 1);
}

static double Datanh (double x, double y)
{ 
    return 1 / (1 - x*x);
}

static double Dlgammafn (double x, double y)
{ 
    return digamma(x);
}

static double Dgammafn (double x, double y)
{ 
    return y * digamma(x);
}

static double Ddigamma (double x, double y)
{ 
    return trigamma(x);
}

static double Dtrigamma (double x, double y)
{ 
    return psigamma(x,2);
}

/* Table to map math1 operation code to derivative function. */

double (* const R_math1_deriv_table[44])(double,double) = {
        /*      0       1       2       3       4       5       6 7 8 9 */
/* 00 */        Dfabs,  0,      0,      Dsqrt,  0,      0,      0,0,0,0,
/* 10 */        Dexp,   Dexpm1, Dlog1p, Dlog,   Dlog2,  Dlog10, 0,0,0,0,
/* 20 */        Dcos,   Dsin,   Dtan,   Dacos,  Dasin,  Datan,  0,0,0,0,
/* 30 */        Dcosh,  Dsinh,  Dtanh,  Dacosh, Dasinh, Datanh, 0,0,0,0,
/* 40 */     Dlgammafn, Dgammafn, Ddigamma, Dtrigamma
};

/* Math1 task procedures.  The opcode has the function code in the low
   byte, and (except for the sum task) and indication of which parallel
   part to do above that. */

void task_math1 (helpers_op_t opcode, SEXP sy, SEXP sa, SEXP ignored)
{
    int op = opcode & 0xff;
    int par = opcode >> 8;
    double (*f)(double) = R_math1_func_table[op];

    R_len_t n = LENGTH(sa);
    double *ra = REAL(sa);
    double *ry = REAL(sy);

    R_len_t i, a;

    if (R_math1_err_table[op] > 1) {

        /* Code for when a warning is possible, only done in master. */

        i = 0;
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
                i += 1;
            } while (i < a);
        }
    }

    else if (par == 0) {

        /* Code for only one task for whole vector, may be done in helper. */

        HELPERS_SETUP_OUT(5);

        i = 0;
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

    else if (par == 1) {  

        /* Code for task that does first half of vector. */

        R_len_t halfn = n >> 1;

        HELPERS_SETUP_OUT(5);

        i = 0;
        while (i < halfn) {
            HELPERS_WAIT_IN1 (a, i, n);
            if (a > halfn) a = halfn;
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else
                    ry[i] = f(ra[i]);
                HELPERS_NEXT_OUT(i);
            } while (i < a);
        }

        /* Must wait for task doing second half to finish before this
           task finishes. */

        HELPERS_WAIT_IN0 (a, n-1, n);
    }

    else {

        /* Code for task that does second half of vector.  Doesn't
           bother with output pipelining since it likely will finish
           before the first half has been computed.  (But needs to be
           scheduled with HELPERS_PIPE_OUT so task for first half will
           be able to start. */

        i = n >> 1;
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else
                    ry[i] = f(ra[i]);
                i += 1;
            } while (i < a);
        }
    }
}

void task_sum_math1 (helpers_op_t opcode, SEXP sy, SEXP sa, SEXP ignored)
{
    int op = opcode;
    double (*f)(double) = R_math1_func_table[op];

    R_len_t n = LENGTH(sa);
    double *ra = REAL(sa);

    long double s = 0.0;
    R_len_t i = 0;
    R_len_t a;

    if (R_math1_err_table[op] > 1) {

        /* Code for when a warning is possible, only done in master. */

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

    else {

        /* Code for when no warning is possible, may be done in a helper. */

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

    REAL(sy)[0] = (double) s;
}

#define ONE_SIMPLE_ARG(args) \
    ( !isNull(args) && isNull(CDR(args)) && isNull(TAG(args)) \
       && CAR(args) != R_DotsSymbol && CAR(args) != R_MissingArg \
       && CAR(args) != R_MissingUnder)

static SEXP nonsimple_log(SEXP call, SEXP op, SEXP args, SEXP env, int variant);
static SEXP do_math2(SEXP call, SEXP op, SEXP args, SEXP env);

#define T_math1 THRESHOLD_ADJUST(10)

SEXP attribute_hidden do_math1 (SEXP call, SEXP op, SEXP args, SEXP env, 
                                int variant)
{
    int opcode = PRIMVAL(op);
    if (opcode == 10003) /* horrible kludge for log */
        opcode = 13;
    else if (opcode >= 44)
        errorcall(call, _("unimplemented real function of 1 argument"));

    int vrnt = VARIANT_PENDING_OK;
    int gradv = 0;
    if ((variant & VARIANT_GRADIENT) && R_math1_deriv_table[opcode])
        gradv = VARIANT_GRADIENT;

    SEXP sa, grad, r;

    if (ONE_SIMPLE_ARG(args)) {
        vrnt |= VARIANT_SCALAR_STACK_OK;
        PROTECT (sa = evalv (CAR(args), env, vrnt | gradv));
        args = R_NilValue;
        PROTECT (grad = R_variant_result & VARIANT_GRADIENT_FLAG ? R_gradient 
                                                                 : R_NilValue);
    }
    else {

        if (opcode == 13 /* log */)
            vrnt |= VARIANT_MISSING_OK;

        args = gradv ? evalList_gradient (args, env, vrnt | gradv, 1, 0)
                     : evalList_v (args, env, vrnt);

        if (opcode == 13 /* log */ && CDR(args) != R_NilValue) 
            return nonsimple_log (call, op, args, env, variant);

        PROTECT(args);
        if (opcode != 5 /* trunc */) checkArity(op, args);
        check1arg_x (args, call);
        sa = CAR(args);

        PROTECT (grad = gradv ? GRADIENT_IN_CELL(args) : R_NilValue);
    }

    R_Visible = TRUE;

    /* At this point, sa is the first argument, and args is either the
       pairlist of arguments, or R_NilValue if that hasn't been created. */

    R_variant_result = 0;

    if (isObject(sa)) {

        SEXP nargs = args == R_NilValue ? CONS(sa,R_NilValue) : args;
        PROTECT(nargs);
        if (grad != R_NilValue)
            SET_ATTRIB (nargs, grad);
        if (DispatchGroup("Math", call, op, nargs, env, &r, variant)) {
            UNPROTECT(3);  /* sa|args, grad, nargs */
            return r;
        }
        UNPROTECT(1);  /* nargs */

        if (opcode == 14 || opcode == 15) {  /* log2 or log10 */
            /* try dispatching on log with base argument. */
            SEXP base = PRIMVAL(op) == 15 ? ScalarRealMaybeConst(10.0)
                                          : ScalarRealMaybeConst(2.0);
            SEXP basel = CONS (base, R_NilValue);
            SEXP call2;
            PROTECT(nargs = CONS (sa, basel));
            if (grad != R_NilValue)
                SET_ATTRIB (nargs, grad);
            PROTECT(call2 = LCONS (R_LogSymbol, CONS (sa, basel)));
            if (DispatchGroup("Math", call2, op, nargs, env, &r, variant)) {
                UNPROTECT(4);  /* sa|args, grad, nargs, call2 */
                return r;
            }
            UNPROTECT(2);  /* nargs, call2 */
        }
    }

    if (isComplex(sa)) {
        /* for the moment, keep the interface to complex_math1 the same */
        if (opcode == 14 || opcode == 15) {  /* log2 or log10 */
            SEXP base = opcode == 15 ? ScalarRealMaybeConst(10.0)
                                     : ScalarRealMaybeConst(2.0);
            SEXP basel = CONS (base, R_NilValue);
            SEXP args2, call2;
            PROTECT(args2 = CONS (sa, basel));
            PROTECT(call2 = LCONS (R_LogSymbol, CONS (sa, basel)));
            r = complex_math2 (call2, op, args2, env);
            UNPROTECT(2);  /* args2, call2 */
        }
        else {
            SEXP tmp;
            PROTECT(tmp = CONS(sa,R_NilValue));
            WAIT_UNTIL_COMPUTED(sa);
            r = complex_math1(call, op, tmp, env);
            UNPROTECT(1);  /* tmp */
        }
        UNPROTECT(2);  /* sa|args, grad */
        return r;
    }

    if (!isNumeric(sa)) {
        if (TYPEOF(sa) == VECSXP) {
            r = unary_listop (sa, OBJECT(sa), grad, env, variant,
                              math1_list_method[opcode]);
            UNPROTECT(2);
            return r;
        }
        non_numeric_errorcall(call);
    }

    int local_assign = 0;
    SEXP sa0 = sa;

    if (TYPEOF(sa) != REALSXP)
        sa = coerceVector(sa, REALSXP); /* coercion can lose the object bit */
    else if (VARIANT_KIND(variant)==VARIANT_LOCAL_ASSIGN1 
              && !NAMEDCNT_GT_1(sa) && grad == R_NilValue
              && sa == findVarInFrame3 (env, CADR(call), 7))
        local_assign = 1;

    PROTECT(sa);

    R_len_t n = LENGTH(sa);
    double opr, res;
    SEXP sy;

    if (n == 1) { /* scalar operation, including on scalar stack. */

        WAIT_UNTIL_COMPUTED(sa);

        opr = REAL(sa)[0];

        if (ISNAN(opr))
            res = opr;
        else {
            res = R_math1_func_table[opcode] (opr);
            if (R_math1_err_table[opcode]>1 && ISNAN(res))
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
    }

    else { /* not scalar */

        /* Note: need to protect sy below because some ops may produce a warning
           and attributes may be duplicated. */

        R_naflag = 0;

        if (VARIANT_KIND(variant) == VARIANT_SUM && grad == R_NilValue) {

            /* Just need the sum. */

            PROTECT(sy = allocVector1REAL());
            DO_NOW_OR_LATER1 (variant, 
                        n >= T_math1 && R_math1_err_table[opcode] <= 1,
                        HELPERS_PIPE_IN1, task_sum_math1, opcode, sy, sa);
        }

        else {

            PROTECT(sy = local_assign || NAMEDCNT_EQ_0(sa) 
                           ? sa : allocVector(REALSXP, n));

            if (helpers_not_multithreading_now || n < 2*T_math1
                || R_math1_err_table[opcode] > 1) {

                /* Use only one task. */

                DO_NOW_OR_LATER1(variant,
                                 n >= T_math1 && R_math1_err_table[opcode] <= 1,
                                 HELPERS_PIPE_IN01_OUT,
                                 task_math1, opcode, sy, sa);
            }
            else {

                /* Use two tasks, computing second half and first half. */

                helpers_do_task (HELPERS_PIPE_IN01_OUT, 
                                 task_math1, opcode | (2<<8),
                                 sy, sa, 0);

                helpers_do_task (variant & VARIANT_PENDING_OK
                                  ? HELPERS_PIPE_IN01_OUT
                                  : HELPERS_PIPE_IN01_OUT | HELPERS_MASTER_NOW,
                                 task_math1, opcode | (1<<8),
                                 sy, sa, 0);
            }

            maybe_dup_attributes (sy, sa, variant);
        }

        if (R_naflag)
            NaN_warningcall(call);
        UNPROTECT(1);
    }

    R_variant_result = local_assign; /* Defer setting to shortly before return*/

    if (grad != R_NilValue && R_math1_deriv_table[opcode] 
                           && TYPEOF(sy) == REALSXP) {

        GRADIENT_TRACE(call);

        SEXP res_grad = R_NilValue;

        if (n == 1) {
            if (!ISNAN(res)) {
                double d = R_math1_deriv_table[opcode] (opr, res);
                res_grad = copy_scaled_gradients (grad, d, 1);
            }
        }
        else {
            double (*df)(double,double) = R_math1_deriv_table[opcode];
            SEXP gr = allocVector (REALSXP, n);
            PROTECT(gr);
            for (R_len_t i = 0; i < n; i++) {
                REAL(gr)[i] = df (REAL(sa)[i], REAL(sy)[i]);
            }
            res_grad = copy_scaled_gradients_vec (grad, gr, n);
            UNPROTECT(1);
        }

        if (res_grad != R_NilValue) {
            R_variant_result |= VARIANT_GRADIENT_FLAG;
            R_gradient = res_grad;
        }
    }

    UNPROTECT(3);  /* sa|args, grad, sa (possibly changed) */
    return sy;
}

/* Cases of log that don't have just one unnamed argument (principally base).
   Does not compute gradients. */

static SEXP nonsimple_log (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    PROTECT(args);
    R_Visible = TRUE;

    /* This seems like some sort of horrible kludge that can't possibly
       be right in general (it ignores the argument names, and silently
       discards arguments after the first two). */

    if (CDR(args) != R_NilValue && CADR(args) == R_MissingArg) {
        double e = 2.718281828459045235;
        PROTECT(args);
        args = list2(CAR(args), ScalarReal(e)); 
        UNPROTECT(1);
    }

    SEXP res, call2;
    PROTECT(call2 = LCONS (CAR(call), args));

    if (DispatchGroup("Math", call2, op, args, env, &res, variant)) {
        UNPROTECT(2); /* args, call2 */
        return res;
    }

    /* match argument names if supplied */
    static const char * const ap[2] = { "x", "base" };
    PROTECT(args = matchArgs_strings (ap, 2, args, call));
    if (length(CADR(args)) == 0)
        errorcall(call, _("invalid argument 'base' of length 0"));

    if (isComplex(CAR(args)) || isComplex(CADR(args)))
        res = complex_math2(call, op, args, env);
    else
        res = do_math2 (call, op, args, env);

    UNPROTECT(3); /* old args, call2, args */
    return res;
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

#define T_abs THRESHOLD_ADJUST(500)

SEXP do_abs(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP r, s, x, g;
    double opr, res;
    R_len_t n;

    PROTECT (args = variant & VARIANT_GRADIENT 
                     ? evalList_gradient (args, env, 0, 1, 0)
                     : evalList_v (args, env, 0));

    checkArity(op, args);
    check1arg_x (args, call);

    x = CAR(args);

    if (DispatchGroup("Math", call, op, args, env, &r, variant)) {
        UNPROTECT(1);
	return r;
    }

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
        n = LENGTH(x);
        if (VARIANT_KIND(variant) == VARIANT_SUM) {
            s = allocVector1REAL();
            DO_NOW_OR_LATER1 (variant, n >= T_abs,
                              HELPERS_PIPE_IN1, task_sum_abs, 0, s, x);
            UNPROTECT(1);
            R_Visible = TRUE;
            return s;
        }
        else if (n == 1) {
            WAIT_UNTIL_COMPUTED(x);
            opr = *REAL(x);
            res = fabs(opr);
            s = NAMEDCNT_EQ_0(x) && TYPEOF(x) == REALSXP ? 
                  (*REAL(x) = res, x)
              : CAN_USE_SCALAR_STACK(variant) && NO_ATTRIBUTES_OK(variant,x) ?
                  PUSH_SCALAR_REAL(res)
              :   ScalarReal(res);
        }
        else {
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

    if (HAS_GRADIENT_IN_CELL(args) && TYPEOF(x) == REALSXP) {

        GRADIENT_TRACE(call);

        SEXP grad = GRADIENT_IN_CELL(args);
        SEXP res_grad = R_NilValue;

        if (n == 1) {
            if (!ISNAN(*REAL(s))) {
                res_grad = copy_scaled_gradients (grad, sign(opr), 1);
            }
        }
        else {
            SEXP gr = allocVector (REALSXP, n);
            PROTECT(gr);
            for (R_len_t i = 0; i < n; i++) {
                REAL(gr)[i] = sign (REAL(x)[i]);
            }
            res_grad = copy_scaled_gradients_vec (grad, gr, n);
            UNPROTECT(1);
        }

        if (res_grad != R_NilValue) {
            R_variant_result |= VARIANT_GRADIENT_FLAG;
            R_gradient = res_grad;
        }
    }
    UNPROTECT(2);  /* args, s */

    R_Visible = TRUE;
    return s;
}


/* MATHEMATICAL FUNCTIONS OF TWO NUMERIC ARGUMENTS 
   (plus 0, 1, or 2 integers). */

/* Derivatives of math2 functions. */

static void Datan2 (double y, double x, double *dy, double *dx, double v)
{
    double r2 = x*x + y*y;

    if (r2 == 0) {
        if (dy) *dy = 0;
        if (dx) *dx = 0;
    }
    else {
        if (dy) *dy = x / r2;
        if (dx) *dx = -y / r2;
    }
}

static void Dlbeta (double a, double b, double *da, double *db, double v)
{
    if (a == 0 || b == 0) {
        if (da) *da = a == 0 ? R_NegInf : 0;
        if (db) *db = b == 0 ? R_NegInf : 0;
    }
    else {
        double diab = digamma(a+b);
        if (da) *da = digamma(a) - diab;
        if (db) *db = digamma(b) - diab;
    }
}

static void Dbeta (double a, double b, double *da, double *db, double v)
{
    if (a == 0 || b == 0) {
        if (da) *da = a == 0 ? R_NegInf : 0;
        if (db) *db = b == 0 ? R_NegInf : 0;
    }
    else {
        double diab = digamma(a+b);
        if (da) *da = v * (digamma(a) - diab);
        if (db) *db = v * (digamma(b) - diab);
    }
}

static void Ddchisq (double x, double df, double *dx, double *ddf,
                     double v, int give_log)
{
    double shape = 0.5*df;
    double x0 = 0.5*x;

    if (x0 <= 0 || !R_FINITE(v)) {
        if (dx) *dx = 0;
        if (ddf) *ddf = 0;
    }
    else {
        if (dx) *dx = 0.5 * ((shape-1)/x0 - 1);
        if (ddf) *ddf = 0.5 * (log(x) - log(2.0) - digamma(shape));
        if (!give_log) {
            if (dx) *dx *= v;
            if (ddf) *ddf *= v;
        }
    }
}

static void Dpchisq (double q, double df, double *dq, double *ddf,
                     double v, int lower_tail, int log_p)
{
    if (ddf) *ddf = 0;

    if (!R_FINITE(v)) {
        if (dq) *dq = 0;
    }
    else {
        if (dq) {
            double d = dchisq(q,df,log_p);
            *dq = log_p ? exp (d-v) : d;
            if (!lower_tail) *dq = -*dq;
        }
    }
}

static void Dqchisq (double p, double df, double *dp, double *ddf,
                     double v, int lower_tail, int log_p)
{
    if (ddf) *ddf = 0;

    if (!R_FINITE(v)) {
        if (dp) *dp = 0;
    }
    else {
        if (dp) {
            double d = dchisq(v,df,FALSE);
            *dp = lower_tail ? 1/d : -1/d;
            if (log_p) *dp *= exp(p);
        }
    }
}

static void Ddexp (double x, double scale, double *dx, double *dscale,
                   double v, int give_log)
{
    if (x < 0) {
        if (dx) *dx = 0;
        if (dscale) *dscale = 0;
    }
    else if (give_log) {
        if (dx) *dx = (-1.0) / scale;
        if (dscale) *dscale = (x/scale - 1) / scale;
    }
    else {
        if (dx) *dx = -v / scale;
        if (dscale) *dscale = v * (x/scale - 1) / scale;
    }
}

static void Dpexp (double q, double scale, double *dq, double *dscale,
                   double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dq) *dq = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = q / scale;
        double dp0 = dexp (q0, 1, 0);

        if (dq) *dq = dp0 / scale;
        if (dscale) *dscale = - dp0 * q0 / scale;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dscale) *dscale = -*dscale;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dscale) *dscale *= expv;
        }
    }
}

static void Dqexp (double p, double scale, double *dp, double *dscale,
                   double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dp) *dp = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = v / scale;

        if (dp) *dp = (lower_tail ? 1 : -1) * scale / dexp (q0, 1, 0);
        if (dscale) *dscale = q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddgeom (double x, double p, double *dx /* must be 0 */, 
                    double *dp, double v, int give_log)
{
    if (dx) abort();
    if (!dp) return;

    if (x < 0 || p <= 0 || x > 0 && p >= 1)
        *dp = 0;
    else {
        *dp = 1/p - x/(1-p);
        if (!give_log) 
            *dp *= v;
    }
}

static void Dpgeom (double q, double p, double *dq /* must be 0 */, double *dp,
                    double v, int lower_tail, int log_p)
{
    if (dq) abort();
    if (!dp) return;

    if (q < 0 || p <= 0)
        *dp = 0;
    else {

        double t = (q+1) / (p-1);

        if (log_p)
            *dp = lower_tail ? -t*expm1(-v) : t;
        else
            *dp = lower_tail ? t*(v-1) : t*v;
    }
}

static void Ddpois (double x, double lambda, double *dx /* must be 0 */, 
                    double *dlambda, double v, int give_log)
{
    if (dx) abort();
    if (!dlambda) return;

    if (x < 0 || lambda < 0)
        *dlambda = 0;
    else {
        *dlambda = x == 0 ? -1 : x/lambda - 1;
        if (!give_log) 
            *dlambda *= v;
    }
}

static void Dppois (double x, double lambda, double *dx /* must be 0 */, 
                    double *dlambda, double v, int lower_tail, int log_p)
{
    if (dx) abort();
    if (!dlambda) return;

    if (x < 0 || lambda < 0)
        *dlambda = 0;
    else {

        /* Sum of Poisson probabilities P(x;lambda) = lambda^x exp(-lambda) / x!
           differentiates to similar sums of Poisson probabilities, which then
           mostly cancel.  For example:

           d/dlambda [exp(-lambda) (1 + lambda + lambda^2/2 + lambda^3/3!)]

             = exp(-lambda) (1 + lambda + lambda^2/2)
                 - exp(-lambda) (1 + lambda + lambda^2/2 + lambda^3/3!)

             = - P(3;lambda)
         */

        double d = dpois(x,lambda,log_p);

        *dlambda = log_p ? exp(d-v) : d;

        if (lower_tail) *dlambda = -*dlambda;
    }
}

static void Ddt (double x, double n, double *dx, double *dn,
                 double v, int give_log)
{
    double f = 1 + x*x/n;

    if (dx) *dx = -x/f - x/(n+x*x);  /* OK for both n->0 and n->oo */

    /* Finds derivative wrt to n of

          0.5*log(n/2) - log((n+1)/2) + lgamma((n+3)/2) - lgamma((n+2)/2)
           - (n/2)*log(1+x*x/n) - 0.5*log(2*M_PI) - 0.5*log(1+x*x/n)
     
       See nmath/dt.c. */

    if (dn) *dn = 0.5/n - 1/(n+1) + 0.5*digamma((n+3)/2) - 0.5*digamma((n+2)/2)
                    + (0.5+0.5/n)/(1+n/(x*x)) - 0.5*log(f);

    if (!give_log) {
        if (dx) *dx *= v;
        if (dn) *dn *= v;
    }
}

static void Dpt (double q, double n, double *dq, double *dn,
                 double v, int lower_tail, int log_p)
{
    if (dn) *dn = 0;

    if (!R_FINITE(v)) {
        if (dq) *dq = 0;
    }
    else {
        if (dq) {
            double d = dt(q,n,log_p);
            *dq = log_p ? exp (d-v) : d;
            if (!lower_tail) *dq = -*dq;
        }
    }
}

static void Dqt (double p, double n, double *dp, double *dn,
                 double v, int lower_tail, int log_p)
{
    if (dn) *dn = 0;

    if (!R_FINITE(v)) {
        if (dp) *dp = 0;
    }
    else {
        if (dp) {
            double d = dt(v,n,FALSE);
            *dp = lower_tail ? 1/d : -1/d;
            if (log_p) *dp *= exp(p);
        }
    }
}

static void Dpsigamma (double x, double deriv, double *dx, 
                       double *dderiv /* must be zero */, double v)
{
    if (dderiv) abort();
    if (!dx) return;

    *dx = psigamma(x,deriv+1);
}


/* Allocate work array for Bessel functions. */

static double *Bessel_work_array (int n2, double *ap2)
{
    double amax;
    long nw;
    int i;
    amax = 0.0;
    for (i = 0; i < n2; i++) {
        double av = ap2[i] < 0 ? -ap2[i] : ap2[i];
        if (av > amax) amax = av;
    }
    nw = 1 + (long)floor(amax);
    return (double *) R_alloc((size_t) nw, sizeof(double));
}

/* Table of functions to compute values and derivatives for math2 functions. */

static struct { double (*fncall)(); void (*Dcall)(); } math2_table[31] = {
    { 0,	0 },
    { atan2,	Datan2 },
    { lbeta,	Dlbeta },
    { beta,	Dbeta },
    { lchoose,	0  /* could differentiate wrt 1st arg, but usually integer */ },
    { choose,	0  /* could differentiate wrt 1st arg, but usually integer */ },
    { dchisq,	Ddchisq },
    { pchisq,	Dpchisq },
    { qchisq,	Dqchisq },
    { dexp,	Ddexp },
    { pexp,	Dpexp },
    { qexp,	Dqexp },
    { dgeom,	Ddgeom },
    { pgeom,	Dpgeom },
    { qgeom,	0 /* discrete */ },
    { dpois,	Ddpois },
    { ppois,	Dppois },
    { qpois,	0 /* discrete */ },
    { dt,	Ddt },
    { pt,	Dpt },
    { qt,	Dqt },
    { dsignrank,   0 /* discrete */ },
    { psignrank,   0 /* discrete */ },
    { qsignrank,   0 /* discrete */ },
    { bessel_j_ex, 0 },
    { bessel_y_ex, 0 },
    { psigamma,	Dpsigamma },
    { fround,	0 /* discrete */ },
    { 0,	0 },
    { logbase,	0 },
    { fprec,	0 }
};

SEXP do_math2 (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res;

    checkArity(op, args);

    if (isComplex(CAR(args)) || (PRIMVAL(op) == 0 && isComplex(CADR(args))))
	return complex_math2(call, op, args, env);

    double (*fncall)();

    int ix = PRIMVAL(op);
    if (ix>10000) ix = ix - 10000 + 26;  /* kludge */
    if (ix < 0 || ix > 30 || (fncall = math2_table[ix].fncall) == 0)
        errorcall (call,
                   _("unimplemented real function of %d numeric arguments"), 2);
    SEXP a1 = CAR(args); SEXP g1 = GRADIENT_IN_CELL(args); args = CDR(args);
    SEXP a2 = CAR(args); SEXP g2 = GRADIENT_IN_CELL(args); args = CDR(args);

    if (!isNumeric(a1) || !isNumeric(a2))
        non_numeric_errorcall(call);

    R_len_t n1 = LENGTH(a1);
    R_len_t n2 = LENGTH(a2);

    if (n1 == 0 || n2 == 0) {
        PROTECT(res = allocVector(REALSXP, 0));
        /* for 0-length a we want the attributes of a, not those of b
           as no recycling will occur */
        if (n1 == 0) DUPLICATE_ATTRIB(res, a1);
        UNPROTECT(1);
        return res;
    }

    PROTECT(a1 = coerceVector (a1, REALSXP));
    PROTECT(a2 = coerceVector (a2, REALSXP));

    int n = n1 > n2 ? n1 : n2;

    PROTECT(res = allocVector (REALSXP, n));
    DUPLICATE_ATTRIB (res, n1 >= n2 ? a1 : a2);

    int i1 = 0, i2 = 0;
    if (args != R_NilValue)
        i1 = asInteger(CAR(args));
    if (CDR(args) != R_NilValue)
        i2 = asInteger(CADR(args));

    double *ap1 = REAL(a1), *ap2 = REAL(a2), *rp = REAL(res);
    int naflag = 0;

    /* Allocate work array for BesselJ & BesselY big enough for all arguments */

    double *work = 0;
    if (fncall == bessel_j_ex || fncall == bessel_y_ex)
        work = Bessel_work_array (n2, ap2);

    mod_iterate (n, n1, n2, j1, j2) {

        double v1 = ap1[j1];
        double v2 = ap2[j2];
        if (MAY_BE_NAN2(v1,v2)) {
            if (ISNA(v1) || ISNA(v2)) {
                rp[i] = NA_REAL;
                continue;
            }
            if (ISNAN(v1) || ISNAN(v2)) {
                rp[i] = R_NaN;
                continue;
            }
        }

        rp[i] = work ? fncall (v1, v2, work)
              : args == R_NilValue ? fncall (v1, v2)
              : CDR(args)==R_NilValue ? fncall (v1, v2, i1)
                                      : fncall (v1, v2, i1, i2);

        if (ISNAN(rp[i])) naflag = 1;
    }

    void (*Dcall)() = math2_table[ix].Dcall;

    if (Dcall != 0 && (g1 != R_NilValue || g2 != R_NilValue)) {

        GRADIENT_TRACE(call);

        SEXP res_grad = R_NilValue;

        if (n == 1) {
            if (!ISNAN(rp[0])) {
                double grad1, grad2;
                double *gp1 = g1 != R_NilValue ? &grad1 : 0;
                double *gp2 = g2 != R_NilValue ? &grad2 : 0;
        
                if (args == R_NilValue)
                    Dcall (ap1[0], ap2[0], gp1, gp2, rp[0]);
                else if (CDR(args)==R_NilValue)
                    Dcall (ap1[0], ap2[0], gp1, gp2, rp[0], i1);
                else
                    Dcall (ap1[0], ap2[0], gp1, gp2, rp[0], i1, i2);
        
                if (g1 != R_NilValue) {
                    res_grad = copy_scaled_gradients (g1, grad1, 1);
                }
                if (g2 != R_NilValue) {
                    res_grad = res_grad == R_NilValue
                                ? copy_scaled_gradients (g2, grad2, 1)
                                : add_scaled_gradients (res_grad, g2, grad2, 1);
                }
            }
        }
        else {

            SEXP gr1 = g1 == R_NilValue ? R_NilValue : allocVector (REALSXP, n);
            PROTECT(gr1);

            SEXP gr2 = g2 == R_NilValue ? R_NilValue : allocVector (REALSXP, n);
            PROTECT(gr2);

            if (args == R_NilValue) {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], 
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           rp[i]);
            }
            else if (CDR(args) == R_NilValue) {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], 
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           rp[i], i1);
            }
            else {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], 
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           rp[i], i1, i2);
            }

            if (g1 != R_NilValue) {
                res_grad = copy_scaled_gradients_vec (g1, gr1, n);
            }
            if (g2 != R_NilValue) {
                res_grad = res_grad == R_NilValue
                            ? copy_scaled_gradients_vec (g2, gr2, n)
                            : add_scaled_gradients_vec (res_grad, g2, gr2, n);
            }

            UNPROTECT(2);
        }

        if (res_grad != R_NilValue) {
            R_gradient = res_grad;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }
    }

    if (naflag) NaN_warning();

    UNPROTECT(3);
    return res;
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP do_Math2(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
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

    if (! DispatchGroup("Math", call2, op, args, env, &res, variant)) {
	if(n == 1) {
	    double digits = 0.0;
            check1arg_x (args, call);
	    if(PRIMVAL(op) == 10004) digits = 6.0;
	    SETCDR(args, CONS(ScalarRealMaybeConst(digits), R_NilValue));
	} else {
	    /* If named, do argument matching by name */
	    if (TAG(args) != R_NilValue || TAG(CDR(args)) != R_NilValue) {
                static const char * const ap[2] = { "x", "digits" };
		PROTECT(args = matchArgs_strings (ap, 2, args, call));
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


/* MATHEMATICAL FUNCTIONS OF THREE NUMERIC ARGUMENTS 
   (plus 0, 1, or 2 integers). */

/* Derivatives of math3 functions. */

static void Ddbeta (double x, double a, double b, 
                    double *dx, double *da, double *db,
                    double v, int give_log)
{
    if (!R_FINITE(v)) {
        if (dx) *dx = 0;
        if (da) *da = 0;
        if (db) *db = 0;
    }
    else {

        if (dx) *dx = (a-1)/x - (b-1)/(1-x);

        if (da || db) {
            double diab = digamma(a+b);
            if (da) *da = log(x) - digamma(a) + diab;
            if (db) *db = log1p(-x) - digamma(b) + diab;
        }

        if (!give_log) {
            if (dx) *dx *= v;
            if (da) *da *= v;
            if (db) *db *= v; 
        }
    }
}

static void Dpbeta (double q, double a, double b, 
                    double *dq, double *da, double *db,
                    double v, int lower_tail, int log_p)
{
    if (da) *da = 0;
    if (db) *db = 0;

    if (!R_FINITE(v)) {
        if (dq) *dq = 0;
    }
    else {
        if (dq) {
            double d = dbeta(q,a,b,log_p);
            *dq = log_p ? exp (d-v) : d;
            if (!lower_tail) *dq = -*dq;
        }
    }
}

static void Dqbeta (double p, double a, double b, 
                    double *dp, double *da, double *db,
                    double v, int lower_tail, int log_p)
{
    if (da) *da = 0;
    if (db) *db = 0;

    if (!R_FINITE(v)) {
        if (dp) *dp = 0;
    }
    else {
        if (dp) {
            double d = dbeta(v,a,b,FALSE);
            *dp = lower_tail ? 1/d : -1/d;
            if (log_p) *dp *= exp(p);
        }
    }
}

static void Ddbinom (double x, double n, double p, 
                     double *dx /* must be 0 */, double *dn /* must be 0 */, 
                     double *dp, double v, int give_log)
{
    if (dx || dn) abort();
    if (!dp) return;

    if (n <= 0 || p <= 0 || p >= 1 || x < 0 || x > n) {
        *dp = 0;
    }
    else {
        double lp = x/p - (n-x)/(1-p);
        *dp = give_log ? lp : lp*v;
    }
}

static void Dpbinom (double q, double n, double p, 
                     double *dq /* must be 0 */, double *dn /* must be 0 */,
                     double *dp, double v, int lower_tail, int log_p)
{
    if (dq || dn) abort();
    if (!dp) return;

    if (n <= 0 || p <= 0 || p >= 1 || q < 0 || q > n)
        *dp = 0;
    else {

        /* Derivative wrt p of tail sum of binomial (n,p) probabilities gives 
           sum of pairs of binomial (n-1,p) probabilities, times +-n, with
           all but one term then cancelling.  Eg:

             d/dp [ P(0;3,p) + P(1;3,p) + P(2;3,p) ] =
             d/dp [ (1-p)^3    + 3*p*(1-p)^2 + 3*p^2*(1-p) ]
               =     0         + 3*(1-p)^2   + 3*2*p*(1-p) 
                   - 3*(1-p)^2 - 3*2*p*(1-p) - 3*p^2       
               =   - 3*p^2
               =   - 3*P(2;2,p)
         */

        if (log_p) {
            double t = dbinom(q,n-1,p,TRUE) + log(n);
            *dp = lower_tail ? -exp(t-v) : exp(t-v);
        }
        else {
            double t = dbinom(q,n-1,p,FALSE)*n;
            *dp = lower_tail ? -t : t;
        }
    }
}

static void Ddcauchy (double x, double location, double scale, 
                      double *dx, double *dlocation, double *dscale,
                      double v, int give_log)
{
    if (!R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dlocation) *dlocation = 0;
        if (dscale) *dscale = 0;
    }
    else {
        double x0 = (x - location) / scale;
        double f = 1 + x0*x0;
        if (dx) *dx = -2*x0 / (f*scale);
        if (dlocation) *dlocation = 2*x0 / (f*scale);
        if (dscale) *dscale = (x0*x0-1) / (f*scale);
        if (!give_log) {
            if (dx) *dx *= v;
            if (dlocation) *dlocation *= v;
            if (dscale) *dscale *= v;
        }
    }
}

static void Dpcauchy (double q, double location, double scale, 
                      double *dq, double *dlocation, double *dscale,
                      double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dq) *dq = 0;
        if (dlocation) *dlocation = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = (q - location) / scale;
        double dp0 = dcauchy (q0, 0, 1, FALSE);

        if (dq) *dq = dp0 / scale;
        if (dlocation) *dlocation = - dp0 / scale;
        if (dscale) *dscale = - dp0 * q0 / scale;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dlocation) *dlocation = -*dlocation;
            if (dscale) *dscale = -*dscale;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dlocation) *dlocation *= expv;
            if (dscale) *dscale *= expv;
        }
    }
}

static void Dqcauchy (double p, double location, double scale, 
                      double *dp, double *dlocation, double *dscale,
                      double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dp) *dp = 0;
        if (dlocation) *dlocation = 1;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = (v - location) / scale;

        if (dp) *dp = (lower_tail ? 1 : -1) * scale / dcauchy (q0, 0, 1, 0);
        if (dlocation) *dlocation = 1;
        if (dscale) *dscale = q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddf (double x, double df1, double df2, 
                 double *dx, double *ddf1, double *ddf2,
                 double v, int give_log)
{
    if (x <= 0 || !R_FINITE(v)) {
        if (dx) *dx = 0;
        if (ddf1) *ddf1 = 0;
        if (ddf2) *ddf2 = 0;
    }
    else {

        double s = 0.5 * (df1 + df2);
        double r = df1 / df2;
        double t = s/(1+r*x);

        if (dx) *dx = (0.5*df1-1)/x - r*t;
        if (ddf1 || ddf2) {
            double g = 0.5 * digamma(s);
            double h = 0.5 * log(1+r*x);
            double m = (x/df2)*t;
            if (ddf1) *ddf1 = g - 0.5*digamma(0.5*df1) + 0.5*log(r)
                            - h + 0.5 + 0.5*log(x) - m;
            if (ddf2) *ddf2 = g - 0.5*digamma(0.5*df2) - 0.5*r
                            - h + r*m;
        }

        if (!give_log) {
            if (dx) *dx *= v;
            if (ddf1) *ddf1 *= v;
            if (ddf2) *ddf2 *= v;
        }
    }
}

static void Dpf (double q, double df1, double df2, 
                 double *dq, double *ddf1, double *ddf2,
                 double v, int lower_tail, int log_p)
{
    if (ddf1) *ddf1 = 0;
    if (ddf2) *ddf2 = 0;

    if (!R_FINITE(v)) {
        if (dq) *dq = 0;
    }
    else {
        if (dq) {
            double d = df(q,df1,df2,log_p);
            *dq = log_p ? exp (d-v) : d;
            if (!lower_tail) *dq = -*dq;
        }
    }
}

static void Dqf (double p, double df1, double df2, 
                 double *dp, double *ddf1, double *ddf2,
                 double v, int lower_tail, int log_p)
{
    if (ddf1) *ddf1 = 0;
    if (ddf2) *ddf2 = 0;

    if (!R_FINITE(v)) {
        if (dp) *dp = 0;
    }
    else {
        if (dp) {
            double d = df(v,df1,df2,FALSE);
            *dp = lower_tail ? 1/d : -1/d;
            if (log_p) *dp *= exp(p);
        }
    }
}

static void Ddgamma (double x, double shape, double scale, 
                     double *dx, double *dshape, double *dscale,
                     double v, int give_log)
{
    double x0 = x / scale;

    if (x0 <= 0 || !R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = 0;
    }
    else {
        if (dx) *dx = ((shape-1)/x0 - 1) / scale;
        if (dshape) *dshape = log(x) - log(scale) - digamma(shape);
        if (dscale) *dscale = (x0 - shape) / scale;
        if (!give_log) {
            if (dx) *dx *= v;
            if (dshape) *dshape *= v;
            if (dscale) *dscale *= v;
        }
    }
}

static void Dpgamma (double q, double shape, double scale, 
                     double *dq, double *dshape, double *dscale,
                     double v, int lower_tail, int log_p)
{
    if (scale <= 0 || shape <= 0 || q <= 0) {
        if (dq) *dq = 0;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = q / scale;
        double dp0 = dgamma (q0, shape, 1, FALSE);

        if (dq) *dq = dp0 / scale;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = - dp0 * q0 / scale;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dscale) *dscale = -*dscale;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dscale) *dscale *= expv;
        }
    }
}

static void Dqgamma (double p, double shape, double scale, 
                     double *dp, double *dshape, double *dscale,
                     double v, int lower_tail, int log_p)
{
    if (scale <= 0 || shape <= 0) {
        if (dp) *dp = 0;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = v / scale;

        if (dp) *dp = (lower_tail ? 1 : -1) * scale / dgamma(q0,shape,1,FALSE);
        if (dshape) *dshape = 0;
        if (dscale) *dscale = q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddlnorm (double x, double mu, double sigma, 
                     double *dx, double *dmu, double *dsigma,
                     double v, int give_log)
{
    if (x <= 0 || sigma <= 0 || !R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dmu) *dmu = 0;
        if (dsigma) *dsigma = 0;
    }
    else {
        double x0 = (log(x) - mu) / sigma;
        if (dx) *dx = -(x0/sigma+1) / x;
        if (dmu) *dmu = x0/sigma;
        if (dsigma) *dsigma = (x0*x0-1) / sigma;
        if (!give_log) {
            if (dx) *dx *= v;
            if (dmu) *dmu *= v;
            if (dsigma) *dsigma *= v;
        }
    }
}

static void Dplnorm (double q, double mu, double sigma, 
                     double *dq, double *dmu, double *dsigma,
                     double v, int lower_tail, int log_p)
{
    if (q <= 0 || sigma <= 0) {
        if (dq) *dq = 0;
        if (dmu) *dmu = 0;
        if (dsigma) *dsigma = 0;
    }
    else {

        double q0 = (log(q) - mu) / sigma;
        double dp0 = dnorm (q0, 0, 1, 0);

        if (dq) *dq = dp0 / (q*sigma);
        if (dmu) *dmu = - dp0 / sigma;
        if (dsigma) *dsigma = - dp0 * q0 / sigma;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dmu) *dmu = -*dmu;
            if (dsigma) *dsigma = -*dsigma;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dmu) *dmu *= expv;
            if (dsigma) *dsigma *= expv;
        }
    }
}

static void Dqlnorm (double p, double mu, double sigma, 
                     double *dp, double *dmu, double *dsigma,
                     double v, int lower_tail, int log_p)
{
    if (sigma <= 0) {
        if (dp) *dp = 0;
        if (dmu) *dmu = 1;
        if (dsigma) *dsigma = 0;
    }
    else {

        double q0 = (log(v) - mu) / sigma;

        if (dp) *dp = (lower_tail ? v : -v) * sigma / dnorm (q0, 0, 1, 0);
        if (dmu) *dmu = v;
        if (dsigma) *dsigma = v * q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddlogis (double x, double location, double scale, 
                     double *dx, double *dlocation, double *dscale,
                     double v, int give_log)
{
    if (!R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dlocation) *dlocation = 0;
        if (dscale) *dscale = 0;
    }
    else {
        double x0 = (x - location) / scale;
        double t = tanh(x0/2);
        if (dx) *dx = - t / scale;
        if (dlocation) *dlocation = t / scale;
        if (dscale) *dscale = (t*x0 - 1) / scale;
        if (!give_log) {
            if (dx) *dx *= v;
            if (dlocation) *dlocation *= v;
            if (dscale) *dscale *= v;
        }
    }
}

static void Dplogis (double q, double location, double scale, 
                     double *dq, double *dlocation, double *dscale,
                     double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dq) *dq = 0;
        if (dlocation) *dlocation = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = (q - location) / scale;
        double dp0 = dlogis (q0, 0, 1, 0);

        if (dq) *dq = dp0 / scale;
        if (dlocation) *dlocation = - dp0 / scale;
        if (dscale) *dscale = - dp0 * q0 / scale;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dlocation) *dlocation = -*dlocation;
            if (dscale) *dscale = -*dscale;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dlocation) *dlocation *= expv;
            if (dscale) *dscale *= expv;
        }
    }
}

static void Dqlogis (double p, double location, double scale, 
                     double *dp, double *dlocation, double *dscale,
                     double v, int lower_tail, int log_p)
{
    if (scale <= 0) {
        if (dp) *dp = 0;
        if (dlocation) *dlocation = 1;
        if (dscale) *dscale = 0;
    }
    else {

        double q0 = (v - location) / scale;

        if (dp) *dp = (lower_tail ? 1 : -1) * scale / dlogis (q0, 0, 1, 0);
        if (dlocation) *dlocation = 1;
        if (dscale) *dscale = q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddnbinom (double x, double size, double prob, 
                      double *dx /* must be 0 */, double *dsize /* must be 0 */,
                      double *dprob, double v, int give_log)
{
    if (dx || dsize) abort();
    if (!dprob) return;

    if (size <= 0 || prob <= 0 || prob >= 1 || x < 0) {
        *dprob = 0;
    }
    else {
        double lp = size/prob - x/(1-prob);
        *dprob = give_log ? lp : lp*v;
    }
}

static void Dpnbinom (double q, double size, double prob, 
                      double *dq /* must be 0 */, double *dsize /* must be 0 */,
                      double *dprob, double v, int lower_tail, int log_p)
{
    if (dq || dsize) abort();
    if (!dprob) return;

    if (size <= 0 || q < 0 || prob <= 0 || prob >= 1)
        *dprob = 0;
    else {
        *dprob = dbeta (prob, size, q+1, FALSE);
        if (!lower_tail) *dprob = -*dprob;
        if (log_p) *dprob *= exp(-v);
    }
}

static void Ddnbinom_mu (double x, double size, double mu, 
                      double *dx /* must be 0 */, double *dsize /* must be 0 */,
                      double *dmu, double v, int give_log)
{
    if (dx || dsize) abort();
    if (!dmu) return;

    if (size <= 0 || mu <= 0 || x < 0) {
        *dmu = 0;
    }
    else {
        double lp = (x/mu - 1) * (size / (size+mu));
        *dmu = give_log ? lp : lp*v;
    }
}

static void Dpnbinom_mu (double q, double size, double mu, 
                      double *dq /* must be 0 */, double *dsize /* must be 0 */,
                      double *dmu, double v, int lower_tail, int log_p)
{
    if (dq || dsize) abort();
    if (!dmu) return;

    if (size <= 0 || q < 0 || mu <= 0)
        *dmu = 0;
    else {
        double prob = size / (size+mu);
        *dmu = - dbeta (prob, size, q+1, FALSE) * prob / (size+mu);
        if (!lower_tail) *dmu = -*dmu;
        if (log_p) *dmu *= exp(-v);
    }
}

static void Ddnorm (double x, double mu, double sigma, 
                    double *dx, double *dmu, double *dsigma,
                    double v, int give_log)
{
    if (!R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dmu) *dmu = 0;
        if (dsigma) *dsigma = 0;
    }
    else {
        double x0 = (x - mu) / sigma;
        if (dx) *dx = -x0 / sigma;
        if (dmu) *dmu = x0 / sigma;
        if (dsigma) *dsigma = (x0*x0 - 1) / sigma;
        if (!give_log) {
            if (dx) *dx *= v;
            if (dmu) *dmu *= v;
            if (dsigma) *dsigma *= v;
        }
    }
}

static void Dpnorm (double q, double mu, double sigma, 
                    double *dq, double *dmu, double *dsigma,
                    double v, int lower_tail, int log_p)
{
    if (sigma <= 0) {
        if (dq) *dq = 0;
        if (dmu) *dmu = 0;
        if (dsigma) *dsigma = 0;
    }
    else {

        double q0 = (q - mu) / sigma;
        double dp0 = dnorm (q0, 0, 1, 0);

        if (dq) *dq = dp0 / sigma;
        if (dmu) *dmu = - dp0 / sigma;
        if (dsigma) *dsigma = - dp0 * q0 / sigma;

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dmu) *dmu = -*dmu;
            if (dsigma) *dsigma = -*dsigma;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dmu) *dmu *= expv;
            if (dsigma) *dsigma *= expv;
        }
    }
}

static void Dqnorm (double p, double mu, double sigma, 
                    double *dp, double *dmu, double *dsigma,
                    double v, int lower_tail, int log_p)
{
    if (sigma <= 0) {
        if (dp) *dp = 0;
        if (dmu) *dmu = 1;
        if (dsigma) *dsigma = 0;
    }
    else {

        double q0 = (v - mu) / sigma;

        if (dp) *dp = (lower_tail ? 1 : -1) * sigma / dnorm (q0, 0, 1, 0);
        if (dmu) *dmu = 1;
        if (dsigma) *dsigma = q0;

        if (log_p)
            if (dp) *dp *= exp(p);
    }
}

static void Ddunif (double x, double a, double b, 
                    double *dx, double *da, double *db,
                    double v, int give_log)
{
    if (dx) *dx = 0;

    if (b <= a || x <= a || x >= b) {
        if (da) *da = 0;
        if (db) *db = 0;
    }
    else {
        double t = 1 / (b-a);
        if (give_log) {
            if (da) *da = t;
            if (db) *db = -t;
        }
        else {
            if (da) *da = t * v;
            if (db) *db = -t * v;
        }
    }
}

static void Dpunif (double q, double a, double b, 
                    double *dq, double *da, double *db,
                    double v, int lower_tail, int log_p)
{
    if (b <= a || q <= a || q >= b) {
        if (dq) *dq = 0;
        if (da) *da = 0;
        if (db) *db = 0;
    }
    else {

        double t = 1 / (b-a);

        if (lower_tail) {
            if (dq) *dq = t;
            if (da) *da = ((q-a)*t - 1) * t;
            if (db) *db = (a-q) * t * t;
        }
        else {
            if (dq) *dq = -t;
            if (da) *da = ((a-q)*t + 1) * t;
            if (db) *db = (q-a) * t * t;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (da) *da *= expv;
            if (db) *db *= expv;
        }
    }
}

static void Dqunif (double p, double a, double b, 
                    double *dp, double *da, double *db,
                    double v, int lower_tail, int log_p)
{
    if (b <= a) {
        if (dp) *dp = 0;
    }
    else {
        if (dp) *dp = lower_tail ? b-a : a-b;
    }

    if (log_p) {
        if (da) *da = lower_tail ? -expm1(p) : exp(p);
        if (db) *db = lower_tail ? exp(p) : -expm1(p);
        if (dp) *dp *= exp(p);
    }
    else {
        if (da) *da = lower_tail ? 1-p : p;
        if (db) *db = lower_tail ? p : 1-p;
    }
}

static void Ddweibull (double x, double shape, double scale, 
                       double *dx, double *dshape, double *dscale,
                       double v, int give_log)
{
    if (x < 0 || !R_FINITE(v)) {
        if (dx) *dx = 0;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = 0;
    }
    else {

        double t = shape * pow(x/scale,shape-1);
        if (dx) 
            *dx = (shape-1)/x - t/scale;
        if (dshape) 
            *dshape = 1/shape + log(x/scale) * (1 - pow(x/scale,shape));
        if (dscale)
            *dscale = -shape/scale + t*x/(scale*scale);

        if (!give_log) {
            if (dx) *dx *= v;
            if (dshape) *dshape *= v;
            if (dscale) *dscale *= v;
        }
    }
}

static void Dpweibull (double q, double shape, double scale, 
                       double *dq, double *dshape, double *dscale,
                       double v, int lower_tail, int log_p)
{
    if (q < 0) {
        if (dq) *dq = 0;
        if (dshape) *dshape = 0;
        if (dscale) *dscale = 0;
    }

    else {

        double w = pow(q/scale,shape);
        double t = exp(-w);  /* using 1-v when !log_p would be less accurate */

        if (dq) 
            *dq = t * shape * pow(q/scale,shape-1) / scale;
        if (dshape) 
            *dshape = - t * w * log(q/scale);
        if (dscale)
            *dscale = - t * shape * q * pow(q/scale,shape-1) / (scale*scale);

        if (!lower_tail) {
            if (dq) *dq = -*dq;
            if (dshape) *dshape = -*dshape;
            if (dscale) *dscale = -*dscale;
        }

        if (log_p) {
            double expv = exp(-v);
            if (dq) *dq *= expv;
            if (dshape) *dshape *= expv;
            if (dscale) *dscale *= expv;
        }
    }
}

static void Dqweibull (double p, double shape, double scale, 
                       double *dp, double *dshape, double *dscale,
                       double v, int lower_tail, int log_p)
{
    if (!R_FINITE(v)) {
        if (dp) *dp = 0;
    }

    if (dp || dshape) {
        if (dp) {
            double d = dweibull(v,shape,scale,FALSE);
            *dp = lower_tail ? 1/d : -1/d;
        }
        if (dshape) {
            double t = lower_tail ? (log_p ? log(-expm1(p)) : log1p(-p))
                                  : (log_p ? p : log(p));
            *dshape = - v * log(-t) / (shape*shape);
        }
        if (log_p) {
            if (dp) *dp *= exp(p);
        }
    }

    if (dscale) {
        *dscale = v / scale;
    }

}

/* Table of functions to compute values and derivatives for math3 functions. */

static struct { double (*fncall)(); void (*Dcall)(); } math3_table[48] = {
    { 0,	0 },
    { dbeta,	Ddbeta },
    { pbeta,	Dpbeta },
    { qbeta,	Dqbeta },
    { dbinom,	Ddbinom },
    { pbinom,	Dpbinom },
    { qbinom,	0 /* discrete */ },
    { dcauchy,	Ddcauchy },
    { pcauchy,	Dpcauchy },
    { qcauchy,	Dqcauchy },
    { df,	Ddf },
    { pf,	Dpf },
    { qf,	Dqf },
    { dgamma,	Ddgamma },
    { pgamma,	Dpgamma },
    { qgamma,	Dqgamma },
    { dlnorm,	Ddlnorm },
    { plnorm,	Dplnorm },
    { qlnorm,	Dqlnorm },
    { dlogis,	Ddlogis },
    { plogis,	Dplogis },
    { qlogis,	Dqlogis },
    { dnbinom,	Ddnbinom },
    { pnbinom,	Dpnbinom },
    { qnbinom,	0 /* discrete */ },
    { dnorm,	Ddnorm },
    { pnorm,	Dpnorm },
    { qnorm,	Dqnorm },
    { dunif,	Ddunif },
    { punif,	Dpunif },
    { qunif,	Dqunif },
    { dweibull,	Ddweibull },
    { pweibull,	Dpweibull },
    { qweibull,	Dqweibull },
    { dnchisq,	0 },
    { pnchisq,	0 },
    { qnchisq,	0 },
    { dnt,	0 },
    { pnt,	0 },
    { qnt,	0 },
    { dwilcox,	0 /* discrete */ },
    { pwilcox,	0 /* discrete */ },
    { qwilcox,	0 /* discrete */ },
    { bessel_i_ex, 0 },
    { bessel_k_ex, 0 },
    { dnbinom_mu,  Ddnbinom_mu },
    { pnbinom_mu,  Dpnbinom_mu },
    { qnbinom_mu,  0 /* discrete */ }
};

SEXP do_math3 (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res;

    checkArity(op, args);

    double (*fncall)();

    int ix = PRIMVAL(op);
    if (ix < 0 || ix > 47 || (fncall = math3_table[ix].fncall) == 0)
        errorcall (call,
                   _("unimplemented real function of %d numeric arguments"), 3);

    SEXP a1 = CAR(args); SEXP g1 = GRADIENT_IN_CELL(args); args = CDR(args);
    SEXP a2 = CAR(args); SEXP g2 = GRADIENT_IN_CELL(args); args = CDR(args);
    SEXP a3 = CAR(args); SEXP g3 = GRADIENT_IN_CELL(args); args = CDR(args);

    if (!isNumeric(a1) || !isNumeric(a2) || !isNumeric(a3))
        non_numeric_errorcall(call);

    R_len_t n1 = LENGTH(a1);
    R_len_t n2 = LENGTH(a2);
    R_len_t n3 = LENGTH(a3);

    if (n1 == 0 || n2 == 0 || n3 == 0)
        return allocVector(REALSXP,0);

    PROTECT(a1 = coerceVector (a1, REALSXP));
    PROTECT(a2 = coerceVector (a2, REALSXP));
    PROTECT(a3 = coerceVector (a3, REALSXP));

    int n; 
    n = n1; if (n2 > n) n = n2; if (n3 > n) n = n3;

    PROTECT(res = allocVector (REALSXP, n));
    DUPLICATE_ATTRIB (res, n == n1 ? a1 : n == n2 ? a2 : a3);

    int i1 = 0, i2 = 0;
    if (args != R_NilValue)
        i1 = asInteger(CAR(args));
    if (CDR(args) != R_NilValue)
        i2 = asInteger(CADR(args));

    double *ap1 = REAL(a1), *ap2 = REAL(a2), *ap3 = REAL(a3), *rp = REAL(res);
    int naflag = 0;

    /* Allocate work array for BesselI & BesselK big enough for all arguments */

    double *work = 0;
    if (fncall == bessel_i_ex || fncall == bessel_k_ex)
        work = Bessel_work_array (n2, ap2);

    R_len_t j, j1, j2, j3;

    for (j = j1 = j2 = j3 = 0; j < n; 
         j1 = (++j1==n1) ? 0 : j1,
         j2 = (++j2==n2) ? 0 : j2,
         j3 = (++j3==n3) ? 0 : j3, j++) {

        double v1 = ap1[j1];
        double v2 = ap2[j2];
        double v3 = ap3[j3];
        if (MAY_BE_NAN3(v1,v2,v3)) {
            if (ISNA(v1) || ISNA(v2) || ISNA(v3)) {
                rp[j] = NA_REAL;
                continue;
            }
            if (ISNAN(v1) || ISNAN(v2) || ISNAN(v3)) {
                rp[j] = R_NaN;
                continue;
            }
        }

        rp[j] = work ? fncall (v1, v2, v3, work)
              : args == R_NilValue ? fncall (v1, v2, v3)
              : CDR(args)==R_NilValue ? fncall (v1, v2, v3, i1)
                                      : fncall (v1, v2, v3, i1, i2);

        if (ISNAN(rp[j])) naflag = 1;
    }

    void (*Dcall)() = math3_table[ix].Dcall;

    if (Dcall!=0 && (g1!=R_NilValue || g2!=R_NilValue || g3!=R_NilValue)) {

        GRADIENT_TRACE(call);

        SEXP res_grad = R_NilValue;

        if (n == 1) {
            if (!ISNAN(rp[0])) {
                double grad1, grad2, grad3;
                double *gp1 = g1 != R_NilValue ? &grad1 : 0;
                double *gp2 = g2 != R_NilValue ? &grad2 : 0;
                double *gp3 = g3 != R_NilValue ? &grad3 : 0;
        
                if (args == R_NilValue)
                    Dcall (ap1[0], ap2[0], ap3[0], gp1, gp2, gp3, rp[0]);
                else if (CDR(args)==R_NilValue)
                    Dcall(ap1[0], ap2[0], ap3[0], gp1, gp2, gp3, rp[0], i1);
                else
                    Dcall(ap1[0], ap2[0], ap3[0], gp1, gp2, gp3, rp[0], i1, i2);
        
                res_grad = R_NilValue;
                if (g1 != R_NilValue) {
                    res_grad = copy_scaled_gradients (g1, grad1, 1);
                }
                if (g2 != R_NilValue) {
                    res_grad = res_grad == R_NilValue 
                                ? copy_scaled_gradients (g2, grad2, 1)
                                : add_scaled_gradients (res_grad, g2, grad2, 1);
                }
                if (g3 != R_NilValue) {
                    res_grad = res_grad == R_NilValue 
                                ? copy_scaled_gradients (g3, grad3, 1)
                                : add_scaled_gradients (res_grad, g3, grad3, 1);
                }
            }
        }

        else {

            SEXP gr1 = g1 == R_NilValue ? R_NilValue : allocVector (REALSXP, n);
            PROTECT(gr1);

            SEXP gr2 = g2 == R_NilValue ? R_NilValue : allocVector (REALSXP, n);
            PROTECT(gr2);

            SEXP gr3 = g3 == R_NilValue ? R_NilValue : allocVector (REALSXP, n);
            PROTECT(gr3);

            if (args == R_NilValue) {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], ap3[i%n3],
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           gr3 != R_NilValue ? REAL(gr3)+i : 0,
                           rp[i]);
            }
            else if (CDR(args) == R_NilValue) {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], ap3[i%n3],
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           gr3 != R_NilValue ? REAL(gr3)+i : 0,
                           rp[i], i1);
            }
            else {
                for (R_len_t i = 0; i < n; i++)
                    Dcall (ap1[i%n1], ap2[i%n2], ap3[i%n3],
                           gr1 != R_NilValue ? REAL(gr1)+i : 0,
                           gr2 != R_NilValue ? REAL(gr2)+i : 0,
                           gr3 != R_NilValue ? REAL(gr3)+i : 0,
                           rp[i], i1, i2);
            }

            if (g1 != R_NilValue) {
                res_grad = copy_scaled_gradients_vec (g1, gr1, n);
            }
            if (g2 != R_NilValue) {
                res_grad = res_grad == R_NilValue
                            ? copy_scaled_gradients_vec (g2, gr2, n)
                            : add_scaled_gradients_vec (res_grad, g2, gr2, n);
            }
            if (g3 != R_NilValue) {
                res_grad = res_grad == R_NilValue
                            ? copy_scaled_gradients_vec (g3, gr3, n)
                            : add_scaled_gradients_vec (res_grad, g3, gr3, n);
            }

            UNPROTECT(3);
        }

        if (res_grad != R_NilValue) {
            R_gradient = res_grad;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }
    }

    if (naflag) NaN_warning();

    UNPROTECT(4);
    return res;
}


/* MATHEMATICAL FUNCTIONS OF FOUR (REAL) ARGUMENTS. */

static void setup_Math4 (SEXP *sa, SEXP *sb, SEXP *sc, SEXP *sd, SEXP *sy, 
                         int na, int nb, int nc, int nd, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb) 
     || !isNumeric(*sc) || !isNumeric(*sd))
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


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_arithmetic[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

/* Mathematical Functions of one argument.  These are primitives.
   They are group generic and so need to eval args (possibly internally). */

{"round",	do_Math2,	10001,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"signif",	do_Math2,	10004,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log",		do_math1,	10003,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log10",	do_math1,	15,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log2",	do_math1,	14,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"abs",		do_abs,		6,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"floor",	do_math1,	1,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ceiling",	do_math1,	2,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sqrt",	do_math1,	3,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sign",	do_math1,	4,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trunc",	do_math1,	5,	1001,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"exp",		do_math1,	10,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expm1",	do_math1,	11,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log1p",	do_math1,	12,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cos",		do_math1,	20,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sin",		do_math1,	21,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tan",		do_math1,	22,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acos",	do_math1,	23,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asin",	do_math1,	24,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atan",	do_math1,	25,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cosh",	do_math1,	30,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinh",	do_math1,	31,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanh",	do_math1,	32,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acosh",	do_math1,	33,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asinh",	do_math1,	34,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atanh",	do_math1,	35,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lgamma",	do_math1,	40,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gamma",	do_math1,	41,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"digamma",	do_math1,	42,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trigamma",	do_math1,	43,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
/* see "psigamma" below !*/

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables.  Internal. */

{"atan2",	do_math2,	1,  21000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	2,  21000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	3,  21000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	4,  21000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	5,  21000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"dchisq",	do_math2,	6,  21000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2,	7,  21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2,	8,  21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2,	9,  21000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2,	10, 21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2,	11, 21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2,	12,  101000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2,	13,  101000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2,	14,  1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2,	15,  101000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2,	16,  101000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2,	17,  1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2,	18,  21000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2,	19,  21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2,	20,  21000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2,	21,  1000011,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2,	22,  1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2,	23,  1000011,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2,	24,  1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	25,  1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	26,  11000011,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	1,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3,	2,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3,	3,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3,	4,   191000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3,	5,   191000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3,	6,   1000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3,	7,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3,	8,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3,	9,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3,	10,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3,	11,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3,	12,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3,	13,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3,	14,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3,	15,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3,	16,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3,	17,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3,	18,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3,	19,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3,	20,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3,	21,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3,	22,   191000011,3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3,	23,   191000011,3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3,	24,   1000011,  3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3,	25,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3,	26,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3,	27,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3,	28,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3,	29,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3,	30,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3,	31,   31000011,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3,	32,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3,	33,   31000011,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

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

{"dnbinom_mu",	do_math3,	45,   191000011,3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3,	46,   191000011,3+2,	{PP_FUNCALL, PREC_FN,	0}},
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
