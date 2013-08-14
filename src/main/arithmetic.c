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

#ifndef _AIX
const double R_Zero_Hack = 0.0;	/* Silence the Sun compiler */
#else
double R_Zero_Hack = 0.0;
#endif
typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;

/* gcc had problems with static const on AIX and Solaris
   Solaris was for gcc 3.1 and 3.2 under -O2 32-bit on 64-bit kernel */
#ifdef _AIX
#define CONST
#elif defined(sparc) && defined (__GNUC__) && __GNUC__ == 3
#define CONST
#else
#define CONST const
#endif

#ifdef WORDS_BIGENDIAN
static CONST int hw = 0;
static CONST int lw = 1;
#else  /* !WORDS_BIGENDIAN */
static CONST int hw = 1;
static CONST int lw = 0;
#endif /* WORDS_BIGENDIAN */


static double R_ValueOfNA(void)
{
    /* The gcc shipping with RedHat 9 gets this wrong without
     * the volatile declaration. Thanks to Marc Schwartz. */
    volatile ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

int R_IsNA(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

int R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] != 1954);
    }
    return 0;
}

/* ISNAN uses isnan, which is undefined by C++ headers
   This workaround is called only when ISNAN() is used
   in a user code in a file with __cplusplus defined */

int R_isnancpp(double x)
{
   return (isnan(x)!=0);
}


/* Mainly for use in packages */
int R_finite(double x)
{
#ifdef HAVE_WORKING_ISFINITE
    return isfinite(x);
#else
    return (!isnan(x) & (x != R_PosInf) & (x != R_NegInf));
#endif
}


/* Arithmetic Initialization */

void attribute_hidden InitArithmetic()
{
    R_NaInt = INT_MIN; /* now mostly unused: NA_INTEGER defined as INT_MIN */
    R_NaN = 0.0/R_Zero_Hack;
    R_NaReal = R_ValueOfNA();
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
    R_NaN_cast_to_int = (int) R_NaN;

#ifdef ENABLE_ISNAN_TRICK
    if (R_NaN_cast_to_int != (int) R_NaReal
     || R_NaN_cast_to_int != (int) (-R_NaReal)
     || R_NaN_cast_to_int != (int) (-R_NaN))
        error("Integer casts of NaN, NA, -NaN, -NA differ, don't define ENABLE_ISNAN_TRICK");
#endif
}

/* Keep these two in step */
/* FIXME: consider using
    tmp = (long double)x1 - floor(q) * (long double)x2;
    
   WARNING:  myfmod may call "warning", and hence may allocate storage.
 */
static double myfmod(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return R_NaN;
    tmp = x1 - floor(q) * x2;
    if(R_FINITE(q) && (fabs(q) > 1/R_AccuracyInfo.eps))
	warning(_("probable complete loss of accuracy in modulus"));
    q = floor(tmp/x2);
    return tmp - q * x2;
}

static double myfloor(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp/x2);
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

    if (ISNAN(x) || ISNAN(y))
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


/* Unary and Binary Operators */

static SEXP do_fast_arith (SEXP call, SEXP op, SEXP arg1, SEXP arg2, SEXP env,
                           int variant)
{
    /* Quickly do real arithmetic and integer plus/minus on scalars with 
       no attributes. */

    int type = TYPEOF(arg1);

    if ((type==REALSXP || type==INTSXP) && LENGTH(arg1)==1 
                                        && ATTRIB(arg1)==R_NilValue) {
        int opcode = PRIMVAL(op);

        if (arg2==NULL) {
            switch (opcode) {
            case PLUSOP:
                return arg1;
            case MINUSOP: ;
                SEXP ans = NAMEDCNT_EQ_0(arg1) ? arg1 : allocVector(type,1);
                WAIT_UNTIL_COMPUTED(arg1);
                if (type==REALSXP) 
                    *REAL(ans) = - *REAL(arg1);
                else /* INTSXP */
                    *INTEGER(ans) = *INTEGER(arg1)==NA_INTEGER ? NA_INTEGER
                                      : - *INTEGER(arg1);
                return ans;
            }
        }
        else if (TYPEOF(arg2)==type && LENGTH(arg2)==1 
                                    && ATTRIB(arg2)==R_NilValue) {
            if (type==REALSXP) {

                SEXP ans = NAMEDCNT_EQ_0(arg1) ? arg1 
                         : NAMEDCNT_EQ_0(arg2) ? arg2
                         : allocVector(type,1);

                WAIT_UNTIL_COMPUTED_2(arg1,arg2);
    
                double a1 = *REAL(arg1), a2 = *REAL(arg2);
        
                switch (opcode) {
                case PLUSOP:
                    *REAL(ans) = a1 + a2;
                    return ans;
                case MINUSOP:
                    *REAL(ans) = a1 - a2;
                    return ans;
                case TIMESOP:
                    *REAL(ans) = a1 * a2;
                    return ans;
                case DIVOP:
                    *REAL(ans) = a1 / a2;
                    return ans;
                case POWOP:
                    if (a2 == 2.0)       *REAL(ans) = a1 * a1;
                    else if (a2 == 1.0)  *REAL(ans) = a1;
                    else if (a2 == 0.0)  *REAL(ans) = 1.0;
                    else if (a2 == -1.0) *REAL(ans) = 1.0 / a1;
                    else                 *REAL(ans) = R_pow(a1,a2);
                    return ans;
                case MODOP:
                    PROTECT(ans);
                    *REAL(ans) = myfmod(a1,a2);
                    UNPROTECT(1);
                    return ans;
                case IDIVOP:
                    *REAL(ans) = myfloor(a1,a2);
                    return ans;
                }
            }
            else if (opcode==PLUSOP || opcode==MINUSOP) { /* type==INTSXP */

                SEXP ans = NAMEDCNT_EQ_0(arg1) ? arg1 
                         : NAMEDCNT_EQ_0(arg2) ? arg2
                         : allocVector(type,1);

                WAIT_UNTIL_COMPUTED_2(arg1,arg2);

                int a1 = *INTEGER(arg1), a2 = *INTEGER(arg2);

                if (a1==NA_INTEGER || a2==NA_INTEGER) {
                    *INTEGER(ans) = NA_INTEGER;
                    return ans;
                }

                if (opcode==MINUSOP) a2 = -a2;

                *INTEGER(ans) = a1 + a2;

                if (a1>0 ? (a2>0 && *INTEGER(ans)<0) 
                         : (a2<0 && *INTEGER(ans)>0)) {
                    PROTECT(ans);
                    warningcall(call, _("NAs produced by integer overflow"));
                    UNPROTECT(1);
                    *INTEGER(ans) = NA_INTEGER;
                }

                return ans;
            }
        }
    }

    /* Otherwise, handle the general case. */

    return arg2==NULL ? R_unary (call, op, arg1, variant) 
                      : R_binary (call, op, arg1, arg2, variant);
}

SEXP attribute_hidden do_arith (SEXP call, SEXP op, SEXP args, SEXP env,
                                int variant)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    if (PRIMFUN_FAST(op)==0)
        SET_PRIMFUN_FAST_BINARY (op, do_fast_arith, 1, 1, 0, 0, 
                                 PRIMVAL(op)==PLUSOP || PRIMVAL(op)==MINUSOP);
    switch (length(args)) {
    case 1:
	return R_unary(call, op, CAR(args), variant);
    case 2:
	return R_binary(call, op, CAR(args), CADR(args), variant);
    default:
	errorcall(call,_("operator needs one or two arguments"));
    }
    return ans;			/* never used; to keep -Wall happy */
}


/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

#define add_func(a,b) ((a)+(b))
#define sub_func(a,b) ((a)-(b))
#define mul_func(a,b) ((a)*(b))
#define div_func(a,b) ((a)/(b))

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
                    do { \
                        result[i] = func(fetch1(s1,i),tmp); \
                        i += 1; \
                    } while (i<=u); \
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
                    do { \
                        result[i] = func(tmp,fetch2(s2,i)); \
                        i += 1; \
                    } while (i<=u); \
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
                    do { \
                        result[i] = func(fetch1(s1,i),fetch2(s2,i)); \
                        i += 1; \
                    } while (i<=u); \
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

#define RFETCH(_s_,_i_) (REAL(_s_)[_i_])
#define RIFETCH(_s_,_i_) \
   ((double) (INTEGER(_s_)[_i_] == NA_INTEGER ? NA_REAL : INTEGER(_s_)[_i_]))

/* The tests using integer comparisons are a bit faster than the tests
   using doubles, but they depend on a two's complement representation
   (but that is almost universal).  The tests that compare results to
   double's depend on being able to accurately represent all int's as
   double's.  Since int's are almost universally 32 bit that should be
   OK. */

#ifndef INT_32_BITS
/* configure checks whether int is 32 bits.  If not this code will
   need to be rewritten.  Since 32 bit ints are pretty much universal,
   we can worry about writing alternate code when the need arises.
   To be safe, we signal a compiler error if int is not 32 bits. */
# error code requires that int have 32 bits
#else
/* Just to be on the safe side, configure ought to check that the
   mashine uses two's complement. A define like
#define USES_TWOS_COMPLEMENT (~0 == (unsigned) -1)
   might work, but at least one compiler (CodeWarrior 6) chokes on it.
   So for now just assume it is true.
*/
#define USES_TWOS_COMPLEMENT 1

#if USES_TWOS_COMPLEMENT
# define OPPOSITE_SIGNS(x, y) ((x < 0) ^ (y < 0))
# define GOODISUM(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
# define GOODIDIFF(x, y, z) (!(OPPOSITE_SIGNS(x, y) && OPPOSITE_SIGNS(x, z)))
#else
# define GOODISUM(x, y, z) ((double) (x) + (double) (y) == (z))
# define GOODIDIFF(x, y, z) ((double) (x) - (double) (y) == (z))
#endif
#define GOODIPROD(x, y, z) ((double) (x) * (double) (y) == (z))
#endif

static int integer_overflow;  /* Set by task_arithmetic_op on integer overflow
                                 (only in a master-now task or a direct call) */

void task_integer_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    int x1, x2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    switch (code) {
    case PLUSOP:
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                INTEGER(ans)[i] = NA_INTEGER;
            else {
                int val = x1 + x2;
                if (val != NA_INTEGER && GOODISUM(x1, x2, val))
                    INTEGER(ans)[i] = val;
                else {
                    INTEGER(ans)[i] = NA_INTEGER;
                    integer_overflow = TRUE;
                }
            }
        }
        break;
    case MINUSOP:
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                INTEGER(ans)[i] = NA_INTEGER;
            else {
                int val = x1 - x2;
                if (val != NA_INTEGER && GOODIDIFF(x1, x2, val))
                    INTEGER(ans)[i] = val;
                else {
                    integer_overflow = TRUE;
                    INTEGER(ans)[i] = NA_INTEGER;
                }
            }
        }
        break;
    case TIMESOP:
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                INTEGER(ans)[i] = NA_INTEGER;
            else {
                int val = x1 * x2;
                if (val != NA_INTEGER && GOODIPROD(x1, x2, val))
                    INTEGER(ans)[i] = val;
                else {
                    integer_overflow = TRUE;
                    INTEGER(ans)[i] = NA_INTEGER;
                }
            }
        }
        break;
    case DIVOP:
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER)
                    REAL(ans)[i] = NA_REAL;
                else
                    REAL(ans)[i] = (double) x1 / (double) x2;
        }
        break;
    case POWOP:
        mod_iterate(n1, n2, i1, i2) {
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
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
                INTEGER(ans)[i] = NA_INTEGER;
            else {
                INTEGER(ans)[i] = /* till 0.63.2: x1 % x2 */
                    (x1 >= 0 && x2 > 0) ? x1 % x2 :
                    (int)myfmod((double)x1,(double)x2);
            }
        }
        break;
    case IDIVOP:
        mod_iterate(n1, n2, i1, i2) {
            x1 = INTEGER(s1)[i1];
            x2 = INTEGER(s2)[i2];
            /* This had x %/% 0 == 0 prior to 2.14.1, but
               it seems conventionally to be undefined */
            if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
                INTEGER(ans)[i] = NA_INTEGER;
            else
                INTEGER(ans)[i] = floor((double)x1 / (double)x2);
        }
        break;
    }
}

void task_real_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    unsigned n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    HELPERS_SETUP_OUT (6);

    switch (code) {
    case PLUSOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,add_func,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,add_func,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,add_func,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case MINUSOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,sub_func,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,sub_func,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,sub_func,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case TIMESOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,mul_func,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,mul_func,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,mul_func,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case DIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,div_func,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,div_func,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,div_func,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
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
                        do {
                            double tmp2 = RFETCH(s1,i);
                            REAL(ans)[i] = tmp2 * tmp2;
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
            else if (tmp == 1.0)
                while (i<n) {
                    HELPERS_WAIT_IN1 (a, i, n);
                    do {
                        R_len_t u = HELPERS_UP_TO(i,a);
                        do {
                            REAL(ans)[i] = RFETCH(s1,i);
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
                            REAL(ans)[i] = 1.0;
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
                            REAL(ans)[i] = 1.0 / RFETCH(s1,i);
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
                            REAL(ans)[i] = R_pow (RFETCH(s1,i), tmp);
                            i += 1;
                        } while (i<=u);
                        helpers_amount_out(i);
                    } while (i<a);
                }
        }
        else if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,R_POW,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,R_POW,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,R_POW,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case MODOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfmod,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfmod,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,myfmod,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    case IDIVOP:
        if (TYPEOF(s1) != REALSXP)
            PIPEARITH(double,myfloor,REAL(ans),n,RIFETCH,s1,n1,RFETCH,s2,n2);
        else if (TYPEOF(s2) != REALSXP)
            PIPEARITH(double,myfloor,REAL(ans),n,RFETCH,s1,n1,RIFETCH,s2,n2);
        else
            PIPEARITH(double,myfloor,REAL(ans),n,RFETCH,s1,n1,RFETCH,s2,n2);
        break;
    }
}

extern double complex R_cpow (double complex, double complex);

void task_complex_arithmetic (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int i,i1, i2, n, n1, n2;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = n1>n2 ? n1 : n2;

    switch (code) {
    case PLUSOP:
        mod_iterate(n1, n2, i1, i2) {
            Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
            COMPLEX(ans)[i].r = x1.r + x2.r;
            COMPLEX(ans)[i].i = x1.i + x2.i;
        }
        break;
    case MINUSOP:
        mod_iterate(n1, n2, i1, i2) {
            Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
            COMPLEX(ans)[i].r = x1.r - x2.r;
            COMPLEX(ans)[i].i = x1.i - x2.i;
        }
        break;
    case TIMESOP:
        mod_iterate(n1, n2, i1, i2) {
            R_from_C99_complex (COMPLEX(ans)+i,
                                C99_from_R_complex(COMPLEX(s1)+i1) 
                                 * C99_from_R_complex(COMPLEX(s2)+i2));
        }
        break;
    case DIVOP:
        mod_iterate(n1, n2, i1, i2) {
            R_from_C99_complex (COMPLEX(ans)+i,
                                C99_from_R_complex(COMPLEX(s1)+i1) 
                                 / C99_from_R_complex(COMPLEX(s2)+i2));
        }
        break;
    case POWOP:
        mod_iterate(n1, n2, i1, i2) {
            R_from_C99_complex (COMPLEX(ans)+i,
                                R_cpow (C99_from_R_complex(COMPLEX(s1)+i1),
                                        C99_from_R_complex(COMPLEX(s2)+i2)));
        }
        break;
    }
}

#define COERCE_IF_NEEDED(v, tp, vpi) do { \
    if (TYPEOF(v) != (tp)) { \
        int __vo__ = OBJECT(v); \
        WAIT_UNTIL_COMPUTED(v); \
        REPROTECT(v = coerceVector(v, (tp)), vpi); \
        if (__vo__) SET_OBJECT(v, 1); \
    } \
} while (0)

#define FIXUP_NULL_AND_CHECK_TYPES(v, vpi) do { \
    switch (TYPEOF(v)) { \
    case NILSXP: REPROTECT(v = allocVector(REALSXP,0), vpi); break; \
    case CPLXSXP: case REALSXP: case INTSXP: case LGLSXP: break; \
    default: errorcall(call, _("non-numeric argument to binary operator")); \
    } \
} while (0)

#define T_arithmetic THRESHOLD_ADJUST(24)  /* further adjusted below */

SEXP attribute_hidden R_binary (SEXP call, SEXP op, SEXP x, SEXP y, int variant)
{
    LOCAL_COPY(R_NilValue);
    helpers_task_proc *task;
    SEXP klass, dims, tsp, xnames, ynames, ans;
    int mismatch = 0, nx, ny, n, xarray, yarray, xts, yts, xS4 = 0, yS4 = 0;
    int xattr, yattr;
    PROTECT_INDEX xpi, ypi;
    ARITHOP_TYPE oper = (ARITHOP_TYPE) PRIMVAL(op);
    int threshold, flags, nprotect;

    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);
    nprotect = 2;

    FIXUP_NULL_AND_CHECK_TYPES(x, xpi);
    FIXUP_NULL_AND_CHECK_TYPES(y, ypi);

    nx = LENGTH(x);
    if (ATTRIB(x) != R_NilValue) {
        xattr = TRUE;
        xarray = isArray(x);
        xts = isTs(x);
        xS4 = isS4(x);
    }
    else xarray = xts = xattr = FALSE;
    ny = LENGTH(y);
    if (ATTRIB(y) != R_NilValue) {
        yattr = TRUE;
        yarray = isArray(y);
        yts = isTs(y);
        yS4 = isS4(y);
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
            PROTECT(klass = getAttrib(x, R_ClassSymbol));
        }
        else if (xts) {
            if (nx < ny)
                ErrorMessage(call, ERROR_TSVEC_MISMATCH);
            PROTECT(tsp = getAttrib(x, R_TspSymbol));
            PROTECT(klass = getAttrib(x, R_ClassSymbol));
        }
        else {                        /* (yts) */
            if (ny < nx)
                ErrorMessage(call, ERROR_TSVEC_MISMATCH);
            PROTECT(tsp = getAttrib(y, R_TspSymbol));
            PROTECT(klass = getAttrib(y, R_ClassSymbol));
        }
        nprotect += 2;
    }
    else klass = tsp = NULL; /* -Wall */

    if (mismatch)
        warningcall (call,
          _("longer object length is not a multiple of shorter object length"));

    /* S4-compatibility change: if nx or ny is 0, result is of length 0 */

    n = nx==0 || ny==0 ? 0 : nx>ny ? nx : ny;

    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
        if (oper==IDIVOP || oper==MODOP)
            errorcall(call,_("unimplemented complex operation"));
        COERCE_IF_NEEDED(x, CPLXSXP, xpi);
        COERCE_IF_NEEDED(y, CPLXSXP, ypi);
        ans = can_save_alloc (x, y, CPLXSXP);
        if (ans==R_NilValue)
            ans = allocVector(CPLXSXP, n);
        task = task_complex_arithmetic;
        flags = 0;  /* Not bothering with pipelining yet. */
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
         /* task_real_arithmetic takes REAL, INT, and LOGICAL operands, 
            and assumes INT and LOGICAL are really the same. */
        ans = can_save_alloc (x, y, REALSXP);
        if (ans==R_NilValue)
            ans = allocVector(REALSXP, n);
        task = task_real_arithmetic;
        flags = HELPERS_PIPE_IN0_OUT;
        if (oper <= POWOP) { /* this is +, -, *, /, and ^ operators */
            if (TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP)
                flags = HELPERS_PIPE_IN0_OUT | HELPERS_MERGE_IN_OUT;
        }
        else if (oper == MODOP)
            flags = HELPERS_PIPE_IN0 |
                    HELPERS_MASTER_NOW; /* since it can produce a warning msg */
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
        else {
            ans = can_save_alloc (x, y, INTSXP);
            if (ans==R_NilValue) 
                ans = allocVector(INTSXP, n);
        }
        task = task_integer_arithmetic;

       /* Only ^, /, and %/% can be done in helpers at present - others must be
          in the master because of possible integer overflow or myfmod warning.
          Not bothering with pipelining yet. */

        flags = oper == POWOP || oper == DIVOP || oper == IDIVOP ? 0 
              : HELPERS_MASTER_NOW;
    }

    PROTECT(ans);
    nprotect++;

    /* Do the actual operation. */

    if (n!=0) {

#ifdef R_MEMORY_PROFILING
        if (RTRACE(x) || RTRACE(y)) {
           if (RTRACE(x) && RTRACE(y)) {
              if (nx > ny)
                  memtrace_report(x, ans);
              else
                  memtrace_report(y, ans);
           } else if (RTRACE(x))
               memtrace_report(x, ans);
           else /* only y */
               memtrace_report(y, ans);
           SET_RTRACE(ans, 1);
        }
#endif
        integer_overflow = 0;

        threshold = T_arithmetic;
        if (TYPEOF(ans)==CPLXSXP) threshold >>= 1;
        if (oper!=PLUSOP && oper!=MINUSOP) threshold >>= 1;
        if (oper==POWOP) threshold >>= 1;

        DO_NOW_OR_LATER2 (variant, n > 1 && n >= threshold,
                          flags, task, oper, ans, x, y);

        if (integer_overflow)
            warningcall(call, _("NAs produced by integer overflow"));
    }

    /* quick return if there are no attributes */

    if (! xattr && ! yattr) {
        UNPROTECT(nprotect);
        return ans;
    }

    /* Copy attributes from arguments as needed. */

    if (yattr && ny==n && ans!=y)
        copyMostAttrib(y, ans);
    if (xattr && nx==n && ans!=x)
        copyMostAttrib(x, ans); /* Done 2nd so x's attrs overwrite y's */

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

    UNPROTECT(nprotect);
    return ans;
}

void task_unary_minus (helpers_op_t op, SEXP ans, SEXP s1, SEXP ignored)
{
    R_len_t n = LENGTH(s1);
    R_len_t i = 0;
    R_len_t a;

    HELPERS_SETUP_OUT (8);

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

SEXP attribute_hidden R_unary (SEXP call, SEXP op, SEXP s1, int variant)
{
    ARITHOP_TYPE operation = (ARITHOP_TYPE) PRIMVAL(op);
    int type = TYPEOF(s1);
    SEXP ans;
    int n;

    if ( ! ((NUMBER_TYPES >> type) & 1))
        errorcall(call, _("invalid argument to unary operator"));

    if (operation==PLUSOP) 
        return s1;

    n = LENGTH(s1);
    PROTECT(ans = NAMEDCNT_EQ_0(s1) ? s1 
                : allocVector (type==LGLSXP ? INTSXP : type, n));

    if (operation==MINUSOP) {
        DO_NOW_OR_LATER1 (variant, n >= T_unary_minus,
          TYPEOF(s1)==REALSXP ? HELPERS_PIPE_IN01_OUT | HELPERS_MERGE_IN_OUT
                              : HELPERS_PIPE_IN01_OUT,
          task_unary_minus, 0, ans, s1);
        if (ans != s1) {
            DUPLICATE_ATTRIB(ans,s1);
        }
    }
    else
        errorcall(call, _("invalid argument to unary operator"));

    UNPROTECT(1);
    return ans;
}


/* MATHEMATICAL FUNCTIONS OF ONE ARGUMENT.  Implements a variant return
   of the sum of the vector result, rather than the vector itself. */

/* Table to map math1 operation code to function.  The entries for fabs, trunc,
   and R_log are not called via do_math1 like the others, but from special
   primitives. */

double (*R_math1_func_table[44])(double) = {
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

char R_math1_err_table[44] = {
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

static SEXP math1(SEXP sa, unsigned opcode, SEXP call, int variant)
{
    SEXP sy;
    int n;

    if (opcode == 10003) /* horrible kludge for log */
        opcode = 13;
    else if (opcode >= 44)
        errorcall(call, _("unimplemented real function of 1 argument"));

    if (!isNumeric(sa))
        errorcall(call, R_MSG_NONNUM_MATH);

    n = LENGTH(sa);
    /* coercion can lose the object bit */
    PROTECT(sa = coerceVector(sa, REALSXP));

    R_naflag = 0;

    /* Note: need to protect sy below because some ops may produce a warning. */

    if (VARIANT_KIND(variant) == VARIANT_SUM) {

        PROTECT(sy = allocVector(REALSXP, 1));
        DO_NOW_OR_LATER1 (variant, 
                       LENGTH(sa) >= T_math1 && R_math1_err_table[opcode] == 0,
                       HELPERS_PIPE_IN1, task_sum_math1, opcode, sy, sa);
        SET_ATTRIB (sy, R_VariantResult);
        UNPROTECT(2);
    }

    else { /* non-variant result */

        PROTECT(sy = NAMEDCNT_EQ_0(sa) ? sa : allocVector(REALSXP, n));
#ifdef R_MEMORY_PROFILING
        if (RTRACE(sa)){
           memtrace_report(sa, sy);
           SET_RTRACE(sy, 1);
        }
#endif
        DO_NOW_OR_LATER1 (variant,
                       LENGTH(sa) >= T_math1 && R_math1_err_table[opcode] == 0,
                       HELPERS_PIPE_IN01_OUT | HELPERS_MERGE_IN_OUT, 
                       task_math1, opcode, sy, sa);
        if (sa!=sy) 
            DUPLICATE_ATTRIB(sy, sa);
        UNPROTECT(2);
    }

    if (R_naflag) warningcall (call, R_MSG_NA);

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

    return math1 (arg, PRIMVAL(op), call, variant);
}


SEXP attribute_hidden do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (PRIMFUN_FAST(op)==0)
        SET_PRIMFUN_FAST_UNARY (op, do_fast_math1, 1, 0);

    return do_fast_math1 (call, op, CAR(args), env, 0);
}

/* Methods for trunc are allowed to have more than one arg */

static SEXP do_fast_trunc (SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    return math1(arg, 5, call, variant);
}

SEXP attribute_hidden do_trunc(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    check1arg_x (args, call);
    if (isComplex(CAR(args)))
	errorcall(call, _("unimplemented complex function"));

    if (PRIMFUN_FAST(op)==0)
        SET_PRIMFUN_FAST_UNARY (op, do_fast_trunc, 1, 0);

    return math1(CAR(args), 5, call, 0);
}

/* Note that abs is slightly different from the do_math1 set, both
   for integer/logical inputs and what it dispatches to for complex ones,
   but uses math1 when the argument is real. */

static SEXP do_fast_abs (SEXP call, SEXP op, SEXP x, SEXP env, int variant)
{   
    SEXP s;

    if (TYPEOF(x) == REALSXP)
        return math1 (x, 0, call, variant);

    else if (isInteger(x) || isLogical(x)) {
	/* integer or logical ==> return integer,
	   factor was covered by Math.factor. */
        int n = LENGTH(x);
	s = NAMEDCNT_EQ_0(x) && TYPEOF(x)==INTSXP ? x : allocVector(INTSXP, n);
        WAIT_UNTIL_COMPUTED(x);
	/* Note: relying on INTEGER(.) === LOGICAL(.) : */
	for (int i = 0 ; i < n ; i++) {
            int v = INTEGER(x)[i];
	    INTEGER(s)[i] = v==NA_INTEGER ? NA_INTEGER : v<0 ? -v : v;
        }
    }

    else if (isComplex(x)) {
        SEXP args;
        PROTECT (args = CONS(x,R_NilValue));
        WAIT_UNTIL_COMPUTED(x);
	s = do_cmathfuns(call, op, args, env);
        UNPROTECT(1);
    }

    else
	errorcall(call, R_MSG_NONNUM_MATH);

    if (x!=s) {
        PROTECT(s);
        DUPLICATE_ATTRIB(s, x);
        UNPROTECT(1);
    }

    return s;
}

SEXP attribute_hidden do_abs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (PRIMFUN_FAST(op)==0)
        SET_PRIMFUN_FAST_UNARY (op, do_fast_abs, 1, 0);

    return do_fast_abs (call, op, CAR(args), env, 0);
}

/* Mathematical Functions of Two Numeric Arguments (plus 0, 1, or 2 integers) */

static void setup_Math2 
    (SEXP *sa, SEXP *sb, SEXP *sy, int na, int nb, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

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

#ifdef R_MEMORY_PROFILING
    if (RTRACE(*sa) || RTRACE(*sb)) {
       if (RTRACE(*sa) && RTRACE(*sb)){
	  if (na > nb)
	      memtrace_report(*sa, *sy);
	  else
	      memtrace_report(*sb, *sy);
       } else if (RTRACE(*sa))
	   memtrace_report(*sa, *sy);
       else /* only s2 */
	   memtrace_report(*sb, *sy);
       SET_RTRACE(*sy, 1);
    }
#endif
}

#define DO_MATH2(y,a,b,n,na,nb,fncall) do { \
    int naflag = 0; \
    double ai, bi; \
    int i, ia, ib; \
    mod_iterate(na, nb, ia, ib) { \
        ai = a[ia]; \
        bi = b[ib]; \
        if (ISNAN(ai+bi)) { \
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
    if (naflag) warning(R_MSG_NA); \
    if (n == na)  DUPLICATE_ATTRIB(sy, sa); \
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb); \
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

SEXP attribute_hidden do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
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
    return op;			/* never used; to keep -Wall happy */
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2;
    int n, nprotect = 2;

    if (length(args) >= 2 &&
	isSymbol(CADR(args)) && R_isMissing(CADR(args), env)) {
	double digits = 0;
	if(PRIMVAL(op) == 10004) digits = 6.0;
	PROTECT(args = list2(CAR(args), ScalarReal(digits))); nprotect++;
    }

    PROTECT(args = evalListKeepMissing(args, env));
    PROTECT(call2 = lang2(CAR(call), R_NilValue));
    SETCDR(call2, args);

    n = length(args);

    if (n != 1 && n != 2)
	error(_("%d arguments passed to '%s' which requires 1 or 2"),
	      n, PRIMNAME(op));

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	if(n == 1) {
	    double digits = 0.0;
            check1arg_x (args, call);
	    if(PRIMVAL(op) == 10004) digits = 6.0;
	    SETCDR(args, CONS(ScalarReal(digits), R_NilValue));
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
    return res;
}

/* log{2,10} are builtins */
SEXP attribute_hidden do_log1arg(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2, args2, tmp = R_NilValue /* -Wall */;

    checkArity(op, args);
    check1arg_x (args, call);

    if (DispatchGroup("Math", call, op, args, env, &res)) return res;

    if(PRIMVAL(op) == 10) tmp = ScalarReal(10.0);
    if(PRIMVAL(op) == 2)  tmp = ScalarReal(2.0);

    PROTECT(call2 = lang3(install("log"), CAR(args), tmp));
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
SEXP attribute_hidden do_log (SEXP call, SEXP op, SEXP args, SEXP env,
                              int variant)
{
    int nprotect = 2;
    int n;

    /* Do the common case of one un-tagged, non-object, argument quickly. */
    if (!isNull(args) && isNull(CDR(args)) && isNull(TAG(args)) 
          && CAR(args) != R_DotsSymbol && CAR(args) != R_MissingArg) {
        SEXP arg, ans;
        PROTECT(arg = evalv (CAR(args), env, VARIANT_PENDING_OK));
        if (isObject(arg)) {
            WAIT_UNTIL_COMPUTED(arg);
            UNPROTECT(1);
            PROTECT(args = CONS(arg, R_NilValue));
            n = 1;
        }
        else {
            ans = do_fast_math1 (call, op, arg, env, variant);
            UNPROTECT(1);
            return ans;
        }
    }
    else {
        n = length(args);

        /* This seems like some sort of horrible kludge that can't possibly
           be right in general (it ignores the argument names, and silently
           discards arguments after the first two). */
        if (n >= 2 && isSymbol(CADR(args)) && R_isMissing(CADR(args), env)) {
#ifdef M_E
	    double e = M_E;
#else
	    double e = exp(1.);
#endif
	    PROTECT(args = list2(CAR(args), ScalarReal(e))); nprotect++;
        }
        PROTECT(args = evalListKeepMissing(args, env));
    }

    SEXP res, call2;
    PROTECT(call2 = lang2(CAR(call), R_NilValue));
    SETCDR(call2, args);
    n = length(args);

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	switch (n) {
	case 1:
            check1arg_x (args, call);
	    if (isComplex(CAR(args)))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(CAR(args), 13, call, variant);
	    break;
	case 2:
	{
	    /* match argument names if supplied */
            static char *ap[2] = { "x", "base" };
	    PROTECT(args = matchArgs(R_NilValue, ap, 2, args, call));
	    nprotect += 1;
	    if (length(CADR(args)) == 0)
		errorcall(call, _("invalid argument 'base' of length 0"));
	    if (isComplex(CAR(args)) || isComplex(CADR(args)))
		res = complex_math2(call, op, args, env);
	    else
		res = math2(CAR(args), CADR(args), logbase, call);
	    break;
	}
	default:
	    error(_("%d arguments passed to 'log' which requires 1 or 2"), n);
	}
    }
    UNPROTECT(nprotect);
    return res;
}


/* Mathematical Functions of Three (Real) Arguments */

static void setup_Math3
    (SEXP *sa, SEXP *sb, SEXP *sc, SEXP *sy, int na, int nb, int nc, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb) || !isNumeric(*sc))
	errorcall(lcall, R_MSG_NONNUM_MATH);

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

#ifdef R_MEMORY_PROFILING
    if (RTRACE(*sa) || RTRACE(*sb) || RTRACE(*sc)) {
       if (RTRACE(*sa))
	  memtrace_report(*sa, *sy);
       else if (RTRACE(*sb))
	  memtrace_report(*sb, *sy);
       else if (RTRACE(*sc))
	  memtrace_report(*sc, *sy);
       SET_RTRACE(*sy, 1);
    }
#endif
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
        if (ISNAN(ai+bi+ci)) { \
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
    if (naflag) warning(R_MSG_NA); \
    if (n == na) DUPLICATE_ATTRIB(sy, sa); \
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb); \
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc); \
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

SEXP attribute_hidden do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
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
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

static void setup_Math4 (SEXP *sa, SEXP *sb, SEXP *sc, SEXP *sd, SEXP *sy, 
                         int na, int nb, int nc, int nd, SEXP lcall)
{
    if (!isNumeric(*sa) || !isNumeric(*sb) || !isNumeric(*sc) || !isNumeric(*sd))
	errorcall(lcall, R_MSG_NONNUM_MATH);

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
        if (ISNAN(ai+bi+ci+di)) { \
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
    if (naflag) warning(R_MSG_NA); \
    if (n == na) DUPLICATE_ATTRIB(sy, sa); \
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb); \
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc); \
    else if (n == nd) DUPLICATE_ATTRIB(sy, sd); \
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


SEXP attribute_hidden do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
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
    return op;			/* never used; to keep -Wall happy */
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
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
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
    if(naflag)					\
	warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) DUPLICATE_ATTRIB(sy, sd);	\
    else if (n == ne) DUPLICATE_ATTRIB(sy, se);	\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP attribute_hidden do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
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
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */


/* This is used for experimenting with parallelized nmath functions -- LT */
CCODE R_get_arith_function(int which)
{
    switch (which) {
    case 1: return do_math1;
    case 2: return do_math2;
    case 3: return do_math3;
    case 4: return do_math4;
    case 11: return complex_math1;
    case 12: return complex_math2;
    default: error("bad arith function index"); return NULL;
    }
}
