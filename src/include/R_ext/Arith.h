/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2007  The R Core Team.
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_ARITH_H_
#define R_ARITH_H_

/* Only for use where config.h has not already been included */
#if defined(HAVE_GLIBC2) && !defined(_BSD_SOURCE)
/* ensure that finite and isnan are declared - probably now obsolete */
# define _BSD_SOURCE 1
#endif

#include <R_ext/libextern.h>
#ifdef  __cplusplus
extern "C" {
#elif !defined(NO_C_HEADERS)
/* may now be unnecessary.. */
# include <math.h>
#endif

/* implementation of these : ../../main/arithmetic.c */
LibExtern double R_NaN;		/* IEEE NaN */
LibExtern int R_NaN_cast_to_int;/* Set to (int) R_NaN */
LibExtern double R_PosInf;	/* IEEE Inf */
LibExtern double R_NegInf;	/* IEEE -Inf */
LibExtern double R_NaReal;	/* NA_REAL: IEEE */
LibExtern int	 R_NaInt;	/* NA_INTEGER:= INT_MIN currently */
#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

#ifndef INT_MIN
#ifdef HAVE_LIMITS_H
#include <limits.h>
#else
#define INT_MIN ((int)(1+((~(unsigned)0)>>1))) /* assumes 2's complement ints */
#endif
#endif

/* The next two are redefined to INT_MIN directly at end of Defn.h, for 
   (possibly) faster access.  They are defined here to refer to a variable 
   because otherwise the RcppEigen package doesn't install. */
#define NA_LOGICAL	R_NaInt
#define NA_INTEGER	R_NaInt

/* #define NA_FACTOR	R_NaInt  unused */
#define NA_REAL		R_NaReal
/* NA_STRING is a SEXP, so defined in Rinternals.h */

int R_IsNA(double);		/* True for R's NA only */
int R_IsNaN(double);		/* True for any NaN that is *not* NA */
int R_finite(double);		/* True if none of NA, NaN, +/-Inf */

/* The code below also appears in Rmath.h */

#ifndef R_ISNAN_ETC_DEFINED
#define R_ISNAN_ETC_DEFINED 1

#ifdef __cplusplus

int R_isnancpp(double); /* in arithmetic.c */
#define ISNAN(x) (R_isnancpp(x))
#define ISNA(x) (R_IsNA(x))
#define R_FINITE(x) (R_finite(x))

#else

/* We need to keep ISNAN, etc. macros, since some packages do things like
   "#ifndef ISNAN"  There are also uses like "if ISNAN(x)" that are OK
   without parentheses if ISNAN is defined to have parentheses. 

   ISNAN_value does the same thing as ISNAN, but may be faster when
   the value is used as data, rather than for a conditional branch. 

   MAY_BE_NAN4(x0,x1,x2,x3) will return TRUE if any of its arguments is
   a NaN.  If none are NaN, it typically returns FALSE, but will sometimes
   return TRUE.  Similarly for MAY_BE_NAN3 and MAY_BE_NAN2. */

#include <stdint.h>

#define ISNAN(x) (ISNAN_inline_fun(x))
static inline int ISNAN_inline_fun (double x)
{
  union { double d; uint64_t u; } un;
  un.d = x;
  return (un.u << 1) > ((uint64_t)0x7ff << 53);
}

#if 0  /* two implementations, selectable based on performance tests */

static inline int MAY_BE_NAN2 (double x0, double x1)
{
  union { double d[2]; uint64_t u[2]; } un;
  un.d[0] = x0;
  un.d[1] = x1;
  return ((un.u[0] | un.u[1]) << 1) > ((uint64_t)0x7ff << 53);
}

static inline int MAY_BE_NAN3 (double x0, double x1, double x2)
{
  union { double d[3]; uint64_t u[3]; } un;
  un.d[0] = x0;
  un.d[1] = x1;
  un.d[2] = x2;
  return ((un.u[0] | un.u[1] | un.u[2]) << 1) > ((uint64_t)0x7ff << 53);
}

static inline int MAY_BE_NAN4 (double x0, double x1, double x2, double x3)
{
  union { double d[4]; uint64_t u[4]; } un;
  un.d[0] = x0;
  un.d[1] = x1;
  un.d[2] = x2;
  un.d[3] = x2;
  return ((un.u[0] | un.u[1] | un.u[2] | un.u[3]) << 1) > ((uint64_t)0x7ff<<53);
}

#else

static inline int MAY_BE_NAN2 (double x0, double x1)
{
  return ISNAN(x0+x1);
}

static inline int MAY_BE_NAN3 (double x0, double x1, double x2)
{
  return ISNAN(x0+x1+x2);
}

static inline int MAY_BE_NAN4 (double x0, double x1, double x2, double x3)
{
  return ISNAN(x0+x1+x2+x3);
}

#endif

static inline int ISNAN_value (double x)
{
  union { double d; uint64_t u; } un;
  un.d = x;
  return (un.u ^ ((un.u & (un.u-1)) + ((uint64_t)1 << 52))) >> 63;
}

#define ISNA(x) (ISNA_inline_fun(x))
static inline int ISNA_inline_fun (double x)
{
  union { double d; uint64_t u; } un;
  un.d = x;
  return ((un.u >> 52) & 0x7ff) == 0x7ff
           && (un.u & (((uint64_t)1<<32)-1)) == 1954;
}

#define R_FINITE(x) (R_FINITE_inline_fun(x))
static inline int R_FINITE_inline_fun (double x)
{
  union { double d; uint64_t u; } un;
  un.d = x;
  return (un.u << 1) < ((uint64_t)0x7ff << 53);
}

#endif

#endif

#ifdef  __cplusplus
}
#endif

#endif /* R_ARITH_H_ */
