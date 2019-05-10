/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2017, 2018, 2019 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Core Team
 *  Copyright (C) 2003--2008  The R Foundation
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
#include <Defn.h>
#include <R_ext/Random.h>
#include <R_ext/Applic.h>	/* for rcont2() */
#include <Rmath.h>		/* for rxxx functions */
#include <errno.h>

R_NORETURN static void invalid(SEXP call)
{
    error(_("invalid arguments"));
}


/* Random sampling from 1-parameter families. */

/* Derivatives for random1 generators of continuous distributions. */

static double Drexp (double r, double scale)
{
    return scale <= 0 ? 0 : r / scale;
}

/* Table of functions for generation and derivatives of random1 distibutions. */

static struct { 
    double (*fncall)(double); double (*Dcall)(double, double); 
} rand1_table[6] = 
{
    { rchisq,	0 },
    { rexp,	Drexp },
    { rgeom,	0 /* discrete */ },
    { rpois,	0 /* discrete */ },
    { rt,	0 },
    { rsignrank,0 /* discrete */ }
};

static SEXP do_random1(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a;
    int n, na;

    checkArity(op, args);

    if (!isVector(CAR(args)) || !isNumeric(a = CADR(args)))
        invalid(call);

    if (LENGTH(CAR(args)) == 1) {
        n = asInteger(CAR(args));
        if (n == NA_INTEGER || n < 0)
            invalid(call);
    }
    else
        n = LENGTH(CAR(args));

    if (TYPEOF(a) != REALSXP) a = coerceVector(a, REALSXP);
    PROTECT(a);
    na = LENGTH(a);

    int opcode = PRIMVAL(op);

    if (opcode < 0 || opcode >= sizeof rand1_table / sizeof rand1_table[0])
        error(_("internal error in do_random1"));
    double (*fncall)(double) = rand1_table[opcode].fncall;

    if (n == 1 && na >= 1) { /* quickly generate single value */

        GetRNGstate();
        double av = *REAL(a);
        double r = fncall (av);
        PutRNGstate();

        if (ISNAN(r)) {
            warning(_("NAs produced"));
        }
        else {

            /* Compute gradient if requested. */

            if (HAS_GRADIENT_IN_CELL(CDR(args))) {
                SEXP ga = GRADIENT_IN_CELL(CDR(args));
                double (*Dcall)(double,double) = rand1_table[opcode].Dcall;
                if (Dcall != 0) {
                    R_gradient = scaled_gradients (ga, Dcall(r,av), 1);
                    R_variant_result = VARIANT_GRADIENT_FLAG;
                    GRADIENT_TRACE(call);
                }
            }
        }

        UNPROTECT(1); /* a */
        return ScalarReal(r);
    }

    SEXP x = allocVector(REALSXP, n);
    PROTECT(x);

    if (n == 0) {
        UNPROTECT(2); /* a, x */
        return x;
    }

    if (na < 1) {
        int i;
        for (i = 0; i < n; i++)
            REAL(x)[i] = NA_REAL;
        warning(_("NAs produced"));
        UNPROTECT(2); /* a, x */
        return x;
    }

    Rboolean naflag = FALSE;

    GetRNGstate();

    double *ap = REAL(a), *xp = REAL(x);

    if (na == 1) {
        double av = *ap;
        int i;
        for (i = 0; i < n; i++) {
            xp[i] = fncall (av);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }
    else {
        int i, i1;
        for (i = 0, i1 = 0; i < n; i++, i1++) {
            if (i1 == na) i1 = 0;
            xp[i] = fncall (ap[i1]);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }

    PutRNGstate();

    if (naflag)
        warning(_("NAs produced"));

    /* Compute gradient if requested. */

    if (HAS_GRADIENT_IN_CELL(CDR(args))) {
        double (*Dcall)(double,double) = rand1_table[opcode].Dcall;
        if (Dcall != 0) {
            SEXP ga = GRADIENT_IN_CELL(CDR(args));
            SEXP gv = allocVector (REALSXP, n);
            PROTECT(gv);
            for (R_len_t i = 0; i < n; i++)
                REAL(gv)[i] = Dcall (REAL(x)[i], REAL(a)[i]);
            R_gradient = scaled_gradients_vec (ga, gv, n);
            R_variant_result = VARIANT_GRADIENT_FLAG;
            GRADIENT_TRACE(call);
            UNPROTECT(1);
        }
    }


    UNPROTECT(2); /* a, x */
    return x;
}


/* Random sampling from 2-parameter families. */

/* Derivatives for random2 generators of continuous distributions. */

static void Drcauchy (double r, double location, double scale, 
                                double *dlocation, double *dscale)
{
    if (dlocation) *dlocation = 1;
    if (dscale) *dscale = scale <= 0 ? 0 : (r - location) / scale;
}

static void Drlnorm (double r, double meanlog, double sdlog, 
                               double *dmeanlog, double *dsdlog)
{
    if (dmeanlog) *dmeanlog = r;
    if (dsdlog) *dsdlog = sdlog <= 0 ? 0 : r * (log(r) - meanlog) / sdlog;
}

static void Drlogis (double r, double location, double scale, 
                               double *dlocation, double *dscale)
{
    if (dlocation) *dlocation = 1;
    if (dscale) *dscale = scale <= 0 ? 0 : (r - location) / scale;
}

static void Drnorm (double r, double mu, double sigma, 
                              double *dmu, double *dsigma)
{
    if (dmu) *dmu = 1;
    if (dsigma) *dsigma = sigma <= 0 ? 0 : (r - mu) / sigma;
}

static void Drunif (double r, double a, double b,
                              double *da, double *db)
{
    double u = b <= a ? 0.5 : (r - a) / (b - a);
    if (da) *da = 1-u;
    if (db) *db = u;
}

static void Drweibull (double r, double shape, double scale, 
                                 double *dshape, double *dscale)
{
    if (dshape) *dshape = - r * log(r/scale) / shape;
    if (dscale) *dscale = r / scale;
}

static struct { 
    double (*fncall)(double, double);
    void (*Dcall)(double, double, double, double *, double *); 
} rand2_table[14] = 
{
    { rbeta,	0 },
    { rbinom,	0 /* discrete */ },
    { rcauchy,	Drcauchy },
    { rf,	0 },
    { rgamma,	0 },
    { rlnorm,	Drlnorm },
    { rlogis,	Drlogis },
    { rnbinom,	0 /* discrete */ },
    { rnorm,	Drnorm },
    { runif,	Drunif },
    { rweibull,	Drweibull },
    { rwilcox,	0 /* discrete */ },
    { rnchisq,	0 },
    { rnbinom_mu, 0 /* discrete */ }
};

static SEXP do_random2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a1, a2;
    int n, na1, na2;

    checkArity(op, args);

    if (!isVector(CAR(args)) || !isNumeric(a1 = CADR(args)) 
                             || !isNumeric(a2 = CADDR(args)))
        invalid(call);

    if (LENGTH(CAR(args)) == 1) {
        n = asInteger(CAR(args));
        if (n == NA_INTEGER || n < 0)
            invalid(call);
    }
    else
        n = LENGTH(CAR(args));

    if (TYPEOF(a1) != REALSXP) a1 = coerceVector(a1, REALSXP);
    PROTECT(a1);
    na1 = LENGTH(a1);

    if (TYPEOF(a2) != REALSXP) a2 = coerceVector(a2, REALSXP);
    PROTECT(a2);
    na2 = LENGTH(a2);

    int opcode = PRIMVAL(op);
    if (opcode < 0 || opcode >= sizeof rand2_table / sizeof rand2_table[0])
        error(_("internal error in do_random2"));
    double (*rf)(double,double) = rand2_table[opcode].fncall;

    if (n == 1 && na1 >= 1 && na2 >= 1) { /* quickly generate single value */

        GetRNGstate();
        double av1 = *REAL(a1), av2 = *REAL(a2);
        double r = rf (av1, av2);
        PutRNGstate();

        if (ISNAN(r)) {
            warning(_("NAs produced"));
        }
        else {

            /* Compute gradient if requested. */

            if (HAS_GRADIENT_IN_CELL(CDR(args)) 
             || HAS_GRADIENT_IN_CELL(CDDR(args))) {
                void (*Dcall)(double, double, double, double *, double *) 
                       = rand2_table[opcode].Dcall;
                if (Dcall != 0) {
                    SEXP g1 = GRADIENT_IN_CELL(CDR(args));
                    SEXP g2 = GRADIENT_IN_CELL(CDDR(args));
                    double gv1, gv2;
                    Dcall (r, av1, av2, 
                           g1 != R_NilValue ? &gv1 : 0,
                           g2 != R_NilValue ? &gv2 : 0);
                    R_gradient = R_NilValue;
                    if (g1 != R_NilValue)
                        R_gradient = scaled_gradients (g1, gv1, 1);
                    if (g2 != R_NilValue) {
                        if (R_gradient == R_NilValue)
                            R_gradient = scaled_gradients (g2, gv2, 1);
                        else
                            R_gradient = add_scaled_gradients (R_gradient,
                                                               g2, gv2, 1);
                    }
                    R_variant_result = VARIANT_GRADIENT_FLAG;
                    GRADIENT_TRACE(call);
                }
            }
        }

        UNPROTECT(2); /* a1, a2 */
        return ScalarReal(r);
    }

    SEXP x = allocVector(REALSXP, n);
    PROTECT(x);

    if (n == 0) {
        UNPROTECT(3); /* a1, a2, x */
        return x;
    }

    if (na1 < 1 || na2 < 1) {
        int i;
        for (i = 0; i < n; i++)
            REAL(x)[i] = NA_REAL;
        warning(_("NAs produced"));
        UNPROTECT(3); /* a1, a2, x */
        return x;
    }

    Rboolean naflag = FALSE;

    GetRNGstate();

    double *ap1 = REAL(a1), *ap2 = REAL(a2), *xp = REAL(x);

    if (na1 == 1 && na2 == 1) {
        double ar1 = *ap1, ar2 = *ap2;
        int i;
        for (i = 0; i < n; i++) {
            xp[i] = rf (ar1, ar2);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }
    else {
        int i, i1, i2;
        for (i = 0, i1 = 0, i2 = 0; i < n; i++, i1++, i2++) {
            if (i1 == na1) i1 = 0;
            if (i2 == na2) i2 = 0;
            xp[i] = rf (ap1[i1], ap2[i2]);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }

    PutRNGstate();

    if (naflag)
        warning(_("NAs produced"));

    /* Compute gradient if requested. */

    if (HAS_GRADIENT_IN_CELL(CDR(args)) 
     || HAS_GRADIENT_IN_CELL(CDDR(args))) {
        void (*Dcall)(double, double, double, double *, double *) 
               = rand2_table[opcode].Dcall;
        if (Dcall != 0) {
            SEXP g1 = GRADIENT_IN_CELL(CDR(args));
            SEXP g2 = GRADIENT_IN_CELL(CDDR(args));
            SEXP gv1 = g1 != R_NilValue ? allocVector (REALSXP, n) : R_NilValue;
            SEXP gv2 = g2 != R_NilValue ? allocVector (REALSXP, n) : R_NilValue;
            PROTECT2(gv1,gv2);
            for (R_len_t i = 0; i < n; i++) {
                Dcall (REAL(x)[i], REAL(a1)[i%na1], REAL(a2)[i%na2], 
                       g1 != R_NilValue ? &REAL(gv1)[i] : 0,
                       g2 != R_NilValue ? &REAL(gv2)[i] : 0);
            }
            R_gradient = R_NilValue;
            if (g1 != R_NilValue)
                R_gradient = scaled_gradients_vec (g1, gv1, n);
            if (g2 != R_NilValue) {
                if (R_gradient == R_NilValue)
                    R_gradient = scaled_gradients_vec (g2, gv2, n);
                else
                    R_gradient = add_scaled_gradients_vec (R_gradient,
                                                           g2, gv2, n);
            }
            R_variant_result = VARIANT_GRADIENT_FLAG;
            GRADIENT_TRACE(call);
            UNPROTECT(2);
        }
    }

    UNPROTECT(3); /* a1, a2, x */
    return x;
}


/* Random sampling from 3-parameter families.  There's only one, and it's
   a discrete distribution, so there are currently no derivatives to compute,
   and hence the code to do so has not been written. (It would follow the
   pattern in do_random2, with one more parameter.) */

static struct { 
    double (*fncall)(double, double, double);
    void (*Dcall)(double, double, double, double, double *, double *, double *);
} rand3_table[1] =
{
    { rhyper,	0 /* discrete */ }
};

static SEXP do_random3(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a1, a2, a3;
    int n, na1, na2, na3;

    checkArity(op, args);

    if (!isVector(CAR(args)) || !isNumeric(a1 = CADR(args)) 
                             || !isNumeric(a2 = CADDR(args))
                             || !isNumeric(a3 = CADDDR(args)))
        invalid(call);

    if (LENGTH(CAR(args)) == 1) {
        n = asInteger(CAR(args));
        if (n == NA_INTEGER || n < 0)
            invalid(call);
    }
    else
        n = LENGTH(CAR(args));

    if (TYPEOF(a1) != REALSXP) a1 = coerceVector(a1, REALSXP);
    PROTECT(a1);
    na1 = LENGTH(a1);

    if (TYPEOF(a2) != REALSXP) a2 = coerceVector(a2, REALSXP);
    PROTECT(a2);
    na2 = LENGTH(a2);

    if (TYPEOF(a3) != REALSXP) a3 = coerceVector(a3, REALSXP);
    PROTECT(a3);
    na3 = LENGTH(a3);

    int opcode = PRIMVAL(op);
    if (opcode < 0 || opcode >= sizeof rand3_table / sizeof rand3_table[0])
        error(_("internal error in do_random3"));
    double (*rf)(double,double,double) = rand3_table[opcode].fncall;

    if (n == 1 && na1 >= 1 && na2 >= 1 && na3 >= 1) { /* generate single value*/
        GetRNGstate();
        double r = rf (*REAL(a1), *REAL(a2), *REAL(a3));
        if (ISNAN(r)) warning(_("NAs produced"));
        PutRNGstate();
        UNPROTECT(3); /* a1, a2, a3 */
        return ScalarReal(r);
    }

    SEXP x = allocVector(REALSXP, n);
    PROTECT(x);

    if (n == 0) {
        UNPROTECT(4); /* a1, a2, a3, x */
        return x;
    }

    if (na1 < 1 || na2 < 1 | na3 < 1) {
        int i;
        for (i = 0; i < n; i++)
            REAL(x)[i] = NA_REAL;
        warning(_("NAs produced"));
        UNPROTECT(4); /* a1, a2, a3, x */
        return x;
    }

    Rboolean naflag = FALSE;

    GetRNGstate();

    double *ap1 = REAL(a1), *ap2 = REAL(a2), *ap3 = REAL(a3), *xp = REAL(x);

    if (na1 == 1 && na2 == 1 && na3 == 1) {
        double ar1 = *ap1, ar2 = *ap2, ar3 = *ap3;
        int i;
        for (i = 0; i < n; i++) {
            xp[i] = rf (ar1, ar2, ar3);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }
    else {
        int i, i1, i2, i3;
        for (i = 0, i1 = 0, i2 = 0, i3 = 0; i < n; i++, i1++, i2++, i3++) {
            if (i1 == na1) i1 = 0;
            if (i2 == na2) i2 = 0;
            if (i3 == na3) i3 = 0;
            xp[i] = rf (ap1[i1], ap2[i2], ap3[i3]);
            if (ISNAN(xp[i])) naflag = TRUE;
        }
    }

    if (naflag)
        warning(_("NAs produced"));

    PutRNGstate();
    UNPROTECT(4); /* a1, a2, a3, x */
    return x;
}


/*  Unequal Probability Sampling.
 *
 *  Modelled after Fortran code provided by:
 *    E. S. Venkatraman <venkat@biosta.mskcc.org>
 *  but with significant modifications in the
 *  "with replacement" case.
 */

/* Unequal probability sampling; with-replacement case */

static void ProbSampleReplace(int n, double *p, int *perm, int nans, int *ans)
{
    double rU;
    int i, j;
    int nm1 = n - 1;

    /* record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* sort the probabilities into descending order */
    revsort(p, perm, n);

    /* compute cumulative probabilities */
    for (i = 1 ; i < n; i++)
	p[i] += p[i - 1];

    /* compute the sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand();
	for (j = 0; j < nm1; j++) {
	    if (rU <= p[j])
		break;
	}
	ans[i] = perm[j];
    }
}

static Rboolean Walker_warn = FALSE;

/* A  version using Walker's alias method, based on Alg 3.13B in
   Ripley (1987).
 */

#define SMALL 10000
static void
walker_ProbSampleReplace(int n, double *p, int *a, int nans, int *ans)
{
    double *q, rU;
    int i, j, k;
    int *HL, *H, *L;

    if (!Walker_warn) {
	Walker_warn = TRUE;
	warning("Walker's alias method used: results are different from R < 2.2.0");
    }


    /* Create the alias tables.
       The idea is that for HL[0] ... L-1 label the entries with q < 1
       and L ... H[n-1] label those >= 1.
       By rounding error we could have q[i] < 1. or > 1. for all entries.
     */
    if(n <= SMALL) {
	/* might do this repeatedly, so speed matters */
	HL = (int *)alloca(n * sizeof(int));
	q = (double *) alloca(n * sizeof(double));
	R_CHECKSTACK();
    } else {
	/* Slow enough anyway not to risk overflow */
	HL = Calloc(n, int);
	q = Calloc(n, double);
    }
    H = HL - 1; L = HL + n;
    for (i = 0; i < n; i++) {
	q[i] = p[i] * n;
	if (q[i] < 1.) *++H = i; else *--L = i;
    }
    if (H >= HL && L < HL + n) { /* So some q[i] are >= 1 and some < 1 */
	for (k = 0; k < n - 1; k++) {
	    i = HL[k];
	    j = *L;
	    a[i] = j;
	    q[j] += q[i] - 1;
	    if (q[j] < 1.) L++;
	    if(L >= HL + n) break; /* now all are >= 1 */
	}
    }
    for (i = 0; i < n; i++) q[i] += i;

    /* generate sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand() * n;
	k = (int) rU;
	ans[i] = (rU < q[k]) ? k+1 : a[k]+1;
    }
    if(n > SMALL) {
	Free(HL);
	Free(q);
    }
}


/* Unequal probability sampling; without-replacement case */

static void ProbSampleNoReplace(int n, double *p, int *perm,
				int nans, int *ans)
{
    double rT, mass, totalmass;
    int i, j, k, n1;

    /* Record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* Sort probabilities into descending order */
    /* Order element identities in parallel */
    revsort(p, perm, n);

    /* Compute the sample */
    totalmass = 1;
    for (i = 0, n1 = n-1; i < nans; i++, n1--) {
	rT = totalmass * unif_rand();
	mass = 0;
	for (j = 0; j < n1; j++) {
	    mass += p[j];
	    if (rT <= mass)
		break;
	}
	ans[i] = perm[j];
	totalmass -= p[j];
	for(k = j; k < n1; k++) {
	    p[k] = p[k + 1];
	    perm[k] = perm[k + 1];
	}
    }
}

/* Equal probability sampling; with-replacement case */

static SEXP SampleReplace (int k, int n)
{
    SEXP r;
    int *y;
    int i;
    PROTECT(r = allocVector (INTSXP, k));
    y = INTEGER(r);
    for (i = 0; i < k; i++)
	y[i] = n * unif_rand() + 1;
    UNPROTECT(1);
    return r;
}

/* Equal probability sampling; without-replacement case.

   This version is written to produce the same result as earlier versions,
   in which the algorithm was as follows (with x being temporary storage,
   and y being the result):

        for (i = 0; i < n; i++)
            x[i] = i;
        for (i = 0; i < k; i++) {
            j = n * unif_rand();
            y[i] = x[j] + 1;
            x[j] = x[--n];
        }

   When k <= 2, special code is used, for speed.

   When n is small or k is not much smaller than n, a modification of the 
   above algorithm is used, which avoids the need for temporary storage - the
   result is allocated as of length n, and then has its length reduced to k
   (usually with no copy being done).

   When k is much smaller than n, and n is not small, a hashing scheme is
   used, in which hash entries record which elements of x in the above
   algorithm would have been modified from their original setting in which
   x[i] == i.

   When k is less than 0.6*n, but not very small compared to n, the original
   scheme is used.  There's a tradeoff between space and time here, which is
   resolved more towards saving space when n is big.
 */

static SEXP SampleNoReplace (int k, int n)
{
    SEXP r;

    if (k <= 2) {
   
        /* Special code for k = 0, 1, or 2, mimicing effect of previous code. */

        if (k == 0)
            return allocVector(INTSXP,0);

        int i1 = 1 + (int) (n * unif_rand());
        if (k == 1) 
            return ScalarInteger (i1);

        int i2 = 1 + (int) ((n-1) * unif_rand());
        if (i2 == i1) i2 = n;
        r = allocVector(INTSXP,2);
        INTEGER(r)[0] = i1;
        INTEGER(r)[1] = i2;
    }

    else if (n < 100 || k >= 0.6*n) {

        /* Code similar to previous method, but with temporary storage avoided.
           This reqires storing the initial sequence in decreasing rather than 
           increasing order, and picking elements from the tail rather than the
           head, so that the space no longer used after each choice can hold the
           result, at the front of the vector.  Note:  Unlike the previous
           code, the indexes in the sequences are from 1 to n, not 0 to n-1. */

        r = allocVector(INTSXP,n);
        int *y = INTEGER(r);
        int i;
        for (i = 0; i < n; i++)
            y[i] = n-i;
        for (i = 0; i < k; i++) {
            int j = n - 1 - (int) ((n-i) * unif_rand());
            int t = y[j];
            y[j] = y[i];
            y[i] = t;
        }
        if (k < n)
            r = reallocVector(r,k,1);
    }

    else if (k >= 0.4*n || k >= 0.1*n && n < 1000000) {

        /* Use the original method. */

        void *vmax = VMAXGET();
        PROTECT(r = allocVector(INTSXP,k));
        int *y = INTEGER(r);
        int *x = (int *) R_alloc(n,sizeof(int));
        int i, j;

        for (i = 0; i < n; i++)
            x[i] = i;
        for (i = 0; i < k; i++) {
            j = n * unif_rand();
            y[i] = x[j] + 1;
            x[j] = x[--n];
        }

        UNPROTECT(1);
        VMAXSET(vmax);
    }

    else {

        /* Hash table implementation, producing same result as previous code.
           Mimics previous code by using a hash table to record how 'x' would
           have been changed.  At each iteration, it looks up x[j] in the
           hash table (j from 1 up), taking its value to be j if it is not in 
           the table, and using this value as the next sampled value.  Also
           lookups up x[n-i], which is taken to be n-i if not present, and 
           replaces/creates the entry for x[j] as having value x[n-i].  The
           hash table is non-chaining, with linear search. */

#       define HASH_STATS 1  /* may enable to get stats for tuning */

        /* Decide on the size of the hash table. */
        
        unsigned tblsize, mintblsize;
        mintblsize = 1.5 * k;
        tblsize = 32;
        while (tblsize < 0x80000000U && tblsize < mintblsize)
            tblsize <<= 1;
        unsigned tblmask = tblsize - 1;

        /* Allocate hash table, as auto variable if small, else with R_alloc. */

        struct tblentry { int pos, val; } *tbl;
        struct tblentry local [ tblsize < 1000 ? tblsize : 1 ];
        void *vmax = VMAXGET();
        tbl = tblsize < 1000 ? local 
                             : (struct tblentry *) R_alloc(tblsize,sizeof *tbl);

        /* Clear all entries to zero.  Non-empty pos values start at 1. */

        memset (tbl, 0, tblsize * sizeof *tbl);

        /* Allocate vector to hold result. */

        r = allocVector(INTSXP,k);
        int *y = INTEGER(r);

        /* Do the sampling as described above. */

        int i;
        for (i = 0; i < k; i++) {
            int j = 1 + (int) ((n-i) * unif_rand());
            unsigned h;
            for (h = j & tblmask; ; h = (h+1) & tblmask) {
                if (tbl[h].pos == 0) {
                    y[i] = j;
                    break;
                }
                if (tbl[h].pos == j) {
                    y[i] = tbl[h].val;
                    break;
                }
            }
            unsigned h2;
            for (h2 = (n-i) & tblmask; ; h2 = (h2+1) & tblmask) {
                if (tbl[h2].pos == 0) {
                    tbl[h].val = n-i;
                    break;
                }
                if (tbl[h2].pos == n-i) {
                    tbl[h].val = tbl[h2].val;
                    break;
                }
            }
            tbl[h].pos = j;  /* don't set until after search for entry n-i */
        }

        VMAXSET(vmax);
    }

    return r;
}

void FixupProb(double *p, int n, int require_k, Rboolean replace)
{
    double sum;
    int i, npos;
    npos = 0;
    sum = 0.;
    for (i = 0; i < n; i++) {
	if (!R_FINITE(p[i]))
	    error(_("NA in probability vector"));
	if (p[i] < 0)
	    error(_("non-positive probability"));
	if (p[i] > 0) {
	    npos++;
	    sum += p[i];
	}
    }
    if (npos == 0 || (!replace && require_k > npos))
	error(_("too few positive probabilities"));
    for (i = 0; i < n; i++)
	p[i] /= sum;
}

/* do_sample - probability sampling with/without replacement.
   .Internal(sample(n, size, replace, prob))
*/
static SEXP do_sample(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, y, prob, sreplace;
    int k, n, replace;
    double *p;

    checkArity(op, args);
    n = asInteger(CAR(args)); args = CDR(args);
    k = asInteger(CAR(args)); args = CDR(args); /* size */
    sreplace = CAR(args); args = CDR(args);
    if(length(sreplace) != 1)
	 error(_("invalid '%s' argument"), "replace");
    replace = asLogical(sreplace);
    prob = CAR(args);
    if (replace == NA_LOGICAL)
	error(_("invalid '%s' argument"), "replace");
    if (n == NA_INTEGER || n < 0 || (k > 0 && n == 0))
	error(_("invalid first argument"));
    if (k == NA_INTEGER || k < 0)
	error(_("invalid '%s' argument"), "size");
    if (!replace && k > n)
	error(_("cannot take a sample larger than the population when 'replace = FALSE'"));
    GetRNGstate();

    if (!isNull(prob)) {
        PROTECT(y = allocVector(INTSXP, k));
	prob = coerceVector(prob, REALSXP);
	if (NAMEDCNT_GT_0(prob)) prob = duplicate(prob);
	PROTECT(prob);
	p = REAL(prob);
	if (length(prob) != n)
	    error(_("incorrect number of probabilities"));
	FixupProb(p, n, k, (Rboolean)replace);
	PROTECT(x = allocVector(INTSXP, n));
	if (replace) {
	    int i, nc = 0;
	    for (i = 0; i < n; i++) if(n * p[i] > 0.1) nc++;
	    if (nc > 200)
		walker_ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	    else
		ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	} else
	    ProbSampleNoReplace(n, p, INTEGER(x), k, INTEGER(y));
	UNPROTECT(2);
    }
    else if (replace)
        PROTECT (y = SampleReplace(k,n));
    else
        PROTECT (y = SampleNoReplace(k,n));

    PutRNGstate();
    UNPROTECT(1);
    return y;
}

static SEXP do_rmultinom(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP prob, ans, nms;
    int n, size, k, i, ik;
    checkArity(op, args);
    n	 = asInteger(CAR(args)); args = CDR(args);/* n= #{samples} */
    size = asInteger(CAR(args)); args = CDR(args);/* X ~ Multi(size, prob) */
    if (n == NA_INTEGER || n < 0)
	error(_("invalid first argument 'n'"));
    if (size == NA_INTEGER || size < 0)
	error(_("invalid second argument 'size'"));
    prob = CAR(args);
    prob = coerceVector(prob, REALSXP);
    k = length(prob);/* k = #{components or classes} = X-vector length */
    if (NAMEDCNT_GT_0(prob)) prob = duplicate(prob);
    PROTECT(prob);
    /* check and make sum = 1: */
    FixupProb(REAL(prob), k, /*require_k = */ 0, TRUE);
    GetRNGstate();
    PROTECT(ans = allocMatrix(INTSXP, k, n));/* k x n : natural for columnwise store */
    for(i=ik = 0; i < n; i++, ik += k)
	rmultinom(size, REAL(prob), k, &INTEGER(ans)[ik]);
    PutRNGstate();
    if(!isNull(nms = getAttrib(prob, R_NamesSymbol))) {
	SEXP dimnms;
	PROTECT(nms);
	PROTECT(dimnms = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnms, 0, nms);
	setAttrib(ans, R_DimNamesSymbol, dimnms);
	UNPROTECT(2);
    }
    UNPROTECT(2);
    return ans;
}

SEXP
R_r2dtable(SEXP n, SEXP r, SEXP c)
{
    int nr, nc, *row_sums, *col_sums, i, *jwork;
    int n_of_samples, n_of_cases;
    double *fact;
    SEXP ans, tmp;
    const void *vmax = VMAXGET();

    nr = length(r);
    nc = length(c);

    /* Note that the R code in r2dtable() also checks for missing and
       negative values.
       Should maybe do the same here ...
    */
    if(!isInteger(n) || (length(n) == 0) ||
       !isInteger(r) || (nr <= 1) ||
       !isInteger(c) || (nc <= 1))
	error(_("invalid arguments"));

    n_of_samples = INTEGER(n)[0];
    row_sums = INTEGER(r);
    col_sums = INTEGER(c);

    /* Compute total number of cases as the sum of the row sums.
       Note that the R code in r2dtable() also checks whether this is
       the same as the sum of the col sums.
       Should maybe do the same here ...
    */
    n_of_cases = 0;
    jwork = row_sums;
    for(i = 0; i < nr; i++)
	n_of_cases += *jwork++;

    /* Log-factorials from 0 to n_of_cases.
       (I.e., lgamma(1), ..., lgamma(n_of_cases + 1).)
    */
    fact = (double *) R_alloc(n_of_cases + 1, sizeof(double));
    fact[0] = 0.;
    for(i = 1; i <= n_of_cases; i++)
	fact[i] = lgammafn((double) (i + 1));

    jwork = (int *) R_alloc(nc, sizeof(int));

    PROTECT(ans = allocVector(VECSXP, n_of_samples));

    GetRNGstate();

    for(i = 0; i < n_of_samples; i++) {
	PROTECT(tmp = allocMatrix(INTSXP, nr, nc));
	rcont2(&nr, &nc, row_sums, col_sums, &n_of_cases, fact,
	       jwork, INTEGER(tmp));
	SET_VECTOR_ELT(ans, i, tmp);
	UNPROTECT(1);
    }

    PutRNGstate();

    UNPROTECT(1);
    VMAXSET(vmax);

    return(ans);
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_random[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"rchisq",	do_random1,	0,   101000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rexp",	do_random1,	1,   101000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rgeom",	do_random1,	2,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rpois",	do_random1,	3,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rt",		do_random1,	4,   101000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rsignrank",	do_random1,	5,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rbeta",	do_random2,	0,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rbinom",	do_random2,	1,   1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rcauchy",	do_random2,	2,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rf",		do_random2,	3,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rgamma",	do_random2,	4,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlnorm",	do_random2,	5,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlogis",	do_random2,	6,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom",	do_random2,	7,   1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnorm",	do_random2,	8,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"runif",	do_random2,	9,   111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rweibull",	do_random2,	10,  111000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rwilcox",	do_random2,	11,  1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnchisq",	do_random2,	12,  1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom_mu",	do_random2,	13,  1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rhyper",	do_random3,	0,   1000011,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample",	do_sample,	0,   1000011,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rmultinom",	do_rmultinom,	0,   1000011,	3,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
