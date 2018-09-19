/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2012  The R Core Team
 *  Copyright (C) 2002, 2004  The R Foundation
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
/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include <R.h>
#include <Rmath.h>
#include <float.h>
#include "mva.h"
#include "stats.h"
#ifdef HAVE_OPENMP
# include <R_ext/MathThreads.h>
#endif

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#ifdef R_160_and_older
#define both_non_NA both_FINITE
#else
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
#endif

static double R_euclidean(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int na_count, j;

    na_count= 0;
    dist = 0;
    for (j = 0; j < nc; j++) {
        dev = x[i1] - x[i2];
        if (ISNAN(dev))
            na_count += 1;
        else
            dist += dev * dev;
        i1 += nr;
        i2 += nr;
    }
    if (na_count > 0) {
        if (na_count == nc)
            return NA_REAL;
        dist /= (double)(nc-na_count) / nc;
    }
    return sqrt(dist);
}

static double R_maximum(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int j;

    dist = -1;
    for (j = 0; j < nc; j++) {
        dev = fabs(x[i1] - x[i2]);
        if (dev > dist) /* won't be true if dev is NA/NaN */
            dist = dev;
        i1 += nr;
        i2 += nr;
    }
    return dist < 0 ? NA_REAL : dist;
}

static double R_manhattan(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int na_count, j;

    na_count = 0;
    dist = 0;
    for (j = 0; j < nc; j++) {
        dev = fabs(x[i1] - x[i2]);
        if (ISNAN(dev))
            na_count += 1;
        else
            dist += dev;
        i1 += nr;
        i2 += nr;
    }
    if (na_count > 0) {
        if (na_count == nc)
            return NA_REAL;
        dist /= (double)(nc-na_count) / nc;
    }
    return dist;
}

static double R_canberra(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist, sum, diff;
    int count, j;

    count = 0;
    dist = 0;
    for (j = 0; j < nc; j++) {
        if (both_non_NA(x[i1], x[i2])) {
            sum = fabs(x[i1]) + fabs(x[i2]);
            diff = fabs(x[i1] - x[i2]);
            if (sum > DBL_MIN || diff > DBL_MIN) {
                dev = diff/sum;
                if (!ISNAN(dev) ||
                   (!R_FINITE(diff) && diff == sum &&
                    /* use Inf = lim x -> oo */ (dev = 1.))) {
                    dist += dev;
                    count++;
                }
            }
        }
        i1 += nr;
        i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return dist;
}

static double R_dist_binary(double *x, int nr, int nc, int i1, int i2)
{
    int total, count, dist;
    int j;

    total = 0;
    count = 0;
    dist = 0;

    for (j = 0; j < nc; j++) {
        if (both_non_NA(x[i1], x[i2])) {
            if (!both_FINITE(x[i1], x[i2])) {
                warning(_("treating non-finite values as NA"));
            }
            else {
                if (x[i1] || x[i2]) {
                    count++;
                    if ( ! (x[i1] && x[i2]) ) dist++;
                }
                total++;
            }
        }
        i1 += nr;
        i2 += nr;
    }

    if (total == 0) return NA_REAL;
    if (count == 0) return 0;
    return (double) dist / count;
}

static double R_minkowski(double *x, int nr, int nc, int i1, int i2, double p)
{
    double dev, dist;
    int na_count, j;

    na_count= 0;
    dist = 0;
    for (j = 0; j < nc; j++) {
        dev = x[i1] - x[i2];
        if (ISNAN(dev))
            na_count += 1;
        else
            dist += R_pow(fabs(dev), p);
        i1 += nr;
        i2 += nr;
    }
    if (na_count > 0) {
        if (na_count == nc)
            return NA_REAL;
        dist /= (double)(nc-na_count) / nc;
    }
    return R_pow(dist, 1.0/p);
}

enum { EUCLIDEAN=1, MAXIMUM, MANHATTAN, CANBERRA, BINARY, MINKOWSKI };
/* == 1,2,..., defined by order in the R function dist */

void R_distance(double *x, int *nr, int *nc, double *d, int *diag,
                int *method, double *p)
{
    int dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */
    int meth = *method;
    R_len_t nrow = *nr;
    R_len_t ncol = *nc;
    double pv = *p;

    size_t ij;  /* can exceed 2^31 - 1 */
    R_len_t i, j;

    ij = 0;

    if (meth == MINKOWSKI && pv == 2.0) meth = EUCLIDEAN;

    switch (meth) {
    case EUCLIDEAN:
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_euclidean (x, nrow, ncol, i, j);
        break;
    case MAXIMUM:
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_maximum (x, nrow, ncol, i, j);
        break;
    case MANHATTAN:
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_manhattan (x, nrow, ncol, i, j);
        break;
    case CANBERRA:
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_canberra (x, nrow, ncol, i, j);
        break;
    case BINARY:
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_dist_binary (x, nrow, ncol, i, j);
        break;
    case MINKOWSKI:
        if (!R_FINITE(pv) || pv <= 0)
            error(_("distance(): invalid p"));
        for (j = 0; j <= nrow; j++)
            for (i = j+dc; i < nrow; i++)
                d[ij++] = R_minkowski (x, nrow, ncol, i, j, pv);
        break;
    default:
        error(_("distance(): invalid distance"));
    }
}

#include <Rinternals.h>

SEXP Cdist(SEXP x, SEXP smethod, SEXP attrs, SEXP p)
{
    SEXP ans;
    int nr = nrows(x), nc = ncols(x), method = asInteger(smethod);
    double Nd;
    int N, diag = 0;
    double rp = asReal(p);
    Nd = (double)nr * (nr-1)/2; /* avoid overflow for N ~ 50,000 */
    if (Nd > R_LEN_T_MAX) 
        error(_("resulting vector exceeds vector length limit"));
    N = (R_len_t)Nd;
    PROTECT(ans = allocVector(REALSXP, N));
    R_distance(REAL(x), &nr, &nc, REAL(ans), &diag, &method, &rp);
    /* tack on attributes */
    SEXP names = getAttrib(attrs, R_NamesSymbol);
    for (int i = 0; i < LENGTH(attrs); i++)
        setAttrib(ans, install(translateChar(STRING_ELT(names, i))),
                  VECTOR_ELT(attrs, i));
    UNPROTECT(1);
    return ans;
}
