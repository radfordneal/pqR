/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2010   The R Development Core Team
 *  Copyright (C) 2002--2008  The R Foundation
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

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>
#include <Rmath.h>
#include <matprod/matprod.h>
#include <R_ext/RS.h>     /* for Calloc/Free */
#include <R_ext/Applic.h> /* for dgemm */

#include <helpers/helpers-app.h>
#include <matprod/piped-matprod.h>

/* "GetRowNames" and "GetColNames" are utility routines which
 * locate and return the row names and column names from the
 * dimnames attribute of a matrix.  They are useful because
 * old versions of R used pair-based lists for dimnames
 * whereas recent versions use vector based lists.

 * These are now very old, plus
 * ``When the "dimnames" attribute is
 *   grabbed off an array it is always adjusted to be a vector.''

 They are used in bind.c and subset.c, and advertised in Rinternals.h
*/
SEXP GetRowNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 0);
    else
	return R_NilValue;
}

SEXP GetColNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 1);
    else
	return R_NilValue;
}

/* Allocate matrix, checking for errors, and putting in dims.  Split into
   two parts to allow the second part to sometimes be done in parallel with 
   computation of matrix elements (after just the first part done). */

static SEXP R_INLINE allocMatrix0 (SEXPTYPE mode, int nrow, int ncol)
{
    if (nrow < 0 || ncol < 0)
	error(_("negative extents to matrix"));

    if ((double)nrow * (double)ncol > INT_MAX)
	error(_("allocMatrix: too many elements specified"));

    return allocVector (mode, nrow*ncol);
}

static SEXP R_INLINE allocMatrix1 (SEXP s, int nrow, int ncol)
{
    SEXP t;

    PROTECT(s);
    PROTECT(t = allocVector(INTSXP, 2));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}

SEXP allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    return allocMatrix1 (allocMatrix0 (mode, nrow, ncol), nrow, ncol);
}

/* Package matrix uses this .Internal with 5 args: should have 7 */
SEXP attribute_hidden do_matrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, ans, snr, snc, dimnames;
    int nr = 1, nc = 1, byrow, lendat, miss_nr, miss_nc;

    checkArity(op, args);
    vals = CAR(args); args = CDR(args);
    /* Supposedly as.vector() gave a vector type, but we check */
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    error(_("'data' must be of a vector type"));
    }
    lendat = length(vals);
    snr = CAR(args); args = CDR(args);
    snc = CAR(args); args = CDR(args);
    byrow = asLogical(CAR(args)); args = CDR(args);
    if (byrow == NA_INTEGER)
	error(_("invalid '%s' argument"), "byrow");
    dimnames = CAR(args);
    args = CDR(args);
    miss_nr = asLogical(CAR(args)); args = CDR(args);
    miss_nc = asLogical(CAR(args));

    if (!miss_nr) {
	if (!isNumeric(snr)) error(_("non-numeric matrix extent"));
	nr = asInteger(snr);
	if (nr == NA_INTEGER)
	    error(_("invalid 'nrow' value (too large or NA)"));
	if (nr < 0)
	    error(_("invalid 'nrow' value (< 0)"));
    }
    if (!miss_nc) {
	if (!isNumeric(snc)) error(_("non-numeric matrix extent"));
	nc = asInteger(snc);
	if (nc == NA_INTEGER)
	    error(_("invalid 'ncol' value (too large or NA)"));
	if (nc < 0)
	    error(_("invalid 'ncol' value (< 0)"));
    }
    if (miss_nr && miss_nc) nr = lendat;
    else if (miss_nr) nr = ceil(lendat/(double) nc);
    else if (miss_nc) nc = ceil(lendat/(double) nr);

    if(lendat > 0 ) {
	if (lendat > 1 && (nr * nc) % lendat != 0) {
	    if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
		((lendat < nr) && (nr / lendat) * lendat != nr))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of rows [%d]"), lendat, nr);
	    else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		     ((lendat < nc) && (nc / lendat) * lendat != nc))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of columns [%d]"), lendat, nc);
	}
	else if ((lendat > 1) && (nr * nc == 0)){
	    warning(_("data length exceeds size of matrix"));
	}
    }

    if ((double)nr * (double)nc > INT_MAX)
	error(_("too many elements specified"));

    PROTECT(ans = allocMatrix(TYPEOF(vals), nr, nc));

    if(lendat) {
	if (isVector(vals))
	    copyMatrix(ans, vals, byrow);
	else
	    copyListMatrix(ans, vals, byrow);
    } else if (isVectorAtomic(vals)) /* VECSXP/EXPRSXP already are R_NilValue */
        set_elements_to_NA_or_NULL (ans, 0, nr*nc);

    if(!isNull(dimnames)&& length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);
    UNPROTECT(1);
    return ans;
}

/**
 * Allocate a 3-dimensional array
 *
 * @param mode The R mode (e.g. INTSXP)
 * @param nrow number of rows
 * @param ncol number of columns
 * @param nface number of faces
 *
 * @return A 3-dimensional array of the indicated dimensions and mode
 */
SEXP alloc3DArray(SEXPTYPE mode, int nrow, int ncol, int nface)
{
    SEXP s, t;
    int n;

    if (nrow < 0 || ncol < 0 || nface < 0)
	error(_("negative extents to 3D array"));
    if ((double)nrow * (double)ncol * (double)nface > INT_MAX)
	error(_("alloc3Darray: too many elements specified"));
    n = nrow * ncol * nface;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 3));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    INTEGER(t)[2] = nface;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}


SEXP allocArray(SEXPTYPE mode, SEXP dims)
{
    SEXP array;
    int i, n;
    double dn;

    dn = n = 1;
    for (i = 0; i < LENGTH(dims); i++) {
	dn *= INTEGER(dims)[i];
	if(dn > INT_MAX)
	    error(_("allocArray: too many elements specified by 'dims'"));
	n *= INTEGER(dims)[i];
    }

    PROTECT(dims = duplicate(dims));
    PROTECT(array = allocVector(mode, n));
    setAttrib(array, R_DimSymbol, dims);
    UNPROTECT(2);
    return array;
}

/* DropDims strips away redundant dimensioning information. */
/* If there is an appropriate dimnames attribute the correct */
/* element is extracted and attached to the vector as a names */
/* attribute.  Note that this function mutates x. */
/* Duplication should occur before this is called. */

SEXP DropDims(SEXP x)
{
    SEXP dims, dimnames, newnames = R_NilValue;
    int i, n, ndims;

    PROTECT(x);
    dims = getAttrib(x, R_DimSymbol);
    dimnames = getAttrib(x, R_DimNamesSymbol);

    /* Check that dropping will actually do something. */
    /* (1) Check that there is a "dim" attribute. */

    if (dims == R_NilValue) {
	UNPROTECT(1);
	return x;
    }
    ndims = LENGTH(dims);

    /* (2) Check whether there are redundant extents */
    n = 0;
    for (i = 0; i < ndims; i++)
	if (INTEGER(dims)[i] != 1) n++;
    if (n == ndims) {
	UNPROTECT(1);
	return x;
    }

    if (n <= 1) {
	/* We have reduced to a vector result.
	   If that has length one, it is ambiguous which dimnames to use,
	   so use it if there is only one (as from R 2.7.0).
	 */
	if (dimnames != R_NilValue) {
	    if(LENGTH(x) != 1) {
		for (i = 0; i < LENGTH(dims); i++) {
		    if (INTEGER(dims)[i] != 1) {
			newnames = VECTOR_ELT(dimnames, i);
			break;
		    }
		}
	    } else { /* drop all dims: keep names if unambiguous */
		int cnt;
		for(i = 0, cnt = 0; i < LENGTH(dims); i++)
		    if(VECTOR_ELT(dimnames, i) != R_NilValue) cnt++;
		if(cnt == 1)
		    for (i = 0; i < LENGTH(dims); i++) {
			newnames = VECTOR_ELT(dimnames, i);
			if(newnames != R_NilValue) break;
		    }
	    }
	}
	PROTECT(newnames);
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, R_NilValue);
	setAttrib(x, R_NamesSymbol, newnames);
	/* FIXME: the following is desirable, but pointless as long as
	   subset.c & others have a contrary version that leaves the
	   S4 class in, incorrectly, in the case of vectors.  JMC
	   3/3/09 */
/* 	if(IS_S4_OBJECT(x)) {/\* no longer valid subclass of array or
 	matrix *\/ */
/* 	    setAttrib(x, R_ClassSymbol, R_NilValue); */
/* 	    UNSET_S4_OBJECT(x); */
/* 	} */
	UNPROTECT(1);
    } else {
	/* We have a lower dimensional array. */
	SEXP newdims, dnn, newnamesnames = R_NilValue;
	dnn = getAttrib(dimnames, R_NamesSymbol);
	PROTECT(newdims = allocVector(INTSXP, n));
	for (i = 0, n = 0; i < ndims; i++)
	    if (INTEGER(dims)[i] != 1)
		INTEGER(newdims)[n++] = INTEGER(dims)[i];
	if (!isNull(dimnames)) {
	    int havenames = 0;
	    for (i = 0; i < ndims; i++)
		if (INTEGER(dims)[i] != 1 &&
		    VECTOR_ELT(dimnames, i) != R_NilValue)
		    havenames = 1;
	    if (havenames) {
		PROTECT(newnames = allocVector(VECSXP, n));
		PROTECT(newnamesnames = allocVector(STRSXP, n));
		for (i = 0, n = 0; i < ndims; i++) {
		    if (INTEGER(dims)[i] != 1) {
			if(!isNull(dnn))
			    SET_STRING_ELT(newnamesnames, n,
					   STRING_ELT(dnn, i));
			SET_VECTOR_ELT(newnames, n++, VECTOR_ELT(dimnames, i));
		    }
		}
	    }
	    else dimnames = R_NilValue;
	}
	PROTECT(dimnames);
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, newdims);
	if (dimnames != R_NilValue)
	{
	    if(!isNull(dnn))
		setAttrib(newnames, R_NamesSymbol, newnamesnames);
	    setAttrib(x, R_DimNamesSymbol, newnames);
	    UNPROTECT(2);
	}
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return x;
}

SEXP attribute_hidden do_drop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xdims;
    int i, n, shorten;

    checkArity(op, args);
    x = CAR(args);
    if ((xdims = getAttrib(x, R_DimSymbol)) != R_NilValue) {
	n = LENGTH(xdims);
	shorten = 0;
	for (i = 0; i < n; i++)
	    if (INTEGER(xdims)[i] == 1) shorten = 1;
	if (shorten) {
	    if (NAMEDCNT_GT_0(x)) x = duplicate(x);
	    x = DropDims(x);
	}
    }
    return x;
}

/* Length of Primitive Objects */

static SEXP do_fast_length (SEXP call, SEXP op, SEXP arg, SEXP rho, 
                            int variant)
{   
    R_len_t len = length(arg);
    return ScalarInteger (len <= INT_MAX ? len : NA_INTEGER);
}

SEXP attribute_hidden do_length(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity (op, args);
    check1arg_x (args, call);

    if (DispatchOrEval (call, op, "length", args, rho, &ans, 0, 1))
        return(ans);

    if (PRIMFUN_FAST(op)==0)
        SET_PRIMFUN_FAST_UNARY (op, do_fast_length, 1, 0);

    return do_fast_length (call, op, CAR(args), rho, 0);
}

void task_row_or_col (helpers_op_t op, SEXP ans, SEXP dim, SEXP ignored)
{
    int nr = INTEGER(dim)[0], nc = INTEGER(dim)[1];
    R_len_t k;
    int i, j;
    int *p;

    HELPERS_SETUP_OUT(10); /* large, since computing one element is very fast */

    p = INTEGER(ans);                   /* store sequentially (down columns)  */
    k = 0;                              /* with k, for good cache performance */

    switch (op) {
    case 1: /* row */
        for (j = 1; j <= nc; j++) {
            for (i = 1; i <= nr; i++) {
                p[k] = i;
                HELPERS_NEXT_OUT (k);
            }
        }
        break;
    case 2: /* col */
        for (j = 1; j <= nc; j++) {
            for (i = 1; i <= nr; i++) {
                p[k] = j;
                HELPERS_NEXT_OUT (k);
            }
        }
        break;
    }
}

#define T_rowscols THRESHOLD_ADJUST(100)

SEXP attribute_hidden do_rowscols (SEXP call, SEXP op, SEXP args, SEXP rho, 
                                   int variant)
{
    SEXP dim, ans;
    int nr, nc;

    checkArity(op, args);
    dim = CAR(args);
    if (!isInteger(dim) || LENGTH(dim) != 2)
	error(_("a matrix-like object is required as argument to 'row/col'"));
    nr = INTEGER(dim)[0];
    nc = INTEGER(dim)[1];

    ans = allocMatrix0 (INTSXP, nr, nc);

    DO_NOW_OR_LATER1 (variant, LENGTH(ans) >= T_rowscols,
      HELPERS_PIPE_OUT, task_row_or_col, PRIMVAL(op), ans, dim);

    return allocMatrix1 (ans, nr, nc);
}

void task_matprod_zero (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{
  double *z = REAL(sz);
  R_len_t u = LENGTH(sz);
  R_len_t i;

  for (i = 0; i < u; i++) z[i] = 0;
}

/* Real matrix product, using the routines in extra/matprod. */

void task_matprod_vec_vec (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int n = LENGTH(sx);

    z[0] = matprod_vec_vec (x, y, n);
}

void task_matprod_mat_vec (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int nrx = LENGTH(sz);
    int ncx = LENGTH(sy);

    matprod_mat_vec (x, y, z, nrx, ncx);
}

void task_matprod_vec_mat (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int nry = LENGTH(sx);
    int ncy = LENGTH(sz);

    matprod_vec_mat (x, y, z, nry, ncy);
}

void task_matprod (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int ncx_nry = op;
    int nrx = LENGTH(sx) / ncx_nry;
    int ncy = LENGTH(sy) / ncx_nry;

    matprod (x, y, z, nrx, ncx_nry, ncy);
}

/* Real matrix product, using the BLAS routines. */

void task_matprod_vec_vec_BLAS (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int n = LENGTH(sx);
    int int_1 = 1;

    z[0] = F77_CALL(ddot) (&n, x, &int_1, y, &int_1);
}

void task_matprod_mat_vec_BLAS (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int nrx = LENGTH(sz);
    int ncx = LENGTH(sy);
    double one = 1.0, zero = 0.0;
    char *transN1 = "N";
    int int_1 = 1;

    F77_CALL(dgemv) (transN1, &nrx, &ncx, &one, 
                     x, &nrx, y, &int_1, &zero, z, &int_1);
}

void task_matprod_vec_mat_BLAS (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int nry = LENGTH(sx);
    int ncy = LENGTH(sz);
    double one = 1.0, zero = 0.0;
    char *transT1 = "T";
    int int_1 = 1;

    F77_CALL(dgemv) (transT1, &nry, &ncy, &one, 
                     y, &nry, x, &int_1, &zero, z, &int_1);
}

void task_matprod_BLAS (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    double *z = REAL(sz), *x = REAL(sx), *y = REAL(sy);
    int ncx_nry = op;
    int nrx = LENGTH(sx) / ncx_nry;
    int ncy = LENGTH(sy) / ncx_nry;
    char *transN1 = "N", *transN2 = "N";
    double one = 1.0, zero = 0.0;

    F77_CALL(dgemm) (transN1, transN2, &nrx, &ncy, &ncx_nry, &one,
                      x, &nrx, y, &ncx_nry, &zero, z, &nrx);
}

/* Complex matrix product. */

void task_cmatprod_zero (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{
  Rcomplex *z = COMPLEX(sz);
  R_len_t u = LENGTH(sz);
  R_len_t i;

  for (i = 0; i < u; i++) z[i].r = z[i].i = 0;
}

void task_cmatprod (helpers_op_t op, SEXP sz, SEXP sx, SEXP sy)
{ 
    Rcomplex *z = COMPLEX(sz), *x = COMPLEX(sx), *y = COMPLEX(sy);
    int ncx_nry = op;
    int nrx = LENGTH(sx) / ncx_nry;
    int ncy = LENGTH(sy) / ncx_nry;

#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    char *transa = "N", *transb = "N";
    Rcomplex one, zero;
    int i;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx_nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &ncy, &ncx_nry, &one,
			x, &nrx, y, &ncx_nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nrx*ncy; i++) z[i].r = z[i].i = 0;
    }
#else
    int i, j, k;
    double xij_r, xij_i, yjk_r, yjk_i;
    long double sum_i, sum_r;

    for (i = 0; i < nrx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * nrx].r = NA_REAL;
	    z[i + k * nrx].i = NA_REAL;
	    sum_r = 0.0;
	    sum_i = 0.0;
	    for (j = 0; j < ncx_nry; j++) {
		xij_r = x[i + j * nrx].r;
		xij_i = x[i + j * nrx].i;
		yjk_r = y[j + k * ncx_nry].r;
		yjk_i = y[j + k * ncx_nry].i;
		if (ISNAN(xij_r) || ISNAN(xij_i)
		    || ISNAN(yjk_r) || ISNAN(yjk_i))
		    goto next_ik;
		sum_r += (xij_r * yjk_r - xij_i * yjk_i);
		sum_i += (xij_r * yjk_i + xij_i * yjk_r);
	    }
	    z[i + k * nrx].r = sum_r;
	    z[i + k * nrx].i = sum_i;
	next_ik:
	    ;
	}
#endif
}

static void symcrossprod(double *x, int nr, int nc, double *z)
{
    char *trans = "T", *uplo = "U";
    double one = 1.0, zero = 0.0;
    int i, j;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nc, &nr, &one, x, &nr, &zero, z, &nc);
	for (i = 1; i < nc; i++)
	    for (j = 0; j < i; j++) z[i + nc *j] = z[j + nc * i];
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nc*nc; i++) z[i] = 0;
    }

}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    char *transa = "T", *transb = "N";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < ncx*ncy; i++) z[i] = 0;
    }
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    char *transa = "T", *transb = "N";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < ncx*ncy; i++) z[i].r = z[i].i = 0;
    }
}

static void symtcrossprod(double *x, int nr, int nc, double *z)
{
    char *trans = "N", *uplo = "U";
    double one = 1.0, zero = 0.0;
    int i, j;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nr, &nc, &one, x, &nr, &zero, z, &nr);
	for (i = 1; i < nr; i++)
	    for (j = 0; j < i; j++) z[i + nr *j] = z[j + nr * i];
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nr*nr; i++) z[i] = 0;
    }

}

static void tcrossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    char *transa = "N", *transb = "T";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < nrx*nry; i++) z[i] = 0;
    }
}

static void tccrossprod(Rcomplex *x, int nrx, int ncx,
			Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    char *transa = "N", *transb = "T";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < nrx*nry; i++) z[i].r = z[i].i = 0;
    }
}

#define T_matmult THRESHOLD_ADJUST(30)

/* "%*%" (op = 0), crossprod (op = 1) or tcrossprod (op = 2) */
SEXP attribute_hidden do_matprod (SEXP call, SEXP op, SEXP args, SEXP rho, 
                                  int variant)
{
    int ldx, ldy, nrx, ncx, nry, ncy, mode;
    SEXP x = CAR(args), y = CADR(args), xdims, ydims, ans;
    Rboolean sym;

    if(PRIMVAL(op) == 0 && /* %*% is primitive, the others are .Internal() */
       (IS_S4_OBJECT(x) || IS_S4_OBJECT(y))
       && R_has_methods(op)) {
	SEXP s, value;
        wait_until_arguments_computed(args);
	/* Remove argument names to ensure positional matching */
	for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
	value = R_possible_dispatch(call, op, args, rho, FALSE);
	if(value) return value;
    }

    sym = isNull(y);
    if (sym && (PRIMVAL(op) > 0)) y = x;
    if ( !(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y)) )
	errorcall(call, _("requires numeric/complex matrix/vector arguments"));

    xdims = getAttrib(x, R_DimSymbol);
    ydims = getAttrib(y, R_DimSymbol);
    ldx = length(xdims);
    ldy = length(ydims);

    if (ldx != 2 && ldy != 2) {		/* x and y non-matrices */
	if (PRIMVAL(op) == 0) {
	    nrx = 1;
	    ncx = LENGTH(x);
	}
	else {
	    nrx = LENGTH(x);
	    ncx = 1;
	}
	nry = LENGTH(y);
	ncy = 1;
    }
    else if (ldx != 2) {		/* x not a matrix */
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
	nrx = 0;
	ncx = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(x) == nry) {	/* x as row vector */
		nrx = 1;
		ncx = nry; /* == LENGTH(x) */
	    }
	    else if (nry == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
	else if (PRIMVAL(op) == 1) { /* crossprod() */
	    if (LENGTH(x) == nry) {	/* x is a col vector */
		nrx = nry; /* == LENGTH(x) */
		ncx = 1;
	    }
	    /* else if (nry == 1) ... not being too tolerant
	       to treat x as row vector, as t(x) *is* row vector */
	}
	else { /* tcrossprod */
	    if (LENGTH(x) == ncy) {	/* x as row vector */
		nrx = 1;
		ncx = ncy; /* == LENGTH(x) */
	    }
	    else if (ncy == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldy != 2) {		/* y not a matrix */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = 0;
	ncy = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(y) == ncx) {	/* y as col vector */
		nry = ncx;
		ncy = 1;
	    }
	    else if (ncx == 1) {	/* y as row vector */
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else if (PRIMVAL(op) == 1) { /* crossprod() */
	    if (LENGTH(y) == nrx) {	/* y is a col vector */
		nry = nrx;
		ncy = 1;
	    }
	}
	else { /* tcrossprod --		y is a col vector */
	    nry = LENGTH(y);
	    ncy = 1;
	}
    }
    else {				/* x and y matrices */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
    }
    /* nr[ow](.) and nc[ol](.) are now defined for x and y */

    if (PRIMVAL(op) == 0) {
	/* primitive, so use call */
	if (ncx != nry)
	    errorcall(call, _("non-conformable arguments"));
    }
    else if (PRIMVAL(op) == 1) {
	if (nrx != nry)
	    error(_("non-conformable arguments"));
    }
    else {
	if (ncx != ncy)
	    error(_("non-conformable arguments"));
    }

    mode = isComplex(x) || isComplex(y) ? CPLXSXP : REALSXP;
   
    if (TYPEOF(x)!=mode) {
        WAIT_UNTIL_COMPUTED(x);
        x = coerceVector(x, mode);
    }
    PROTECT(x);

    if (TYPEOF(y)!=mode) {
        WAIT_UNTIL_COMPUTED(y);
        y = coerceVector(y, mode);
    }
    PROTECT(y);

    if (PRIMVAL(op) == 0) {			/* op == 0 : %*% */

	ans = allocMatrix0 (mode, nrx, ncy);

        if (LENGTH(ans) != 0) {

            int inhlpr = nrx*(ncx+1.0)*ncy > T_matmult;
            int no_pipelining = !inhlpr || helpers_not_pipelining;
            helpers_task_proc *task_proc;
            int flags = 0;

            if (mode == CPLXSXP) {
                if (ncx==0) {
                    task_proc = task_cmatprod_zero;
                }
                else {
                    task_proc = task_cmatprod;
#ifndef R_MAT_MULT_WITH_BLAS_IN_HELPERS_OK
                    inhlpr = 0;
#endif
                }
            }
            else {
                if (ncx==0) {
                    task_proc = task_matprod_zero;
                }
                else if (nrx==1 && ncy==1) {
                    if (R_mat_mult_with_BLAS[0]) {
                        task_proc = task_matprod_vec_vec_BLAS;
#ifndef R_MAT_MULT_WITH_BLAS_IN_HELPERS_OK
                        inhlpr = 0;
#endif
                    }
                    else if (no_pipelining)
                        task_proc = task_matprod_vec_vec;
                    else {
                        task_proc = task_piped_matprod_vec_vec;
                        flags = HELPERS_PIPE_IN2;
                    }
                }
                else if (ncy==1) {
                    if (R_mat_mult_with_BLAS[1]) {
                        task_proc = task_matprod_mat_vec_BLAS;
#ifndef R_MAT_MULT_WITH_BLAS_IN_HELPERS_OK
                        inhlpr = 0;
#endif
                    }
                    else if (no_pipelining)
                        task_proc = task_matprod_mat_vec;
                    else {
                        task_proc = task_piped_matprod_mat_vec;
                        flags = HELPERS_PIPE_IN2;
                    }
                }
                else if (nrx==1) {
                    if (R_mat_mult_with_BLAS[2]) {
                        task_proc = task_matprod_vec_mat_BLAS;
#ifndef R_MAT_MULT_WITH_BLAS_IN_HELPERS_OK
                        inhlpr = 0;
#endif
                    }
                    else if (no_pipelining)
                        task_proc = task_matprod_vec_mat;
                    else {
                        task_proc = task_piped_matprod_vec_mat;
                        flags = HELPERS_PIPE_IN2_OUT;
                    }
                }
                else {
                    if (R_mat_mult_with_BLAS[3]) {
                        task_proc = task_matprod_BLAS;
#ifndef R_MAT_MULT_WITH_BLAS_IN_HELPERS_OK
                        inhlpr = 0;
#endif
                    }
                    else if (no_pipelining)
                        task_proc = task_matprod;
                    else {
                        task_proc = task_piped_matprod;
                        flags = HELPERS_PIPE_IN2_OUT;
                    }
                }
            }

            DO_NOW_OR_LATER2(variant, inhlpr, flags, task_proc, ncx, ans, x, y);
        }

        PROTECT(ans = allocMatrix1 (ans, nrx, ncy));

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx == 1) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }

#define YDIMS_ET_CETERA							\
	    if (ydims != R_NilValue) {					\
		if (ldy == 2) {						\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 1)); \
		} else if (nry == 1) {					\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0)); \
		}							\
	    }								\
									\
	    /* We sometimes attach a dimnames attribute			\
	     * whose elements are all NULL ...				\
	     * This is ugly but causes no real damage.			\
	     * Now (2.1.0 ff), we don't anymore: */			\
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||			\
		VECTOR_ELT(dimnames,1) != R_NilValue) {			\
		if (dnx != R_NilValue || dny != R_NilValue)		\
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);	\
		setAttrib(ans, R_DimNamesSymbol, dimnames);		\
	    }								\
	    UNPROTECT(2)

	    YDIMS_ET_CETERA;
	}
    }

    else if (PRIMVAL(op) == 1) {	/* op == 1: crossprod() */

	PROTECT(ans = allocMatrix(mode, ncx, ncy));

        WAIT_UNTIL_COMPUTED_2(x,y);

	if (mode == CPLXSXP)
	    if(sym)
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		crossprod(REAL(x), nrx, ncx,
			  REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {/* not nrx==1 : .. fixed, ihaka 2003-09-30 */
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 1));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 1));
		}
	    }

	    YDIMS_ET_CETERA;
	}

    }
    else {					/* op == 2: tcrossprod() */

	PROTECT(ans = allocMatrix(mode, nrx, nry));

        WAIT_UNTIL_COMPUTED_2(x,y);

	if (mode == CPLXSXP)
	    if(sym)
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symtcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		tcrossprod(REAL(x), nrx, ncx,
			   REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

	    if (xdims != R_NilValue) {
		if (ldx == 2) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }
	    if (ydims != R_NilValue) {
		if (ldy == 2) {
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));
		    dny = getAttrib(ydims, R_NamesSymbol);
		    if(!isNull(dny))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0));
		}
	    }
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||
		VECTOR_ELT(dimnames,1) != R_NilValue) {
		if (dnx != R_NilValue || dny != R_NilValue)
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
		setAttrib(ans, R_DimNamesSymbol, dimnames);
	    }

	    UNPROTECT(2);
	}
    }
    UNPROTECT(5);
    return ans;
}
#undef YDIMS_ET_CETERA

void task_transpose (helpers_op_t op, SEXP r, SEXP a, SEXP ignored)
{
    int nrow = EXTRACT_LENGTH1(op);
    int ncol = EXTRACT_LENGTH2(op);
    R_len_t len = LENGTH(a);
    R_len_t l_1 = len-1;
    R_len_t l_2 = len-2;
    unsigned i, j, k;

    HELPERS_SETUP_OUT (8);

    switch (TYPEOF(a)) {

    /* For LGL, INT, and REAL, access successive pairs from two rows, and 
       store in successive positions in two columns (except perhaps for the 
       first row & column).  This improves memory access performance.

       Keep in mind:  nrow & ncol are the number of rows and columns in the
                      input - this is swapped for the output! */

    case LGLSXP:
    case INTSXP:
        k = 0;
        i = 0;
        if (nrow & 1) {
            for (j = 0; i < ncol; j += nrow, i++) 
                INTEGER(r)[i] = INTEGER(a)[j];
            HELPERS_BLOCK_OUT(k,ncol);
        }
        j = nrow & 1;
        while (i < len) {
            INTEGER(r)[i] = INTEGER(a)[j]; 
            INTEGER(r)[i+ncol] = INTEGER(a)[j+1];
            i += 1; j += nrow;
            if (j >= len) { 
                i += ncol; 
                j -= l_2; 
                HELPERS_BLOCK_OUT(k,2*ncol);
            }
        }
        break;

    case REALSXP:
        k = 0;
        i = 0;
        if (nrow & 1) {
            for (j = 0; i < ncol; j += nrow, i++) 
                REAL(r)[i] = REAL(a)[j];
            HELPERS_BLOCK_OUT(k,ncol);
        }
        j = nrow & 1;
        while (i < len) {
            REAL(r)[i] = REAL(a)[j]; 
            REAL(r)[i+ncol] = REAL(a)[j+1];
            i += 1; j += nrow;
            if (j >= len) { 
                i += ncol; 
                j -= l_2; 
                HELPERS_BLOCK_OUT(k,2*ncol);
            }
        }
        break;

    /* For less-used types below, just access row-wise and store column-wise. */

    case CPLXSXP:
        for (i = 0, j = 0; i < len; j += nrow) {
            if (j > l_1) j -= l_1;
            COMPLEX(r)[i] = COMPLEX(a)[j];
            HELPERS_NEXT_OUT(i);
        }
        break;

    case RAWSXP:
        for (i = 0, j = 0; i < len; j += nrow) {
            if (j > l_1) j -= l_1;
            RAW(r)[i] = RAW(a)[j];
            HELPERS_NEXT_OUT(i);
        }
        break;

    case STRSXP:
        for (i = 0, j = 0; i < len; j += nrow) {
            if (j > l_1) j -= l_1;
            SET_STRING_ELT(r, i, STRING_ELT(a,j));
            HELPERS_NEXT_OUT(i);
        }
        break;

    case EXPRSXP:
    case VECSXP:
        for (i = 0, j = 0; i < len; j += nrow) {
            if (j > l_1) j -= l_1;
            SET_VECTOR_ELT(r, i, VECTOR_ELT(a,j));
            HELPERS_NEXT_OUT(i);
        }
        break;
    }
}

#define T_transpose THRESHOLD_ADJUST(10)

SEXP attribute_hidden do_transpose (SEXP call, SEXP op, SEXP args, SEXP rho,
                                    int variant)
{
    SEXP a, r, dims, dimnames, dimnamesnames, ndimnamesnames, rnames, cnames;
    int ldim, len, ncol, nrow;

    checkArity(op, args);
    a = CAR(args);

    if (!isVector(a)) goto not_matrix;

    dims = getAttrib(a, R_DimSymbol);
    ldim = length(dims); /* not LENGTH, since could be null */

    if (ldim > 2) goto not_matrix;

    len = LENGTH(a);
    nrow = ldim == 2 ? nrows(a) : len;
    ncol = ldim == 2 ? ncols(a) : 1;

    PROTECT(r = allocVector(TYPEOF(a), len));

    /* Start task.  Matrices with pointer elements must be done in master only.
       Since we don't assume that writing a pointer to memory is atomic, the
       garbage collector could read a garbage pointer if written in a helper. */

    DO_NOW_OR_LATER1 (variant, LENGTH(a) >= T_transpose,
        isVectorNonpointer(a) ? HELPERS_PIPE_OUT : HELPERS_MASTER_ONLY, 
        task_transpose, COMBINE_LENGTHS(nrow,ncol), r, a);

    rnames = R_NilValue;
    cnames = R_NilValue;
    dimnamesnames = R_NilValue;

    switch(ldim) {
    case 0:
        rnames = getAttrib(a, R_NamesSymbol);
        dimnames = rnames;/* for isNull() below*/
        break;
    case 1:
        dimnames = getAttrib(a, R_DimNamesSymbol);
        if (dimnames != R_NilValue) {
            rnames = VECTOR_ELT(dimnames, 0);
            dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
        }
        break;
    case 2:
        dimnames = getAttrib(a, R_DimNamesSymbol);
        if (dimnames != R_NilValue) {
            rnames = VECTOR_ELT(dimnames, 0);
            cnames = VECTOR_ELT(dimnames, 1);
            dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
        }
        break;
    }

    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1);

    /* R <= 2.2.0: dropped list(NULL,NULL) dimnames :
     * if(rnames != R_NilValue || cnames != R_NilValue) */
    if(!isNull(dimnames)) {
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, cnames);
	SET_VECTOR_ELT(dimnames, 1, rnames);
	if(!isNull(dimnamesnames)) {
	    PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
	    SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
	    SET_VECTOR_ELT(ndimnamesnames, 0,
			   (ldim == 2) ? STRING_ELT(dimnamesnames, 1):
			   R_BlankString);
	    setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
	    UNPROTECT(1);
	}
	setAttrib(r, R_DimNamesSymbol, dimnames);
	UNPROTECT(1);
    }
    copyMostAttrib(a, r);
    UNPROTECT(1);
    return r;

 not_matrix:
    error(_("argument is not a matrix"));
    return R_NilValue;/* never used; just for -Wall */
}

/*
 New version of aperm, using strides for speed.
 Jonathan Rougier <J.C.Rougier@durham.ac.uk>

 v1.0 30.01.01

 M.Maechler : expanded	all ../include/Rdefines.h macros
 */

/* this increments iip and sets j using strides */

#define CLICKJ						\
    for (itmp = 0; itmp < n; itmp++)			\
	if (iip[itmp] == isr[itmp]-1) iip[itmp] = 0;	\
	else {						\
	    iip[itmp]++;				\
	    break;					\
	}						\
    for (j = 0, itmp = 0; itmp < n; itmp++)	       	\
	j += iip[itmp] * stride[itmp];

/* aperm (a, perm, resize = TRUE) */
SEXP attribute_hidden do_aperm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, perm, r, dimsa, dimsr, dna;
    int i, j, n, len, itmp;

    checkArity(op, args);

    a = CAR(args);
    if (!isArray(a))
	error(_("invalid first argument, must be an array"));

    PROTECT(dimsa = getAttrib(a, R_DimSymbol));
    n = LENGTH(dimsa);
    int *isa = INTEGER(dimsa);

    /* check the permutation */

    int *pp = (int *) R_alloc((size_t) n, sizeof(int));
    perm = CADR(args);
    if (length(perm) == 0) {
	for (i = 0; i < n; i++) pp[i] = n-1-i;
    } else if (isString(perm)) {
	SEXP dna = getAttrib(a, R_DimNamesSymbol);
	if (isNull(dna))
	    error(_("'a' does not have named dimnames"));
	SEXP dnna = getAttrib(dna, R_NamesSymbol);
	if (isNull(dnna))
	    error(_("'a' does not have named dimnames"));
	for (i = 0; i < n; i++) {
	    const char *this = translateChar(STRING_ELT(perm, i));
	    for (j = 0; j < n; j++)
		if (streql(translateChar(STRING_ELT(dnna, j)),
			   this)) {pp[i] = j; break;}
	    if (j >= n)
		error(_("perm[%d] does not match a dimension name"), i+1);
	}
    } else {
	PROTECT(perm = coerceVector(perm, INTSXP));
	if (length(perm) == n) {
	    for (i = 0; i < n; i++) pp[i] = INTEGER(perm)[i] - 1;
	    UNPROTECT(1);
	} else error(_("'perm' is of wrong length"));
    }

    int *iip = (int *) R_alloc((size_t) n, sizeof(int));
    for (i = 0; i < n; iip[i++] = 0);
    for (i = 0; i < n; i++)
	if (pp[i] >= 0 && pp[i] < n) iip[pp[i]]++;
	else error(_("value out of range in 'perm'"));
    for (i = 0; i < n; i++)
	if (iip[i] == 0) error(_("invalid '%s' argument"), "perm");

    /* create the stride object and permute */

    int *stride = (int *) R_alloc((size_t) n, sizeof(int));
    for (iip[0] = 1, i = 1; i<n; i++) iip[i] = iip[i-1] * isa[i-1];
    for (i = 0; i < n; i++) stride[i] = iip[pp[i]];

    /* also need to have the dimensions of r */

    PROTECT(dimsr = allocVector(INTSXP, n));
    int *isr = INTEGER(dimsr);
    for (i = 0; i < n; i++) isr[i] = isa[pp[i]];

    /* and away we go! iip will hold the incrementer */

    len = LENGTH(a);
    len = length(a);
    PROTECT(r = allocVector(TYPEOF(a), len));

    for (i = 0; i < n; iip[i++] = 0);

    switch (TYPEOF(a)) {

    case INTSXP:
	for (j=0, i=0; i < len; i++) {
	    INTEGER(r)[i] = INTEGER(a)[j];
	    CLICKJ;
	}
	break;

    case LGLSXP:
	for (j=0, i=0; i < len; i++) {
	    LOGICAL(r)[i] = LOGICAL(a)[j];
	    CLICKJ;
	}
	break;

    case REALSXP:
	for (j=0, i=0; i < len; i++) {
	    REAL(r)[i] = REAL(a)[j];
	    CLICKJ;
	}
	break;

    case CPLXSXP:
	for (j=0, i=0; i < len; i++) {
	    COMPLEX(r)[i].r = COMPLEX(a)[j].r;
	    COMPLEX(r)[i].i = COMPLEX(a)[j].i;
	    CLICKJ;
	}
	break;

    case STRSXP:
	for (j=0, i=0; i < len; i++) {
	    SET_STRING_ELT(r, i, STRING_ELT(a, j));
	    CLICKJ;
	}
	break;

    case VECSXP:
	for (j=0, i=0; i < len; i++) {
	    SET_VECTOR_ELT(r, i, VECTOR_ELT(a, j));
	    CLICKJ;
	}
	break;

    case RAWSXP:
	for (j=0, i=0; i < len; i++) {
	    RAW(r)[i] = RAW(a)[j];
	    CLICKJ;
	}
	break;

    default:
	UNIMPLEMENTED_TYPE("aperm", a);
    }

    /* handle the resize */
    int resize = asLogical(CADDR(args));
    if (resize == NA_LOGICAL) error(_("'resize' must be TRUE or FALSE"));
    setAttrib(r, R_DimSymbol, resize ? dimsr : dimsa);

    /* and handle the dimnames, if any */
    if (resize) {
	PROTECT(dna = getAttrib(a, R_DimNamesSymbol));
	if (dna != R_NilValue) {
	    SEXP dnna, dnr, dnnr;

	    PROTECT(dnr  = allocVector(VECSXP, n));
	    PROTECT(dnna = getAttrib(dna, R_NamesSymbol));
	    if (dnna != R_NilValue) {
		PROTECT(dnnr = allocVector(STRSXP, n));
		for (i = 0; i < n; i++) {
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
		    SET_STRING_ELT(dnnr, i, STRING_ELT(dnna, pp[i]));
		}
		setAttrib(dnr, R_NamesSymbol, dnnr);
		UNPROTECT(1);
	    } else {
		for (i = 0; i < n; i++)
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
	    }
	    setAttrib(r, R_DimNamesSymbol, dnr);
	    UNPROTECT(2);
	}
	UNPROTECT(1);
    }

    UNPROTECT(3); /* dimsa, r, dimsr */
    return r;
}

/* colSums(x, n, p, na.rm) and also the same with "row" and/or "Means". */

void task_colSums_or_colMeans (helpers_op_t op, SEXP ans, SEXP x, SEXP ignored)
{
    int keepNA = op&1;        /* Don't skip NA/NaN elements? */
    int Means = op&2;         /* Find means rather than sums? */
    unsigned n = op>>3;       /* Number of rows in matrix */
    unsigned p = LENGTH(ans); /* Number of columns in matrix */
    double *a = REAL(ans);    /* Pointer to start of result vector */
    int np = n*p;             /* Number of elements we need in total */
    int avail = 0;            /* Number of input elements known to be computed*/

    int cnt;                  /* # elements not NA/NaN, if Means and !keepNA */
    int i, j;                 /* Row and column indexes */
    int k;                    /* Index going sequentially through whole matrix*/

    HELPERS_SETUP_OUT (n>500 ? 3 : n>50 ? 4 : 5);

    if (TYPEOF(x) == REALSXP) {
        double *rx = REAL(x);
        long double sum;
        k = 0;
        j = 0;
        if (keepNA) {
            while (j < p) {
                if (avail < k+n) HELPERS_WAIT_IN1 (avail, k+n-1, np);
                sum = (n & 1) ? rx[k++] : 0.0;
                for (i = n - (n & 1); i > 0; i -= 2) { 
                    sum += rx[k++];
                    sum += rx[k++];
                }
                a[j] = !Means ? sum : sum/n;
                HELPERS_NEXT_OUT (j);
            }
        }
        else { /* ! keepNA */
            if (!Means) {
                while (j < p) {
                    if (avail < k+n) HELPERS_WAIT_IN1 (avail, k+n-1, np);
                    for (sum = 0.0, i = n; i > 0; i--, k++)
                        if (!ISNAN(rx[k])) sum += rx[k];
                    a[j] = sum;
                    HELPERS_NEXT_OUT (j);
                }
            }
            else {
                while (j < p) {
                    if (avail < k+n) HELPERS_WAIT_IN1 (avail, k+n-1, np);
                    for (cnt = 0, sum = 0.0, i = n; i > 0; i--, k++)
                        if (!ISNAN(rx[k])) { cnt += 1; sum += rx[k]; }
                    a[j] = sum/cnt;
                    HELPERS_NEXT_OUT (j);
                }
            }
        }
    }

    else {

        int_fast64_t lsum; /* good to sum up to 2^32 integers or 2^63 logicals*/
        int *ix;

        switch (TYPEOF(x)) {
        case INTSXP:
            ix = INTEGER(x);
            k = 0;
            j = 0;
            while (j < p) {
                if (avail < k+n) HELPERS_WAIT_IN1 (avail, k+n-1, np);
                for (cnt = 0, lsum = 0, i = 0; i < n; i++, k++)
                    if (ix[k] != NA_INTEGER) {
                        cnt += 1; 
                        lsum += ix[k];
                    }
                    else if (keepNA) {
                        a[j] = NA_REAL;
                        k += n-i;
                        goto next_int;
                    }
                a[j] = !Means ? lsum : (double)lsum/cnt;
              next_int:
                HELPERS_NEXT_OUT (j);
            }
            break;
        case LGLSXP:
            ix = LOGICAL(x);
            k = 0;
            j = 0;
            while (j < p) {
                if (avail < k+n) HELPERS_WAIT_IN1 (avail, k+n-1, np);
                for (cnt = 0, lsum = 0, i = 0; i < n; i++, k++)
                    if (ix[k] != NA_LOGICAL) {
                        cnt += 1; 
                        lsum += ix[k];
                    }
                    else if (keepNA) {
                        a[j] = NA_REAL; 
                        k += n-i;
                        goto next_logical;
                    }
                a[j] = !Means ? lsum : (double)lsum/cnt;
              next_logical:
                HELPERS_NEXT_OUT (j);
            }
            break;
        }
    }
}

#define rowSums_together 16   /* Sum this number of rows (or fewer) together */

void task_rowSums_or_rowMeans (helpers_op_t op, SEXP ans, SEXP x, SEXP ignored)
{
    int keepNA = op&1;        /* Don't skip NA/NaN elements? */
    int Means = op&2;         /* Find means rather than sums? */
    unsigned p = op>>3;       /* Number of columns in matrix */
    unsigned n = LENGTH(ans); /* Number of rows in matrix */
    double *a = REAL(ans);    /* Pointer to result vector, initially the start*/

    int i, j;                 /* Row and column indexes */

    HELPERS_SETUP_OUT (p>20 ? 4 : 5);

    if (TYPEOF(x) == REALSXP) {

        i = 0;
        while (i < n) {

            long double sums[rowSums_together];
            int cnts[rowSums_together];
            double *rx, *rx2;
            long double *s;
            int k, u;
            int *c;

            rx = REAL(x) + i;
            u = n - i;
            if (u > rowSums_together) u = rowSums_together;

            if (keepNA) { /* uses unwrapped loop to sum two columns at once */
                if (p & 1) {
                    for (k = u, s = sums, rx2 = rx; k > 0; k--, s++, rx2++) 
                        *s = *rx2;
                    rx += n;
                }
                else
                    for (k = u, s = sums; k > 0; k--, s++) 
                        *s = 0.0;
                for (j = p - (p & 1); j > 0; j -= 2) {
                    for (k = u, s = sums, rx2 = rx; k > 0; k--, s++, rx2++) {
                        *s += *rx2;
                        *s += *(rx2+n);
                    }
                    rx += 2*n;
                }
                if (!Means)
                    for (k = u, s = sums; k > 0; k--, a++, s++) 
                        *a = *s;
                else
                    for (k = u, s = sums; k > 0; k--, a++, s++)
                        *a = (*s)/p;
            }

            else { /* ! keepNA */
                if (!Means) {
                    s = sums;
                    for (k = u; k > 0; k--, s++) 
                        *s = 0.0; 
                    for (j = p; j > 0; j--) {
                        for (k = u, s = sums, rx2 = rx; k > 0; k--, s++, rx2++)
                            if (!ISNAN(*rx2)) *s += *rx2;
                        rx += n;
                    }
                    for (k = u, s = sums; k > 0; k--, a++, s++) 
                        *a = *s;
                }
                else {
                    for (k = u, s = sums, c = cnts; k > 0; k--, s++, c++) { 
                        *s = 0.0; 
                        *c = 0; 
                    }
                    for (j = p; j > 0; j--) {
                        for (k = u, s = sums, rx2 = rx, c = cnts; 
                             k > 0; 
                             k--, s++, rx2++, c++)
                            if (!ISNAN(*rx2)) { *s += *rx2; *c += 1; }
                        rx += n;
                    }
                    for (k = u, s = sums, c = cnts; k > 0; k--, a++, s++, c++) 
                        *a = (*s)/(*c);
                }
            }

            HELPERS_BLOCK_OUT (i, u);
        }
    }

    else {

        int_fast64_t lsum; /* good to sum up to 2^32 integers or 2^63 logicals*/
        int cnt;
        int *ix;

        switch (TYPEOF(x)) {
        case INTSXP:
            i = 0;
            while (i < n) {
                ix = INTEGER(x) + i;
                for (cnt = 0, lsum = 0, j = 0; j < p; j++, ix += n)
                    if (*ix != NA_INTEGER) {
                        cnt += 1; 
                        lsum += *ix;
                    }
                    else if (keepNA) {
                        a[i] = NA_REAL; 
                        goto next_int;
                    }
                a[i] = !Means ? lsum : (double)lsum/cnt;
              next_int:
                HELPERS_NEXT_OUT (i);
            }
            break;
        case LGLSXP:
            i = 0;
            while (i < n) {
                ix = LOGICAL(x) + i;
                for (cnt = 0, lsum = 0, j = 0; j < p; j++, ix += n)
                    if (*ix != NA_LOGICAL) {
                        cnt += 1; 
                        lsum += *ix;
                    }
                    else if (keepNA) {
                        a[i] = NA_REAL; 
                        goto next_logical;
                    }
                a[i] = !Means ? lsum : (double)lsum/cnt;
              next_logical:
                HELPERS_NEXT_OUT (i);
            }
            break;
        }
    }
}

#define T_colSums THRESHOLD_ADJUST(20)
#define T_rowSums THRESHOLD_ADJUST(20)

SEXP attribute_hidden do_colsum (SEXP call, SEXP op, SEXP args, SEXP rho, 
                                 int variant)
{
    SEXP x, ans;
    int OP, n, p;
    Rboolean NaRm;

    checkArity(op, args);

    /* we let x be being computed */
    x = CAR(args); args = CDR(args);
    /* other arguments we wait for */
    wait_until_arguments_computed(args);
    n = asInteger(CAR(args));    args = CDR(args);
    p = asInteger(CAR(args));    args = CDR(args);
    NaRm = asLogical(CAR(args)); args = CDR(args);

    if (n == NA_INTEGER || n < 0)
	error(_("invalid '%s' argument"), "n");
    if (p == NA_INTEGER || p < 0)
	error(_("invalid '%s' argument"), "p");
    if (NaRm == NA_LOGICAL) error(_("invalid '%s' argument"), "na.rm");

    switch (TYPEOF(x)) {
    case LGLSXP: break;
    case INTSXP: break;
    case REALSXP: break;
    default:
	error(_("'x' must be numeric"));
    }

    if ((double)n*p > LENGTH(x))
	error(_("invalid '%s' argument"), "n*p");

    OP = PRIMVAL(op);

    if (OP < 2) { /* columns */
        ans = allocVector (REALSXP, p);
        DO_NOW_OR_LATER1 (variant, LENGTH(x) >= T_colSums,
          HELPERS_PIPE_IN1_OUT, task_colSums_or_colMeans, 
          ((helpers_op_t)n<<3) | (OP<<1) | !NaRm, ans, x);
    }

    else { /* rows */
        ans = allocVector (REALSXP, n);
        DO_NOW_OR_LATER1 (variant, LENGTH(x) >= T_rowSums,
          HELPERS_PIPE_OUT, task_rowSums_or_rowMeans, 
          ((helpers_op_t)p<<3) | (OP<<1) | !NaRm, ans, x);
    }

    return ans;
}
