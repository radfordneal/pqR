/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2007   The R Development Core Team
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
 *
 *
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 *
 *
 *  Subscripts that are ranges:  "[" now asks for a variant result from
 *  a sequence operator (for first index), hoping to get a range rather
 *  than a sequence, allowing for faster extraction, and avoidance of
 *  memory use for the sequence.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"

/* JMC convinced MM that this was not a good idea: */
#undef _S4_subsettable

/* Convert range to vector. The caller must ensure that its size won't 
   overflow. */

static SEXP VectorFromRange(int rng0, int rng1)
{ 
    SEXP vec;
    int i, n;

    n = rng1>=rng0 ? rng1-rng0+1 : 0;
  
    vec = allocVector(INTSXP, n);

    for (i = 0; i<n; i++) 
        INTEGER(vec)[i] = rng0 + i;
  
    return vec;
}

/* Take a pointer to a range, check for validity, and either extract
   a range (possibly empty) of positive subscripts into start and end
   and return R_NilValue, or convert the range to a vector of negative
   integer subscripts that is returned. */

static SEXP DecideVectorOrRange(SEXP rng, int *start, int *end, SEXP call)
{
    int rng0, rng1;

    if (TYPEOF(rng)!=INTSXP || LENGTH(rng)!=2) /* shouldn't happen*/
        errorcall(call, "internal inconsistency with variant op in subset!");

    rng0 = INTEGER(rng)[0];
    rng1 = INTEGER(rng)[1];

    if (rng0==0) rng0 = 1; /* get rid of 0 subscript at beginning or end */
    if (rng1==0) rng1 = -1;

    if (rng1<rng0) { /* make any null range be 1:0 (avoid overflow, etc) */
        rng0 = 1; 
        rng1 = 0; 
    }

    if (rng0<0 && rng1>0) 
        errorcall(call, _("only 0's may be mixed with negative subscripts"));

    if (rng0>0) {
        *start = rng0;
        *end = rng1;
        return R_NilValue;
    }
    else {
        return VectorFromRange(rng0,rng1);
    }
}


/* ExtractRange does the transfer of elements from "x" to "result" 
   according to the range given by start and end.  The caller will
   have allocated "result" to be at least the required length, and
   for VECSXP and EXPRSXP, the entries will be R_NilValue (done by
   allocVector). */

static SEXP ExtractRange(SEXP x, SEXP result, int start, int end, SEXP call)
{
    SEXP tmp, tmp2;
    int mode, nx, n, m, i;

    if (x == R_NilValue)
        return x;

    mode = TYPEOF(x);

    nx = length(x);

    start -= 1;
    n = end-start;
    m = end<=nx ? n : nx-start;

    tmp = result;

    switch (mode) {
    case LGLSXP:
        memcpy (LOGICAL(result), LOGICAL(x)+start, m * sizeof *LOGICAL(x));
        for (i = m; i<n; i++) LOGICAL(result)[i] = NA_LOGICAL;
        break;
    case INTSXP:
        memcpy (INTEGER(result), INTEGER(x)+start, m * sizeof *INTEGER(x));
        for (i = m; i<n; i++) INTEGER(result)[i] = NA_INTEGER;
        break;
    case REALSXP:
        memcpy (REAL(result), REAL(x)+start, m * sizeof *REAL(x));
        for (i = m; i<n; i++) REAL(result)[i] = NA_REAL;
        break;
    case CPLXSXP:
        memcpy (COMPLEX(result), COMPLEX(x)+start, m * sizeof *COMPLEX(x));
        for (i = m; i<n; i++) {
            COMPLEX(result)[i].r = NA_REAL;
            COMPLEX(result)[i].i = NA_REAL;
        }
        break;
    case STRSXP:
        copy_string_elements (result, 0, x, start, m);
        for (i = m; i<n; i++) SET_STRING_ELT(result, i, NA_STRING);
        break;
    case VECSXP:
    case EXPRSXP:
        copy_vector_elements (result, 0, x, start, m);
        /* remaining elements already set to R_NilValue */
        break;
    case LISTSXP:
            /* cannot happen: pairlists are coerced to lists */
    case LANGSXP:
        for (i = 0; i<m; i++) {
            tmp2 = nthcdr(x, start+i);
            SETCAR(tmp, CAR(tmp2));
            SET_TAG(tmp, TAG(tmp2));
            tmp = CDR(tmp);
        }
        for ( ; i<n; i++) {
            SETCAR(tmp, R_NilValue);
            tmp = CDR(tmp);
        }
        break;
    case RAWSXP:
        memcpy (RAW(result), RAW(x)+start, m * sizeof *RAW(x));
        for (i = m; i<n; i++) RAW(result)[i] = (Rbyte) 0;
        break;
    default:
        errorcall(call, R_MSG_ob_nonsub, type2char(mode));
    }

    return result;
}


/* ExtractSubset does the transfer of elements from "x" to "result" 
   according to the integer subscripts given in "indx". The caller will
   have allocated "result" to be at least the required length, and
   for VECSXP and EXPRSXP, the entries will be R_NilValue (done by
   allocVector). */

static SEXP ExtractSubset(SEXP x, SEXP result, SEXP indx, SEXP call)
{
    int i, ii, n, nx, mode;
    SEXP tmp, tmp2;
    mode = TYPEOF(x);
    n = LENGTH(indx);
    nx = length(x);
    tmp = result;

    if (x == R_NilValue)
	return x;

    switch (mode) {
    case LGLSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                LOGICAL(result)[i] = NA_LOGICAL;
            else
                LOGICAL(result)[i] = LOGICAL(x)[ii];
        break;
    case INTSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                INTEGER(result)[i] = NA_INTEGER;
            else
                INTEGER(result)[i] = INTEGER(x)[ii];
        break;
    case REALSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                REAL(result)[i] = NA_REAL;
            else
                REAL(result)[i] = REAL(x)[ii];
        break;
    case CPLXSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx) {
                COMPLEX(result)[i].r = NA_REAL;
                COMPLEX(result)[i].i = NA_REAL; 
            }
            else
                COMPLEX(result)[i] = COMPLEX(x)[ii];
        break;
    case STRSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                SET_STRING_ELT(result, i, NA_STRING);
            else
                SET_STRING_ELT(result, i, STRING_ELT(x, ii));
        break;
    case VECSXP:
    case EXPRSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                /* nothing, already R_NilValue */ ;
            else
                SET_VECTOR_ELT(result, i, VECTOR_ELT(x, ii));
        break;
    case LISTSXP:
	    /* cannot happen: pairlists are coerced to lists */
    case LANGSXP:
        for (i = 0; i<n; i++) {
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                SETCAR(tmp, R_NilValue);
            else {
                tmp2 = nthcdr(x, ii);
                SETCAR(tmp, CAR(tmp2));
                SET_TAG(tmp, TAG(tmp2));
            }
            tmp = CDR(tmp);
        }
        break;
    case RAWSXP:
        for (i = 0; i<n; i++)
            if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                RAW(result)[i] = (Rbyte) 0;
            else
                RAW(result)[i] = RAW(x)[ii];
        break;
    default:
        errorcall(call, R_MSG_ob_nonsub, type2char(mode));
    }

    return result;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP s, SEXP call)
{
    int mode, spi, stretch = 1;
    SEXP result, attrib, nattrib;
    int start = 1, end = 0, n = 0;
    SEXP indx = R_NilValue;

    if (s == R_MissingArg) return duplicate(x);

    PROTECT_WITH_INDEX (s, &spi);
    attrib = getAttrib(x, R_DimSymbol);

    /* Check for variant result, which will be a range rather than a vector, 
       and if we have a range, see whether it can be used directly, or must
       be converted to a vector to be handled as other vectors. */

    if (ATTRIB(s) == R_VariantResult) {
        REPROTECT(s = DecideVectorOrRange(s,&start,&end,call), spi);
        if (s == R_NilValue)
            n = end - start + 1;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    if (s != R_NilValue 
          && isMatrix(s) && isArray(x) && ncols(s) == length(attrib)) {
        if (isString(s)) {
            s = strmat2intmat(s, GetArrayDimnames(x), call);
            REPROTECT(s,spi);
        }
        if (isInteger(s) || isReal(s)) {
            s = mat2indsub(attrib, s, call);
            REPROTECT(s,spi);
        }
    }

    /* Convert s to a vector of integer subscripts */

    if (s != R_NilValue) {
        PROTECT(indx = makeSubscript(x, s, &stretch, call, 0));
        n = LENGTH(indx);
    }

    /* Allocate the result. */

    mode = TYPEOF(x);
    /* No protection needed as ExtractSubset and ExtractRange don't allocate */
    result = allocVector(mode, n);
    if (mode == VECSXP || mode == EXPRSXP)
	/* we do not duplicate the values when extracting the subset,
	   so to be conservative mark the result as NAMED = 2 */
	SET_NAMEDCNT_MAX(result);

    PROTECT(result = s==R_NilValue ? ExtractRange(x, result, start, end, call)
                                   : ExtractSubset(x, result, indx, call));

    if (result != R_NilValue) {
	if (
	    ((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
	    ( /* here we might have an array.  Use row names if 1D */
		isArray(x) && LENGTH(getAttrib(x, R_DimNamesSymbol)) == 1 &&
		(attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
		(attrib = GetRowNames(attrib)) != R_NilValue
		)
	    ) {
	    nattrib = allocVector(TYPEOF(attrib), n);
	    PROTECT(nattrib = s==R_NilValue 
                               ? ExtractRange(attrib, nattrib, start, end, call)
                               : ExtractSubset(attrib, nattrib, indx, call));
	    setAttrib(result, R_NamesSymbol, nattrib);
	    UNPROTECT(1);
	}
	if ((attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
	    TYPEOF(attrib) == VECSXP) {
	    nattrib = allocVector(VECSXP, n);
	    PROTECT(nattrib = s==R_NilValue 
                               ? ExtractRange(attrib, nattrib, start, end, call)
                               : ExtractSubset(attrib, nattrib, indx, call));
	    setAttrib(result, R_SrcrefSymbol, nattrib);
	    UNPROTECT(1);
	}
	/* FIXME:  this is wrong, because the slots are gone, so result is an invalid object of the S4 class! JMC 3/3/09 */
#ifdef _S4_subsettable
	if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	    SET_S4_OBJECT(result);
	}
#endif
    }

    UNPROTECT(2 + (s!=R_NilValue));

    return result;
}


/* Used in MatrixSubset to set a whole row or column of a matrix to NAs. */

static void set_row_or_col_to_na (SEXP result, int start, int step, int end,
                                  SEXP call)
{
    int i;
    switch (TYPEOF(result)) {
    case LGLSXP:
        for (i = start; i<end; i += step)
            LOGICAL(result)[i] = NA_LOGICAL;
        break;
    case INTSXP:
        for (i = start; i<end; i += step)
            INTEGER(result)[i] = NA_INTEGER;
        break;
    case REALSXP:
        for (i = start; i<end; i += step)
            REAL(result)[i] = NA_REAL;
        break;
    case CPLXSXP:
        for (i = start; i<end; i += step) {
            COMPLEX(result)[i].r = NA_REAL;
            COMPLEX(result)[i].i = NA_REAL;
        }
        break;
    case STRSXP:
        for (i = start; i<end; i += step)
            SET_STRING_ELT(result, i, NA_STRING);
        break;
    case VECSXP:
        for (i = start; i<end; i += step)
            SET_VECTOR_ELT(result, i, R_NilValue);
        break;
    case RAWSXP:
        for (i = start; i<end; i += step)
            RAW(result)[i] = (Rbyte) 0;
        break;
    default:
        errorcall(call, _("matrix subscripting not handled for this type"));
    }
}


/* Used in MatrixSubset when only one (valid) row is accessed. */

static void one_row_of_matrix (SEXP call, SEXP x, SEXP result, 
                               int ii, int nr, SEXP sc, int ncs, int nc)
{
    int typeofx = TYPEOF(x);
    int j, jj, st;

    st = (ii-1) - nr;

    for (j = 0; j < ncs; j++) {

        jj = INTEGER(sc)[j];

        if (jj == NA_INTEGER) {
            set_row_or_col_to_na (result, j, 1, j+1, call);
            continue;
        }

        if (jj < 1 || jj > nc)
            errorcall(call, R_MSG_subs_o_b);

        switch (typeofx) {
        case LGLSXP:
            LOGICAL(result)[j] = LOGICAL(x) [st+jj*nr];
            break;
        case INTSXP:
            INTEGER(result)[j] = INTEGER(x) [st+jj*nr];
            break;
        case REALSXP:
            REAL(result)[j] = REAL(x) [st+jj*nr];
            break;
        case CPLXSXP:
            COMPLEX(result)[j] = COMPLEX(x) [st+jj*nr];
            break;
        case STRSXP:
            SET_STRING_ELT(result, j, STRING_ELT(x, st+jj*nr));
            break;
        case VECSXP:
            SET_VECTOR_ELT(result, j, VECTOR_ELT(x, st+jj*nr));
            break;
        case RAWSXP:
            RAW(result)[j] = RAW(x) [st+jj*nr];
            break;
        default:
            errorcall(call, _("matrix subscripting not handled for this type"));
        }
    }
}


/* Used in MatrixSubset for subsetting with range of rows. */

static void range_of_rows_of_matrix (SEXP call, SEXP x, SEXP result, 
    int start, int nrs, int nr, SEXP sc, int ncs, int nc)
{
    int i, j, jj, ij, jjnr;

    start -= 1;

    if (start < 0 || start+nrs > nr)
        errorcall(call, R_MSG_subs_o_b);

    /* Loop to handle extraction, with outer loop over columns. */

    ij = 0;
    for (j = 0; j < ncs; j++) {
        jj = INTEGER(sc)[j];

        /* If column index is NA, just set column of result to NAs. */

        if (jj == NA_INTEGER) {
            set_row_or_col_to_na (result, ij, 1, ij+nrs, call);
            ij += nrs;
            continue;
        }

        /* Check for bad column index. */

        if (jj < 1 || jj > nc)
            errorcall(call, R_MSG_subs_o_b);

        /* Loops over range of rows. */

        jjnr = (jj-1) * nr + start;
        switch (TYPEOF(x)) {
        case LGLSXP:
            memcpy (LOGICAL(result)+ij, LOGICAL(x)+jjnr, 
                    nrs * sizeof *LOGICAL(x));
            break;
        case INTSXP:
            memcpy (INTEGER(result)+ij, INTEGER(x)+jjnr, 
                    nrs * sizeof *INTEGER(x));
            break;
        case REALSXP:
            memcpy (REAL(result)+ij, REAL(x)+jjnr, 
                    nrs * sizeof *REAL(x));
            break;
        case CPLXSXP:
            memcpy (COMPLEX(result)+ij, COMPLEX(x)+jjnr, 
                    nrs * sizeof *COMPLEX(x));
            break;
        case STRSXP:
            copy_string_elements (result, ij, x, jjnr, nrs);
            break;
        case VECSXP:
            copy_vector_elements (result, ij, x, jjnr, nrs);
            break;
        case RAWSXP:
            memcpy (RAW(result)+ij, RAW(x)+jjnr, 
                    nrs * sizeof *RAW(x));
            break;
        default:
            errorcall(call, _("matrix subscripting not handled for this type"));
        }

        ij += nrs;
    }
}


/* Used in MatrixSubset for the general case of subsetting. */

static void multiple_rows_of_matrix (SEXP call, SEXP x, SEXP result, 
    SEXP sr, int nrs, int nr, SEXP sc, int ncs, int nc)
{
    int i, j, ii, jj, ij, iijj, jjnr;

    /* Set rows of result to NAs where there are NA row indexes.  Also check 
       for bad row indexes (once here rather than many times in loop). */

    for (i = 0; i < nrs; i++) {
        ii = INTEGER(sr)[i];
        if (ii == NA_INTEGER) 
            set_row_or_col_to_na (result, i, nrs, i+nrs*ncs, call);
        else if (ii < 1 || ii > nr)
            errorcall(call, R_MSG_subs_o_b);
    }

    /* Loop to handle extraction except for NAs.  Outer loop is over columns so
       writes are sequential, which is faster for indexing, and probably better
       for memory speed. */

    for (j = 0, ij = 0; j < ncs; j++) {
        jj = INTEGER(sc)[j];

        /* If column index is NA, just set column of result to NAs. */

        if (jj == NA_INTEGER) {
            set_row_or_col_to_na (result, j*nrs, 1, (j+1)*nrs, call);
            ij += nrs;
            continue;
        }

        /* Check for bad column index. */

        if (jj < 1 || jj > nc)
            errorcall(call, R_MSG_subs_o_b);

        /* Loops over row indexes, except skips NA row indexes, done above. */

        jjnr = (jj-1) * nr;
        switch (TYPEOF(x)) {
        case LGLSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    LOGICAL(result)[ij] = LOGICAL(x)[(ii-1)+jjnr];
            break;
        case INTSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    INTEGER(result)[ij] = INTEGER(x)[(ii-1)+jjnr];
            break;
        case REALSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    REAL(result)[ij] = REAL(x)[(ii-1)+jjnr];
            break;
        case CPLXSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    COMPLEX(result)[ij] = COMPLEX(x)[(ii-1)+jjnr];
            break;
        case STRSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    SET_STRING_ELT(result, ij, STRING_ELT(x, (ii-1)+jjnr));
            break;
        case VECSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    SET_VECTOR_ELT(result, ij, VECTOR_ELT(x, (ii-1)+jjnr));
            break;
        case RAWSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                    RAW(result)[ij] = RAW(x)[(ii-1)+jjnr];
            break;
        default:
            errorcall(call, _("matrix subscripting not handled for this type"));
        }
    }
}


/* Subset for a vector with dim attribute specifying two dimensions. */

static SEXP MatrixSubset(SEXP x, SEXP s0, SEXP s1, SEXP call, int drop)
{
    SEXP attr, result, sr, sc;
    int start = 1, end = 0;
    int nr, nc, nrs = 0, ncs = 0;
    int nprotect = 0;
    int ii;

    SEXP dim = getAttrib(x, R_DimSymbol);

    nr = INTEGER(dim)[0];
    nc = INTEGER(dim)[1];

    if (s0 == R_MissingArg) {
        s0 = R_NilValue;
        start = 1;
        end = nr;
        nrs = nr;
    }
    else if (ATTRIB(s0) == R_VariantResult) {
        PROTECT(s0 = DecideVectorOrRange(s0,&start,&end,call));
        nprotect++;
        if (s0 == R_NilValue)
            nrs = end - start + 1;
    }

    if (s0 != R_NilValue) {
        PROTECT (sr = arraySubscript(0, s0, dim, getAttrib, (STRING_ELT), x));
        nprotect++;
        nrs = LENGTH(sr);
    }

    PROTECT (sc = arraySubscript(1, s1, dim, getAttrib, (STRING_ELT), x));
    nprotect++;
    ncs = LENGTH(sc);

    /* Check this does not overflow */
    if ((double)nrs * (double)ncs > R_LEN_T_MAX)
        error(_("dimensions would exceed maximum size of array"));

    PROTECT (result = allocVector(TYPEOF(x), nrs*ncs));
    nprotect++;

    /* Extract elements from matrix x to result. */

    if (s0 == R_NilValue) 
        range_of_rows_of_matrix(call, x, result, start, nrs, nr, sc, ncs, nc);
    else if (nrs == 1 && (ii = INTEGER(sr)[0]) != NA_INTEGER 
                      && ii >= 0 && ii <= nr)
        one_row_of_matrix (call, x, result, ii, nr, sc, ncs, nc);
    else
        multiple_rows_of_matrix (call, x, result, sr, nrs, nr, sc, ncs, nc);

    /* Set up dimensions attribute. */

    if(nrs >= 0 && ncs >= 0) {
	PROTECT(attr = allocVector(INTSXP, 2));
        nprotect++;
	INTEGER(attr)[0] = nrs;
	INTEGER(attr)[1] = ncs;
	setAttrib(result, R_DimSymbol, attr);
    }

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.	 Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    if (nrs >= 0 && ncs >= 0) {
	SEXP dimnames, dimnamesnames, newdimnames;
	dimnames = getAttrib(x, R_DimNamesSymbol);
	dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	if (!isNull(dimnames)) {
	    PROTECT(newdimnames = allocVector(VECSXP, 2));
            nprotect++;
	    if (TYPEOF(dimnames) == VECSXP) {
	      SET_VECTOR_ELT(newdimnames, 0, s0 == R_NilValue 
		  ? ExtractRange (VECTOR_ELT(dimnames, 0),
				  allocVector(STRSXP, nrs), start, end, call)
		  : ExtractSubset(VECTOR_ELT(dimnames, 0),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1, 
                    ExtractSubset(VECTOR_ELT(dimnames, 1),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    else {
	      SET_VECTOR_ELT(newdimnames, 0, s0 == R_NilValue 
		  ? ExtractRange (CAR(dimnames),
				  allocVector(STRSXP, nrs), start, end, call)
		  : ExtractSubset(CAR(dimnames),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(CADR(dimnames),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(result, R_DimNamesSymbol, newdimnames);
	}
    }
    /*  Probably should not do this:
    copyMostAttrib(x, result); */
    if (drop)
	DropDims(result);

    UNPROTECT(nprotect);
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
    int i, j, k, ii, jj, mode, n;
    int **subs, *indx, *offset, *bound;
    SEXP dimnames, dimnamesnames, p, q, r, result, xdims;
    const void *vmaxsave;

    mode = TYPEOF(x);
    xdims = getAttrib(x, R_DimSymbol);
    k = length(xdims);

    vmaxsave = VMAXGET();
    subs = (int**)R_alloc(k, sizeof(int*));
    indx = (int*)R_alloc(k, sizeof(int));
    offset = (int*)R_alloc(k, sizeof(int));
    bound = (int*)R_alloc(k, sizeof(int));

    /* Construct a vector to contain the returned values. */
    /* Store its extents. */

    n = 1;
    r = s;
    for (i = 0; i < k; i++) {
	SETCAR(r, arraySubscript(i, CAR(r), xdims, getAttrib,
				 (STRING_ELT), x));
	bound[i] = LENGTH(CAR(r));
	n *= bound[i];
	r = CDR(r);
    }
    PROTECT(result = allocVector(mode, n));
    r = s;
    for (i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER(CAR(r));
	r = CDR(r);
    }
    offset[0] = 1;
    for (i = 1; i < k; i++)
	offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];

    /* Transfer the subset elements from "x" to "a". */

    for (i = 0; i < n; i++) {
	ii = 0;
	for (j = 0; j < k; j++) {
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) {
		ii = NA_INTEGER;
		goto assignLoop;
	    }
	    if (jj < 1 || jj > INTEGER(xdims)[j])
		errorcall(call, R_MSG_subs_o_b);
	    ii += (jj - 1) * offset[j];
	}

      assignLoop:
	switch (mode) {
	case LGLSXP:
	    if (ii != NA_INTEGER)
		LOGICAL(result)[i] = LOGICAL(x)[ii];
	    else
		LOGICAL(result)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    if (ii != NA_INTEGER)
		INTEGER(result)[i] = INTEGER(x)[ii];
	    else
		INTEGER(result)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (ii != NA_INTEGER)
		REAL(result)[i] = REAL(x)[ii];
	    else
		REAL(result)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    if (ii != NA_INTEGER) {
		COMPLEX(result)[i] = COMPLEX(x)[ii];
	    }
	    else {
		COMPLEX(result)[i].r = NA_REAL;
		COMPLEX(result)[i].i = NA_REAL;
	    }
	    break;
	case STRSXP:
	    if (ii != NA_INTEGER)
		SET_STRING_ELT(result, i, STRING_ELT(x, ii));
	    else
		SET_STRING_ELT(result, i, NA_STRING);
	    break;
	case VECSXP:
	    if (ii != NA_INTEGER)
		SET_VECTOR_ELT(result, i, VECTOR_ELT(x, ii));
	    else
		SET_VECTOR_ELT(result, i, R_NilValue);
	    break;
	case RAWSXP:
	    if (ii != NA_INTEGER)
		RAW(result)[i] = RAW(x)[ii];
	    else
		RAW(result)[i] = (Rbyte) 0;
	    break;
	default:
	    errorcall(call, _("array subscripting not handled for this type"));
	    break;
	}
	if (n > 1) {
	    j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % k;
	    }
	}
    }

    PROTECT(xdims = allocVector(INTSXP, k));
    for(i = 0 ; i < k ; i++)
	INTEGER(xdims)[i] = bound[i];
    setAttrib(result, R_DimSymbol, xdims);
    UNPROTECT(1);

    /* The array elements have been transferred. */
    /* Now we need to transfer the attributes. */
    /* Most importantly, we need to subset the */
    /* dimnames of the returned value. */

    dimnames = getAttrib(x, R_DimNamesSymbol);
    dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
    if (dimnames != R_NilValue) {
	/*SEXP xdims;
	int */ j = 0;
	PROTECT(xdims = allocVector(VECSXP, k));
	if (TYPEOF(dimnames) == VECSXP) {
	    r = s;
	    for (i = 0; i < k ; i++) {
		if (bound[i] > 0) {
		  SET_VECTOR_ELT(xdims, j++,
			ExtractSubset(VECTOR_ELT(dimnames, i),
				      allocVector(STRSXP, bound[i]),
				      CAR(r), call));
		} else { /* 0-length dims have NULL dimnames */
		    SET_VECTOR_ELT(xdims, j++, R_NilValue);
		}
		r = CDR(r);
	    }
	}
	else {
	    p = dimnames;
	    q = xdims;
	    r = s;
	    for(i = 0 ; i < k; i++) {
		SETCAR(q, allocVector(STRSXP, bound[i]));
		SETCAR(q, ExtractSubset(CAR(p), CAR(q), CAR(r), call));
		p = CDR(p);
		q = CDR(q);
		r = CDR(r);
	    }
	}
	setAttrib(xdims, R_NamesSymbol, dimnamesnames);
	setAttrib(result, R_DimNamesSymbol, xdims);
	UNPROTECT(1);
    }
    /* This was removed for matrices in 1998
       copyMostAttrib(x, result); */
    /* Free temporary memory */
    VMAXSET(vmaxsave);
    if (drop)
	DropDims(result);
    UNPROTECT(1);
    return result;
}


/* Returns and removes a named argument from argument list args.
   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NilValue is returned.
 */
static SEXP ExtractArg(SEXP args, SEXP arg_sym)
{
    SEXP arg, prev_arg;
    int found = 0;

    for (arg = prev_arg = args; arg != R_NilValue; arg = CDR(arg)) {
	if(TAG(arg) == arg_sym) {
	    if (arg == prev_arg) /* found at head of args */
		args = CDR(args);
	    else
		SETCDR(prev_arg, CDR(arg));
	    found = 1;
	    break;
	}
	else  prev_arg = arg;
    }
    return found ? CAR(arg) : R_NilValue;
}

/* Extracts the drop argument, if present, from the argument list.
   The object being subsetted must be the first argument. */
static void ExtractDropArg(SEXP el, int *drop)
{
    SEXP dropArg = ExtractArg(el, R_DropSymbol);
    *drop = asLogical(dropArg);
    if (*drop == NA_LOGICAL) *drop = 1;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used
 */
static int ExtractExactArg(SEXP args)
{
    SEXP argval = ExtractArg(args, R_ExactSymbol);
    int exact;
    if(isNull(argval)) return 1; /* Default is true as from R 2.7.0 */
    exact = asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}


/* Returns simple (positive or negative) index, or zero if not so simple. */

static R_INLINE R_len_t simple_index (SEXP s)
{
    switch (TYPEOF(s)) {
    case REALSXP:
        if (LENGTH(s) != 1 || ISNAN(REAL(s)[0])
         || REAL(s)[0] > R_LEN_T_MAX || REAL(s)[0] < -R_LEN_T_MAX)
            return 0;
         return (R_len_t) REAL(s)[0];
    case INTSXP:
        if (LENGTH(s) != 1 || INTEGER(s)[0] == NA_INTEGER)
            return 0;
        return INTEGER(s)[0];
    default:
        return 0;
    }
}


/* Look for the simple case of subscripting an atomic vector with one 
   valid integer or real subscript that is positive or negative (not zero, 
   NA, or out of bounds).  Returns the result, or R_NilValue if it's not 
   so simple.  The arguments x and s do not need to be protected before 
   this function is called. */

static SEXP one_vector_subscript (SEXP x, SEXP s)
{
    R_len_t ix, n;
    int typeofx;

    typeofx = TYPEOF(x);

    if (!isVectorAtomic(x))
        return R_NilValue;

    n = LENGTH(x);
    ix = simple_index (s);

    if (ix == 0 || ix > n || ix < -n)
        return R_NilValue;

    if (ix>0) {
        switch (typeofx) {
        case LGLSXP:  return ScalarLogical (LOGICAL(x)[ix-1]);
        case INTSXP:  return ScalarInteger (INTEGER(x)[ix-1]);
        case REALSXP: return ScalarReal (REAL(x)[ix-1]);
        case RAWSXP:  return ScalarRaw (RAW(x)[ix-1]);
        case STRSXP:  return ScalarString (STRING_ELT(x,ix-1));
        case CPLXSXP: return ScalarComplex (COMPLEX(x)[ix-1]);
        }
    }
    else { /* ix < 0 */

        R_len_t i, j, ex;
        SEXP r;

        PROTECT(x);
        r = allocVector (typeofx, n-1);

        j = 0;
        ix = -ix-1;
        ex = n-ix-1;

        switch (typeofx) {
        case LGLSXP: 
            if (ix!=0) memcpy(LOGICAL(r), LOGICAL(x), ix * sizeof *LOGICAL(r));
            if (ex!=0) memcpy(LOGICAL(r)+ix, LOGICAL(x)+ix+1, 
                                                      ex * sizeof *LOGICAL(r));
            break;
        case INTSXP: 
            if (ix!=0) memcpy(INTEGER(r), INTEGER(x), ix * sizeof *INTEGER(r));
            if (ex!=0) memcpy(INTEGER(r)+ix, INTEGER(x)+ix+1, 
                                                      ex * sizeof *INTEGER(r));
            break;
        case REALSXP: 
            if (ix!=0) memcpy(REAL(r), REAL(x), ix * sizeof *REAL(r));
            if (ex!=0) memcpy(REAL(r)+ix, REAL(x)+ix+1, ex * sizeof *REAL(r));
            break;
        case RAWSXP: 
            if (ix!=0) memcpy(RAW(r), RAW(x), ix * sizeof *RAW(r));
            if (ex!=0) memcpy(RAW(r)+ix, RAW(x)+ix+1, ex * sizeof *RAW(r));
            break;
        case STRSXP: 
            if (ix!=0) copy_string_elements (r, 0, x, 0, ix);
            if (ex!=0) copy_string_elements (r, ix, x, ix+1, ex); 
            break;
        case CPLXSXP: 
            if (ix!=0) memcpy(COMPLEX(r), COMPLEX(x), ix * sizeof *COMPLEX(r));
            if (ex!=0) memcpy(COMPLEX(r)+ix, COMPLEX(x)+ix+1, 
                                                      ex * sizeof *COMPLEX(r));
            break;
        }

        UNPROTECT(1);
        return r;
    }
}


/* Look for the simple case of subscripting an atomic matrix with two
   valid integer or real subscript that are positive (not negative, zero, 
   NA, or out of bounds).  Returns the result, or R_NilValue if it's not 
   so simple.  The arguments x, dim, s1, and s2 do not need to be 
   protected before this function is called. */

static SEXP two_matrix_subscripts (SEXP x, SEXP dim, SEXP s1, SEXP s2)
{
    R_len_t ix1, ix2, nrow, ncol, e;

    if (!isVectorAtomic(x))
        return R_NilValue;

    nrow = INTEGER(dim)[0];
    ix1 = simple_index (s1);
    if (ix1 <= 0 || ix1 > nrow)
        return R_NilValue;

    ncol = INTEGER(dim)[1];
    ix2 = simple_index (s2);
    if (ix2 <= 0 || ix2 > ncol)
        return R_NilValue;

    e = (ix1 - 1) + nrow * (ix2 - 1);

    switch (TYPEOF(x)) {
    case LGLSXP:  return ScalarLogical (LOGICAL(x)[e]);
    case INTSXP:  return ScalarInteger (INTEGER(x)[e]);
    case REALSXP: return ScalarReal (REAL(x)[e]);
    case RAWSXP:  return ScalarRaw (RAW(x)[e]);
    case STRSXP:  return ScalarString (STRING_ELT(x,e));
    case CPLXSXP: return ScalarComplex (COMPLEX(x)[e]);
    }
}


/* The "[" subset operator.
 * This provides the most general form of subsetting. */

SEXP attribute_hidden do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int argsevald = 0;
    int nprotect = 0;

    /* If we can easily determine that this will be handled by subset_dflt
       and has one or two index arguments in total, evaluate the first index
       with VARIANT_SEQ so it may come as a range rather than a vector. */

    if (args != R_NilValue && CAR(args) != R_DotsSymbol) {
        SEXP array = CAR(args);
        SEXP ixlist = CDR(args);
        if (ixlist != R_NilValue && TAG(ixlist) == R_NilValue) {
            SEXP idx = CAR(ixlist);
            if (TYPEOF(idx) == LANGSXP) {
                PROTECT(array = eval(array,rho));
                nprotect++;
                if (!isObject(array)) {
                    int variant = 0;
                    SEXP remargs = CDR(ixlist);
                    if (remargs != R_NilValue) {
                        PROTECT(remargs = evalListKeepMissing(remargs,rho));
                        nprotect++;
                    }
                    if (remargs == R_NilValue || CDR(remargs) == R_NilValue) 
                        variant = VARIANT_SEQ;
                    args = CONS(array,CONS(evalv(idx,rho,variant),remargs));
                    UNPROTECT(nprotect);
                    return do_subset_dflt(call, op, args, rho); 
                }
                else {
                    args = CONS(array,evalListKeepMissing(ixlist,rho));
                    UNPROTECT(nprotect);
                    argsevald = 1;
                }
            }
        }
    }

    /* If the first argument is an object and there is an */
    /* approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall through */
    /* to the generic code below.  Note that evaluation */
    /* retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "[", args, rho, &ans, 0, argsevald)) {
/*     if(DispatchAnyOrEval(call, op, "[", args, rho, &ans, 0, 0)) */
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);
	return(ans);
    }

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    return do_subset_dflt(call, op, ans, rho);
}


/* NB: do_subset_dflt is sometimes called directly, not just from do_subset */

SEXP attribute_hidden do_subset_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ax, px, x, subs;
    int drop, i, nsubs, type;
    SEXP cdrArgs = CDR(args);
    SEXP cddrArgs = CDR(cdrArgs);

    /* By default we drop extents of length 1 */

    if (CAR(args) != R_NilValue && cdrArgs != R_NilValue 
                                && TAG(cdrArgs) == R_NilValue) {

        /* Check for one subscript, handling simple cases like this */
        if (cddrArgs == R_NilValue) { 
            SEXP x = CAR(args);
            SEXP attr = ATTRIB(x);
            if (attr != R_NilValue) {
                if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
                    SEXP dim = CAR(attr);
                    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 1)
                        attr = R_NilValue;  /* only a dim attribute, 1D */
                }
            }
            if (attr == R_NilValue) {
                SEXP r = one_vector_subscript(x,CAR(cdrArgs));
                if (r != R_NilValue)
                    return r;
            }
        }

        /* Check for two subscripts, handling simple cases like this */
        else if (cddrArgs != R_NilValue && CDR(cddrArgs) == R_NilValue
	          && TAG(cddrArgs) == R_NilValue) { /* two subscripts */
            SEXP x = CAR(args);
            SEXP attr = ATTRIB(x);
            if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
                SEXP dim = CAR(attr);
                if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {
                    /* x is a matrix */
                    SEXP r = two_matrix_subscripts (x, dim, CAR(cdrArgs),
                                                            CAR(cddrArgs));
                    if (r != R_NilValue)
                        return r;
                }
            }
        }
    }

    PROTECT(args);

    drop = 1;
    ExtractDropArg(args, &drop);
    x = CAR(args);

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */
    /* FIXME: replace the test by isNull ... ? */

    if (x == R_NilValue) {
	UNPROTECT(1);
	return x;
    }
    subs = CDR(args);
    nsubs = length(subs);
    type = TYPEOF(x);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    ax = x;
    if (isVector(x))
	PROTECT(ax);
    else if (isPairList(x)) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	if (ndim > 1) {
	    PROTECT(ax = allocArray(VECSXP, dim));
	    setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    PROTECT(ax = allocVector(VECSXP, length(x)));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
	for(px = x, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    if(nsubs < 2) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	PROTECT(ans = VectorSubset(ax, (nsubs == 1 ? CAR(subs) : R_MissingArg),
				   call));
	/* one-dimensional arrays went through here, and they should
	   have their dimensions dropped only if the result has
	   length one and drop == TRUE
	*/
	if(ndim == 1) {
	    SEXP attr, attrib, nattrib;
	    int len = length(ans);

	    if(!drop || len > 1) {
		PROTECT(attr = allocVector(INTSXP, 1));
		INTEGER(attr)[0] = length(ans);
		setAttrib(ans, R_DimSymbol, attr);
		UNPROTECT(1);
		if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
		    /* reinstate dimnames, include names of dimnames */
		    PROTECT(nattrib = duplicate(attrib));
		    SET_VECTOR_ELT(nattrib, 0,
				   getAttrib(ans, R_NamesSymbol));
		    setAttrib(ans, R_DimNamesSymbol, nattrib);
		    setAttrib(ans, R_NamesSymbol, R_NilValue);
		    UNPROTECT(1);
		}
	    }
	}
    } else {
	if (nsubs != length(getAttrib(x, R_DimSymbol)))
	    errorcall(call, _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, CAR(subs), CADR(subs), call, drop);
	else
	    ans = ArraySubset(ax, subs, call, drop);
	PROTECT(ans);
    }

    /* Note: we do not coerce back to pair-based lists. */
    /* They are "defunct" in this version of R. */

    if (type == LANGSXP) {
	ax = ans;
	PROTECT(ans = allocList(LENGTH(ax)));
	if ( LENGTH(ax) > 0 )
	    SET_TYPEOF(ans, LANGSXP);
	for(px = ans, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SETCAR(px, VECTOR_ELT(ax, i++));
	setAttrib(ans, R_DimSymbol, getAttrib(ax, R_DimSymbol));
	setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
	setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
	SET_NAMEDCNT(ans, NAMEDCNT(ax)); /* PR#7924 */
    }
    else {
	PROTECT(ans);
    }
    if (ATTRIB(ans) != R_NilValue) { /* remove probably erroneous attr's */
	setAttrib(ans, R_TspSymbol, R_NilValue);
#ifdef _S4_subsettable
	if(!IS_S4_OBJECT(x))
#endif
	    setAttrib(ans, R_ClassSymbol, R_NilValue);
    }
    UNPROTECT(4);

    return ans;
}


/* The [[ subset operator.  It needs to be fast. */
/* The arguments to this call are evaluated on entry. */

SEXP attribute_hidden do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "[[", args, rho, &ans, 0, 0)) {
/*     if(DispatchAnyOrEval(call, op, "[[", args, rho, &ans, 0, 0)) */
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);
	return(ans);
    }

    /* Method dispatch has failed. */
    /* We now run the generic internal code. */

    return do_subset2_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, dimnames, indx, subs, x;
    int i, ndims, nsubs, offset = 0;
    int drop = 1, pok, exact = -1;
    int named_x;

    PROTECT(args);
    ExtractDropArg(args, &drop);
    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    exact = ExtractExactArg(args);
    if (exact == -1)
	pok = exact;
    else
	pok = !exact;

    x = CAR(args);

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */

    if (x == R_NilValue) {
	UNPROTECT(1);
	return x;
    }

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    subs = CDR(args);
    if(0 == (nsubs = length(subs)))
	errorcall(call, _("no index specified"));
    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    if(nsubs > 1 && nsubs != ndims)
	errorcall(call, _("incorrect number of subscripts"));

    /* code to allow classes to extend environment */
    if(TYPEOF(x) == S4SXP) {
        x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	  errorcall(call, _("this S4 class is not subsettable"));
    }

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
      if( nsubs != 1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	errorcall(call, _("wrong arguments for subsetting an environment"));
      ans = findVarInFrame(x, install(translateChar(STRING_ELT(CAR(subs), 0))));
      if( TYPEOF(ans) == PROMSXP ) {
	    PROTECT(ans);
	    ans = eval(ans, R_GlobalEnv);
	    UNPROTECT(1);
      } else {
	    SET_NAMEDCNT_MAX(ans);
      }

      UNPROTECT(1);
      if(ans == R_UnboundValue )
	  return(R_NilValue);
      if (NAMEDCNT_GT_0(ans))
	  SET_NAMEDCNT_MAX(ans);
      return(ans);
    }

    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    named_x = NAMEDCNT(x); /* x may change below; save this now. See PR#13411 */

    if(nsubs == 1) { /* vector indexing */
	SEXP thesub = CAR(subs);
	int len = length(thesub);

	if (len > 1)
	    x = vectorIndex(x, thesub, 0, len-1, pok, call);
	    
	offset = get1index(thesub, getAttrib(x, R_NamesSymbol),
			   length(x), pok, len > 1 ? len-1 : -1, call);
	if (offset < 0 || offset >= length(x)) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(1);
		return R_NilValue;
	    }
	    else errorcall(call, R_MSG_subs_o_b);
	}
    } else { /* matrix indexing */
	/* Here we use the fact that: */
	/* CAR(R_NilValue) = R_NilValue */
	/* CDR(R_NilValue) = R_NilValue */

	int ndn; /* Number of dimnames. Unlikely to be anything but
		    0 or nsubs, but just in case... */

	PROTECT(indx = allocVector(INTSXP, nsubs));
	dimnames = getAttrib(x, R_DimNamesSymbol);
	ndn = length(dimnames);
	for (i = 0; i < nsubs; i++) {
	    INTEGER(indx)[i] =
		get1index(CAR(subs), (i < ndn) ? VECTOR_ELT(dimnames, i) :
			  R_NilValue,
			  INTEGER(indx)[i], pok, -1, call);
	    subs = CDR(subs);
	    if (INTEGER(indx)[i] < 0 ||
		INTEGER(indx)[i] >= INTEGER(dims)[i])
		errorcall(call, R_MSG_subs_o_b);
	}
	offset = 0;
	for (i = (nsubs - 1); i > 0; i--)
	    offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	offset += INTEGER(indx)[0];
	UNPROTECT(1);
    }

    if(isPairList(x)) {
	ans = CAR(nthcdr(x, offset));
	if (named_x > NAMEDCNT(ans))
	    SET_NAMEDCNT(ans, named_x);
    } else if(isVectorList(x)) {
	/* did unconditional duplication before 2.4.0 */
	ans = VECTOR_ELT(x, offset);
	if (named_x > NAMEDCNT(ans))
	    SET_NAMEDCNT(ans, named_x);
    } else {
	ans = allocVector(TYPEOF(x), 1);
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    INTEGER(ans)[0] = INTEGER(x)[offset];
	    break;
	case REALSXP:
	    REAL(ans)[0] = REAL(x)[offset];
	    break;
	case CPLXSXP:
	    COMPLEX(ans)[0] = COMPLEX(x)[offset];
	    break;
	case STRSXP:
	    SET_STRING_ELT(ans, 0, STRING_ELT(x, offset));
	    break;
	case RAWSXP:
	    RAW(ans)[0] = RAW(x)[offset];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("do_subset2", x);
	}
    }
    UNPROTECT(1);
    return ans;
}

/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
SEXP attribute_hidden do_subset3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP from, what, ans, input;

    SEXP string = R_NilValue;
    SEXP name = R_NilValue;
    int argsevald = 0;

    checkArity(op, args);
    from = CAR(args);
    what = CADR(args);

    if (isSymbol(what))
        name = what;
    else if (isString(what)) 
        string = STRING_ELT(what,0);
    else
	errorcall(call, _("invalid subscript type '%s'"), 
                        type2char(TYPEOF(what)));

    /* Handle usual case with no "..." and not from an object quickly, without
       overhead of allocation and calling of DispatchOrEval. */

    if (from != R_DotsSymbol) {
        from = eval (from, env);
        if (isObject(from)) {
            PROTECT(from);
            argsevald = 1;
        } else 
            return R_subset3_dflt (from, string, name, call);
    }

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */
    PROTECT(input = allocVector(STRSXP, 1));

    if (name!=R_NilValue)
	SET_STRING_ELT(input, 0, PRINTNAME(name));
    else
	SET_STRING_ELT(input, 0, string);

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when nlist was */
    /* ..., as in PR#8718 */
    PROTECT(args = CONS(from, CONS(input, R_NilValue)));

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "$", args, env, &ans, 0, argsevald)) {
        UNPROTECT(2+argsevald);
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);
	return(ans);
    }

    UNPROTECT(2+argsevald);
    return R_subset3_dflt(CAR(ans), string, name, call);
}

/* Used above and in eval.c.  The field to extract is specified by either the
   "input" argument or the "name" argument, or both. */

SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input, SEXP name, SEXP call)
{
    const char *cinp, *ctarg;
    int mtch;
    SEXP y;

     /* The mechanism to allow  a class extending "environment" */
    if( IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP ){
        PROTECT(x);
        x = R_getS4DataSlot(x, ANYSXP);
        UNPROTECT(1);
	if(x == R_NilValue)
	    errorcall(call, "$ operator not defined for this S4 class");
    }

    /* If this is not a list object we return NULL. */
    /* Or should this be allocVector(VECSXP, 0)? */

    if (isPairList(x)) {
	SEXP xmatch = R_NilValue;
	int havematch;
        if (name!=R_NilValue) {
            /* Quick check for exact match by name */
            for (y = x; y != R_NilValue; y = CDR(y))
                if (TAG(y)==name) {
                    y = CAR(y);
                    if (NAMEDCNT(x) > NAMEDCNT(y)) SET_NAMEDCNT(y, NAMEDCNT(x));
    	            return y;
                }
        }
        cinp = input==R_NilValue ? CHAR(PRINTNAME(name)) : translateChar(input);
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
            ctarg = CHAR(PRINTNAME(TAG(y)));
	    mtch = ep_match_strings(ctarg, cinp);
	    if (mtch>0) /* exact */ {
		y = CAR(y);
		if (NAMEDCNT(x) > NAMEDCNT(y)) SET_NAMEDCNT(y, NAMEDCNT(x));
		return y;
            }
            else if (mtch<0) /* partial */ {
		havematch++;
		xmatch = y;
            }
	}
	if (havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
                ctarg = CHAR(PRINTNAME(TAG(xmatch)));
		warningcall(call, _("partial match of '%s' to '%s'"),
			    cinp, ctarg);
            }
	    y = CAR(xmatch);
	    if (NAMEDCNT(x) > NAMEDCNT(y)) SET_NAMEDCNT(y, NAMEDCNT(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (isVectorList(x)) {
	int i, n, havematch, imatch=-1;
        SEXP str_elt;
        SEXP nlist = getAttrib(x, R_NamesSymbol);
        cinp = input==R_NilValue ? CHAR(PRINTNAME(name)) : translateChar(input);
	n = length(nlist);
	havematch = 0;
	for (i = 0 ; i < n ; i = i + 1) {
            str_elt = STRING_ELT (nlist, i);
            ctarg = TYPEOF(str_elt)==CHARSXP ? translateChar(str_elt)/*always?*/
                                             : CHAR(PRINTNAME(str_elt));
	    mtch = ep_match_strings(ctarg, cinp);
            if (mtch>0) /* exact */ {
		y = VECTOR_ELT(x, i);
		if (NAMEDCNT(x) > NAMEDCNT(y))
		    SET_NAMEDCNT(y, NAMEDCNT(x));
		return y;
            }
	    else if (mtch<0) /* partial */ {
		havematch++;
		if (havematch == 1) {
		    /* partial matches can cause aliasing in eval.c:evalseq
		       This is overkill, but alternative ways to prevent
		       the aliasing appear to be even worse */
		    y = VECTOR_ELT(x,i);
		    SET_NAMEDCNT_MAX(y);
		    SET_VECTOR_ELT(x,i,y);
		}
		imatch = i;
	    }
	}
	if(havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
                str_elt = STRING_ELT (nlist, imatch);
                ctarg = TYPEOF(str_elt)==CHARSXP ? translateChar(str_elt)
                                                 : CHAR(PRINTNAME(str_elt));
		warningcall(call, _("partial match of '%s' to '%s'"),
			    cinp, ctarg);
	    }
	    y = VECTOR_ELT(x, imatch);
	    if (NAMEDCNT(x) > NAMEDCNT(y)) SET_NAMEDCNT(y, NAMEDCNT(x));
	    return y;
	}
	return R_NilValue;
    }
    else if( isEnvironment(x) ){
        if (name==R_NilValue) 
            name = install(translateChar(input));
	y = findVarInFrame (x, name);
	if( TYPEOF(y) == PROMSXP ) {
	    PROTECT(y);
	    y = eval(y, R_GlobalEnv);
	    UNPROTECT(1);
	}
        if (y == R_UnboundValue)
            return R_NilValue;
        SET_NAMEDCNT_MAX(y); /* Likely overkill but 2.13.0 does the equivalent*/
        return y;
    }
    else if( isVectorAtomic(x) ){
	errorcall(call, "$ operator is invalid for atomic vectors");
    }
    else /* e.g. a function */
	errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    return R_NilValue;
}
