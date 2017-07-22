/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2007   The R Core Team
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

#include "scalar-stack.h"

#include <helpers/helpers-app.h>

/* JMC convinced MM that this was not a good idea: */
#undef _S4_subsettable

/* Convert range to vector. The caller must ensure that its size won't 
   overflow. */

SEXP attribute_hidden Rf_VectorFromRange (int rng0, int rng1)
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
   and return R_NoObject, or convert the range to a vector of negative
   integer subscripts that is returned. */

SEXP attribute_hidden Rf_DecideVectorOrRange (SEXP rng, int *start, int *end, 
                                              SEXP call)
{
    int rng0, rng1;

    if (TYPEOF(rng)!=INTSXP || LENGTH(rng)!=2) /* shouldn't happen*/
        errorcall(call, "internal inconsistency subsetting with VARIANT_SEQ!");

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
        return R_NoObject;
    }
    else {
        return Rf_VectorFromRange(rng0,rng1);
    }
}


/* ExtractRange does the transfer of elements from "x" to "result" 
   according to the range given by start and end.  The caller will
   have allocated "result" to be at least the required length, and
   for VECSXP and EXPRSXP, the entries will be R_NilValue (done by
   allocVector).

   Arguments x and result must be protected by the caller. */

static void ExtractRange(SEXP x, SEXP result, int start, int end, SEXP call)
{
    int nx = length(x);

    SEXP tmp, tmp2;
    int n, m, i;

    start -= 1;
    n = end-start;
    m = start>=nx ? 0 : end<=nx ? n : nx-start;

    tmp = result;

    switch (TYPEOF(x)) {
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
        if (!DUPVE || NAMEDCNT_EQ_0(x)) {
            copy_vector_elements (result, 0, x, start, m);
            if (NAMEDCNT_GT_0(x))
                for (i = 0; i<m; i++)
                    INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,i));
        }
        else {
            for (i = 0; i<m; i++)
                SET_VECTOR_ELT (result, i, duplicate(VECTOR_ELT(x,start+i)));
        }
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
        nonsubsettable_error(call,x);
    }
}


/* ExtractSubset does the transfer of elements from "x" to "result" 
   according to the integer subscripts given in "indx". The caller will
   have allocated "result" to be at least the required length, and
   for VECSXP and EXPRSXP, the entries will be R_NilValue (done by
   allocVector).

   Arguments x and result must be protected by the caller. */

static void ExtractSubset(SEXP x, SEXP result, SEXP indx, SEXP call)
{
    int n = LENGTH(indx);
    int nx = LENGTH(x);
    int i, ii;

    switch (TYPEOF(x)) {
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
        if (NAMEDCNT_EQ_0(x)) {
            for (i = 0; i<n; i++)
                if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                    /* nothing, already R_NilValue */ ;
                else {
                    SEXP ve = VECTOR_ELT(x, ii);
                    SET_VECTOR_ELT(result, i, ve);
                    if (i > 0) INC_NAMEDCNT_0_AS_1(ve);
                }
        }
        else {
            for (i = 0; i<n; i++)
                if ((ii=INTEGER(indx)[i]) == NA_INTEGER || --ii < 0 || ii >= nx)
                    /* nothing, already R_NilValue */ ;
                else 
                    SET_VECTOR_ELEMENT_FROM_VECTOR(result, i, x, ii);
        }
        break;
    case LISTSXP:
	    /* cannot happen: pairlists are coerced to lists */
    case LANGSXP: ;
        SEXP tmp, tmp2;
        tmp = result;
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
        nonsubsettable_error(call,x);
    }
}


/* Check whether subscript is such that we suppress dropping dimensions 
   when the drop argument is NA (the default).  Assumes that it's not a
   missing argument (which needs to be checked separately). */

static inline int whether_suppress_drop (SEXP sb)
{
    SEXP d;
    return TYPEOF(sb) != LGLSXP 
             && HAS_ATTRIB(sb)
             && (d = getAttrib(sb,R_DimSymbol)) != R_NilValue
             && LENGTH(d) == 1;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP subs, int seq, int drop, SEXP call)
{
    SEXP sb = subs == R_NilValue ? R_MissingArg : CAR(subs);
    SEXP indx = R_NilValue;
    SEXP result, dims, attrib, nattrib;
    int start = 1, end = 0, n = 0;
    int suppress_drop = 0;
    int spi, ndim;

    /* R_NilValue is handled specially, always returning R_NilValue. */

    if (x == R_NilValue)
        return R_NilValue;

    /* SPECIAL KLUDGE:  To avoid breaking inexplicable code in some 
       packages, just return a duplicate of x if the subscripting has 
       the form x[,drop=FALSE]. */

    if (sb == R_MissingArg && drop == FALSE)
        return duplicate(x);

    PROTECT_WITH_INDEX (sb, &spi);
    dims = getAttrib(x, R_DimSymbol);
    ndim = length(dims);

    /* Check for variant result, which will be a range rather than a vector, 
       and if we have a range, see whether it can be used directly, or must
       be converted to a vector to be handled as other vectors. */

    if (seq) {
        suppress_drop = LEVELS(sb);  /* get flag for seq having a dim attr */
        REPROTECT(sb = Rf_DecideVectorOrRange(sb,&start,&end,call), spi);
        if (sb == R_NoObject)
            n = end - start + 1;
    }

    /* If subscript is missing, convert to range over entire vector. 
       Also, suppress dropping if it was missing from '_'. */

    if (sb == R_MissingArg) {
        suppress_drop = subs != R_NilValue && MISSING(subs) == 2;
        start = 1;
        end = length(x);
        n = end;
        sb = R_NoObject;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    if (sb != R_NoObject && isMatrix(sb) && isArray(x) && ncols(sb) == ndim) {
        if (isString(sb)) {
            sb = strmat2intmat(sb, GetArrayDimnames(x), call);
            REPROTECT(sb,spi);
        }
        if (isInteger(sb) || isReal(sb)) {
            sb = mat2indsub(dims, sb, call);
            REPROTECT(sb,spi);
        }
    }

    /* Convert sb to a vector of integer subscripts (unless we have a range).
       We suppress dropping when drop is NA when the index is not logical
       and is a 1D array. */

    if (sb != R_NoObject) {
        int stretch = 1;  /* allow out of bounds, not for assignment */
        SEXP d;
        if (drop == NA_LOGICAL) suppress_drop = whether_suppress_drop(sb);
        PROTECT(indx = makeSubscript(x, sb, &stretch, call, 0));
        n = length(indx);
    }

    /* Allocate and extract the result. */

    PROTECT (result = allocVector(TYPEOF(x),n));
    if (sb==R_NoObject)
        ExtractRange(x, result, start, end, call);
    else 
        ExtractSubset(x, result, indx, call);

    if (((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
        ( /* here we might have an array.  Use row names if 1D */
            ndim == 1 &&
            (attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
            (attrib = GetRowNames(attrib)) != R_NilValue
            )
        ) {
        PROTECT(attrib);
        PROTECT(nattrib = allocVector(TYPEOF(attrib), n));
        if (sb==R_NoObject)
            ExtractRange(attrib, nattrib, start, end, call);
        else
            ExtractSubset(attrib, nattrib, indx, call);
        setAttrib(result, R_NamesSymbol, nattrib);
        UNPROTECT(2);
    }
    if ((attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
        TYPEOF(attrib) == VECSXP) {
        PROTECT(attrib);
        PROTECT(nattrib = allocVector(VECSXP, n));
        if (sb==R_NoObject)
            ExtractRange(attrib, nattrib, start, end, call);
        else
            ExtractSubset(attrib, nattrib, indx, call);
        setAttrib(result, R_SrcrefSymbol, nattrib);
           UNPROTECT(2);
    }

    /* FIXME: this is wrong, because the slots are gone, so result is
       an invalid object of the S4 class! JMC 3/3/09 */
#ifdef _S4_subsettable
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
        setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
        SET_S4_OBJECT(result);
    }
#endif

    UNPROTECT(2 + (sb!=R_NoObject));

  done:
    /* One-dimensional arrays should have their dimensions dropped only 
       if the result has length one and drop TRUE or is NA_INTEGER without
       the drop being suppressed by the index being a 1D array. */

    if (ndim == 1) {
        int len = length(result);

        if (len > 1 || drop == FALSE || drop == NA_INTEGER && suppress_drop) {
            SEXP attr, nm;
            PROTECT(result);
            PROTECT(attr = allocVector1INT());
            INTEGER(attr)[0] = len;
            if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
                /* reinstate dimnames, include names of dimnames, which
                   should be in the names attribute at this point. */
                PROTECT(attrib = dup_top_level(attrib));
                SET_VECTOR_ELT(attrib, 0,
                               getAttrib(result, R_NamesSymbol));
                setAttrib(result, R_DimSymbol, attr);
                setAttrib(result, R_DimNamesSymbol, attrib);
                setAttrib(result, R_NamesSymbol, R_NilValue);
                UNPROTECT(1);
            }
            else 
                setAttrib(result, R_DimSymbol, attr);
            UNPROTECT(2);
        }
    }

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
            out_of_bounds_error(call);

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
        case VECSXP: ;
            SET_VECTOR_ELEMENT_FROM_VECTOR(result, j, x, st+jj*nr);
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
        out_of_bounds_error(call);

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
            out_of_bounds_error(call);

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
            if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                copy_vector_elements (result, ij, x, jjnr, nrs);
                if (NAMEDCNT_GT_0(x))
                    for (i = 0; i<nrs; i++)
                        INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,ij+i));
            }
            else {
                for (i = 0; i<nrs; i++)
                    SET_VECTOR_ELT (result, ij+i, 
                                    duplicate(VECTOR_ELT(x,jjnr+i)));
            }
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
    int i, j, ii, jj, ij, jjnr;

    /* Set rows of result to NAs where there are NA row indexes.  Also check 
       for bad row indexes (once here rather than many times in loop). */

    for (i = 0; i < nrs; i++) {
        ii = INTEGER(sr)[i];
        if (ii == NA_INTEGER) 
            set_row_or_col_to_na (result, i, nrs, i+nrs*ncs, call);
        else if (ii < 1 || ii > nr)
            out_of_bounds_error(call);
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
            out_of_bounds_error(call);

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
            if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                for (i = 0; i < nrs; i++, ij++) 
                    if ((ii = INTEGER(sr)[i]) != NA_INTEGER) {
                        SEXP ve = VECTOR_ELT(x, (ii-1)+jjnr);
                        SET_VECTOR_ELT (result, ij, ve);
                        INC_NAMEDCNT_0_AS_1(ve);
                    }
            }
            else {
                for (i = 0; i < nrs; i++, ij++) 
                    if ((ii = INTEGER(sr)[i]) != NA_INTEGER) 
                        SET_VECTOR_ELT (result, ij, 
                          duplicate(VECTOR_ELT(x,(ii-1)+jjnr)));
            }
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

static SEXP MatrixSubset(SEXP x, SEXP subs, SEXP call, int drop, int seq)
{
    SEXP s0 = CAR(subs), s1 = CADR(subs);
    SEXP dims, result, sr, sc;
    int start = 1, end = 0;
    int nr, nc, nrs = 0, ncs = 0;
    int suppress_drop_row = 0, suppress_drop_col = 0;
    int nprotect = 0;
    int ii;

    SEXP dim = getAttrib(x, R_DimSymbol);

    nr = INTEGER(dim)[0];
    nc = INTEGER(dim)[1];

    /* s0 is set to R_NoObject when we have a range for the first subscript */

    if (s0 == R_MissingArg) {
        suppress_drop_row = MISSING(subs)==2;
        start = 1;
        end = nr;
        nrs = nr;
        s0 = R_NoObject;
    }
    else if (seq) {
        suppress_drop_row = LEVELS(s0);
        PROTECT(s0 = Rf_DecideVectorOrRange(s0,&start,&end,call));
        nprotect++;
        if (s0 == R_NoObject)
            nrs = end - start + 1;
    }

    if (s0 != R_NoObject) {
        if (drop == NA_LOGICAL) 
            suppress_drop_row = whether_suppress_drop(s0);
        PROTECT (sr = arraySubscript(0, s0, dim, getAttrib, (STRING_ELT), x));
        nprotect++;
        nrs = LENGTH(sr);
    }

    if (drop == NA_LOGICAL) 
        suppress_drop_col = s1 == R_MissingArg ? MISSING(CDR(subs)) == 2 
                                               : whether_suppress_drop(s1);

    PROTECT (sc = arraySubscript(1, s1, dim, getAttrib, (STRING_ELT), x));
    nprotect++;
    ncs = LENGTH(sc);

    if (nrs < 0 || ncs < 0)
        abort();  /* shouldn't happen, but code was conditional before... */

    /* Check this does not overflow */
    if ((double)nrs * (double)ncs > R_LEN_T_MAX)
        error(_("dimensions would exceed maximum size of array"));

    PROTECT (result = allocVector(TYPEOF(x), nrs*ncs));
    nprotect++;

    /* Extract elements from matrix x to result. */

    if (s0 == R_NoObject)
        range_of_rows_of_matrix(call, x, result, start, nrs, nr, sc, ncs, nc);
    else if (nrs == 1 && (ii = INTEGER(sr)[0]) != NA_INTEGER 
                      && ii >= 0 && ii <= nr)
        one_row_of_matrix (call, x, result, ii, nr, sc, ncs, nc);
    else
        multiple_rows_of_matrix (call, x, result, sr, nrs, nr, sc, ncs, nc);

    /* Set up dimensions attribute. */

    PROTECT(dims = allocVector(INTSXP, 2));
    nprotect++;
    INTEGER(dims)[0] = nrs;
    INTEGER(dims)[1] = ncs;
    setAttrib(result, R_DimSymbol, dims);

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.	 Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    SEXP dimnames, dimnamesnames, newdimnames;
    dimnames = getAttrib(x, R_DimNamesSymbol);
    PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
    nprotect++;

    if (!isNull(dimnames)) {
        PROTECT(newdimnames = allocVector(VECSXP, 2));
        nprotect++;
        if (TYPEOF(dimnames) == VECSXP) {
            if (VECTOR_ELT(dimnames,0) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 0, allocVector(STRSXP, nrs));
                if (s0 == R_NoObject)
                    ExtractRange (VECTOR_ELT(dimnames,0),
                      VECTOR_ELT(newdimnames,0), start, end, call);
                else
                    ExtractSubset(VECTOR_ELT(dimnames,0),
                      VECTOR_ELT(newdimnames,0), sr, call);
            }
            if (VECTOR_ELT(dimnames,1) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 1, allocVector(STRSXP, ncs));
                ExtractSubset(VECTOR_ELT(dimnames,1),
                  VECTOR_ELT(newdimnames,1), sc, call);
            }
        }
        else {
            if (CAR(dimnames) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 0, allocVector(STRSXP, nrs));
                if (s0 == R_NoObject)
                    ExtractRange (CAR(dimnames),
                      VECTOR_ELT(newdimnames,0), start, end, call);
                else
                    ExtractSubset(CAR(dimnames),
                      VECTOR_ELT(newdimnames,0), sr, call);
            }
            if (CADR(dimnames) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 1, allocVector(STRSXP, ncs));
                ExtractSubset(CADR(dimnames),
                  VECTOR_ELT(newdimnames,1), sc, call);
            }
        }
        setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
        setAttrib(result, R_DimNamesSymbol, newdimnames);
    }

    /* Possibly drop the dimensions.  For compatibility, we always drop
       either neither or both, though this doesn't really make sense. */

    if (nrs == 1 || ncs == 1) {
        if (drop == TRUE || drop == NA_LOGICAL 
                             && (nrs == 1 && !suppress_drop_row
                                  || ncs == 1 && !suppress_drop_col)) {
            DropDims(result);
        }
    }

    UNPROTECT(nprotect);
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop, SEXP xdims, int k)
{
    int i, j, ii, jj, n;
    SEXP dimnames, dimnamesnames, r, result;
    int mode = TYPEOF(x);

    int *subs[k], indx[k], nsubs[k], offset[k], suppress_drop[k];
    SEXP subv[k];

    n = 1; r = s;
    for (i = 0; i < k; i++) {
        if (drop==NA_LOGICAL) 
            suppress_drop[i] = CAR(r) == R_MissingArg ? MISSING(r) == 2
                                : whether_suppress_drop(CAR(r));
        PROTECT (subv[i] = arraySubscript (i, CAR(r), xdims, getAttrib,
                                           (STRING_ELT), x));
        subs[i] = INTEGER(subv[i]);
	nsubs[i] = LENGTH(subv[i]);
        n *= nsubs[i];
        indx[i] = 0;
	r = CDR(r);
    }

    offset[1] = INTEGER(xdims)[0];  /* offset[0] is not used */
    for (i = 2; i < k; i++)
        offset[i] = offset[i-1] * INTEGER(xdims)[i-1];

    /* Check for out-of-bounds indexes.  Disabled, since it seems unnecessary,
       given that arraySubscript checks bounds. */

#if 0
    for (j = 0; j < k; j++) {
        for (i = 0; i < nsubs[j]; i++) {
            jj = subs[j][i];
            if (jj != NA_INTEGER && (jj < 1 || jj > INTEGER(xdims)[j])) {
                out_of_bounds_error(call);
            }
        }
    }
#endif

    /* Vector to contain the returned values. */

    PROTECT(result = allocVector(mode, n));

    /* Transfer the subset elements from "x" to "a". */

    if (n > 0) for (i = 0; ; i++) {

        jj = subs[0][indx[0]];
        if (jj == NA_INTEGER) goto assign;
	ii = jj-1;
	for (j = 1; j < k; j++) {
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) goto assign;
	    ii += (jj-1) * offset[j];
	}

      assign:
        if (jj != NA_INTEGER) {
            switch (mode) {
            case LGLSXP:
                LOGICAL(result)[i] = LOGICAL(x)[ii];
                break;
            case INTSXP:
                INTEGER(result)[i] = INTEGER(x)[ii];
                break;
            case REALSXP:
                REAL(result)[i] = REAL(x)[ii];
                break;
            case CPLXSXP:
                COMPLEX(result)[i] = COMPLEX(x)[ii];
                break;
            case STRSXP:
                SET_STRING_ELT(result, i, STRING_ELT(x, ii));
                break;
            case VECSXP:
                if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                    SET_VECTOR_ELT (result, i, VECTOR_ELT(x, ii));
                    INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,i));
                }
                else
                    SET_VECTOR_ELT (result, i, duplicate(VECTOR_ELT(x,ii)));
                break;
            case RAWSXP:
                RAW(result)[i] = RAW(x)[ii];
                break;
            default:
                errorcall(call, _("array subscripting not handled for this type"));
                break;
            }
        }
        else { /* jj == NA_INTEGER */
            switch (mode) {
            case LGLSXP:
                LOGICAL(result)[i] = NA_LOGICAL;
                break;
            case INTSXP:
                INTEGER(result)[i] = NA_INTEGER;
                break;
            case REALSXP:
                REAL(result)[i] = NA_REAL;
                break;
            case CPLXSXP:
                COMPLEX(result)[i].r = NA_REAL;
                COMPLEX(result)[i].i = NA_REAL;
                break;
            case STRSXP:
                SET_STRING_ELT(result, i, NA_STRING);
                break;
            case VECSXP:
                SET_VECTOR_ELT(result, i, R_NilValue);
                break;
            case RAWSXP:
                RAW(result)[i] = (Rbyte) 0;
                break;
            default:
                errorcall(call, _("array subscripting not handled for this type"));
                break;
            }
        }

        j = 0;
        while (++indx[j] >= nsubs[j]) {
            indx[j] = 0;
            if (++j >= k) goto done;
        }
    }

  done:
    PROTECT(xdims = allocVector(INTSXP, k));
    for(i = 0 ; i < k ; i++)
	INTEGER(xdims)[i] = nsubs[i];
    setAttrib(result, R_DimSymbol, xdims);
    UNPROTECT(1); /* xdims */

    /* The array elements have been transferred. */
    /* Now we need to transfer the attributes. */
    /* Most importantly, we need to subset the */
    /* dimnames of the returned value. */

    dimnames = getAttrib(x, R_DimNamesSymbol);
    PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
    if (TYPEOF(dimnames) == VECSXP) { /* broken code for others in R-2.15.0 */
	SEXP new_xdims;
	PROTECT(new_xdims = allocVector(VECSXP, k));
        for (i = 0; i < k ; i++) {
            if (nsubs[i] > 0 && VECTOR_ELT(dimnames,i) != R_NilValue) {
                SET_VECTOR_ELT(new_xdims, i, allocVector(STRSXP, nsubs[i]));
                ExtractSubset (VECTOR_ELT(dimnames, i), VECTOR_ELT(new_xdims,i),
                               subv[i], call);
            } 
            /* else leave as NULL for 0-length dims */
        }
	setAttrib(new_xdims, R_NamesSymbol, dimnamesnames);
	setAttrib(result, R_DimNamesSymbol, new_xdims);
	UNPROTECT(1);
    }
    UNPROTECT(1);

    /* See if we need to drop any dimensions */

    if (drop == TRUE)
	DropDims(result);
    else if (drop == NA_LOGICAL)
        DropDimsNotSuppressed(result,suppress_drop);

    UNPROTECT(k+1);
    return result;
}


/* Returns and removes a named argument from the argument list 
   pointed to by args_ptr, and updates args_ptr to account for
   the removal (when it was at the head).

   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NoObject is is returned.

   The caller does not need to protect *args_ptr before.
 */
static SEXP ExtractArg(SEXP *args_ptr, SEXP arg_sym)
{
    SEXP prev_arg = R_NoObject;
    SEXP arg;

    for (arg = *args_ptr; arg != R_NilValue; arg = CDR(arg)) {
	if(TAG(arg) == arg_sym) {
	    if (prev_arg == R_NoObject) /* found at head of args */
		*args_ptr = CDR(arg);
	    else
		SETCDR(prev_arg, CDR(arg));
	    return CAR(arg);
	}
	prev_arg = arg;
    }
    return R_NoObject;
}

/* Extracts the drop argument, if present, from the argument list.
   The argument list will be modified, and the pointer passed changed
   if the first argument is deleted.  The caller does not need to
   protect *args_ptr before.  The value is FALSE, TRUE, or NA_LOGICAL,
   with NA_LOGICAL being the default when no drop argument is present.
   When used as a C boolean, NA_LOGICAL will have the same effect as
   TRUE, but NA_LOGICAL will sometimes allow dropping to be suppressed
   when a vector index has a 1D dim attribute. */

static int ExtractDropArg(SEXP *args_ptr)
{
    SEXP drop_arg = ExtractArg(args_ptr, R_DropSymbol);
    return drop_arg != R_NoObject ? asLogical(drop_arg) : NA_LOGICAL;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used

   The argument list pointed to by args_ptr may be modified.  The
   caller does not need to protect *args_ptr before.
 */
static int ExtractExactArg(SEXP *args_ptr)
{
    SEXP argval = ExtractArg(args_ptr, R_ExactSymbol);
    if (argval == R_NoObject) return 1; /* Default is true as from R 2.7.0 */
    int exact = asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}


/* Returns simple (positive or negative) index, with no dim attribute, or 
   zero if not so simple. */

static R_INLINE R_len_t simple_index (SEXP s)
{
    if (HAS_ATTRIB(s) && getAttrib(s,R_DimSymbol) != R_NilValue)
        return 0;

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
   NA, or out of bounds), with no dim attribute.  Returns the result, or 
   R_NilValue if it's not so simple.  Return result may be on scalar stack,
   if variant allows.

   The arguments x and s do not need to be protected before this
   function is called.  It's OK for x to still be being computed. The
   variant for the return result is the last argument. */

static inline SEXP one_vector_subscript (SEXP x, SEXP s, int variant)
{
    R_len_t ix, n;
    int typeofx;
    SEXP r;

    typeofx = TYPEOF(x);

    if (!isVectorAtomic(x))
        return R_NilValue;

    n = LENGTH(x);
    ix = simple_index (s);

    if (ix == 0 || ix > n || ix < -n)
        return R_NilValue;

    if (ix>0) {
        R_len_t avail;
        ix -= 1;
        if (helpers_is_being_computed(x)) {
            helpers_start_computing_var(x);
            HELPERS_WAIT_IN_VAR (x, avail, ix, n);
        }
        switch (typeofx) {
        case LGLSXP:  
            return ScalarLogicalMaybeConst (LOGICAL(x)[ix]);
        case INTSXP:  
            if (CAN_USE_SCALAR_STACK(variant))
                return PUSH_SCALAR_INTEGER(INTEGER(x)[ix]);
            else
                return ScalarIntegerMaybeConst(INTEGER(x)[ix]);
        case REALSXP: 
            if (CAN_USE_SCALAR_STACK(variant))
                return PUSH_SCALAR_REAL(REAL(x)[ix]);
            else
                return ScalarRealMaybeConst(REAL(x)[ix]);
        case RAWSXP:  
            return ScalarRawMaybeConst (RAW(x)[ix]);
        case STRSXP:  
            return ScalarStringMaybeConst (STRING_ELT(x,ix));
        case CPLXSXP: 
            return ScalarComplexMaybeConst (COMPLEX(x)[ix]);
        default: abort();
        }
    }
    else { /* ix < 0 */

        R_len_t ex;

        WAIT_UNTIL_COMPUTED(x);
        PROTECT(x);
        r = allocVector (typeofx, n-1);

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
        default: abort();
        }

        UNPROTECT(1);
        return r;
    }
}


/* Look for the simple case of subscripting an atomic matrix with two
   valid integer or real subscript that are positive (not negative, zero, 
   NA, or out of bounds), with no dim attribute.  Returns the result, or 
   R_NilValue if it's not so simple.  The arguments x, dim, s1, and s2 do 
   not need to be protected before this function is called. It's OK for x to 
   still be being computed. The variant for the return result is the last 
   argument. */

static inline SEXP two_matrix_subscripts (SEXP x, SEXP dim, SEXP s1, SEXP s2, 
                                          int variant)
{
    R_len_t ix1, ix2, nrow, ncol, avail, e;
    SEXP r;

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

    if (helpers_is_being_computed(x)) {
        helpers_start_computing_var(x);
        HELPERS_WAIT_IN_VAR (x, avail, e, LENGTH(x));
    }

    switch (TYPEOF(x)) {
    case LGLSXP:  
        return ScalarLogicalMaybeConst (LOGICAL(x)[e]);
    case INTSXP:  
        if (CAN_USE_SCALAR_STACK(variant))
            return PUSH_SCALAR_INTEGER(INTEGER(x)[e]);
        else
            return ScalarIntegerMaybeConst(INTEGER(x)[e]);
    case REALSXP: 
        if (CAN_USE_SCALAR_STACK(variant))
            return PUSH_SCALAR_REAL(REAL(x)[e]);
        else
            return ScalarRealMaybeConst(REAL(x)[e]);
    case RAWSXP:  
        return ScalarRawMaybeConst (RAW(x)[e]);
    case STRSXP:  
        return ScalarStringMaybeConst (STRING_ELT(x,e));
    case CPLXSXP: 
        return ScalarComplexMaybeConst (COMPLEX(x)[e]);
    default: abort();
    }
}


/* The "[" subset operator.
 * This provides the most general form of subsetting. */

static SEXP do_subset_dflt_seq (SEXP call, SEXP op, SEXP x, SEXP sb1, SEXP sb2,
                                SEXP subs, SEXP rho, int variant, int seq);

static SEXP do_subset(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP ans;
    int argsevald = 0;

    /* If we can easily determine that this will be handled by
       subset_dflt and has one or two index arguments in total, try to
       evaluate the first index with VARIANT_SEQ, so it may come as a
       range rather than a vector.  Also, evaluate the array with
       VARIANT_PENDING_OK, and perhaps evaluate indexes with
       VARIANT_SCALAR_STACK (should be safe, since there will be no
       later call of eval). */

    if (args != R_NilValue && CAR(args) != R_DotsSymbol) {
        SEXP array = CAR(args);
        SEXP ixlist = CDR(args);
        PROTECT(array = evalv(array,rho,VARIANT_PENDING_OK));
        if (isObject(array)) {
            args = CONS(array,ixlist);
            argsevald = -1;
            UNPROTECT(1);  /* array */
        }
        else if (ixlist == R_NilValue || TAG(ixlist) != R_NilValue 
                                      || CAR(ixlist) == R_DotsSymbol) {
            args = evalListKeepMissing(ixlist,rho);
            UNPROTECT(1);  /* array */
            return do_subset_dflt_seq (call, op, array, R_NoObject, R_NoObject,
                                       args, rho, variant, 0);
        }
        else {
            SEXP sv_scalar_stack = R_scalar_stack;
            SEXP ixlist2 = CDR(ixlist);
            SEXP sb1;
            int seq = 0;
            PROTECT (sb1 = evalv (CAR(ixlist), rho, 
                     CDR(ixlist2) == R_NilValue /* no more than two arguments */
                       ? VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                         VARIANT_MISSING_OK | VARIANT_PENDING_OK
                       : VARIANT_SCALAR_STACK_OK | 
                         VARIANT_MISSING_OK | VARIANT_PENDING_OK));
            if (R_variant_result) {
                seq = 1;
                R_variant_result = 0;
            }
            SEXP remargs = ixlist2;
            SEXP sb2 = R_NoObject;
            if (sb1 == R_MissingArg && isSymbol(CAR(ixlist))) {
                UNPROTECT(1);
                PROTECT(remargs = CONS(sb1,remargs));
                SET_MISSING (remargs, R_isMissing(CAR(ixlist),rho));
                sb1 = R_NoObject;
            }
            else if (ixlist2 != R_NilValue && TAG(ixlist2) == R_NilValue 
                                      && CAR(ixlist2) != R_DotsSymbol) {
                PROTECT (sb2 = evalv (CAR(ixlist2), rho, 
                         CDR(ixlist2)==R_NilValue /*no more than two arguments*/
                           ? VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                             VARIANT_MISSING_OK
                           : VARIANT_SCALAR_STACK_OK | 
                             VARIANT_MISSING_OK));
                remargs = CDR(ixlist2);
            }
            if (remargs != R_NilValue)
                remargs = evalList_v (remargs, rho, VARIANT_SCALAR_STACK_OK |
                                      VARIANT_PENDING_OK | VARIANT_MISSING_OK);
            PROTECT(remargs);
            if (sb2 == R_MissingArg && isSymbol(CAR(ixlist2))) {
                UNPROTECT(2);  /* remargs, sb2 */
                PROTECT(remargs = CONS(sb2,remargs));
                SET_MISSING (remargs, R_isMissing(CAR(ixlist2),rho));
                sb2 = R_NoObject;
            }
            if (sb1 != R_NoObject)
                WAIT_UNTIL_COMPUTED(sb1);
            UNPROTECT (3 + (sb2 != R_NoObject));  /* remargs, sb2, sb1, array */
            wait_until_arguments_computed(remargs);
            R_scalar_stack = sv_scalar_stack;
            SEXP r = do_subset_dflt_seq (call, op, array, sb1, sb2,
                                         remargs, rho, variant, seq);
            return r;
        }
    }

    /* If the first argument is an object and there is an */
    /* appropriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall through */
    /* to the generic code below.  Note that evaluation */
    /* retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "[", args, rho, &ans, 0, argsevald)) {
/*     if(DispatchAnyOrEval(call, op, "[", args, rho, &ans, 0, 0)) */
	if (NAMEDCNT_GT_0(ans))
	    SET_NAMEDCNT_MAX(ans);    /* IS THIS NECESSARY? */
	return(ans);
    }

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    SEXP x = CAR(ans);
    args = CDR(ans);

    if (args == R_NilValue || TAG(args) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, R_NoObject, R_NoObject, 
                                   args, rho, variant, 0);
    else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, CAR(args), R_NoObject,
                                   CDR(args), rho, variant, 0);
    else
        return do_subset_dflt_seq (call, op, x, CAR(args), CADR(args),
                                   CDDR(args), rho, variant, 0);
}


/* N.B.  do_subset_dflt is sometimes called directly from outside this module. 
   It doesn't have the "seq" argument of do_subset_dflt_seq, and takes all
   arguments as an arg list. */

SEXP attribute_hidden do_subset_dflt (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args);
    args = CDR(args);
    
    if (args == R_NilValue || TAG(args) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, R_NoObject, R_NoObject, 
                                   args, rho, 0, 0);
    else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, CAR(args), R_NoObject,
                                   CDR(args), rho, 0, 0);
    else
        return do_subset_dflt_seq (call, op, x, CAR(args), CADR(args),
                                   CDDR(args), rho, 0, 0);
}

/* The "seq" argument below is 1 if the first subscript is a sequence spec
   (a variant result).  The first argument (the array, x) is passed separately
   rather than as part of an argument list, for efficiency.  If sb1 is not
   R_NoObject, it is the first subscript, which has no tag, and subs is the
   list of remaining subscripts/args; otherwise subs has all subscripts.

   Note:  x, sb1, and subs may not be protected on entry. */

static SEXP do_subset_dflt_seq (SEXP call, SEXP op, SEXP x, SEXP sb1, SEXP sb2,
                                SEXP subs, SEXP rho, int variant, int seq)
{
    int drop, i, nsubs, type;
    SEXP ans, ax, px;

    if (!seq && x != R_NilValue && sb1 != R_NoObject) {

        /* Check for one subscript, handling simple cases like this */
        if (sb2 == R_NoObject && subs == R_NilValue) { 
            SEXP attr = ATTRIB(x);
            if (attr != R_NilValue) {
                if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
                    SEXP dim = CAR(attr);
                    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 1)
                        attr = R_NilValue;  /* only a dim attribute, 1D */
                }
            }
            if (attr == R_NilValue) {
                SEXP r = one_vector_subscript (x, sb1, variant);
                if (r != R_NilValue)
                    return r;
            }
        }

        /* Check for two subscripts, handling simple cases like this */
        else if (sb2 != R_NoObject && subs == R_NilValue) {
            SEXP attr = ATTRIB(x);
            if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
                SEXP dim = CAR(attr);
                if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {
                    /* x is a matrix */
                    SEXP r = two_matrix_subscripts (x, dim, sb1, sb2, variant);
                    if (r != R_NilValue)
                        return r;
                }
            }
        }
    }

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */

    if (x == R_NilValue)
	return x;

    PROTECT(x);

    drop = ExtractDropArg(&subs);
    if (sb2 != R_NoObject) 
        subs = CONS (sb2, subs);
    if (sb1 != R_NoObject) 
        subs = CONS (sb1, subs);
    PROTECT(subs);

    WAIT_UNTIL_COMPUTED(x);

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
        SET_NAMEDCNT(ax,NAMEDCNT(x));
	for(px = x, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else
        nonsubsettable_error(call,x);

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    if(nsubs < 2) {
	PROTECT(ans = VectorSubset(ax, subs, seq, drop, call));
    } else {
        SEXP xdims = getAttrib(x, R_DimSymbol);
	if (nsubs != length(xdims))
	    errorcall(call, _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, subs, call, drop, seq);
	else
	    ans = ArraySubset(ax, subs, call, drop, xdims, nsubs);
	PROTECT(ans);
    }

    /* Note: we do not coerce back to pair-based lists. */

    if (type == LANGSXP) {
	ax = ans;
	PROTECT(ans = allocList(LENGTH(ax)));
	if (ans != R_NilValue) {
	    SET_TYPEOF(ans, LANGSXP);
            for (px = ans, i = 0 ; px != R_NilValue ; px = CDR(px))
                SETCAR(px, VECTOR_ELT(ax, i++));
            setAttrib(ans, R_DimSymbol, getAttrib(ax, R_DimSymbol));
            setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
            setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
            SET_NAMEDCNT_MAX(ans);
        }
        UNPROTECT(2);
        PROTECT(ans);
    }

    if (HAS_ATTRIB(ans)) { /* remove probably erroneous attr's */
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

static SEXP do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
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
	    SET_NAMEDCNT_MAX(ans);    /* IS THIS NECESSARY? */
	return(ans);
    }

    /* Method dispatch has failed. */
    /* We now run the generic internal code. */

    return do_subset2_dflt(call, op, ans, rho, variant);
}

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho,
                                      int variant)
{
    SEXP ans, dims, dimnames, indx, subs, x;
    int i, ndims, nsubs, offset = 0;
    int pok, exact = -1;

    (void) ExtractDropArg(&args);  /* a "drop" arg is tolerated, but ignored */
    exact = ExtractExactArg(&args);
    PROTECT(args);

    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    if (exact == -1)
	pok = exact;
    else
	pok = !exact;

    PROTECT(x = CAR(args));

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */

    if (x == R_NilValue) {
	UNPROTECT(2);
	return x;
    }

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    subs = CDR(args);
    nsubs = length(subs);
    if (nsubs == 0)
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
        UNPROTECT(1);
        PROTECT(x);
    }

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
        if (nsubs != 1 || !isString(CAR(subs)) || length(CAR(subs)) != 1)
            errorcall(call, _("wrong arguments for subsetting an environment"));
        SEXP sym = installed_already (translateChar (STRING_ELT(CAR(subs),0)));
        if (sym == R_NoObject)
            ans = R_NilValue;
        else {
            ans = findVarInFrame (x, sym);
            if (ans == R_UnboundValue)
                ans = R_NilValue;
            else {
                if (TYPEOF(ans) == PROMSXP)
                    ans = forcePromise(ans);
                SET_NAMEDCNT_NOT_0(ans);
            }
        }
        UNPROTECT(2);
        return(ans);
    }

    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	nonsubsettable_error(call,x);

    int max_named = NAMEDCNT(x);

    if(nsubs == 1) { /* vector indexing */

	SEXP thesub = CAR(subs);
        int str_sym_sub = isString(thesub) || isSymbol(thesub);
	int len = length(thesub);
        int i, lenx;

        for (i = 1; i < len; i++) {
            if (!isVectorList(x) && !isPairList(x))
                errorcall(call,_("recursive indexing failed at level %d\n"),i);
            lenx = length(x);
            offset = get1index(thesub, 
                       str_sym_sub ? getAttrib(x, R_NamesSymbol) : R_NilValue,
                       lenx, pok, i-1, call);
            if (offset < 0 || offset >= lenx)
                errorcall(call, _("no such index at level %d\n"), i);
            if (isPairList(x)) {
                x = CAR(nthcdr(x, offset));
                max_named = MAX_NAMEDCNT;
            } 
            else {
                x = VECTOR_ELT(x, offset);
                int nm = NAMEDCNT(x);
                if (nm > max_named) 
                    max_named = nm;
            }
        }

        lenx = length(x);
	offset = get1index(thesub, 
                   str_sym_sub ? getAttrib(x, R_NamesSymbol) : R_NilValue,
                   lenx, pok, len > 1 ? len-1 : -1, call);
	if (offset < 0 || offset >= lenx) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(2);
		return R_NilValue;
	    }
	    else out_of_bounds_error(call);
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
		get1index(CAR(subs),
                          (i < ndn) ? VECTOR_ELT(dimnames, i) : R_NilValue,
			  INTEGER(indx)[i], pok, -1, call);
	    subs = CDR(subs);
	    if (INTEGER(indx)[i] < 0 ||
		INTEGER(indx)[i] >= INTEGER(dims)[i])
		out_of_bounds_error(call);
	}
	offset = 0;
	for (i = (nsubs - 1); i > 0; i--)
	    offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	offset += INTEGER(indx)[0];
	UNPROTECT(1);
    }

    if (isPairList(x)) {
	ans = CAR(nthcdr(x, offset));
        SET_NAMEDCNT_MAX(ans);
    } else if (isVectorList(x)) {
	ans = VECTOR_ELT(x, offset);
	if (max_named > 0)
            SET_NAMEDCNT_NOT_0(ans);
        if (VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
             && max_named <= 1 && !NAMEDCNT_GT_1(ans))
            R_variant_result = 1;
    } else {
	ans = allocVector(TYPEOF(x), 1);
        copy_elements (ans, 0, 0, x, offset, 0, 1);
    }
    UNPROTECT(2);
    return ans;
}

/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
static SEXP do_subset3(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP from, what, ans, input, ncall;

    SEXP string = R_NilValue;
    SEXP name = R_NilValue;
    int argsevald = 0;

    checkArity(op, args);
    from = CAR(args);
    what = CADR(args);

    if (TYPEOF(what) == PROMSXP)
        what = PRCODE(what);
    if (isSymbol(what))
        name = what;
    else if (isString(what) && LENGTH(what) > 0) 
        string = STRING_ELT(what,0);
    else
	errorcall(call, _("invalid subscript type '%s'"), 
                        type2char(TYPEOF(what)));

    /* Handle usual case with no "..." and not from an object quickly, without
       overhead of allocation and calling of DispatchOrEval. */

    if (from != R_DotsSymbol) {
        from = evalv (from, env, VARIANT_ONE_NAMED | VARIANT_UNCLASS);
        if (isObject(from) && ! (R_variant_result & VARIANT_UNCLASS_FLAG)) {
            PROTECT(from);
            argsevald = 1;
        } else {
            R_variant_result = 0;
            return R_subset3_dflt (from, string, name, call, variant);
        }
    }

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */

    input = allocVector(STRSXP,1);

    if (name!=R_NilValue)
	SET_STRING_ELT(input, 0, PRINTNAME(name));
    else
	SET_STRING_ELT(input, 0, string);

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when "from" was */
    /* ..., as in PR#8718 */
    PROTECT(args = CONS(from, CONS(input, R_NilValue)));
    /* Change call used too, for compatibility with
       R-2.15.0:  It's accessible using "substitute", 
       and was a string in R-2.15.0. */
    PROTECT(ncall = LCONS(CAR(call), CONS(CADR(call), CONS(input,R_NilValue))));

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(DispatchOrEval(ncall, op, "$", args, env, &ans, 0, argsevald)) {
        UNPROTECT(2+argsevald);
	if (NAMEDCNT_GT_0(ans))         /* IS THIS NECESSARY? */
	    SET_NAMEDCNT_MAX(ans);
	return ans;
    }

    ans = R_subset3_dflt(CAR(ans), string, name, call, variant);
    UNPROTECT(2+argsevald);
    return ans;
}

/* Used above and in eval.c.  The field to extract is specified by either 
   the "input" argument or the "name" argument, or both.  Protects x. */

SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input, SEXP name, SEXP call,
                                     int variant)
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

    PROTECT(x);

    if (isPairList(x)) {

	SEXP xmatch = R_NilValue;
	int havematch;
        if (name!=R_NilValue) {
            /* Quick check for exact match by name */
            for (y = x; y != R_NilValue; y = CDR(y))
                if (TAG(y)==name) {
                    y = CAR(y);
                    goto found_pairlist;
                }
        }
        cinp = input==R_NilValue ? CHAR(PRINTNAME(name)) : translateChar(input);
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
            if (TAG(y) == R_NilValue)
                continue;
            ctarg = CHAR(PRINTNAME(TAG(y)));
	    mtch = ep_match_strings(ctarg, cinp);
	    if (mtch>0) /* exact */ {
		y = CAR(y);
                goto found_pairlist;
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
            goto found_pairlist;
	}

        UNPROTECT(1);
	return R_NilValue;

      found_pairlist:
        SET_NAMEDCNT_MAX(y);
        UNPROTECT(1);
        return y;
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
                goto found_veclist;
            }
	    else if (mtch<0) /* partial */ {
		havematch++;
		if (havematch == 1) {
		    /* partial matches can cause aliasing in eval.c:evalseq
		       This is overkill, but alternative ways to prevent
		       the aliasing appear to be even worse */
		    SET_NAMEDCNT_MAX(VECTOR_ELT(x,i));
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
	    goto found_veclist;
	}

        UNPROTECT(1);
	return R_NilValue;

      found_veclist:
        if (NAMEDCNT_GT_0(x) && NAMEDCNT_EQ_0(y))
            SET_NAMEDCNT(y,1);
        if (VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
             && !NAMEDCNT_GT_1(x) && !NAMEDCNT_GT_1(y))
            R_variant_result = 1;
        UNPROTECT(1);
        return y;
    }
    else if (isEnvironment(x)) {
        if (name==R_NilValue) {
            name = installed_already (translateChar(input));
            if (name == R_NoObject) {
                UNPROTECT(1);
                return R_NilValue;
            }
        }

        y = findVarInFrame (x, name);
        if (y == R_UnboundValue)
            y = R_NilValue;
        else {
             if (TYPEOF(y) == PROMSXP)
                 y = forcePromise(y);
             else {
                 SET_NAMEDCNT_NOT_0(y);
                 if (VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET
                       && !NAMEDCNT_GT_1(y))
                     R_variant_result = R_binding_cell == R_NilValue ? 2 : 1;
             }
        }
        UNPROTECT(1);
        return y;
    }
    else if( isVectorAtomic(x) ){
	errorcall(call, "$ operator is invalid for atomic vectors");
    }
    else /* e.g. a function */
	nonsubsettable_error(call,x);

    return R_NilValue;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_subset[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"[",		do_subset,	1,	1000,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	2,	1000,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	3,	1000,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},

{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1001,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
