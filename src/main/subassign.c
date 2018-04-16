/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
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
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"
#include <R_ext/RS.h> /* for test of S4 objects */

#include "scalar-stack.h"


/* EnlargeVector() takes a vector "x" and changes its length to
   "newlen".  This allows to assign values "past the end" of the
   vector or list.  Names are extended as well, if present.  The dim
   and dimnames attributes are deleted. Other attributes are
   preserved. */

static SEXP EnlargeVector(SEXP call, SEXP x, R_len_t new_len)
{
    if (!isVector(x))
	errorcall(call,_("attempt to enlarge non-vector"));

    R_len_t len = LENGTH(x);

    if (LOGICAL(GetOption1(install("check.bounds")))[0])
	warningcall (call,
          _("assignment outside vector/list limits (extending from %d to %d)"),
          len, new_len);

    SEXP xnames = getNamesAttrib(x);
    SEXP new_xnames = R_NilValue;
    SEXP new_x;

    if (xnames != R_NilValue) {
        R_len_t old_len = LENGTH(xnames);
        R_len_t i;
        if (NAMEDCNT_GT_1(xnames)) {
            new_xnames = allocVector (STRSXP, new_len);
            copy_string_elements (new_xnames, 0, xnames, 0, old_len);
        }
        else
            new_xnames = reallocVector (xnames, new_len, 1);
        for (i = old_len; i < new_len; i++)
            SET_STRING_ELT_BLANK (new_xnames, i);
    }

    PROTECT(new_xnames);
    PROTECT(new_x = reallocVector (x, new_len, 1));
    no_dim_attributes(new_x);
    if (xnames != R_NilValue && new_xnames != xnames)
        setAttrib (new_x, R_NamesSymbol, new_xnames);
    UNPROTECT(2);  /* new_x, new_xnames */

    return new_x;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in which coerceVector should
   fail; namely, S4SXP */
static SEXP embedInVector(SEXP v)
{
    SEXP ans;
    PROTECT(ans = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, v);
    UNPROTECT(1);
    return (ans);
}

/* Coerces the LHS or RHS to make assignment possible.

   Level 0 and 1 are used for [<-.  They coerce the RHS to a list when the
   LHS is a list.  Level 1 also coerces to avoid assignment of numeric vector 
   to string vector or raw vector to any other numeric or string vector.

   Level 2 is used for [[<-.  It does not coerce when assigning into a list.
*/

static void SubassignTypeFix (SEXP *x, SEXP *y, int stretch, int level, 
                              SEXP call)
{
    Rboolean x_is_object = OBJECT(*x);  /* coercion can lose the object bit */

    const int type_x = TYPEOF(*x), type_y = TYPEOF(*y);
    int atom_x;

    if (type_x == type_y || type_y == NILSXP) {
        /* nothing to do */
    }
    else if ((atom_x = isVectorAtomic(*x)) && isVectorAtomic(*y)) { 
        /* Follow STR > CPLX > REAL > INT > LGL > RAW conversion hierarchy, 
           which never produces warnings. */
        if (type_x == CPLXSXP
              && type_y == STRSXP
         || type_x == REALSXP
              && ((((1<<STRSXP) + (1<<CPLXSXP)) >> type_y) & 1)
         || type_x == INTSXP 
              && ((((1<<STRSXP) + (1<<CPLXSXP) + (1<<REALSXP)) >> type_y) & 1)
         || type_x == LGLSXP
              && ((((1<<STRSXP) + (1<<CPLXSXP) + (1<<REALSXP) + (1<<INTSXP))
                   >> type_y) & 1)
         || type_x == RAWSXP
              && type_y != RAWSXP)
            *x = coerceVector (*x, type_y);
        if (level == 1) { 
            /* For when later code doesn't handle these cases. */
            if (type_y == RAWSXP && type_x != RAWSXP
             || type_y != STRSXP && type_x == STRSXP)
                *y = coerceVector (*y, type_x);
        }
    }
    else if (atom_x && isVectorList(*y)) {
        *x = coerceVector (*x, type_y);
    }
    else if (isVectorList(*x)) {
        if (level != 2)
	    *y = type_y==S4SXP ? embedInVector(*y) : coerceVector (*y, type_x);
    }
    else {
	errorcall(call,
              _("incompatible types (from %s to %s) in subassignment type fix"),
	      type2char(type_y), type2char(type_x));
    }

    if (stretch) {
	PROTECT(*y);
	*x = EnlargeVector(call, *x, stretch);
	UNPROTECT(1);
    }
    SET_OBJECT(*x, x_is_object);
}

/* Returns list made from x (a LISTSXP, EXPRSXP, or NILSXP) with elements 
   from start to end (inclusive) deleted.  The start index will be positive. 
   If start>end, no elements are deleted (x returned unchanged). */

static SEXP DeleteListElementsSeq (SEXP x, R_len_t start, R_len_t end)
{
    SEXP xnew, xnames, xnewnames;
    R_len_t i, len;

    len = length(x);
    if (start < 1)
        start = 1;
    if (end > len) 
        end = len;
    if (start > end)
        return x;

    if (NAMEDCNT_GT_1(x)) {
        PROTECT(xnew = allocVector(TYPEOF(x), len-(end-start+1)));
        for (i = start; i <= end; i++) /* after we know alloc won't fail */
            DEC_NAMEDCNT (VECTOR_ELT (x, i-1));
        if (start>1) 
            copy_vector_elements (xnew, 0, x, 0, start-1);
        if (end<len) 
            copy_vector_elements (xnew, start-1, x, end, len-end);
        copyMostAttrib(x, xnew);
    }
    else {
        for (i = start; i <= end; i++)
            DEC_NAMEDCNT (VECTOR_ELT (x, i-1));
        if (end<len) 
            copy_vector_elements (x, start-1, x, end, len-end);
        PROTECT(xnew = reallocVector (x, len-(end-start+1), 1));
        no_dim_attributes(xnew);
    }

    xnames = getNamesAttrib(x);
    if (xnames != R_NilValue) {
        if (NAMEDCNT_GT_1(xnames)) {
            PROTECT(xnewnames = allocVector(STRSXP, len-(end-start+1)));
            if (start > 1)
                copy_string_elements(xnewnames, 0, xnames, 0, start-1);
            if (end < len)
                copy_string_elements(xnewnames, start-1, xnames, end, len-end);
        }
        else {
            if (end < len)
                copy_string_elements(xnames, start-1, xnames, end, len-end);
            PROTECT(xnewnames = reallocVector (xnames, len-(end-start+1), 1));
        }
        if (xnew != x || xnewnames != xnames) 
            setAttrib(xnew, R_NamesSymbol, xnewnames);
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return xnew;
}

/* Returns list made from x (a LISTSXP, EXPRSXP, or NILSXP) with elements 
   indexed by elements in "which" (an INSTSXP or NILSXP) deleted. */

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    R_len_t i, ii, len, lenw;

    if (x==R_NilValue || which==R_NilValue)
        return x;
    len = LENGTH(x);
    lenw = LENGTH(which);
    if (len==0 || lenw==0) 
        return x;

    /* handle deletion of a contiguous block (incl. one element) specially. */
    for (i = 1; i < lenw; i++)
        if (INTEGER(which)[i] != INTEGER(which)[i-1]+1)
            break;
    if (i == lenw) {
        int start = INTEGER(which)[0];
        int end = INTEGER(which)[lenw-1];
        if (start < 1) start = 1;
        return DeleteListElementsSeq (x, start, end);
    }

    /* create vector indicating which to delete */
    PROTECT(include = allocVector(INTSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(include)[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = INTEGER(which)[i];
	if (0 < ii  && ii <= len)
	    INTEGER(include)[ii - 1] = 0;
    }

    /* calculate the length of the result */
    ii = 0;
    for (i = 0; i < len; i++)
	ii += INTEGER(include)[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }

    PROTECT(xnew = allocVector(TYPEOF(x), ii));
    ii = 0;
    for (i = 0; i < len; i++) {
	if (INTEGER(include)[i] == 1) {
	    SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
	    ii++;
	}
        else
            DEC_NAMEDCNT (VECTOR_ELT (x, i));
    }

    xnames = getNamesAttrib(x);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_STRING_ELT(xnewnames, ii, STRING_ELT(xnames, i));
		ii++;
	    }
	}
	setAttrib(xnew, R_NamesSymbol, xnewnames);
	UNPROTECT(1);
    }

    copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

/* Assigns to a contiguous block of elements with indexes from start to end
   (inclusive).  The start index will be positive.  If start>end, no elements 
   are assigned, but the returned value may have been coerced to match types. 
   The y argument must have length of 1 or the size of the block (end-start+1),
   or be R_NilValue (for deletion).

   The x argument must be protected by the caller. */

static SEXP VectorAssignSeq 
              (SEXP call, SEXP x, R_len_t start, R_len_t end, SEXP y)
{
    int i, n, ny;

    if (x==R_NilValue && y==R_NilValue)
	return R_NilValue;

    n = end - start + 1;

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */

    SubassignTypeFix (&x, &y, end > length(x) ? end : 0, 0, call);

    PROTECT(x);

    ny = length(y);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (n > 0 && ny == 0)
	    errorcall(call,_("replacement has length zero"));
    }

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Do the actual assignment... */

    if (n == 0) {
        /* nothing to do */
    }
    else if (isVectorAtomic(x) && isVectorAtomic(y)) {
        copy_elements_coerced (x, start-1, 1, y, 0, ny>1, n);
    }
    else  if (isVectorList(x) && isVectorList(y)) {
        if (ny == 1) {
            SET_VECTOR_ELEMENT_FROM_VECTOR (x, start-1, y, 0);
            SEXP y0 = VECTOR_ELT (y, 0);
            for (i = 1; i < n; i++) {
                SET_VECTOR_ELT (x, start-1+i, y0);
                INC_NAMEDCNT_0_AS_1 (y0);
            }
        }
        else {
            for (i = 0; i < n; i++)
                SET_VECTOR_ELEMENT_FROM_VECTOR (x, start-1+i, y, i);
        }
    }
    else if (isVectorList(x) && y == R_NilValue) {
	x = DeleteListElementsSeq(x, start, end);
    }
    else {
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

    UNPROTECT(2);
    return x;
}

/* Remove NA indexes from indx, whose length is *n, with first NA at
   index (from 1) first_na.  Returns possibly new index vector, and
   updates *n to number of indexes. */

static SEXP NA_rem (SEXP indx, int *n, int first_na)
{
    int i, j;

    if (NAMEDCNT_GT_0(indx))
        indx = duplicate(indx);

    j = first_na - 1;
    for (i = j+1; i < *n; i++) {
        if (INTEGER(indx)[i] != NA_INTEGER) {
            INTEGER(indx)[j] = INTEGER(indx)[i];
            j += 1;
        }
    }

    *n = j;

    return indx;
}

/* If "err" is 1, raise an error if any NAs are present in "indx", and
   otherwise return "indx" unchanged.  If "err" is 0, return an index
   vector (possibly newly allocated) with any NAs removed, updating "n"
   to its new length. */

static inline SEXP NA_check_remove (SEXP call, SEXP indx, int *n, int err)
{
    int ii;

    for (ii = 0; ii < *n && INTEGER(indx)[ii] != NA_INTEGER; ii++) ;

    if (ii == *n)
        return indx;

    if (err)
        errorcall(call,_("NAs are not allowed in subscripted assignments"));

    return NA_rem (indx, n, ii+1);
}


static SEXP VectorAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    SEXP indx, newnames;
    int i, ii, iy, n, nx, ny;
    double ry;

    if (x==R_NilValue && y==R_NilValue)
	return R_NilValue;

    PROTECT(s);

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    if (isMatrix(s) && isArray(x)) {
        SEXP dim = getDimAttrib(x);
        if (ncols(s) == LENGTH(dim)) {
            if (isString(s)) {
		SEXP dnames = PROTECT(GetArrayDimnames(x));
		s = strmat2intmat(s, dnames, call);
		UNPROTECT(2); /* dnames, s */
                PROTECT(s);
            }
            if (isInteger(s) || isReal(s)) {
                s = mat2indsub(dim, s, call);
                UNPROTECT(1);
                PROTECT(s);
            }
        }
    }

    int stretch = -1; /* allow out of bounds, for assignment */
    int pindx;
    int hasna;
    PROTECT_WITH_INDEX (indx = makeSubscript(x, s, &stretch, &hasna, call, 1),
                        &pindx);
    n = LENGTH(indx);

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */
    SubassignTypeFix(&x, &y, stretch, 1, call);
    if (n == 0) {
	UNPROTECT(2);
	return x;
    }

    PROTECT(x);

    ny = length(y);
    nx = length(x);

    int oldn = n;

    if (hasna) {
        if (length(y) > 1)
            errorcall(call,_("NAs are not allowed in subscripted assignments"));
        REPROTECT (indx = NA_rem (indx, &n, hasna), pindx);
    }

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (oldn > 0 && ny == 0)
	    errorcall(call,_("replacement has length zero"));
	if (oldn > 0 && n % ny)
	    warningcall(call,
             _("number of items to replace is not a multiple of replacement length"));
    }

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int k = 0;
    switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

    case (LGLSXP<<5) + LGLSXP:
    case (INTSXP<<5) + LGLSXP:
    case (INTSXP<<5) + INTSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    INTEGER(x)[ii] = INTEGER(y)[k];
	    if (++k == ny) k = 0;
        }
	break;

    case (REALSXP<<5) + LGLSXP:
    case (REALSXP<<5) + INTSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    iy = INTEGER(y)[k];
	    if (iy == NA_INTEGER)
		REAL(x)[ii] = NA_REAL;
	    else
		REAL(x)[ii] = iy;
	    if (++k == ny) k = 0;
        }
	break;

    case (REALSXP<<5) + REALSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    REAL(x)[ii] = REAL(y)[k];
	    if (++k == ny) k = 0;
        }
	break;

    case (CPLXSXP<<5) + LGLSXP:
    case (CPLXSXP<<5) + INTSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    iy = INTEGER(y)[k];
	    if (iy == NA_INTEGER) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = iy;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    if (++k == ny) k = 0;
        }
	break;

    case (CPLXSXP<<5) + REALSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    ry = REAL(y)[k];
	    if (ISNA(ry)) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = ry;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    if (++k == ny) k = 0;
        }
	break;

    case (CPLXSXP<<5) + CPLXSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    COMPLEX(x)[ii] = COMPLEX(y)[k];
	    if (++k == ny) k = 0;
        }
	break;

    case (STRSXP<<5) + STRSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    SET_STRING_ELT(x, ii, STRING_ELT(y, k));
	    if (++k == ny) k = 0;
        }
	break;

    case (RAWSXP<<5) + RAWSXP:
	for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
	    RAW(x)[ii] = RAW(y)[k];
	    if (++k == ny) k = 0;
        }
	break;

    case (EXPRSXP<<5) + VECSXP:
    case (EXPRSXP<<5) + EXPRSXP:
    case (VECSXP<<5)  + EXPRSXP:
    case (VECSXP<<5)  + VECSXP:
        for (i = 0; i < n; i++) {
            ii = INTEGER(indx)[i] - 1;
            if (i < ny) {
                SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, i);
            }
            else { /* first time we get here, k is and should be 0 */
                SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, k);
                if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ii)))
                    SET_NAMEDCNT(VECTOR_ELT(x,ii),2);
                if (++k == ny) k = 0;
            }
        }
        break;

    case (EXPRSXP<<5) + NILSXP:
    case (VECSXP<<5)  + NILSXP:
	x = DeleteListElements(x, indx);
	UNPROTECT(4);
	return x;
	break;

    default:
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

    /* Check for additional named elements, if subscripting with strings. */
    /* Note makeSubscript passes the additional names back as the use.names
       attribute (a vector list) of the generated subscript vector */
    if (TYPEOF(s)==STRSXP && 
          (newnames = getAttrib(indx, R_UseNamesSymbol)) != R_NilValue) {
        SEXP oldnames = getNamesAttrib(x);
        if (oldnames == R_NilValue) {
            PROTECT(oldnames = allocVector(STRSXP,nx)); /* all R_BlankString */
            setAttrib(x, R_NamesSymbol, oldnames);
            UNPROTECT(1);
        }
        for (i = 0; i < n; i++) {
            if (VECTOR_ELT(newnames, i) != R_NilValue) {
                ii = INTEGER(indx)[i];
                if (ii == NA_INTEGER) continue;
                ii = ii - 1;
                SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
            }
        }
    }
    UNPROTECT(4);
    return x;
}

static SEXP MatrixAssign(SEXP call, SEXP x, SEXP sb1, SEXP sb2, SEXP y)
{
    int i, j, iy;
    int rhasna, chasna;
    int_fast64_t n;
    double ry;
    int nr;
    int nrs, ncs;
    SEXP sr, sc, dim;

    dim = getDimAttrib(x);

    if (dim == R_NilValue || LENGTH(dim) != 2)
	errorcall(call,_("incorrect number of subscripts on matrix"));

    nr = INTEGER(dim)[0];

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT (sr = array_sub (sb1, dim, 0, x, &rhasna));
    nrs = LENGTH(sr);

    PROTECT (sc = array_sub (sb2, dim, 1, x, &chasna));
    ncs = LENGTH(sc);

    /* Do assignment of a single atomic element with matching type specially. */

    if (nrs == 1 && ncs == 1 && isVectorAtomic(x) 
                             && TYPEOF(x) == TYPEOF(y) && LENGTH(y) == 1) {
        if (*INTEGER(sr) != NA_INTEGER && *INTEGER(sc) != NA_INTEGER) {
            R_len_t isub = (*INTEGER(sr)-1) + (*INTEGER(sc)-1) * nr;
            switch (TYPEOF(x)) {
            case RAWSXP: 
                RAW(x)[isub] = *RAW(y);
                break;
            case LGLSXP: 
                LOGICAL(x)[isub] = *LOGICAL(y);
                break;
            case INTSXP: 
                INTEGER(x)[isub] = *INTEGER(y);
                break;
            case REALSXP: 
                REAL(x)[isub] = *REAL(y);
                break;
            case CPLXSXP: 
                COMPLEX(x)[isub] = *COMPLEX(y);
                break;
            case STRSXP:
                SET_STRING_ELT (x, isub, STRING_ELT(y,0));
                break;
            }
        }
        UNPROTECT(2);
        R_scalar_stack = sv_scalar_stack;
        return x;
    }

    R_len_t ny = length(y);

    if (rhasna) {
        sr = NA_check_remove (call, sr, &nrs, ny > 1);
        UNPROTECT(2);
        PROTECT2(sr,sc);
    }
    if (chasna) {
        sc = NA_check_remove (call, sc, &ncs, ny > 1);
        UNPROTECT(2);
        PROTECT2(sr,sc);
    }

    n = nrs * ncs;

    if (n > 0 && ny == 0)
	errorcall(call,_("replacement has length zero"));
    if (n > 0 && n % ny)
	errorcall(call,
       _("number of items to replace is not a multiple of replacement length"));

    SubassignTypeFix(&x, &y, 0, 1, call);
    if (n == 0) {
        UNPROTECT(2);
        R_scalar_stack = sv_scalar_stack;
        return x;
    }

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int * restrict scix = INTEGER(sc);
    int * restrict srix = INTEGER(sr);
    int colix;

    if (ny == 1) {

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP: {
            int tmp = INTEGER(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    INTEGER(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
        case (REALSXP<<5) + REALSXP: {
            double tmp = TYPEOF(y) == REALSXP ? REAL(y)[0] 
                       : INTEGER(y)[0] == NA_INTEGER ? NA_REAL : INTEGER(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    REAL(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP: 
        case (CPLXSXP<<5) + REALSXP:
        case (CPLXSXP<<5) + CPLXSXP: {
            Rcomplex tmp;
            if (TYPEOF(y) == CPLXSXP) 
                tmp = COMPLEX(y)[0];
            else if (TYPEOF(y) == REALSXP) {
                if (ISNAN(REAL(y)[0])) tmp.r = tmp.i = NA_REAL;
                else { tmp.r = REAL(y)[0]; tmp.i = 0; }
            }
            else { /* INT or LGL */
                if (INTEGER(y)[0] == NA_INTEGER) tmp.r = tmp.i = NA_REAL;
                else { tmp.r = INTEGER(y)[0]; tmp.i = 0; }
            }
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    COMPLEX(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (STRSXP<<5) + STRSXP: {
            SEXP tmp = STRING_ELT (y, 0);
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    SET_STRING_ELT (x, srix[i] - 1 + colix, tmp);
            }
            break;
        }
        case (RAWSXP<<5) + RAWSXP: {
            Rbyte tmp = RAW(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    RAW(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP: {
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i]-1+colix, y, 0);
                    if (i+j>0 && NAMEDCNT_EQ_0 (VECTOR_ELT(x,srix[i]-1+colix)))
                        SET_NAMEDCNT (VECTOR_ELT (x, srix[i]-1+colix), 2);
                }
            }
            break;
        }
        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }
    else if (ny >= nrs * ncs) {

        int k = 0;

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    INTEGER(x) [srix[i] - 1 + colix] = INTEGER(y)[k];
                    k += 1;
                }
            }
            break;

        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER)
                        REAL(x) [srix[i] - 1 + colix] = NA_REAL;
                    else
                        REAL(x) [srix[i] - 1 + colix] = iy;
                    k += 1;
                }
            }
            break;

        case (REALSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    REAL(x) [srix[i] - 1 + colix] = REAL(y)[k];
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = iy;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    ry = REAL(y)[k];
                    if (ISNA(ry)) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = ry;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + CPLXSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    COMPLEX(x) [srix[i] - 1 + colix] = COMPLEX(y)[k];
                    k += 1;
                }
            }
            break;

        case (STRSXP<<5) + STRSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_STRING_ELT(x, srix[i] - 1 + colix, STRING_ELT(y, k));
                    k += 1;
                }
            }
            break;

        case (RAWSXP<<5) + RAWSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    RAW(x) [srix[i] - 1 + colix] = RAW(y)[k];
                    k += 1;
                }
            }
            break;

        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    if (k < ny) {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k);
                    }
                    else {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k%ny);
                        if (NAMEDCNT_EQ_0 (VECTOR_ELT (x, srix[i] - 1 + colix)))
                            SET_NAMEDCNT(VECTOR_ELT(x, srix[i] - 1 + colix), 2);
                    }
                    k += 1;
                }
            }
            break;

        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }
    else {  /* y is recycled, and is not of length 1 */

        int k = 0;

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    INTEGER(x) [srix[i] - 1 + colix] = INTEGER(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER)
                        REAL(x) [srix[i] - 1 + colix] = NA_REAL;
                    else
                        REAL(x) [srix[i] - 1 + colix] = iy;
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (REALSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    REAL(x) [srix[i] - 1 + colix] = REAL(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = iy;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    ry = REAL(y)[k];
                    if (ISNA(ry)) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = ry;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + CPLXSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    COMPLEX(x) [srix[i] - 1 + colix] = COMPLEX(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (STRSXP<<5) + STRSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_STRING_ELT(x, srix[i] - 1 + colix, STRING_ELT(y, k));
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (RAWSXP<<5) + RAWSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    RAW(x) [srix[i] - 1 + colix] = RAW(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    if (k < ny) {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k);
                    }
                    else {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k%ny);
                        if (NAMEDCNT_EQ_0 (VECTOR_ELT (x, srix[i] - 1 + colix)))
                            SET_NAMEDCNT(VECTOR_ELT(x, srix[i] - 1 + colix), 2);
                    }
                    k += 1;
                }
            }
            break;

        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }

    UNPROTECT(4);
    R_scalar_stack = sv_scalar_stack;
    return x;
}


static SEXP ArrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, iy, k=0, ny;
    int rep_assign = 0; /* 1 if elements assigned repeatedly into list array */
    int any_hasna = 0;
    SEXP dims, tmp;
    double ry;

    PROTECT(dims = getDimAttrib(x));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	errorcall(call,_("incorrect number of subscripts"));

    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    SubassignTypeFix(&x, &y, 0, 1, call);

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    ny = length(y);

    int *subs[k], indx[k], bound[k], offset[k], hasna[k];
    SEXP sv_scalar_stack = R_scalar_stack;

    int nmod = ny > 1;
    int zero = 0;
    for (i = 0; i < k; i++) {
        PROTECT(tmp = array_sub (CAR(s), dims, i, x, &hasna[i]));
	bound[i] = LENGTH(tmp);
        if (hasna[i]) {
            any_hasna = 1;
            if (ny <= 1) {
                tmp = NA_rem (tmp, &bound[i], hasna[i]);
                UNPROTECT(1);
                PROTECT(tmp);
            }
        }
        subs[i] = INTEGER(tmp);
        if (bound[i] == 0) zero = 1;
        if (ny > 1) nmod = ((int_fast64_t)nmod * bound[i]) % ny;
        indx[i] = 0;
	s = CDR(s);
    }

    if (!zero && ny == 0)
	errorcall(call,_("replacement has length zero"));
    if (!zero && nmod != 0)
	errorcall(call,
       _("number of items to replace is not a multiple of replacement length"));

    if (any_hasna && ny > 1)
        errorcall (call, _("NAs are not allowed in subscripted assignments"));

    if (zero) {
	UNPROTECT(k+3);
        R_scalar_stack = sv_scalar_stack;
	return x;
    }

    offset[1] = INTEGER(dims)[0];
    offset[0] = offset[1] + 1;
    for (i = 2; i < k; i++) {
        offset[i] = offset[i-1] * INTEGER(dims)[i-1];
        offset[0] += offset[i];
    }

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int which = (TYPEOF(x)<<5) + TYPEOF(y);

#   define AA_PRELUDE \
    for (;;) { \
        ii = subs[0][indx[0]] - offset[0]; \
        if (k == 3) { \
            ii += offset[1] * subs[1][indx[1]]; \
            ii += offset[2] * subs[2][indx[2]]; \
        } \
        else if (k == 4) { \
            ii += offset[1] * subs[1][indx[1]]; \
            ii += offset[2] * subs[2][indx[2]]; \
            ii += offset[3] * subs[3][indx[3]]; \
        } \
        else { \
            for (j = 1; j < k; j++) \
                ii += offset[j] * subs[j][indx[j]]; \
        }

#   define AA_POSTLUDE \
        j = 0; \
        while (++indx[j] >= bound[j]) { \
            indx[j] = 0; \
            if (++j >= k) goto done; \
        } \
        if (++i == ny) i = 0; \
    } /* end of for(;;) */

    i = 0;

    switch (which) {

    case (LGLSXP<<5) + LGLSXP:
    case (INTSXP<<5) + LGLSXP:
    case (INTSXP<<5) + INTSXP:
        AA_PRELUDE
            INTEGER(x)[ii] = INTEGER(y)[i];
        AA_POSTLUDE
        break;

    case (REALSXP<<5) + LGLSXP:
    case (REALSXP<<5) + INTSXP:
        AA_PRELUDE
            iy = INTEGER(y)[i];
            if (iy == NA_INTEGER)
                REAL(x)[ii] = NA_REAL;
            else
                REAL(x)[ii] = iy;
        AA_POSTLUDE
        break;

    case (REALSXP<<5) + REALSXP:
        AA_PRELUDE
            REAL(x)[ii] = REAL(y)[i];
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + LGLSXP:
    case (CPLXSXP<<5) + INTSXP:
        AA_PRELUDE
            iy = INTEGER(y)[i];
            if (iy == NA_INTEGER) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = iy;
                COMPLEX(x)[ii].i = 0.0;
            }
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + REALSXP:
        AA_PRELUDE
            ry = REAL(y)[i];
            if (ISNA(ry)) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = ry;
                COMPLEX(x)[ii].i = 0.0;
            }
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + CPLXSXP:
        AA_PRELUDE
            COMPLEX(x)[ii] = COMPLEX(y)[i];
        AA_POSTLUDE
        break;

    case (STRSXP<<5) + STRSXP:
        AA_PRELUDE
            SET_STRING_ELT(x, ii, STRING_ELT(y, i));
        AA_POSTLUDE
        break;

    case (RAWSXP<<5) + RAWSXP:
        AA_PRELUDE
            RAW(x)[ii] = RAW(y)[i];
        AA_POSTLUDE
        break;

    case (EXPRSXP<<5) + VECSXP:
    case (EXPRSXP<<5) + EXPRSXP:
    case (VECSXP<<5)  + EXPRSXP:
    case (VECSXP<<5)  + VECSXP:
        AA_PRELUDE
            SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, i);
            if (!rep_assign) {
                if (i == ny - 1) 
                    rep_assign = 1;
            }
            else {
                if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ii)))
                    SET_NAMEDCNT(VECTOR_ELT(x,ii),2);
            }
        AA_POSTLUDE
        break;

    default:
        warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

  done:
    UNPROTECT(k+3);
    R_scalar_stack = sv_scalar_stack;
    return x;
}

/* Returns 'subs' as the index list, and 'y' as the value to assign.
   May modify 'subs'. */

static void SubAssignArgs(SEXP *subs, SEXP *y, SEXP call)
{
    SEXP args = *subs;

    if (args == R_NilValue)
	errorcall(call,_("SubAssignArgs: invalid number of arguments"));

    if (CDR(args) == R_NilValue) {
	*subs = R_NilValue;
	*y = CAR(args);
    }
    else {
	while (CDDR(args) != R_NilValue)
	    args = CDR(args);
	*y = CADR(args);
	SETCDR_NIL(args);
    }
}

/* The [<- operator. */

static SEXP do_subassign_dflt_seq (SEXP call, SEXP x, SEXP sb1, SEXP sb2, 
                                   SEXP subs, SEXP rho, SEXP y, int64_t seq);

static SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP sv_scalar_stack = R_scalar_stack;

    SEXP ans, x, sb1, sb2, subs, y;
    int argsevald = 0;
    int64_t seq = 0;

    /* See if we are using the fast interface. */

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {

        /* Fast interface: object assigned into (x) comes already
           evaluated.  Evaluate indexes using VARIANT_SCALAR_STACK_OK,
           and evaluate a single index with VARIANT_SEQ so it may come
           as a range rather than a vector.  */

        y = R_fast_sub_replacement;  /* may be on scalar stack */
        x = R_fast_sub_var;
        sb1 = CAR(args);
        subs = CDR(args);

        if (subs == R_NilValue) {
            sb1 = evalv (sb1, rho, VARIANT_SEQ | VARIANT_SCALAR_STACK_OK |
                                                 VARIANT_MISSING_OK);
            if (R_variant_result) {
                seq = R_variant_seq_spec;
                R_variant_result = 0;
            }
            sb2 = R_NoObject;
        }
        else {
            sb1 = evalv (sb1, rho, VARIANT_SCALAR_STACK_OK |
                                   VARIANT_MISSING_OK);
            PROTECT(sb1);
            if (TAG(subs) != R_NilValue || CAR(subs) == R_DotsSymbol)
                sb2 = R_NoObject;
            else {
                sb2 = evalv (CAR(subs), rho, VARIANT_SCALAR_STACK_OK |
                                             VARIANT_MISSING_OK);
                subs = CDR(subs);
            }
            if (subs != R_NilValue) {
                PROTECT(sb2);
                subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                              VARIANT_MISSING_OK);
                UNPROTECT(1); /* sb2 */
            }
            UNPROTECT(1); /* sb1 */
        }

        goto dflt_seq;
    }

    y = R_NoObject;  /* found later after other arguments */
    x = CAR(args);   /* args are (x, indexes..., y) */
    sb1 = R_NoObject;
    sb2 = R_NoObject;
    subs = CDR(args);

    if (x != R_DotsSymbol) {

        /* Mostly called from do_set, with first arg an evaluated promise. */

        PROTECT (x = TYPEOF(x) == PROMSXP && PRVALUE(x) != R_UnboundValue
                        ? PRVALUE(x) : eval(x,rho));
        if (isObject(x)) {
            args = CONS(x,subs);
            UNPROTECT(1);
            argsevald = -1;
        }
        else if (TYPEOF(CAR(subs)) != LANGSXP || CDR(subs) != R_NilValue) {
            /* in particular, CAR(subs) might be missing or ... */
            subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                          VARIANT_MISSING_OK);
            UNPROTECT(1);
            goto dflt_seq;
        }
        else {
            PROTECT(sb1 = evalv (CAR(subs), rho, VARIANT_SEQ |
                            VARIANT_SCALAR_STACK_OK | VARIANT_MISSING_OK));
            if (R_variant_result) {
                seq = R_variant_seq_spec;
                R_variant_result = 0;
            }
            subs = R_NilValue;
            UNPROTECT(2);
            goto dflt_seq;
        }
    }

    /* This code performs an internal version of method dispatch. */
    /* We evaluate the first argument and attempt to dispatch on it. */
    /* If the dispatch fails, we "drop through" to the default code below. */

    if (DispatchOrEval(call, op, "[<-", args, rho, &ans, 0, argsevald))
        return ans;

    return do_subassign_dflt_seq
       (call, CAR(ans), R_NoObject, R_NoObject, CDR(ans), rho, R_NoObject, 0);

    /* ... path that bypasses DispatchOrEval ... */

  dflt_seq: ;

    SEXP r = do_subassign_dflt_seq (call, x, sb1, sb2, subs, rho, y, seq);

    R_scalar_stack = sv_scalar_stack;
    return r;
}

/* N.B.  do_subassign_dflt is called directly from elsewhere. */

SEXP attribute_hidden do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return do_subassign_dflt_seq 
      (call, CAR(args), R_NoObject, R_NoObject, CDR(args), rho, R_NoObject, 0);
}

/* The last "seq" argument below is non-zero if the first subscript is a 
   sequence spec (a variant result). */

static SEXP do_subassign_dflt_seq (SEXP call, SEXP x, SEXP sb1, SEXP sb2, 
                                   SEXP subs, SEXP rho, SEXP y, int64_t seq)
{
    BEGIN_PROTECT0 ();
    ALSO_PROTECT5 (x, sb1, sb2, subs, y);

    if (y == R_NoObject)
        SubAssignArgs (&subs, &y, call);
    else if (ON_SCALAR_STACK(y) && !isVectorAtomic(x))
        y = DUP_STACK_VALUE(y);

    if (sb1 == R_NoObject) {
        if (subs != R_NilValue) {
            sb1 = CAR(subs);
            subs = CDR(subs);
        }
    }

    if (sb2 == R_NoObject) {
        if (subs != R_NilValue) {
            sb2 = CAR(subs);
            subs = CDR(subs);
        }
    }

    Rboolean S4 = IS_S4_OBJECT(x);
    int oldtype = NILSXP;

    WAIT_UNTIL_COMPUTED(x);

    if (isVector(x)) {
        if (LENGTH(x) == 0) {
            if (length(y) == 0) {
                RETURN_SEXP_INSIDE_PROTECT(x);
            }
        }
        else if (NAMEDCNT_GT_1(x))
            x = dup_top_level(x);
    }
    else if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP) {
        oldtype = TYPEOF(x);
        SEXP ox = x;
        x = PairToVectorList(x);
        setAttrib (x, R_DimSymbol, getAttrib (ox, R_DimSymbol));
        setAttrib (x, R_DimNamesSymbol, getAttrib (ox, R_DimNamesSymbol));
    }
    else if (x == R_NilValue) {
        if (length(y) == 0) {
            RETURN_SEXP_INSIDE_PROTECT(x);
        }
        x = coerceVector(x, TYPEOF(y));
    }
    else
        nonsubsettable_error(call,x);

    if (sb1 == R_NoObject) {
        /* 0 subscript arguments */
        x = VectorAssign(call, x, R_MissingArg, y);
    }
    else if (sb2 == R_NoObject) {
        /* 1 subscript argument */
        if (seq) {
            int start, end;
            sb1 = Rf_DecideVectorOrRange (seq, &start, &end, call);
            if (sb1 == R_NoObject) {
                R_len_t leny;
                if (start < 0 || y != R_NilValue && (leny = length(y)) != 1
                                                 && leny != end - start + 1)
                    sb1 = Rf_VectorFromRange (start, end);
                else
                    x = VectorAssignSeq (call, x, start, end, y);
            }
        }
        if (sb1 != R_NoObject) {
            /* do simple scalar cases quickly */
            if ((TYPEOF(sb1) == INTSXP || TYPEOF(sb1) == REALSXP)
                  && TYPEOF(x) == TYPEOF(y) && LENGTH(sb1) == 1 
                  && isVector(x) && LENGTH(y) == 1) {
                double sub;
                if (TYPEOF(sb1) == INTSXP)
                    sub = *INTEGER(sb1) == NA_INTEGER ? 0 : *INTEGER(sb1);
                else
                    sub = ISNAN(*REAL(sb1)) ? 0 : *REAL(sb1);
                if (sub >= 1 && sub <= LENGTH(x)) {
                    int isub = (int) sub - 1;
                    switch (TYPEOF(x)) {
                        case RAWSXP: 
                            RAW(x)[isub] = *RAW(y);
                            break;
                        case LGLSXP: 
                            LOGICAL(x)[isub] = *LOGICAL(y);
                            break;
                        case INTSXP: 
                            INTEGER(x)[isub] = *INTEGER(y);
                            break;
                        case REALSXP: 
                            REAL(x)[isub] = *REAL(y);
                            break;
                        case CPLXSXP: 
                            COMPLEX(x)[isub] = *COMPLEX(y);
                            break;
                        case STRSXP:
                            SET_STRING_ELT (x, isub, STRING_ELT(y,0));
                            break;
                        case VECSXP: case EXPRSXP:
                            DEC_NAMEDCNT (VECTOR_ELT (x, isub));
                            SET_VECTOR_ELEMENT_FROM_VECTOR (x, isub, y, 0);
                            break;
                    }
                    goto out;
                }
            }
            x = VectorAssign (call, x, sb1, y);
        }
    }
    else if (subs == R_NilValue) {
        /* 2 subscript arguments */
        x = MatrixAssign(call, x, sb1, sb2, y);
    }
    else {
        /* More than 2 subscript arguments */
        subs = CONS(sb1,CONS(sb2,subs));
        x = ArrayAssign(call, x, subs, y);
    }

  out:
    if (oldtype == LANGSXP) {
        if (LENGTH(x)==0)
            errorcall(call,
              _("result is zero-length and so cannot be a language object"));
        x = VectorToPairList(x);
        SET_TYPEOF (x, LANGSXP);
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    if (!isList(x)) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);

    RETURN_SEXP_INSIDE_PROTECT(x);
    END_PROTECT;
}

/* The [[<- operator; should be fast. */

static SEXP do_subassign2_dflt_int
        (SEXP call, SEXP x, SEXP sb1, SEXP sb2, SEXP subs, SEXP rho, SEXP y);

static SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {

        SEXP scalar_stack_sv = R_scalar_stack;
        SEXP y = R_fast_sub_replacement; /* may be on the scalar stack */
        SEXP x = R_fast_sub_var;
        SEXP sb1, sb2, subs;

        sb1 = evalv (CAR(args), rho, VARIANT_SCALAR_STACK_OK | 
                                     VARIANT_MISSING_OK);
        subs = CDR(args);

        PROTECT(sb1);
        if (subs == R_NilValue || TAG(subs) != R_NilValue 
                               || CAR(subs) == R_DotsSymbol)
            sb2 = R_NoObject;
        else {
            sb2 = evalv (CAR(subs), rho, VARIANT_SCALAR_STACK_OK |
                                         VARIANT_MISSING_OK);
            subs = CDR(subs);
        }
        if (subs != R_NilValue) {
            PROTECT(sb2);
            subs = evalList_v (subs, rho, VARIANT_SCALAR_STACK_OK |
                                          VARIANT_MISSING_OK);
            UNPROTECT(1); /* sb2 */
        }
        UNPROTECT(1); /* sb1 */

        SEXP r = do_subassign2_dflt_int (call, x, sb1, sb2, subs, rho, y);
        R_scalar_stack = scalar_stack_sv;
        return r;
    }

    SEXP ans;

    if (DispatchOrEval(call, op, "[[<-", args, rho, &ans, 0, 0))
        return ans;

    return do_subassign2_dflt_int 
            (call, CAR(ans), R_NoObject, R_NoObject, CDR(ans), rho, R_NoObject);
}

/* Also called directly from elsewhere. */

SEXP attribute_hidden do_subassign2_dflt
                               (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return do_subassign2_dflt_int 
         (call, CAR(args), R_NoObject, R_NoObject, CDR(args), rho, R_NoObject);
}

static SEXP do_subassign2_dflt_int
         (SEXP call, SEXP x, SEXP sb1, SEXP sb2, SEXP subs, SEXP rho, SEXP y)
{
    SEXP dims, newname, xup;
    int i, ndims, nsubs, offset, off = -1 /* -Wall */, stretch;
    Rboolean S4, recursed;
    R_len_t length_x;

    BEGIN_PROTECT2 (names, xtop);
    ALSO_PROTECT5 (x, sb1, sb2, subs, y);

    SEXP xOrig = R_NilValue;

    if (y == R_NoObject)
        SubAssignArgs (&subs, &y, call);
    else if (ON_SCALAR_STACK(y) && !isVectorAtomic(x))
        y = DUP_STACK_VALUE(y);

    nsubs = 0;

    if (sb1 != R_NoObject)
        nsubs += 1;
    else {
        if (subs != R_NilValue) {
            sb1 = CAR(subs);
            subs = CDR(subs);
            nsubs += 1;
        }
    }

    if (sb2 != R_NoObject)
        nsubs += 1;
    else {
        if (subs != R_NilValue) {
            sb2 = CAR(subs);
            subs = CDR(subs);
            nsubs += 1;
        }
    }

    if (subs != R_NilValue)
        nsubs += length(subs);

    /* At this point, nsubs will be the number of indexes, sb1 will be the first
       index, sb2 the second, subs will be a pairlist of remaining indexes. */

    WAIT_UNTIL_COMPUTED(x);

    S4 = IS_S4_OBJECT(x);

    dims = getDimAttrib(x);
    ndims = dims==R_NilValue ? 1 : LENGTH(dims);

    /* Note: below, no duplication is necessary for environments. */

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
        xOrig = x; /* will be an S4 object */
        x = R_getS4DataSlot(x, ANYSXP);
        if(TYPEOF(x) != ENVSXP)
          errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    /* ENVSXP special case first; requires that nsubs be 1. */
    if( TYPEOF(x) == ENVSXP) {
        if (nsubs != 1 || !isString(sb1) || length(sb1) != 1)
            errorcall(call,_("wrong args for environment subassignment"));
        SEXP name = install (translateChar (STRING_ELT (sb1, 0)));
        set_var_in_frame (name, y, x, TRUE, 3);
        RETURN_SEXP_INSIDE_PROTECT (S4 ? xOrig : x);
    }

    if (x == R_NilValue) {
        /* Handle NULL left-hand sides.  If the right-hand side is NULL,
           just return NULL, otherwise replace x by a zero length list 
           (VECSXP) or vector of type of y (if y of length one).  (This
           dependence on the length of y is of dubious wisdom!) */
        if (y == R_NilValue) {
            RETURN_SEXP_INSIDE_PROTECT (R_NilValue);
        }
        if (length(y) == 1)
            x = allocVector(TYPEOF(y), 0);
        else
            x = allocVector(VECSXP, 0);
    }
    else if (isPairList(x))
        x = duplicate(x);
    else if (isVectorList(x)) {
        if (NAMEDCNT_GT_1(x) || x == y)
            x = dup_top_level(x);
    }

    /* Special fast handling of one numeric or string index for a vector object,
       with no complications.  Any possible error conditions are handled by
       just falling through for the code below to handle it. */

    if (nsubs == 1 && ndims <= 1 && !S4
        && (TYPEOF(sb1) == REALSXP || TYPEOF(sb1) == INTSXP 
                                   || TYPEOF(sb1) == STRSXP) && LENGTH(sb1) == 1
        && (isVectorAtomic(x) && TYPEOF(x) == TYPEOF(y) && LENGTH(y) == 1
              || isVectorList(x) && y != R_NilValue)) {
        R_len_t lenx = LENGTH(x);
        R_len_t ix = 0;
        if (TYPEOF(sb1) == INTSXP)
            ix = INTEGER(sb1)[0];
        else if (TYPEOF(sb1) == REALSXP) {
            double d = REAL(sb1)[0];
            if (!ISNAN(d)) {
                ix = (int) d;
                if ((double) ix != d)
                    ix = 0;
            }
        }
        else { /* string */
            SEXP names = getAttrib (x, R_NamesSymbol);
            if (TYPEOF(names) == STRSXP) {
                SEXP se = STRING_ELT(sb1,0);
                if (se != NA_STRING && CHAR(se)[0] != 0) {
                    for (int i = 0; i < lenx; i++) {
                        if (STRING_ELT(names,i) != NA_STRING 
                             && SEQL (STRING_ELT(names,i), se)) {
                            ix = i + 1;
                            break;
                        }
                    }
                }
            }
        }
        if (ix > 0 && ix <= lenx) {
            ix -= 1;
            switch (TYPEOF(x)) {
                case RAWSXP:
                    RAW(x)[ix] = *RAW(y);
                    break;
                case LGLSXP:
                    LOGICAL(x)[ix] = *LOGICAL(y);
                    break;
                case INTSXP:
                    INTEGER(x)[ix] = *INTEGER(y);
                    break;
                case REALSXP:
                    REAL(x)[ix] = *REAL(y);
                    break;
                case CPLXSXP:
                    COMPLEX(x)[ix] = *COMPLEX(y);
                    break;
                case STRSXP:
                    SET_STRING_ELT (x, ix, STRING_ELT(y,0));
                    break;
                case VECSXP: case EXPRSXP:
                    DEC_NAMEDCNT (VECTOR_ELT (x, ix));
                    SET_VECTOR_ELEMENT_TO_VALUE (x, ix, y);
                break;
            }
            SET_NAMEDCNT_0(x);
            RETURN_SEXP_INSIDE_PROTECT (x);
        }
    }

    xtop = xup = x; /* x will contain the element which is assigned to; */
                    /*   xup may contain x; xtop is what is returned.  */ 

    recursed = FALSE;

    R_len_t len;

    if (nsubs == 1) { /* One vector index for a list. */
        len = length(sb1);
        if (len > 1) {
            int str_sym_sub = isString(sb1) || isSymbol(sb1);
            for (int i = 0; i < len-1; i++) {
                if (!isVectorList(x) && !isPairList(x))
                    errorcall (call, 
                      _("recursive indexing failed at level %d\n"), i+1);
                length_x = length(x);
                off = get1index (sb1, 
                        str_sym_sub ? getNamesAttrib(x) : R_NilValue,
                        length_x, TRUE, i, call);
                if (off < 0 || off >= length_x)
                    errorcall(call, _("no such index at level %d\n"), i+1);
                xup = x;
                if (isPairList(xup))
                    x = CAR (nthcdr (xup, off));
                else {
                    x = VECTOR_ELT (xup, off);
                    if (isPairList(x)) {
                        x = duplicate(x);
                        SET_VECTOR_ELT (xup, off, x);
                    }
                    else if (isVectorList(x) && NAMEDCNT_GT_1(x)) {
                        x = dup_top_level(x);
                        SET_VECTOR_ELT (xup, off, x);
                    }
                }
            }
            recursed = TRUE;
        }
    }

    if (isVector(x)) {
        R_len_t length_y = length(y);
        if (!isVectorList(x) && length_y == 0)
            errorcall(call,_("replacement has length zero"));
        if (!isVectorList(x) && length_y > 1)
            errorcall(call,
               _("more elements supplied than there are to replace"));
        if (nsubs == 0 || sb1 == R_MissingArg)
            errorcall(call,_("[[ ]] with missing subscript"));
    }

    stretch = 0;
    newname = R_NilValue;

    length_x = length(x);

    if (nsubs == 1) {
        int str_sym_sub = isString(sb1) || isSymbol(sb1);
        offset = get1index (sb1, 
                   str_sym_sub ? getNamesAttrib(x) : R_NilValue,
                   length_x, FALSE, (recursed ? len-1 : -1), call);
        if (offset < 0) {
            if (str_sym_sub)
                offset = length_x;
            else
                errorcall(call,_("[[ ]] subscript out of bounds"));
        }
        if (offset >= length_x) {
            stretch = offset + 1;
            newname = isString(sb1) ? STRING_ELT(sb1,len-1)
                    : isSymbol(sb1) ? PRINTNAME(sb1) : R_NilValue;
        }
    }
    else {
        if (ndims != nsubs)
            errorcall(call,_("[[ ]] improper number of subscripts"));
        names = getAttrib(x, R_DimNamesSymbol);
        offset = 0;
        SEXP s = subs;
        for (i = ndims-1; i >= 0; i--) {
            SEXP ix;
            if (i == 0)
                ix = sb1;
            else if (i == 1)
                ix = sb2;
            else {
                ix = CAR(s);
                s = CDR(s);
            }
            R_len_t ii = 
             get1index(ix, names==R_NilValue ? R_NilValue : VECTOR_ELT(names,i),
                       INTEGER(dims)[i], /*partial ok*/ FALSE, -1, call);
            if (ii < 0 || ii >= INTEGER(dims)[i])
                errorcall(call,_("[[ ]] subscript out of bounds"));
            offset += ii;
            if (i > 0) offset *= INTEGER(dims)[i-1];
        }
    }

    if (isVector(x)) {

        if (nsubs == 1 && isVectorList(x) && y == R_NilValue) {
            x = DeleteListElementsSeq (x, offset+1, offset+1);
        }
        else {

            SubassignTypeFix(&x, &y, stretch, 2, call);
    
            if (NAMEDCNT_GT_1(x) || x == y)
                x = dup_top_level(x);
    
            if (isVectorAtomic(x))
                copy_elements_coerced (x, offset, 0, y, 0, 0, 1);
            else if (isVectorList(x)) {
                DEC_NAMEDCNT (VECTOR_ELT(x, offset));
                SET_VECTOR_ELEMENT_TO_VALUE (x, offset, y);
            }
            else
                errorcall(call,
                   _("incompatible types (from %s to %s) in [[ assignment"),
                      type2char(TYPEOF(y)), type2char(TYPEOF(x)));
    
            /* If we stretched, we may have a new name. */
            /* In this case we must create a names attribute */
            /* (if it doesn't already exist) and set the new */
            /* value in the names attribute. */
            if (stretch && newname != R_NilValue) {
                names = getNamesAttrib(x);
                if (names == R_NilValue) {
                    names = allocVector(STRSXP, LENGTH(x));
                    SET_STRING_ELT(names, offset, newname);
                    setAttrib(x, R_NamesSymbol, names);
                }
                else
                    SET_STRING_ELT(names, offset, newname);
            }
        }
    }

    else if (isPairList(x)) {

        SET_NAMEDCNT_MAX(y);
        if (nsubs == 1) {
            if (y == R_NilValue)
                x = with_no_nth(x,offset+1);
            else if (!stretch)
                x = with_changed_nth(x,offset+1,y);
            else {
                SEXP append = 
                  cons_with_tag (y, R_NilValue, newname == R_NilValue ? 
                                  R_NilValue : install(translateChar(newname)));
                for (i = length_x + 1; i < stretch; i++)
                    append = CONS(R_NilValue,append);
                x = with_pairlist_appended(x,append);
            }
        }
        else {
            SEXP nth = nthcdr(x,offset);
            SETCAR(nth,y);
        }
    }

    else 
        nonsubsettable_error(call,x);

    /* The modified "x" may now be a different object, due to deletion or
       extension, so we need to update the reference to it. */

    if (recursed) {
        if (isVectorList(xup))
            SET_VECTOR_ELT(xup, off, x);
        else {
            SETCAR(nthcdr(xup,off),x); /* xup was duplicated, so this is safe */
            SET_NAMEDCNT_MAX(x);
        }
    }
    else
        xtop = x;

    if (!isList(xtop)) SET_NAMEDCNT_0(xtop);
    if(S4) SET_S4_OBJECT(xtop);

    RETURN_SEXP_INSIDE_PROTECT (xtop);
    END_PROTECT;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string. */

static SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP into, what, value, ans, string, ncall;

    SEXP schar = R_NilValue;
    SEXP name = R_NilValue;
    int argsevald = 0;

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {
        value = R_fast_sub_replacement; /* may be on scalar stack */
        into = R_fast_sub_var;
        what = CAR(args);
        if (args == R_NilValue || CDR(args) != R_NilValue)
            errorcall (call, _("%d arguments passed to '%s' which requires %d"),
                             length(args)+2, PRIMNAME(op), 3);
    }
    else {
        into = CAR(args);
        what = CADR(args);
        value = CADDR(args);
        if (CDDR(args) == R_NilValue || CDR(CDDR(args)) != R_NilValue)
            errorcall (call, _("%d arguments passed to '%s' which requires %d"),
                             length(args), PRIMNAME(op), 3);
    }

    if (TYPEOF(what) == PROMSXP)
        what = PRCODE(what);

    if (isSymbol(what)) {
        name = what;
        schar = PRINTNAME(name);
    }
    else if (isString(what) && LENGTH(what) > 0)
        schar = STRING_ELT(what,0);
    else
        errorcall(call, _("invalid subscript type '%s'"), 
                        type2char(TYPEOF(what)));

    /* Handle the fast case, for which 'into' and 'value' have been evaluated,
       and 'into' is not an object. */

    if (VARIANT_KIND(variant) == VARIANT_FAST_SUB) {
        if (name == R_NilValue) name = install(translateChar(schar));
        return R_subassign3_dflt (call, into, name, value);
    }

    /* Handle usual case with no "..." and not into an object quickly, without
       overhead of allocation and calling of DispatchOrEval. */

    if (into != R_DotsSymbol) {
        /* Note: mostly called from do_set, w first arg an evaluated promise */
        into = TYPEOF(into) == PROMSXP && PRVALUE(into) != R_UnboundValue
                 ? PRVALUE(into) : eval (into, env);
        if (isObject(into)) {
            argsevald = -1;
        } 
        else {
            PROTECT(into);
            if (name == R_NilValue) name = install(translateChar(schar));
            value = eval (value, env);
            UNPROTECT(1);
            return R_subassign3_dflt (call, into, name, value);
        }
    }

    /* First translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly.
       We also change the call used, as in do_subset3, since the
       destructive change in R-2.15.0 has this side effect. */

    PROTECT(into);
    string = allocVector(STRSXP,1);
    SET_STRING_ELT (string, 0, schar);
    PROTECT(args = CONS(into, CONS(string, CDDR(args))));
    PROTECT(ncall = 
      LCONS(CAR(call),CONS(CADR(call),CONS(string,CDR(CDDR(call))))));

    if (DispatchOrEval (ncall, op, "$<-", args, env, &ans, 0, argsevald)) {
        UNPROTECT(3);
        return ans;
    }

    PROTECT(ans);
    if (name == R_NilValue) name = install(translateChar(schar));
    UNPROTECT(4);

    return R_subassign3_dflt(call, CAR(ans), name, CADDR(ans));
}

/* Also called directly from elsewhere.  Protects x and val; name should be
   a symbol and hence not needing protection. */

#define na_or_empty_string(strelt) ((strelt)==NA_STRING || CHAR((strelt))[0]==0)

SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP name, SEXP val)
{
   PROTECT_INDEX pvalidx, pxidx;
    Rboolean S4; SEXP xS4 = R_NilValue;

    if (ON_SCALAR_STACK(val)) /* currently, never puts value in atomic vector */
        val = DUP_STACK_VALUE(val); 

    WAIT_UNTIL_COMPUTED(x);

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = IS_S4_OBJECT(x);

    /* code to allow classes to extend ENVSXP */
    if (TYPEOF(x) == S4SXP) {
	xS4 = x;
        x = R_getS4DataSlot(x, ANYSXP);
	if (x == R_NilValue)
            errorcall (call, 
              _("no method for assigning subsets of this S4 class"));
    }

    switch (TYPEOF(x)) {

    case LISTSXP: case LANGSXP: ;
        int ix = tag_index(x,name);
        if (ix == 0) {
            if (val != R_NilValue) {
                x = with_pairlist_appended (x,
                      cons_with_tag (val, R_NilValue, name));
                SET_NAMEDCNT_MAX(val);
            }
        }
        else if (val == R_NilValue) {
            x = with_no_nth (x, ix);
        }
        else {
            x = with_changed_nth (x, ix, val);
            SET_NAMEDCNT_MAX(val);
        }
        break;

    case ENVSXP:
	set_var_in_frame (name, val, x, TRUE, 3);
        break;

    case SYMSXP: case CLOSXP: case SPECIALSXP: case BUILTINSXP:
        /* Used to 'work' in R < 2.8.0 */
        nonsubsettable_error(call,x);

    default:
	warningcall(call,_("Coercing LHS to a list"));
	REPROTECT(x = coerceVector(x, VECSXP), pxidx);
	/* fall through to VECSXP / EXPRSXP / NILSXP code */

    case VECSXP: case EXPRSXP: case NILSXP: ;

        SEXP pname = PRINTNAME(name);
        int type = TYPEOF(x);
        int imatch = -1;
        SEXP names;
        R_len_t nx;

        if (type == NILSXP) {
            names = R_NilValue;
            type = VECSXP;
            nx = 0;
        }
        else {
            if (NAMEDCNT_GT_1(x) || x == val)
                REPROTECT(x = dup_top_level(x), pxidx);
            names = getNamesAttrib(x);
            nx = LENGTH(x);

            /* Set imatch to the index of the selected element, stays at
               -1 if not present.  Note that NA_STRING and "" don't match 
               anything. */

            if (names != R_NilValue && !na_or_empty_string(pname)) {
                for (int i = 0; i < nx; i++) {
                    SEXP ni = STRING_ELT(names, i);
                    if (SEQL(ni,pname) && !na_or_empty_string(ni)) {
                        imatch = i;
                        break;
                    }
                }
            }
        }

        if (val == R_NilValue) {
            /* If "val" is NULL, this is an element deletion if there
               is a match to "name" otherwise "x" is unchanged. */
            if (imatch >= 0)
                x = DeleteListElementsSeq (x, imatch+1, imatch+1);
            /* else x is unchanged */
        }
        else {
            /* If "val" is non-NULL, we are either replacing an existing 
               list element or we are adding a new element. */
	    if (imatch >= 0) {
		/* We are just replacing an element */
                DEC_NAMEDCNT (VECTOR_ELT(x,imatch));
		SET_VECTOR_ELEMENT_TO_VALUE(x, imatch, val);
	    }
	    else {
		SEXP ans, ansnames;
                if (x == R_NilValue)
                    PROTECT (ans = allocVector (VECSXP, 1));
                else
                    PROTECT (ans = reallocVector (x, nx+1, 1));
                if (names == R_NilValue || NAMEDCNT_GT_1(names)) {
                    PROTECT(ansnames = allocVector (STRSXP, nx+1));
                    if (names != R_NilValue)
                        copy_string_elements (ansnames, 0, names, 0, nx);
                }
                else {
                    PROTECT(ansnames = reallocVector (names, nx+1, 1));
                }
		SET_VECTOR_ELEMENT_TO_VALUE (ans, nx, val);
		SET_STRING_ELT (ansnames, nx, pname);
                no_dim_attributes(ans);
		setAttrib (ans, R_NamesSymbol, ansnames);
		UNPROTECT(2);
		x = ans;
	    }
	}
        break;
    }

    UNPROTECT(2);
    if(xS4 != R_NilValue)
	x = xS4; /* x was an env't, the data slot of xS4 */
    if (!isList(x)) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_subassign[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"[<-",		do_subassign,	0,	101000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	101000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	101000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
