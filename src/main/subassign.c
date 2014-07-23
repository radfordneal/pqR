/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
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
 */

/*		Warnings/Errors

    In this file we generally do not make use of the call, as it
    will be something like `[<-`(`*tmp`, ...) and that just confuses
    the user.  The call that is deduced from the context is generally
    much clearer.
 */

/*
 *  Regarding logical objects, note that we want to allow any assignment
 *  of the form "x[s] <- NA" (here NA is logical), and the interpreted
 *  "ifelse" requires assignment into a logical object.
 */

/*
 *  2000/02/17  Altered to allow closures/primitives in lists (VECSXPs) BDR
 */

/*
 *  2000/08/01  Also promises, expressions, environments when using [[ PD
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"
#include <R_ext/RS.h> /* for test of S4 objects */


/* EnlargeVector() takes a vector "x" and changes its length to "newlen".
   This allows to assign values "past the end" of the vector or list.
   Note that, unlike S, we only extend as much as is necessary.
*/
static SEXP EnlargeVector(SEXP x, R_len_t newlen)
{
    R_len_t i, len;
    SEXP newx, names, newnames;

    /* Sanity Checks */
    if (!isVector(x))
	error(_("attempt to enlarge non-vector"));

    /* Enlarge the vector itself. */
    len = LENGTH(x);
    if (LOGICAL(GetOption1(install("check.bounds")))[0])
	warning(_("assignment outside vector/list limits (extending from %d to %d)"),
		len, newlen);
    PROTECT(x);
    PROTECT(newx = allocVector(TYPEOF(x), newlen));

    if (isVectorList(x)) {
        /* should be OK to copy without adjusting NAMEDCNT (x won't be used) */
        copy_vector_elements (newx, 0, x, 0, len);
        /* elements after ones copied were set to R_NilValue by allocVector */
    }
    else {
        copy_elements (newx, 0, 1, x, 0, 1, len);
        set_elements_to_NA_or_NULL (newx, len, newlen-len);
    }

    /* Adjust the attribute list. */
    names = getAttrib(x, R_NamesSymbol);
    if (names != R_NilValue) {
	PROTECT(newnames = allocVector(STRSXP, newlen));
	for (i = 0; i < len; i++)
	    SET_STRING_ELT(newnames, i, STRING_ELT(names, i));
	for (i = len; i < newlen; i++)
	    SET_STRING_ELT(newnames, i, R_BlankString);
	setAttrib(newx, R_NamesSymbol, newnames);
	UNPROTECT(1);
    }
    copyMostAttrib(x, newx);
    UNPROTECT(2);
    return newx;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in wich coerceVector should
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

    const int type_x = TYPEOF(*x), 
              type_y = TYPEOF(*y);
    const int atom_x = isVectorAtomic(*x),
              atom_y = isVectorAtomic(*y);

    if (type_x == type_y || type_y == NILSXP) {
        /* nothing to do */
    }
    else if (atom_x && atom_y) { 
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
	error(_("incompatible types (from %s to %s) in subassignment type fix"),
	      type2char(type_y), type2char(type_x));
    }

    if (stretch) {
	PROTECT(*y);
	*x = EnlargeVector(*x, stretch);
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
    if (end > len) 
        end = len;
    if (start>end)
        return x;

    PROTECT(xnew = allocVector(TYPEOF(x), len-(end-start+1)));
    if (start>1) 
        copy_vector_elements (xnew, 0, x, 0, start-1);
    for (i = start; i <= end; i++)
        DEC_NAMEDCNT (VECTOR_ELT (x, i-1));
    if (end<len) 
        copy_vector_elements (xnew, start-1, x, end, len-end);

    xnames = getAttrib(x, R_NamesSymbol);
    if (xnames != R_NilValue) {
        PROTECT(xnewnames = allocVector(STRSXP, len-(end-start+1)));
        if (start>1) 
            copy_string_elements (xnewnames, 0, xnames, 0, start-1);
        if (end<len) 
            copy_string_elements (xnewnames, start-1, xnames, end, len-end);
        setAttrib(xnew, R_NamesSymbol, xnewnames);
        UNPROTECT(1);
    }

    copyMostAttrib(x, xnew);
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

    xnames = getAttrib(x, R_NamesSymbol);
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
    LOCAL_COPY(R_NilValue);
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
	    error(_("replacement has length zero"));
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

/* If "err" is 1, raise an error if any NAs are present in "indx", and
   otherwise return "indx" unchanged.  If "err" is 0, return an index
   vector (possibly newly allocated) with any NAs removed, updating "n"
   to its new length. */

static SEXP NA_check_remove (SEXP indx, int *n, int err)
{
    int ii, i;

    for (ii = 0; ii < *n && INTEGER(indx)[ii] != NA_INTEGER; ii++) ;

    if (ii < *n) {
        if (err)
            error(_("NAs are not allowed in subscripted assignments"));
        if (NAMEDCNT_GT_0(indx))
            indx = duplicate(indx);
        for (i = ii + 1 ; i < *n; i++) {
            if (INTEGER(indx)[i] != NA_INTEGER) {
                INTEGER(indx)[ii] = INTEGER(indx)[i];
                ii += 1;
            }
        }
        *n = ii;
    }

    return indx;
}

static SEXP VectorAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    LOCAL_COPY(R_NilValue);
    SEXP indx, newnames;
    int i, ii, iy, n, nx, ny, stretch;
    double ry;

    if (x==R_NilValue && y==R_NilValue)
	return R_NilValue;

    PROTECT(s);

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    if (isMatrix(s) && isArray(x)) {
        SEXP dim = getAttrib(x, R_DimSymbol);
        if (ncols(s) == LENGTH(dim)) {
            if (isString(s)) {
                s = strmat2intmat(s, GetArrayDimnames(x), call);
                UNPROTECT(1);
                PROTECT(s);
            }
            if (isInteger(s) || isReal(s)) {
                s = mat2indsub(dim, s, R_NilValue);
                UNPROTECT(1);
                PROTECT(s);
            }
        }
    }

    stretch = 1;
    int pindx;
    PROTECT_WITH_INDEX(indx = makeSubscript(x, s, &stretch, R_NilValue, 1), 
                       &pindx);
    n = length(indx);

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

    REPROTECT (indx = NA_check_remove (indx, &n, length(y) > 1), pindx);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (oldn > 0 && ny == 0)
	    error(_("replacement has length zero"));
	if (oldn > 0 && n % ny)
	    warning(_("number of items to replace is not a multiple of replacement length"));
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
	SEXP oldnames = getAttrib(x, R_NamesSymbol);
	if (oldnames != R_NilValue) {
	    for (i = 0; i < n; i++) {
		if (VECTOR_ELT(newnames, i) != R_NilValue) {
		    ii = INTEGER(indx)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
		}
	    }
	}
	else {
	    PROTECT(oldnames = allocVector(STRSXP, nx));
	    for (i = 0; i < nx; i++)
		SET_STRING_ELT(oldnames, i, R_BlankString);
	    for (i = 0; i < n; i++) {
		if (VECTOR_ELT(newnames, i) != R_NilValue) {
		    ii = INTEGER(indx)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
		}
	    }
	    setAttrib(x, R_NamesSymbol, oldnames);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(4);
    return x;
}

static SEXP MatrixAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, jj, ij, iy, k, n;
    double ry;
    int nr, ny;
    int nrs, ncs;
    SEXP sr, sc, dim;

    if (!isMatrix(x))
	error(_("incorrect number of subscripts on matrix"));

    nr = nrows(x);
    ny = LENGTH(y);

    /* Note that "s" has been protected. */
    /* No GC problems here. */

    dim = getAttrib(x, R_DimSymbol);
    sr = SETCAR(s, arraySubscript(0, CAR(s), dim, getAttrib,
				  (STRING_ELT), x));
    sc = SETCADR(s, arraySubscript(1, CADR(s), dim, getAttrib,
				   (STRING_ELT), x));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    n = nrs * ncs;

    SETCAR (s, sr = NA_check_remove (sr, &nrs, ny > 1));
    SETCADR (s, sc = NA_check_remove (sc, &ncs, ny > 1));

    if (n > 0 && ny == 0)
	error(_("replacement has length zero"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    SubassignTypeFix(&x, &y, 0, 1, call);
    if (n == 0) return x;

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

    k = 0;
    switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

    case (LGLSXP<<5) + LGLSXP:
    case (INTSXP<<5) + LGLSXP:
    case (INTSXP<<5) + INTSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		INTEGER(x)[ij] = INTEGER(y)[k];
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (REALSXP<<5) + LGLSXP:
    case (REALSXP<<5) + INTSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		iy = INTEGER(y)[k];
		if (iy == NA_INTEGER)
		    REAL(x)[ij] = NA_REAL;
		else
		    REAL(x)[ij] = iy;
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (REALSXP<<5) + REALSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] -1 ;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] -1 ;
		ij = ii + jj * nr;
		REAL(x)[ij] = REAL(y)[k];
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (CPLXSXP<<5) + LGLSXP:
    case (CPLXSXP<<5) + INTSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		iy = INTEGER(y)[k];
		if (iy == NA_INTEGER) {
		    COMPLEX(x)[ij].r = NA_REAL;
		    COMPLEX(x)[ij].i = NA_REAL;
		}
		else {
		    COMPLEX(x)[ij].r = iy;
		    COMPLEX(x)[ij].i = 0.0;
		}
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (CPLXSXP<<5) + REALSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		ry = REAL(y)[k];
		if (ISNA(ry)) {
		    COMPLEX(x)[ij].r = NA_REAL;
		    COMPLEX(x)[ij].i = NA_REAL;
		}
		else {
		    COMPLEX(x)[ij].r = ry;
		    COMPLEX(x)[ij].i = 0.0;
		}
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (CPLXSXP<<5) + CPLXSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		COMPLEX(x)[ij] = COMPLEX(y)[k];
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (STRSXP<<5) + STRSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		SET_STRING_ELT(x, ij, STRING_ELT(y, k));
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (RAWSXP<<5) + RAWSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
		RAW(x)[ij] = RAW(y)[k];
		if (++k == ny) k = 0;
	    }
	}
	break;

    case (EXPRSXP<<5) + VECSXP:
    case (EXPRSXP<<5) + EXPRSXP:
    case (VECSXP<<5)  + EXPRSXP:
    case (VECSXP<<5)  + VECSXP:
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j] - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i] - 1;
		ij = ii + jj * nr;
                if (k < ny) {
                    SET_VECTOR_ELEMENT_FROM_VECTOR(x, ij, y, k);
                }
                else {
                    SET_VECTOR_ELEMENT_FROM_VECTOR(x, ij, y, k % ny);
                    if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ij)))
                        SET_NAMEDCNT(VECTOR_ELT(x,ij),2);
                }
		k += 1;
	    }
	}
	break;

    default:
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }
    UNPROTECT(2);
    return x;
}


static SEXP ArrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, iy, jj, k=0, n, ny;
    int rep_assign = 0; /* 1 if elements assigned repeatedly into list array */
    SEXP dims, tmp;
    double ry;

    PROTECT(dims = getAttrib(x, R_DimSymbol));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	error(_("incorrect number of subscripts"));

    int *subs[k], indx[k], bound[k], offset[k];

    ny = LENGTH(y);

    n = 1;
    for (i = 0; i < k; i++) {
        PROTECT(tmp = arraySubscript (i,CAR(s),dims,getAttrib,(STRING_ELT),x));
        subs[i] = INTEGER(tmp);
	bound[i] = LENGTH(tmp);
        n *= bound[i];
        indx[i] = 0;
	s = CDR(s);
    }

    if (n > 0 && ny == 0)
	error(_("replacement has length zero"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    if (ny > 1) { /* check for NAs in indices */
	for (i = 0; i < k; i++)
	    for (j = 0; j < bound[i]; j++)
		if (subs[i][j] == NA_INTEGER)
		    error(_("NAs are not allowed in subscripted assignments"));
    }

    offset[1] = INTEGER(dims)[0];  /* offset[0] is not used */
    for (i = 2; i < k; i++)
        offset[i] = offset[i-1] * INTEGER(dims)[i-1];

    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    SubassignTypeFix(&x, &y, 0, 1, call);

    if (n == 0) {
	UNPROTECT(k+1);
	return(x);
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

    int which = (TYPEOF(x)<<5) + TYPEOF(y);

    i = 0;
    for (;;) {

        jj = subs[0][indx[0]];
        if (jj == NA_INTEGER) goto next;
	ii = jj-1;
	for (j = 1; j < k; j++) {
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) goto next;
	    ii += (jj-1) * offset[j];
	}

	switch (which) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP:
	    INTEGER(x)[ii] = INTEGER(y)[i];
	    break;

        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
	    iy = INTEGER(y)[i];
	    if (iy == NA_INTEGER)
		REAL(x)[ii] = NA_REAL;
	    else
		REAL(x)[ii] = iy;
	    break;

        case (REALSXP<<5) + REALSXP:
	    REAL(x)[ii] = REAL(y)[i];
	    break;

        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP:
	    iy = INTEGER(y)[i];
	    if (iy == NA_INTEGER) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = iy;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    break;

        case (CPLXSXP<<5) + REALSXP:
	    ry = REAL(y)[i];
	    if (ISNA(ry)) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = ry;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    break;

        case (CPLXSXP<<5) + CPLXSXP:
	    COMPLEX(x)[ii] = COMPLEX(y)[i];
	    break;

        case (STRSXP<<5) + STRSXP:
	    SET_STRING_ELT(x, ii, STRING_ELT(y, i));
	    break;

        case (RAWSXP<<5) + RAWSXP:
	    RAW(x)[ii] = RAW(y)[i];
	    break;

        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP:
            SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, i);
            if (!rep_assign) {
                if (i == ny - 1) 
                    rep_assign = 1;
            }
            else {
                if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ii)))
                    SET_NAMEDCNT(VECTOR_ELT(x,ii),2);
            }
	    break;

	default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
	}

      next:
        j = 0;
        while (++indx[j] >= bound[j]) {
            indx[j] = 0;
            if (++j >= k) goto done;
        }
        if (++i == ny) i = 0;
    }

  done:
    UNPROTECT(k+3);
    return x;
}

static void SubAssignArgs(SEXP args, SEXP *x, SEXP *s, SEXP *y)
{
    LOCAL_COPY(R_NilValue);

    *x = CAR(args); /* OK even if args is R_NilValue */
    if (args == R_NilValue || (args = CDR(args)) == R_NilValue)
	error(_("SubAssignArgs: invalid number of arguments"));

    if (CDR(args) == R_NilValue) {
	*s = R_NilValue;
	*y = CAR(args);
    }
    else {
        *s = args;
	while (CDDR(args) != R_NilValue)
	    args = CDR(args);
	*y = CADR(args);
	SETCDR(args, R_NilValue);
    }
}

/* The [<- operator.  "x" is the vector that is to be assigned into, 
   y is the vector that is going to provide the new values and subs is
   the vector of subscripts that are going to be replaced. 

   If the variant is VARIANT_MUST_COPY, copying is required regardless
   of NAMEDCNT. */

static SEXP do_subassign_dflt_seq 
              (SEXP call, SEXP op, SEXP args, SEXP rho, int variant, int seq);

static SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP ans, a1, a2, a3;
    int argsevald = 0;

    /* If we can easily determine that this will be handled by subset_dflt
       and has one index argument, evaluate the index with VARIANT_SEQ so 
       it may come as a range rather than a vector. */

    if (args != R_NilValue && CAR(args) != R_DotsSymbol 
         && (a2 = CDR(args)) != R_NilValue && CAR(a2) != R_DotsSymbol
         && (a3 = CDR(a2)) != R_NilValue && CDR(a3) == R_NilValue) {
        PROTECT(a1 = eval(CAR(args),rho));
	if (isObject(a1)) {
            args = CONS(a1,a2);
            UNPROTECT(1);
            argsevald = -1;
        }
        else if (TYPEOF(CAR(a2)) != LANGSXP) {
            /* ... in particular, it might be missing ... */
            args = CONS (a1, evalListKeepMissing(a2,rho));
            UNPROTECT(1);
            return do_subassign_dflt (call, op, args, rho, variant); 
        }
        else {
            int seq;
            PROTECT(a2 = evalv (CAR(a2), rho, VARIANT_SEQ));
            seq = R_variant_result;
            R_variant_result = 0;
            args = CONS (a1, CONS (a2, evalListKeepMissing (a3, rho)));
            UNPROTECT(2);
            return do_subassign_dflt_seq (call, op, args, rho, variant, seq); 
        }
    }

    /* This code performs an internal version of method dispatch. */
    /* We evaluate the first argument and attempt to dispatch on it. */
    /* If the dispatch fails, we "drop through" to the default code below. */

    if(DispatchOrEval(call, op, "[<-", args, rho, &ans, 0, argsevald))
        return(ans);

    return do_subassign_dflt(call, op, ans, rho, variant);
}

/* N.B.  do_subassign_dflt is called directly from elsewhere.  For role of
   variant, see above. */

SEXP attribute_hidden do_subassign_dflt
                        (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    return do_subassign_dflt_seq (call, op, args, rho, variant, 0);
}

/* The last "seq" argument below is 1 if the first subscript is a sequence spec
   (a variant result). */

static SEXP do_subassign_dflt_seq
              (SEXP call, SEXP op, SEXP args, SEXP rho, int variant, int seq)
{
    SEXP subs, x, y;

    PROTECT(args);

    SubAssignArgs(args, &x, &subs, &y);

    Rboolean S4 = IS_S4_OBJECT(x);
    int oldtype = NILSXP;

    if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP) {
	oldtype = TYPEOF(x);
	x = PairToVectorList(x);
    }
    else if (x == R_NilValue) {
	if (length(y) == 0) {
	    UNPROTECT(1);
	    return x;
	}
        x = coerceVector(x, TYPEOF(y));
    }
    else if (isVector(x)) {
        if (LENGTH(x) == 0) {
            if (length(y) == 0) {
                UNPROTECT(1);
                return x;
            }
	}
        else if (VARIANT_KIND(variant) == VARIANT_MUST_COPY || NAMEDCNT_GT_1(x))
            x = dup_top_level(x);
    }
    else
	error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    PROTECT(x);

    if (subs == R_NilValue) {
        /* 0 subscript arguments */
        x = VectorAssign(call, x, R_MissingArg, y);
    }
    else if (CDR(subs) == R_NilValue) { 
        /* 1 subscript argument */
        SEXP sub1 = CAR(subs);
        if (seq) {
            int start, end;
            sub1 = Rf_DecideVectorOrRange (sub1, &start, &end, call);
            if (sub1 == NULL) {
                R_len_t leny;
                if (start < 0 || y != R_NilValue && (leny = length(y)) != 1
                                                 && leny != end - start + 1)
                    sub1 = Rf_VectorFromRange (start, end);
                else
                    x = VectorAssignSeq (call, x, start, end, y);
            }
        }
        if (sub1 != NULL)
            x = VectorAssign (call, x, sub1, y);
    }
    else if (CDDR(subs) == R_NilValue) {
        /* 2 subscript arguments */
        x = MatrixAssign(call, x, subs, y);
    }
    else {
        /* More than 2 subscript arguments */
        x = ArrayAssign(call, x, subs, y);
    }

    if (oldtype == LANGSXP) {
	if (LENGTH(x)==0)
	    error(_("result is zero-length and so cannot be a language object"));
        x = VectorToPairList(x);
        SET_TYPEOF (x, LANGSXP);
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    UNPROTECT(2);
    if (x != R_NilValue) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}

static SEXP DeleteOneVectorListItem(SEXP x, int which)
{
    SEXP y, xnames, ynames;
    int n;
    n = length(x);
    if (0 <= which && which < n) {
	PROTECT(y = allocVector(TYPEOF(x), n-1));
        copy_vector_elements (y, 0, x, 0, which);
        copy_vector_elements (y, which, x, which+1, n-which-1);
        DEC_NAMEDCNT(VECTOR_ELT(x,which));
	xnames = getAttrib(x, R_NamesSymbol);
	if (xnames != R_NilValue) {
	    PROTECT(ynames = allocVector(STRSXP, n - 1));
            copy_string_elements (ynames, 0, xnames, 0, which);
            copy_string_elements (ynames, which, xnames, which+1, n-which-1);
	    setAttrib(y, R_NamesSymbol, ynames);
	    UNPROTECT(1);
	}
	copyMostAttrib(x, y);
	UNPROTECT(1);
	return y;
    }
    return x;
}

/* The [[<- operator; should be fast.
       ====
   args[1] = object being subscripted
   args[2] = list of subscripts
   args[3] = replacement values 

   If the variant is VARIANT_MUST_COPY, copying is required regardless
   of NAMEDCNT. */

static SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP ans;

    if(DispatchOrEval(call, op, "[[<-", args, rho, &ans, 0, 0))
/*     if(DispatchAnyOrEval(call, op, "[[<-", args, rho, &ans, 0, 0)) */
      return(ans);

    return do_subassign2_dflt(call, op, ans, rho, variant);
}

/* Also called directly from elsewhere.  For role of variant, see above. */

SEXP attribute_hidden do_subassign2_dflt
    (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    LOCAL_COPY(R_NilValue);

    SEXP dims, names, newname, subs, x, xtop, xup, y;
    int i, ndims, nsubs, offset, off = -1 /* -Wall */, stretch, len = 0 /* -Wall */;
    Rboolean S4, recursed;
    R_len_t length_x;

    SEXP thesub = R_NilValue, xOrig = R_NilValue;

    PROTECT(args);

    SubAssignArgs(args, &x, &subs, &y);
    S4 = IS_S4_OBJECT(x);

    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    nsubs = length(subs);

    /* Note: below, no duplication is necessary for environments. */

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xOrig = x; /* will be an S4 object */
        x = R_getS4DataSlot(x, ANYSXP);
	if(TYPEOF(x) != ENVSXP)
	  errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    /* ENVSXP special case first */
    if( TYPEOF(x) == ENVSXP) {
	if( nsubs!=1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	    error(_("wrong args for environment subassignment"));
	defineVar(install(translateChar(STRING_ELT(CAR(subs), 0))), y, x);
	UNPROTECT(1);
	return(S4 ? xOrig : x);
    }

    if (x == R_NilValue) {
        /* Handle NULL left-hand sides.  If the right-hand side is NULL,
           just return NULL, otherwise replace x by a zero length list 
           (VECSXP) or vector of type of y (if y of length one).  (This
           dependence on the length of y is of dubious wisdom!) */
	if (y == R_NilValue) {
	    UNPROTECT(1);
	    return R_NilValue;
	}
	if (length(y) == 1)
	    x = allocVector(TYPEOF(y), 0);
	else
	    x = allocVector(VECSXP, 0);
    }
    else if (isPairList(x))
        x = duplicate(x);
    else if (isVectorList(x)) {
        if (VARIANT_KIND(variant) == VARIANT_MUST_COPY 
         || NAMEDCNT_GT_1(x) || x == y)
            x = dup_top_level(x);
    }

    xtop = xup = x; /* x will contain the element which is assigned to; */
                    /*   xup may contain x; xtop is what is returned.  */ 
    PROTECT(xtop);

    recursed = FALSE;

    if (nsubs == 1) { /* One vector index for a list. */
	thesub = CAR(subs);
	len = length(thesub);
        if (len > 1) {
            for (int i = 0; i < len-1; i++) {
                if (!isVectorList(x) && !isPairList(x))
                    errorcall (call, 
                      _("recursive indexing failed at level %d\n"), i+1);
                length_x = length(x);
                off = get1index (thesub, getAttrib(x, R_NamesSymbol),
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
                    else if (isVectorList(x) && (NAMEDCNT_GT_1(x) ||
                               VARIANT_KIND(variant) == VARIANT_MUST_COPY)) {
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
	    error(_("replacement has length zero"));
	if (!isVectorList(x) && length_y > 1)
	    error(_("more elements supplied than there are to replace"));
	if (nsubs == 0 || CAR(subs) == R_MissingArg)
	    error(_("[[ ]] with missing subscript"));
    }

    stretch = 0;
    newname = R_NilValue;

    length_x = length(x);

    if (nsubs == 1) {
        offset = get1index (thesub, getAttrib(x,R_NamesSymbol), length_x, 
                            FALSE, (recursed ? len-1 : -1), R_NilValue);
        if (offset < 0) {
            if (isString(thesub) || isSymbol(thesub))
                offset = length_x;
            else
                error(_("[[ ]] subscript out of bounds"));
        }
        if (offset >= length_x) {
            stretch = offset + 1;
            newname = isString(thesub) ? STRING_ELT(thesub,len-1)
                    : isSymbol(thesub) ? PRINTNAME(thesub) : R_NilValue;
        }
    }
    else {
        if (ndims != nsubs)
            error(_("[[ ]] improper number of subscripts"));
        names = getAttrib(x, R_DimNamesSymbol);
        offset = 0;
        for (i = ndims-1; i >= 0; i--) {
            R_len_t ix = get1index (CAR(nthcdr(subs,i)),
                           names==R_NilValue ? R_NilValue : VECTOR_ELT(names,i),
                           INTEGER(dims)[i],/*partial ok*/ FALSE, -1, call);
            if (ix < 0 || ix >= INTEGER(dims)[i])
                error(_("[[ ]] subscript out of bounds"));
            offset += ix;
            if (i > 0) offset *= INTEGER(dims)[i-1];
        }
    }

    if (isVector(x)) {

        if (nsubs == 1 && isVectorList(x) && y == R_NilValue) {
            PROTECT(x = DeleteOneVectorListItem(x, offset));
        }
        else {

            SubassignTypeFix(&x, &y, stretch, 2, call);
    
            if (VARIANT_KIND(variant) == VARIANT_MUST_COPY
             || NAMEDCNT_GT_1(x) || x == y)
                x = dup_top_level(x);
    
            PROTECT(x);
    
            if (isVectorAtomic(x))
                copy_elements_coerced (x, offset, 0, y, 0, 0, 1);
            else if (isVectorList(x)) {
                DEC_NAMEDCNT (VECTOR_ELT(x, offset));
                SET_VECTOR_ELEMENT_TO_VALUE (x, offset, y);
            }
            else
                error(_("incompatible types (from %s to %s) in [[ assignment"),
                      type2char(TYPEOF(y)), type2char(TYPEOF(x)));
    
            /* If we stretched, we may have a new name. */
            /* In this case we must create a names attribute */
            /* (if it doesn't already exist) and set the new */
            /* value in the names attribute. */
            if (stretch && newname != R_NilValue) {
                names = getAttrib(x, R_NamesSymbol);
                if (names == R_NilValue) {
                    PROTECT(names = allocVector(STRSXP, LENGTH(x)));
                    SET_STRING_ELT(names, offset, newname);
                    setAttrib(x, R_NamesSymbol, names);
                    UNPROTECT(1);
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
        PROTECT(x);
    }

    else 
        error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));

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

    if (xtop != R_NilValue) SET_NAMEDCNT_0(xtop);
    if(S4) SET_S4_OBJECT(xtop);

    UNPROTECT(3);
    return xtop;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string

   If the variant is VARIANT_MUST_COPY, copying is required regardless
   of NAMEDCNT. */

static SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP name, ans, input;
    int iS;

    checkArity(op, args);

    /* Note the RHS has already been evaluated at this point */

    input = allocVector(STRSXP, 1);

    name = CADR(args);
    iS = isSymbol(name);
    if (iS)
	SET_STRING_ELT(input, 0, PRINTNAME(name));
    else if(isString(name) )
	SET_STRING_ELT(input, 0, STRING_ELT(name, 0));
    else {
	error(_("invalid subscript type '%s'"), type2char(TYPEOF(name)));
	return R_NilValue; /*-Wall*/
    }

    /* replace the second argument with a string */
    SETCADR(args, input);

    if(DispatchOrEval(call, op, "$<-", args, env, &ans, 0, 0))
        return(ans);

    if (!iS)
	name = install(translateChar(STRING_ELT(input, 0)));

    return R_subassign3_dflt(call, CAR(ans), name, CADDR(ans), variant);
}

/* Also called directly from elsewhere.  For role of variant, see above. */

SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP name, SEXP val, int variant)
{
    LOCAL_COPY(R_NilValue);

    PROTECT_INDEX pvalidx, pxidx;
    Rboolean S4; SEXP xS4 = R_NilValue;

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

    if ((isList(x) || isLanguage(x)) && x != R_NilValue) {

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
    }

    /* cannot use isEnvironment since we do not want NULL here */
    else if( TYPEOF(x) == ENVSXP ) {

	set_var_in_frame (name, val, x, TRUE, 3);

    }

    else if( TYPEOF(x) == SYMSXP || /* Used to 'work' in R < 2.8.0 */
	     TYPEOF(x) == CLOSXP ||
	     TYPEOF(x) == SPECIALSXP ||
	     TYPEOF(x) == BUILTINSXP) {
	error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));
    }

    else {
        int type = VECSXP;

	if (isExpression(x)) 
	    type = EXPRSXP;
	else if (!isNewList(x)) {
	    warning(_("Coercing LHS to a list"));
	    REPROTECT(x = coerceVector(x, VECSXP), pxidx);
	}

        if (VARIANT_KIND(variant) == VARIANT_MUST_COPY 
         || NAMEDCNT_GT_1(x) || x == val)
            REPROTECT(x = dup_top_level(x), pxidx);

        SEXP pname = PRINTNAME(name);
	SEXP names = getAttrib(x, R_NamesSymbol);
	R_len_t nx = length(x);  /* x could be R_NilValue */

        /* set imatch to the index of the selected element, -1 if not present */

        int imatch = -1;
        if (names != R_NilValue) {
            for (int i = 0; i < nx; i++) {
                if (NonNullStringMatch(STRING_ELT(names, i), pname)) {
                    imatch = i;
                    break;
                }
            }
        }

        if (val == R_NilValue) {
            /* If "val" is NULL, this is an element deletion */
            /* if there is a match to "name" otherwise "x" */
            /* is unchanged.  The attributes need adjustment. */
            if (imatch >= 0) {
                SEXP ans, ansnames;
                PROTECT(ans = allocVector(type, nx - 1));
                PROTECT(ansnames = allocVector(STRSXP, nx - 1));
                DEC_NAMEDCNT (VECTOR_ELT(x,imatch));
                if (imatch > 0) {
                    copy_vector_elements (ans, 0, x, 0, imatch);
                    copy_string_elements (ansnames, 0, names, 0, imatch);
                }
                if (imatch+1 < nx) {
                    copy_vector_elements (ans, imatch, x, imatch+1, 
                                          nx-imatch-1);
                    copy_string_elements (ansnames, imatch, names, imatch+1,
                                          nx-imatch-1);
                }
                setAttrib(ans, R_NamesSymbol, ansnames);
                copyMostAttrib(x, ans);
                UNPROTECT(2);
                x = ans;
            }
            /* else x is unchanged */
        }
        else {
	    /* If "val" is non-NULL, we are either replacing */
	    /* an existing list element or we are adding a new */
	    /* element. */
	    if (imatch >= 0) {
		/* We are just replacing an element */
                DEC_NAMEDCNT (VECTOR_ELT(x,imatch));
		SET_VECTOR_ELEMENT_TO_VALUE(x, imatch, val);
	    }
	    else {
		/* We are introducing a new element.
		   Enlarge the list, add the new element,
		   and finally, adjust the attributes. */
		SEXP ans, ansnames;
		PROTECT(ans = allocVector(type, nx + 1));
		PROTECT(ansnames = allocVector(STRSXP, nx + 1));
                copy_vector_elements (ans, 0, x, 0, nx);
		if (names == R_NilValue)
		    for (int i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, R_BlankString);
		else
                    copy_string_elements (ansnames, 0, names, 0, nx);
		SET_VECTOR_ELEMENT_TO_VALUE(ans, nx, val);
		SET_STRING_ELT(ansnames, nx, pname);
		setAttrib(ans, R_NamesSymbol, ansnames);
		copyMostAttrib(x, ans);
		UNPROTECT(2);
		x = ans;
	    }
	}
    }

    UNPROTECT(2);
    if(xS4 != R_NilValue)
	x = xS4; /* x was an env't, the data slot of xS4 */
    if (x != R_NilValue) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_subassign[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"[<-",		do_subassign,	0,	1000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	1000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	1000,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
