/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Core Team
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
#include <errno.h>

#include "static-boxes.h"  /* for inline static_box_eval2 function */


static SEXP integer_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP real_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP real_relop_and(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP real_relop_or(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP string_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP string_relop_and(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP string_relop_or(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP raw_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP raw_relop_and(RELOP_TYPE code, int F, SEXP s1, SEXP s2);
static SEXP raw_relop_or(RELOP_TYPE code, int F, SEXP s1, SEXP s2);

SEXP attribute_hidden R_relop (SEXP call, SEXP op, SEXP x, SEXP y, 
                               int objx, int objy, SEXP env, int variant)
{
    SEXP klass = R_NilValue;
    SEXP tsp = R_NilValue;
    SEXP xnames, ynames, tmp, ans, dims;
    int xarray, yarray, xts, yts, itmp;
    Rboolean mismatch = FALSE, iS;
    PROTECT_INDEX xpi, ypi;

    /* Reduce operation codes to EQOP and LTOP by swapping and negating. */

    RELOP_TYPE code = (RELOP_TYPE) PRIMVAL(op);
    int negate = 0;

    switch (code) {
    case NEOP: 
        code = EQOP; 
        negate = 1; 
        break; 
    case GTOP: 
        code = LTOP; 
        tmp = x; x = y; y = tmp; 
        itmp = objx; objx = objy; objy = itmp; 
        break;
    case LEOP:
        code = LTOP;
        negate = 1;
        tmp = x; x = y; y = tmp;
        itmp = objx; objx = objy; objy = itmp; 
        break;
    case GEOP:
        code = LTOP;
        negate = 1;
        break;
    default:
        break;
    }

    WAIT_UNTIL_COMPUTED_2(x,y);

    /* Get types and lengths of operands.  Pretend that logical is
       integer, as they have the same representation. */

    SEXPTYPE typeof_x = TYPEOF(x);
    SEXPTYPE typeof_y = TYPEOF(y);

    if (typeof_x == LGLSXP) typeof_x = INTSXP;
    if (typeof_y == LGLSXP) typeof_y = INTSXP;

    int nx = (ATOMIC_VECTOR_TYPES >> typeof_x) & 1 ? LENGTH(x) : length(x);
    int ny = (ATOMIC_VECTOR_TYPES >> typeof_y) & 1 ? LENGTH(y) : length(y);

    /* Handle integer/logical/real vectors that have no attributes quickly. */ 

    if ((typeof_x == REALSXP || typeof_x == INTSXP)
          && (typeof_y == REALSXP || typeof_y == INTSXP)
          && !HAS_ATTRIB(x) && !HAS_ATTRIB(y) 
          && nx > 0 && ny > 0) {

        /* Handle scalars even quicker using ScalarLogicalMaybeConst. */
    
        if (nx==1 && ny==1) {

            /* Assumes integers can be represented to full precision as reals */

            double x1 = typeof_x == REALSXP ? REAL(x)[0]
               : INTEGER(x)[0]!=NA_INTEGER ? INTEGER(x)[0] : NA_REAL;

            double y1 = typeof_y == REALSXP ? REAL(y)[0]
               : INTEGER(y)[0]!=NA_INTEGER ? INTEGER(y)[0] : NA_REAL;

            int result = ISNAN(x1) || ISNAN(y1) ? NA_LOGICAL
                       : code == EQOP ? x1 == y1 : /* LTOP */ x1 < y1;

            return ScalarLogicalMaybeConst (negate && result != NA_LOGICAL 
                                             ? !result : result);
        } 
        else {
            PROTECT2(x,y);
            if (((nx > ny) ? nx % ny : ny % nx) != 0) {
 	            warningcall (call,
          _("longer object length is not a multiple of shorter object length"));
            }
            if (typeof_x == INTSXP && typeof_y == INTSXP) 
                ans = integer_relop (code, negate, x, y);
            else {
                if (typeof_x == INTSXP) {
                    x = coerceVector(x,REALSXP);
                    UNPROTECT(2);
                    PROTECT2(x,y);
                }
                if (typeof_y == INTSXP) {
                    y = coerceVector(y,REALSXP);
                    UNPROTECT(2);
                    PROTECT2(x,y);
                }
                switch (VARIANT_KIND(variant)) {
                case VARIANT_AND: 
                    ans = real_relop_and (code, negate, x, y); 
                    break;
                case VARIANT_OR:
                    ans = real_relop_or (code, negate, x, y);
                    break;
                default:
                    ans = real_relop (code, negate, x, y);
                    break;
                }
            }
            UNPROTECT(2);
            return ans;
        }
    }
    
    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    /* That symbols and calls were allowed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    if ((iS = isSymbol(x)) || typeof_x == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) :
		       STRING_ELT(deparse1(x, 0, DEFAULTDEPARSE), 0));
	REPROTECT(x = tmp, xpi);
	UNPROTECT(1);
    }
    if ((iS = isSymbol(y)) || typeof_y == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
		       STRING_ELT(deparse1(y, 0, DEFAULTDEPARSE), 0));
	REPROTECT(y = tmp, ypi);
	UNPROTECT(1);
    }

    if (!isVector(x) || !isVector(y)) {
	if (isNull(x) || isNull(y)) {
	    UNPROTECT(2);
	    return allocVector(LGLSXP,0);
	}
	errorcall(call,
	  _("comparison (%d) is possible only for atomic and list types"),
	  PRIMVAL(op));
    }

    if (typeof_x == EXPRSXP || typeof_y == EXPRSXP)
	errorcall(call, _("comparison is not allowed for expressions"));

    /* ELSE :  x and y are both atomic or list */

    if (LENGTH(x) <= 0 || LENGTH(y) <= 0) {
	UNPROTECT(2);
	return allocVector(LGLSXP,0);
    }

    mismatch = FALSE;
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (nx > 0 && ny > 0)
	mismatch = ((nx > ny) ? nx % ny : ny % nx) != 0;

    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, _("non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else /*(yarray)*/ {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }
    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, _("non-conformable time series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = !objx ? R_NilValue : getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = !objx ? R_NilValue : getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = !objy ? R_NilValue : getAttrib(y, R_ClassSymbol));
	}
    }
    if (mismatch)
	warningcall(call, _("longer object length is not a multiple of shorter object length"));

    if (isString(x) || isString(y)) {
	REPROTECT(x = coerceVector(x, STRSXP), xpi);
	REPROTECT(y = coerceVector(y, STRSXP), ypi);
        switch (VARIANT_KIND(variant)) {
        case VARIANT_AND: 
            ans = string_relop_and (code, negate, x, y); 
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        case VARIANT_OR:
            ans = string_relop_or (code, negate, x, y);
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        default:
            ans = string_relop (code, negate, x, y);
            break;
        }
    }
    else if (isComplex(x) || isComplex(y)) {
	REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
	REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
        if (code != EQOP)
	    errorcall(call, _("invalid comparison with complex values"));
	ans = complex_relop (code, negate, x, y);
    }
    else if (isReal(x) || isReal(y)) {
	REPROTECT(x = coerceVector(x, REALSXP), xpi);
	REPROTECT(y = coerceVector(y, REALSXP), ypi);
        switch (VARIANT_KIND(variant)) {
        case VARIANT_AND: 
            ans = real_relop_and (code, negate, x, y); 
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        case VARIANT_OR:
            ans = real_relop_or (code, negate, x, y);
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        default:
            ans = real_relop (code, negate, x, y);
            break;
        }
    }
    else if (typeof_x == INTSXP || typeof_y == INTSXP) {
        /* NOT isInteger, since it needs to be true for factors with
           VARIANT_UNCLASS_FLAG.  Assumes LOGICAL same as INTEGER. */
	REPROTECT(x = coerceVector(x, INTSXP), xpi);
	REPROTECT(y = coerceVector(y, INTSXP), ypi);
	ans = integer_relop (code, negate, x, y);
    }
    else if (typeof_x == RAWSXP || typeof_y == RAWSXP) {
	REPROTECT(x = coerceVector(x, RAWSXP), xpi);
	REPROTECT(y = coerceVector(y, RAWSXP), ypi);
	ans = raw_relop (code, negate, x, y);
    }
    else 
        errorcall(call, _("comparison of these types is not implemented"));


    PROTECT(ans);
    if (dims != R_NilValue) {
	setAttrib(ans, R_DimSymbol, dims);
	if (xnames != R_NilValue)
	    setAttrib(ans, R_DimNamesSymbol, xnames);
	else if (ynames != R_NilValue)
	    setAttrib(ans, R_DimNamesSymbol, ynames);
    }
    else {
	if (length(ans) == length(xnames))
	    setAttrib(ans, R_NamesSymbol, xnames);
	else if (length(ans) == length(ynames))
	    setAttrib(ans, R_NamesSymbol, ynames);
    }
    if (xts || yts) {
	setAttrib(ans, R_TspSymbol, tsp);
	setAttrib(ans, R_ClassSymbol, klass);
	UNPROTECT(2);
    }

    UNPROTECT(6);
    return ans;
}

static SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP argsevald, ans, x, y;
    int objx, objy;

    /* Evaluate arguments, maybe putting them in static boxes. */

    PROTECT(argsevald = 
      static_box_eval2 (args, &x, &y, &objx, &objy, env, call, variant));
    PROTECT2(x,y);

    /* Check for dispatch on S3 or S4 objects. */

    if (objx || objy) {
        if (DispatchGroup("Ops", call, op, argsevald, env, &ans)) {
            UNPROTECT(3);
            return ans;
        }
    }

    /* Check argument count now (after dispatch, since other methods may allow
       other argument count). */

    checkArity(op,argsevald);

    /* Arguments are now in x and y, and are protected.  They may be static
       boxes. */

    ans = R_relop (call, op, x, y, objx, objy, env, variant);

    UNPROTECT(3);
    return ans;
}

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

#define RELOP_MACRO(FETCH,NANCHK1,NANCHK2,EQCMP,LTCMP) do { \
 \
    int i, i1, i2, n, n1, n2; \
    int T = !F; \
    SEXP ans; \
 \
    n1 = LENGTH(s1); \
    n2 = LENGTH(s2); \
    n = (n1 > n2) ? n1 : n2; \
    PROTECT (ans = allocVector(LGLSXP, n)); \
 \
    if (code == EQOP) { \
        if (n2 == 1) { \
            x2 = FETCH(s2,0); \
            if (NANCHK2) \
                for (i = 0; i<n; i++) LOGICAL(ans)[i] = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x1 = FETCH(s1,i); \
                    LOGICAL(ans)[i] = \
                      NANCHK1 ? NA_LOGICAL : EQCMP ? T : F; \
                } \
        } \
        else if (n1 == 1) { \
            x1 = FETCH(s1,0); \
            if (NANCHK1) \
                for (i = 0; i<n; i++) LOGICAL(ans)[i] = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x2 = FETCH(s2,i); \
                    LOGICAL(ans)[i] = \
                      NANCHK2 ? NA_LOGICAL : EQCMP ? T : F; \
                } \
        } \
        else if (n1 == n2) { \
            for (i = 0; i<n; i++) { \
	        x1 = FETCH(s1,i); \
                x2 = FETCH(s2,i); \
                LOGICAL(ans)[i] = NANCHK1 || NANCHK2 ? NA_LOGICAL \
	                        : EQCMP ? T : F; \
            } \
        } \
        else { \
	    mod_iterate(n1, n2, i1, i2) { \
	        x1 = FETCH(s1,i1); \
                x2 = FETCH(s2,i2); \
                LOGICAL(ans)[i] = NANCHK1 || NANCHK2 ? NA_LOGICAL \
	                        : EQCMP ? T : F; \
            } \
	} \
    } \
    else { /* LTOP */ \
        if (n2 == 1) { \
            x2 = FETCH(s2,0); \
            if (NANCHK2) \
                for (i = 0; i<n; i++) LOGICAL(ans)[i] = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x1 = FETCH(s1,i); \
                    LOGICAL(ans)[i] = \
                      NANCHK1 ? NA_LOGICAL : LTCMP ? T : F; \
                } \
        } \
        else if (n1 == 1) { \
            x1 = FETCH(s1,0); \
            if (NANCHK1) \
                for (i = 0; i<n; i++) LOGICAL(ans)[i] = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x2 = FETCH(s2,i); \
                    LOGICAL(ans)[i] = \
                      NANCHK2 ? NA_LOGICAL : LTCMP ? T : F; \
                } \
        } \
        else if (n1 == n2) { \
            for (i = 0; i<n; i++) { \
	        x1 = FETCH(s1,i); \
                x2 = FETCH(s2,i); \
                LOGICAL(ans)[i] = NANCHK1 || NANCHK2 ? NA_LOGICAL \
	                        : LTCMP ? T : F; \
            } \
        } \
        else { \
	    mod_iterate(n1, n2, i1, i2) { \
	        x1 = FETCH(s1,i1); \
                x2 = FETCH(s2,i2); \
                LOGICAL(ans)[i] = NANCHK1 || NANCHK2 ? NA_LOGICAL \
	                        : LTCMP ? T : F; \
            } \
	} \
    } \
 \
    UNPROTECT(1); \
    return ans; \
 \
} while (0)

#define RELOP_AND_MACRO(FETCH,NANCHK1,NANCHK2,EQCMP,LTCMP) do { \
 \
    int i, i1, i2, n, n1, n2; \
    int T = !F; \
    int ans; \
 \
    n1 = LENGTH(s1); \
    n2 = LENGTH(s2); \
    n = (n1 > n2) ? n1 : n2; \
 \
    ans = TRUE; \
 \
    if (code == EQOP) { \
        if (n2 == 1) { \
            x2 = FETCH(s2,0); \
            if (NANCHK2) \
                ans = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x1 = FETCH(s1,i); \
                    if (NANCHK1) \
                        ans = NA_LOGICAL; \
                    else if (EQCMP ? F : T) \
                        goto false; \
                } \
        } \
        else if (n1 == 1) { \
            x1 = FETCH(s1,0); \
            if (NANCHK1) \
                ans = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x2 = FETCH(s2,i); \
                    if (NANCHK2) \
                        ans = NA_LOGICAL; \
                    else if (EQCMP ? F : T) \
                        goto false; \
                } \
        } \
        else if (n1 == n2) { \
            for (i = 0; i<n; i++) { \
	        x1 = FETCH(s1,i); \
                x2 = FETCH(s2,i); \
                if (NANCHK1 || NANCHK2) \
                    ans = NA_LOGICAL; \
                else if (EQCMP ? F : T) \
                    goto false; \
            } \
        } \
        else { \
	    mod_iterate(n1, n2, i1, i2) { \
	        x1 = FETCH(s1,i1); \
                x2 = FETCH(s2,i2); \
                if (NANCHK1 || NANCHK2) \
                    ans = NA_LOGICAL; \
                else if (EQCMP ? F : T) \
                    goto false; \
            } \
	} \
    } \
    else { /* LTOP */ \
        if (n2 == 1) { \
            x2 = FETCH(s2,0); \
            if (NANCHK2) \
                ans = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x1 = FETCH(s1,i); \
                    if (NANCHK1) \
                        ans = NA_LOGICAL; \
                    else if (LTCMP ? F : T) \
                        goto false; \
                } \
        } \
        else if (n1 == 1) { \
            x1 = FETCH(s1,0); \
            if (NANCHK1) \
                ans = NA_LOGICAL; \
            else \
                for (i = 0; i<n; i++) { \
                    x2 = FETCH(s2,i); \
                    if (NANCHK2) \
                        ans = NA_LOGICAL; \
                    else if (LTCMP ? F : T) \
                        goto false; \
                } \
        } \
        else if (n1 == n2) { \
            for (i = 0; i<n; i++) { \
	        x1 = FETCH(s1,i); \
                x2 = FETCH(s2,i); \
                if (NANCHK1 || NANCHK2) \
                    ans = NA_LOGICAL; \
                else if (LTCMP ? F : T) \
                    goto false; \
            } \
        } \
        else { \
	    mod_iterate(n1, n2, i1, i2) { \
	        x1 = FETCH(s1,i1); \
                x2 = FETCH(s2,i2); \
                if (NANCHK1 || NANCHK2) \
                    ans = NA_LOGICAL; \
                else if (LTCMP ? F : T) \
                    goto false; \
            } \
	} \
    } \
 \
    return ScalarLogicalMaybeConst(ans); \
 \
  false: \
    return ScalarLogicalMaybeConst(FALSE); \
 \
} while (0)


static SEXP integer_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int x1, x2;

#   define INT_FETCH(s,i) (INTEGER(s)[i])

    RELOP_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1==x2, x1<x2);
}

static SEXP real_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    double x1, x2;

#   define REAL_FETCH(s,i) (REAL(s)[i])

    RELOP_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1==x2, x1<x2);
}

static SEXP real_relop_and(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    double x1, x2;
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = TRUE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = REAL(s2)[0];
            if (ISNAN(x2)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x1 = REAL(s1)[i];
                    if (ISNAN(x1)) 
                        ans = NA_LOGICAL;
                    else if (x1 == x2 ? F : T) 
                        goto false;
                }
        }
        else if (n1 == 1) {
            x1 = REAL(s1)[0];
            if (ISNAN(x1)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x2 = REAL(s2)[i];
                    if (ISNAN(x2)) 
                        ans = NA_LOGICAL;
                    else if (x1 == x2 ? F : T) 
                        goto false;
                }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = REAL(s1)[i];
                x2 = REAL(s2)[i];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 == x2 ? F : T) 
                    goto false;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = REAL(s1)[i1];
                x2 = REAL(s2)[i2];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 == x2 ? F : T) 
                    goto false;
            }
	}
    }
    else { /* LTOP */
        if (n2 == 1) {
            x2 = REAL(s2)[0];
            if (ISNAN(x2)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x1 = REAL(s1)[i];
                    if (ISNAN(x1)) 
                        ans = NA_LOGICAL;
                    else if (x1 < x2 ? F : T) 
                        goto false;
                }
        }
        else if (n1 == 1) {
            x1 = REAL(s1)[0];
            if (ISNAN(x1)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x2 = REAL(s2)[i];
                    if (ISNAN(x2)) 
                        ans = NA_LOGICAL;
                    else if (x1 < x2 ? F : T) 
                        goto false;
                }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = REAL(s1)[i];
                x2 = REAL(s2)[i];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 < x2 ? F : T) 
                    goto false;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = REAL(s1)[i1];
                x2 = REAL(s2)[i2];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 < x2 ? F : T) 
                    goto false;
            }
	}
    }

    return ScalarLogicalMaybeConst(ans);

false:
    return ScalarLogicalMaybeConst(FALSE);
}

static SEXP real_relop_or(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    double x1, x2;
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = FALSE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = REAL(s2)[0];
            if (ISNAN(x2)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x1 = REAL(s1)[i];
                    if (ISNAN(x1)) 
                        ans = NA_LOGICAL;
                    else if (x1 == x2 ? T : F) 
                        goto true;
                }
        }
        else if (n1 == 1) {
            x1 = REAL(s1)[0];
            if (ISNAN(x1)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x2 = REAL(s2)[i];
                    if (ISNAN(x2)) 
                        ans = NA_LOGICAL;
                    else if (x1 == x2 ? T : F) 
                        goto true;
                }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = REAL(s1)[i];
                x2 = REAL(s2)[i];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 == x2 ? T : F) 
                    goto true;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = REAL(s1)[i1];
                x2 = REAL(s2)[i2];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 == x2 ? T : F) 
                    goto true;
            }
	}
    }
    else { /* LTOP */
        if (n2 == 1) {
            x2 = REAL(s2)[0];
            if (ISNAN(x2)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x1 = REAL(s1)[i];
                    if (ISNAN(x1)) 
                        ans = NA_LOGICAL;
                    else if (x1 < x2 ? T : F) 
                        goto true;
                }
        }
        else if (n1 == 1) {
            x1 = REAL(s1)[0];
            if (ISNAN(x1)) 
                ans = NA_LOGICAL;
            else 
                for (i = 0; i<n; i++) {
                    x2 = REAL(s2)[i];
                    if (ISNAN(x2)) 
                        ans = NA_LOGICAL;
                    else if (x1 < x2 ? T : F) 
                        goto true;
                }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = REAL(s1)[i];
                x2 = REAL(s2)[i];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 < x2 ? T : F) 
                    goto true;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = REAL(s1)[i1];
                x2 = REAL(s2)[i2];
                if (ISNAN(x1) || ISNAN(x2)) 
                    ans = NA_LOGICAL;
                else if (x1 < x2 ? T : F) 
                    goto true;
            }
	}
    }

    return ScalarLogicalMaybeConst(ans);

true:
    return ScalarLogicalMaybeConst(TRUE);
    
}

static SEXP complex_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    Rcomplex x1, x2;
    int T = !F;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    ans = allocVector(LGLSXP, n);

    /* Only handles EQOP */

    mod_iterate(n1, n2, i1, i2) {
        x1 = COMPLEX(s1)[i1];
        x2 = COMPLEX(s2)[i2];
        LOGICAL(ans)[i] = 
         ISNAN(x1.r) || ISNAN(x1.i) || ISNAN(x2.r) || ISNAN(x2.i) ? NA_LOGICAL
          : x1.r == x2.r && x1.i == x2.i ? T : F;
    }

    return ans;
}


static SEXP string_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2, res;
    SEXP ans, x1, x2;
    int T = !F;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = STRING_ELT(s2,0);
            for (i = 0; i<n; i++) {
                x1 = STRING_ELT(s1,i);
                LOGICAL(ans)[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                                : SEQL(x1, x2) ? T : F;
            }
        }
        else if (n1 == 1) {
            x1 = STRING_ELT(s1,0);
            for (i = 0; i<n; i++) {
                x2 = STRING_ELT(s2,i);
                LOGICAL(ans)[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                                : SEQL(x1, x2) ? T : F;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = STRING_ELT(s1,i);
                x2 = STRING_ELT(s2,i);
                LOGICAL(ans)[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                                : SEQL(x1, x2) ? T : F;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = STRING_ELT(s1,i1);
                x2 = STRING_ELT(s2,i2);
                LOGICAL(ans)[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                                : SEQL(x1, x2) ? T : F;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = STRING_ELT(s1, i % n1);
	    x2 = STRING_ELT(s2, i % n2);
	    if (x1 == NA_STRING || x2 == NA_STRING)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else if (x1 == x2)
		LOGICAL(ans)[i] = F;
	    else {
                /* POSIX allows EINVAL when one of the strings contains
                   characters outside the collation domain. */
		errno = 0;
		res = Scollate(x1, x2);
		LOGICAL(ans)[i] = errno ? NA_LOGICAL : res < 0 ? T : F;
	    }
	}
    }

    UNPROTECT(1);
    return ans;
}


static SEXP string_relop_and(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2, res;
    SEXP x1, x2;
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = TRUE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = STRING_ELT(s2,0);
            if (x2 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x1 = STRING_ELT(s1,i);
                    if (x1==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? F : T)
                        goto false;
            }
        }
        else if (n1 == 1) {
            x1 = STRING_ELT(s1,0);
            if (x1 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x2 = STRING_ELT(s2,i);
                    if (x2==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? F : T)
                        goto false;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = STRING_ELT(s1,i);
                x2 = STRING_ELT(s2,i);
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? F : T)
                    goto false;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = STRING_ELT(s1,i1);
                x2 = STRING_ELT(s2,i2);
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? F : T)
                    goto false;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = STRING_ELT(s1, i % n1);
	    x2 = STRING_ELT(s2, i % n2);
	    if (x1 == NA_STRING || x2 == NA_STRING)
		ans = NA_LOGICAL;
	    else if (x1 == x2)
		goto false;
	    else {
                /* POSIX allows EINVAL when one of the strings contains
                   characters outside the collation domain. */
		errno = 0;
		res = Scollate(x1, x2);
                if (errno)
                    ans = NA_LOGICAL;
                else if (res < 0 ? F : T)
                    goto false;
	    }
	}
    }

    return ScalarLogicalMaybeConst(ans);

false:
    return ScalarLogicalMaybeConst(FALSE);
}


static SEXP string_relop_or(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2, res;
    SEXP x1, x2;
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = FALSE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = STRING_ELT(s2,0);
            if (x2 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x1 = STRING_ELT(s1,i);
                    if (x1==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? T : F)
                        goto true;
            }
        }
        else if (n1 == 1) {
            x1 = STRING_ELT(s1,0);
            if (x1 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x2 = STRING_ELT(s2,i);
                    if (x2==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? T : F)
                        goto true;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = STRING_ELT(s1,i);
                x2 = STRING_ELT(s2,i);
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? T : F)
                    goto true;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = STRING_ELT(s1,i1);
                x2 = STRING_ELT(s2,i2);
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? T : F)
                    goto true;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = STRING_ELT(s1, i % n1);
	    x2 = STRING_ELT(s2, i % n2);
	    if (x1 == NA_STRING || x2 == NA_STRING)
		ans = NA_LOGICAL;
	    else if (x1 != x2) {
                /* POSIX allows EINVAL when one of the strings contains
                   characters outside the collation domain. */
		errno = 0;
		res = Scollate(x1, x2);
                if (errno)
                    ans = NA_LOGICAL;
                else if (res < 0 ? T : F)
                    goto true;
	    }
	}
    }

    return ScalarLogicalMaybeConst(ans);

true:
    return ScalarLogicalMaybeConst(TRUE);
}


static SEXP raw_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    Rbyte x1, x2;

#   define RAW_FETCH(s,i) (RAW(s)[i])

    RELOP_MACRO (RAW_FETCH, 0, 0, x1==x2, x1<x2);
}


SEXP bitwiseNot(SEXP a)
{
    int  m = LENGTH(a);
    SEXP ans = allocVector(INTSXP, m);
    for(int i = 0; i < m; i++) INTEGER(ans)[i] =  ~INTEGER(a)[i];
    return ans;
}

SEXP bitwiseAnd(SEXP a, SEXP b)
{
    int  m = LENGTH(a), n = LENGTH(b), mn = (m && n) ? fmax2(m, n) : 0;
    SEXP ans = allocVector(INTSXP, mn);
    for(int i = 0; i < mn; i++)
	INTEGER(ans)[i] = INTEGER(a)[i%m] & INTEGER(b)[i%n];
    return ans;
}

SEXP bitwiseOr(SEXP a, SEXP b)
{
    int  m = LENGTH(a), n = LENGTH(b), mn = (m && n) ? fmax2(m, n) : 0;
    SEXP ans = allocVector(INTSXP, mn);
    for(int i = 0; i < mn; i++)
	INTEGER(ans)[i] = INTEGER(a)[i%m] | INTEGER(b)[i%n];
    return ans;
}

SEXP bitwiseXor(SEXP a, SEXP b)
{
    int  m = LENGTH(a), n = LENGTH(b), mn = (m && n) ? fmax2(m, n) : 0;
    SEXP ans = allocVector(INTSXP, mn);
    for(int i = 0; i < mn; i++)
	INTEGER(ans)[i] = INTEGER(a)[i%m] ^ INTEGER(b)[i%n];
    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_relop[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

/* Relational Operators, all primitives */
/* these are group generic and so need to eval args (inside, as special) */

{"==",		do_relop,	EQOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop,	NEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop,	LTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop,	LEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop,	GEOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop,	GTOP,	1000,	2,	{PP_BINARY,  PREC_COMPARE,0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
