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

#include <helpers/helpers-app.h>

#include "scalar-stack.h"


/* MACROS FOR BUILDING PROCEDURES THAT DO THE RELATIONAL OPERATIONS.  
   Separate macros are defined for non-variant operations, and for
   the and, or, and sum variants. */

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

#define RAW_FETCH(s,i)  RAW(s)[i]
#define INT_FETCH(s,i)  INTEGER(s)[i]
#define REAL_FETCH(s,i) REAL(s)[i]
#define CPLX_FETCH(s,i) COMPLEX(s)[i]

#define RELOP_MACRO(FETCH,NANCHK1,NANCHK2,COMPARE) do { \
 \
    if (n2 == 1) { \
        x2 = FETCH(s2,0); \
        if (NANCHK2) \
            for (i = 0; i<n; i++) lp[i] = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x1 = FETCH(s1,i); \
                lp[i] = NANCHK1 ? NA_LOGICAL : COMPARE ? T : F; \
            } \
    } \
    else if (n1 == 1) { \
        x1 = FETCH(s1,0); \
        if (NANCHK1) \
            for (i = 0; i<n; i++) lp[i] = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x2 = FETCH(s2,i); \
                lp[i] = NANCHK2 ? NA_LOGICAL : COMPARE ? T : F; \
            } \
    } \
    else if (n1 == n2) { \
        for (i = 0; i<n; i++) { \
            x1 = FETCH(s1,i); \
            x2 = FETCH(s2,i); \
            lp[i] = \
              NANCHK1 || NANCHK2 ? NA_LOGICAL : COMPARE ? T : F; \
        } \
    } \
    else { \
        mod_iterate(n1, n2, i1, i2) { \
            x1 = FETCH(s1,i1); \
            x2 = FETCH(s2,i2); \
            lp[i] = \
              NANCHK1 || NANCHK2 ? NA_LOGICAL : COMPARE ? T : F; \
        } \
    } \
} while (0)

#define RELOP_AND_MACRO(FETCH,NANCHK1,NANCHK2,COMPARE) do { \
 \
    res = TRUE; \
 \
    if (n2 == 1) { \
        x2 = FETCH(s2,0); \
        if (NANCHK2) \
            res = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x1 = FETCH(s1,i); \
                if (NANCHK1) \
                    res = NA_LOGICAL; \
                else if (COMPARE ? F : T) { \
                    res = FALSE; \
                    break; \
                } \
            } \
    } \
    else if (n1 == 1) { \
        x1 = FETCH(s1,0); \
        if (NANCHK1) \
            res = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x2 = FETCH(s2,i); \
                if (NANCHK2) \
                    res = NA_LOGICAL; \
                else if (COMPARE ? F : T) { \
                    res = FALSE; \
                    break; \
                } \
            } \
    } \
    else if (n1 == n2) { \
        for (i = 0; i<n; i++) { \
            x1 = FETCH(s1,i); \
            x2 = FETCH(s2,i); \
            if (NANCHK1 || NANCHK2) \
                res = NA_LOGICAL; \
            else if (COMPARE ? F : T) { \
                res = FALSE; \
                break; \
            } \
        } \
    } \
    else { \
        mod_iterate(n1, n2, i1, i2) { \
            x1 = FETCH(s1,i1); \
            x2 = FETCH(s2,i2); \
            if (NANCHK1 || NANCHK2) \
                res = NA_LOGICAL; \
            else if (COMPARE ? F : T) { \
                res = FALSE; \
                break; \
            } \
        } \
    } \
 \
} while (0)

#define RELOP_OR_MACRO(FETCH,NANCHK1,NANCHK2,COMPARE) do { \
 \
    res = FALSE; \
 \
    if (n2 == 1) { \
        x2 = FETCH(s2,0); \
        if (NANCHK2) \
            res = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x1 = FETCH(s1,i); \
                if (NANCHK1) \
                    res = NA_LOGICAL; \
                else if (COMPARE ? T : F) { \
                    res = TRUE; \
                    break; \
                } \
            } \
    } \
    else if (n1 == 1) { \
        x1 = FETCH(s1,0); \
        if (NANCHK1) \
            res = NA_LOGICAL; \
        else \
            for (i = 0; i<n; i++) { \
                x2 = FETCH(s2,i); \
                if (NANCHK2) \
                    res = NA_LOGICAL; \
                else if (COMPARE ? T : F) { \
                    res = TRUE; \
                    break; \
                } \
            } \
    } \
    else if (n1 == n2) { \
        for (i = 0; i<n; i++) { \
            x1 = FETCH(s1,i); \
            x2 = FETCH(s2,i); \
            if (NANCHK1 || NANCHK2) \
                res = NA_LOGICAL; \
            else if (COMPARE ? T : F) { \
                res = TRUE; \
                break; \
            } \
        } \
    } \
    else { \
        mod_iterate(n1, n2, i1, i2) { \
            x1 = FETCH(s1,i1); \
            x2 = FETCH(s2,i2); \
            if (NANCHK1 || NANCHK2) \
                res = NA_LOGICAL; \
            else if (COMPARE ? T : F) { \
                res = TRUE; \
                break; \
            } \
        } \
    } \
 \
} while (0)

#define RELOP_SUM_MACRO(FETCH,NANCHK1,NANCHK2,COMPARE) do { \
 \
    res = 0; \
 \
    if (n2 == 1) { \
        x2 = FETCH(s2,0); \
        if (NANCHK2) \
            res = NA_INTEGER; \
        else \
            for (i = 0; i<n; i++) { \
                x1 = FETCH(s1,i); \
                if (NANCHK1) { \
                    res = NA_INTEGER; \
                    break; \
                } \
                else if (COMPARE ? T : F) \
                    res += 1; \
            } \
    } \
    else if (n1 == 1) { \
        x1 = FETCH(s1,0); \
        if (NANCHK1) \
            res = NA_INTEGER; \
        else \
            for (i = 0; i<n; i++) { \
                x2 = FETCH(s2,i); \
                if (NANCHK2) { \
                    res = NA_INTEGER; \
                    break; \
                } \
                else if (COMPARE ? T : F) \
                    res += 1; \
            } \
    } \
    else if (n1 == n2) { \
        for (i = 0; i<n; i++) { \
            x1 = FETCH(s1,i); \
            x2 = FETCH(s2,i); \
            if (NANCHK1 || NANCHK2) { \
                res = NA_INTEGER; \
                break; \
            } \
            else if (COMPARE ? T : F) \
                res += 1; \
        } \
    } \
    else { \
        mod_iterate(n1, n2, i1, i2) { \
            x1 = FETCH(s1,i1); \
            x2 = FETCH(s2,i2); \
            if (NANCHK1 || NANCHK2) { \
                res = NA_INTEGER; \
                break; \
            } \
            else if (COMPARE ? T : F) \
                res += 1; \
        } \
    } \
 \
} while (0)


/* TASK PROCEDURES FOR RELATIONAL OPERATIONS NOT ON STRINGS.  Note
   that the string operations may require translation, which involves
   memory allocation, and hence cannot be done in a procedure executed
   in a helper thread.  There are task procedures for non-variant
   operations and for AND, OR, and SUM variants. */

void task_relop (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int * restrict lp = LOGICAL(ans);
    int F = code & 1;
    int T = !F;

    code >>= 1;

    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);
    int n = n1>n2 ? n1 : n2;

    int i, i1, i2;

    switch (TYPEOF(s1)) {
    case RAWSXP: {
        Rbyte x1, x2;
        switch (code) {
        case EQOP:
            RELOP_MACRO (RAW_FETCH, 0, 0, x1 == x2);
            return;
        case LTOP:
            RELOP_MACRO (RAW_FETCH, 0, 0, x1 < x2);
            return;
        }
    }
    case LGLSXP: case INTSXP: {
        int x1, x2;
        switch (code) {
        case EQOP:
            RELOP_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1==x2);
            return;
        case LTOP:
            RELOP_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1<x2);
            return;
        }
    }
    case REALSXP: {
        double x1, x2;
        switch (code) {
        case EQOP:
            RELOP_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 == x2);
            return;
        case LTOP:
            RELOP_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 < x2);
            return;
        }
    }
    case CPLXSXP: {
        Rcomplex x1, x2;
        switch (code) {
        case EQOP:
            RELOP_MACRO (CPLX_FETCH, (ISNAN(x1.r) || ISNAN(x1.i)), 
                                     (ISNAN(x2.r) || ISNAN(x2.i)), 
                                     (x1.r == x2.r && x1.i == x2.i));
            return;
        }
    }}
}

void task_relop_and (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int F = code & 1;
    int T = !F;

    code >>= 1;

    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);
    int n = n1>n2 ? n1 : n2;

    int i, i1, i2;
    int res;

    switch (TYPEOF(s1)) {
    case RAWSXP: {
        Rbyte x1, x2;
        switch (code) {
        case EQOP:
            RELOP_AND_MACRO (RAW_FETCH, 0, 0, x1 == x2);
            goto done;
        case LTOP:
            RELOP_AND_MACRO (RAW_FETCH, 0, 0, x1 < x2);
            goto done;
        }
    }
    case LGLSXP: case INTSXP: {
        int x1, x2;
        switch (code) {
        case EQOP:
            RELOP_AND_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1==x2);
            goto done;
        case LTOP:
            RELOP_AND_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1<x2);
            goto done;
        }
    }
    case REALSXP: {
        double x1, x2;
        switch (code) {
        case EQOP:
            RELOP_AND_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 == x2);
            goto done;
        case LTOP:
            RELOP_AND_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 < x2);
            goto done;
        }
    }
    case CPLXSXP: {
        Rcomplex x1, x2;
        switch (code) {
        case EQOP:
            RELOP_AND_MACRO (CPLX_FETCH, (ISNAN(x1.r) || ISNAN(x1.i)), 
                                         (ISNAN(x2.r) || ISNAN(x2.i)), 
                                         (x1.r == x2.r && x1.i == x2.i));
            goto done;
        }
    }}

  done:
    LOGICAL(ans)[0] = res;
}

void task_relop_or (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int F = code & 1;
    int T = !F;

    code >>= 1;

    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);
    int n = n1>n2 ? n1 : n2;

    int i, i1, i2;
    int res;

    switch (TYPEOF(s1)) {
    case RAWSXP: {
        Rbyte x1, x2;
        switch (code) {
        case EQOP:
            RELOP_OR_MACRO (RAW_FETCH, 0, 0, x1 == x2);
            goto done;
        case LTOP:
            RELOP_OR_MACRO (RAW_FETCH, 0, 0, x1 < x2);
            goto done;
        }
    }
    case LGLSXP: case INTSXP: {
        int x1, x2;
        switch (code) {
        case EQOP:
            RELOP_OR_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1==x2);
            goto done;
        case LTOP:
            RELOP_OR_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1<x2);
            goto done;
        }
    }
    case REALSXP: {
        double x1, x2;
        switch (code) {
        case EQOP:
            RELOP_OR_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 == x2);
            goto done;
        case LTOP:
            RELOP_OR_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 < x2);
            goto done;
        }
    }
    case CPLXSXP: {
        Rcomplex x1, x2;
        switch (code) {
        case EQOP:
            RELOP_OR_MACRO (CPLX_FETCH, (ISNAN(x1.r) || ISNAN(x1.i)), 
                                        (ISNAN(x2.r) || ISNAN(x2.i)), 
                                        (x1.r == x2.r && x1.i == x2.i));
            goto done;
        }
    }}

  done:
    LOGICAL(ans)[0] = res;
}

void task_relop_sum (helpers_op_t code, SEXP ans, SEXP s1, SEXP s2)
{
    int F = code & 1;
    int T = !F;

    code >>= 1;

    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);
    int n = n1>n2 ? n1 : n2;

    int i, i1, i2;
    int res;

    switch (TYPEOF(s1)) {
    case RAWSXP: {
        Rbyte x1, x2;
        switch (code) {
        case EQOP:
            RELOP_SUM_MACRO (RAW_FETCH, 0, 0, x1 == x2);
            goto done;
        case LTOP:
            RELOP_SUM_MACRO (RAW_FETCH, 0, 0, x1 < x2);
            goto done;
        }
    }
    case LGLSXP: case INTSXP: {
        int x1, x2;
        switch (code) {
        case EQOP:
            RELOP_SUM_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1==x2);
            goto done;
        case LTOP:
            RELOP_SUM_MACRO (INT_FETCH, x1==NA_INTEGER, x2==NA_INTEGER, x1<x2);
            goto done;
        }
    }
    case REALSXP: {
        double x1, x2;
        switch (code) {
        case EQOP:
            RELOP_SUM_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 == x2);
            goto done;
        case LTOP:
            RELOP_SUM_MACRO (REAL_FETCH, ISNAN(x1), ISNAN(x2), x1 < x2);
            goto done;
        }
    }
    case CPLXSXP: {
        Rcomplex x1, x2;
        switch (code) {
        case EQOP:
            RELOP_SUM_MACRO (CPLX_FETCH, (ISNAN(x1.r) || ISNAN(x1.i)), 
                                         (ISNAN(x2.r) || ISNAN(x2.i)), 
                                         (x1.r == x2.r && x1.i == x2.i));
            goto done;
        }
    }}

  done:
    INTEGER(ans)[0] = res;
}


/* PROCEDURES FOR RELATIONAL OPERATIONS ON STRINGS.  Separate versions
   for non-variant operations, and for AND, OR, and SUM variants. */

static SEXP string_relop(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    SEXP ans, x1, x2;
    const SEXP *e1 = STRING_PTR(s1);
    const SEXP *e2 = STRING_PTR(s2);
    int T = !F;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    PROTECT(ans = allocVector(LGLSXP, n));
    int * restrict lp = LOGICAL(ans);

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = e2[0];
            for (i = 0; i<n; i++) {
                x1 = e1[i];
                lp[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                      : SEQL(x1, x2) ? T : F;
            }
        }
        else if (n1 == 1) {
            x1 = e1[0];
            for (i = 0; i<n; i++) {
                x2 = e2[i];
                lp[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                      : SEQL(x1, x2) ? T : F;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = e1[i];
                x2 = e2[i];
                lp[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                      : SEQL(x1, x2) ? T : F;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = e1[i1];
                x2 = e2[i2];
                lp[i] = x1==NA_STRING || x2==NA_STRING ? NA_LOGICAL
                      : SEQL(x1, x2) ? T : F;
            }
	}
    }
    else { /* LTOP */
        if (n2 == 1) {
            x2 = e2[0];
            for (i = 0; i<n; i++) {
                x1 = e1[i];
                if (x1 == NA_STRING || x2 == NA_STRING)
                    lp[i] = NA_LOGICAL;
                else if (x1 == x2)
                    lp[i] = F;
                else
                    lp[i] = Scollate(x1, x2) < 0 ? T : F;
            }
        }
        else if (n1 == 1) {
            x1 = e1[0];
            for (i = 0; i<n; i++) {
                x2 = e2[i];
                if (x1 == NA_STRING || x2 == NA_STRING)
                    lp[i] = NA_LOGICAL;
                else if (x1 == x2)
                    lp[i] = F;
                else
                    lp[i] = Scollate(x1, x2) < 0 ? T : F;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = e1[i];
                x2 = e2[i];
                if (x1 == NA_STRING || x2 == NA_STRING)
                    lp[i] = NA_LOGICAL;
                else if (x1 == x2)
                    lp[i] = F;
                else
                    lp[i] = Scollate(x1, x2) < 0 ? T : F;
            }
        }
        else {
            mod_iterate(n1, n2, i1, i2) {
                x1 = e1[i1];
                x2 = e2[i2];
                if (x1 == NA_STRING || x2 == NA_STRING)
                    lp[i] = NA_LOGICAL;
                else if (x1 == x2)
                    lp[i] = F;
                else
                    lp[i] = Scollate(x1, x2) < 0 ? T : F;
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
    const SEXP *e1 = STRING_PTR(s1);
    const SEXP *e2 = STRING_PTR(s2);
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = TRUE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = e2[0];
            if (x2 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x1 = e1[i];
                    if (x1==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? F : T)
                        goto false;
            }
        }
        else if (n1 == 1) {
            x1 = e1[0];
            if (x1 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x2 = e2[i];
                    if (x2==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? F : T)
                        goto false;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = e1[i];
                x2 = e2[i];
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? F : T)
                    goto false;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = e1[i1];
                x2 = e2[i2];
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? F : T)
                    goto false;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = e1[i % n1];
	    x2 = e2[i % n2];
	    if (x1 == NA_STRING || x2 == NA_STRING)
		ans = NA_LOGICAL;
	    else if (x1 == x2)
		goto false;
	    else {
                if (Scollate(x1, x2) < 0 ? F : T)
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
    const SEXP *e1 = STRING_PTR(s1);
    const SEXP *e2 = STRING_PTR(s2);
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = FALSE;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = e2[0];
            if (x2 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x1 = e1[i];
                    if (x1==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? T : F)
                        goto true;
            }
        }
        else if (n1 == 1) {
            x1 = e1[0];
            if (x1 == NA_STRING)
                ans = NA_LOGICAL;
            else
                for (i = 0; i<n; i++) {
                    x2 = e2[i];
                    if (x2==NA_STRING)
                        ans = NA_LOGICAL;
                    else if (SEQL(x1, x2) ? T : F)
                        goto true;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = e1[i];
                x2 = e2[i];
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? T : F)
                    goto true;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = e1[i1];
                x2 = e2[i2];
                if (x1==NA_STRING || x2==NA_STRING)
                    ans = NA_LOGICAL;
                else if (SEQL(x1, x2) ? T : F)
                    goto true;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = e1[i % n1];
	    x2 = e2[i % n2];
	    if (x1 == NA_STRING || x2 == NA_STRING)
		ans = NA_LOGICAL;
	    else if (x1 != x2) {
                if (Scollate(x1, x2) < 0 ? T : F)
                    goto true;
	    }
	}
    }

    return ScalarLogicalMaybeConst(ans);

true:
    return ScalarLogicalMaybeConst(TRUE);
}

static SEXP string_relop_sum(RELOP_TYPE code, int F, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2, res;
    SEXP x1, x2;
    const SEXP *e1 = STRING_PTR(s1);
    const SEXP *e2 = STRING_PTR(s2);
    int T = !F;
    int ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    ans = 0;

    if (code == EQOP) {
        if (n2 == 1) {
            x2 = e2[0];
            if (x2 == NA_STRING)
                ans = NA_INTEGER;
            else
                for (i = 0; i<n; i++) {
                    x1 = e1[i];
                    if (x1==NA_STRING) {
                        ans = NA_INTEGER;
                        break;
                    }
                    else if (SEQL(x1, x2) ? T : F)
                        ans += 1;
            }
        }
        else if (n1 == 1) {
            x1 = e1[0];
            if (x1 == NA_STRING)
                ans = NA_INTEGER;
            else
                for (i = 0; i<n; i++) {
                    x2 = e2[i];
                    if (x2==NA_STRING) {
                        ans = NA_INTEGER;
                        break;
                    }
                    else if (SEQL(x1, x2) ? T : F)
                        ans += 1;
            }
        }
        else if (n1 == n2) {
            for (i = 0; i<n; i++) {
	        x1 = e1[i];
                x2 = e2[i];
                if (x1==NA_STRING || x2==NA_STRING) {
                    ans = NA_INTEGER;
                    break;
                }
                else if (SEQL(x1, x2) ? T : F)
                    ans += 1;
            }
        }
        else {
	    mod_iterate(n1, n2, i1, i2) {
	        x1 = e1[i1];
                x2 = e2[i2];
                if (x1==NA_STRING || x2==NA_STRING) {
                    ans = NA_INTEGER;
                    break;
                }
                else if (SEQL(x1, x2) ? T : F)
                    ans += 1;
            }
	}
    }
    else { /* LTOP */
	for (i = 0; i < n; i++) {
	    x1 = e1[i % n1];
	    x2 = e2[i % n2];
	    if (x1 == NA_STRING || x2 == NA_STRING) {
		ans = NA_INTEGER;
                break;
            }
	    else if (x1 != x2) {
                if (Scollate(x1, x2) < 0 ? T : F)
                    ans += 1;
	    }
	}
    }

    return ScalarIntegerMaybeConst(ans);
}


/* MAIN PART OF IMPLEMENTATION OF RELATIONAL OPERATORS.  Called from
   do_relop below, and from elsewhere. */

#define T_relop THRESHOLD_ADJUST(24) 

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

    /* Get types and lengths of operands.  Pretend that logical is
       integer, as they have the same representation. */

    SEXPTYPE typeof_x = TYPEOF(x);
    SEXPTYPE typeof_y = TYPEOF(y);

    if (typeof_x == LGLSXP) typeof_x = INTSXP;
    if (typeof_y == LGLSXP) typeof_y = INTSXP;

    int nx = (ATOMIC_VECTOR_TYPES >> typeof_x) & 1 ? LENGTH(x) : length(x);
    int ny = (ATOMIC_VECTOR_TYPES >> typeof_y) & 1 ? LENGTH(y) : length(y);
    int n = nx>ny ? nx : ny;

    /* Handle integer/real/string vectors that have no attributes (or whose
       attributes we will ignore) quickly. */ 

    if ((typeof_x == REALSXP || typeof_x == INTSXP || typeof_x == STRSXP)
          && (typeof_y == typeof_x || typeof_x != STRSXP 
                                 && (typeof_y == REALSXP || typeof_y == INTSXP))
          && nx > 0 && ny > 0
          && ((variant & VARIANT_ANY_ATTR) != 0
                || !HAS_ATTRIB(x) && !HAS_ATTRIB(y))) {

        /* Handle scalars even quicker using ScalarLogicalMaybeConst. */
    
        if (nx==1 && ny==1) {

            int result;

            if (typeof_x == STRSXP) {

                SEXP x1 = STRING_ELT(x,0), y1 = STRING_ELT(y,0);
                result = x1==NA_STRING || y1==NA_STRING ? NA_LOGICAL
                          : code == EQOP ? SEQL(x1,y1) 
                          : /* LTOP */ x1 == y1 ? FALSE : Scollate(x1,y1) < 0;
            }
            else {  /* INTSXP or REALSXP */

                WAIT_UNTIL_COMPUTED_2(x,y);

                /* Assumes ints can be represented to full precision as reals */

                double x1 = typeof_x == REALSXP ? REAL(x)[0]
                   : INTEGER(x)[0]!=NA_INTEGER ? INTEGER(x)[0] : NA_REAL;

                double y1 = typeof_y == REALSXP ? REAL(y)[0]
                   : INTEGER(y)[0]!=NA_INTEGER ? INTEGER(y)[0] : NA_REAL;

                result = ISNAN(x1) || ISNAN(y1) ? NA_LOGICAL
                           : code == EQOP ? x1 == y1 : /* LTOP */ x1 < y1;
            }

            return ScalarLogicalMaybeConst (negate && result != NA_LOGICAL 
                                             ? !result : result);
        } 
        else {
            PROTECT2(x,y);
            if (((nx > ny) ? nx % ny : ny % nx) != 0) {
 	            warningcall (call,
          _("longer object length is not a multiple of shorter object length"));
            }

            if (typeof_x != REALSXP && typeof_y == REALSXP)
                x = coerceVector(x,REALSXP);
            else if (typeof_y != REALSXP && typeof_x == REALSXP)
                y = coerceVector(y,REALSXP);
            UNPROTECT(2);
            PROTECT2(x,y);
            PROTECT3(dims=R_NilValue,xnames=R_NilValue,ynames=R_NilValue);
            xts = yts = 0;
        }
    }

    else { /* the general case */

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
            typeof_x = STRSXP;
            UNPROTECT(1);
        }
        if ((iS = isSymbol(y)) || typeof_y == LANGSXP) {
            SEXP tmp = allocVector(STRSXP, 1);
            PROTECT(tmp);
            SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
                           STRING_ELT(deparse1(y, 0, DEFAULTDEPARSE), 0));
            REPROTECT(y = tmp, ypi);
            typeof_y = STRSXP;
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

        /* At this point, x and y are both atomic or vector list */

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
                PROTECT(dims = getDimAttrib(x));
            }
            else if (xarray) {
                PROTECT(dims = getDimAttrib(x));
            }
            else /*(yarray)*/ {
                PROTECT(dims = getDimAttrib(y));
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
                PROTECT(klass = !objx ? R_NilValue :getClassAttrib(x));
            }
            else if (xts) {
                if (length(x) < length(y))
                    ErrorMessage(call, ERROR_TSVEC_MISMATCH);
                PROTECT(tsp = getAttrib(x, R_TspSymbol));
                PROTECT(klass = !objx ? R_NilValue :getClassAttrib(x));
            }
            else /*(yts)*/ {
                if (length(y) < length(x))
            	ErrorMessage(call, ERROR_TSVEC_MISMATCH);
                PROTECT(tsp = getAttrib(y, R_TspSymbol));
                PROTECT(klass = !objy ? R_NilValue :getClassAttrib(y));
            }
        }

        if (mismatch)
            warningcall (call, 
          _("longer object length is not a multiple of shorter object length"));

        if (typeof_x == STRSXP || typeof_y == STRSXP) {
            if (typeof_x != STRSXP) REPROTECT(x = coerceVector(x, STRSXP), xpi);
            if (typeof_y != STRSXP) REPROTECT(y = coerceVector(y, STRSXP), ypi);
        }
        else if (typeof_x == CPLXSXP || typeof_y == CPLXSXP) {
            if (typeof_x!=CPLXSXP) REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
            if (typeof_y!=CPLXSXP) REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
            if (code != EQOP)
                errorcall (call, _("invalid comparison with complex values"));
        }
        else if (typeof_x == REALSXP || typeof_y == REALSXP) {
            if (typeof_x!=REALSXP) REPROTECT(x = coerceVector(x, REALSXP), xpi);
            if (typeof_y!=REALSXP) REPROTECT(y = coerceVector(y, REALSXP), ypi);
        }
        else if (typeof_x == INTSXP || typeof_y == INTSXP) {
            /* NOT isInteger, since it needs to be true for factors with
               VARIANT_UNCLASS_FLAG.  Assumes LOGICAL same as INTEGER. */
            if (typeof_x != INTSXP) REPROTECT(x = coerceVector(x, INTSXP), xpi);
            if (typeof_y != INTSXP) REPROTECT(y = coerceVector(y, INTSXP), ypi);
        }
        else if (typeof_x == RAWSXP || typeof_y == RAWSXP) {
            if (typeof_x != RAWSXP) REPROTECT(x = coerceVector(x, RAWSXP), xpi);
            if (typeof_y != RAWSXP) REPROTECT(y = coerceVector(y, RAWSXP), ypi);
        }
        else 
            errorcall (call, 
                       _("comparison of these types is not implemented"));
    }

    if (typeof_x == STRSXP || typeof_y == STRSXP) {
        WAIT_UNTIL_COMPUTED_2(x,y);
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
        case VARIANT_SUM:
            ans = string_relop_sum (code, negate, x, y);
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        default:
            PROTECT(ans = string_relop (code, negate, x, y));
            break;
        }
    }
    else /* not strings */ {
        helpers_op_t codeop = (code<<1) | negate;
        switch (VARIANT_KIND(variant)) {
        case VARIANT_AND: 
            WAIT_UNTIL_COMPUTED_2(x,y);
            ans = allocVector1LGL();
            task_relop_and (codeop, ans, x, y); 
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        case VARIANT_OR:
            WAIT_UNTIL_COMPUTED_2(x,y);
            ans = allocVector1LGL();
            task_relop_or (codeop, ans, x, y); 
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(5);
            return ans;
        case VARIANT_SUM:
            PROTECT(ans = allocVector1INT());
            if (ON_SCALAR_STACK(x) && ON_SCALAR_STACK(y)) {
                PROTECT(x = DUP_STACK_VALUE(x));
                y = DUP_STACK_VALUE(y);
                UNPROTECT(1);
            }
            else if (ON_SCALAR_STACK(x)) x = DUP_STACK_VALUE(x);
            else if (ON_SCALAR_STACK(y)) y = DUP_STACK_VALUE(y);
            DO_NOW_OR_LATER2 (variant, n >= T_relop, 0, task_relop_sum, codeop, 
                              ans, x, y);
            if (xts || yts) UNPROTECT(2);
            UNPROTECT(6);
            return ans;
        default:
            PROTECT(ans = allocVector(LGLSXP,n));
            if (ON_SCALAR_STACK(x) && ON_SCALAR_STACK(y)) {
                PROTECT(x = DUP_STACK_VALUE(x));
                y = DUP_STACK_VALUE(y);
                UNPROTECT(1);
            }
            else if (ON_SCALAR_STACK(x)) x = DUP_STACK_VALUE(x);
            else if (ON_SCALAR_STACK(y)) y = DUP_STACK_VALUE(y);
            DO_NOW_OR_LATER2 (variant, n >= T_relop, 0, task_relop, codeop, 
                              ans, x, y);
            break;
        }
    }

    /* Tack on dims, names, ts stuff, if necessary. */

    if (! (variant & VARIANT_ANY_ATTR)) {
        if (dims != R_NilValue) {
            setAttrib(ans, R_DimSymbol, dims);
            if (xnames != R_NilValue)
                setAttrib(ans, R_DimNamesSymbol, xnames);
            else if (ynames != R_NilValue)
                setAttrib(ans, R_DimNamesSymbol, ynames);
        }
        else if (xnames != R_NilValue && LENGTH(ans) == LENGTH(xnames))
            setAttrib(ans, R_NamesSymbol, xnames);
        else if (ynames != R_NilValue && LENGTH(ans) == LENGTH(ynames))
            setAttrib(ans, R_NamesSymbol, ynames);
    
        if (xts || yts) {
            setAttrib(ans, R_TspSymbol, tsp);
            setAttrib(ans, R_ClassSymbol, klass);
            UNPROTECT(2);
        }
    }

    UNPROTECT(6);
    return ans;
}


/* RELATIONAL OPERATORS.  May dispatch by class; otherwise implemented 
   by R_relop. */

static SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP argsevald, ans, x, y;
    int objx, objy;

    /* Evaluate arguments, maybe putting them on the scalar stack. */

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT(argsevald = 
              scalar_stack_eval2 (args, &x, &y, &objx, &objy, env));
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

    /* Arguments are now in x and y, and are protected.  They may be on
       the scalar stack, but if so are popped off here (but retain their
       values if eval is not called). */

    /* Below does same as POP_IF_TOP_OF_STACK(y); POP_IF_TOP_OF_STACK(x);
       but faster. */

    R_scalar_stack = sv_scalar_stack;

    ans = R_relop (call, op, x, y, objx, objy, env, variant);

    UNPROTECT(3);
    return ans;
}


/* BITWISE INTEGER OPERATORS.  Used for "octmode" versions of the !,
   &, |, and xor operator, defined in 'base' (not for the operations
   on raw bytes). */

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
