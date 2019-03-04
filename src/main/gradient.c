/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2019 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2011   The R Core Team.
 *  Copyright (C) 2004-5      The R Foundation
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
#define R_USE_SIGNALS
#include "Defn.h"

#include <matprod/matprod.h>


#define GRADIENT_WRT_LEN(g) \
  (LENGTH(g) == 1 ? 1 : TRUELENGTH(g))

#define SET_GRADIENT_WRT_LEN(g,l) \
  (LENGTH(g) == 1 ? (void) 0 : (void) SET_TRUELENGTH(g,l))

static void R_NORETURN gradient_matrix_too_large_error (void) {
    error (_("gradient matrix would be too large"));
}


/* Expand the structure of 'grad' to be a full gradient for 'value' by
   replacing NULL elements that correspond to non-NULL elements by the
   appropriate zero Jacobian.  The 'idg' argument is the identify
   gradient to use as a skeleton, in top levels with GRAD_WRT_GRAD
   set, and just below that, with GRADIENT_WRT_LEN set to the length of the
   numeric vector that that part of the gradient is with respect to.
   Names are added to lists to match those in 'value' or 'idg'.  A 'dim'
   attribute is added for Jacobian matrices with more than one column. */

static SEXP expand_gradient (SEXP value, SEXP grad, SEXP idg)
{
    SEXP res;

    if (TYPEOF(idg) == VECSXP && GRAD_WRT_LIST(idg)) {

        /* Create list for gradient with respect to a list, filling in
           element with recursive calls of expand_gradient. */

        R_len_t n = LENGTH(idg);
        if (grad != R_NilValue) {
            if (TYPEOF(grad) != VECSXP || !GRAD_WRT_LIST(grad)) abort();
            if (LENGTH(grad) != n) abort();
        }
        PROTECT (res = allocVector(VECSXP,n));
        setAttrib (res, R_NamesSymbol, getNamesAttrib(idg));
        R_len_t i;
        for (i = 0; i < n; i++)
            SET_VECTOR_ELT (res, i, expand_gradient (value, 
                             grad==R_NilValue ? R_NilValue : VECTOR_ELT(grad,i),
                             VECTOR_ELT(idg,i)));
        UNPROTECT(1);
        return res;
    }

    if (grad == R_NilValue) {

        if (TYPEOF(value) == VECSXP) {

            /* Fill in zero gradient for a NULL in 'grad' that corresponds
               to a list in 'value', by calling expand_gradient recursively
               to create the zero Jacobian matrices to put in the list. */

            R_len_t n = LENGTH(value);
            PROTECT (res = allocVector(VECSXP,n));
            setAttrib (res, R_NamesSymbol, getNamesAttrib(value));
            R_len_t i;
            for (i = 0; i < n; i++)
                SET_VECTOR_ELT (res, i, 
                  expand_gradient (VECTOR_ELT(value,i), R_NilValue, idg));
            UNPROTECT(1);
            return res;
        }

        else if (TYPEOF(value) == REALSXP) { 

            /* Fill in a zero Jacobian matrix for a missing gradient w.r.t.
               a numeric vector, with the number of columns taken from 
               GRADIENT_WRT_LEN of 'idg'. */

            R_len_t vlen = LENGTH(value);
            R_len_t glen = GRADIENT_WRT_LEN(idg);
            uint64_t Jlen = (uint64_t)vlen * (uint64_t)glen;
            if (Jlen > R_LEN_T_MAX) gradient_matrix_too_large_error();
            if (Jlen == 1)
                return R_ScalarRealZero;
            res = allocVector (REALSXP, (R_len_t) Jlen);
            memset (REAL(res), 0, Jlen * sizeof(double));
            if (glen != 1) {
                PROTECT(res);
                SEXP dim = allocVector (INTSXP, 2);
                INTEGER(dim)[0] = vlen;
                INTEGER(dim)[1] = glen;
                setAttrib (res, R_DimSymbol, dim);
                UNPROTECT(1);
            }
            return res;
        }

        else {

            /* Let the gradient for a non-numeric value be NULL. */

            return R_NilValue;
        }
    }

    if (TYPEOF(grad) == VECSXP) {

        /* Recursively scan lower levels when the gradient/value is a list. */

        if (TYPEOF(value) != VECSXP) abort();
        if (LENGTH(value) != LENGTH(grad)) abort();

        R_len_t n = LENGTH(value);
        PROTECT (res = allocVector(VECSXP,n));
        setAttrib (res, R_NamesSymbol, getAttrib(value,R_NamesSymbol));
        for (R_len_t i = 0; i < n; i++) {
            SET_VECTOR_ELT (res, i, 
              expand_gradient (VECTOR_ELT(value,i), VECTOR_ELT(grad,i), idg));
        }
        UNPROTECT(1);

        return res;
    }

    if (TYPEOF(grad) == REALSXP) {

        if (TYPEOF(value) != REALSXP) abort();

        R_len_t vlen = LENGTH(value);
        R_len_t glen = GRADIENT_WRT_LEN(idg);
        uint64_t Jlen = (uint64_t)vlen * (uint64_t)glen;

        if (LENGTH(grad) != Jlen) abort();

        if (glen != 1) {
            PROTECT(grad);
            SEXP dim = allocVector (INTSXP, 2);
            INTEGER(dim)[0] = vlen;
            INTEGER(dim)[1] = glen;
            setAttrib (grad, R_DimSymbol, dim);
            UNPROTECT(1);
        }

        return grad;
    }

    abort();  /* 'grad' should always be R_NilValue, VECSXP, or REALSXP */
}


/* Make an "identity" gradient for a value.  Has names at the top list
   levels, with GRAD_WRT_LIST set, since will be used as 'idg' in
   expand_gradient.  Has GRADIENT_WRT_LEN set to the length of the
   numeric vector just below the GRAD_WRT_LIST level, as well as in
   the Jacobian matrix itself.  The "identity" gradient for a numeric
   vector is an identity matrix (currenty represented explicitly); for
   a list, it is a list with all but one element NULL, with that
   element being the recursively-defined "identity" value. */

static SEXP make_id_numeric (SEXP v)
{
    SEXP res;

    if (LENGTH(v) == 1) {
        res = R_ScalarRealOne;
    }
    else {

        R_len_t vlen = LENGTH(v);
        uint64_t Jlen = (uint64_t)vlen * (uint64_t)vlen;
        if (Jlen > R_LEN_T_MAX) gradient_matrix_too_large_error();

        res = allocVector (REALSXP, (R_len_t) Jlen);
        SET_GRADIENT_WRT_LEN (res, vlen);
        memset (REAL(res), 0, Jlen * sizeof(double));
        for (R_len_t j = 0; j < Jlen; j += vlen+1)
            REAL(res)[j] = 1.0;
    }

    return res;
}

static SEXP make_id_recursive (SEXP val, SEXP top)
{
    R_len_t n = LENGTH(val);

    SEXP res = PROTECT(allocVector(VECSXP,n));
    setAttrib (res, R_NamesSymbol, getNamesAttrib(val));
    SET_GRAD_WRT_LIST (res, 1);

    for (R_len_t i = 0; i < n; i++) {

        /* this code could be made more efficient, bypassing duplicate */
        SEXP ntop = PROTECT (i == n-1 ? top : duplicate(top));
        SEXP bot = ntop;
        R_len_t j = 0;
        while (j < LENGTH(bot)) {
            if (VECTOR_ELT(bot,j) != R_NilValue) { 
                bot = VECTOR_ELT(bot,j);
                j = 0;
            }
            else
                j += 1;
        }

        SEXP v = VECTOR_ELT (val, i);
        if (TYPEOF(v) == REALSXP) {
            SET_VECTOR_ELT (bot, i, make_id_numeric(v));
            SET_GRADIENT_WRT_LEN (ntop, LENGTH(v));
            SET_VECTOR_ELT (res, i, ntop);
        }
        else if (TYPEOF(v) == VECSXP) {
            SET_VECTOR_ELT (bot, i, allocVector (VECSXP, LENGTH(v)));
            SET_VECTOR_ELT (res, i, make_id_recursive (v, ntop));
        }
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return res;
}

static SEXP make_id_grad (SEXP val)
{
    SEXP res = R_NilValue;

    if (TYPEOF(val) == REALSXP) {
        res = make_id_numeric(val);
    }

    else if (TYPEOF(val) == VECSXP) {
        SEXP top;
        PROTECT (top = allocVector (VECSXP, LENGTH(val)));
        res = make_id_recursive (val, top);
        UNPROTECT(1);
    }
 
    return res;
}


/* Test whether the structure of a gradient value matches the structure of
   what it is the gradient of. */

static int match_structure (SEXP val, SEXP grad)
{
    if (TYPEOF(val) == REALSXP) {
        if (TYPEOF(grad) != REALSXP)
            return 0;
        if (LENGTH(val) != LENGTH(grad))  /* FOR NOW */
            return 0;
    }
    else if (TYPEOF(val) == VECSXP) {
        if (TYPEOF(grad) != VECSXP)
            return 0;
        if (LENGTH(val) != LENGTH(grad))
            return 0;
        R_len_t i;
        for (i = 0; i < LENGTH(val); i++) {
            if (! match_structure (VECTOR_ELT(val,i), VECTOR_ELT(grad,i)))
                return 0;
        }
    }
    else {
        if (grad != R_NilValue)
            return 0;
    }

    return 1;
}


/* Get gradient identified by env from the gradients in R_gradient (unless
   not active), which is protected for the duration of this function.  The 
   gradient is returned as an unnamed vector list for multiple variables, or
   a single gradient if there is only one gradient variable for this
   environment.  Gradients may be R_NilValue if they are zero (or lists of 
   zeros, etc.). */

static SEXP get_gradient (SEXP env)
{
    SEXP gr = R_variant_result&VARIANT_GRADIENT_FLAG ? R_gradient : R_NilValue;
    PROTECT(gr);

    SEXP gv = GRADVARS(env);
    PROTECT(gv);
    if (TYPEOF(gv) != VECSXP) abort();
    
    int nv = GRADVARS_NV(gv);

    SEXP r = nv == 1 ? R_NilValue : allocVector(VECSXP,nv);
    PROTECT(r);

    SEXP p;

    for (p = gr; p != R_NilValue; p = CDR(p)) {
        if (TAG(p) == env) {
            int ix = GRADINDEX(p);
            SET_NAMEDCNT_MAX(CAR(p));  /* may be able to be less drastic */
            if (nv == 1) {
                if (ix != 1 || r != R_NilValue) abort();
                r = CAR(p);
                /*break;*/  /* could stop, but continue for debug error check */
            }
            else {
                if (ix<1 || ix>nv || VECTOR_ELT(r,ix-1) != R_NilValue) abort();
                SET_VECTOR_ELT (r, ix-1, CAR(p));
            }
        }
    }

    UNPROTECT(3);  /* gr, gv, r */
    return r;
}


/* Get gradients excluding those for xenv from those in R_gradient, which
   is protected for the duration of this function.  The gradients are
   returned as a pairlist. */

static inline SEXP get_other_gradients (SEXP xenv)
{
    PROTECT(R_gradient);

    SEXP p, q;

    for (p = R_gradient, q = R_NilValue; p != R_NilValue; p = CDR(p)) {
        if (TAG(p) != xenv) {
            q = cons_with_tag (CAR(p), q, TAG(p));
            SET_GRADINDEX (q, GRADINDEX(p));
        }
    }

    UNPROTECT(1);
    return q;
}


/* Macro for building a function that applies an operation to all gradients. 
   Protects grad, then unprotects it at end, so surrounding function will
   need to protect it again if required. */

#define RECURSIVE_GRADIENT_APPLY(fun,grad,...) do { \
    PROTECT(grad); \
    if (TYPEOF(grad) == LISTSXP) { \
        SEXP res = R_NilValue; \
        SEXP tail; \
        while (grad != R_NilValue) { \
            SEXP g = cons_with_tag (fun (CAR(grad), __VA_ARGS__), \
                                         R_NilValue, TAG(grad)); \
            SET_GRADINDEX (g, GRADINDEX(grad)); \
            if (res == R_NilValue) { \
                PROTECT (res = g); \
            } \
            else \
                SETCDR (tail, g); \
            tail = g; \
            grad = CDR(grad); \
        } \
        UNPROTECT(2); \
        return res; \
    } \
    else if (TYPEOF(grad) == VECSXP && GRAD_WRT_LIST(grad)) { \
        R_len_t m = LENGTH(grad); \
        SEXP res = PROTECT (allocVector(VECSXP,m)); \
        SET_GRAD_WRT_LIST (res, 1); \
        for (R_len_t j = 0; j < m; j++) \
            SET_VECTOR_ELT (res, j, fun (VECTOR_ELT(grad,j), __VA_ARGS__)); \
        UNPROTECT(2); \
        return res; \
    } \
    UNPROTECT(1); \
} while (0)


/* Create set of gradients from subsetting the i'th element of gradients for 
   a vector list of length n.  Used for [[.]].  Protects its grad argument. */

SEXP attribute_hidden subset2_list_gradient (SEXP grad, R_len_t i, R_len_t n)
{
#if 0
REprintf("subset2_list_gradient %d %d\n",i,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset2_list_gradient, grad, i, n);

    if (grad == R_NilValue)
        return R_NilValue;
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
    if (i < 0 || i >= n) abort();

    return VECTOR_ELT (grad, i);
}


/* Create set of gradients from subsetting i'th to j'th elements of gradients
   for a vector list of length n.  Used for [.].  Protects its grad argument. */

SEXP attribute_hidden subset_range_list_gradient 
                        (SEXP grad, R_len_t i, R_len_t j, R_len_t n)
{
#if 0
REprintf("subset_range_list_gradient %d %d %d\n",i,j,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset_range_list_gradient, grad, i, j, n);

    if (grad == R_NilValue)
        return R_NilValue;
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    if (j < 0 || i >= n || j < i)
        return R_NilValue;

    PROTECT(grad);

    SEXP res = allocVector (VECSXP, j-i+1);

    if (i < 0) i = 0;
    if (j >= n) j = n-1;

    for (R_len_t k = i; k <= j; k++)
        SET_VECTOR_ELT (res, k-i, VECTOR_ELT(grad,k));

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of gradients
   for a vector list of length n.  Used for [.].  Protects its grad argument. */

SEXP attribute_hidden subset_indexes_list_gradient 
                        (SEXP grad, SEXP indx, R_len_t n)
{
#if 0
REprintf("subset_indexes_list_gradient %d\n",n);
R_inspect(grad); REprintf("..\n"); R_inspect(indx); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset_indexes_list_gradient, grad, indx, n);

    if (grad == R_NilValue)
        return R_NilValue;
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    int k = LENGTH(indx);
    SEXP res = allocVector (VECSXP, k);
    
    for (R_len_t j = 0; j < k; j++) {
        R_len_t i = INTEGER(indx)[j];
        if (i >= 1 && i <= n)
            SET_VECTOR_ELT (res, j, VECTOR_ELT(grad,i-1));
    }

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting i'th to j'th elements of gradients
   for numeric vector of length n.  Used for [.] and for [[.]] (with i==j). 
   Protects its grad argument. */

SEXP attribute_hidden subset_range_numeric_gradient 
                        (SEXP grad, R_len_t i, R_len_t j, R_len_t n)
{
#if 0
REprintf("subset_range_numeric_gradient %d %d %d\n",i,j,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset_range_numeric_gradient, grad, i, j, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (j < 0 || i >= n || j < i)
        return R_NilValue;

    if (i < 0) i = 0;
    if (j >= n) j = n-1;

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t glen = LENGTH(grad);

    if (glen == n && i == j)
        return ScalarReal (REAL(grad)[i]);

    PROTECT(grad);

    if (glen % n != 0) abort();
    R_len_t slen = glen/n;
    R_len_t m = j-i+1;
    SEXP res = allocVector (REALSXP, m * slen);
    SET_GRADIENT_WRT_LEN (res, slen);
    R_len_t k, l;
    k = l = 0;
    while (k < glen) {
        copy_elements (res, l, 1, grad, k+i, 1, m);
        k += n;
        l += m;
    }

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of gradients for
   numeric vector of length n.  Used for [.].  Protects its grad argument. 
   Caller must protect indx. */

SEXP attribute_hidden subset_indexes_numeric_gradient 
                        (SEXP grad, SEXP indx, R_len_t n)
{
#if 0
REprintf("subset_indexes_numeric_gradient %d\n",n);
R_inspect(grad); REprintf("..\n"); R_inspect(indx); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset_indexes_numeric_gradient, grad, indx, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    int m = LENGTH(indx);

    if ((uint64_t)m * gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();

    SEXP res = allocVector (REALSXP, m * gvars);
    
    for (R_len_t j = 0; j < m; j++) {
        R_len_t i = INTEGER(indx)[j];
        if (i >= 1 && i <= n) {
            for (R_len_t k = 0; k < gvars; k++)
                REAL(res)[j+k*m] = REAL(grad)[(i-1)+k*n];
        }
    }

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting elements from one row of
   gradients for vector list matrix of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sc. */

SEXP attribute_hidden matrix_subset_one_row_list_gradient (SEXP grad, 
       R_len_t ii, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_one_row_list_gradient %d %d %d %d %d\n",
          ii,nr,LENGTH(sc),*INTEGER(sc),n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_one_row_list_gradient, grad,
                              ii, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);
    SEXP res = allocVector (VECSXP, ncs);

    int st = (ii-1) - nr;
    int j;

    for (j = 0; j < ncs; j++) {
        int jj = INTEGER(sc)[j];
        if (jj != NA_INTEGER)
            SET_VECTOR_ELT (res, j, VECTOR_ELT (grad, st+jj*nr));
    }
#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting elements from one row of
   gradients for a numeric matrix of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sc. */

SEXP attribute_hidden matrix_subset_one_row_numeric_gradient (SEXP grad, 
       R_len_t ii, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_one_row_numeric_gradient %d %d %d %d %d\n",
          ii,nr,LENGTH(sc),*INTEGER(sc),n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_one_row_numeric_gradient, grad,
                              ii, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();

    SEXP res = allocVector (REALSXP, ncs * gvars);
    SET_GRADIENT_WRT_LEN (res, gvars);

    memset (REAL(res), 0, LENGTH(res) * sizeof(double));

    int st = (ii-1) - nr;
    int j, k;

    for (j = 0; j < ncs; j++) {
        int jj = INTEGER(sc)[j];
        if (jj != NA_INTEGER) {
            for (k = 0; k < gvars; k++)
                REAL(res)[j+k*ncs] = REAL(grad)[st+jj*nr+k*n];
        }
    }
#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting elements from a range of rows
   of gradients for vector list matrix of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sc. */

SEXP attribute_hidden matrix_subset_range_list_gradient (SEXP grad, 
       R_len_t start, R_len_t nrs, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_range_list_gradient %d %d %d %d %d %d\n",
          start,nrs,nr,LENGTH(sc),*INTEGER(sc),n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_range_list_gradient, grad,
                              start, nrs, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX) gradient_matrix_too_large_error();

    SEXP res = allocVector (VECSXP, ncs * nrs);

    start -= 1;

    int ij = 0;
    int j;

    for (j = 0; j < ncs; j++) {

        int jj = INTEGER(sc)[j];
        if (jj == NA_INTEGER) {
            ij += nrs;
            continue;
        }

        int jjnr = (jj-1) * nr + start;

        copy_vector_elements (res, ij, grad, jjnr, nrs);

        ij += nrs;
    }

#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting elements from a range of rows
   of gradients for numeric vector of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sc. */

SEXP attribute_hidden matrix_subset_range_numeric_gradient (SEXP grad, 
       R_len_t start, R_len_t nrs, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_range_numeric_gradient %d %d %d %d %d %d\n",
          start,nrs,nr,LENGTH(sc),*INTEGER(sc),n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_range_numeric_gradient, grad,
                              start, nrs, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX /* if true, next might overflow */
     || (uint64_t)ncs * nrs * gvars > R_LEN_T_MAX) 
        gradient_matrix_too_large_error();

    SEXP res = allocVector (REALSXP, ncs * nrs * gvars);
    SET_GRADIENT_WRT_LEN (res, gvars);

    memset (REAL(res), 0, LENGTH(res) * sizeof(double));

    start -= 1;

    int m = nrs * ncs;
    int ij = 0;
    int j, k;

    for (j = 0; j < ncs; j++) {

        int jj = INTEGER(sc)[j];
        if (jj == NA_INTEGER) {
            ij += nrs;
            continue;
        }

        int jjnr = (jj-1) * nr + start;

        for (k = 0; k < gvars; k++)
            memcpy (REAL(res)+ij+k*m, REAL(grad)+jjnr+k*n, nrs*sizeof(double));

        ij += nrs;
    }

#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of
   gradients for vector list matrix of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sr and sc. */

SEXP attribute_hidden matrix_subset_indexes_list_gradient (SEXP grad, 
    SEXP sr, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_indexes_list_gradient %d %d %d %d %d %d\n",
          LENGTH(sr),LENGTH(sc),*INTEGER(sr),*INTEGER(sc),nr,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_indexes_list_gradient, grad,
                              sr, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    int i, j, ii, jj, ij, jjnr;

    R_len_t nrs = LENGTH(sr);
    R_len_t ncs = LENGTH(sc);

    SEXP res = allocVector (VECSXP, ncs * nrs);

    for (j = 0, ij = 0; j < ncs; j++) {

        jj = INTEGER(sc)[j];
        if (jj == NA_INTEGER) {
            ij += nrs;
            continue;
        }

        int *sri = INTEGER(sr);
        jjnr = (jj-1) * nr;

        for (i = 0; i < nrs; i++, ij++) {
            if ((ii = sri[i]) != NA_INTEGER) {
                SEXP ve = VECTOR_ELT (grad, (ii-1)+jjnr);
                SET_VECTOR_ELT (res, ij, ve);
            }
        }
    }

#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of
   gradients for a numeric matrix of length n.  Used for [.].
   Protects its grad argument.  Caller must protect sr and sc. */

SEXP attribute_hidden matrix_subset_indexes_numeric_gradient (SEXP grad, 
    SEXP sr, R_len_t nr, SEXP sc, R_len_t n)
{
#if 0
REprintf("matrix_subset_indexes_numeric_gradient %d %d %d %d %d %d\n",
          LENGTH(sr),LENGTH(sc),*INTEGER(sr),*INTEGER(sc),nr,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (matrix_subset_indexes_numeric_gradient, grad,
                              sr, nr, sc, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    int i, j, k, ii, jj, ij, jjnr;

    R_len_t nrs = LENGTH(sr);
    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX /* if true, next might overflow */
     || (uint64_t)ncs * nrs * gvars > R_LEN_T_MAX) 
        gradient_matrix_too_large_error();

    SEXP res = allocVector (REALSXP, ncs * nrs * gvars);
    SET_GRADIENT_WRT_LEN (res, gvars);

    memset (REAL(res), 0, LENGTH(res) * sizeof(double));

    int m = nrs * ncs;

    for (j = 0, ij = 0; j < ncs; j++) {

        jj = INTEGER(sc)[j];
        if (jj == NA_INTEGER) {
            ij += nrs;
            continue;
        }

        int *sri = INTEGER(sr);
        jjnr = (jj-1) * nr;

        for (i = 0; i < nrs; i++, ij++) {
            if ((ii = sri[i]) != NA_INTEGER) {
                for (k = 0; k < gvars; k++)
                    REAL(res)[ij+k*m] = REAL(grad)[(ii-1)+jjnr+k*n];
            }
        }
    }

#if 0
REprintf("&&\n"); R_inspect(res);
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of gradients for
   vector list array of length n with k dimensions.  Used for [.].  Protects 
   its grad argument. */

SEXP attribute_hidden array_subset_indexes_list_gradient (SEXP grad, 
    int **subs, int *nsubs, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("array_subset_indexes_list_gradient %d %d\n",k,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (array_subset_indexes_list_gradient,
                              grad, subs, nsubs, offset, k, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t m, i, v;
    int indx[k];
    int j;

    m = 1;
    for (j = 0; j < k; j++) {
        m *= nsubs[j];
        indx[j] = 0;
    }

    SEXP res = allocVector (VECSXP, m);
    int last = 0;

    for (i = 0; !last; i++) {
        R_len_t ii;
        ii = array_offset_from_index (subs, nsubs, indx, offset, k, &last);
        if (ii != NA_INTEGER)
            SET_VECTOR_ELT (res, i, VECTOR_ELT (grad, ii));
    }

    if (i != m) abort();

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from subsetting indexed elements of gradients for
   numeric array of length n with k dimensions.  Used for [.].  Protects 
   its grad argument. */

SEXP attribute_hidden array_subset_indexes_numeric_gradient (SEXP grad, 
    int **subs, int *nsubs, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("array_subset_indexes_numeric_gradient %d %d\n",k,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (array_subset_indexes_numeric_gradient,
                              grad, subs, nsubs, offset, k, n);

    if (grad == R_NilValue)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    R_len_t m, i, v;
    int indx[k];
    int j;

    m = 1;
    for (j = 0; j < k; j++) {
        m *= nsubs[j];
        indx[j] = 0;
    }

    if ((uint64_t)m * gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();

    SEXP res = allocVector (REALSXP, m * gvars);
    int last = 0;

    for (i = 0; !last; i++) {
        R_len_t ii;
        ii = array_offset_from_index (subs, nsubs, indx, offset, k, &last);
        if (ii == NA_INTEGER) {
            for (v = 0; v < gvars; v++)
                REAL(res)[i+v*m] = NA_INTEGER;
        }
        else {
            for (v = 0; v < gvars; v++)
                REAL(res)[i+v*m] = REAL(grad)[ii+v*n];
        }
    }

    if (i != m) abort();

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from deleting the i'th element of gradients for 
   a vector list of length n.  Used for [[.]].  Protects its grad argument. */

SEXP attribute_hidden delete_list_gradient (SEXP grad, R_len_t i, R_len_t n)
{

#if 0
REprintf("*** delete_list_gradient %d %d\n",i,n);
R_inspect(grad);
REprintf("--\n");
#endif

    RECURSIVE_GRADIENT_APPLY (delete_list_gradient, grad, i, n);

    if (grad == R_NilValue)
        return R_NilValue;
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
    if (i < 0 || i >= n) abort();

    PROTECT(grad);

    SEXP res = allocVector (VECSXP, n-1);
    if (i > 0) copy_vector_elements (res, 0, grad, 0, i);
    if (i < n-1) copy_vector_elements (res, i, grad, i+1, n-1-i);

#if 0
REprintf("*** delete_list_gradient end\n");
R_inspect(grad);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Copy scaled gradients from those in grad, which is protected here.  The
   length of the value is given by n.  If grad is shorter, it is recycled. */

attribute_hidden SEXP copy_scaled_gradients(SEXP grad, double factor, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (copy_scaled_gradients, grad, factor, n);

    if (grad == R_NilValue)
        return R_NilValue;

    PROTECT(grad);

    if (TYPEOF(grad) != REALSXP) abort();
    R_len_t gn, gvars, glen;
    R_len_t i, j, k;
    SEXP r;

    gvars = GRADIENT_WRT_LEN(grad);
    if ((uint64_t)n * gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();
    glen = n * gvars;
    gn = LENGTH(grad) / gvars;
    if (LENGTH(grad) != gvars * gn) abort();

    if (glen == 1)
        r = ScalarRealMaybeConst (*REAL(grad) * factor);
    else {
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        k = 0;
        for (i = 0; i < glen; i += n) {
            for (j = 0; j < n; j++) {
                REAL(r)[i+j] = REAL(grad)[k+j%gn] * factor;
            }
            k += gn;
        }
    }

    UNPROTECT(1);
    return r;
}


/* Copy scaled gradients from those in grad with vector of scaling factors.
   If grad is shorter, it is recycled. 

   Caller must protect factors, but not grad. */

attribute_hidden SEXP copy_scaled_gradients_vec (SEXP grad, SEXP factors)
{
    RECURSIVE_GRADIENT_APPLY (copy_scaled_gradients_vec, grad, factors);

#if 0
REprintf("cs: %d %d - %d %d - %d\n",TYPEOF(grad),TYPEOF(factors),
LENGTH(grad),LENGTH(factors),GRADIENT_WRT_LEN(grad));
#endif

    if (grad == R_NilValue)
        return R_NilValue;

    PROTECT(grad);

    if (TYPEOF(factors) != REALSXP) abort();
    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t flen = LENGTH(factors);
    R_len_t gn, gvars, glen;
    R_len_t i, j, k;
    SEXP r;

    gvars = GRADIENT_WRT_LEN(grad);
    if ((uint64_t)flen * gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();
    glen = flen * gvars;
    gn = LENGTH(grad) / gvars;
    if (LENGTH(grad) != gvars * gn) abort();
    r = allocVector (REALSXP, glen);
    SET_GRADIENT_WRT_LEN (r, gvars);
    k = 0;
    for (i = 0; i < glen; i += flen) {
        for (j = 0; j < flen; j++) {
            REAL(r)[i+j] = REAL(grad)[k + j%gn] * REAL(factors)[j];
        }
        k += gn;
    }

    UNPROTECT(1);
    return r;
}


/* Macro for building a function that applies a binary operation to
   all pairs of gradients in g1 and g2.  Protects g1 and g2, then
   unprotects them at the end, so surrounding function will need to
   protect them again if required. */

#define RECURSIVE_GRADIENT_APPLY2(fun,g1,g2,...) do { \
    PROTECT2(g1,g2); \
    if (TYPEOF(g1) == LISTSXP || TYPEOF(g2) == LISTSXP) { \
        if (g1 != R_NilValue && TYPEOF(g1) != LISTSXP) abort(); \
        if (g2 != R_NilValue && TYPEOF(g2) != LISTSXP) abort(); \
        SEXP p1, p2, res; \
        PROTECT (res = R_NilValue); \
        for (p1 = g1; p1 != R_NilValue; p1 = CDR(p1)) { \
            for (p2 = g2; p2 != R_NilValue; p2 = CDR(p2)) { \
                if (TAG(p2) == TAG(p1) && GRADINDEX(p2) == GRADINDEX(p1)) \
                    break; \
            } \
            res = cons_with_tag (fun (CAR(p1), CAR(p2), __VA_ARGS__), \
                                 res, TAG(p1)); \
            SET_GRADINDEX (res, GRADINDEX(p1)); \
            UNPROTECT_PROTECT(res); \
        } \
        for (p2 = g2; p2 != R_NilValue; p2 = CDR(p2)) { \
            for (p1 = g1; p1 != R_NilValue; p1 = CDR(p1)) { \
                if (TAG(p1) == TAG(p2) && GRADINDEX(p1) == GRADINDEX(p2)) \
                    goto next; \
            } \
            res = cons_with_tag (fun (R_NilValue, CAR(p2), __VA_ARGS__), \
                                 res, TAG(p2)); \
            SET_GRADINDEX (res, GRADINDEX(p2)); \
            UNPROTECT_PROTECT(res); \
          next: ; \
        } \
        UNPROTECT(3); \
        return res; \
    } \
    else if (TYPEOF(g1) == VECSXP && GRAD_WRT_LIST(g1) \
             || TYPEOF(g2) == VECSXP && GRAD_WRT_LIST(g2)) { \
        R_len_t m = TYPEOF(g1) == VECSXP ? LENGTH(g1) : LENGTH(g2); \
        if (g1 != R_NilValue) { \
            if (TYPEOF(g1) != VECSXP || !GRAD_WRT_LIST(g1)) abort(); \
            if (LENGTH(g1) != m) abort(); \
        } \
        if (g2 != R_NilValue) { \
            if (TYPEOF(g2) != VECSXP || !GRAD_WRT_LIST(g2)) abort(); \
            if (LENGTH(g2) != m) abort(); \
        } \
        SEXP res = PROTECT (allocVector(VECSXP,m)); \
        SET_GRAD_WRT_LIST (res, 1); \
        for (R_len_t j = 0; j < m; j++) { \
            SET_VECTOR_ELT (res, j, \
              fun (g1==R_NilValue ? R_NilValue : VECTOR_ELT(g1,j), \
                   g2==R_NilValue ? R_NilValue : VECTOR_ELT(g2,j), \
                   __VA_ARGS__)); \
        } \
        UNPROTECT(3); \
        return res; \
    } \
    UNPROTECT(2); \
} while (0)


/* Create set of gradients from grad that account for assigning an element
   with gradient v to the i'th element out of n of a vector list with gradient
   grad (creating lists as necessary).

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_list_gradient 
                       (SEXP grad, SEXP v, R_len_t i, R_len_t n)
{
#if 0
REprintf("*** subassign_list_gradient %d %d\n",i,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_list_gradient, grad, v, i, n);

    if (grad == R_NilValue && v == R_NilValue)
        return R_NilValue;

    PROTECT2(grad,v);

    if (grad == R_NilValue) 
        grad = allocVector (VECSXP, n);
    else {
        if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
        grad = dup_top_level(grad);
    }

    if (i < 0 || i >= n) abort();
    SET_VECTOR_ELT (grad, i, v);

    UNPROTECT(2);
    return grad;
}

/* Create set of gradients from grad that account for assigning an element
   with gradient v as a new element of a list at index n (so new length is n+1).

   Protects its grad and v arguments. */

SEXP attribute_hidden extend_list_gradient (SEXP grad, SEXP v, R_len_t n)
{
#if 0
REprintf("*** extend_list_gradient %d\n",n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (extend_list_gradient, grad, v, n);

    if (grad == R_NilValue && v == R_NilValue)
        return R_NilValue;

    PROTECT2(grad,v);

    SEXP res = allocVector (VECSXP, n+1);
    if (grad != R_NilValue) {
        if (TYPEOF(grad) != VECSXP) abort();
        copy_vector_elements (res, 0, grad, 0, LENGTH(grad));
    }

    SET_VECTOR_ELT (res, n, v);

#if 0
REprintf("*** extend_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Create set of gradients from grad that account for assigning an element
   with gradient v to the i'th element out of n of a numeric vector with
   gradient grad (creating lists as necessary).

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_numeric_gradient 
                       (SEXP grad, SEXP v, R_len_t i, R_len_t n)
{
#if 0
REprintf("*** subassign_numeric_gradient %d %d\n",i,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_numeric_gradient, grad, v, i, n);

    if (grad == R_NilValue && v == R_NilValue)
        return R_NilValue;

    PROTECT2(grad,v);

    R_len_t vlen = LENGTH(v);

    if ((uint64_t)n * vlen > R_LEN_T_MAX) gradient_matrix_too_large_error();

    if (grad == R_NilValue) {
        grad = allocVector (REALSXP, n * vlen);
        SET_GRADIENT_WRT_LEN (grad, vlen);
        memset (REAL(grad), 0, LENGTH(grad) * sizeof(double));
    }
    else {
        if (TYPEOF(grad) != REALSXP) abort();
        if (LENGTH(grad) != n * vlen) abort();
        grad = dup_top_level(grad);
    }

    if (i < 0 || i >= n) abort();
    copy_elements (grad, i, n, v, 0, 1, vlen);

    UNPROTECT(2);
    return grad;
}


/* Create set of gradients from grad that account for assigning an element
   with gradient v as a new element of a numeric vector at index n (so new
   length is n+1).

   Protects its grad and v arguments. */

SEXP attribute_hidden extend_numeric_gradient (SEXP grad, SEXP v, R_len_t n)
{
#if 0
REprintf("*** extend_numeric_gradient %d\n",n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (extend_numeric_gradient, grad, v, n);

    if (grad == R_NilValue && v == R_NilValue)
        return R_NilValue;

    PROTECT2(grad,v);

    R_len_t vlen = LENGTH(v);
    R_len_t m = 0;

    SEXP res = allocVector (REALSXP, (n+1) * vlen);
    SET_GRADIENT_WRT_LEN (res, vlen);

    memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    if (grad != R_NilValue) {
        if (TYPEOF(grad) != REALSXP) abort();
        if (LENGTH(grad) % vlen != 0) abort();
        m = LENGTH(grad) / vlen;
        for (R_len_t i = 0; i < m; i++)
            copy_elements (res, i, n+1, grad, i, m, vlen);
    }
    copy_elements (res, n, n+1, v, 0, 1, vlen);

#if 0
REprintf("*** extend_numeric_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Add the product of gradients in extra times a scalar factor to the set of
   gradients in base.  GRADIENT_WRT_LEN must be the same for base and extra
   (except when R_NilValue).  The length of the result is given by n, with
   base and extra recycled if shorter.

   Protects its arguments. */

attribute_hidden SEXP add_scaled_gradients (SEXP base, SEXP extra, 
                                            double factor, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY2 (add_scaled_gradients, base, extra, factor, n);

    if (base == R_NilValue && extra == R_NilValue)
        return R_NilValue;

    PROTECT2(base,extra);
    R_len_t glen, gvars, elen, blen, en, bn;
    R_len_t i, j, k, l;
    SEXP r;

    if (base == R_NilValue) {
        if (TYPEOF(extra) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(extra);
        if ((uint64_t)n*gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();
        glen = n * gvars;
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        elen = LENGTH(extra);
        en = elen / gvars;
        if (en * gvars != elen) abort();
        l = 0;
        for (i = 0; i < glen; i += n) {
            for (j = 0; j < n; j++)
                REAL(r)[i+j] = REAL(extra)[l+j%en]*factor;
            l += en;
        }
    }
    else if (extra == R_NilValue) {
        if (TYPEOF(base) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(base);
        if ((uint64_t)n*gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();
        glen = n * gvars;
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        blen = LENGTH(base);
        bn = blen / gvars;
        if (bn * gvars != blen) abort();
        k = 0;
        for (i = 0; i < glen; i += n) {
            for (j = 0; j < n; j++)
                REAL(r)[i+j] = REAL(base)[k+j%bn];
            k += bn;
        }
    }
    else {
        if (TYPEOF(base) != REALSXP) abort();
        if (TYPEOF(extra) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(base);
        if (GRADIENT_WRT_LEN(extra) != gvars) abort();
        if ((uint64_t)n*gvars > R_LEN_T_MAX) gradient_matrix_too_large_error();
        glen = n * gvars;
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        blen = LENGTH(base);
        bn = blen / gvars;
        if (bn * gvars != blen) abort();
        elen = LENGTH(extra);
        en = elen / gvars;
        if (en * gvars != elen) abort();
        k = l = 0;
        for (i = 0; i < glen; i += n) {
            for (j = 0; j < n; j++)
                REAL(r)[i+j] = REAL(base)[i+j%bn] + REAL(extra)[i+j%en]*factor;
            k += bn;
            l += en;
        }
    }   

    UNPROTECT(2);

    return r;
}


/* Add the product of gradients in extra times elements in factors to
   the set of gradients in base.  The length of factors indicates the
   length of the vector this is the gradient for; the GRADIENT_WRT_LEN
   of base and/or extra indicates the length of the vector that the
   gradient is with respect to (they must match if neither is R_NilValue).
   It is possible for base or extra to be short, with elements recycled to the
   length of factors, and around that, there may be recycling over multiple
   gradient elements.

   Protects its base and extra arguments, but caller must protect factors. */

attribute_hidden SEXP add_scaled_gradients_vec (SEXP base, SEXP extra, 
                                                SEXP factors)
{
    RECURSIVE_GRADIENT_APPLY2 (add_scaled_gradients_vec, base, extra, factors);

#if 0
REprintf("as: %d %d %d - %d %d %d\n",TYPEOF(base),TYPEOF(extra),TYPEOF(factors),
LENGTH(base),LENGTH(extra),LENGTH(factors));
#endif

    if (base == R_NilValue && extra == R_NilValue)
        return R_NilValue;

    PROTECT2(base,extra);

    if (TYPEOF(factors) != REALSXP) abort();

    R_len_t flen = LENGTH(factors);
    R_len_t en, bn, gvars;
    R_len_t i, j, k, l;
    uint64_t glen;
    SEXP r;

    if (base == R_NilValue) {
        if (TYPEOF(extra) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(extra);
        glen = (uint64_t)flen * (uint64_t)gvars;
        en = LENGTH(extra) / gvars;
        if (LENGTH(extra) != gvars * en) abort();
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        k = 0;
        for (i = 0; i < glen; i += flen) {
            for (j = 0; j < flen; j++) {
                REAL(r)[i+j] = REAL(extra)[k + j%en] * REAL(factors)[j];
            }
            k += en;
        }
    }

    else if (extra == R_NilValue) {
        if (TYPEOF(base) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(base);
        glen = (uint64_t)flen * (uint64_t)gvars;
        bn = LENGTH(base) / gvars;
        if (LENGTH(base) != gvars * bn) abort();
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        l = 0;
        for (i = 0; i < glen; i += flen) {
            for (j = 0; j < flen; j++) {
                REAL(r)[i+j] = REAL(base)[l + j%bn];
            }
            l += bn;
        }
    }

    else {
        if (TYPEOF(base) != REALSXP) abort();
        if (TYPEOF(extra) != REALSXP) abort();
        gvars = GRADIENT_WRT_LEN(extra);
        if (GRADIENT_WRT_LEN(base) != gvars) abort();
        glen = (uint64_t)flen * (uint64_t)gvars;
        bn = LENGTH(base) / gvars;
        if (LENGTH(base) != gvars * bn) abort();
        en = LENGTH(extra) / gvars;
        if (LENGTH(extra) != gvars * en) abort();
        r = allocVector (REALSXP, glen);
        SET_GRADIENT_WRT_LEN (r, gvars);
        k = l = 0;
        for (i = 0; i < glen; i += flen) {
            for (j = 0; j < flen; j++) {
                REAL(r)[i+j] = REAL(base)[l + j%bn] 
                                + REAL(extra)[k + j%en] * REAL(factors)[j];
            }
            k += en;
            l += bn;
        }
    }   

    UNPROTECT(2);

    return r;
}


/* Auxiliary functions used by backpropagate_gradients (below). */

static SEXP add_scaled_list (SEXP a, SEXP b, SEXP f)
{
    if (b == R_NilValue)
        return a;

    SEXP res;

    if (TYPEOF(b) == VECSXP) {
        R_len_t n = LENGTH(b);
        PROTECT2(a,b);
        PROTECT (res = allocVector(VECSXP,n));
        if (a == R_NilValue) {
            for (R_len_t i = 0; i < n; i++)
              SET_VECTOR_ELT (res, i, add_scaled_list (a, VECTOR_ELT(b,i), f));
        }
        else {

            if (TYPEOF(b) != VECSXP || LENGTH(b) != n) abort();
            for (R_len_t i = 0; i < n; i++) {
                SET_VECTOR_ELT (res, i, add_scaled_list (VECTOR_ELT(a,i),
                                                         VECTOR_ELT(b,i), f));
            }
        }
        UNPROTECT(3);
        return res;
    }

    if (TYPEOF(b) != REALSXP) abort();

    if (LENGTH(b) == 1 && LENGTH(f) == 1) {
        if (a == R_NilValue)
            return ScalarRealMaybeConst (*REAL(b) * *REAL(f));
        if (TYPEOF(a) != REALSXP) abort();
        if (LENGTH(a) != 1) abort();
        return ScalarRealMaybeConst (*REAL(a) + *REAL(b) * *REAL(f));
    }

    R_len_t k = GRADIENT_WRT_LEN(b);

    if (LENGTH(b) % k != 0) abort();
    R_len_t n = LENGTH(b) / k;

    if (LENGTH(f) % k != 0) abort();
    R_len_t m = LENGTH(f) / k;

    uint64_t glen = (uint64_t)n * (uint64_t)m;
    if (glen > R_LEN_T_MAX) gradient_matrix_too_large_error();

    res = allocVector (REALSXP, glen);
    SET_GRADIENT_WRT_LEN (res, m);

    matprod_mat_mat (REAL(b), REAL(f), REAL(res), n, k, m);

    if (a == R_NilValue)
        return res;

    if (TYPEOF(a) != REALSXP) abort();
    if (LENGTH(a) != glen) abort();

    for (R_len_t i = 0; i < glen; i++) 
        REAL(res)[i] += REAL(a)[i];

    return res;
}

static SEXP backup (SEXP g, SEXP a, SEXP b)
{
    if (a == R_NilValue || b == R_NilValue)
        return g;

    PROTECT3(a,b,g);

    if (TYPEOF(b) == VECSXP && GRAD_WRT_LIST(b)) {
        R_len_t n = LENGTH(b);
        if (TYPEOF(a) != VECSXP || LENGTH(a) != n) abort();
        for (R_len_t i = 0; i < n; i++)
            g = backup (g, VECTOR_ELT(a,i), VECTOR_ELT(b,i));
    }
    else {
        if (TYPEOF(a) != REALSXP) abort();
        g = add_scaled_list (g, b, a);
    }

    UNPROTECT(3);
    return g;
}


/* Backpropagate gradients, adding contributions to gradients in 'base',
   that are found by multiplying gradients of inner vars w.r.t. outer 
   vars (in 'extra') by gradients of expression value w.r.t. inner vars
   (in 'factors').  The caller must protect 'factors', but not 'base' or
   'extra'. */

static SEXP backpropagate_gradients (SEXP base, SEXP extra, SEXP factors)
{
    RECURSIVE_GRADIENT_APPLY2 (backpropagate_gradients, base, extra, factors);

    if (extra == R_NilValue || factors == R_NilValue)
        return base;

    PROTECT2(base,extra);

    SEXP res = backup (base, extra, factors);

    UNPROTECT(2);
    return res;
}


/* Create user-visible form of gradient. */

static SEXP create_gradient (SEXP result, SEXP result_grad, SEXP gv)
{
    int nv = GRADVARS_NV(gv);

    SEXP gr, gn;

    if (nv == 1)
        gr = expand_gradient (result, result_grad, VECTOR_ELT(gv,1));
    else {
        R_len_t i;
        PROTECT (gr = allocVector (VECSXP, nv));
        gn = allocVector (STRSXP, nv);
        setAttrib (gr, R_NamesSymbol, gn); 
        for (i = 0; i < nv; i++) {
            SET_STRING_ELT (gn, i, PRINTNAME (VECTOR_ELT (gv, i)));
            SET_VECTOR_ELT (gr, i, expand_gradient (result, 
                                                    VECTOR_ELT(result_grad,i),
                                                    VECTOR_ELT(gv,nv+i)));
        }
        UNPROTECT(1);
    }

    return gr;
}


/* with gradient (op==0), track gradient (op==1), and back gradient (op==2). */

static SEXP do_gradient (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int need_grad = PRIMVAL(op) != 2 || (variant & VARIANT_GRADIENT);

    /* Check for errors in argument structure, and store symbols for variables
       for GRADVARS in 'gv'. */

    int nv = length(args) - 1;
    if (nv < 1)
        errorcall (call, _("no gradient variables"));

    SEXP gv = allocVector (VECSXP, GRADVARS_LEN(nv));
    SET_NAMEDCNT_MAX(gv);
    PROTECT(gv);

    SEXP p, q, r;
    int vr;
    int i;

    for (i = 0, p = args; i < nv; i++, p = CDR(p)) {
        if (CAR(p) == R_MissingArg || CAR(p) == R_MissingUnder)
            errorcall (call, _("no gradient variables"));
        SEXP t = TAG(p)==R_NilValue && TYPEOF(CAR(p))==SYMSXP ? CAR(p) 
                  : TAG(p);
        if (t == R_NilValue)
            errorcall (call, _("gradient target must be named, or be symbol"));
        for (r = CDR(p); CDR(r) != R_NilValue; r = CDR(r)) {
            SEXP t2 = TAG(r)==R_NilValue && TYPEOF(CAR(r))==SYMSXP ? CAR(p) 
                       : TAG(r);
            if (t2 == t)
                errorcall (call, _("repeated name for gradient variable"));
        }
        SET_VECTOR_ELT(gv, i, t);
    }

    if (nv > 255)
        errorcall (call, _("too many variables (max 255)"));

    /* Create new environment, empty for now. */

    SEXP newenv = NewEnvironment (R_NilValue, R_NilValue, env);
    PROTECT(newenv);

    /* Evaluate initial values assigned to variables, and put in binding 
       cells.  Unless the gradient isn't needed, also put identity gradient
       in binding cells, also put in 'gv' (which will become GRADVARS).
       Store the gradients of initial variable values in 'vargrad' (but
       only if need gradients). */

    SEXP frame = R_NilValue;
    SEXP vargrad[nv];

    PROTECT_INDEX fix;
    PROTECT_WITH_INDEX(frame,&fix);

    vr = need_grad ? VARIANT_GRADIENT | VARIANT_PENDING_OK : VARIANT_PENDING_OK;

    for (i = 0, p = args; i < nv; i++, p = CDR(p)) {
        SEXP val = evalv (CAR(p), env, vr);
        REPROTECT (frame = cons_with_tag (val, frame, VECTOR_ELT(gv,i)), fix);
        if (need_grad) {
            PROTECT (vargrad[i] = R_variant_result ? R_gradient : R_NilValue);
            SEXP id_grad = PROTECT (make_id_grad (val));
            if (id_grad != R_NilValue) {
                SET_VECTOR_ELT (gv, nv+i, id_grad);
                SEXP gcell = cons_with_tag (id_grad, R_NilValue, newenv);
                SET_GRADIENT_IN_CELL (frame, gcell);
                SET_GRADINDEX (gcell, i+1);
            }
        }
        INC_NAMEDCNT(val);
    }

    /* Finish setting up new environment. */

    SET_FRAME(newenv,frame);
    SET_RDEBUG(newenv,RDEBUG(env));
    set_symbits_in_env (newenv);
    if (need_grad) {
        SET_STORE_GRAD(newenv,1);
        SET_GRADVARS(newenv,gv);
    }

    /* Evaluate body. */

    vr = variant & VARIANT_PENDING_OK;
    if (need_grad)
        vr |= VARIANT_GRADIENT;
              
    SEXP result = evalv (CAR(p), newenv, vr);
    PROTECT_INDEX rix;                   
    PROTECT_WITH_INDEX(result,&rix);

    int res_has_grad = R_variant_result & VARIANT_GRADIENT_FLAG;

    SEXP result_grad = need_grad ? get_gradient (newenv) : R_NilValue;
    PROTECT(result_grad);

    /* For 'with gradient', attach gradient attribute. */
    
    if (PRIMVAL(op) == 0) { /* with gradient */

        if (NAMEDCNT_GT_0(result))
            REPROTECT (result = duplicate(result), rix);

        SEXP gr = create_gradient (result, result_grad, gv);
        setAttrib (result, R_GradientSymbol, gr);
        INC_NAMEDCNT(gr);
    }

    /* Propage gradients backwards with the chain rule. */

    R_variant_result = 0;

    if (need_grad && res_has_grad && (variant & VARIANT_GRADIENT)) {

        SEXP other_grads = get_other_gradients (newenv);

        if (result_grad != R_NilValue) {
            PROTECT(other_grads);
            for (i = 0; i < nv; i++) {
                SEXP g = vargrad[i];
                SEXP r = nv > 1 ? VECTOR_ELT(result_grad,i) : result_grad;
                if (g != R_NilValue && r != R_NilValue)
                    other_grads = backpropagate_gradients (other_grads, g, r);
            }
            UNPROTECT(1);
        }

        if (other_grads != R_NilValue) {
            R_gradient = other_grads;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }

    }

    SET_GRADVARS(newenv,R_NilValue);  /* just in case it's somehow referenced */

    if (need_grad)
        UNPROTECT(5+2*nv);
    else
        UNPROTECT(5);

    return result;
}


static SEXP do_compute_grad (SEXP call, SEXP op, SEXP args, SEXP env,
                             int variant)
{
    int na = length(args);
    if (na < 3 || (na & 1) != 1)
        errorcall (call, _("invalid argument list"));

    int nv = na / 2;

    SEXP skip = nthcdr (args, nv);
    SEXP grads = CDR(skip);
    SEXP body = CAR(skip);

    SEXP p, q, r;

    /* Check for errors in argument structure. */

    for (p = args, q = grads; p != skip; p = CDR(p), q = CDR(q)) {
        SEXP t = TAG(p)==R_NilValue && TYPEOF(CAR(p))==SYMSXP ? CAR(p) 
                  : TAG(p);
        if (t == R_NilValue)
            errorcall (call, _("gradient target must be named, or be symbol"));
        for (r = CDR(p); r != skip; r = CDR(r)) {
            SEXP t2 = TAG(r)==R_NilValue && TYPEOF(CAR(r))==SYMSXP ? CAR(p) 
                       : TAG(r);
            if (t2 == t)
                errorcall (call, _("repeated name for gradient variable"));
        }
        if (TAG(q) != R_NilValue && TAG(q) != t)
            errorcall (call, 
           _("name for gradient expression must match corresponding variable"));
    }

    if (nv > 255) 
        errorcall (call, _("too many variables (max 255)"));

    /* Create new environment, with gradient variables, storing gradients
       of their expressions if gradient of final result will be needed. */

    SEXP vargrad [variant & VARIANT_GRADIENT ? nv : 1];  /* 0 not allowed */
    int vgi;  /* index in vargrad */

    SEXP newenv = NewEnvironment (R_NilValue, R_NilValue, env);
    SET_RDEBUG(newenv,RDEBUG(env));
    PROTECT(newenv);

    SEXP frame = R_NilValue;
    R_symbits_t bits = 0;
    vgi = 0;

    int any_grad = 0;

    for (p = args, q = grads; p != skip; p = CDR(p), q = CDR(q)) {

        SEXP t = TAG(p)==R_NilValue && TYPEOF(CAR(p))==SYMSXP ? CAR(p) : TAG(p);
        SEXP val = evalv (CAR(p), env, 
                          (variant & VARIANT_GRADIENT) | VARIANT_PENDING_OK);
        frame = cons_with_tag (val, frame, t);
        bits |= SYMBITS(t);
        INC_NAMEDCNT(val);
        SET_FRAME (newenv, frame);  /* protects frame */

        if (variant & VARIANT_GRADIENT) {
            if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                PROTECT (vargrad[vgi] = R_gradient);
                any_grad = 1;
            }
            else {
                PROTECT (vargrad[vgi] = R_NilValue);
            }
        }
        vgi += 1;

    }

    SET_ENVSYMBITS (newenv, bits);

    /* Evaluate body. */

    SEXP result;

    PROTECT (result = evalv (body, newenv, variant & ~VARIANT_GRADIENT));
    int vr = R_variant_result;

    /* Compute gradients of result if requested, and might be non-null, using
       the chain rule applied with the given expressions and the previous 
       gradients. */

    if (any_grad) {
        SEXP resgrad = R_NilValue;
        PROTECT(resgrad);
        vgi = 0;
        for (q = grads; q != R_NilValue; q = CDR(q)) {
            if (vargrad[vgi] != R_NilValue) {
                SEXP gval;
                PROTECT (gval = evalv (CAR(q), newenv, VARIANT_PENDING_OK));
                if (! match_structure (result, gval))
                    errorcall (call, 
                      _("computed gradient does not match type of value"));
                resgrad = backpropagate_gradients (resgrad, vargrad[vgi], gval);
                UNPROTECT(2);  /* gval, old resgrad */
                PROTECT(resgrad);
            }
            vgi += 1;
        }
        vr |= VARIANT_GRADIENT_FLAG;
        R_gradient = resgrad;
        UNPROTECT(1);  /* resgrad */
    }

    if (variant & VARIANT_GRADIENT)
        UNPROTECT(nv);  /* vargrad */

    UNPROTECT(2);  /* newenv, result */

    R_variant_result = vr;
    return result;
}


static SEXP do_gradient_of(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    checkArity (op, args);

    if (TYPEOF(GRADVARS(env)) != VECSXP)
        errorcall (call, _("gradient_of called outside gradient construct"));

    SEXP result = PROTECT (evalv (CAR(args), env, VARIANT_GRADIENT));
    SEXP result_grad = PROTECT (get_gradient(env));

    SEXP r = create_gradient (result, result_grad, GRADVARS(env));

    R_variant_result = 0;
    UNPROTECT(2);
    return r;
}


static SEXP do_no_gradient(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    checkArity (op, args);

    return evalv (CAR(args), env, variant & ~VARIANT_GRADIENT);
}


/* .Internal, for debugging gradient implementation. */

static SEXP do_all_gradients_of (SEXP call, SEXP op, SEXP args, SEXP env,
                                 int variant)
{
    checkArity (op, args);

    (void) evalv (CAR(args), env, VARIANT_GRADIENT);

    SEXP r = R_NilValue;
    if (R_variant_result & VARIANT_GRADIENT_FLAG) {
        SEXP s = R_gradient;
        PROTECT(s);
        while (s != R_NilValue) {
            SEXP ix = PROTECT(ScalarIntegerMaybeConst(GRADINDEX(s)));
            r = CONS (TAG(s), CONS (ix, CONS (CAR(s), r)));
            UNPROTECT(1);
            s = CDR(s);
        }
        UNPROTECT(1);
    }

    R_variant_result = 0;
    return r;
}


/* Trace tracking of the gradients in R_gradient. */

attribute_hidden void Rf_gradient_trace (SEXP call)
{
    REprintf("GRADIENT TRACE: ");

    SEXP p;
    for (p = R_gradient; p != R_NilValue; p = CDR(p)) {
        int ix = GRADINDEX(p);
        SEXP env = TAG(p);
        SEXP gv = GRADVARS(env);
        if (gv==R_NoObject || TYPEOF(gv)!=VECSXP || ix<1 || ix>GRADVARS_NV(gv))
            REprintf("?? ");
        else
            REprintf("%s ",CHAR(PRINTNAME(VECTOR_ELT(gv,ix-1))));
    }

    RCNTXT *cptr;

    REprintf (": ");

    if (call != R_NilValue && TYPEOF(CAR(call)) == SYMSXP)
        REprintf ("\"%s\" ", CHAR(PRINTNAME(CAR(call))));

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
            REprintf ("\"%s\" ",
               TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) : "<Anonymous>");
	}
    }

    REprintf("\n");
}


/* .Internal, for debugging gradient implementation. */

static SEXP do_tracking_gradients (SEXP call, SEXP op, SEXP args, SEXP env,
                                   int variant)
{
    checkArity (op, args);

    return ScalarLogicalMaybeConst (STORE_GRAD(env));
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_gradient[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"with gradient", do_gradient,  0,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"track gradient", do_gradient, 1,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"back gradient", do_gradient,  2,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"compute gradient", do_compute_grad,  0, 1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"gradient_of", do_gradient_of, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"no_gradient", do_no_gradient, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"all_gradients_of", do_all_gradients_of, 1, 1210, 1,	{PP_FUNCALL, PREC_FN,	0}},

{"tracking_gradients", do_tracking_gradients, 1, 1210, 0, {PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
