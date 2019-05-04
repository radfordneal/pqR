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
#include <helpers/helpers-app.h>


static void R_NORETURN gradient_matrix_too_large_error (void) {
    error (_("gradient matrix would be too large"));
}

static inline SEXP alloc_list_gradient (R_len_t n)
{
    return allocVector (VECSXP, n);
}

static SEXP alloc_numeric_gradient (R_len_t gvars, R_len_t n)
{
    if ((uint64_t) gvars * n > R_LEN_T_MAX)
        error (_("gradient matrix would be too large"));

    SEXP res = allocVector (REALSXP, gvars * n);
    SET_GRADIENT_WRT_LEN (res, gvars);

    return res;
}


static void inc_gradient_namedcnt (SEXP v)
{
#if 0
REprintf("inc_gradient_namedcnt: %d %d %d %d.%d %p\n",
TYPEOF(v),length(v),NAMEDCNT(v),CPTR_FROM_SEXP(v)/64,CPTR_FROM_SEXP(v)%64,v);
#endif
    switch (TYPEOF(v)) {
    case LISTSXP:
        for ( ; v != R_NilValue; v = CDR(v)) {
            INC_NAMEDCNT(v);
            if (CAR(v) != R_NilValue)
                inc_gradient_namedcnt(CAR(v));
        }
        break;
    case VECSXP: ;
        INC_NAMEDCNT(v);
        R_len_t len = LENGTH(v);
        R_len_t i;
        for (i = 0; i < len; i++)
            if (VECTOR_ELT(v,i) != R_NilValue)
                inc_gradient_namedcnt (VECTOR_ELT(v,i));
        break;
    case REALSXP:
        INC_NAMEDCNT(v);
        if (JACOBIAN_CACHED_AS_ATTRIB(v))
            INC_NAMEDCNT(ATTRIB_W(v));
        break;
    }
}


static void dec_gradient_namedcnt (SEXP v)
{
#if 0
REprintf("dec_gradient_namedcnt: %d %d %d %d.%d %p\n",
TYPEOF(v),length(v),NAMEDCNT(v),CPTR_FROM_SEXP(v)/64,CPTR_FROM_SEXP(v)%64,v);
#endif
    switch (TYPEOF(v)) {
    case LISTSXP:
        for ( ; v != R_NilValue; v = CDR(v)) {
            DEC_NAMEDCNT(v);
            if (CAR(v) != R_NilValue)
                dec_gradient_namedcnt(CAR(v));
        }
        break;
    case VECSXP: ;
        DEC_NAMEDCNT(v);
        R_len_t len = LENGTH(v);
        R_len_t i;
        for (i = 0; i < len; i++)
            if (VECTOR_ELT(v,i) != R_NilValue)
                dec_gradient_namedcnt (VECTOR_ELT(v,i));
        break;
    case REALSXP:
        DEC_NAMEDCNT(v);
        if (JACOBIAN_CACHED_AS_ATTRIB(v))
            DEC_NAMEDCNT(ATTRIB_W(v));
        break;
    }
}


void SET_GRADIENT_IN_CELL (SEXP x, SEXP v)
{
    if (ATTRIB_W(x) != R_NilValue && !HAS_GRADIENT_IN_CELL(x)) abort();
    if (v != R_NilValue && TYPEOF(v) != LISTSXP) abort();

    if (ATTRIB_W(x) != v) {
        dec_gradient_namedcnt (ATTRIB_W(x));
        SET_ATTRIB_TO_ANYTHING (x,v);
        inc_gradient_namedcnt (v);
    }
}


void SET_GRADIENT_IN_CELL_NR (SEXP x, SEXP v)
{
    if (ATTRIB_W(x) != R_NilValue && !HAS_GRADIENT_IN_CELL(x)) abort();
    if (v != R_NilValue && TYPEOF(v) != LISTSXP) abort();

    SET_ATTRIB_TO_ANYTHING (x,v);
}


/* Expand a compact Jacobian representation to a full Jacobian matrix. 

   Protects its grad argument. */

static SEXP expand_to_full_jacobian (SEXP grad)
{
    if (TYPEOF(grad) != REALSXP) abort();

    if (JACOBIAN_CACHED_AS_ATTRIB(grad))
        return ATTRIB_W(grad);

    if (DIAGONAL_JACOBIAN(grad)) {
        R_len_t gvars = GRADIENT_WRT_LEN(grad);
        PROTECT(grad);
        SEXP new = alloc_numeric_gradient (gvars, gvars);
        memset (REAL(new), 0, LENGTH(new) * sizeof(double));
        R_len_t i, j;
        if (LENGTH(grad) == 1) {
            double d = REAL(grad)[0];
            i = 0; j = 0;
            for (;;) {
                REAL(new)[j] = d;
                i += 1;
                if (i == gvars) break;
                j += gvars+1;
            }
        }
        else {
            i = 0; j = 0;
            for (;;) {
                REAL(new)[j] = REAL(grad)[i];
                i += 1;
                if (i == gvars) break;
                j += gvars+1;
            }
        }

        SET_ATTRIB_TO_ANYTHING(grad,new);
        SET_JACOBIAN_CACHED_AS_ATTRIB(grad,1);
        SET_NAMEDCNT(new,NAMEDCNT(grad));

        UNPROTECT(1);
        return new;
    }

    else 
        return grad;
}


/* Copy a jacobian with scaling, either a full jacobian or diagonal. 
   Gradient to copy with scaling is in grad, for vector of length gn,
   with respect to gvars variables.  Result should be a gradient for
   a vector of length n with gvars variables.  Scaling factors are in f, with 
   length flen, not necessarily equal to n.  When gn or flen are less than
   n, recycling is done as necessary.

   Protects the grad argument. */

static SEXP copy_scaled_jacobian (SEXP grad, R_len_t gvars, R_len_t gn,
                                  double *f, R_len_t flen, R_len_t n)
{
    R_len_t i, j, k;
    SEXP r;

    if (flen == 1 && *f == 1.0 && gn == n)
        return grad;

    if (DIAGONAL_JACOBIAN(grad) && gn != n)
        grad = expand_to_full_jacobian (grad);


    PROTECT(grad);

    if (DIAGONAL_JACOBIAN(grad)) {

        R_len_t glen = JACOBIAN_VALUE_LENGTH(grad);
        if (flen == 1) {
            r = allocVector (REALSXP, glen);
            SET_GRADIENT_WRT_LEN (r, gvars);
            SET_DIAGONAL_JACOBIAN (r, 1);
            double d = *f;
            for (i = 0; i < glen; i++)
                REAL(r)[i] = REAL(grad)[i] * d;
        }
        else {
            r = allocVector (REALSXP, n);
            SET_GRADIENT_WRT_LEN (r, gvars);
            SET_DIAGONAL_JACOBIAN (r, 1);
            if (flen >= n && glen == 1) {
                double g = REAL(grad)[0];
                for (j = 0; j < n; j++)
                    REAL(r)[j] = g * f[j];
            }
            else if (flen >= n && glen >= n) {
                for (j = 0; j < n; j++)
                    REAL(r)[j] = REAL(grad)[j] * f[j];
            }
            else {
                R_len_t jg = 0, jf = 0;
                for (j = 0; j < n; j++) {
                    REAL(r)[j] = REAL(grad)[jg] * f[jf];
                    if (++jg == glen) jg = 0;
                    if (++jf == flen) jf = 0;
                }
            }
        }
    }

    else if (n == 1 && gvars == 1)  {  /* 1-by-1 jacobian */

        r = ScalarRealMaybeConst (*REAL(grad) * *f);
    }

    else {  /* full jacobian, not 1-by-1 */

        r = alloc_numeric_gradient (gvars, n);
        R_len_t glen = n * gvars;
        k = 0;
        if (gn == n && flen == 1) {
            double d = *f;
            for (i = 0; i < glen; i++)
                REAL(r)[i] = REAL(grad)[i] * d;
        }
        else if (gn == 1 && flen == 1) {
            double d = *f;
            for (i = 0; i < glen; i += n) {
                double g = REAL(grad)[k] * d;
                for (j = 0; j < n; j++)
                    REAL(r)[i+j] = g;
                k += 1;
            }
        }
        else if (gn == 1 && flen >= n) {
            for (i = 0; i < glen; i += n) {
                double g = REAL(grad)[k];
                for (j = 0; j < n; j++)
                    REAL(r)[i+j] = g * f[j];
                k += 1;
            }
        }
        else if (gn >= n && flen == 1) {
            double d = *f;
            for (i = 0; i < glen; i += n) {
                for (j = 0; j < n; j++)
                    REAL(r)[i+j] = REAL(grad)[k + j] * d;
                k += gn;
            }
        }
        else if (gn >= n && flen >= n) {
            for (i = 0; i < glen; i += n) {
                for (j = 0; j < n; j++)
                    REAL(r)[i+j] = REAL(grad)[k + j] * f[j];
                k += gn;
            }
        }
        else {  /* general case */
            for (i = 0; i < glen; i += n) {
                R_len_t jg = 0, jf = 0;
                for (j = 0; j < n; j++) {
                    REAL(r)[i+j] = REAL(grad)[k + jg] * f[jf];
                    if (++jg == gn) jg = 0;
                    if (++jf == flen) jf = 0;
                }
                k += gn;
            }
        }
    }

    UNPROTECT(1);
    return r;
}


/* Expand the structure of 'grad' to be a full gradient for 'value' by
   replacing NULL elements that correspond to non-NULL elements by the
   appropriate zero Jacobian.  The 'idg' argument is the identity
   gradient to use as a skeleton - it may have top VECSXP levels with
   GRAD_WRT_LIST set, and just below that, GRADIENT_WRT_LEN is set to the 
   length of the numeric vector that that part of the gradient (possibly
   a list) is with respect to.

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
        if (grad != R_NilValue) {
            R_len_t i;
            for (i = 0; i < n; i++)
                SET_VECTOR_ELT(res, i, expand_gradient (value, 
                                        VECTOR_ELT(grad,i), VECTOR_ELT(idg,i)));
        }
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
                SET_VECTOR_ELT (res, i, expand_gradient (VECTOR_ELT(value,i),
                                         R_NilValue, idg));
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
            SET_VECTOR_ELT (res, i, expand_gradient (VECTOR_ELT(value,i), 
                                      VECTOR_ELT(grad,i), idg));
        }
        UNPROTECT(1);

        return res;
    }

    if (TYPEOF(grad) == REALSXP) {

        if (TYPEOF(value) != REALSXP) abort();

        R_len_t vlen = LENGTH(value);
        R_len_t glen = GRADIENT_WRT_LEN(idg);
        uint64_t Jlen = (uint64_t)vlen * (uint64_t)glen;

        if (JACOBIAN_LENGTH(grad) != Jlen) abort();

        if (glen != 1) {
            PROTECT(grad);
            SEXP dim = allocVector (INTSXP, 2);
            INTEGER(dim)[0] = vlen;
            INTEGER(dim)[1] = glen;
            grad = expand_to_full_jacobian (grad);
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
   the Jacobian matrices themselves.  The "identity" gradient for a
   numeric vector is an identity matrix, represented compactly; for
   a list, it is a list with all but one element NULL, with that element 
   being the recursively-defined "identity" value. */

static SEXP make_id_numeric (SEXP v)
{
    R_len_t vlen = LENGTH(v);
    SEXP res;

    if (vlen == 1) {
        res = R_ScalarRealOne;
    }
    else {
        res = allocVector1REAL();
        SET_GRADIENT_WRT_LEN (res, vlen);
        SET_DIAGONAL_JACOBIAN (res, 1);
        REAL(res)[0] = 1;
    }

    return res;
}

static SEXP make_id_recursive (SEXP val, SEXP top)
{
    R_len_t n = LENGTH(val);

    SEXP res = PROTECT(alloc_list_gradient(n));
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
            SET_VECTOR_ELT (bot, i, alloc_list_gradient (LENGTH(v)));
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
        PROTECT (top = alloc_list_gradient(LENGTH(val)));
        res = make_id_recursive (val, top);
        UNPROTECT(1);
    }

    return res;
}


/* Test whether the structure of a gradient value matches the structure of
   what it is the gradient of.  Returns R_NoObject if doesn't match.  Returns
   'grad' with GRADIENT_WRT_LEN fields set in Jacobian matrices (possibly
   duplicating to do so). */

static SEXP match_structure (SEXP val, SEXP grad, R_len_t gvars)
{
    if (TYPEOF(val) == REALSXP) {
        if (TYPEOF(grad) != REALSXP)
            return R_NoObject;
        if (LENGTH(grad) != (uint64_t) gvars * LENGTH(val) != 0)
            return R_NoObject;
        if (NAMEDCNT_GT_0(grad)) grad = duplicate(grad);
        SET_GRADIENT_WRT_LEN (grad, gvars);
    }
    else if (TYPEOF(val) == VECSXP) {
        if (TYPEOF(grad) != VECSXP)
            return R_NoObject;
        if (LENGTH(val) != LENGTH(grad))
            return R_NoObject;
        PROTECT(grad);
        R_len_t i;
        for (i = 0; i < LENGTH(val); i++) {
            SEXP e;
            e = match_structure (VECTOR_ELT(val,i), VECTOR_ELT(grad,i), gvars);
            if (e == R_NoObject)
                return R_NoObject;
            if (e != VECTOR_ELT(grad,i)) {
                if (NAMEDCNT_GT_0(grad)) {
                    grad = duplicate(grad);
                    UNPROTECT_PROTECT(grad);
                }
                SET_VECTOR_ELT (grad, i, e);
            }
        }
        UNPROTECT(1);
    }
    else {
        if (grad != R_NilValue)
            return R_NoObject;
    }

    return grad;
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
   need to protect it again if required.  The NO_EXPAND version does not
   expand compact gradient representations. */

#define RECURSIVE_GRADIENT_APPLY(fun,grad,...) do { \
    RECURSIVE_GRADIENT_APPLY_NO_EXPAND(fun,grad,__VA_ARGS__); \
    if (TYPEOF(grad) == REALSXP && DIAGONAL_JACOBIAN(grad)) \
        grad = expand_to_full_jacobian(grad); \
} while (0)

#define RECURSIVE_GRADIENT_APPLY_NO_EXPAND(fun,grad,...) do { \
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
        for (R_len_t jjj = 0; jjj < m; jjj++) \
            SET_VECTOR_ELT (res, jjj, fun(VECTOR_ELT(grad,jjj),__VA_ARGS__)); \
        UNPROTECT(2); \
        return res; \
    } \
    UNPROTECT(1); \
    if (grad == R_NilValue) \
        return R_NilValue; \
} while (0)


/* Create set of gradients from recycling a vector list to be of length n.
   Protects its grad argument. */

SEXP attribute_hidden copy_list_recycled_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("copy_list_recycled_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (copy_list_recycled_gradient, grad, n);
	
    if (TYPEOF(grad) != VECSXP) abort();
    R_len_t k = LENGTH(grad);

    SEXP res = alloc_list_gradient (n);
    PROTECT(res);

    copy_vector_elements (res, 0, grad, 0, n>k ? k : n);

    for (R_len_t i = k; i < n; i++) {
        SET_VECTOR_ELT (res, i, VECTOR_ELT(res,i-k));
    }

    UNPROTECT(1);
#if 0
REprintf("copy_list_recycled_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from repeating each element of a vector list,
   ending up as length n.  Protects its grad argument. */

SEXP attribute_hidden rep_each_list_gradient (SEXP grad, SEXP t, R_len_t n)
{
#if 0
REprintf("rep_each_list_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (rep_each_list_gradient, grad, t, n);
	
    if (TYPEOF(grad) != VECSXP) abort();
    R_len_t l = LENGTH(grad);
    R_len_t i, j, k;

    SEXP res = alloc_list_gradient (n);
    PROTECT(res);

    if (LENGTH(t) == 1) {
        R_len_t each = INTEGER(t)[0];
        for (i = 0, j = 0; i < n; j++) {
            if (j >= l) j = 0;
            SEXP v = VECTOR_ELT (grad, j);
            for (k = each; k > 0; k--) {
                if (i >= n) abort();
                SET_VECTOR_ELT (res, i, v);
                i += 1;
            }
        }
    }
    else {
        int *eachv = INTEGER(t);
        for (i = 0, j = 0; i < n; j++) {
            if (j >= l) j = 0;
            SEXP v = VECTOR_ELT (grad, j);
            for (k = eachv[j]; k > 0; k--) {
                if (i >= n) abort();
                SET_VECTOR_ELT (res, i, v);
                i += 1;
            }
        }
    }

    UNPROTECT(1);
#if 0
REprintf("rep_each_list_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from repeating each element of a numeric vector,
   ending up as length n.  Protects its grad argument. */

SEXP attribute_hidden rep_each_numeric_gradient (SEXP grad, SEXP t, R_len_t n)
{
#if 0
REprintf("rep_each_numeric_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (rep_each_numeric_gradient, grad, t, n);
	
    if (TYPEOF(grad) != REALSXP) abort();
    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t gn = JACOBIAN_LENGTH(grad) / gvars;
    R_len_t i, j, k, h;

    SEXP res = alloc_numeric_gradient (gvars, n);
    PROTECT(res);


    if (LENGTH(t) == 1) {
        R_len_t each = INTEGER(t)[0];
        for (i = 0, j = 0; i < n; j++) {
            if (j >= gn) j = 0;
            for (k = each; k > 0; k--) {
                if (i >= n) abort();
                for (h = 0; h < gvars; h++) {
                    double v = REAL(grad)[h*gn+j];
                    REAL(res)[h*n+i] = v;
                }
                i += 1;
            }
        }
    }
    else {
        int *eachv = INTEGER(t);
        for (i = 0, j = 0; i < n; j++) {
            if (j >= gn) j = 0;
            for (k = eachv[j]; k > 0; k--) {
                if (i >= n) abort();
                for (h = 0; h < gvars; h++) {
                    double v = REAL(grad)[h*gn+j];
                    REAL(res)[h*n+i] = v;
                }
                i += 1;
            }
        }
    }

done:
    UNPROTECT(1);
#if 0
REprintf("rep_each_numeric_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from recycling a vector list to be of length n,
   filling in a matrix by row.  Protects its grad argument. */

SEXP attribute_hidden copy_list_recycled_byrow_gradient 
                        (SEXP grad, R_len_t nr, R_len_t n)
{
#if 0
REprintf("copy_list_recycled_byrow_gradient %d %d\n",nr,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (copy_list_recycled_byrow_gradient, grad, nr, n);
	
    if (TYPEOF(grad) != VECSXP) abort();
    R_len_t ng = LENGTH(grad);
    R_len_t nc = n / nr;

    SEXP res = alloc_list_gradient (n);
    PROTECT(res);

    R_len_t n_1 = n-1;
    R_len_t i, j, k;

    for (i = 0, j = 0; i <= n_1; i++, j += nc) {
        if (j > n_1) j -= n_1;
        SET_VECTOR_ELT (res, i, VECTOR_ELT (grad, j % ng));
    }

    UNPROTECT(1);
#if 0
REprintf("copy_list_recycled_byrow_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from recycling a numeric vector to be of length n.
   Protects its grad argument. */

SEXP attribute_hidden copy_numeric_recycled_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("copy_numeric_recycled_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (copy_numeric_recycled_gradient, grad, n);
	
    if (TYPEOF(grad) != REALSXP) abort();
    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t k = JACOBIAN_LENGTH(grad) / gvars;

    SEXP res = alloc_numeric_gradient (gvars, n);
    PROTECT(res);

    R_len_t m = n > k ? k : n;
    for (R_len_t h = 0; h < gvars; h++) {
        for (R_len_t i = 0; i < m; i++)
            REAL(res)[h*n+i] = REAL(grad)[h*k+i];
        for (R_len_t i = k; i < n; i++)
            REAL(res)[h*n+i] = REAL(res)[h*n+i-k];
    }

    UNPROTECT(1);
#if 0
REprintf("copy_numeric_recycled_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from recycling a numeric vector to be of length n,
   filling in a matrix by row.  Protects its grad argument. */

SEXP attribute_hidden copy_numeric_recycled_byrow_gradient 
                                   (SEXP grad, R_len_t nr, R_len_t n)
{
#if 0
REprintf("copy_numeric_recycled_byrow_gradient %d %d\n",nr,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY(copy_numeric_recycled_byrow_gradient, grad, nr, n);
	
    if (TYPEOF(grad) != REALSXP) abort();
    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t ng = JACOBIAN_LENGTH(grad) / gvars;
    R_len_t nc = n / nr;

    SEXP res = alloc_numeric_gradient (gvars, n);
    PROTECT(res);

    R_len_t n_1 = n-1;
    R_len_t i, k, h;
    unsigned j;

    for (h = 0; h <gvars; h++) {
        for (i = 0, j = 0; i <= n_1; i++, j += nc) {
            if (j > n_1) j -= n_1;
            REAL(res) [h*n + i] = REAL(grad) [h*ng + j % ng];
        }
    }

    UNPROTECT(1);
#if 0
REprintf("copy_numeric_recycled_byrow_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from converting a numeric vector of length n
   to a list.  Protects its grad argument. */

SEXP attribute_hidden as_list_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("as_list_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (as_list_gradient, grad, n);
	
    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN(grad);

    if (LENGTH(grad) != n * gvars) abort();

    SEXP res = alloc_list_gradient (n);
    PROTECT(res);

    for (R_len_t i = 0; i < n; i++) {
        SEXP r = alloc_numeric_gradient (gvars, 1);
        SET_VECTOR_ELT (res, i, r);
        memcpy (REAL(r), REAL(grad)+i*gvars, gvars * sizeof(double));
    }

    UNPROTECT(2);
#if 0
REprintf("as_list_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from converting a vector list of length n
   to a numeric vector.  Protects its grad argument. */

SEXP attribute_hidden as_numeric_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("as_numeric_gradient %d\n",n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (as_numeric_gradient, grad, n);
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t gvars = -1;
    R_len_t i, j, h;

    for (i = 0; i < n; i++) {
        SEXP e = VECTOR_ELT(grad,i);
        if (TYPEOF(e) == REALSXP) {
            R_len_t gv = GRADIENT_WRT_LEN(e);
            if (LENGTH(e) != gv) abort();
            if (gvars == -1) 
                gvars = gv;
            else 
                if (gvars != gv) abort();
        }
    }

    if (gvars == -1) {
        UNPROTECT(1);
        return R_NilValue;
    }

    SEXP res = alloc_numeric_gradient (gvars, n);
    PROTECT(res);

    j = 0;
    for (i = 0; i < n; i++) {
        SEXP e = VECTOR_ELT(grad,i);
        if (TYPEOF(e) != REALSXP)
           for (h = 0; h < gvars; h++) REAL(res)[j++] = 0;
        else
           for (h = 0; h < gvars; h++) REAL(res)[j++] = REAL(e)[h];
    }

    UNPROTECT(2);
#if 0
REprintf("as_numeric_gradient end\n");
R_inspect(res); REprintf("==\n");
#endif
    return res;
}


/* Create set of gradients from subsetting the i'th element of gradients for 
   a vector list of length n.  Used for [[.]].  Protects its grad argument. */

SEXP attribute_hidden subset2_list_gradient (SEXP grad, R_len_t i, R_len_t n)
{
#if 0
REprintf("subset2_list_gradient %d %d\n",i,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (subset2_list_gradient, grad, i, n);
	
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
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    if (i < 0 || j < 0) abort();

    if (i >= n || j < i)
        return R_NilValue;

    PROTECT(grad);

    SEXP res = alloc_list_gradient (j-i+1);

    if (j >= n) j = n-1;

    for (R_len_t k = i; k <= j; k++)
        SET_VECTOR_ELT (res, k-i, VECTOR_ELT(grad,k));
#if 0
REprintf("subset_range_list_gradient end\n");
R_inspect(grad); REprintf("==\n");
#endif

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
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    int k = LENGTH(indx);
    SEXP res = alloc_list_gradient (k);
    
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

    if (i < 0 || j < 0) abort();

    if (i >= n || j < i)
        return R_NilValue;

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t glen = LENGTH(grad);

    if (glen == n && i == j)
        return ScalarReal (REAL(grad)[i]);

    PROTECT(grad);

    if (glen % n != 0) abort();
    R_len_t gvars = glen/n;
    R_len_t m = j-i+1;
    R_len_t t = m;
    SEXP res = alloc_numeric_gradient (gvars, m);
    if (j >= n) {
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
        t = n - i;
    }
    R_len_t k, l;
    k = l = 0;
    while (k < glen) {
        copy_elements (res, l, 1, grad, k+i, 1, t);
        k += n;
        l += m;
    }

#if 0
REprintf("subset_range_numeric_gradient end\n");
R_inspect(res); REprintf("--\n");
#endif

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

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    int m = LENGTH(indx);

    SEXP res = alloc_numeric_gradient (gvars, m);
    
    for (R_len_t j = 0; j < m; j++) {
        R_len_t i = INTEGER(indx)[j];
        if (i >= 1 && i <= n) {
            for (R_len_t k = 0; k < gvars; k++)
                REAL(res)[j+k*m] = REAL(grad)[(i-1)+k*n];
        }
        else {
            for (R_len_t k = 0; k < gvars; k++)
                REAL(res)[j+k*m] = 0;
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

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);
    SEXP res = alloc_list_gradient (ncs);

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

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    SEXP res = alloc_numeric_gradient (gvars, ncs);

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

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX) gradient_matrix_too_large_error();

    SEXP res = alloc_list_gradient (ncs * nrs);

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

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX)
        gradient_matrix_too_large_error();

    SEXP res = alloc_numeric_gradient (gvars, ncs*nrs);

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

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    int i, j, ii, jj, ij, jjnr;

    R_len_t nrs = LENGTH(sr);
    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX)
        gradient_matrix_too_large_error();

    SEXP res = alloc_list_gradient (ncs * nrs);

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

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    if (LENGTH(grad) != (uint64_t)gvars * n) abort();

    PROTECT(grad);

    int i, j, k, ii, jj, ij, jjnr;

    R_len_t nrs = LENGTH(sr);
    R_len_t ncs = LENGTH(sc);

    if ((uint64_t)ncs * nrs > R_LEN_T_MAX)
        gradient_matrix_too_large_error();

    SEXP res = alloc_numeric_gradient (gvars, ncs*nrs);

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
    int **subs, int *bound, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("array_subset_indexes_list_gradient %d %d\n",k,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (array_subset_indexes_list_gradient,
                              grad, subs, bound, offset, k, n);

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    PROTECT(grad);

    R_len_t m, i, v;
    int indx[k];
    int j;

    m = 1;
    for (j = 0; j < k; j++) {
        m *= bound[j];
        indx[j] = 0;
    }

    SEXP res = alloc_list_gradient (m);
    int last = 0;

    for (i = 0; !last; i++) {
        R_len_t ii;
        ii = array_offset_from_index (subs, bound, indx, offset, k, 1, &last);
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
    int **subs, int *bound, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("array_subset_indexes_numeric_gradient %d %d\n",k,n);
R_inspect(grad); REprintf("--\n");
#endif
    RECURSIVE_GRADIENT_APPLY (array_subset_indexes_numeric_gradient,
                              grad, subs, bound, offset, k, n);

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN (grad);
    R_len_t m, i, v;
    int indx[k];
    int j;

    m = 1;
    for (j = 0; j < k; j++) {
        m *= bound[j];
        indx[j] = 0;
    }

    SEXP res = alloc_numeric_gradient (gvars, m);

    int last = 0;

    for (i = 0; !last; i++) {
        R_len_t ii;
        ii = array_offset_from_index (subs, bound, indx, offset, k, 1, &last);
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


/* Create set of gradients from deleting the i'th to j'th elements of gradients
   for a vector list of length n.  Used for [[.]].  Protects its grad argument.
 */

SEXP attribute_hidden delete_range_list_gradient (SEXP grad, 
    R_len_t i, R_len_t j, R_len_t n)
{

#if 0
REprintf("*** delete_range_list_gradient %d %d %d\n",i,j,n);
R_inspect(grad);
REprintf("--\n");
#endif

    RECURSIVE_GRADIENT_APPLY (delete_range_list_gradient, grad, i, j, n);
	
    if (i < 0) i = 0;
    if (i >= n) i = n-1;
    if (j >= n) j = n-1;

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    if (j < i)
        return grad;

    PROTECT(grad);

    SEXP res = alloc_list_gradient (n-(j-i+1));
    if (i > 0) copy_vector_elements (res, 0, grad, 0, i);
    if (j < n-1) copy_vector_elements (res, i, grad, j+1, n-1-j);

#if 0
REprintf("*** delete_range_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from deleting elements of gradients not in 'include'
   for a vector list of length n.  Used for [[.]].  Protects its grad argument.
 */

SEXP attribute_hidden delete_selected_list_gradient (SEXP grad, 
    SEXP include, R_len_t n_remain, R_len_t n)
{

#if 0
REprintf("*** delete_selected_list_gradient %d %d\n",n_remain,n);
R_inspect(grad);
REprintf("--\n");
#endif

    RECURSIVE_GRADIENT_APPLY (delete_selected_list_gradient, grad,
                              include, n_remain, n);

    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();

    if (n_remain == n)
        return grad;

    PROTECT(grad);

    SEXP res = alloc_list_gradient (n_remain);
    R_len_t i, j;

    j = 0;
    for (i = 0; i < n; i++) {
        if (INTEGER(include)[i]) {
            SET_VECTOR_ELT (res, j, VECTOR_ELT(grad,i));
            j += 1;
        }
    }

#if 0
REprintf("*** delete_selected_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Copy scaled gradients from those in grad, which is protected here.  The
   length of the value is given by n.  If grad is shorter, it is recycled. */

attribute_hidden SEXP copy_scaled_gradients(SEXP grad, double factor, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY_NO_EXPAND (copy_scaled_gradients, grad, factor, n);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);

    if (DIAGONAL_JACOBIAN(grad) && n != gvars)
        grad = expand_to_full_jacobian(grad);

    R_len_t gn = JACOBIAN_LENGTH(grad) / gvars;

    return copy_scaled_jacobian (grad, gvars, gn, &factor, 1, n);
}


/* Copy scaled gradients from those in grad with vector of scaling factors.
   Length of the value is given by n; if grad or factors is shorter, it is 
   recycled. 

   Caller must protect factors, but not grad. */

attribute_hidden SEXP copy_scaled_gradients_vec  
    (SEXP grad, SEXP factors, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY_NO_EXPAND (copy_scaled_gradients_vec,
                                        grad, factors, n);
#if 0
REprintf("csv: %d %d %d - %d %d - %d\n",TYPEOF(grad),TYPEOF(factors),n,
LENGTH(grad),LENGTH(factors),GRADIENT_WRT_LEN(grad));
R_inspect(grad);
REprintf("--\n");
R_inspect(factors);
#endif

    if (TYPEOF(factors) != REALSXP) abort();
    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t i, j, k;
    SEXP r;

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t gn = JACOBIAN_LENGTH(grad) / gvars;
    R_len_t flen = LENGTH(factors);

    if (DIAGONAL_JACOBIAN(grad) && n != gvars)
        grad = expand_to_full_jacobian(grad);

    return copy_scaled_jacobian (grad, gvars, gn, REAL(factors), flen, n);
}


/* Find gradient of mean of vector, of length n. */

attribute_hidden SEXP mean_gradient (SEXP grad, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (mean_gradient, grad, n);

    PROTECT(grad);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t glen = LENGTH(grad);
    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    if (glen != gvars * n) abort();

    SEXP r = alloc_numeric_gradient (gvars, 1);

    R_len_t i, j, k;

    for (i = 0; i < gvars; i++) {
        long double s = 0;
        R_len_t e = (i+1)*n;
        for (j = i*n; j < e; j++)
            s += REAL(grad)[j];
        REAL(r)[i] = s / n;
    }

    UNPROTECT(1);
    return r;
}


/* Create set of gradients from grad that account for setting the length
   of a vector list to n.

   Protects its grad argument. */

SEXP attribute_hidden set_length_list_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("*** set_length_list_gradient %d\n",n);
R_inspect(grad);
#endif

    RECURSIVE_GRADIENT_APPLY (set_length_list_gradient, grad, n);

    if (TYPEOF(grad) != VECSXP) abort();

    PROTECT(grad);

    SEXP res = alloc_list_gradient (n);

    copy_vector_elements (res, 0, grad, 0, LENGTH(grad));

#if 0
REprintf("*** set_length_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients from grad that account for setting the length
   of a numeric vector to n.

   Protects its grad argument. */

SEXP attribute_hidden set_length_numeric_gradient (SEXP grad, R_len_t n)
{
#if 0
REprintf("*** set_length_list_gradient %d\n",n);
R_inspect(grad);
#endif

    RECURSIVE_GRADIENT_APPLY (set_length_numeric_gradient, grad, n);

    PROTECT(grad);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t k = JACOBIAN_LENGTH(grad) / gvars;

    SEXP res = alloc_numeric_gradient (gvars, n);

    if (n > k) {
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
        for (R_len_t h = 0; h < gvars; h++)
            memcpy (REAL(res)+h*n, REAL(grad)+h*k, k * sizeof(double));
    }
    else {
        for (R_len_t h = 0; h < gvars; h++)
            memcpy (REAL(res)+h*n, REAL(grad)+h*k, n * sizeof(double));
    }

#if 0
REprintf("*** set_length_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients for a matrix created with a vector on its diagonal.

   Protects its grad argument. */

SEXP attribute_hidden create_diag_matrix_gradient 
      (SEXP grad, R_len_t ng, R_len_t nr, R_len_t mn, R_len_t n)
{
#if 0
REprintf("*** create_diag_matrix_gradient %d %d %d %d\n",ng,nr,mn,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY (create_diag_matrix_gradient, grad, ng, nr, mn, n);

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN(grad);

    SEXP res = alloc_numeric_gradient (gvars, n);
    memset (REAL(res), 0, LENGTH(res) * sizeof(double));

    for (int h = 0; h < gvars; h++) {
        R_len_t jg = 0;
        for (int j = 0; j < mn; j++) {
            REAL(res) [h*n + j*nr + j] = REAL(grad) [h*ng + jg];
            if (++jg == ng) jg = 0;
        }
    }

#if 0
REprintf("*** create_diag_matrix_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Create set of gradients for the result of (row/col)(Sums/Means).

   Protects its grad argument. */

SEXP attribute_hidden rowcolsumsmeans_gradient 
                 (SEXP grad, SEXP x, int OP, int keepna, R_len_t nr, R_len_t nc)
{
    extern helpers_task_proc task_rowSums_or_rowMeans;
    extern helpers_task_proc task_colSums_or_colMeans;
#if 0
REprintf("*** rowcolsumsmeans_gradient %d %d %d %d\n",OP,keepna,nr,nc);
R_inspect(grad);
REprintf("--\n");
#endif

    RECURSIVE_GRADIENT_APPLY (rowcolsumsmeans_gradient,
                              grad, x, OP, keepna, nr, nc);

    if (TYPEOF(grad) != REALSXP) abort();

    PROTECT(grad);

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t n = OP < 2 ? nc : nr;

    SEXP res = alloc_numeric_gradient (gvars, n);

    if (OP < 2) {  /* colSums/Means */
        for (R_len_t h = 0; h < gvars; h++) {
            task_colSums_or_colMeans 
              ( ((helpers_op_t)(h*nc) << 33) |
                ((helpers_op_t)nc << 2) | (OP<<1)&2, res, grad,
                                                     keepna ? R_NilValue : x);
        }
    }
    else {  /* rowSums/Means */
        for (R_len_t h = 0; h < gvars; h++) {
            task_rowSums_or_rowMeans 
              ( ((helpers_op_t)(h*nr) << 33) |
                ((helpers_op_t)nr << 2) | (OP<<1)&2, res, grad,
                                                     keepna ? R_NilValue : x);
        }
    }

#if 0
REprintf("*** rowcolsumsmeans_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(1);
    return res;
}


/* Find gradient for the cumulative sum of a vector.  The s argument is the
   cumsum result, used to adjust for NA.

   Protects the grad argument, but not the s argument. */

attribute_hidden SEXP cumsum_gradient (SEXP grad, SEXP s, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (cumsum_gradient, grad, s, n);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t glen = JACOBIAN_LENGTH(grad);
    if (glen != (double) gvars * n) abort();

    PROTECT(grad);

    SEXP res = alloc_numeric_gradient (gvars, n);

    R_len_t i, j, k;

    for (k = n; k > 0 && ISNAN(REAL(s)[k-1]); k--) ;

    for (i = 0; i < glen; i += n) {
        double sg = 0;
        for (j = 0; j < k; j++) {
            sg += REAL(grad)[i+j];
            REAL(res)[i+j] = sg;
        }
        for ( ; j < n; j++)
            REAL(res)[i+j] = 0;
    }

    UNPROTECT(1);
    return res;
}


/* Find gradient for the cumulative product of a vector.  The v argument is the
   cummax argument; the s argument is the cumprod result.

   Protects the grad argument, but not the v and s arguments. */

attribute_hidden SEXP cumprod_gradient (SEXP grad, SEXP v, SEXP s, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (cumprod_gradient, grad, v, s, n);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t glen = JACOBIAN_LENGTH(grad);
    if (glen != (double) gvars * n) abort();

    PROTECT(grad);

    SEXP res = alloc_numeric_gradient (gvars, n);

    R_len_t i, j, k;

    for (k = n; k > 0 && ISNAN(REAL(s)[k-1]); k--) ;

    for (i = 0; i < glen; i += n) {
        if (k > 0) {
            REAL(res)[i] = REAL(grad)[i];
            for (j = 1; j < k; j++)
                REAL(res)[i+j] = REAL(v)[j] * REAL(res)[i+j-1]
                               + REAL(grad)[i+j] * REAL(s)[j-1];
        }
        for (j = k; j < n; j++)
            REAL(res)[i+j] = 0;
    }

    UNPROTECT(1);
    return res;
}


/* Find gradient for the cumulative max of a vector.  The v argument is the
   cummax argument; the s argument is the cummax result.

   Protects the grad argument, but not the v and s arguments. */

attribute_hidden SEXP cummax_gradient (SEXP grad, SEXP v, SEXP s, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (cummax_gradient, grad, v, s, n);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t glen = JACOBIAN_LENGTH(grad);
    if (glen != (double) gvars * n) abort();

    PROTECT(grad);

    SEXP res = alloc_numeric_gradient (gvars, n);
    memset (REAL(res), 0, glen * sizeof(double));

    R_len_t i, j, k, m;

    for (k = n; k > 0 && ISNAN(REAL(s)[k-1]); k--) ;

    m = 0;
    for (j = 0; j < k; j++) {
        if (REAL(v)[j] > REAL(v)[m])
            m = j;
        for (i = 0; i < glen; i += n)
            REAL(res)[i+j] = REAL(grad)[i+m];
    }

    UNPROTECT(1);
    return res;
}


/* Find gradient for the cumulative min of a vector.  The v argument is the
   cummax argument; the s argument is the cummin result.

   Protects the grad argument, but not the v and s arguments. */

attribute_hidden SEXP cummin_gradient (SEXP grad, SEXP v, SEXP s, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (cummin_gradient, grad, v, s, n);

    if (TYPEOF(grad) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(grad);
    R_len_t glen = JACOBIAN_LENGTH(grad);
    if (glen != (double) gvars * n) abort();

    PROTECT(grad);

    SEXP res = alloc_numeric_gradient (gvars, n);
    memset (REAL(res), 0, glen * sizeof(double));

    R_len_t i, j, k, m;

    for (k = n; k > 0 && ISNAN(REAL(s)[k-1]); k--) ;

    m = 0;
    for (j = 0; j < k; j++) {
        if (REAL(v)[j] < REAL(v)[m])
            m = j;
        for (i = 0; i < glen; i += n)
            REAL(res)[i+j] = REAL(grad)[i+m];
    }

    UNPROTECT(1);
    return res;
}


/* Macro for building a function that applies a binary operation to
   all pairs of gradients in g1 and g2.  Protects g1 and g2, then
   unprotects them at the end, so surrounding function will need to
   protect them again if required.  The NO_EXPAND version does not
   expand compact gradient representations. */

#define RECURSIVE_GRADIENT_APPLY2(fun,g1,g2,...) do { \
    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND(fun,g1,g2,__VA_ARGS__); \
    if (TYPEOF(g1) == REALSXP && DIAGONAL_JACOBIAN(g1)) { \
        PROTECT(g2); \
        g1 = expand_to_full_jacobian(g1); \
        UNPROTECT(1); \
    } \
    if (TYPEOF(g2) == REALSXP && DIAGONAL_JACOBIAN(g2)) { \
        PROTECT(g1); \
        g2 = expand_to_full_jacobian(g2); \
        UNPROTECT(1); \
    } \
} while (0)

#define RECURSIVE_GRADIENT_APPLY2_NO_EXPAND(fun,g1,g2,...) do { \
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
        for (R_len_t jjj = 0; jjj < m; jjj++) { \
            SET_VECTOR_ELT (res, jjj, \
              fun (g1==R_NilValue ? R_NilValue : VECTOR_ELT(g1,jjj), \
                   g2==R_NilValue ? R_NilValue : VECTOR_ELT(g2,jjj), \
                   __VA_ARGS__)); \
        } \
        UNPROTECT(3); \
        return res; \
    } \
    UNPROTECT(2); \
    if (g1 == R_NilValue && g2 == R_NilValue) \
        return R_NilValue; \
} while (0)


/* Add gradient of sum of vector (a) with gradient (v) of length n, to 
   previous gradient (grad), which may be modified, and should be in
   expanded form.  

   Protects its grad and v arguments. */

attribute_hidden SEXP sum_gradient 
                       (SEXP grad, SEXP v, SEXP a, int narm, R_len_t n)
{
#if 0
REprintf("*** sum_gradient %d %d\n",narm,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
REprintf("--\n");
R_inspect(a);
#endif
    extern helpers_task_proc task_colSums_or_colMeans;

    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND (sum_gradient, grad, v, a, narm, n);

    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();

    if (v == R_NilValue)
        return grad;

    if (TYPEOF(v) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (v);

    PROTECT2(v,grad);

    if (TYPEOF(grad) == REALSXP) {
        if (DIAGONAL_JACOBIAN(grad)) abort();
        if (JACOBIAN_LENGTH(grad) != gvars) abort();
    }

    SEXP r = grad;
    if (r == R_NilValue)
        r = alloc_numeric_gradient (gvars, 1);

    if (DIAGONAL_JACOBIAN(v)) {
        R_len_t i;
        if (grad == R_NilValue)
            memset (REAL(r), 0, LENGTH(r) * sizeof(double));
        if (LENGTH(v) == 1) {
            double d = REAL(v)[0];
            if (!narm || !ISNAN(d))
                for (i = 0; i < gvars; i++)
                    REAL(r)[i] += d;
        }
        else if (narm) {
            for (i = 0; i < gvars; i++)
                if (!ISNAN(REAL(v)[i]))
                    REAL(r)[i] += REAL(v)[i];
        }
        else {
            for (i = 0; i < gvars; i++)
                REAL(r)[i] += REAL(v)[i];
        }
    }
    else {
        if (narm) {
            R_len_t i, j;
            if (grad == R_NilValue)
                memset (REAL(r), 0, LENGTH(r) * sizeof(double));
            for (i = 0; i < gvars; i++) {
                long double s = 0;
                R_len_t b = i*n;
                for (j = 0; j < n; j++)
                    if (!ISNAN(REAL(a)[j])) s += REAL(v)[b+j];
                REAL(r)[i] += s;
            }
        }
        else {
            task_colSums_or_colMeans 
              (((helpers_op_t)gvars<<2) | (grad!=R_NilValue), r, v, R_NilValue);
        }
    }

#if 0
REprintf("*** sum_gradient end\n",narm,n);
R_inspect(r);
#endif

    UNPROTECT(2);
    return r;
}


/* Add gradient of product of vector (a) with gradient (v) of length n, to 
   previous gradient (grad), which may be modified.  The pprod argument 
   is the previous part of the product; nprod is the current part (product
   of elements in a).

   Protects its grad and v arguments. */

attribute_hidden SEXP prod_gradient 
  (SEXP grad, SEXP v, SEXP a, double pprod, double nprod, int narm, R_len_t n)
{
#if 0
REprintf("*** prod_gradient %f %f %d %d\n",pprod,nprod,narm,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
REprintf("--\n");
R_inspect(a);
#endif

    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND (prod_gradient, grad, v, a, 
                                         pprod, nprod, narm, n);

    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();

    R_len_t i, j, k;

    if (v == R_NilValue) {
        R_len_t glen = JACOBIAN_VALUE_LENGTH(grad);
        for (i = 0; i < glen; i++) {
            REAL(grad)[i] *= nprod;
        }
        return grad;
    }

    if (TYPEOF(v) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN(v);

    PROTECT2(v,grad);

    if (TYPEOF(grad) == REALSXP) {
        if (DIAGONAL_JACOBIAN(grad)) abort();
        if (JACOBIAN_LENGTH(grad) != gvars) abort();
    }

    SEXP r = grad;
    if (r == R_NilValue) {
        r = alloc_numeric_gradient (gvars, 1);
        memset (REAL(r), 0, LENGTH(r) * sizeof(double));
    }

    if (DIAGONAL_JACOBIAN(v)) {
        for (i = 0; i < gvars; i++) {
            long double g = REAL(r)[i];
            long double p = pprod;
            double d = LENGTH(v) == 1 ? *REAL(v) : REAL(v)[i];
            if (narm) {
                for (j = 0; j < n; j++) {
                    if (!ISNAN(REAL(a)[j])) {
                        g *= REAL(a)[j];
                        if (j == i) g += p * d;
                        p *= REAL(a)[j];
                    }
                }
            }
            else {
                for (j = 0; j < n; j++) {
                    g *= REAL(a)[j];
                    if (j == i) g += p * d;
                    p *= REAL(a)[j];
                }
            }
            REAL(r)[i] = g;
        }
    }
    else {
        for (i = 0; i < gvars; i++) {
            long double g = REAL(r)[i];
            long double p = pprod;
            R_len_t b = i*n;
            if (narm) {
                for (j = 0; j < n; j++) {
                    if (!ISNAN(REAL(a)[j])) {
                        g = g * REAL(a)[j] + p * REAL(v)[b+j];
                        p *= REAL(a)[j];
                    }
                }
            }
            else {
                for (j = 0; j < n; j++) {
                    g = g * REAL(a)[j] + p * REAL(v)[b+j];
                    p *= REAL(a)[j];
                }
            }
            REAL(r)[i] = g;
        }
    }

    UNPROTECT(2);
    return r;
}


/* Find gradient of a matrix product.

   Protects its x_grad and y_grad arguments. */

attribute_hidden SEXP matprod_gradient
                        (SEXP x_grad, SEXP y_grad, SEXP x, SEXP y,
                         int primop, R_len_t nrows, R_len_t k, R_len_t ncols)
{
#if 0
REprintf("*** matprod_gradient %d %d %d %d\n",primop,nrows,k,ncols);
R_inspect(x_grad);
REprintf("--\n");
R_inspect(y_grad);
REprintf("--\n");
#endif

    RECURSIVE_GRADIENT_APPLY2 (matprod_gradient, x_grad, y_grad, x, y,
                               primop, nrows, k, ncols);

    if (x_grad != R_NilValue && TYPEOF(x_grad) != REALSXP) abort();
    if (y_grad != R_NilValue && TYPEOF(y_grad) != REALSXP) abort();

    PROTECT2(x_grad,y_grad);

    R_len_t gvars = GRADIENT_WRT_LEN (x_grad != R_NilValue ? x_grad : y_grad);

    R_len_t sz = nrows * ncols;
    int init_grad = 0;
    SEXP grad;
    PROTECT (grad = alloc_numeric_gradient (gvars, sz));

    if (y_grad != R_NilValue && primop != 2) {
        if (primop == 0)
            matprod_mat_mat (REAL(x), REAL(y_grad), REAL(grad), 
                             nrows, k, ncols * gvars);
        else /* primop == 1 */
            matprod_trans1 (REAL(x), REAL(y_grad), REAL(grad), 
                            nrows, k, ncols * gvars);
        init_grad = 1;
    }

    if (x_grad != R_NilValue) {
        if (!init_grad) {
            memset (REAL(grad), 0, LENGTH(grad) * sizeof(double));
            init_grad = 1;
        }
        SEXP tmpr, tmpx;
        PROTECT (tmpr = allocVector (REALSXP, sz));
        PROTECT (tmpx = allocVector (REALSXP, nrows * k));
        for (R_len_t h = 0; h < gvars; h++) {
            SEXP xg;
            if (h == 0) 
                xg = x_grad;
            else {
                xg = tmpx;
                memcpy (REAL(xg), REAL(x_grad) + h*nrows*k, 
                        sizeof(double) * nrows*k);
            }
            if (primop == 0)
                matprod_mat_mat(REAL(xg), REAL(y), REAL(tmpr), nrows, k, ncols);
            else if (primop == 1)
                matprod_trans1(REAL(xg), REAL(y), REAL(tmpr), nrows, k, ncols);
            else /* primop == 2 */
                matprod_trans2(REAL(xg), REAL(y), REAL(tmpr), nrows, k, ncols);
            R_len_t i, j;
            j = h * sz;
            for (i = 0; i < sz; i++) 
                REAL(grad)[j++] += REAL(tmpr)[i];
        }
        UNPROTECT(2);
    }

    if (y_grad != R_NilValue && primop == 2) {
        if (!init_grad) {
            memset (REAL(grad), 0, LENGTH(grad) * sizeof(double));
            init_grad = 1;
        }
        SEXP tmpr, tmpy;
        PROTECT (tmpr = allocVector (REALSXP, sz));
        PROTECT (tmpy = allocVector (REALSXP, k * ncols));
        for (R_len_t h = 0; h < gvars; h++) {
            SEXP yg;
            if (h == 0) 
                yg = y_grad;
            else {
                yg = tmpy;
                memcpy (REAL(yg), REAL(y_grad) + h*k*ncols, 
                        sizeof(double) * k*ncols);
            }
            matprod_trans2(REAL(x), REAL(yg), REAL(tmpr), nrows, k, ncols);
            R_len_t i, j;
            j = h * sz;
            for (i = 0; i < sz; i++) 
                REAL(grad)[j++] += REAL(tmpr)[i];
        }
        UNPROTECT(2);
    }

    UNPROTECT(3);
    return grad;
}


/* Create set of gradients from grad that account for assigning a range
   of elements from v (recycled) to a vector list, extending to length n.
   May use grad as the result if NAMEDCNT not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_range_list_gradient 
                        (SEXP grad, SEXP v, R_len_t i, R_len_t j, R_len_t n)
{
#if 0
REprintf("*** subassign_range_list_gradient %d %d %d\n",i,j,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_range_list_gradient, grad, v, i, j, n);

    PROTECT2(grad,v);

    SEXP res;

    if (grad == R_NilValue) 
        res = alloc_list_gradient (n);
    else {
        if (TYPEOF(grad) != VECSXP) abort();
        res = NAMEDCNT_GT_1(grad) ? dup_top_level(grad) : grad;
        if (LENGTH(res) < n) res = reallocVector (res, n, 1);
    }

    R_len_t k;
    if (v == R_NilValue || LENGTH(v) == 0) {
        for (k = 0; k <= j-i; k++)
            SET_VECTOR_ELT (res, i+k, R_NilValue);
    }
    else {
        R_len_t lenv = LENGTH(v);
        R_len_t kv = 0;
        for (k = 0; k <= j-i; k++) {
            SET_VECTOR_ELT (res, i+k, VECTOR_ELT (v, kv));
            if (++kv == lenv) kv = 0;
        }
    }

#if 0
REprintf("*** subassign_range_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Create set of gradients from grad that account for assigning a range
   of elements from v (recycled) to a numeric vector, extending to length n.
   May use grad as the result, if NAMEDCNT is not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_range_numeric_gradient 
                        (SEXP grad, SEXP v, R_len_t i, R_len_t j, R_len_t n)
{
#if 0
REprintf("*** subassign_range_numeric_gradient %d %d %d\n",i,j,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_range_numeric_gradient, grad, v,
                               i, j, n);

    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();
    if (v != R_NilValue && TYPEOF(v) != REALSXP) abort();

    if (i < 0) i = 0;
    if (i >= n) i = n-1;
    if (j >= n) j = n-1;
    if (j < i) abort();

    PROTECT2(grad,v);

    R_len_t gvars = GRADIENT_WRT_LEN (grad != R_NilValue ? grad : v);

    R_len_t h, k, m;
    SEXP res;

    if (grad == R_NilValue) {
        res = alloc_numeric_gradient (gvars, n);
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    }
    else {
        if (TYPEOF(grad) != REALSXP) abort();
        if (LENGTH(grad) == n * gvars)
            res = NAMEDCNT_GT_1(grad) ? duplicate(grad) : grad;
        else {
            res = alloc_numeric_gradient (gvars, n);
            m = JACOBIAN_LENGTH(grad) / gvars;
            for (h = 0; h < gvars; h++) {
                memcpy (REAL(res) + h*n, REAL(grad) + h*m, m * sizeof(double));
                memset (REAL(res) + h*n + m, 0, (n-m) * sizeof(double));
            }
        }
    }

    R_len_t sij = j-i;

    if (v == R_NilValue || LENGTH(v) == 0) {
        for (h = 0; h < gvars; h++) {
            R_len_t hn = h*n;
            for (k = 0; k <= sij; k++)
                REAL(res)[hn + i + k] = 0;
        }
    }
    else {
        m = JACOBIAN_LENGTH(v) / gvars;
        if (m * gvars != LENGTH(v)) abort();
        for (h = 0; h < gvars; h++) {
            R_len_t hn = h*n, hm = h*m;
            if (m == 1) {
                double vv = REAL(v)[hm];
                for (k = 0; k <= sij; k++)
                    REAL(res)[hn + i + k] = vv;
            }
            else if (m >= sij) {
                for (k = 0; k <= sij; k++)
                    REAL(res)[hn + i + k] = REAL(v)[hm + k];
            }
            else {
                R_len_t kv = 0;
                for (k = 0; k <= sij; k++) {
                    REAL(res)[hn + i + k] = REAL(v)[hm + kv];
                    if (++kv == m) kv = 0;
                }
            }
        }
    }

#if 0
REprintf("*** subassign_range_numeric_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Create set of gradients from grad that account for assigning indexed
   elements from v (recycled) to a numeric vector, extending to length n.
   May use grad as the result, if NAMEDCNT is not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_indexes_numeric_gradient 
                        (SEXP grad, SEXP v, SEXP indx, R_len_t n)
{
#if 0
REprintf("*** subassign_indexes_numeric_gradient %d\n",n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_indexes_numeric_gradient, grad, v,
                               indx, n);

    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();
    if (v != R_NilValue && TYPEOF(v) != REALSXP) abort();

    PROTECT2(grad,v);

    R_len_t gvars = GRADIENT_WRT_LEN (grad != R_NilValue ? grad : v);

    R_len_t h, m;
    SEXP res;

    if (grad == R_NilValue) {
        res = alloc_numeric_gradient (gvars, n);
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    }
    else {
        if (TYPEOF(grad) != REALSXP) abort();
        if (LENGTH(grad) == n * gvars)
            res = NAMEDCNT_GT_1(grad) ? duplicate(grad) : grad;
        else {
            res = alloc_numeric_gradient (gvars, n);
            m = JACOBIAN_LENGTH(grad) / gvars;
            for (h = 0; h < gvars; h++) {
                memcpy (REAL(res) + h*n, REAL(grad) + h*m, m * sizeof(double));
                memset (REAL(res) + h*n + m, 0, (n-m) * sizeof(double));
            }
        }
    }

    R_len_t k = LENGTH(indx);

    if (v == R_NilValue || LENGTH(v) == 0) {
        for (R_len_t j = 0; j < k; j++) {
            R_len_t i = INTEGER(indx)[j];
            if (i >= 1 && i <= n)
                for (h = 0; h < gvars; h++)
                    REAL(res) [h*n + i-1] = 0.0;
        }
    }
    else {
        if (TYPEOF(v) != REALSXP) abort();
        m = JACOBIAN_LENGTH(v) / gvars;
        R_len_t jv = 0;
        for (R_len_t j = 0; j < k; j++) {
            R_len_t i = INTEGER(indx)[j];
            if (i >= 1 && i <= n)
                for (h = 0; h < gvars; h++)
                    REAL(res) [h*n + i-1] = REAL(v) [h*m + jv];
            if (++jv == m) jv = 0;
        }
    }

#if 0
REprintf("*** subassign_indexes_numeric_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Create set of gradients from grad that account for assigning indexed
   elements of a vector list gradients from v (recycled), extending to length n.
   May use grad as the result, if NAMEDCNT is not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_indexes_list_gradient 
                        (SEXP grad, SEXP v, SEXP indx, R_len_t n)
{
#if 0
REprintf("*** subassign_indexes_list_gradient %d\n",n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_indexes_list_gradient, grad, v, 
                               indx, n);

    PROTECT2(grad,v);

    SEXP res;

    if (grad == R_NilValue) 
        res = alloc_list_gradient (n);
    else {
        if (TYPEOF(grad) != VECSXP) abort();
        res = NAMEDCNT_GT_1(grad) ? dup_top_level(grad) : grad;
        if (LENGTH(res) < n) res = reallocVector (res, n, 1);
    }

    if (v == R_NilValue || LENGTH(v) == 0) {
        R_len_t k = LENGTH(indx);
        for (R_len_t j = 0; j < k; j++) {
            R_len_t i = INTEGER(indx)[j];
            if (i >= 1 && i <= n)
                SET_VECTOR_ELT (res, i-1, R_NilValue);
        }
    }
    else {
        if (TYPEOF(v) != VECSXP) abort();
        R_len_t lenv = LENGTH(v);
        R_len_t k = LENGTH(indx);
        R_len_t jv = 0;
        for (R_len_t j = 0; j < k; j++) {
            R_len_t i = INTEGER(indx)[j];
            if (i >= 1 && i <= n)
                SET_VECTOR_ELT (res, i-1, VECTOR_ELT (v, jv));
            if (++jv == lenv) jv = 0;
        }
    }

#if 0
REprintf("*** subassign_indexes_list_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}

/* Create set of gradients from grad that account for assigning indexed
   elements of a list array of length n gradients from v (recycled).
   May use grad as the result, if NAMEDCNT is not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden array_subassign_indexes_list_gradient (SEXP grad, SEXP v,
                      int **subs, int *bound, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("*** array_subassign_indexes_list_gradient %d %d\n",k,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (array_subassign_indexes_list_gradient, grad, v, 
                               subs, bound, offset, k, n);

    PROTECT2(grad,v);
    SEXP res;

    if (grad == R_NilValue) 
        res = alloc_list_gradient (n);
    else {
        if (TYPEOF(grad) != VECSXP) abort();
        res = NAMEDCNT_GT_1(grad) ? dup_top_level(grad) : grad;
        if (LENGTH(res) < n) res = reallocVector (res, n, 1);
    }

    int last = 0;
    int indx[k];
    int j;

    for (j = 0; j < k; j++) indx[j] = 0;

    if (v == R_NilValue || LENGTH(v) == 0) {
        while (!last) {
            R_len_t ii;
            ii = array_offset_from_index (subs, bound, indx, offset, 
                                          k, 0, &last);
            SET_VECTOR_ELT (res, ii, R_NilValue);
        }
    }
    else {
        if (TYPEOF(v) != VECSXP) abort();
        R_len_t lenv = LENGTH(v);
        R_len_t i = 0;
        while (!last) {
            R_len_t ii;
            ii = array_offset_from_index (subs, bound, indx, offset, 
                                          k, 0, &last);
            SET_VECTOR_ELT (res, ii, VECTOR_ELT (v, i));
            if (++i == lenv) i = 0;
        }
    }

#if 0
REprintf("*** array_subassign_indexes_list_gradient end\n");
R_inspect(grad);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}

/* Create set of gradients from grad that account for assigning indexed
   elements of a numeric array of length n gradients from v (recycled).
   May use grad as the result, if NAMEDCNT is not greater than one.

   Protects its grad and v arguments. */

SEXP attribute_hidden array_subassign_indexes_numeric_gradient 
  (SEXP grad, SEXP v, int **subs, int *bound, int *offset, R_len_t k, R_len_t n)
{
#if 0
REprintf("*** array_subassign_indexes_numeric_gradient %d %d\n",k,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2(array_subassign_indexes_numeric_gradient, grad, v,
                              subs, bound, offset, k, n);

    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();
    if (v != R_NilValue && TYPEOF(v) != REALSXP) abort();

    PROTECT2(grad,v);

    R_len_t gvars = GRADIENT_WRT_LEN (grad != R_NilValue ? grad : v);
    SEXP res;

    if (grad == R_NilValue) {
        res = alloc_numeric_gradient (gvars, n);
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    }
    else {
        if (TYPEOF(grad) != REALSXP) abort();
        res = NAMEDCNT_GT_1(grad) ? duplicate(grad) : grad;
    }

    int last = 0;
    int indx[k];
    int j;

    for (j = 0; j < k; j++) indx[j] = 0;

    if (v == R_NilValue || LENGTH(v) == 0) {
        while (!last) {
            R_len_t ii, h;
            ii = array_offset_from_index (subs, bound, indx, offset, 
                                          k, 0, &last);
            for (h = 0; h < gvars; h++)
                REAL(res) [h*n + ii] = 0;
        }
    }
    else {
        R_len_t m = JACOBIAN_LENGTH(v) / gvars;
        if (m * gvars != LENGTH(v)) abort();
        R_len_t i = 0;
        while (!last) {
            R_len_t ii, h;
            ii = array_offset_from_index (subs, bound, indx, offset, 
                                          k, 0, &last);
            for (h = 0; h < gvars; h++)
                REAL(res) [h*n + ii] = REAL(v) [h*m + i];
            if (++i == m) i = 0;
        }
    }

#if 0
REprintf("*** array_subassign_indexes_numeric_gradient end\n");
R_inspect(grad);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
}


/* Create set of gradients from grad that account for assigning an element
   with gradient v to the i'th element out of n of a vector list with gradient
   grad (creating lists as necessary). May use grad as the result, if NAMEDCNT
   is not greater than one.

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

    PROTECT2(grad,v);

    SEXP res;

    if (grad == R_NilValue) 
        res = alloc_list_gradient (n);
    else {
        if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
        res = NAMEDCNT_GT_1(grad) ? dup_top_level(grad) : grad;
    }

    if (i < 0 || i >= n) abort();
    SET_VECTOR_ELT (res, i, v);

#if 0
REprintf("*** subassign_list_gradient end\n");
R_inspect(res);
#endif

    UNPROTECT(2);
    return res;
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

    PROTECT2(grad,v);

    SEXP res = alloc_list_gradient (n+1);
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
   gradient grad.  May use grad as the result, if NAMEDCNT is not greater
   than one.

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

    PROTECT2(grad,v);

    R_len_t gvars = GRADIENT_WRT_LEN (grad != R_NilValue ? grad : v); 
    SEXP res;

    if (grad == R_NilValue) {
        res = alloc_numeric_gradient (gvars, n);
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    }
    else {
        if (TYPEOF(grad) != REALSXP) abort();
        if (LENGTH(grad) != (uint64_t) n * gvars) abort();
        res = NAMEDCNT_GT_1(grad) ? duplicate(grad) : grad;
    }

    if (i < 0 || i >= n) abort();
    if (v == R_NilValue)
        copy_elements (res, i, n, R_ScalarRealZero, 0, 0, gvars);        
    else
        copy_elements (res, i, n, v, 0, 1, gvars);

#if 0
REprintf("*** subasign_numeric_gradient end\n");
R_inspect(res);
REprintf("==\n");
#endif

    UNPROTECT(2);
    return res;
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

    PROTECT2(grad,v);

    R_len_t vlen = LENGTH(v);
    R_len_t m = 0;

    SEXP res = alloc_numeric_gradient (vlen, n+1);

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


/* Create set of gradients for pmin/pmax of vector of length 'n'.  The
   grad argument has accumulated gradients from earlier arguments; v
   is the next argument (which may shorter than 'n', and if so recycled).
   Result is updated 'grad', if not R_NilValue, which may be modified
   (assumed unshared).  The 'ans' and 'arg' arguments are the full
   answer, and the argument corresponding to v.

   Protects its grad and v arguments. */

SEXP attribute_hidden minmax_gradient (SEXP grad, SEXP v, SEXP ans, SEXP arg,
                                       R_len_t n)
{
#if 0
REprintf("*** minmax_gradient %d\n",n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
REprintf("--\n");
R_inspect(ans);
REprintf("--\n");
R_inspect(arg);
#endif

    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND (minmax_gradient, grad, v, ans, arg, n);

    if (v == R_NilValue)
        return grad;

    if (TYPEOF(v) != REALSXP) abort();
    if (grad != R_NilValue && TYPEOF(grad) != REALSXP) abort();

    PROTECT2(grad,v);

    R_len_t gvars = GRADIENT_WRT_LEN(v);
    R_len_t nv = JACOBIAN_LENGTH(v) / gvars;
    SEXP res;

    res = grad;
    if (res == R_NilValue) {
        if (!DIAGONAL_JACOBIAN(v) || gvars != n)
            res = alloc_numeric_gradient (gvars, n);
        else {
            res = allocVector (REALSXP, gvars);
            SET_GRADIENT_WRT_LEN (res, gvars);
            SET_DIAGONAL_JACOBIAN (res, 1);
        }
        memset (REAL(res), 0, LENGTH(res) * sizeof(double));
    }

    if (DIAGONAL_JACOBIAN(v) && DIAGONAL_JACOBIAN(res)) {
        R_len_t i = 0, j = 0;
        while (i < n) {
            if (REAL(ans)[i] == REAL(arg)[j])
                REAL(res)[i] = LENGTH(v) == 1 ? *REAL(v) : REAL(v)[j];
            i += 1;
            j += 1;
            if (j >= nv) j = 0;
        }
    }
    else if (DIAGONAL_JACOBIAN(v)) {
        R_len_t i = 0, j = 0;
        R_len_t h;
        while (i < n) {
            if (REAL(ans)[i] == REAL(arg)[j]) {
                for (h = 0; h < gvars; h++)
                    REAL(res)[h*n+i] = 0;
                REAL(res)[j*n+i] = LENGTH(v) == 1 ? *REAL(v) : REAL(v)[j];
            }
            i += 1;
            j += 1;
            if (j >= nv) j = 0;
        }
    }
    else {
        res = expand_to_full_jacobian(res);
        R_len_t i = 0, j = 0;
        R_len_t h;
        while (i < n) {
            if (REAL(ans)[i] == REAL(arg)[j]) {
                for (h = 0; h < gvars; h++)
                    REAL(res)[h*n+i] = REAL(v)[h*nv+j];
            }
            i += 1;
            j += 1;
            if (j >= nv) j = 0;
        }
    }

#if 0
REprintf("*** minmax_gradient end\n");
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

   The result may sometimes equal 'base' or 'extra'.  Sometimes 'base' or
   'extra' be modified and resused for the result, if it has NAMEDCNT of zero,
   or they may be returned unchanged.

   Protects its arguments. */

attribute_hidden SEXP add_scaled_gradients (SEXP base, SEXP extra, 
                                            double factor, R_len_t n)
{
#if 0
REprintf("add_scaled_gradients: %d %d - %f %d %d %d\n",
TYPEOF(base),TYPEOF(extra), factor, n,
LENGTH(base),LENGTH(extra));
#endif

    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND (add_scaled_gradients, 
                                         base, extra, factor, n);

    R_len_t gvars = GRADIENT_WRT_LEN (base != R_NilValue ? base : extra);

    if (extra == R_NilValue && LENGTH(base) == (double)gvars*n)
        return base;

    R_len_t en, bn;

    if (base == R_NilValue) {
        if (TYPEOF(extra) != REALSXP) abort();
        en = JACOBIAN_LENGTH(extra) / gvars;
        return copy_scaled_jacobian (extra, gvars, en, &factor, 1, n);
    }

    if (extra == R_NilValue) {
        if (TYPEOF(base) != REALSXP) abort();
        bn = JACOBIAN_LENGTH(base) / gvars;
        static double one = 1.0;
        return copy_scaled_jacobian (base, gvars, bn, &one, 1, n);
    }

    if (TYPEOF(base) != REALSXP) abort();
    if (TYPEOF(extra) != REALSXP) abort();
    if (GRADIENT_WRT_LEN(extra) != gvars) abort();

    bn = JACOBIAN_LENGTH(base) / gvars;
    en = JACOBIAN_LENGTH(extra) / gvars;

    R_len_t i, j, k, l;
    SEXP r;

    if (DIAGONAL_JACOBIAN(base) && bn==n && DIAGONAL_JACOBIAN(extra) && en==n) {

        PROTECT2(base,extra);

        if (NAMEDCNT_EQ_0(base) && LENGTH(base) == LENGTH(extra)) {
            r = base;
            l = LENGTH(base);
            for (i = 0; i < l; i++) 
                REAL(r)[i] += REAL(extra)[i] * factor;
        }
        else if (LENGTH(base) == 1 && LENGTH(extra) == 1)
            r = ScalarReal (*REAL(base) + *REAL(extra) * factor);
        else if (LENGTH(base) == 1) {
            r = allocVector (REALSXP, gvars);
            double d = *REAL(base);
            for (i = 0; i < gvars; i++) 
                REAL(r)[i] = d + REAL(extra)[i] * factor;
        }
        else if (LENGTH(extra) == 1) {
            r = allocVector (REALSXP, gvars);
            double d = *REAL(extra) * factor;
            for (i = 0; i < gvars; i++) 
                REAL(r)[i] = REAL(base)[i] + d;
        }
        else {
            r = allocVector (REALSXP, gvars);
            for (i = 0; i < gvars; i++) 
                REAL(r)[i] = REAL(base)[i] + REAL(extra)[i] * factor;
        }

        SET_GRADIENT_WRT_LEN (r, gvars);
        SET_DIAGONAL_JACOBIAN (r, 1);

        UNPROTECT(2);
        return r;
    }

    PROTECT(extra);
    base = expand_to_full_jacobian(base);
    UNPROTECT(1);
    PROTECT(base);
    extra = expand_to_full_jacobian(extra);
    PROTECT(extra);

    r = NAMEDCNT_EQ_0(base) && LENGTH(base) == (uint64_t) gvars * n ? base
         : alloc_numeric_gradient (gvars, n);
    R_len_t glen = n * gvars;
    k = l = 0;
    for (i = 0; i < glen; i += n) {
        R_len_t jb = 0, je = 0;
        for (j = 0; j < n; j++) {
            REAL(r)[i+j] = REAL(base)[k+jb] + REAL(extra)[l+je]*factor;
            if (++jb == bn) jb = 0;
            if (++je == en) je = 0;
        }
        k += bn;
        l += en;
    }

    UNPROTECT(2);

    return r;
}


/* Add the product of gradients in extra times elements in factors to
   the set of gradients in base.  The 'n' argument is the length of
   the vector this is the gradient for; the GRADIENT_WRT_LEN of base
   and/or extra indicates the length of the vector that the gradient
   is with respect to.  It is possible for base, extra, or factors to
   be short, with elements recycled to length n.

   Protects its base and extra arguments, but caller must protect factors. */

attribute_hidden SEXP add_scaled_gradients_vec (SEXP base, SEXP extra, 
                                                SEXP factors, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY2_NO_EXPAND (add_scaled_gradients_vec, base, extra, 
                                         factors, n);
#if 0
REprintf("add_scaled_gradients_vec: %d %d %d - %d %d %d %d\n",
TYPEOF(base),TYPEOF(extra),TYPEOF(factors), n,
LENGTH(base),LENGTH(extra),LENGTH(factors));
#endif

    if (TYPEOF(factors) != REALSXP) abort();

    R_len_t gvars = GRADIENT_WRT_LEN (base != R_NilValue ? base : extra);

    if (extra == R_NilValue && LENGTH(base) == (double)gvars*n)
        return base;

    R_len_t flen = LENGTH(factors);
    R_len_t en, bn;

    if (base == R_NilValue) {
        if (TYPEOF(extra) != REALSXP) abort();
        en = JACOBIAN_LENGTH(extra) / gvars;
        return copy_scaled_jacobian (extra, gvars, en, REAL(factors), flen, n);
    }

    if (extra == R_NilValue) {
        if (TYPEOF(base) != REALSXP) abort();
        bn = JACOBIAN_LENGTH(base) / gvars;
        static double one = 1.0;
        return copy_scaled_jacobian (base, gvars, bn, &one, 1, n);
    }

    if (TYPEOF(base) != REALSXP) abort();
    if (TYPEOF(extra) != REALSXP) abort();
    if (GRADIENT_WRT_LEN(extra) != gvars) abort();

    bn = JACOBIAN_LENGTH(base) / gvars;
    en = JACOBIAN_LENGTH(extra) / gvars;

    R_len_t i, j, k, l;
    SEXP r;

    if (DIAGONAL_JACOBIAN(base) && bn==n && DIAGONAL_JACOBIAN(extra) && en==n) {

        PROTECT2(base,extra);

        r = NAMEDCNT_EQ_0(base) && LENGTH(base) == gvars ? base
             : allocVector (REALSXP, gvars);
        R_len_t blen = LENGTH(base);
        R_len_t elen = LENGTH(extra);
        R_len_t jb = 0, je = 0, jf = 0;
        for (i = 0; i < gvars; i++) {
            REAL(r)[i] = REAL(base)[jb] + REAL(extra)[je] * REAL(factors)[jf];
            if (++jb == blen) jb = 0;
            if (++je == elen) je = 0;
            if (++jf == flen) jf = 0;
        }

        SET_GRADIENT_WRT_LEN (r, gvars);
        SET_DIAGONAL_JACOBIAN (r, 1);

        UNPROTECT(2);
        return r;
    }

    PROTECT(extra);
    base = expand_to_full_jacobian(base);
    UNPROTECT(1);
    PROTECT(base);
    extra = expand_to_full_jacobian(extra);
    PROTECT(extra);

    r = NAMEDCNT_EQ_0(base) && LENGTH(base) == (uint64_t) gvars * n ? base
         : alloc_numeric_gradient (gvars, n);
    R_len_t glen = n * gvars;
    k = l = 0;
    for (i = 0; i < glen; i += n) {
        R_len_t jb = 0, je = 0, jf = 0;
        for (j = 0; j < n; j++) {
            REAL(r)[i+j] = REAL(base)[l + jb] 
                            + REAL(extra)[k + je] * REAL(factors)[jf];
            if (++jb == bn) jb = 0;
            if (++je == en) je = 0;
            if (++jf == flen) jf = 0;
        }
        k += en;
        l += bn;
    }   

    UNPROTECT(2);

    return r;
}


/* Backpropagate gradients, adding contributions to gradients in 'base', 
   that are found by multiplying gradients of inner vars w.r.t. outer vars 
   (in 'a') by gradients of expression value w.r.t. inner vars (in 'b').  
   The caller must protect 'b', but not 'base' or 'a'. */

static SEXP add_jacobian_product (SEXP a, SEXP b, SEXP f);
static SEXP backup (SEXP g, SEXP a, SEXP b);

static SEXP backpropagate_gradients (SEXP base, SEXP a, SEXP b)
{
#if 0
REprintf("backpropagate_gradients\n");
R_inspect(base); REprintf("--\n");
R_inspect(a); REprintf("--\n");
R_inspect(b); REprintf("==\n");
#endif
    RECURSIVE_GRADIENT_APPLY2 (backpropagate_gradients, base, a, b);

    return backup (base, a, b);
}

/* Add gradients w.r.t. outer variables to 'base' (which is a gradient for
   a single variable/element) that are found from gradients of inner variables
   w.r.t. outer variables in 'a', multiplied by gradients of expression value
   w.r.t. inner variables in 'b'. */

static SEXP backup (SEXP base, SEXP a, SEXP b)
{
#if 0
REprintf("backup\n");
R_inspect(base); REprintf("--\n");
R_inspect(a); REprintf("--\n");
R_inspect(b); REprintf("==\n");
#endif
    if (a == R_NilValue || b == R_NilValue)
        return base;

    PROTECT3(base,a,b);

    if (TYPEOF(b) == VECSXP && GRAD_WRT_LIST(b)) {
        R_len_t n = LENGTH(b);
        if (TYPEOF(a) != VECSXP || LENGTH(a) != n) abort();
        for (R_len_t i = 0; i < n; i++)
            base = backup (base, VECTOR_ELT(a,i), VECTOR_ELT(b,i));
    }
    else
        base = add_jacobian_product (base, a, b);

    UNPROTECT(3);
    return base;
}

static SEXP add_jacobian_product (SEXP base, SEXP a, SEXP b)
{
#if 0
REprintf("add jacobian product\n");
R_inspect(base); REprintf("--\n");
R_inspect(a); REprintf("--\n");
R_inspect(b); REprintf("==\n");
#endif
    if (TYPEOF(a) != REALSXP) abort();

    if (b == R_NilValue)
        return base;

    SEXP res;

    if (TYPEOF(b) == VECSXP) {
        R_len_t n = LENGTH(b);
        PROTECT3(base,a,b);
        PROTECT (res = alloc_list_gradient (n));
        if (base == R_NilValue) {
            for (R_len_t i = 0; i < n; i++)
                SET_VECTOR_ELT (res, i, add_jacobian_product
                                 (R_NilValue, a, VECTOR_ELT(b,i)));
        }
        else {
            for (R_len_t i = 0; i < n; i++) {
                SET_VECTOR_ELT (res, i, add_jacobian_product
                                 (VECTOR_ELT(base,i), a, VECTOR_ELT(b,i)));
            }
        }

#if 0
REprintf("add jacobian product end (1)\n");
R_inspect(res); REprintf("..\n");
#endif
        UNPROTECT(4);
        return res;
    }

    if (TYPEOF(b) != REALSXP) abort();

    if (LENGTH(a) == 1 && LENGTH(b) == 1) {
        if (base == R_NilValue)
            return ScalarRealMaybeConst (*REAL(a) * *REAL(b));
        if (TYPEOF(base) != REALSXP) abort();
        if (LENGTH(base) != 1) abort();
        return ScalarRealMaybeConst (*REAL(base) + *REAL(a) * *REAL(b));
    }

    R_len_t k = GRADIENT_WRT_LEN(b);

    if (LENGTH(b) % k != 0) abort();
    R_len_t n = LENGTH(b) / k;

    if (LENGTH(a) % k != 0) abort();
    R_len_t gvars = LENGTH(a) / k;

    res = alloc_numeric_gradient (gvars, n);
    R_len_t glen = gvars * n;

    matprod_mat_mat (REAL(b), REAL(a), REAL(res), n, k, gvars);

    if (base == R_NilValue) {
#if 0
REprintf("add jacobian product end (2)\n");
R_inspect(res); REprintf("..\n");
#endif
        return res;
    }

    if (TYPEOF(base) != REALSXP) abort();
    if (LENGTH(base) != glen) abort();

    for (R_len_t i = 0; i < glen; i++) 
        REAL(res)[i] += REAL(base)[i];

#if 0
REprintf("add jacobian product end (3)\n");
R_inspect(res); REprintf("..\n");
#endif

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
        if (LENGTH(result_grad) != nv) abort();
        PROTECT (gr = allocVector (VECSXP, nv));
        gn = allocVector (STRSXP, nv);
        setAttrib (gr, R_NamesSymbol, gn); 
        for (R_len_t i = 0; i < nv; i++) {
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

static void cleanup_gradient_environment (void *venv)
{
    SEXP env = (SEXP)venv;

    /* Remove all variables in environment, decrementing NAMEDCNT for them. */

    for (SEXP b = FRAME(env); b != R_NilValue; b = CDR(b)) DEC_NAMEDCNT(CAR(b));

    SET_FRAME(env,R_NilValue);

    /* Clear GRADVARS, just in case it's somehow referenced.  Note that it's
       not seen by the garbage collector, so it better not be used anymore! */

    SET_GRADVARS(env,R_NilValue);
}

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

    /* Evaluate body. Do it in a new context so cleanup_gradient_environment
       can be called on an error exit. */

    RCNTXT cntx;
    begincontext (&cntx, CTXT_CCODE, call, newenv, env, R_NilValue, R_NilValue);
    cntx.cend = cleanup_gradient_environment;
    cntx.cenddata = newenv;

    vr = variant & VARIANT_PENDING_OK;
    if (need_grad)
        vr |= VARIANT_GRADIENT;

    SEXP result = evalv (CAR(p), newenv, vr);

    PROTECT_INDEX rix;                   
    PROTECT_WITH_INDEX(result,&rix);

    int res_has_grad = R_variant_result & VARIANT_GRADIENT_FLAG;

    SEXP result_grad = need_grad ? get_gradient (newenv) : R_NilValue;
    PROTECT(result_grad);

    cleanup_gradient_environment(newenv);
    dec_gradient_namedcnt(result_grad);

    endcontext (&cntx);

    /* For 'with gradient', attach gradient attribute. */
    
    if (PRIMVAL(op) == 0) { /* with gradient */

        if (NAMEDCNT_GT_0(result))
            REPROTECT (result = duplicate(result), rix);

        SEXP gr = create_gradient (result, result_grad, gv);
        setAttrib (result, R_GradientSymbol, gr);
        inc_gradient_namedcnt(result_grad);  /* after setAttrib, to avoid dup */
    }

    /* Propagate gradients backwards with the chain rule. */

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

    if (need_grad)
        UNPROTECT(5+2*nv);
    else
        UNPROTECT(5);

    R_Visible = TRUE;
    return result;
}

/* Look for a non-null Jacobian matrix, and return the number of rows
   it has.  Returns zero if no non-null Jacobian matrix is found. */

static R_len_t Jacobian_rows (SEXP g)
{
    R_len_t r;

    if (g == R_NilValue) 
        return 0;

    if (TYPEOF(g) == LISTSXP) {
        for (SEXP e = g; e != R_NilValue; e = CDR(e)) {
            r = Jacobian_rows (CAR(e));
            if (r != 0)
                return r;
        }
    }

    if (TYPEOF(g) == VECSXP) {
        for (R_len_t i = 0; i < LENGTH(g); i++) {
            r = Jacobian_rows (VECTOR_ELT(g,i));
            if (r != 0)
                return r;
        }
    }

    if (TYPEOF(g) == REALSXP) {
        R_len_t gvars = GRADIENT_WRT_LEN(g);
        r = JACOBIAN_LENGTH(g) / gvars;
        if (JACOBIAN_LENGTH(g) != r * gvars) abort();
        return r;
    }

    abort();
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
            vargrad[vgi] = R_NilValue;
            if (R_variant_result & VARIANT_GRADIENT_FLAG) {
                vargrad[vgi] = R_gradient;
                any_grad = 1;
            }
            PROTECT(vargrad[vgi]);
        }
        vgi += 1;

    }

    SET_ENVSYMBITS (newenv, bits);

    if (vgi != nv) abort();

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
            SEXP vg = vargrad[vgi++];
            R_len_t gvars = Jacobian_rows (vg);
            if (gvars == 0)
                continue;
            SEXP gval = evalv (CAR(q), newenv, VARIANT_PENDING_OK);
            PROTECT(gval);
            gval = match_structure (result, gval, gvars);
            if (gval == R_NoObject)
                errorcall (call, 
                  _("computed gradient does not have the correct structure"));
            UNPROTECT_PROTECT(gval);
            resgrad = backpropagate_gradients (resgrad, vg, gval);
            UNPROTECT(2);  /* gval, old resgrad */
            PROTECT(resgrad);
        }
        vr |= VARIANT_GRADIENT_FLAG;
        R_gradient = resgrad;
        UNPROTECT(1);  /* resgrad */
    }

    if (variant & VARIANT_GRADIENT)
        UNPROTECT(nv);  /* vargrad */

    UNPROTECT(2);  /* newenv, result */

    R_variant_result = vr;
    R_Visible = TRUE;
    return result;
}


static SEXP do_gradient_of(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    checkArity (op, args);

    RCNTXT *cntxt = R_GlobalContext;

    while (TYPEOF(cntxt->cloenv) != ENVSXP 
            || TYPEOF(GRADVARS(cntxt->cloenv)) != VECSXP) {
        cntxt = cntxt->nextcontext;
        if (cntxt == NULL)
           errorcall (call, 
             _("gradient_of called when there is no gradient construct"));
    }

    SEXP genv = cntxt->cloenv;
    SEXP result = PROTECT (evalv (CAR(args), env, VARIANT_GRADIENT));
    SEXP result_grad = PROTECT (get_gradient(genv));
    inc_gradient_namedcnt(result_grad);

    SEXP r = create_gradient (result, result_grad, GRADVARS(genv));

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
