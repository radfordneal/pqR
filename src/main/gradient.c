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


/* Expand the structure of 'list' to match 'base' by replacing NULL elements 
   that correspond to non-NULL elements by zeros, lists of zeros, etc.
   Top levels with GRAD_WRT_LIST set are matched instead agains base2. 
   Names are added to match those in the base or base2. */

static SEXP expand_structure (SEXP base, SEXP list, SEXP base2)
{
    if (TYPEOF(base2) == VECSXP && GRAD_WRT_LIST(base2)) {
        R_len_t n = LENGTH(base2);
        if (list != R_NilValue) {
            if (TYPEOF(list) != VECSXP || !GRAD_WRT_LIST(list)) abort();
            if (LENGTH(list) != n) abort();
        }
        SEXP res = PROTECT(allocVector(VECSXP,n));
        setAttrib (res, R_NamesSymbol, getNamesAttrib(base2));
        R_len_t i;
        for (i = 0; i < n; i++)
            SET_VECTOR_ELT (res, i, expand_structure (base, 
                             list==R_NilValue ? R_NilValue : VECTOR_ELT(list,i),
                             VECTOR_ELT(base2,i)));
        UNPROTECT(1);
        return res;
    }

    if (list == R_NilValue) {
        if (base == R_NilValue)
            return R_NilValue;
        if (TYPEOF(base) != VECSXP)
            return R_ScalarRealZero;
        R_len_t n = LENGTH(base);
        SEXP res = PROTECT(allocVector(VECSXP,n));
        setAttrib (res, R_NamesSymbol, getNamesAttrib(base));
        R_len_t i;
        for (i = 0; i < n; i++)
            SET_VECTOR_ELT (res, i, 
              expand_structure (VECTOR_ELT(base,i), list, base2));
        UNPROTECT(1);
        return res;
    }

    if (TYPEOF(list) == VECSXP) {
        if (TYPEOF(base) != VECSXP) abort();
        if (LENGTH(base) != LENGTH(list)) abort();
        R_len_t n = LENGTH(base);
        SEXP res = PROTECT(allocVector(VECSXP,n));
        setAttrib (res, R_NamesSymbol, getAttrib(base,R_NamesSymbol));
        for (R_len_t i = 0; i < n; i++) {
            SET_VECTOR_ELT (res, i, 
              expand_structure (VECTOR_ELT(base,i), VECTOR_ELT(list,i), base2));
        }
        UNPROTECT(1);
        return res;
    }

    if (TYPEOF(base) == VECSXP) abort();

    return list;
}


/* Make an "identity" gradient for a value.  Has names at the top list level,
   since will be used as 'base2' in expand_structure. */

static SEXP make_id_recursive (SEXP val, SEXP top)
{
    R_len_t n = LENGTH(val);

    SEXP res = PROTECT(allocVector(VECSXP,n));
    setAttrib (res, R_NamesSymbol, getNamesAttrib(val));
    SET_GRAD_WRT_LIST (res, 1);

    for (R_len_t i = 0; i < n; i++) {

        /* this code could be made for efficient, bypassing duplicate */
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
        if (TYPEOF(v) == REALSXP && LENGTH(v) == 1) {
            SET_VECTOR_ELT (bot, i, R_ScalarRealOne);
            SET_VECTOR_ELT (res, i, ntop);
        }
        else if (TYPEOF(v) == VECSXP) {
            SET_VECTOR_ELT (bot, i, allocVector(VECSXP,LENGTH(v)));
            SET_VECTOR_ELT (res, i, make_id_recursive (v, ntop));
        }
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return res;
}

static SEXP make_id_grad (SEXP val)
{
    if (TYPEOF(val) == REALSXP && LENGTH(val) == 1)
        return R_ScalarRealOne;

    if (TYPEOF(val) == VECSXP) {
        R_len_t n = LENGTH(val);
        SEXP top = PROTECT(allocVector(VECSXP,n));
        SEXP res = make_id_recursive (val, top);
        UNPROTECT(1);
        return res;
    }
 
    return R_NilValue;
}


/* Test whether the structure of a gradient value matches the structure of
   what it is the gradient of.  If any_for_0 is non-zero, a scalar zero in 
   grad can match anything. */

static int match_structure (SEXP val, SEXP grad, int any_for_0)
{
    if (any_for_0 && TYPEOF(grad) == REALSXP
                  && LENGTH(grad) == 1 
                  && *REAL(grad) == 0)
        return 1;

    if (TYPEOF(val) != TYPEOF(grad))
        return 0;

    if (TYPEOF(val) == REALSXP) {
        if (LENGTH(val) != LENGTH(grad))
            return 0;
    }
    else if (TYPEOF(val) == VECSXP) {
        if (LENGTH(val) != LENGTH(grad))
            return 0;
        R_len_t i;
        for (i = 0; i < LENGTH(val); i++) {
            if (! match_structure (VECTOR_ELT(val,i),
                                   VECTOR_ELT(grad,i), any_for_0))
                return 0;
        }
    }
    else
        return 0;

    return 1;
}


/* Compute a list from another list by adding the given amount to its scalar 
   elements, recursively when the list has list elements.  (Also works for a 
   'list' that is a simple scalar).  Copies the GRAD_WRT_LIST flag.

   Protects the 'list' argument. */

static SEXP addto_list (SEXP list, double amount)
{
    SEXP res;
    
    PROTECT(list);

    if (list == R_NilValue)
        res = ScalarRealMaybeConst (amount);
    else if (TYPEOF(list) == REALSXP) {
        if (LENGTH(list) != 1) abort();
        res = ScalarRealMaybeConst (*REAL(list) + amount);
    }
    else {
        if (TYPEOF(list) != VECSXP) abort();
        res = allocVector(VECSXP,LENGTH(list));
        SET_GRAD_WRT_LIST (res, GRAD_WRT_LIST(list));
        R_len_t i;
        for (i = 0; i < LENGTH(list); i++)
            SET_VECTOR_ELT (res, i, addto_list (VECTOR_ELT(list,i), amount));
    }
        
    UNPROTECT(1);
    return res;
}


/* Compute a list from another list by scaling scalar elements by the given
   factor, recursively when the list has list elements.  (Also works for
   a 'list' that is a simple scalar). Copies the GRAD_WRT_LIST flag.

   Protects the 'list' argument. */

static SEXP scaled_list (SEXP list, double factor)
{
    SEXP res;
    
    PROTECT(list);

    if (list == R_NilValue)
        res = ScalarRealMaybeConst (0.0);
    else if (TYPEOF(list) == REALSXP) {
        if (LENGTH(list) != 1) abort();
        res = ScalarRealMaybeConst (*REAL(list) * factor);
    }
    else {
        if (TYPEOF(list) != VECSXP) abort();
        res = allocVector(VECSXP,LENGTH(list));
        SET_GRAD_WRT_LIST (res, GRAD_WRT_LIST(list));
        R_len_t i;
        for (i = 0; i < LENGTH(list); i++)
            SET_VECTOR_ELT (res, i, scaled_list (VECTOR_ELT(list,i), factor));
    }
        
    UNPROTECT(1);
    return res;
}


/* Compute a list from two other lists by scaling scalar elements of the 
   second list by the given factor, and adding to the first, recursively 
   when the lists have list elements.  (Also works for 'lists' that are 
   simple scalars).  The two input lists must have the same form, except
   a scalar will be match (by replication) any structure in the other list,
   and R_NilValue will be regarded as the same as zero.  Copies the 
   GRAD_WRT_LIST flag from alist, or blist if alist is not a list.

   Protects the 'alist' and 'blist' arguments. */

static SEXP add_scaled_list (SEXP alist, SEXP blist, double factor)
{
    SEXP res;
    
    PROTECT2(alist,blist);

    if (blist == R_NilValue)
        res = alist;
    else if (alist == R_NilValue)
        res = scaled_list (blist, factor);
    else if (TYPEOF(alist) == REALSXP && TYPEOF(blist) == REALSXP) {
        if (LENGTH(alist) != 1 || LENGTH(blist) != 1) abort();
        res = ScalarRealMaybeConst (*REAL(alist) + *REAL(blist) * factor);
    }
    else if (TYPEOF(alist) == VECSXP && TYPEOF(blist) == REALSXP) {
        if (LENGTH(blist) != 1) abort();
        res = allocVector (VECSXP, LENGTH(alist));
        SET_GRAD_WRT_LIST (res, GRAD_WRT_LIST(alist));
        double amt = *REAL(blist) * factor;
        R_len_t i;
        for (i = 0; i < LENGTH(res); i++)
            SET_VECTOR_ELT(res, i, addto_list (VECTOR_ELT(alist,i), amt));
    }
    else if (TYPEOF(alist) == REALSXP && TYPEOF(blist) == VECSXP) {
        if (LENGTH(alist) != 1) abort();
        res = allocVector (VECSXP, LENGTH(blist));
        SET_GRAD_WRT_LIST (res, GRAD_WRT_LIST(blist));
        R_len_t i;
        for (i = 0; i < LENGTH(res); i++)
            SET_VECTOR_ELT(res, i, 
                           add_scaled_list(alist, VECTOR_ELT(blist,i), factor));
    }
    else if (TYPEOF(alist) == VECSXP && TYPEOF(blist) == VECSXP) {
        if (LENGTH(alist) != LENGTH(blist)) abort();
        res = allocVector (VECSXP, LENGTH(alist));
        SET_GRAD_WRT_LIST (res, GRAD_WRT_LIST(alist));
        R_len_t i;
        for (i = 0; i < LENGTH(res); i++)
            SET_VECTOR_ELT (res, i, 
             add_scaled_list(VECTOR_ELT(alist,i), VECTOR_ELT(blist,i), factor));
    }
    else
        abort();

    UNPROTECT(2);
    return res;
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


/* Create set of gradients from subsetting i'th element of gradients for 
   a vector of length n.  Protects its grad argument. */

SEXP attribute_hidden subset_vector_gradient (SEXP grad, R_len_t i, R_len_t n)
{
    RECURSIVE_GRADIENT_APPLY (subset_vector_gradient, grad, i, n);

    if (grad == R_NilValue)
        return R_NilValue;
	
    if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
    if (i < 0 || i >= n) abort();

    return VECTOR_ELT (grad, i);
}


/* Copy scaled gradients from those in grad, which is protected here. */

attribute_hidden SEXP copy_scaled_gradients (SEXP grad, double factor)
{
    RECURSIVE_GRADIENT_APPLY (copy_scaled_gradients, grad, factor);

    if (grad == R_NilValue)
        return R_NilValue;

    PROTECT(grad);

    SEXP r = ScalarRealMaybeConst (*REAL(grad) * factor);

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


/* Create set of gradients from grad that account for assigning v to the 
   i'th element out of n (creating lists as necessary).  The grad argument
   may be modified here.  

   Protects its grad and v arguments. */

SEXP attribute_hidden subassign_vector_gradient 
                        (SEXP grad, SEXP v, R_len_t i, R_len_t n)
{

#if 0
REprintf("*** subassign_vector_gradient %d %d\n",i,n);
R_inspect(grad);
REprintf("--\n");
R_inspect(v);
#endif

    RECURSIVE_GRADIENT_APPLY2 (subassign_vector_gradient, grad, v, i, n);

    if (grad == R_NilValue && v == R_NilValue)
        return R_NilValue;

    PROTECT2(grad,v);

    if (grad == R_NilValue) 
        grad = allocVector (VECSXP, n);
    else {
        if (TYPEOF(grad) != VECSXP || LENGTH(grad) != n) abort();
        if (i < 0 || i >= n) abort();
    }

    SET_VECTOR_ELT (grad, i, v);

    UNPROTECT(2);
    return grad;
}


/* Add the product of gradients in extra and in factor, to the set of
   gradients in base. 

   Protects its arguments. */

attribute_hidden SEXP add_scaled_gradients(SEXP base, SEXP extra, double factor)
{
    SEXP p, q, r;

    PROTECT2(base,extra);

    r = R_NilValue;
    
    /* Include gradients found in base, possibly adding gradient from extra
       times factor. */

    for (p = base; p != R_NilValue; p = CDR(p)) {
        for (q = extra; q != R_NilValue; q = CDR(q)) {
            if (TAG(p) == TAG(q) && GRADINDEX(p) == GRADINDEX(q)) {
                if (TYPEOF(CAR(q)) != REALSXP) abort();
                if (TYPEOF(CAR(p)) != REALSXP) abort();
                PROTECT(r);
                r = cons_with_tag (
                     ScalarRealMaybeConst(*REAL(CAR(p)) + *REAL(CAR(q))*factor),
                     r, TAG(q));
                SET_GRADINDEX (r, GRADINDEX(p));
                UNPROTECT(1);
                goto next_base;
            }
        }
        r = cons_with_tag (CAR(p), r, TAG(p));
        SET_GRADINDEX (r, GRADINDEX(p));
      next_base: ;
    }

    /* Include gradients only found in extra, times factor. */

    for (q = extra; q != R_NilValue; q = CDR(q)) {
        for (p = base; p != R_NilValue; p = CDR(p)) {
            if (TAG(p) == TAG(q) && GRADINDEX(p) == GRADINDEX(q)) {
                goto next_extra;
            }
        }
        if (TYPEOF(CAR(q)) != REALSXP) abort();
        PROTECT(r);
        r = cons_with_tag (ScalarRealMaybeConst (*REAL(CAR(q)) * factor),
                           r, TAG(q));
        SET_GRADINDEX (r, GRADINDEX(q));
        UNPROTECT(1);
      next_extra: ;
    }

    UNPROTECT(2);
    return r;
}


/* Add the product of gradients in extra and in factor, to the set of
   gradients in base.  The gradients in extra must be scalars. 

   Protects its arguments. */

static SEXP add_scaled_SEXP(SEXP base, SEXP extra, SEXP factor)
{
    SEXP p, q, r;

    if (TYPEOF(factor) == REALSXP) {
        if (LENGTH(factor) != 1) abort();
        return add_scaled_gradients (base, extra, *REAL(factor));
    }

    PROTECT3(base,extra,factor);

    r = R_NilValue;
    
    /* Include gradients found in base, possibly adding gradient from extra
       times factor. */

    for (p = base; p != R_NilValue; p = CDR(p)) {
        for (q = extra; q != R_NilValue; q = CDR(q)) {
            if (TAG(p) == TAG(q) && GRADINDEX(p) == GRADINDEX(q)) {
                if (TYPEOF(CAR(q)) != REALSXP) abort();
                if (TYPEOF(CAR(p)) != TYPEOF(factor)) abort();
                PROTECT(r);
                r = cons_with_tag (
                     add_scaled_list (CAR(p), factor, *REAL(CAR(q))),
                     r, TAG(q));
                SET_GRADINDEX (r, GRADINDEX(p));
                UNPROTECT(1);
                goto next_base;
            }
        }
        r = cons_with_tag (CAR(p), r, TAG(p));
        SET_GRADINDEX (r, GRADINDEX(p));
      next_base: ;
    }

    /* Include gradients only found in extra, times factor. */

    for (q = extra; q != R_NilValue; q = CDR(q)) {
        for (p = base; p != R_NilValue; p = CDR(p)) {
            if (TAG(p) == TAG(q) && GRADINDEX(p) == GRADINDEX(q)) {
                goto next_extra;
            }
        }
        if (TYPEOF(CAR(q)) != REALSXP) abort();
        PROTECT(r);
        r = cons_with_tag (scaled_list (factor, *REAL(CAR(q))), r, TAG(q));
        SET_GRADINDEX (r, GRADINDEX(q));
        UNPROTECT(1);
      next_extra: ;
    }

    UNPROTECT(3);
    return r;
}


/* Create user-visible form of gradient. */

static SEXP create_gradient (SEXP result, SEXP result_grad, SEXP gv)
{
    int nv = GRADVARS_NV(gv);

    SEXP gr, gn;

    if (nv == 1)
        gr = expand_structure (result, result_grad, VECTOR_ELT(gv,1));
    else {
        R_len_t i;
        PROTECT (gr = allocVector (VECSXP, nv));
        gn = allocVector (STRSXP, nv);
        setAttrib (gr, R_NamesSymbol, gn); 
        for (i = 0; i < nv; i++) {
            SET_STRING_ELT (gn, i, PRINTNAME (VECTOR_ELT (gv, i)));
            SET_VECTOR_ELT (gr, i, expand_structure (result, 
                                                     VECTOR_ELT(result_grad,i),
                                                     VECTOR_ELT(gv,nv+i)));
        }
        UNPROTECT(1);
    }

    return gr;
}


/* with_gradient (op == 0) and track_gradient (op == 1). */

static SEXP do_gradient (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    /* Check for errors in argument structure, and store symbols for variables
       for GRADVARS in 'gv'. */

    int nv = length(args) - 1;
    if (nv < 1)
        errorcall (call, _("no gradient variables"));

    SEXP gv = allocVector (VECSXP, GRADVARS_LEN(nv));
    SET_NAMEDCNT_MAX(gv);
    PROTECT(gv);

    SEXP p, q, r;
    int i;

    for (i = 0, p = args; i < nv; i++, p = CDR(p)) {
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
       cells, along with identity for gradient, which also goes in GRADVARS.
       Also store the gradients of initial variable values in 'vargrad'. */

    SEXP frame = R_NilValue;
    SEXP vargrad[nv];

    PROTECT_INDEX fix;
    PROTECT_WITH_INDEX(frame,&fix);

    for (i = 0, p = args; i < nv; i++, p = CDR(p)) {
        SEXP val = evalv (CAR(p), env, VARIANT_GRADIENT | VARIANT_PENDING_OK);
        REPROTECT (frame = cons_with_tag (val, frame, VECTOR_ELT(gv,i)), fix);
        PROTECT (vargrad[i] = R_variant_result ? R_gradient : R_NilValue);
        SEXP id_grad = PROTECT (make_id_grad (val));
        if (id_grad != R_NilValue) {
            SET_VECTOR_ELT (gv, nv+i, id_grad);
            SEXP gcell = cons_with_tag (id_grad, R_NilValue, newenv);
            SET_GRADIENT_IN_CELL (frame, gcell);
            SET_GRADINDEX (gcell, i+1);
        }
        INC_NAMEDCNT(val);
    }

    /* Finish setting up new environment. */

    SET_FRAME(newenv,frame);
    SET_RDEBUG(newenv,RDEBUG(env));
    set_symbits_in_env (newenv);
    SET_STORE_GRAD(newenv,1);
    SET_GRADVARS(newenv,gv);

    /* Evaluate body. */

    SEXP result = evalv (CAR(p), newenv, 
                         VARIANT_GRADIENT | (variant & VARIANT_PENDING_OK));
    PROTECT_INDEX rix;                   
    PROTECT_WITH_INDEX(result,&rix);

    int res_has_grad = R_variant_result & VARIANT_GRADIENT_FLAG;

    SEXP result_grad = get_gradient (newenv);
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

    if (res_has_grad && (variant & VARIANT_GRADIENT)) {

        SEXP other_grads = get_other_gradients (newenv);

        if (result_grad != R_NilValue) {
            PROTECT(other_grads);
            for (i = 0; i < nv; i++) {
                SEXP g = vargrad[i];
                SEXP r = nv > 1 ? VECTOR_ELT(result_grad,i) : result_grad;
                if (g != R_NilValue && r != R_NilValue)
                    other_grads = add_scaled_SEXP (other_grads, g, r);
            }
            UNPROTECT(1);
        }

        if (other_grads != R_NilValue) {
            R_gradient = other_grads;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }

    }

    SET_GRADVARS(newenv,R_NilValue);  /* just in case it's somehow referenced */

    UNPROTECT(5+2*nv);
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
                if (! match_structure (result, gval, 0))
                    errorcall (call, 
                      _("computed gradient does not match type of value"));
                resgrad = add_scaled_SEXP (resgrad, vargrad[vgi], gval);
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

{"track gradient", do_gradient,  1,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"compute gradient", do_compute_grad,  0, 1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"gradient_of", do_gradient_of, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"no_gradient", do_no_gradient, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"all_gradients_of", do_all_gradients_of, 1, 1210, 1,	{PP_FUNCALL, PREC_FN,	0}},

{"tracking_gradients", do_tracking_gradients, 1, 1210, 0, {PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
