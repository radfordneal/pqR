/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2018 by Radford M. Neal
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
#include "Defn.h"


/* Get gradient identified by env from the gradients in R_gradient. */

static inline SEXP get_gradient (SEXP env)
{
    SEXP p;

    for (p = R_gradient; p != R_NilValue; p = CDR(p)) {
        if (TAG(p) == env) 
            return CAR(p);
    }

    return R_NilValue;
}

/* Copy gradients excluding one for xenv from those in R_gradient. */

static inline SEXP copy_gradients (SEXP xenv)
{
    PROTECT(R_gradient);

    SEXP p, q;

    for (p = R_gradient, q = R_NilValue; p != R_NilValue; p = CDR(p)) {
        if (TAG(p) != xenv) {
            q = cons_with_tag (CAR(p), q, TAG(p));
        }
    }

    UNPROTECT(1);
    return q;
}

/* Add a set of gradients, multiplied by factor, to another set of gradients. */

static inline SEXP add_grads (SEXP base, SEXP extra, SEXP factor)
{
    SEXP p, q, r;

    r = R_NilValue;
    
    /* Include gradients in base, possibly adding gradient from extra
       times factor. */

    for (p = base; p != R_NilValue; p = CDR(p)) {
        for (q = extra; q != R_NilValue; q = CDR(q)) {
            if (TAG(p) == TAG(q)) {
                double d = *REAL(CAR(p)) + *REAL(CAR(q)) * *REAL(factor);
                r = cons_with_tag (ScalarRealMaybeConst(d), r, TAG(p));
                goto next_base;
            }
        }
        r = cons_with_tag (CAR(p), r, TAG(p));
      next_base: ;
    }

    /* Include gradients only in extra times factor. */

    for (q = extra; q != R_NilValue; q = CDR(q)) {
        for (p = base; p != R_NilValue; p = CDR(p)) {
            if (TAG(p) == TAG(q)) {
                goto next_extra;
            }
        }
        double d = *REAL(CAR(q)) * *REAL(factor);
        r = cons_with_tag (ScalarRealMaybeConst(d), r, TAG(q));
      next_extra: ;
    }

    return r;
}


/* with_gradient (op == 0) and track_gradient (op == 1). */

static SEXP do_gradient (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    checkArity(op, args);

    SEXP sym = TAG(args) == R_NilValue ? CAR(args) : TAG(args);
    SEXP expr = CAR(args);
    if (TYPEOF(sym) != SYMSXP)
        error(_("gradient variable must be named"));

    SEXP val = evalv (expr, env, VARIANT_GRADIENT | VARIANT_PENDING_OK);
    SEXP val_grads = R_variant_result ? R_gradient : R_NilValue;
    PROTECT(val_grads);
    if (TYPEOF(val) != REALSXP || LENGTH(val) != 1) /* for now */
        error (_("gradient variable must have real scalar value"));

    SEXP cell = cons_with_tag (val, R_NilValue, sym);
    SEXP newenv = NewEnvironment (R_NilValue, cell, env);
    SET_RDEBUG(newenv,RDEBUG(env));
    PROTECT(newenv);
    INC_NAMEDCNT(val);
    SET_ENVSYMBITS (newenv, SYMBITS(sym));
    SET_STORE_GRAD(newenv,1);

    SEXP id_grad = ScalarRealMaybeConst(1.0);
    SET_ATTRIB (cell, cons_with_tag (id_grad, R_NilValue, newenv));
                
    SEXP result = evalv (CADR(args), newenv, 
                         VARIANT_GRADIENT | (variant & VARIANT_PENDING_OK));
    PROTECT(result);
    
    int got_grad = R_variant_result & VARIANT_GRADIENT_FLAG;
    SEXP result_grad = got_grad ? get_gradient (newenv) : R_NilValue;
    PROTECT(result_grad);
    R_variant_result = 0;

    if (PRIMVAL(op) == 0) {  /* with_gradient */
        if (NAMEDCNT_GT_0(result))
            result = duplicate(result);
        if (result_grad == R_NilValue)
            setAttrib (result, R_GradientSymbol, ScalarRealMaybeConst(0.0));
        else {
            setAttrib (result, R_GradientSymbol, result_grad);
            INC_NAMEDCNT(result_grad);
        }
    }

    if (got_grad && (variant & VARIANT_GRADIENT)) {

        SEXP other_grads = copy_gradients (newenv);
        PROTECT(other_grads);

        if (val_grads != R_NilValue && result_grad != R_NilValue) {
            other_grads = add_grads (other_grads, val_grads, result_grad);
        }
        if (other_grads != R_NilValue) {
            R_gradient = other_grads;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }

        UNPROTECT(1);
    }

    UNPROTECT(4);
    return result;
}


static SEXP do_comp_grad (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP result;

    result = R_NilValue;

    return result;
}


static SEXP do_gradient_of(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    checkArity (op, args);

    (void) evalv (CAR(args), env, VARIANT_GRADIENT);

    SEXP r = R_NilValue;
    if (R_variant_result & VARIANT_GRADIENT_FLAG) 
        r = get_gradient (env);

    R_variant_result = 0;
    return r;
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_gradient[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"with_gradient", do_gradient,  0,	1200,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"track_gradient", do_gradient,  1,	1200,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"compute_gradient", do_comp_grad,  0,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"gradient_of", do_gradient_of, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
