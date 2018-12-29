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


/* Get gradient identified by env from the gradients in R_gradient, which
   is protected for the duration of this function.  The gradient is return
   as a named vector list for multiple variables, or a single gradient if
   there is only one gradient variable for this environment. */

static SEXP get_gradient (SEXP env)
{
    if (! (R_variant_result & VARIANT_GRADIENT_FLAG))
        R_gradient = R_NilValue;

    PROTECT(R_gradient);

    SEXP gv = GRADVARS(env);
    if (TYPEOF(gv) != VECSXP) abort();

    int nv = LENGTH(gv);

    SEXP r = R_NilValue;
    if (nv > 1) {
        PROTECT(r = allocVector (VECSXP, nv));
        setAttrib (r, R_NamesSymbol, gv);
    }

    SEXP p;

    for (p = R_gradient; p != R_NilValue; p = CDR(p)) {
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

    if (nv == 1) {
        if (r == R_NilValue) r = ScalarRealMaybeConst(0.0);
    }
    else {
        int i;
        for (i = 0; i < nv; i++) {
            if (VECTOR_ELT(r,i) == R_NilValue)
                SET_VECTOR_ELT (r, i, ScalarRealMaybeConst(0.0));
        }
        UNPROTECT(1); /* r */
    }

    UNPROTECT(1);  /* R_gradient */
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

/* Copy scaled gradients from those in grad, which is protected here. */

attribute_hidden SEXP copy_scaled_gradients (SEXP grad, double factor)
{
    SEXP p, q;

    PROTECT(grad);

    for (p = grad, q = R_NilValue; p != R_NilValue; p = CDR(p)) {
        PROTECT(q);
        q = cons_with_tag (ScalarReal(*REAL(CAR(p)) * factor), q, TAG(p));
        SET_GRADINDEX (q, GRADINDEX(p));
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return q;
}

/* Add a set of gradients, in extra, multiplied by factor, to another set 
   of gradients, in base.  Protects its arguments. */

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
                double d = *REAL(CAR(p)) + *REAL(CAR(q)) * factor;
                PROTECT(r);
                r = cons_with_tag (ScalarRealMaybeConst(d), r, TAG(p));
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
        double d = *REAL(CAR(q)) * factor;
        PROTECT(r);
        r = cons_with_tag (ScalarRealMaybeConst(d), r, TAG(q));
        SET_GRADINDEX (r, GRADINDEX(q));
        UNPROTECT(1);
      next_extra: ;
    }

    UNPROTECT(2);
    return r;
}


/* with_gradient (op == 0) and track_gradient (op == 1). */

static SEXP do_gradient (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    /* Check for errors in argument structure, and store symbols for variables
       for GRADVARS in 'gv'. */

    int nv = length(args) - 1;
    if (nv < 1)
        errorcall (call, _("no gradient variables"));

    SEXP gv = allocVector (VECSXP, nv);
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
       cells, along with identity for gradient.  Also store gradients 
       they may have in 'vargrad'. */

    SEXP frame = R_NilValue;
    SEXP vargrad[nv];

    PROTECT(frame);
    for (i = 0, p = args; i < nv; i++, p = CDR(p)) {
        SEXP val = evalv (CAR(p), env, VARIANT_GRADIENT | VARIANT_PENDING_OK);
        vargrad[i] = R_variant_result ? R_gradient : R_NilValue;
        PROTECT(vargrad[i]);
        if (TYPEOF(val) != REALSXP || LENGTH(val) != 1 /* for now */)
            errorcall(call,_("gradient variable must have real scalar value"));
        frame = cons_with_tag (val, frame, VECTOR_ELT(gv,i));
        UNPROTECT(2);
        PROTECT2(vargrad[i],frame);
        SEXP id_grad = ScalarRealMaybeConst(1.0);
        SEXP gcell = cons_with_tag (id_grad, R_NilValue, newenv);
        SET_GRADINDEX (gcell, i+1);
        SET_ATTRIB (frame, gcell);
        INC_NAMEDCNT(val);
    }
    UNPROTECT(1); /* frame */

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

    /* For with_gradient, attach gradient attribute. */
    
    if (PRIMVAL(op) == 0 /* with_gradient */ && TYPEOF(result) == REALSXP
          && LENGTH(result) == 1 /* for now */) {

        if (NAMEDCNT_GT_0(result))
            REPROTECT (result = duplicate(result), rix);

        if (result_grad == R_NilValue)
            setAttrib (result, R_GradientSymbol, ScalarRealMaybeConst(0.0));
        else {
            setAttrib (result, R_GradientSymbol, result_grad);
            INC_NAMEDCNT(result_grad);
        }
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
                if (g != R_NilValue && r != R_NilValue) {
                    other_grads = 
                      add_scaled_gradients (other_grads, g, *REAL(r));
                }
            }
            UNPROTECT(1);
        }

        if (other_grads != R_NilValue) {
            R_gradient = other_grads;
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }

    }

    UNPROTECT(4+nv);
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
                if (TYPEOF(gval) != REALSXP || LENGTH(gval) != 1)
                    errorcall (call, 
                               _("computed value for gradient is invalid"));
                resgrad = add_scaled_gradients (resgrad, vargrad[vgi], 
                                                *REAL(gval));
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

    (void) evalv (CAR(args), env, VARIANT_GRADIENT);

    SEXP r = get_gradient (env);

    R_variant_result = 0;
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
            r = CONS (TAG(s), CONS (CAR(s), r));
            s = CDR(s);
        }
        UNPROTECT(1);
    }

    R_variant_result = 0;
    return r;
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

{"with_gradient", do_gradient,  0,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"track_gradient", do_gradient,  1,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"compute_gradient", do_compute_grad,  0, 1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"gradient_of", do_gradient_of, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"no_gradient", do_no_gradient, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"all_gradients_of", do_all_gradients_of, 1, 1210, 1,	{PP_FUNCALL, PREC_FN,	0}},

{"tracking_gradients", do_tracking_gradients, 1, 1210, 0, {PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
