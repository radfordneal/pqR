/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2010 The R Core Team
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
#include <Rdynpriv.h>
#include <Rmodules/Rlapack.h>

static R_LapackRoutines *ptr;

static int initialized = 0;

static void La_Init(void)
{
    int res = R_moduleCdynload("lapack", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!ptr->svd)
	error(_("lapack routines cannot be accessed in module"));
    initialized = 1;
    return;
}

/* Regretably, package 'party' calls this: attribute_hidden */
SEXP La_svd(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v, SEXP method)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->svd)(jobu, jobv, x, s, u, v, method);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_svd_cmplx(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->svd_cmplx)(jobu, jobv, x, s, u, v);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_rs(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->rs)(x, only_values);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_rs_cmplx(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->rs_cmplx)(x, only_values);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_rg(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->rg)(x, only_values);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_rg_cmplx(SEXP x, SEXP only_values)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->rg_cmplx)(x, only_values);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_chol(SEXP A)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->chol)(A);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_chol2inv(SEXP x, SEXP size)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->chol2inv)(x, size);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_dgecon(SEXP A, SEXP norm)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->dgecon)(A, norm);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}
attribute_hidden
SEXP La_dtrcon(SEXP A, SEXP norm)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->dtrcon)(A, norm);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}
attribute_hidden
SEXP La_zgecon(SEXP A, SEXP norm)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->zgecon)(A, norm);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}
attribute_hidden
SEXP La_ztrcon(SEXP A, SEXP norm)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->ztrcon)(A, norm);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_dlange(SEXP A, SEXP type)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->dlange)(A, type);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_zgesv(SEXP A, SEXP B)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->zgesv)(A, B);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_zgeqp3(SEXP A)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->zgeqp3)(A);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP qr_coef_cmplx(SEXP Q, SEXP B)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->qr_coef_cmplx)(Q, B);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP qr_qy_cmplx(SEXP Q, SEXP B, SEXP trans)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->qr_qy_cmplx)(Q, B, trans);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_dgesv(SEXP A, SEXP B, SEXP tol)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->dgesv)(A, B, tol);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP La_dgeqp3(SEXP A)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->dgeqp3)(A);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP qr_coef_real(SEXP Q, SEXP B)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->qr_coef_real)(Q, B);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP qr_qy_real(SEXP Q, SEXP B, SEXP trans)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->qr_qy_real)(Q, B, trans);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

attribute_hidden
SEXP det_ge_real(SEXP A, SEXP logarithm)
{
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->det_ge_real)(A, logarithm);
    else {
	error(_("lapack routines cannot be loaded"));
	return R_NilValue;
    }
}

R_LapackRoutines *
R_setLapackRoutines(R_LapackRoutines *routines)
{
    R_LapackRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}

static SEXP do_lapack(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    SEXP a1 = CAR(args); args = CDR(args);
    SEXP a2 = CAR(args); args = CDR(args);
    SEXP a3 = CAR(args); args = CDR(args);

    switch(PRIMVAL(op)) {
    case 1:   ans = La_rs (a1, a2); break;
    case 2:   ans = La_rs_cmplx (a1, a2); break;
    case 3:   ans = La_rg (a1, a2); break;
    case 41:  ans = La_rg_cmplx (a1, a2); break;
    case 5:   ans = La_rs (a1, a2); break;
    case 51:  ans = La_rs_cmplx (a1, a2); break;
    case 6:   ans = La_dlange (a1, a2); break;
    case 7:   ans = La_dgecon (a1, a2); break;
    case 8:   ans = La_dtrcon (a1, a2); break;
    case 9:   ans = La_zgecon (a1, a2); break;
    case 10:  ans = La_ztrcon (a1, a2); break;

    case 200: ans = La_chol (a1); break;
    case 201: ans = La_chol2inv (a1, a2); break;

    case 300: ans = qr_coef_real (a1, a2); break;
    case 301: ans = qr_qy_real (a1, a2, a3); break;
    case 302: ans = det_ge_real (a1, a2); break;
    case 303: ans = qr_coef_cmplx (a1, a2); break;
    case 304: ans = qr_qy_cmplx (a1, a2, a3); break;

    case 400: {
        SEXP a4 = CAR(args); args = CDR(args);
        SEXP a5 = CAR(args); args = CDR(args);
        SEXP a6 = CAR(args); args = CDR(args);
        SEXP a7 = CAR(args);
        ans = La_svd (a1, a2, a3, a4, a5, a6, a7);
        break;
    }
    case 401: {
        SEXP a4 = CAR(args); args = CDR(args);
        SEXP a5 = CAR(args); args = CDR(args);
        SEXP a6 = CAR(args);
        ans =  La_svd_cmplx (a1, a2, a3, a4, a5, a6);
        break;
    }

    case 500: ans = La_dgesv (a1, a2, a3); break;
    case 501: ans = La_dgeqp3 (a1); break;
    case 502: ans = La_zgesv (a1, a2); break;
    case 503: ans = La_zgeqp3 (a1); break;

    default:
        ans = R_NilValue;
        break;
    }

    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_lapack[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"La_rs",	do_lapack,     	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack,     	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack,     	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack,     	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,     	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",	do_lapack,     	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack,     	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack,     	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack,     	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack,     	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack,     	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack,     	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack,     	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack,     	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack,     	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack,     	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack,    	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmpl",	do_lapack,     	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack,     	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack,     	401,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},

{"La_dgesv",	do_lapack,     	500,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgeqp3",	do_lapack,     	501,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgesv",	do_lapack,     	502,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgeqp3",	do_lapack,     	503,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
