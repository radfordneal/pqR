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


/* with_gradient, back_gradient, ad computer_gradient */

static SEXP do_gradient (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP val;
    checkArity(op, args);
    val = R_NilValue;
    return val;
}


static SEXP do_gradient_of (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP val;
    checkArity(op, args);
    val = R_NilValue;
    return val;
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_gradient[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"with_gradient", do_gradient,  0,	1200,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"back_gradient", do_gradient,  1,	1200,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"compute_gradient", do_gradient,  2,	1200,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"gradient_of", do_gradient_of, 0,	1200,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
