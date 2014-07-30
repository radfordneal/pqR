/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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
#define R_USE_SIGNALS 1
#include <Defn.h>

static SEXP do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

    checkArity(op,args);
#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = install(translateChar(STRING_ELT(CAR(args), 0))));	\
	SETCAR(args, findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP && TYPEOF(CAR(args)) != SPECIALSXP 
         &&  TYPEOF(CAR(args)) != BUILTINSXP )
	errorcall(call, _("argument must be a closure"));
    switch(PRIMVAL(op)) {
    case 0:
	SET_RDEBUG(CAR(args), 1);
	break;
    case 1:
	if( RDEBUG(CAR(args)) != 1 )
	    warningcall(call, "argument is not being debugged");
	SET_RDEBUG(CAR(args), 0);
	break;
    case 2:
        ans = ScalarLogical(RDEBUG(CAR(args)));
        break;
    case 3:
        SET_RSTEP(CAR(args), 1);
        break;
    }
    return ans;
}

/* primitives .primTrace and .primUntrace */
static SEXP do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg_x (args, call);

    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP)
	    errorcall(call, _("argument must be a function"));

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace state */

static Rboolean tracing_state = TRUE;
#define GET_TRACE_STATE tracing_state
#define SET_TRACE_STATE(value) tracing_state = value

SEXP R_traceOnOff(SEXP onOff)
{
    Rboolean prev = GET_TRACE_STATE;
    if(length(onOff) > 0) {
	Rboolean _new = asLogical(onOff);
	if(_new == TRUE || _new == FALSE)
	    SET_TRACE_STATE(_new);
	else
	    error("Value for tracingState must be TRUE or FALSE");
    }
    return ScalarLogical(prev);
}

Rboolean attribute_hidden
R_current_trace_state() { return GET_TRACE_STATE; }


/* memory tracing - no longer exists. */

static SEXP do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Do nothing - this function is no longer implemented (but is kept
       for compatibility. */

    return R_NilValue;
}


static SEXP do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Do nothing - this function is no longer implemented (but is kept
       for compatibility. */

    return R_NilValue;
}

static SEXP do_retracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Do nothing - this function is no longer implemented (but is kept
       for compatibility. */

    return R_NilValue;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_debug[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"debug",	do_debug,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"tracemem",    do_tracemem,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"retracemem",  do_retracemem,  0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
