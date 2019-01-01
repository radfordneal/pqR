/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2015, 2016, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2011   The R Core Team.
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
#include "Print.h"

#include <helpers/helpers-app.h>

/* The global var. R_Expressions is in Defn.h */
#define R_MIN_EXPRESSIONS_OPT	25
#define R_MAX_EXPRESSIONS_OPT	500000

/* Interface to the (polymorphous!)  options(...)  command.
  
   We have two kind of options:
     1) those used exclusively from R code,
  	typically initialized in Rprofile.

  	Their names need not appear here, but may, when we want
  	to make sure that they are assigned `valid' values only.
  
     2) Those used (and sometimes set) from C code;
  	Either accessing and/or setting a global C variable,
  	or just accessed by e.g.  GetOption1(install("pager"))
  
   A (complete?!) list of these (2):

        "parse_dotdot"
  
  	"prompt"
  	"continue"
  	"expressions"
  	"width"
  	"digits"
  	"echo"
  	"verbose"
  	"keep.parens"
  	"keep.source"
  	"keep.source.pkgs"
  	"browserNLdisabled"
  
  	"de.cellwidth"		../unix/X11/ & ../gnuwin32/dataentry.c
  	"device"
  	"pager"
  	"paper.size"		./devPS.c

  	"timeout"		./connections.c

  	"check.bounds"
  	"error"
  	"error.messages"
  	"show.error.messages"
  	"warn"
  	"warning.length"
  	"warning.expression"
  	"nwarnings"

  
   S additionally/instead has (and one might think about some)
   "free",	"keep"
   "length",	"memory"
   "object.size"
   "reference", "show"
   "scrap"
*/

static SEXP Options(void)
{
    return install(".Options");
}

static attribute_noinline SEXP FindTaggedItem(SEXP lst, SEXP tag)
{
    if (tag == R_NilValue) return R_NilValue;  /* just in case... */

    do {
	if (TAG(lst) == tag)
	    break;
        lst = CDR(lst);
	if (TAG(lst) == tag)
	    break;
        lst = CDR(lst);
	if (TAG(lst) == tag)
	    break;
        lst = CDR(lst);
	if (TAG(lst) == tag)
	    break;
        lst = CDR(lst);
    } while (lst != R_NilValue);

    return lst;
}

static SEXP makeErrorCall(SEXP fun)
{
  SEXP call;
  PROTECT(call = allocList(1));
  SET_TYPEOF(call, LANGSXP);
  SETCAR(call, fun);
  UNPROTECT(1);
  return call;
}

SEXP GetOption(SEXP tag, SEXP rho)
{
    return GetOption1(tag);
}


SEXP GetOption1(SEXP tag)
{
    SEXP opt = findVar(Options(), R_BaseEnv);
    if (!isList(opt)) error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);
    return CAR(opt);
}

int GetOptionWidth(void)
{
    int w;
    w = asInteger(GetOption1(install("width")));
    if (w < R_MIN_WIDTH_OPT || w > R_MAX_WIDTH_OPT) {
	warning(_("invalid printing width, used 80"));
	return 80;
    }
    return w;
}

int GetOptionDigits(void)
{
    int d;
    d = asInteger(GetOption1(install("digits")));
    if (d < R_MIN_DIGITS_OPT || d > R_MAX_DIGITS_OPT) {
	warning(_("invalid printing digits, used 7"));
	return 7;
    }
    return d;
}


Rboolean Rf_GetOptionDeviceAsk(void)
{
    int ask;
    ask = asLogical(GetOption1(install("device.ask.default")));
    if(ask == NA_LOGICAL) {
	warning(_("invalid value for \"device.ask.default\", using FALSE"));
	return FALSE;
    }
    return ask != 0;
}


/* Change the value of an option, or add a new option or (if called
   with value R_NilValue) remove that option.  Returns the old option
   value. */

static SEXP SetOption(SEXP tag, SEXP value)
{
    SEXP opt, old, t;
    PROTECT(value);
    t = opt = SYMVALUE(Options());
    if (!isList(opt))
	error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);

    /* The option is being removed. */
    if (value == R_NilValue) {
	for ( ; t != R_NilValue ; t = CDR(t))
	    if (TAG(CDR(t)) == tag) {
		old = CAR(t);
		SETCDR(t, CDDR(t));
		UNPROTECT(1); /* value */
		return old;
	    }
	UNPROTECT(1); /* value */
	return R_NilValue;
    }
    /* If the option is new, a new slot */
    /* is added to the end of .Options */
    if (opt == R_NilValue) {
	while (CDR(t) != R_NilValue)
	    t = CDR(t);
	SETCDR(t, allocList(1));
	opt = CDR(t);
	SET_TAG(opt, tag);
    }
    old = CAR(opt);
    SETCAR(opt, value);
    UNPROTECT(1); /* value */
    return old;
}

/* Set the width of lines for printing i.e. like options(width=...) */
/* Returns the previous value for the options. */

int attribute_hidden R_SetOptionWidth(int w)
{
    SEXP t, v;
    if (w < R_MIN_WIDTH_OPT) w = R_MIN_WIDTH_OPT;
    if (w > R_MAX_WIDTH_OPT) w = R_MAX_WIDTH_OPT;
    PROTECT(t = install("width"));
    PROTECT(v = ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(2);
    return INTEGER(v)[0];
}

int attribute_hidden R_SetOptionWarn(int w)
{
    SEXP t, v;

    t = install("warn");
    PROTECT(v = ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(1);
    return INTEGER(v)[0];
}

/* Note that options are stored as a dotted pair list */
/* This is barely historical, but is also useful. */

void attribute_hidden InitOptions(void)
{
    SEXP val, v;
    char *p;

    PROTECT (val = CONS(R_NilValue,R_NilValue));
    v = val;

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("parse_dotdot"));
    p = getenv("R_PARSE_DOTDOT");
    R_parse_dotdot = p == NULL || strcmp(p,"FALSE") != 0 && strcmp(p,"no") != 0;
    SETCAR(v, ScalarLogical(R_parse_dotdot));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("prompt"));
    SETCAR(v, mkString("> "));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("continue"));
    SETCAR(v, mkString("+ "));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("expressions"));
    SETCAR(v, ScalarInteger(R_Expressions));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("width"));
    SETCAR(v, ScalarInteger(80));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("digits"));
    SETCAR(v, ScalarInteger(7));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("echo"));
    SETCAR(v, ScalarLogical(!R_Slave));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("verbose"));
    SETCAR(v, ScalarLogical(R_Verbose));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("check.bounds"));
    SETCAR(v, ScalarLogical(0));	/* no checking */

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("keep.parens"));
    SETCAR(v, ScalarLogical(0));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    p = getenv("R_KEEP_PKG_SOURCE");
    R_KeepSource = (p && (strcmp(p, "yes") == 0)) ? 1 : 0;
    SET_TAG(v, install("keep.source")); /* overridden in common.R */
    SETCAR(v, ScalarLogical(R_KeepSource));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("keep.source.pkgs"));
    SETCAR(v, ScalarLogical(R_KeepSource));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("warning.length"));
    SETCAR(v, ScalarInteger(1000));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("nwarnings"));
    SETCAR(v, ScalarInteger(50));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("OutDec"));
    SETCAR(v, mkString("."));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("browserNLdisabled"));
    SETCAR(v, ScalarLogical(FALSE));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("BLAS_in_helpers"));
    SETCAR(v, ScalarLogical(R_BLAS_in_helpers));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("mat_mult_with_BLAS"));
    SETCAR(v, allocVector (LGLSXP, R_mat_mult_with_BLAS_len));
    for (int i = 0; i < R_mat_mult_with_BLAS_len; i++) 
        LOGICAL(CAR(v))[i] = R_mat_mult_with_BLAS[i];

#ifdef HAVE_RL_COMPLETION_MATCHES
    /* value from Rf_initialize_R */
    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("rl_word_breaks"));
    SETCAR(v, mkString(" \t\n\"\\'`><=%;,|&{()}"));
    set_rl_word_breaks(" \t\n\"\\'`><=%;,|&{()}");
#endif

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_disable"));
    SETCAR(v, ScalarLogical(helpers_are_disabled));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_no_multithreading"));
    SETCAR(v, ScalarLogical(helpers_not_multithreading_now));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_no_pipelining"));
    SETCAR(v, ScalarLogical(helpers_not_pipelining_now));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_no_holding"));
    SETCAR(v, ScalarLogical(helpers_not_holding_now));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_no_merging"));
    SETCAR(v, ScalarLogical(helpers_not_merging));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("helpers_trace"));
    SETCAR(v, ScalarLogical(getenv("R_HELPERS_TRACE")!=0));

    SETCDR(v,CONS(R_NilValue,R_NilValue));
    v = CDR(v);
    SET_TAG(v, install("gradient_trace"));
    SETCAR(v, ScalarLogical(FALSE));

    SET_SYMVALUE(install(".Options"), CDR(val));
    UNPROTECT(1);
}


/* This needs to manage R_Visible */
static SEXP do_options(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP argi= R_NilValue, argnames= R_NilValue, namei= R_NilValue,
	names, options, s, tag, value; /* = R_Nil..: -Wall */
    SEXP sind, names2, value2;
    int i, k, n, *indx;

    /* Locate the options values in the symbol table.
       This will need to change if options are to live in the session
       frame.
       */

    options = SYMVALUE(Options());

    if (args == R_NilValue) {
	/* This is the zero argument case.
	   We alloc up a real list and write the system values into it.
	*/
	n = length(options);
	PROTECT(value = allocVector(VECSXP, n));
	PROTECT(names = allocVector(STRSXP, n));
	i = 0;
	while (options != R_NilValue) {
	    SET_STRING_ELT(names, i, PRINTNAME(TAG(options)));
	    SET_VECTOR_ELT(value, i, duplicate(CAR(options)));
	    options = CDR(options); i++;
	}
	PROTECT(sind = allocVector(INTSXP, n));  indx = INTEGER(sind);
	orderVector1(indx, n, names, TRUE, FALSE, R_NilValue);
	PROTECT(value2 = allocVector(VECSXP, n));
	PROTECT(names2 = allocVector(STRSXP, n));
	for(i = 0; i < n; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]-1));
	    SET_VECTOR_ELT(value2, i, VECTOR_ELT(value, indx[i]-1));
	}
	setAttrib(value2, R_NamesSymbol, names2);
	UNPROTECT(5);
	R_Visible = TRUE;
	return value2;
    }

    /* The arguments to "options" can either be a sequence of
       name = value form, or can be a single list.
       This means that we must code so that both forms will work.
       [ Vomits quietly onto shoes ... ]
       */

    n = length(args);
    if (n == 1 && (isPairList(CAR(args)) || isVectorList(CAR(args)))
	&& TAG(args) == R_NilValue ) {
	args = CAR(args);
	n = length(args);
    }
    PROTECT(value = allocVector(VECSXP, n));
    PROTECT(names = allocVector(STRSXP, n));

    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
	argnames = R_NilValue;
	break;
    case VECSXP:
	argnames = getAttrib(args, R_NamesSymbol);
	if(LENGTH(argnames) != n)
	    error(_("list argument has no valid names"));
	break;
    default:
	UNIMPLEMENTED_TYPE("options", args);
    }

    R_Visible = FALSE;
    for (i = 0 ; i < n ; i++) { /* i-th argument */

	switch (TYPEOF(args)) {
	case LISTSXP:
	    argi = CAR(args);
	    namei = EnsureString(TAG(args)); /* gives "" for no tag */
	    args = CDR(args);
	    break;
	case VECSXP:
	    argi = VECTOR_ELT(args, i);
	    namei = STRING_ELT(argnames, i);
	    break;
	default: /* already checked, but be safe here */
	    UNIMPLEMENTED_TYPE("options", args);
	}

        const char *opname = CHAR(namei);

	if (*opname) { /* name = value  ---> assignment */

	    tag = install_translated (namei);
            SEXP val;

            /* switch on first character of option name for efficiency */

            switch (*opname) {
            
            case 'a':
            break;
            
            case 'b':
	    if (streql(opname, "browserNLdisabled")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		if (k == NA_LOGICAL)
		    error(_("invalid value for '%s'"), opname);
		R_DisableNLinBrowser = k;
		val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'c':
	    if (streql(opname, "continue")) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    error(_("invalid value for '%s'"), opname);
		/* We want to make sure these are in the native encoding */
		val = mkString(translateChar(s));
                goto set;
	    }
	    if (streql(opname, "contrasts")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 2)
		    error(_("invalid value for '%s'"), opname);
		val = argi;
                goto set;
	    }
	    if (streql(opname, "check.bounds")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		/* R_CheckBounds = k; */
		val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'd':
	    if (streql(opname, "digits")) {
		k = asInteger(argi);
		if (k < R_MIN_DIGITS_OPT || k > R_MAX_DIGITS_OPT)
		    error(_("invalid 'digits' parameter, allowed %d...%d"),
			  R_MIN_DIGITS_OPT, R_MAX_DIGITS_OPT);
		val = ScalarInteger(k);
                goto set;
	    }
            break;
            
            case 'e':
	    if (streql(opname, "expressions")) {
		k = asInteger(argi);
		if (k < R_MIN_EXPRESSIONS_OPT || k > R_MAX_EXPRESSIONS_OPT)
		    error(_("'expressions' parameter invalid, allowed %d...%d"),
			  R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
		R_Expressions = R_Expressions_keep = k;
		val = ScalarInteger(k);
                goto set;
	    }
	    if (streql(opname, "error")) {
		if(isFunction(argi))
		  argi = makeErrorCall(argi);
		else if( !isLanguage(argi) &&  !isExpression(argi) )
		    error(_("invalid value for '%s'"), opname);
		val = argi;
                goto set;
	    }
	    if (streql(opname, "editor") && isString(argi)) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    error(_("invalid value for '%s'"), opname);
		val = ScalarString(s);
                goto set;
	    }
	    if (streql(opname, "echo")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		/* Should be quicker than checking options(echo)
		   every time R prompts for input:
		   */
		R_Slave = !k;
		val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'f':
            break;
            
            case 'g':
	    if (streql(opname, "gradient_trace")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		R_gradient_trace = asLogical(argi);
		val = ScalarLogical(R_gradient_trace);
                goto set;
	    }
            break;
            
            case 'h':
	    if (streql(opname, "helpers_disable")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_disable(k);
		val = ScalarLogical(helpers_are_disabled);
                goto set;
	    }
	    if (streql(opname, "helpers_no_multithreading")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_no_multithreading(k);
                val = ScalarLogical(helpers_not_multithreading);
                goto set;
	    }
	    if (streql(opname, "helpers_no_pipelining")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_no_pipelining(k);
                val = ScalarLogical(helpers_not_pipelining);
                goto set;
	    }
	    if (streql(opname, "helpers_no_holding")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_no_holding(k);
                val = ScalarLogical(helpers_not_holding);
                goto set;
	    }
	    if (streql(opname, "helpers_no_merging")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_no_merging(k);
                val = ScalarLogical(helpers_not_merging);
                goto set;
	    }
	    if (streql(opname, "helpers_trace")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		helpers_trace(k);
                val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'i':
            break;
            
            case 'j':
            break;
            
            case 'k':
	    if (streql(opname, "keep.parens")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "keep.source")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_KeepSource = k;
		val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'l':
            break;
            
            case 'm':
	    if (streql(opname, "max.print")) {
		k = asInteger(argi);
		if (k < 1) error(_("invalid value for '%s'"), opname);
		val = ScalarInteger(k);
                goto set;
	    }
	    if (streql(opname, "max.contour.segments")) {
		k = asInteger(argi);
                /* ... as many times above: rely on NA_INTEGER < <finite_int> */
		if (k < 0) 
		    error(_("invalid value for '%s'"), opname);
		max_contour_segments = k;
		val = ScalarInteger(k);
                goto set;
	    }
	    if (streql(opname, "mat_mult_with_BLAS")) {
                SEXP ov;
                int j;
		if (TYPEOF(argi)!=LGLSXP
                 || LENGTH(argi)!=1 && LENGTH(argi)!=R_mat_mult_with_BLAS_len)
		    error(_("invalid value for '%s'"), opname);
                ov = allocVector (LGLSXP, R_mat_mult_with_BLAS_len);
                for (j = 0; j<R_mat_mult_with_BLAS_len; j++)
                    LOGICAL(ov)[j] = LOGICAL(argi) [LENGTH(argi)==1 ? 0 : j];
                for (j = 0; j<R_mat_mult_with_BLAS_len; j++) 
                    R_mat_mult_with_BLAS[j] = LOGICAL(ov)[j];
		val = ov;
                goto set;
	    }
            break;
            
            case 'n':
	    if (streql(opname, "nwarnings")) {
		k = asInteger(argi);
		if (k < 1) error(_("invalid value for '%s'"), opname);
		R_nwarnings = k;
		R_CollectWarnings = 0; /* force a reset */
		val = ScalarInteger(k);
                goto set;
	    }
            break;
            
            case 'o':
            break;
            
            case 'p':
	    if (streql(opname, "parse_dotdot")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
                R_parse_dotdot = k;
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "prompt")) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    error(_("invalid value for '%s'"), opname);
		/* We want to make sure these are in the native encoding */
		val = mkString(translateChar(s));
                goto set;
	    }
	    if (streql(opname, "par.ask.default")) {
		error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }
            break;
            
            case 'q':
            break;
            
            case 'r':
	    if (streql(opname, "rl_word_breaks")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
#ifdef HAVE_RL_COMPLETION_MATCHES
		set_rl_word_breaks(CHAR(STRING_ELT(argi, 0)));
#endif
		val = duplicate(argi);
                goto set;
	    }
            break;
            
            case 's':
	    if (streql(opname, "show.error.messages")) {
                /* handle this here to avoid GetOption during error handling */
		if( !isLogical(argi) && length(argi) != 1 )
		    error(_("invalid value for '%s'"), opname);
		R_ShowErrorMessages = LOGICAL(argi)[0];
		val = argi;
                goto set;
	    }
	    if (streql(opname, "showWarnCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_ShowWarnCalls = k;
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "showErrorCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_ShowErrorCalls = k;
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "showNCalls")) {
		k = asInteger(argi);
		if (k < 30 || k > 500 || k == NA_INTEGER || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		R_NShowCalls = k;
		val = ScalarInteger(k);
                goto set;
	    }
            break;
            
            case 't':
            break;
            
            case 'u':
            break;
            
            case 'v':
            break;
            
            case 'w':
	    if (streql(opname, "width")) {
		k = asInteger(argi);
		if (k < R_MIN_WIDTH_OPT || k > R_MAX_WIDTH_OPT)
		    error(_("invalid 'width' parameter, allowed %d...%d"),
			  R_MIN_WIDTH_OPT, R_MAX_WIDTH_OPT);
		val = ScalarInteger(k);
                goto set;
	    }
	    if (streql(opname, "warn")) {
		if (!isNumeric(argi) || length(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		val = argi;
                goto set;
	    }
	    if (streql(opname, "warning.length")) {
		k = asInteger(argi);
		if (k < 100 || k > 8170)
		    error(_("invalid value for '%s'"), opname);
		R_WarnLength = k;
		val = argi;
                goto set;
	    }
	    if (streql(opname, "warning.expression") )  {
		if( !isLanguage(argi) &&  ! isExpression(argi) )
		    error(_("invalid value for '%s'"), opname);
		val = argi;
                goto set;
	    }
	    if (streql(opname, "warnPartialMatchDollar")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_warn_partial_match_dollar = k;
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "warnPartialMatchArgs")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_warn_partial_match_args = k;
		val = ScalarLogical(k);
                goto set;
	    }
	    if (streql(opname, "warnPartialMatchAttr")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), opname);
		k = asLogical(argi);
		R_warn_partial_match_attr = k;
		val = ScalarLogical(k);
                goto set;
	    }
            break;
            
            case 'x':
            break;
            
            case 'y':
            break;
            
            case 'z':
            break;

            default:  /* don't start with a-z */
	    if (streql(opname, "OutDec")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1 ||
		    strlen(CHAR(STRING_ELT(argi, 0))) != 1)
		    error(_("invalid value for '%s'"), opname);
		OutDec = CHAR(STRING_ELT(argi, 0))[0];
		val = duplicate(argi);
                goto set;
	    }
	    if (streql(opname, "BLAS_in_helpers")) {
		if (TYPEOF(argi)!=LGLSXP || LENGTH(argi)!=1 
                                         || *LOGICAL(argi)==NA_LOGICAL)
		    error(_("invalid value for '%s'"), opname);
                k = asLogical(argi);
                if (R_BLAS_IN_HELPERS_DEFAULT != FALSE)
                    val = ScalarLogical(k);
                goto set;
	    }
            break;
            }


            /* none of the above... */

            val = duplicate(argi);

          set:
            SET_VECTOR_ELT (value, i, SetOption (tag, val));
	    SET_STRING_ELT (names, i, namei);
	}
	else { /* querying arg */
	    SEXP tag;
	    if (!isString(argi) || LENGTH(argi) <= 0)
		error(_("invalid argument"));
	    tag = STRING_ELT (argi, 0);
	    if (streql (CHAR(tag), "par.ask.default")) {
		error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }

	    SET_VECTOR_ELT (value, i, duplicate (CAR 
                             (FindTaggedItem (options, installChar(tag)))));
	    SET_STRING_ELT (names, i, tag);
	    R_Visible = TRUE;
	}
    } /* for() */

    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_options[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"options",	do_options,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
