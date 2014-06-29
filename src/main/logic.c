/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999--2010  The R Development Core Team.
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


static SEXP binaryLogic(int code, SEXP s1, SEXP s2);
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);


static SEXP do_fast_andor (SEXP call, SEXP op, SEXP x, SEXP y, SEXP env, 
                           int variant)
{
/* logical binary : "&" or "|" */
    SEXP dims, tsp, klass, xnames, ynames;
    int mismatch, nx, ny, xarray, yarray, xts, yts;
    mismatch = 0;
    if (isRaw(x) && isRaw(y)) {
    }
    else if (!isNumber(x) || !isNumber(y))
        errorcall(call,
          _("operations are possible only for numeric, logical or complex types"));
    tsp = R_NilValue;		/* -Wall */
    klass = R_NilValue;		/* -Wall */
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		error(_("binary operation on non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else /*(yarray)*/ {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }
    nx = length(x);
    ny = length(y);
    if(nx > 0 && ny > 0) {
	if(nx > ny) mismatch = nx % ny;
	else mismatch = ny % nx;
    }
    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, _("non-conformable time series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }
    if(mismatch)
	warningcall(call,
		    _("longer object length is not a multiple of shorter object length"));

    if (isLogical(x) && isLogical(y))
	PROTECT(x = binaryLogic(PRIMVAL(op), x, y));
    else if (isRaw(x) && isRaw(y))
	PROTECT(x = binaryLogic2(PRIMVAL(op), x, y));
    else {
	if (!isNumber(x) || !isNumber(y))
	    errorcall(call,
              _("operations are possible only for numeric, logical or complex types"));
	PROTECT(x = coerceVector(x, LGLSXP));
	PROTECT(y = coerceVector(y, LGLSXP));
	x = binaryLogic(PRIMVAL(op), x, y);
        UNPROTECT(2);
        PROTECT(x);
    }

    if (dims != R_NilValue) {
	setAttrib(x, R_DimSymbol, dims);
	if(xnames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, xnames);
	else if(ynames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, ynames);
    }
    else {
	if(length(x) == length(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if(length(x) == length(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }

    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, klass);
	UNPROTECT(2);
    }

    UNPROTECT(4);
    return x;
}


/* & | */

SEXP attribute_hidden do_andor(SEXP call, SEXP op, SEXP args, SEXP env, 
                               int variant)
{
    SEXP ans;

    if (DispatchGroup("Ops",call, op, args, env, &ans))
	return ans;

    checkArity (op, args);

    return do_fast_andor (call, op, CAR(args), CADR(args), env, variant);
}

/* Handles the ! operator. */

static SEXP do_fast_not(SEXP call, SEXP op, SEXP arg, SEXP env, int variant)
{
    SEXP x, dim, dimnames, names;
    int i, len;

    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (length(arg)==0) 
            return allocVector(LGLSXP, 0);
	else
            errorcall(call, _("invalid argument type"));
    }
    len = LENGTH(arg);

    /* Quickly do scalar operation on logical with no attributes. */

    if (len==1 && isLogical(arg) && ATTRIB(arg)==R_NilValue) {
        int v = LOGICAL(arg)[0];
        return ScalarLogical (v==NA_LOGICAL ? v : !v);
    }

    /* The general case... */

    PROTECT(names = getAttrib(arg, R_NamesSymbol));
    PROTECT(dim = getAttrib(arg, R_DimSymbol));
    PROTECT(dimnames = getAttrib(arg, R_DimNamesSymbol));
    PROTECT(x = allocVector(isRaw(arg) ? RAWSXP : LGLSXP, len));
    switch(TYPEOF(arg)) {
    case LGLSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (LOGICAL(arg)[i] == NA_LOGICAL) ?
		NA_LOGICAL : LOGICAL(arg)[i] == 0;
	break;
    case INTSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (INTEGER(arg)[i] == NA_INTEGER) ?
		NA_LOGICAL : INTEGER(arg)[i] == 0;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = ISNAN(REAL(arg)[i]) ?
		NA_LOGICAL : REAL(arg)[i] == 0;
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (ISNAN(COMPLEX(arg)[i].r) || ISNAN(COMPLEX(arg)[i].i))
		? NA_LOGICAL : (COMPLEX(arg)[i].r == 0. && COMPLEX(arg)[i].i == 0.);
	break;
    case RAWSXP:
	for (i = 0; i < len; i++)
	    RAW(x)[i] = 0xFF ^ RAW(arg)[i];
	break;
    default:
	UNIMPLEMENTED_TYPE("do_fast_not", arg);
    }
    if(names != R_NilValue) setAttrib(x, R_NamesSymbol, names);
    if(dim != R_NilValue) setAttrib(x, R_DimSymbol, dim);
    if(dimnames != R_NilValue) setAttrib(x, R_DimNamesSymbol, dimnames);
    UNPROTECT(4);
    return x;
}

/* ! */

SEXP attribute_hidden do_not(SEXP call, SEXP op, SEXP args, SEXP env, 
                             int variant)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    checkArity (op, args);

    return do_fast_not (call, op, CAR(args), env, variant);
}

/* Does && (op 1) and || (op 2). */

SEXP attribute_hidden do_andor2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s1, s2;
    int x1, x2;

    if (length(args) != 2)
	error(_("'%s' operator requires 2 arguments"),
	      PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = eval(CAR(args), env);
    if (!isNumber(s1))
	errorcall(call, _("invalid 'x' type in 'x %s y'"),
		  PRIMVAL(op) == 1 ? "&&" : "||");
    x1 = asLogical(s1);

    if (PRIMVAL(op)==1 && x1==FALSE)  /* FALSE && ... */
        return ScalarLogical(FALSE);

    if (PRIMVAL(op)==2 && x1==TRUE)   /* TRUE || ... */
        return ScalarLogical(TRUE);
    
    s2  = eval(CADR(args), env);
    if (!isNumber(s2))	
        errorcall(call, _("invalid 'y' type in 'x %s y'"),
	          PRIMVAL(op) == 1 ? "&&" : "||");		
    x2 = asLogical(s2);

    if (PRIMVAL(op)==1) /* ... && ... */
        return ScalarLogical (x2==FALSE ? FALSE
                            : x1==TRUE && x2==TRUE ? TRUE
                            : NA_LOGICAL);
    else /* ... || ... */
        return ScalarLogical (x2==TRUE ? TRUE
                            : x1==FALSE && x2==FALSE ? FALSE
                            : NA_LOGICAL);
}

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

static SEXP binaryLogic(int code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    int x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(LGLSXP, 0);
	return ans;
    }
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case 1:  /* & : AND */
        if (n1 == n2) {
            for (i = 0; i<n; i++) {
                x1 = LOGICAL(s1)[i];
                x2 = LOGICAL(s2)[i];
                if (x1 == 0 || x2 == 0)
                    LOGICAL(ans)[i] = 0;
                else if (x1 == NA_LOGICAL || x2 == NA_LOGICAL)
                    LOGICAL(ans)[i] = NA_LOGICAL;
                else
                    LOGICAL(ans)[i] = 1;
            }
        }
        else {
            mod_iterate(n1,n2,i1,i2) {
                x1 = LOGICAL(s1)[i1];
                x2 = LOGICAL(s2)[i2];
                if (x1 == 0 || x2 == 0)
                    LOGICAL(ans)[i] = 0;
                else if (x1 == NA_LOGICAL || x2 == NA_LOGICAL)
                    LOGICAL(ans)[i] = NA_LOGICAL;
                else
                    LOGICAL(ans)[i] = 1;
            }
        }
        break;
    case 2:  /* | : OR */
        if (n1 == n2) {
            for (i = 0; i<n; i++) {
                x1 = LOGICAL(s1)[i];
                x2 = LOGICAL(s2)[i];
                if (x1 == 0)
                    LOGICAL(ans)[i] = 
                      x2==0 ? 0 : x2==NA_LOGICAL ? NA_LOGICAL : 1;
                else if (x1 == NA_LOGICAL)
                    LOGICAL(ans)[i] = 
                      x2==0 || x2==NA_LOGICAL ? NA_LOGICAL : 1;
                else
                    LOGICAL(ans)[i] = 1;
            }
        }
        else {
            mod_iterate(n1,n2,i1,i2) {
                x1 = LOGICAL(s1)[i1];
                x2 = LOGICAL(s2)[i2];
                if (x1 == 0)
                    LOGICAL(ans)[i] = 
                      x2==0 ? 0 : x2==NA_LOGICAL ? NA_LOGICAL : 1;
                else if (x1 == NA_LOGICAL)
                    LOGICAL(ans)[i] = 
                      x2==0 || x2==NA_LOGICAL ? NA_LOGICAL : 1;
                else
                    LOGICAL(ans)[i] = 1;
            }
        }
        break;
    case 3:
        error(_("Unary operator `!' called with two arguments"));
        break;
    }
    return ans;
}

static SEXP binaryLogic2(int code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(RAWSXP, 0);
	return ans;
    }
    ans = allocVector(RAWSXP, n);

    switch (code) {
    case 1:  /* & : AND */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] & RAW(s2)[i];
        }
        else {
            mod_iterate(n1,n2,i1,i2)
                RAW(ans)[i] = RAW(s1)[i1] & RAW(s2)[i2];
        }
	break;
    case 2:  /* | : OR */
        if (n1 == n2) {
            for (i = 0; i<n; i++)
                RAW(ans)[i] = RAW(s1)[i] | RAW(s2)[i];
        }
        else {
            mod_iterate(n1,n2,i1,i2)
                RAW(ans)[i] = RAW(s1)[i1] | RAW(s2)[i2];
        }
	break;
    }
    return ans;
}

#define OP_ALL 1
#define OP_ANY 2

static int checkValues(int op, int na_rm, int *x, int n)
{
    int has_na = 0;

    if (op == OP_ANY) {
        for (int i = 0; i<n; i++) {
            if (x[i]!=FALSE) {
                if (x[i]==TRUE) 
                    return TRUE;
                else 
                    has_na = 1;
            }
        }
        return has_na && !na_rm ? NA_LOGICAL : FALSE;
    }
    else { /* OP_ALL */
        for (int i = 0; i<n; i++) {
            if (x[i]!=TRUE) {
                if (x[i]==FALSE) 
                    return FALSE;
                else 
                    has_na = 1;
            }
        }
        return has_na && !na_rm ? NA_LOGICAL : TRUE;
    }
}


/* fast version handles only one unnamed argument, so narm is FALSE. */

static SEXP do_fast_allany (SEXP call, SEXP op, SEXP arg, SEXP env, 
                            int variant)
{
    int val;

    if (length(arg) == 0)
        /* Avoid memory waste from coercing empty inputs, and also
           avoid warnings with empty lists coming from sapply */
        val = PRIMVAL(op) == OP_ALL ? TRUE : FALSE;

    else {
	if (TYPEOF(arg) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if (TYPEOF(arg) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(arg)));
	    arg = coerceVector(arg, LGLSXP);
	}
        if (LENGTH(arg) == 1) /* includes variant return of AND or OR of vec */
            val = LOGICAL(arg)[0];
        else
            val = checkValues (PRIMVAL(op), FALSE, LOGICAL(arg), LENGTH(arg));
    }

    return ScalarLogical(val);
}

static SEXP do_allany(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    Rboolean val = PRIMVAL(op) == OP_ALL ? TRUE : FALSE;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = duplicate(call));
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2);
	return(ans);
    }

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(length(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(t)));
	    t = coerceVector(t, LGLSXP);
	}
	val = checkValues(PRIMVAL(op), narm, LOGICAL(t), LENGTH(t));
        if (val == NA_LOGICAL)
            has_na = 1;
        else {
            if (PRIMVAL(op) == OP_ANY && val || PRIMVAL(op) == OP_ALL && !val) {
                has_na = 0;
                break;
            }
        } 
    }
    UNPROTECT(2);
    return has_na ? ScalarLogical(NA_LOGICAL) : ScalarLogical(val);
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_logic[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

/* Logical Operators, all primitives */

/* these are group generic and so need to eval args */
{"&",		do_andor,	1,	1001,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_andor,	2,	1001,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_not,		1,	1001,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_andor2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_andor2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},

/* these are group generic and so need to eval args */
{"all",		do_allany,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_allany,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};

/* Fast built-in functions in this file. See names.c for documentation */

attribute_hidden FASTFUNTAB R_FastFunTab_logic[] = {
/*slow func	fast func,     code or -1  uni/bi/both dsptch  variants */
{ do_andor,	do_fast_andor,	-1,		2,	1, 1,  0, 0 },
{ do_not,	do_fast_not,	-1,		1,	1, 0,  0, 0 },
{ do_allany,	do_fast_allany,	OP_ALL,		1,	1, 0,  VARIANT_AND, 0 },
{ do_allany,	do_fast_allany,	OP_ANY,		1,	1, 0,  VARIANT_OR, 0 },
{ 0,		0,		0,		0,	0, 0,  0, 0 }
};
