/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2019	The R Core Team.
 *  Copyright (C) 1995-1998	Robert Gentleman and Ross Ihaka.
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
 *  https://www.R-project.org/Licenses/
 *
 *
 *  print.default()  ->	 do_printdefault (with call tree below)
 *
 *  auto-printing   ->  PrintValueEnv
 *                      -> PrintValueRec
 *                      -> call print() for objects
 *  Note that auto-printing does not call print.default.
 *  PrintValue, R_PV are similar to auto-printing.
 *
 *  do_printdefault
 *	    -> PrintObject (if S4 dispatch needed)
 *	    -> PrintValueRec
 *		-> PrintGenericVector	-> PrintDispatch & PrintValueRec
 *		-> printList		-> PrintDispatch & PrintValueRec
 *		-> printAttributes	-> PrintValueRec  (recursion)
 *		-> PrintSpecial
 *		-> PrintExpression
 *		-> PrintClosure         -> PrintLanguage
 *		-> printVector		>>>>> ./printvector.c
 *		-> printNamedVector	>>>>> ./printvector.c
 *		-> printMatrix		>>>>> ./printarray.c
 *		-> printArray		>>>>> ./printarray.c
 *
 * PrintDispatch
 *	-> PrintObject
 *      -> PrintValueRec
 *
 *  do_prmatrix
 *	-> PrintDefaults
 *	-> printMatrix			>>>>> ./printarray.c
 *
 *
 *  See ./printutils.c	 for general remarks on Printing
 *			 and the EncodeString() and all Encode*() utils,
 *
 *  Also ./printvector.c,  ./printarray.c
 *
 *  do_sink moved to connections.c as of 1.3.0
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include "Defn.h"
#include <Internal.h>
#include "Print.h"
#include "Fileio.h"
#include "Rconnections.h"
#include <R_ext/RS.h>

/* Global print parameter struct: */
R_PrintData R_print;

static void printAttributes(SEXP, R_PrintData *, Rboolean);
static void PrintObject(SEXP, R_PrintData *);


#define TAGBUFLEN 256
#define TAGBUFLEN0 TAGBUFLEN + 6
static char tagbuf[TAGBUFLEN0 * 2]; /* over-allocate to allow overflow check */

void PrintInit(R_PrintData *data, SEXP env)
{
    data->na_string = NA_STRING;
    data->na_string_noquote = mkChar("<NA>");
    data->na_width = (int) strlen(CHAR(data->na_string));
    data->na_width_noquote = (int) strlen(CHAR(data->na_string_noquote));
    data->quote = 1;
    data->right = Rprt_adj_left;
    data->digits = GetOptionDigits();
    data->scipen = asInteger(GetOption1(install("scipen")));
    if (data->scipen == NA_INTEGER) data->scipen = 0;
    data->max = asInteger(GetOption1(install("max.print")));
    if (data->max == NA_INTEGER || data->max < 0) data->max = 99999;
    else if(data->max == INT_MAX) data->max--; // so we can add
    data->gap = 1;
    data->width = GetOptionWidth();
    data->useSource = USESOURCE;
    data->cutoff = GetOptionCutoff();
    data->env = env;
    data->callArgs = R_NilValue;
}

/* Used in X11 module for dataentry */
/* NB this is called by R.app even though it is in no public header, so
   alter there if you alter this */
void PrintDefaults(void)
{
    PrintInit(&R_print, R_GlobalEnv);
}

SEXP attribute_hidden do_invisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    switch (length(args)) {
    case 0:
	return R_NilValue;
    case 1:
	check1arg(args, call, "x");
	return CAR(args);
    default:
	checkArity(op, args); /* must fail */
	return call;/* never used, just for -Wall */
    }
}

/* This is *only* called via outdated R_level prmatrix() : */
SEXP attribute_hidden do_prmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int quote;
    SEXP a, x, rowlab, collab, naprint;
    char *rowname = NULL, *colname = NULL;

    checkArity(op,args);
    PrintDefaults();
    a = args;
    x = CAR(a); a = CDR(a);
    rowlab = CAR(a); a = CDR(a);
    collab = CAR(a); a = CDR(a);

    quote = asInteger(CAR(a)); a = CDR(a);
    R_print.right = (Rprt_adj) asInteger(CAR(a)); a = CDR(a);
    naprint = CAR(a);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    error(_("invalid 'na.print' specification"));
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    (int) strlen(CHAR(R_print.na_string));
    }

    if (length(rowlab) == 0) rowlab = R_NilValue;
    if (length(collab) == 0) collab = R_NilValue;
    if (!isNull(rowlab) && !isString(rowlab))
	error(_("invalid row labels"));
    if (!isNull(collab) && !isString(collab))
	error(_("invalid column labels"));

    printMatrix(x, 0, getAttrib(x, R_DimSymbol), quote, R_print.right,
		rowlab, collab, rowname, colname);
    PrintDefaults(); /* reset, as na.print.etc may have been set */
    return x;
}/* do_prmatrix */

static void PrintLanguage(SEXP s, R_PrintData *data)
{
    int i;
    SEXP t = getAttrib(s, R_SrcrefSymbol);
    Rboolean useSrc = data->useSource && isInteger(t);
    if (useSrc) {
	PROTECT(t = lang2(R_AsCharacterSymbol, t));
	t = eval(t, R_BaseEnv);
	UNPROTECT(1);
    } else {
	t = deparse1w(s, 0, data->useSource | DEFAULTDEPARSE);
	R_print = *data; /* Deparsing calls PrintDefaults() */
    }
    PROTECT(t);
    for (i = 0; i < LENGTH(t); i++) {
 	Rprintf("%s\n", translateChar(STRING_ELT(t, i))); // translate: for srcref part (PR#16732)
    }
    UNPROTECT(1);
}

static void PrintClosure(SEXP s, R_PrintData *data)
{
    PrintLanguage(s, data);

    if (isByteCode(BODY(s)))
	Rprintf("<bytecode: %p>\n", BODY(s));
    SEXP t = CLOENV(s);
    if (t != R_GlobalEnv)
	Rprintf("%s\n", EncodeEnvironment(t));
}

/* This advances `args` and `prev`. If an argument should not be
   forwarded because it was not explicitly supplied by the user,
   `prev` skips one element. If an argument is found to be
   non-missing, we set `allMissing` to false so we know we cannot use
   show() on S4 objects. */
static void advancePrintArgs(SEXP* args, SEXP* prev,
			     int** missingArg, int* allMissing) {
    *args = CDR(*args);

    if (**missingArg) {
	SETCDR(*prev, *args);
    } else {
	*allMissing = 0;
	*prev = CDR(*prev);
    }

    ++(*missingArg);
}

/* .Internal(print.default(x, args, missings)) */
SEXP attribute_hidden do_printdefault(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP x = CAR(args); args = CDR(args);

    R_PrintData data;
    PrintInit(&data, rho);

    /* These indicate whether an argument should be forwarded */
    int* missingArg = LOGICAL(CADR(args));
    int allMissing = 1;

    /* The remaining arguments are wrapped in another pairlist that
       will be forwarded on recursion */
    args = CAR(args);

    /* Wrap in a parent node to facilitate rechaining */
    SEXP orig = PROTECT(CONS(R_NilValue, args));
    SEXP prev = orig;

    if(!isNull(CAR(args))) {
	data.digits = asInteger(CAR(args));
	if (data.digits == NA_INTEGER ||
	    data.digits < R_MIN_DIGITS_OPT ||
	    data.digits > R_MAX_DIGITS_OPT)
	    error(_("invalid '%s' argument"), "digits");
    }
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    data.quote = asLogical(CAR(args));
    if(data.quote == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quote");
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    SEXP naprint = CAR(args);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    error(_("invalid 'na.print' specification"));
	data.na_string = data.na_string_noquote = STRING_ELT(naprint, 0);
	data.na_width = data.na_width_noquote =
	    (int) strlen(CHAR(data.na_string));
    }
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    SEXP gap = CAR(args);
    if(!isNull(gap)) {
	data.gap = asInteger(gap);
	if (data.gap == NA_INTEGER || data.gap < 0)
	    error(_("'gap' must be non-negative integer"));
    }
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    data.right = (Rprt_adj) asLogical(CAR(args)); /* Should this be asInteger()? */
    if(data.right == NA_LOGICAL)
	error(_("invalid '%s' argument"), "right");
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    SEXP max = CAR(args);
    if(!isNull(max)) {
	data.max = asInteger(max);
	if(data.max == NA_INTEGER || data.max < 0)
	    error(_("invalid '%s' argument"), "max");
	else if(data.max == INT_MAX) data.max--; // so we can add
    }
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    data.useSource = asLogical(CAR(args));
    if(data.useSource == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useSource");
    if(data.useSource) data.useSource = USESOURCE;
    advancePrintArgs(&args, &prev, &missingArg, &allMissing);

    /* The next arguments are those forwarded in `...`. If all named
       arguments were missing and there are no arguments in `...`, the
       user has not supplied any parameter and we can use show() on S4
       objects */
    int noParams = allMissing && args == R_NilValue;

    data.callArgs = CDR(orig);

    /* Initialise the global R_init as other routines still depend on it */
    R_print = data;

    tagbuf[0] = '\0';
    if (noParams && IS_S4_OBJECT(x) && isMethodsDispatchOn())
	PrintObject(x, &data);
    else
	PrintValueRec(x, &data);

    PrintDefaults(); /* reset, as na.print etc may have been set */

    UNPROTECT(1);
    return x;
}/* do_printdefault */

/*
  NOTE: The S3/S4 versions do not save and restore state like
	PrintObject() does.
*/
static void PrintObjectS4(SEXP s, R_PrintData *data)
{
    /*
      Note that can assume there is a loaded "methods"
      namespace.  It is tempting to cache the value of show in
      the namespace, but the latter could be unloaded and
      reloaded in a session.
    */
    SEXP methodsNS = PROTECT(R_FindNamespace(mkString("methods")));
    if (methodsNS == R_UnboundValue)
	error("missing methods namespace: this should not happen");

    SEXP fun = findVarInFrame3(methodsNS, install("show"), TRUE);
    if (TYPEOF(fun) == PROMSXP) {
	PROTECT(fun);
	fun = eval(fun, R_BaseEnv);
	UNPROTECT(1);
    }
    if (fun == R_UnboundValue)
	error("missing show() in methods namespace: this should not happen");

    SEXP call = PROTECT(lang2(fun, s));

    eval(call, data->env);
    UNPROTECT(2);
}

static void PrintObjectS3(SEXP s, R_PrintData *data)
{
    /*
      Bind value to a variable in a local environment, similar to
      a local({ x <- <value>; print(x) }) call. This avoids
      problems in previous approaches with value duplication and
      evaluating the value, which might be a call object.
    */
    SEXP xsym = install("x");
    SEXP mask = PROTECT(NewEnvironment(R_NilValue, R_NilValue, data->env));
    defineVar(xsym, s, mask);

    /* Forward user-supplied arguments to print() */
    SEXP fun = PROTECT(findFun(install("print"), R_BaseNamespace));
    SEXP args = PROTECT(cons(xsym, data->callArgs));
    SEXP call = PROTECT(lcons(fun, args));

    eval(call, mask);

    defineVar(xsym, R_NilValue, mask); /* To eliminate reference to s */
    UNPROTECT(4); /* mask, fun, args, call */
}

static void PrintObject(SEXP s, R_PrintData *data)
{
    /* Save the tagbuffer to restore indexing tags after evaluation
       because calling into base::print() resets the buffer */
    char save[TAGBUFLEN0];
    strcpy(save, tagbuf);

    if (isMethodsDispatchOn() && IS_S4_OBJECT(s))
	PrintObjectS4(s, data);
    else
	PrintObjectS3(s, data);

    R_print = *data;
    strcpy(tagbuf, save);
}

static void PrintDispatch(SEXP s, R_PrintData *data) {
    if (isObject(s))
	PrintObject(s, data);
    else
	PrintValueRec(s, data);
}

static void PrintGenericVector(SEXP s, R_PrintData *data)
{
    int i, taglen, ns, w, d, e, wr, dr, er, wi, di, ei;
    SEXP dims, t, names, tmp;
    char pbuf[115], *ptag;

    ns = length(s);
    if((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	/* FIXME: check (ns <= data->max +1) ? ns : data->max; */
	for (i = 0; i < ns; i++) {
	    switch(TYPEOF(PROTECT(tmp = VECTOR_ELT(s, i)))) {
	    case NILSXP:
		snprintf(pbuf, 115, "NULL");
		break;
	    case LGLSXP:
		if (LENGTH(tmp) == 1) {
		    const int *x = LOGICAL_RO(tmp);
		    formatLogical(x, 1, &w);
		    snprintf(pbuf, 115, "%s",
			     EncodeLogical(x[0], w));
		} else
		    snprintf(pbuf, 115, "Logical,%d", LENGTH(tmp));
		break;
	    case INTSXP:
		/* factors are stored as integers */
		if (inherits(tmp, "factor")) {
		    snprintf(pbuf, 115, "factor,%d", LENGTH(tmp));
		} else {
		    if (LENGTH(tmp) == 1) {
			const int *x = INTEGER_RO(tmp);
			formatInteger(x, 1, &w);
			snprintf(pbuf, 115, "%s",
				 EncodeInteger(x[0], w));
		    } else
			snprintf(pbuf, 115, "Integer,%d", LENGTH(tmp));
		}
		break;
	    case REALSXP:
		if (LENGTH(tmp) == 1) {
		    const double *x = REAL_RO(tmp);
		    formatReal(x, 1, &w, &d, &e, 0);
		    snprintf(pbuf, 115, "%s",
			     EncodeReal0(x[0], w, d, e, OutDec));
		} else
		    snprintf(pbuf, 115, "Numeric,%d", LENGTH(tmp));
		break;
	    case CPLXSXP:
		if (LENGTH(tmp) == 1) {
		    const Rcomplex *x = COMPLEX_RO(tmp);
		    if (ISNA(x[0].r) || ISNA(x[0].i))
			/* formatReal(NA) --> w=data->na_width, d=0, e=0 */
			snprintf(pbuf, 115, "%s",
				 EncodeReal0(NA_REAL, data->na_width, 0, 0, OutDec));
		    else {
			formatComplex(x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
			snprintf(pbuf, 115, "%s",
				 EncodeComplex(x[0],
					       wr, dr, er, wi, di, ei, OutDec));
		    }
		} else
		snprintf(pbuf, 115, "Complex,%d", LENGTH(tmp));
		break;
	    case STRSXP:
		if (LENGTH(tmp) == 1) {
		    const void *vmax = vmaxget();
		    /* This can potentially overflow */
		    const char *ctmp = translateChar(STRING_ELT(tmp, 0));
		    int len = (int) strlen(ctmp);
		    if(len < 100)
			snprintf(pbuf, 115, "\"%s\"", ctmp);
		    else {
			snprintf(pbuf, 101, "\"%s\"", ctmp);
			pbuf[100] = '"'; pbuf[101] = '\0';
			strcat(pbuf, " [truncated]");
		    }
		    vmaxset(vmax);
		} else
		snprintf(pbuf, 115, "Character,%d", LENGTH(tmp));
		break;
	    case RAWSXP:
		snprintf(pbuf, 115, "Raw,%d", LENGTH(tmp));
		break;
	    case LISTSXP:
	    case VECSXP:
		snprintf(pbuf, 115, "List,%d", length(tmp));
		break;
	    case LANGSXP:
		snprintf(pbuf, 115, "Expression");
		break;
	    default:
		snprintf(pbuf, 115, "?");
		break;
	    }
	    UNPROTECT(1); /* tmp */
	    pbuf[114] = '\0';
	    SET_STRING_ELT(t, i, mkChar(pbuf));
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    const char *rn, *cn;
	    GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    /* as from 1.5.0: don't quote here as didn't in array case */
	    printMatrix(t, 0, dims, 0, data->right, rl, cl,
			rn, cn);
	}
	else {
	    PROTECT(names = GetArrayDimnames(s));
	    printArray(t, dims, 0, Rprt_adj_left, names);
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    else { // no dim()
	PROTECT(names = getAttrib(s, R_NamesSymbol));
	taglen = (int) strlen(tagbuf);
	ptag = tagbuf + taglen;

	if(ns > 0) {
	    int n_pr = (ns <= data->max +1) ? ns : data->max;
	    /* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	    for (i = 0; i < n_pr; i++) {
		if (i > 0) Rprintf("\n");
		if (names != R_NilValue &&
		    STRING_ELT(names, i) != R_NilValue &&
		    *CHAR(STRING_ELT(names, i)) != '\0') {
		    const void *vmax = vmaxget();
		    /* Bug for L <- list(`a\\b` = 1, `a\\c` = 2)  :
		       const char *ss = translateChar(STRING_ELT(names, i));
		    */
		    const char *ss = EncodeChar(STRING_ELT(names, i));
#ifdef Win32
		    /* FIXME: double translation to native encoding, in
		         EncodeChar and translateChar; it is however necessary
			 to call isValidName() on a string without Rgui
			 escapes, because Rgui escapes cause a name to be
			 regarded invalid;
			 note also differences with printList
		    */
		    const char *st = ss;
		    if (WinUTF8out)
			st = translateChar(STRING_ELT(names, i));
#endif
		    if (taglen + strlen(ss) > TAGBUFLEN) {
			if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else {
			/* we need to distinguish character NA from "NA", which
			   is a valid (if non-syntactic) name */
			if (STRING_ELT(names, i) == NA_STRING)
			    sprintf(ptag, "$<NA>");
#ifdef Win32
			else if( isValidName(st) )
#else
			else if( isValidName(ss) )
#endif
			    sprintf(ptag, "$%s", ss);
			else
			    sprintf(ptag, "$`%s`", ss);
		    }
		    vmaxset(vmax);
		}
		else {
		    if (taglen + IndexWidth(i) > TAGBUFLEN) {
			if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else
			sprintf(ptag, "[[%d]]", i+1);
		}
                Rprintf("%s\n", tagbuf);
		PrintDispatch(VECTOR_ELT(s, i), data);
                *ptag = '\0';
	    }
	    Rprintf("\n");
	    if(n_pr < ns)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			ns - n_pr);
	}
	else { /* ns = length(s) == 0 */
	    const void *vmax = vmaxget();
	    /* Formal classes are represented as empty lists */
	    const char *className = NULL;
	    SEXP klass;
	    if(isObject(s) && isMethodsDispatchOn()) {
		klass = getAttrib(s, R_ClassSymbol);
		if(length(klass) == 1) {
		    /* internal version of isClass() */
		    char str[201];
		    const char *ss = translateChar(STRING_ELT(klass, 0));
		    snprintf(str, 200, ".__C__%s", ss);
		    if(findVar(install(str), data->env) != R_UnboundValue)
			className = ss;
		}
	    }
	    if(className) {
		Rprintf("An object of class \"%s\"\n", className);
		UNPROTECT(1); /* names */
		printAttributes(s, data, TRUE);
		vmaxset(vmax);
		return;
	    }
	    else {
		if(names != R_NilValue) Rprintf("named ");
		Rprintf("list()\n");
	    }
	    vmaxset(vmax);
	}
	UNPROTECT(1); /* names */
    }
    printAttributes(s, data, FALSE);
} // PrintGenericVector


// For pairlist()s only --- the predecessor of PrintGenericVector() above,
// and hence very similar  (and no longer compatible!)
static void printList(SEXP s, R_PrintData *data)
{
    int i, taglen;
    SEXP dims, dimnames, t;
    char pbuf[101], *ptag;
    const char *rn, *cn;

    if ((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	i = 0;
	while(s != R_NilValue) {
	    switch(TYPEOF(CAR(s))) {

	    case NILSXP:
		snprintf(pbuf, 100, "NULL");
		break;

	    case LGLSXP:
		snprintf(pbuf, 100, "Logical,%d", LENGTH(CAR(s)));
		break;

	    case INTSXP:
	    case REALSXP:
		snprintf(pbuf, 100, "Numeric,%d", LENGTH(CAR(s)));
		break;

	    case CPLXSXP:
		snprintf(pbuf, 100, "Complex,%d", LENGTH(CAR(s)));
		break;

	    case STRSXP:
		snprintf(pbuf, 100, "Character,%d", LENGTH(CAR(s)));
		break;

	    case RAWSXP:
		snprintf(pbuf, 100, "Raw,%d", LENGTH(CAR(s)));
		break;

	    case LISTSXP:
		snprintf(pbuf, 100, "List,%d", length(CAR(s)));
		break;

	    case LANGSXP:
		snprintf(pbuf, 100, "Expression");
		break;

	    default:
		snprintf(pbuf, 100, "?");
		break;
	    }
	    pbuf[100] ='\0';
	    SET_STRING_ELT(t, i++, mkChar(pbuf));
	    s = CDR(s);
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    printMatrix(t, 0, dims, data->quote, data->right, rl, cl,
			rn, cn);
	}
	else {
	    PROTECT(dimnames = getAttrib(s, R_DimNamesSymbol));
	    printArray(t, dims, 0, Rprt_adj_left, dimnames);
	    UNPROTECT(1);
	}
	UNPROTECT(2);
    }
    else { // no dim()
	i = 1;
	taglen = (int) strlen(tagbuf);
	ptag = tagbuf + taglen;
	while (TYPEOF(s) == LISTSXP) {
	    if (i > 1) Rprintf("\n");
	    if (TAG(s) != R_NilValue && isSymbol(TAG(s))) {
		if (taglen + strlen(CHAR(PRINTNAME(TAG(s)))) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else {
		    /* we need to distinguish character NA from "NA", which
		       is a valid (if non-syntactic) name */
		    if (PRINTNAME(TAG(s)) == NA_STRING)
			sprintf(ptag, "$<NA>");
		    else if( isValidName(CHAR(PRINTNAME(TAG(s)))) )
			sprintf(ptag, "$%s", CHAR(PRINTNAME(TAG(s))));
		    else
			sprintf(ptag, "$`%s`", EncodeChar(PRINTNAME(TAG(s))));
		}
	    }
	    else {
		if (taglen + IndexWidth(i) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else
		    sprintf(ptag, "[[%d]]", i);
	    }

            Rprintf("%s\n", tagbuf);
	    PrintDispatch(CAR(s), data);
            *ptag = '\0';

	    s = CDR(s);
	    i++;
	}
	if (s != R_NilValue) {
	    Rprintf("\n. \n\n");
	    PrintValueRec(s, data);
	}
	Rprintf("\n");
    }
    printAttributes(s, data, FALSE);
}

static void PrintExpression(SEXP s, R_PrintData *data)
{
    SEXP u;
    int i, n;

    u = PROTECT(deparse1w(s, 0, data->useSource | DEFAULTDEPARSE));
    R_print = *data; /* Deparsing calls PrintDefaults() */

    n = LENGTH(u);
    for (i = 0; i < n; i++)
	Rprintf("%s\n", CHAR(STRING_ELT(u, i))); /*translated */
    UNPROTECT(1); /* u */
}

static void PrintSpecial(SEXP s, R_PrintData *data)
{
    /* This is OK as .Internals are not visible to be printed */
    char *nm = PRIMNAME(s);
    SEXP env, s2;
    PROTECT_INDEX xp;
    PROTECT_WITH_INDEX(env = findVarInFrame3(R_BaseEnv,
					     install(".ArgsEnv"), TRUE),
		       &xp);
    if (TYPEOF(env) == PROMSXP) REPROTECT(env = eval(env, R_BaseEnv), xp);
    s2 = findVarInFrame3(env, install(nm), TRUE);
    if(s2 == R_UnboundValue) {
	REPROTECT(env = findVarInFrame3(R_BaseEnv,
					install(".GenericArgsEnv"), TRUE),
		  xp);
	if (TYPEOF(env) == PROMSXP)
	    REPROTECT(env = eval(env, R_BaseEnv), xp);
	s2 = findVarInFrame3(env, install(nm), TRUE);
    }
    if(s2 != R_UnboundValue) {
	SEXP t;
	PROTECT(s2);
	t = deparse1m(s2, 0, DEFAULTDEPARSE); // or deparse1() ?
	R_print = *data; /* Deparsing calls PrintDefaults() */

	Rprintf("%s ", CHAR(STRING_ELT(t, 0))); /* translated */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
	UNPROTECT(1);
    } else /* missing definition, e.g. 'if' */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
    UNPROTECT(1);
}

#ifdef Win32
static void print_cleanup(void *data)
{
    WinUTF8out = *(Rboolean *)data;
}
#endif

/* PrintValueRec -- recursively print an SEXP

 * This is the "dispatching" function for  print.default()
 */
void attribute_hidden PrintValueRec(SEXP s, R_PrintData *data)
{
    SEXP t;

#ifdef Win32
    RCNTXT cntxt;
    Rboolean havecontext = FALSE;
    Rboolean saveWinUTF8out = WinUTF8out;

    WinCheckUTF8();
    if (WinUTF8out != saveWinUTF8out) {
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
	             R_NilValue, R_NilValue);
	cntxt.cend = &print_cleanup;
	cntxt.cenddata = &saveWinUTF8out;
	havecontext = TRUE;
    }
#endif
    if(!isMethodsDispatchOn() && (IS_S4_OBJECT(s) || TYPEOF(s) == S4SXP) ) {
	SEXP cl = getAttrib(s, R_ClassSymbol);
	if(isNull(cl)) {
	    /* This might be a mistaken S4 bit set */
	    if(TYPEOF(s) == S4SXP)
		Rprintf("<S4 object without a class>\n");
	    else
		Rprintf("<Object of type '%s' with S4 bit but without a class>\n",
			type2char(TYPEOF(s)));
	} else {
	    SEXP pkg = getAttrib(s, R_PackageSymbol);
	    if(isNull(pkg)) {
		Rprintf("<S4 object of class \"%s\">\n",
			CHAR(STRING_ELT(cl, 0)));
	    } else {
		Rprintf("<S4 object of class \"%s\" from package '%s'>\n",
			CHAR(STRING_ELT(cl, 0)), CHAR(STRING_ELT(pkg, 0)));
	    }
	}
	goto done;
    }
    switch (TYPEOF(s)) {
    case NILSXP:
	Rprintf("NULL\n");
	break;
    case SYMSXP:
	/* Use deparse here to handle backtick quotification of "weird names". */
	t = deparse1(s, 0, SIMPLEDEPARSE); // TODO ? rather deparse1m()
	R_print = *data; /* Deparsing calls PrintDefaults() */
	Rprintf("%s\n", CHAR(STRING_ELT(t, 0))); /* translated */
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	PrintSpecial(s, data);
	break;
    case CHARSXP:
	Rprintf("<CHARSXP: ");
	Rprintf("%s", EncodeString(s, 0, '"', Rprt_adj_left));
	Rprintf(">\n");
	goto done; /* skip attribute printing for CHARSXP; they are used */
		   /* in managing the CHARSXP cache. */
    case EXPRSXP:
	PrintExpression(s, data);
	break;
    case LANGSXP:
	PrintLanguage(s, data);
	break;
    case CLOSXP:
	PrintClosure(s, data);
	break;
    case ENVSXP:
	Rprintf("%s\n", EncodeEnvironment(s));
	break;
    case PROMSXP:
	Rprintf("<promise: %p>\n", s);
	break;
    case DOTSXP:
	Rprintf("<...>\n");
	break;
    case VECSXP:
	PrintGenericVector(s, data); /* handles attributes/slots */
	goto done;
    case LISTSXP:
	printList(s, data);
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
	PROTECT(t = getAttrib(s, R_DimSymbol));
	if (TYPEOF(t) == INTSXP) {
	    if (LENGTH(t) == 1) {
		const void *vmax = vmaxget();
		PROTECT(t = getAttrib(s, R_DimNamesSymbol));
		if (t != R_NilValue && VECTOR_ELT(t, 0) != R_NilValue) {
		    SEXP nn = getAttrib(t, R_NamesSymbol);
		    const char *title = NULL;

		    if (!isNull(nn))
			title = translateChar(STRING_ELT(nn, 0));

		    printNamedVector(s, VECTOR_ELT(t, 0), data->quote, title);
		}
		else
		    printVector(s, 1, data->quote);
		UNPROTECT(1);
		vmaxset(vmax);
	    }
	    else if (LENGTH(t) == 2) {
		SEXP rl, cl;
		const char *rn, *cn;
		GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
		printMatrix(s, 0, t, data->quote, data->right, rl, cl,
			    rn, cn);
	    }
	    else {
		SEXP dimnames;
		PROTECT(dimnames = GetArrayDimnames(s));
		printArray(s, t, data->quote, data->right, dimnames);
		UNPROTECT(1);
	    }
	}
	else {
	    UNPROTECT(1);
	    PROTECT(t = getAttrib(s, R_NamesSymbol));
	    if (t != R_NilValue)
		printNamedVector(s, t, data->quote, NULL);
	    else
		printVector(s, 1, data->quote);
	}
	UNPROTECT(1);
	break;
    case EXTPTRSXP:
	Rprintf("<pointer: %p>\n", R_ExternalPtrAddr(s));
	break;
    case BCODESXP:
	Rprintf("<bytecode: %p>\n", s);
	break;
    case WEAKREFSXP:
	Rprintf("<weak reference>\n");
	break;
    case S4SXP:
	/*  we got here because no show method, usually no class.
	    Print the "slots" as attributes, since we don't know the class.
	*/
	Rprintf("<S4 Type Object>\n");
	break;
    default:
	UNIMPLEMENTED_TYPE("PrintValueRec", s);
    }
    printAttributes(s, data, FALSE);

done:

#ifdef Win32
    if (havecontext)
	endcontext(&cntxt);
    print_cleanup(&saveWinUTF8out);
#endif
    return; /* needed when Win32 is not defined */
}

/* 2000-12-30 PR#715: remove list tags from tagbuf here
   to avoid $a$battr("foo").  Need to save and restore, since
   attributes might be lists with attributes or just have attributes ...
 */
static void printAttributes(SEXP s, R_PrintData *data, Rboolean useSlots)
{
    SEXP a;
    char *ptag;
    char save[TAGBUFLEN0] = "\0";

    a = ATTRIB(s);
    if (a != R_NilValue) {
	/* guard against cycles through attributes on environments */
	if (strlen(tagbuf) > TAGBUFLEN0)
	    error(_("print buffer overflow"));
	strcpy(save, tagbuf);
	/* remove the tag if it looks like a list not an attribute */
	if (strlen(tagbuf) > 0 &&
	    *(tagbuf + strlen(tagbuf) - 1) != ')')
	    tagbuf[0] = '\0';
	ptag = tagbuf + strlen(tagbuf);
	while (a != R_NilValue) {
	    if(useSlots && TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    if(isArray(s) || isList(s)) {
		if(TAG(a) == R_DimSymbol ||
		   TAG(a) == R_DimNamesSymbol)
		    goto nextattr;
	    }
	    if(inherits(s, "factor")) {
		if(TAG(a) == R_LevelsSymbol)
		    goto nextattr;
		if(TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    }
	    if(isFrame(s)) {
		if(TAG(a) == R_RowNamesSymbol)
		    goto nextattr;
	    }
	    if(!isArray(s)) {
		if (TAG(a) == R_NamesSymbol)
		    goto nextattr;
	    }
	    if(TAG(a) == R_CommentSymbol || TAG(a) == R_SrcrefSymbol
	       || TAG(a) == R_WholeSrcrefSymbol || TAG(a) == R_SrcfileSymbol)
		goto nextattr;
	    if(useSlots)
		sprintf(ptag, "Slot \"%s\":", EncodeChar(PRINTNAME(TAG(a))));
	    else
		sprintf(ptag, "attr(,\"%s\")", EncodeChar(PRINTNAME(TAG(a))));
	    Rprintf("%s", tagbuf); Rprintf("\n");
	    if (TAG(a) == R_RowNamesSymbol) {
		/* need special handling AND protection */
		SEXP val;
		PROTECT(val = getAttrib(s, R_RowNamesSymbol));
		PrintValueRec(val, data);
		UNPROTECT(1);
		goto nextattr;
	    }
	    PrintDispatch(CAR(a), data);
	nextattr:
	    *ptag = '\0';
	    a = CDR(a);
	}
	strcpy(tagbuf, save);
    }
}/* printAttributes */


/* Print an S-expression using (possibly) local options.
   This is used for auto-printing from main.c */

void attribute_hidden PrintValueEnv(SEXP s, SEXP env)
{
    PrintDefaults();
    tagbuf[0] = '\0';
    PROTECT(s);

    R_PrintData data;
    PrintInit(&data, env);
    if (isFunction(s))
	/* printed via print() -> print.function() in order to allow user-defined
	   print.function() methods to also work in auto-printing: */
        PrintObject(s, &data);
    else
        PrintDispatch(s, &data);

    UNPROTECT(1);
}


/* Print an S-expression using global options */

void PrintValue(SEXP s)
{
    PrintValueEnv(s, R_GlobalEnv);
}


/* Ditto, but only for objects, for use in debugging */

void R_PV(SEXP s)
{
    if(isObject(s)) PrintValueEnv(s, R_GlobalEnv);
}


void attribute_hidden CustomPrintValue(SEXP s, SEXP env)
{
    tagbuf[0] = '\0';

    R_PrintData data;
    PrintInit(&data, env);
    PrintValueRec(s, &data);
}


/* xxxpr are mostly for S compatibility (as mentioned in V&R).
   The Fortran interfaces are in xxxpr.f and call these.
    They are always called with *nchar >= 0.
 */

#ifdef FC_LEN_T
# include <stddef.h>
#endif

attribute_hidden
#ifdef FC_LEN_T
void F77_NAME(dblep0) (const char *label, int *nchar, double *data, int *ndata,
		       const FC_LEN_T label_len)
#else
void F77_NAME(dblep0) (const char *label, int *nchar, double *data, int *ndata)
#endif
{
    int nc = *nchar;
    if(nc > 255) {
	warning(_("invalid character length in 'dblepr'"));
	nc = 0;
    } else if(nc > 0) {
	for (int k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printRealVector(data, *ndata, 1);
}

attribute_hidden
#ifdef FC_LEN_T
void F77_NAME(intpr0) (const char *label, int *nchar, int *data, int *ndata,
		       const FC_LEN_T label_len)
#else
void F77_NAME(intpr0) (const char *label, int *nchar, int *data, int *ndata)
#endif
{
    int nc = *nchar;

    if(nc > 255) {
	warning(_("invalid character length in 'intpr'"));
	nc = 0;
    } else if(nc > 0) {
	for (int k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printIntegerVector(data, *ndata, 1);
}

attribute_hidden
#ifdef FC_LEN_T
void F77_NAME(realp0) (const char *label, int *nchar, float *data, int *ndata,
		      const FC_LEN_T label_len)
#else
void F77_NAME(realp0) (const char *label, int *nchar, float *data, int *ndata)
#endif
{
    int nc = *nchar, nd = *ndata;
    double *ddata;

    if(nc > 255) {
	warning(_("invalid character length in 'realpr'"));
	nc = 0;
    }
    else if(nc > 0) {
	for (int k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(nd > 0) {
	ddata = (double *) malloc(nd*sizeof(double));
	if(!ddata) error(_("memory allocation error in 'realpr'"));
	for (int k = 0; k < nd; k++) ddata[k] = (double) data[k];
	printRealVector(ddata, nd, 1);
	free(ddata);
    }
}

/* Fortran-callable error routine for lapack */

#ifdef FC_LEN_T
void NORET F77_NAME(xerbla)(const char *srname, int *info, 
			    const FC_LEN_T srname_len)
#else
void NORET F77_NAME(xerbla)(const char *srname, int *info)
#endif
{
   /* srname is not null-terminated.  It will be 6 characters for
      mainstream BLAS/LAPACK routines (for those with < 6 the name
      is right space-padded), and > 6 for recentish additions from
      LAPACK, 7 for a few used with R ). */
#ifdef FC_LEN_T
    char buf[21];
    // Precaution for incorrectly passed length type
    int len = (srname_len > 20) ? (int)srname_len : 20;
    strncpy(buf, srname, len);
    buf[len] = '\0';
#else
    // This version will truncate to 6 chars and left space-pads
    // for fewer than 6 (at least with gfortran and Solaris).
    char buf[7];
    strncpy(buf, srname, 6);
    buf[6] = '\0';
#endif
    error(_("BLAS/LAPACK routine '%6s' gave error code %d"), buf, -(*info));
}
