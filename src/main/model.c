/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2017, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Core Team
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
 *
 *
 *  Model Formula Manipulation
 *
 *  Can you say ``recurse your brains out'';
 *  I knew you could. -- Mr Ro(ss)gers
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>

/* inline-able versions */
static R_INLINE Rboolean isUnordered_int(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits_CHAR (s, R_factor_CHARSXP)
	    && !inherits_CHAR (s, R_ordered_CHARSXP));
}

static R_INLINE Rboolean isOrdered_int(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits_CHAR (s, R_factor_CHARSXP)
	    && inherits_CHAR (s, R_ordered_CHARSXP));
}



#define WORDSIZE (8*sizeof(int))

static SEXP tildeSymbol = R_NoObject;
static SEXP plusSymbol  = R_NoObject;
static SEXP minusSymbol = R_NoObject;
static SEXP timesSymbol = R_NoObject;
static SEXP slashSymbol = R_NoObject;
static SEXP colonSymbol = R_NoObject;
static SEXP powerSymbol = R_NoObject;
static SEXP dotSymbol   = R_NoObject;
static SEXP parenSymbol = R_NoObject;
static SEXP inSymbol    = R_NoObject;
/* unused static SEXP identSymbol = R_NoObject; */


static int intercept;		/* intercept term in the model */
static int parity;		/* +/- parity */
static int response;		/* response term in the model */
static int nvar;		/* Number of variables in the formula */
static int nwords;		/* # of words (ints) to code a term */
static int nterm;		/* # of model terms */
static SEXP varlist;		/* variables in the model */
attribute_hidden
SEXP framenames;		/* variables names for specified frame */
/* NOTE: framenames can't be static because it must be protected from
   garbage collection. */
static Rboolean haveDot;	/* does RHS of formula contain `.'? */

static int isZeroOne(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return (asReal(x) == 0.0 || asReal(x) == 1.0);
}

static int isZero(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return asReal(x) == 0.0;
}

static int isOne(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return asReal(x) == 1.0;
}


/* MatchVar determines whether two ``variables'' are */
/* identical.  Expressions are identical if they have */
/* the same list structure and their atoms are identical. */
/* This is just EQUAL from lisp. */

static int MatchVar(SEXP var1, SEXP var2)
{
    /* For expedience, and sanity... */
    if ( var1 == var2 )
	return 1;
    /* Handle Nulls */
    if (isNull(var1) && isNull(var2))
	return 1;
    if (isNull(var1) || isNull(var2))
	return 0;
    /* Non-atomic objects - compare CARs & CDRs */
    if ((isList(var1) || isLanguage(var1)) &&
	(isList(var2) || isLanguage(var2)))
	return MatchVar(CAR(var1), CAR(var2)) &&
	       MatchVar(CDR(var1), CDR(var2));
    /* Symbols */
    if (isSymbol(var1) && isSymbol(var2))
	return (var1 == var2);
    /* Literal Numerics */
    if (isNumeric(var1) && isNumeric(var2))
	return (asReal(var1) == asReal(var2));
    /* Literal Strings */
    if (isString(var1) && isString(var2))
	return Seql(STRING_ELT(var1, 0), STRING_ELT(var2, 0));
    /* Nothing else matches */
    return 0;
}


/* InstallVar locates a ``variable'' in the model */
/* variable list;  adding it to the global varlist if not found. */

static int InstallVar(SEXP var)
{
    SEXP v;
    int indx;
    /* Check that variable is legitimate */
    if (!isSymbol(var) && !isLanguage(var) && !isZeroOne(var))
	error(_("invalid term in model formula"));
    /* Lookup/Install it */
    indx = 0;
    for (v = varlist; CDR(v) != R_NilValue; v = CDR(v)) {
	indx++;
	if (MatchVar(var, CADR(v)))
	    return indx;
    }
    SETCDR(v, CONS(var, R_NilValue));
    return indx + 1;
}


/* If there is a dotsxp being expanded then we need to see */
/* whether any of the variables in the data frame match with */
/* the variable on the lhs. If so they shouldn't be included */
/* in the factors */

static void CheckRHS(SEXP v)
{
    int i, j;
    SEXP s, t;
    while ((isList(v) || isLanguage(v)) && v != R_NilValue) {
	CheckRHS(CAR(v));
	v = CDR(v);
    }
    if (isSymbol(v) && framenames != R_NilValue) {
	for (i = 0; i < LENGTH(framenames); i++) {
	    s = install_translated (STRING_ELT(framenames,i));
	    if (v == s) {
		t = allocVector(STRSXP, LENGTH(framenames)-1);
		for (j = 0; j < LENGTH(t); j++) {
		    if (j < i)
			SET_STRING_ELT(t, j, STRING_ELT(framenames, j));
		    else
			SET_STRING_ELT(t, j, STRING_ELT(framenames, j+1));
		}
		framenames=t;
	    }
	}
    }
}


/* ExtractVars recursively extracts the variables */
/* in a model formula.  It calls InstallVar to do */
/* the installation.  The code takes care of unary */
/* + and minus.  No checks are made of the other */
/* ``binary'' operators.  Maybe there should be some. */

static void ExtractVars(SEXP formula, int checkonly)
{
    int len, i;
    SEXP v;

    if (isNull(formula) || isZeroOne(formula))
	return;
    if (isSymbol(formula)) {
	if (formula == dotSymbol) haveDot = TRUE;
	if (!checkonly) {
	    if (formula == dotSymbol && framenames != R_NilValue) {
		haveDot = TRUE;
		for (i = 0; i < LENGTH(framenames); i++) {
		    v = install_translated (STRING_ELT(framenames,i));
		    if (!MatchVar(v, CADR(varlist))) InstallVar(v);
		}
	    } else
		InstallVar(formula);
	}
	return;
    }
    if (isLanguage(formula)) {
	len = length(formula);
	if (CAR(formula) == tildeSymbol) {
	    if (response)
		error(_("invalid model formula"));
	    if (isNull(CDDR(formula))) {
		response = 0;
		ExtractVars(CADR(formula), 0);
	    }
	    else {
		response = 1;
		InstallVar(CADR(formula));
		ExtractVars(CADDR(formula), 0);
	    }
	    return;
	}
	if (CAR(formula) == plusSymbol) {
	    if (length(formula) > 1)
		ExtractVars(CADR(formula), checkonly);
	    if (length(formula) > 2)
		ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == colonSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == powerSymbol) {
	    if (!isNumeric(CADDR(formula)))
		error(_("invalid power in formula"));
	    ExtractVars(CADR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == timesSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == inSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == slashSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == minusSymbol) {
	    if (len == 2) {
		ExtractVars(CADR(formula), 1);
	    }
	    else {
		ExtractVars(CADR(formula), checkonly);
		ExtractVars(CADDR(formula), 1);
	    }
	    return;
	}
	if (CAR(formula) == parenSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    return;
	}
	InstallVar(formula);
	return;
    }
    error(_("invalid model formula in ExtractVars"));
}


/* AllocTerm allocates an integer array for */
/* bit string representation of a model term */

static SEXP AllocTerm(void)
{
    int i;
    SEXP term = allocVector(INTSXP, nwords);
    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = 0;
    return term;
}


/* SetBit sets bit ``whichBit'' to value ``value'' */
/* in the bit string representation of a term. */

static void SetBit(SEXP term, int whichBit, int value)
{
    int word, offset;
    word = (whichBit - 1) / WORDSIZE;
    offset = (WORDSIZE - whichBit) % WORDSIZE;
    if (value)
	((unsigned *) INTEGER(term))[word] |= ((unsigned) 1 << offset);
    else
	((unsigned *) INTEGER(term))[word] &= ~((unsigned) 1 << offset);
}


/* GetBit gets bit ``whichBit'' from the */
/* bit string representation of a term. */

static int GetBit(SEXP term, int whichBit)
{
    unsigned int word, offset;
    word = (whichBit - 1) / WORDSIZE;
    offset = (WORDSIZE - whichBit) % WORDSIZE;
    return ((((unsigned *) INTEGER(term))[word]) >> offset) & 1;
}


/* OrBits computes a new (bit string) term */
/* which contains the logical OR of the bits */
/* in ``term1'' and ``term2''. */

static SEXP OrBits(SEXP term1, SEXP term2)
{
    SEXP term;
    int i;
    term = AllocTerm();
    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = INTEGER(term1)[i] | INTEGER(term2)[i];
    return term;
}


/* BitCount counts the number of ``on'' */
/* bits in a term */

static int BitCount(SEXP term)
{
    int i, sum;
    sum = 0;
    for (i = 1; i <= nvar; i++)
	sum += GetBit(term, i);
    return sum;
}


/* TermZero tests whether a (bit string) term is zero */

static int TermZero(SEXP term)
{
    int i, val;
    val = 1;
    for (i = 0; i < nwords; i++)
	val = val && (INTEGER(term)[i] == 0);
    return val;
}


/* TermEqual tests two (bit string) terms for equality. */

static int TermEqual(SEXP term1, SEXP term2)
{
    int i, val;
    val = 1;
    for (i = 0; i < nwords; i++)
	val = val && (INTEGER(term1)[i] == INTEGER(term2)[i]);
    return val;
}


/* StripTerm strips the specified term from */
/* the given list.  This mutates the list. */

static SEXP StripTerm(SEXP term, SEXP list)
{
    SEXP tail;
    if (TermZero(term))
	intercept = 0;
    if (list == R_NilValue)
	return list;
    /* This can be highly recursive */
    R_CHECKSTACK();
    tail = StripTerm(term, CDR(list));
    if (TermEqual(term, CAR(list)))
	return tail;
    SETCDR(list, tail);
    return list;
}


/* TrimRepeats removes duplicates of (bit string) terms */
/* in a model formula by repeated use of ``StripTerm''. */
/* Also drops zero terms. */

static SEXP TrimRepeats(SEXP list)
{
    /* Highly recursive, but StripTerm does the checking */
    if (list == R_NilValue)
	return R_NilValue;
    if (TermZero(CAR(list)))
	return TrimRepeats(CDR(list));
    SETCDR(list, TrimRepeats(StripTerm(CAR(list), CDR(list))));
    return list;
}



/*==========================================================================*/

/* Model Formula Manipulation */
/* These functions take a numerically coded */
/* formula and fully expand it. */

static SEXP EncodeVars(SEXP);/* defined below */


/* PlusTerms expands ``left'' and ``right'' and */
/* concatenates their terms (removing duplicates). */

static SEXP PlusTerms(SEXP left, SEXP right)
{
    PROTECT(left = EncodeVars(left));
    right = EncodeVars(right);
    UNPROTECT(1);
    return TrimRepeats(listAppend(left, right));
}


/* InteractTerms expands ``left'' and ``right'' */
/* and forms a new list of terms containing the bitwise */
/* OR of each term in ``left'' with each term in ``right''. */

static SEXP InteractTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = allocList(length(left) * length(right)));
    t = term;
    for (l = left; l != R_NilValue; l = CDR(l))
	for (r = right; r != R_NilValue; r = CDR(r)) {
	    SETCAR(t, OrBits(CAR(l), CAR(r)));
	    t = CDR(t);
	}
    UNPROTECT(3);
    return TrimRepeats(term);
}


/* CrossTerms expands ``left'' and ``right'' */
/* and forms the ``cross'' of the list of terms.  */
/* Duplicates are removed. */

static SEXP CrossTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = allocList(length(left) * length(right)));
    t = term;
    for (l = left; l != R_NilValue; l = CDR(l))
	for (r = right; r != R_NilValue; r = CDR(r)) {
	    SETCAR(t, OrBits(CAR(l), CAR(r)));
	    t = CDR(t);
	}
    UNPROTECT(3);
    listAppend(right, term);
    listAppend(left, right);
    return TrimRepeats(left);
}


/* PowerTerms expands the ``left'' form and then */
/* raises it to the power specified by the right term. */
/* Allocation here is wasteful, but so what ... */

static SEXP PowerTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    int i, ip;
    ip = asInteger(right);
    if (ip==NA_INTEGER || ip <= 1)
	error(_("invalid power in formula"));
    term = R_NilValue;		/* -Wall */
    PROTECT(left = EncodeVars(left));
    right = left;
    for (i=1; i < ip; i++)  {
	PROTECT(right);
	PROTECT(term = allocList(length(left) * length(right)));
	t = term;
	for (l = left; l != R_NilValue; l = CDR(l))
	    for (r = right; r != R_NilValue; r = CDR(r)) {
		SETCAR(t, OrBits(CAR(l), CAR(r)));
		t = CDR(t);
	    }
	UNPROTECT(2);
	right = TrimRepeats(term);
    }
    UNPROTECT(1);
    return term;
}


/* InTerms expands ``left'' and ``right'' and */
/* forms the ``nest'' of the the left in the */
/* interaction of the right */

static SEXP InTerms(SEXP left, SEXP right)
{
    SEXP term, t;
    int i;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = AllocTerm());
    /* Bitwise or of all terms on right */
    for (t = right; t != R_NilValue; t = CDR(t)) {
	for (i = 0; i < nwords; i++)
	    INTEGER(term)[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    }
    /* Now bitwise or with each term on the left */
    for (t = left; t != R_NilValue; t = CDR(t))
	for (i = 0; i < nwords; i++)
	    INTEGER(CAR(t))[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    UNPROTECT(3);
    return TrimRepeats(left);
}

/* NestTerms expands ``left'' and ``right'' */
/* and forms the ``nest'' of the list of terms.  */
/* Duplicates are removed. */

static SEXP NestTerms(SEXP left, SEXP right)
{
    SEXP term, t;
    int i;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = AllocTerm());
    /* Bitwise or of all terms on left */
    for (t = left; t != R_NilValue; t = CDR(t)) {
	for (i = 0; i < nwords; i++)
	    INTEGER(term)[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    }
    /* Now bitwise or with each term on the right */
    for (t = right; t != R_NilValue; t = CDR(t))
	for (i = 0; i < nwords; i++)
	    INTEGER(CAR(t))[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    UNPROTECT(3);
    listAppend(left, right);
    return TrimRepeats(left);
}


/* DeleteTerms expands ``left'' and ``right'' */
/* and then removes any terms which appear in */
/* ``right'' from ``left''. */

static SEXP DeleteTerms(SEXP left, SEXP right)
{
    SEXP t;
    PROTECT(left = EncodeVars(left));
    parity = 1-parity;
    PROTECT(right = EncodeVars(right));
    parity = 1-parity;
    for (t = right; t != R_NilValue; t = CDR(t))
	left = StripTerm(CAR(t), left);
    UNPROTECT(2);
    return left;
}


/* EncodeVars performs  model expansion and bit string encoding. */
/* This is the real workhorse of model expansion. */

static SEXP EncodeVars(SEXP formula)
{
    SEXP term;
    int len;

    if (isNull(formula))
	return R_NilValue;

    if (isOne(formula)) {
	if (parity) intercept = 1;
	else intercept = 0;
	return R_NilValue;
    }
    else if (isZero(formula)) {
	if (parity) intercept = 0;
	else intercept = 1;
	return R_NilValue;
    }
    if (isSymbol(formula)) {
	if (formula == dotSymbol && framenames != R_NilValue) {
	    /* prior to 1.7.0 this made term.labels in reverse order. */
	    SEXP r = R_NilValue, v = R_NilValue; /* -Wall */
	    int i, j; const char *c;

	    if (!LENGTH(framenames)) return r;
	    for (i = 0; i < LENGTH(framenames); i++) {
		/* change in 1.6.0 do not use duplicated names */
		c = translateChar(STRING_ELT(framenames, i));
		for(j = 0; j < i; j++)
		    if(!strcmp(c, translateChar(STRING_ELT(framenames, j))))
			error(_("duplicated name '%s' in data frame using '.'"),
			      c);
		int cIndex = InstallVar(install(c));
		term = AllocTerm();
		SetBit(term, cIndex, 1);
		if(i == 0) PROTECT(v = r = cons(term, R_NilValue));
		else {SETCDR(v, CONS(term, R_NilValue)); v = CDR(v);}
	    }
	    UNPROTECT(1);
	    return r;
	}
	else {
	    int formulaIndex = InstallVar(formula);
	    term = AllocTerm();
	    SetBit(term, formulaIndex, 1);
	    return CONS(term, R_NilValue);
	}
    }
    if (isLanguage(formula)) {
	len = length(formula);
	if (CAR(formula) == tildeSymbol) {
	    if (isNull(CDDR(formula)))
		return EncodeVars(CADR(formula));
	    else
		return EncodeVars(CADDR(formula));
	}
	if (CAR(formula) == plusSymbol) {
	    if (len == 2)
		return EncodeVars(CADR(formula));
	    else
		return PlusTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == colonSymbol) {
	    return InteractTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == timesSymbol) {
	    return CrossTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == inSymbol) {
	    return InTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == slashSymbol) {
	    return NestTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == powerSymbol) {
	    return PowerTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == minusSymbol) {
	    if (len == 2)
		return DeleteTerms(R_NilValue, CADR(formula));
	    return DeleteTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == parenSymbol) {
	    return EncodeVars(CADR(formula));
	}
	int formulaIndex = InstallVar(formula);
	term = AllocTerm();
	SetBit(term, formulaIndex, 1);
	return CONS(term, R_NilValue);
    }
    error(_("invalid model formula in EncodeVars"));
}


/* TermCode decides on the encoding of a model term. */
/* Returns 1 if variable ``whichBit'' in ``thisTerm'' */
/* is to be encoded by contrasts and 2 if it is to be */
/* encoded by dummy variables.  This is decided using */
/* the heuristic described in Statistical Models in S, page 38. */

static int TermCode(SEXP termlist, SEXP thisterm, int whichbit, SEXP term)
{
    SEXP t;
    int allzero, i;

    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = INTEGER(CAR(thisterm))[i];

    /* Eliminate factor ``whichbit'' */

    SetBit(term, whichbit, 0);

    /* Search preceding terms for a match */
    /* Zero is a possibility - it is a special case */

    allzero = 1;
    for (i = 0; i < nwords; i++) {
	if (INTEGER(term)[i]) {
	    allzero = 0;
	    break;
	}
    }
    if (allzero)
	return 1;

    for (t = termlist; t != thisterm; t = CDR(t)) {
	allzero = 1;
	for (i = 0; i < nwords; i++) {
	    if ((~(INTEGER(CAR(t))[i])) & INTEGER(term)[i])
		allzero = 0;
	}
	if (allzero)
	    return 1;
    }
    return 2;
}


/* Internal code for the ``terms'' function */
/* The value is a formula with an assortment */
/* of useful attributes. */

/* .Internal(terms.formula(x, new.specials, abb, data, keep.order)) */

static SEXP ExpandDots(SEXP object, SEXP value);

static SEXP do_termsform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, v, pattern, formula, varnames, term, termlabs, ord;
    SEXP specials, t, data, rhs;
    int i, j, k, l, n, keepOrder, allowDot;

    Rboolean hadFrameNames = FALSE;

    checkArity(op, args);

    /* Always fetch these values rather than trying */
    /* to remember them between calls.  The overhead */
    /* is minimal and we don't have to worry about */
    /* intervening dump/restore problems. */

    tildeSymbol = install("~");
    plusSymbol  = install("+");
    minusSymbol = install("-");
    timesSymbol = install("*");
    slashSymbol = install("/");
    colonSymbol = install(":");
    powerSymbol = install("^");
    dotSymbol   = install(".");
    parenSymbol = install("(");
    inSymbol = install("%in%");
    /* identSymbol = install("I"); */

    /* Do we have a model formula? */
    /* Check for unary or binary ~ */

    if (!isLanguage(CAR(args)) ||
	CAR(CAR(args)) != tildeSymbol ||
	(length(CAR(args)) != 2 && length(CAR(args)) != 3))
	error(_("argument is not a valid model"));

    haveDot = FALSE;

    PROTECT(ans = duplicate(CAR(args)));

    /* The formula will be returned, modified if haveDot becomes TRUE */

    specials = CADR(args);
    if(length(specials) && !isString(specials))
	error(_("'specials' must be NULL or a character vector"));
    a = CDDR(args);

    /* We use data to get the value to */
    /* substitute for "." in formulae */

    data = CAR(a);
    a = CDR(a);
    if (isNull(data) || isEnvironment(data))
	framenames = R_NilValue;
    else if (isFrame(data))
	framenames = getAttrib(data, R_NamesSymbol);
    else
	error(_("'data' argument is of the wrong type"));

    if (framenames != R_NilValue) {
	if(length(framenames)) hadFrameNames = TRUE;
	if (length(CAR(args))== 3)
	    CheckRHS(CADR(CAR(args)));
    }

    /* Preserve term order? */

    keepOrder = asLogical(CAR(a));
    if (keepOrder == NA_LOGICAL)
	keepOrder = 0;

    a = CDR(a);
    allowDot = asLogical(CAR(a));
    if (allowDot == NA_LOGICAL) allowDot = 0;

    if (specials == R_NilValue) {
	a = allocList(8);
	SET_ATTRIB(ans, a);
    }
    else {
	a = allocList(9);
	SET_ATTRIB(ans, a);
    }

    /* Step 1: Determine the ``variables'' in the model */
    /* Here we create an expression of the form */
    /* list(...).  You can evaluate it to get */
    /* the model variables or use substitute and then */
    /* pull the result apart to get the variable names. */

    intercept = 1;
    parity = 1;
    response = 0;
    PROTECT(varlist = LCONS(install("list"), R_NilValue));
    ExtractVars(CAR(args), 1);
    UNPROTECT(1);
    SETCAR(a, varlist);
    SET_TAG(a, install("variables"));
    a = CDR(a);

    nvar = length(varlist) - 1;

    /* In allocating words need to allow for intercept term */
    nwords = (int) (nvar / WORDSIZE + 1);

    /* Step 2: Recode the model terms in binary form */
    /* and at the same time, expand the model formula. */

    /* FIXME: this includes specials in the model */
    /* There perhaps needs to be a an extra pass */
    /* through the model to delete any terms which */
    /* contain specials.  Actually, specials should */
    /* only enter additively so this should also be */
    /* checked and abort forced if not. */

    /* BDR 2002-01-29: S does include specials, so code may rely on this */

    /* FIXME: this is also the point where nesting */
    /* needs to be taken care of. */

    PROTECT(formula = EncodeVars(CAR(args)));

    nvar = length(varlist) - 1; /* need to recompute, in case
				   EncodeVars stretched it */

    /* Step 2a: Compute variable names */

    PROTECT(varnames = allocVector(STRSXP, nvar));
    for (v = CDR(varlist), i = 0; v != R_NilValue; v = CDR(v)) {
        SEXP str = deparse1line(CAR(v), 0);
	SET_STRING_ELT(varnames, i++, STRING_ELT(str, 0));
    }

    /* Step 2b: Find and remove any offset(s) */

    /* first see if any of the variables are offsets */
    for (l = response, k = 0; l < nvar; l++)
	if (!strncmp(CHAR(STRING_ELT(varnames, l)), "offset(", 7)) k++;
    if (k > 0) {
	Rboolean foundOne = FALSE; /* has there been a non-offset term? */
	/* allocate the "offsets" attribute */
	SETCAR(a, v = allocVector(INTSXP, k));
	for (l = response, k = 0; l < nvar; l++)
	    if (!strncmp(CHAR(STRING_ELT(varnames, l)), "offset(", 7))
		INTEGER(v)[k++] = l + 1;
	SET_TAG(a, install("offset"));
	a = CDR(a);
	/* now remove offset terms from the formula */
	call = formula; /* call is to be the previous term once one is found */
	while (1) {
	    SEXP thisterm = foundOne ? CDR(call) : call;
	    Rboolean have_offset = FALSE;
	    if(length(thisterm) == 0) break;
	    for (i = 1; i <= nvar; i++)
		if (GetBit(CAR(thisterm), i) &&
		    !strncmp(CHAR(STRING_ELT(varnames, i-1)), "offset(", 7)) {
		    have_offset = TRUE;
		    break;
		}
	    if (have_offset) {
		if (!foundOne) call = formula = CDR(formula);
		else SETCDR(call, CDR(thisterm));
	    } else {
		if (foundOne) call = CDR(call);
		foundOne = TRUE;
	    }
	}
    }
    nterm = length(formula);

    /* Step 3: Reorder the model terms by BitCount, otherwise
       preserving their order. */

    PROTECT(ord = allocVector(INTSXP, nterm));
    {
	SEXP sCounts;
	int *counts, bitmax = 0, *iord = INTEGER(ord), m = 0;

	PROTECT(pattern = allocVector(VECSXP, nterm));
	PROTECT(sCounts = allocVector(INTSXP, nterm));
	counts = INTEGER(sCounts);
	for (call = formula, n = 0; call != R_NilValue; call = CDR(call)) {
	    SET_VECTOR_ELT(pattern, n, CAR(call));
	    counts[n++] = BitCount(CAR(call));
	}
	for (n = 0; n < nterm; n++)
	    if(counts[n] > bitmax) bitmax = counts[n];
	if(keepOrder) {
	    for (n = 0; n < nterm; n++)
		iord[n] = counts[n];
	} else {
	    call = formula;
	    m = 0;
	    for (i = 0; i <= bitmax; i++) /* can order 0 occur? */
		for (n = 0; n < nterm; n++)
		    if (counts[n] == i) {
			SETCAR(call, VECTOR_ELT(pattern, n));
			call = CDR(call);
			iord[m++] = i;
		    }
	}
	UNPROTECT(2);
    }


    /* Step 4: Compute the factor pattern for the model. */
    /* 0 - the variable does not appear in this term. */
    /* 1 - code the variable by contrasts in this term. */
    /* 2 - code the variable by indicators in this term. */

    if (nterm > 0) {
	SETCAR(a, pattern = allocMatrix(INTSXP, nvar, nterm));
	SET_TAG(a, install("factors"));
	a = CDR(a);
	for (i = 0; i < nterm * nvar; i++)
	    INTEGER(pattern)[i] = 0;
	PROTECT(term = AllocTerm());
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call)) {
	    for (i = 1; i <= nvar; i++) {
		if (GetBit(CAR(call), i))
		    INTEGER(pattern)[i-1+n*nvar] =
			TermCode(formula, call, i, term);
	    }
	    n++;
	}
	UNPROTECT(1);
    }
    else {
	SETCAR(a, pattern = allocVector(INTSXP,0));
	SET_TAG(a, install("factors"));
	a = CDR(a);
    }

    /* Step 5: Compute term labels */

    PROTECT(termlabs = allocVector(STRSXP, nterm));
    n = 0;
    for (call = formula; call != R_NilValue; call = CDR(call)) {
	l = 0;
	for (i = 1; i <= nvar; i++) {
	    if (GetBit(CAR(call), i)) {
		if (l > 0)
		    l += 1;
		l += strlen(CHAR(STRING_ELT(varnames, i - 1)));
	    }
	}
	char cbuf[l+1];
	cbuf[0] = '\0';
	l = 0;
	for (i = 1; i <= nvar; i++) {
	    if (GetBit(CAR(call), i)) {
		if (l > 0)
		    strcat(cbuf, ":");
		strcat(cbuf, CHAR(STRING_ELT(varnames, i - 1)));
		l++;
	    }
	}
	SET_STRING_ELT(termlabs, n, mkChar(cbuf));
	n++;
    }
    PROTECT(v = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(v, 0, varnames);
    SET_VECTOR_ELT(v, 1, termlabs);
    if (nterm > 0)
	setAttrib(pattern, R_DimNamesSymbol, v);

    SETCAR(a, termlabs);
    SET_TAG(a, install("term.labels"));
    a = CDR(a);

    /* If there are specials stick them in here */

    if (specials != R_NilValue) {
	i = length(specials);
	PROTECT(v = allocList(i));
	for (j = 0, t = v; j < i; j++, t = CDR(t)) {
	    const char *ss = translateChar(STRING_ELT(specials, j));
	    SET_TAG(t, install(ss));
	    n = strlen(ss);
	    SETCAR(t, allocVector(INTSXP, 0));
	    k = 0;
	    for (l = 0; l < nvar; l++) {
		if (!strncmp(CHAR(STRING_ELT(varnames, l)), ss, n))
		    if (CHAR(STRING_ELT(varnames, l))[n] == '(')
			k++;
	    }
	    if (k > 0) {
		SETCAR(t, allocVector(INTSXP, k));
		k = 0;
		for (l = 0; l < nvar; l++) {
		    if (!strncmp(CHAR(STRING_ELT(varnames, l)), ss, n))
			if (CHAR(STRING_ELT(varnames, l))[n] == '('){
			    INTEGER(CAR(t))[k++] = l+1;
			}
		}
	    }
	    else
                SETCAR_NIL(t);
	}
	SETCAR(a, v);
	SET_TAG(a, install("specials"));
	a = CDR(a);
	UNPROTECT(1);
    }

    UNPROTECT(2);	/* keep termlabs until here */

    /* Step 6: Fix up the formula by substituting for dot, which should be
       the framenames joined by + */

    if (haveDot) {
	if(length(framenames)) {
	    PROTECT_INDEX ind;
	    PROTECT_WITH_INDEX(rhs = install_translated (STRING_ELT(framenames,0)),
			       &ind);
	    for (i = 1; i < LENGTH(framenames); i++) {
		REPROTECT(rhs = lang3(plusSymbol, rhs,
				      install_translated (STRING_ELT(framenames,i))),
			  ind);
	    }
	    if (!isNull(CADDR(ans)))
		SETCADDR(ans, ExpandDots(CADDR(ans), rhs));
	    else
		SETCADR(ans, ExpandDots(CADR(ans), rhs));
	    UNPROTECT(1);
	} else if(!allowDot && !hadFrameNames) {
	    error(_("'.' in formula and no 'data' argument"));
	}
    }

    SETCAR(a, allocVector(INTSXP, nterm));
    n = 0;
    {
	int *ia = INTEGER(CAR(a)), *iord = INTEGER(ord);
	for (call = formula; call != R_NilValue; call = CDR(call), n++)
	    ia[n] = iord[n];
    }

    SET_TAG(a, install("order"));
    a = CDR(a);

    SETCAR(a, ScalarInteger(intercept != 0));
    SET_TAG(a, install("intercept"));
    a = CDR(a);

    SETCAR(a, ScalarInteger(response != 0));
    SET_TAG(a, install("response"));
    a = CDR(a);

    SETCAR(a, mkString("terms"));
    SET_TAG(a, install("class"));
    SET_OBJECT(ans, 1);

    SETCDR_NIL(a);  /* truncate if necessary */

    UNPROTECT(4);
    return ans;
}

/* Update a model formula by the replacement of "." templates. */

static SEXP ExpandDots(SEXP object, SEXP value)
{
    SEXP op;

    if (TYPEOF(object) == SYMSXP) {
	if (object == dotSymbol)
	    object = duplicate(value);
	return object;
    }

    if (TYPEOF(object) == LANGSXP) {
	op = TYPEOF(value) == LANGSXP ? CAR(value) : R_NoObject;
	PROTECT(object);
	if (CAR(object) == plusSymbol) {
	    if (length(object) == 2) {
		SETCADR(object, ExpandDots(CADR(object), value));
	    }
	    else if (length(object) == 3) {
		SETCADR(object, ExpandDots(CADR(object), value));
		SETCADDR(object, ExpandDots(CADDR(object), value));
	    }
	    else goto badformula;
	}
	else if (CAR(object) == minusSymbol) {
	    if (length(object) == 2) {
		if (CADR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADR(object, lang2(parenSymbol,
					  ExpandDots(CADR(object), value)));
		else
		    SETCADR(object, ExpandDots(CADR(object), value));
	    }
	    else if (length(object) == 3) {
		if (CADR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADR(object, lang2(parenSymbol,
					  ExpandDots(CADR(object), value)));
		else
		    SETCADR(object, ExpandDots(CADR(object), value));
		if (CADDR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADDR(object, lang2(parenSymbol,
					   ExpandDots(CADDR(object), value)));
		else
		    SETCADDR(object, ExpandDots(CADDR(object), value));
	    }
	    else goto badformula;
	}
	else if (CAR(object) == timesSymbol || CAR(object) == slashSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else if (CAR(object) == colonSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol ||
		op == timesSymbol || op == slashSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else if (CAR(object) == powerSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol ||
		op == timesSymbol || op == slashSymbol ||
		op == colonSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else {
	    op = object;
	    while(op != R_NilValue) {
		SETCAR(op, ExpandDots(CAR(op), value));
		op = CDR(op);
	    }
	}
	UNPROTECT(1);
	return object;
    }
    else return object;

 badformula:
    error(_("invalid formula in 'update'"));
}

static SEXP do_updateform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP _new, old, lhs, rhs;

    checkArity(op, args);

    /* Always fetch these values rather than trying */
    /* to remember them between calls.  The overhead */
    /* is minimal and we don't have to worry about */
    /* intervening dump/restore problems. */

    tildeSymbol = install("~");
    plusSymbol  = install("+");
    minusSymbol = install("-");
    timesSymbol = install("*");
    slashSymbol = install("/");
    colonSymbol = install(":");
    powerSymbol = install("^");
    dotSymbol   = install(".");
    parenSymbol = install("(");
    inSymbol = install("%in%");
    /* identSymbol = install("I"); */

    /* We must duplicate here because the */
    /* formulae may be part of the parse tree */
    /* and we don't want to modify it. */

    old = CAR(args);
    _new = SETCADR(args, duplicate(CADR(args)));

    /* Check of new and old formulae. */
    if (TYPEOF(old) != LANGSXP ||
       (TYPEOF(_new) != LANGSXP && CAR(old) != tildeSymbol) ||
       CAR(_new) != tildeSymbol)
	error(_("formula expected"));

    if (length(old) == 3) {
	lhs = CADR(old);
	rhs = CADDR(old);
	/* We now check that new formula has a valid lhs.
	   If it doesn't, we add one and set it to the rhs of the old
	   formula. */
	if (length(_new) == 2)
	    SETCDR(_new, CONS(lhs, CDR(_new)));
	/* Now we check the left and right sides of the new formula
	   and substitute the correct value for any "." templates.
	   We must parenthesize the rhs or we might upset arity and
	   precedence. */
	PROTECT(rhs);
	SETCADR(_new, ExpandDots(CADR(_new), lhs));
	SETCADDR(_new, ExpandDots(CADDR(_new), rhs));
	UNPROTECT(1);
    }
    else {
	/* The old formula had no lhs, so we only expand the rhs of the
	   new formula. */
	rhs = CADR(old);
	if (length(_new) == 3)
	    SETCADDR(_new, ExpandDots(CADDR(_new), rhs));
	else
	    SETCADR(_new, ExpandDots(CADR(_new), rhs));
    }

    /* It might be overkill to zero the */
    /* the attribute list of the returned */
    /* value, but it can't hurt. */

    SET_ATTRIB(_new, R_NilValue);
    SET_OBJECT(_new, 0);
    setAttrib(_new, R_DotEnvSymbol, getAttrib(old, R_DotEnvSymbol));

    return _new;
}



/*
 *  model.frame
 *
 *  The argument "terms" contains the terms object generated from the
 *  model formula.  We first evaluate the "variables" attribute of
 *  "terms" in the "data" environment.  This gives us a list of basic
 *  variables to be in the model frame.  We do some basic sanity
 *  checks on these to ensure that resulting object make sense.
 *
 *  The argument "dots" gives additional things like "weights", "offsets"
 *  and "subset" which will also go into the model frame so that they can
 *  be treated in parallel.
 *
 *  Next we subset the data frame according to "subset" and finally apply
 *  "na.action" to get the final data frame.
 *
 *  Note that the "terms" argument is glued to the model frame as an
 *  attribute.  Code downstream appears to need this.
 *
 *  Q: Is this really needed, or can we get by with less info?
 */

/* time to move more functionality back into compiled
   code (cycle of reincarnation) */

/* .Internal(model.frame(terms, rownames, variables, varnames, */
/*           dots, dotnames, subset, na.action)) */

static SEXP do_modelframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP terms, data, names, variables, varnames, dots, dotnames, na_action;
    SEXP ans, row_names, subset, tmp;
    char buf[256];
    int i, j, nr, nc;
    int nvars, ndots, nactualdots;

    checkArity(op, args);
    terms = CAR(args); args = CDR(args);
    row_names = CAR(args); args = CDR(args);
    variables = CAR(args); args = CDR(args);
    varnames = CAR(args); args = CDR(args);
    dots = CAR(args); args = CDR(args);
    dotnames = CAR(args); args = CDR(args);
    subset = CAR(args); args = CDR(args);
    na_action = CAR(args); args = CDR(args);

    /* Argument Sanity Checks */

    if (!isNewList(variables))
	error(_("invalid variables"));
    if (!isString(varnames))
	error(_("invalid variable names"));
    if ((nvars = length(variables)) != length(varnames))
	error(_("number of variables != number of variable names"));

    if (!isNewList(dots))
	error(_("invalid extra variables"));
    if ((ndots = length(dots)) != length(dotnames))
	error(_("number of variables != number of variable names"));
    if ( ndots && !isString(dotnames))
	error(_("invalid extra variable names"));

    /*  check for NULL extra arguments -- moved from interpreted code */

    nactualdots = 0;
    for (i = 0; i < ndots; i++)
	if (VECTOR_ELT(dots, i) != R_NilValue) nactualdots++;

    /* Assemble the base data frame. */

    PROTECT(data = allocVector(VECSXP, nvars + nactualdots));
    PROTECT(names = allocVector(STRSXP, nvars + nactualdots));

    for (i = 0; i < nvars; i++) {
	SET_VECTOR_ELT(data, i, VECTOR_ELT(variables, i));
	SET_STRING_ELT(names, i, STRING_ELT(varnames, i));
    }
    for (i = 0,j = 0; i < ndots; i++) {
	const char *ss;
	if (VECTOR_ELT(dots, i) == R_NilValue) continue;
	ss = translateChar(STRING_ELT(dotnames, i));
        if (!copy_3_strings (buf, 256, "(", ss, ")"))
	    error(_("overlong names in '%s'"), ss);
	SET_VECTOR_ELT(data, nvars + j, VECTOR_ELT(dots, i));
	SET_STRING_ELT(names, nvars + j,  mkChar(buf));
	j++;
    }
    setAttrib(data, R_NamesSymbol, names);
    UNPROTECT(2);

    /* Sanity checks to ensure that the the answer can become */
    /* a data frame.  Be deeply suspicious here! */

    nc = length(data);
    if (nc > 0) {
	nr = nrows(VECTOR_ELT(data, 0));
	for (i = 0; i < nc; i++) {
	    ans = VECTOR_ELT(data, i);
	    switch(TYPEOF(ans)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case CPLXSXP:
	    case STRSXP:
	    case RAWSXP:
		break;
	    default:
		error(_("invalid type (%s) for variable '%s'"),
		      type2char(TYPEOF(ans)),
		      translateChar(STRING_ELT(names, i)));
	    }
	    if (nrows(ans) != nr)
		error(_("variable lengths differ (found for '%s')"),
		      translateChar(STRING_ELT(names, i)));
	}
    }
    else 
        nr = length(row_names);

    PROTECT(data);
    PROTECT(subset);

    /* Turn the data "list" into a "data.frame" */
    /* so that subsetting methods will work. */
    /* To do this we must attach "class"  and */
    /* "row.names" attributes */

    PROTECT(tmp = mkString("data.frame"));
    setAttrib(data, R_ClassSymbol, tmp);
    UNPROTECT(1);
    if (length(row_names) == nr) {
	setAttrib(data, R_RowNamesSymbol, row_names);
    } else {
	/*
	PROTECT(row_names = allocVector(INTSXP, nr));
	for (i = 0; i < nr; i++) INTEGER(row_names)[i] = i+1; */
	PROTECT(row_names = allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nr;
	setAttrib(data, R_RowNamesSymbol, row_names);
	UNPROTECT(1);
    }

    /* Do the subsetting, if required. */
    /* Need to save and restore 'most' attributes */

    if (subset != R_NilValue) {
	PROTECT(tmp=install("[.data.frame"));
	PROTECT(tmp=LCONS(tmp,list4(data,subset,R_MissingArg,mkFalse())));
	data = eval(tmp, rho);
	UNPROTECT(2);
    }
    UNPROTECT(2);
    PROTECT(data);

    /* finally, we run na.action on the data frame */
    /* usually, this will be na.omit */

    if (na_action != R_NilValue) {
	/* some na.actions need this to distinguish responses from
	   explanatory variables */
	setAttrib(data, install("terms"), terms);
	if (isString(na_action) && length(na_action) > 0)
	    na_action = install_translated (STRING_ELT(na_action,0));
	PROTECT(na_action);
	PROTECT(tmp = lang2(na_action, data));
	PROTECT(ans = eval(tmp, rho));
        if (ans != data) {
            if (NAMEDCNT_GT_0 (ans)) {
                ans = duplicate(ans);
                UNPROTECT(1);
                PROTECT(ans);
            }
            if (!isNewList(ans) || length(ans) != length(data))
                error(_("invalid result from na.action"));
            /* need to transfer _all but tsp and dim_ attributes, possibly lost
               by subsetting in na.action.  */
            for ( i = length(ans) ; i-- ; ) {
                if (NAMEDCNT_GT_0 (VECTOR_ELT(ans,i)))
                    SET_VECTOR_ELT (ans, i, duplicate(VECTOR_ELT(ans,i)));
                copyMostAttribNoTs(VECTOR_ELT(data, i),VECTOR_ELT(ans, i));
            }
        }

	UNPROTECT(3);
    }
    else ans = data;
    UNPROTECT(1);
    PROTECT(ans);

    /* Finally, tack on a terms attribute
       Now done at R level.
       setAttrib(ans, install("terms"), terms); */
    UNPROTECT(1);
    return ans;
}

	/* Internal code for the ~ operator */
	/* Just returns the unevaluated call */
	/* No longer needed??? */

static SEXP do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    if (isObject(call))
	return duplicate(call);
    else {
	SEXP klass;
	PROTECT(call = duplicate(call));
	PROTECT(klass = mkString("formula"));
	setAttrib(call, R_ClassSymbol, klass);
	setAttrib(call, R_DotEnvSymbol, rho);
	UNPROTECT(2);
	return call;
    }
}


	/* The code below is related to model expansion */
	/* and is ultimately called by do_modelmatrix. */

static void firstfactor(double *x, int nrx, int ncx,
			double *c, int nrc, int ncc, int *v)
{
    double *cj, *xj;
    int i, j;

    for (j = 0; j < ncc; j++) {
	xj = &x[j*nrx];
	cj = &c[j*nrc];
	for (i = 0; i < nrx; i++)
	    if(v[i] == NA_INTEGER) xj[i] = NA_REAL;
	    else xj[i] = cj[v[i]-1];
    }
}

static void addfactor(double *x, int nrx, int ncx,
		      double *c, int nrc, int ncc, int *v)
{
    int i, j, k;
    double *ck, *xj, *yj;

    for (k = ncc - 1; k >= 0; k--) {
	for (j = 0; j < ncx; j++) {
	    xj = &x[j*nrx];
	    yj = &x[(k*ncx+j)*nrx];
	    ck = &c[k*nrc];
	    for (i = 0; i < nrx; i++)
	    if(v[i] == NA_INTEGER) yj[i] = NA_REAL;
	    else yj[i] = ck[v[i]-1] * xj[i];
	}
    }
}

static void firstvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
    double *cj, *xj;
    int i, j;

    for (j = 0; j < ncc; j++) {
	xj = &x[j*nrx];
	cj = &c[j*nrc];
	for (i = 0; i < nrx; i++)
	    xj[i] = cj[i];
    }
}

static void addvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
    int i, j, k;
    double *ck, *xj, *yj;

    for (k = ncc - 1; k >= 0; k--) {
	for (j = 0; j < ncx; j++) {
	    xj = &x[j*nrx];
	    yj = &x[(k*ncx+j)*nrx];
	    ck = &c[k*nrc];
	    for (i = 0; i < nrx; i++)
		yj[i] = ck[i] * xj[i];
	}
    }
}

#define BUFSIZE 4096

static char *AppendString(char *buf, const char *str)
{
    while (*str)
	*buf++ = *str++;
    *buf = '\0';
    return buf;
}

static char *AppendInteger(char *buf, int i)
{
    sprintf(buf, "%d", i);
    while(*buf) buf++;
    return buf;
}

static SEXP ColumnNames(SEXP x)
{
    SEXP dn = getAttrib(x, R_DimNamesSymbol);
    if (dn == R_NilValue)
	return R_NilValue;
    else
	return VECTOR_ELT(dn, 1);
}

static SEXP do_modelmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, factors, terms, vars, vnames, assign;
    SEXP xnames, tnames, rnames;
    SEXP count, contrast, contr1, contr2, nlevs, ordered, columns, x;
    SEXP variable, var_i;
    int fik, first, i, j, k, kk, ll, n, nc, nterms, nVar;
    int intrcept, jstart, jnext, risponse, indx, rhs_response;
    double dk, dnc;
    char buf[BUFSIZE]="\0";
    char *bufp;
    const char *addp;

    checkArity(op, args);

    /* Get the "terms" structure and extract */
    /* the intercept and response attributes. */

    terms = CAR(args);

    intrcept = asLogical(getAttrib(terms, install("intercept")));
    if (intrcept == NA_INTEGER)
	intrcept = 0;

    risponse = asLogical(getAttrib(terms, install("response")));
    if (risponse == NA_INTEGER)
	risponse = 0;

    /* Get the factor pattern matrix.  We duplicate this because */
    /* we may want to alter it if we are in the no-intercept case. */
    /* Note: the values of "nVar" and "nterms" are the REAL number of */
    /* variables in the model data frame and the number of model terms. */

    PROTECT(factors = duplicate(getAttrib(terms, install("factors"))));
    if (length(factors) == 0) {
	/* if (intrcept == 0)
	   error("invalid model (zero parameters).");*/
	nVar = 1;
	nterms = 0;
    }
    else if (isInteger(factors) && isMatrix(factors)) {
	nVar = nrows(factors);
	nterms = ncols(factors);
    }
    else error(_("invalid '%s' argument"), "terms");

    /* Get the variable names from the factor matrix */

    vnames = getAttrib(factors, R_DimNamesSymbol);
    if (length(factors) > 0) {
	if (length(vnames) < 1 ||
	    (nVar - intrcept > 0 && !isString(VECTOR_ELT(vnames, 0))))
	    error(_("invalid '%s' argument"), "terms");
	vnames = VECTOR_ELT(vnames, 0);
    }

    /* Get the variables from the model frame.  First perform */
    /* elementary sanity checks.  Notes:  1) We need at least */
    /* one variable (lhs or rhs) to compute the number of cases. */
    /* 2) We don't type-check the response. */

    vars = CADR(args);
    if (!isNewList(vars) || length(vars) < nVar)
	error(_("invalid model frame"));
    if (length(vars) == 0)
	error(_("do not know how many cases"));
    n = nrows(VECTOR_ELT(vars, 0));
    /* This could be generated, so need to protect it */
    PROTECT(rnames = getAttrib(vars, R_RowNamesSymbol));

    /* This section of the code checks the types of the variables
       in the model frame.  Note that it should really only check
       the variables if they appear in a term in the model.
       Because it does not, we need to allow other types here, as they
       might well occur on the LHS.
       The R code converts all character variables in the model frame to
       factors, so the only types that ought to be here are logical,
       integer (including factor), numeric and complex.
     */

    PROTECT(variable = allocVector(VECSXP, nVar));
    PROTECT(nlevs = allocVector(INTSXP, nVar));
    PROTECT(ordered = allocVector(LGLSXP, nVar));
    PROTECT(columns = allocVector(INTSXP, nVar));

    for (i = 0; i < nVar; i++) {
	var_i = SET_VECTOR_ELT(variable, i, VECTOR_ELT(vars, i));
	if (nrows(var_i) != n)
	    error(_("variable lengths differ (found for variable %d)"), i);
	if (isOrdered_int(var_i)) {
	    LOGICAL(ordered)[i] = 1;
	    if((INTEGER(nlevs)[i] = nlevels(var_i)) < 1)
		error(_("variable %d has no levels"), i+1);
	    /* will get updated later when contrasts are set */
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isUnordered_int(var_i)) {
	    LOGICAL(ordered)[i] = 0;
	    if((INTEGER(nlevs)[i] = nlevels(var_i)) < 1)
		error(_("variable %d has no levels"), i+1);
	    /* will get updated later when contrasts are set */
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isLogical(var_i)) {
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 2;
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isNumeric(var_i)) {
	    SET_VECTOR_ELT(variable, i, coerceVector(var_i, REALSXP));
	    var_i = VECTOR_ELT(variable, i);
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 0;
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else {
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 0;
	    INTEGER(columns)[i] = ncols(var_i);
	}
/*	else
	    error(_("invalid variable type for '%s'"),
	    CHAR(STRING_ELT(vnames, i))); */
    }

    /* If there is no intercept we look through the factor pattern */
    /* matrix and adjust the code for the first factor found so that */
    /* it will be coded by dummy variables rather than contrasts. */

    if (!intrcept) {
	for (j = 0; j < nterms; j++) {
	    for (i = risponse; i < nVar; i++) {
		if (INTEGER(nlevs)[i] > 1
		    && INTEGER(factors)[i + j * nVar] > 0) {
		    INTEGER(factors)[i + j * nVar] = 2;
		    goto alldone;
		}
	    }
	}
    }
 alldone:
    ;

    /* Compute the required contrast or dummy variable matrices. */
    /* We set up a symbolic expression to evaluate these, substituting */
    /* the required arguments at call time.  The calls have the following */
    /* form: (contrast.type nlevs contrasts) */

    PROTECT(contr1 = allocVector(VECSXP, nVar));
    PROTECT(contr2 = allocVector(VECSXP, nVar));

    PROTECT(expr = allocList(3));
    SET_TYPEOF(expr, LANGSXP);
    SETCAR(expr, install("contrasts"));
    SETCADDR(expr, allocVector1LGL());

    /* FIXME: We need to allow a third argument to this function */
    /* which allows us to specify contrasts directly.  That argument */
    /* would be used here in exactly the same way as the below. */
    /* I.e. we would search the list of constrast specs before */
    /* we try the evaluation below. */

    for (i = 0; i < nVar; i++) {
	if (INTEGER(nlevs)[i]) {
	    k = 0;
	    for (j = 0; j < nterms; j++) {
		if (INTEGER(factors)[i + j * nVar] == 1)
		    k |= 1;
		else if (INTEGER(factors)[i + j * nVar] == 2)
		    k |= 2;
	    }
	    SETCADR(expr, VECTOR_ELT(variable, i));
	    if (k & 1) {
		LOGICAL(CADDR(expr))[0] = 1;
		SET_VECTOR_ELT(contr1, i, eval(expr, rho));
	    }
	    if (k & 2) {
		LOGICAL(CADDR(expr))[0] = 0;
		SET_VECTOR_ELT(contr2, i, eval(expr, rho));
	    }
	}
    }

    /* By convention, an rhs term identical to the response generates nothing
       in the model matrix (but interactions involving the response do). */

    rhs_response = -1;
    if (risponse > 0) /* there is a response specified */
	for (j = 0; j < nterms; j++)
	    if (INTEGER(factors)[risponse - 1 + j * nVar]) {
		for (i = 0, k = 0; i < nVar; i++)
		    k += INTEGER(factors)[i + j * nVar] > 0;
		if (k == 1) {
		    rhs_response = j;
		    break;
		}
	    }


    /* We now have everything needed to build the design matrix. */
    /* The first step is to compute the matrix size and to allocate it. */
    /* Note that "count" holds a count of how many columns there are */
    /* for each term in the model and "nc" gives the total column count. */

    PROTECT(count = allocVector(INTSXP, nterms));
    if (intrcept)
	dnc = 1;
    else
	dnc = 0;
    for (j = 0; j < nterms; j++) {
	if (j == rhs_response) {
	    warning(_("the response appeared on the right-hand side and was dropped"));
	    INTEGER(count)[j] = 0;  /* need this initialised */
	    continue;
	}
	dk = 1;	/* accumulate in a double to detect overflow */
	for (i = 0; i < nVar; i++) {
	    if (INTEGER(factors)[i + j * nVar]) {
		if (INTEGER(nlevs)[i]) {
		    switch(INTEGER(factors)[i + j * nVar]) {
		    case 1:
			dk *= ncols(VECTOR_ELT(contr1, i));
			break;
		    case 2:
			dk *= ncols(VECTOR_ELT(contr2, i));
			break;
		    }
		}
		else dk *= INTEGER(columns)[i];
	    }
	}
	if (dk > INT_MAX) error(_("term %d would require %.0g columns"), j+1, dk);
	INTEGER(count)[j] = dk;
	dnc = dnc + dk;
    }
    if (dnc > INT_MAX) error(_("matrix would require %.0g columns"), dnc);
    nc = dnc;

    /* Record which columns of the design matrix are associated */
    /* with which model terms. */

    PROTECT(assign = allocVector(INTSXP, nc));
    k = 0;
    if (intrcept) INTEGER(assign)[k++] = 0;
    for (j = 0; j < nterms; j++) {
	if(INTEGER(count)[j] <= 0)
	    warning(_("problem with term %d in model.matrix: no columns are assigned"),
		      j+1);
	for (i = 0; i < INTEGER(count)[j]; i++)
	    INTEGER(assign)[k++] = j+1;
    }


    /* Create column labels for the matrix columns. */

    PROTECT(xnames = allocVector(STRSXP, nc));


    /* Here we loop over the terms in the model and, within each */
    /* term, loop over the corresponding columns of the design */
    /* matrix, assembling the names. */

    /* FIXME : The body within these two loops should be embedded */
    /* in its own function. */

    k = 0;
    if (intrcept)
	SET_STRING_ELT(xnames, k++, mkChar("(Intercept)"));

    for (j = 0; j < nterms; j++) {
	if (j == rhs_response) continue;
	for (kk = 0; kk < INTEGER(count)[j]; kk++) {
	    first = 1;
	    indx = kk;
	    bufp = &buf[0];
	    for (i = 0; i < nVar; i++) {
		ll = INTEGER(factors)[i + j * nVar];
		if (ll) {
		    var_i = VECTOR_ELT(variable, i);
		    if (!first)
			bufp = AppendString(bufp, ":");
		    first = 0;
		    if (isFactor(var_i) || isLogical(var_i)) {
			if (ll == 1) {
			    x = ColumnNames(VECTOR_ELT(contr1, i));
			    ll = ncols(VECTOR_ELT(contr1, i));
			}
			else {
			    x = ColumnNames(VECTOR_ELT(contr2, i));
			    ll = ncols(VECTOR_ELT(contr2, i));
			}
			addp = translateChar(STRING_ELT(vnames, i));
			if(strlen(buf) + strlen(addp) < BUFSIZE)
			    bufp = AppendString(bufp, addp);
			else
			    warning(_("term names will be truncated"));
			if (x == R_NilValue) {
			    if(strlen(buf) + 10 < BUFSIZE)
				bufp = AppendInteger(bufp, indx % ll + 1);
			    else
				warning(_("term names will be truncated"));
			} else {
			    addp = translateChar(STRING_ELT(x, indx % ll));
			    if(strlen(buf) + strlen(addp) < BUFSIZE)
				bufp = AppendString(bufp, addp);
			    else
				warning(_("term names will be truncated"));
			}
		    } else if (isComplex(var_i)) {
			error(_("complex variables are not currently allowed in model matrices"));
		    } else if(isNumeric(var_i)) { /* numeric */
			x = ColumnNames(var_i);
			ll = ncols(var_i);
			addp = translateChar(STRING_ELT(vnames, i));
			if(strlen(buf) + strlen(addp) < BUFSIZE)
			    bufp = AppendString(bufp, addp);
			else
			    warning(_("term names will be truncated"));
			if (ll > 1) {
			    if (x == R_NilValue) {
				if(strlen(buf) + 10 < BUFSIZE)
				    bufp = AppendInteger(bufp, indx % ll + 1);
				else
				    warning(_("term names will be truncated"));
			    } else {
				addp = translateChar(STRING_ELT(x, indx % ll));
				if(strlen(buf) + strlen(addp) < BUFSIZE)
				    bufp = AppendString(bufp, addp);
				else
				    warning(_("term names will be truncated"));
			    }
			}
		    } else
			error(_("variables of type '%s' are not allowed in model matrices"),
			      type2char(TYPEOF(var_i)));
		    indx /= ll;
		}
	    }
	    SET_STRING_ELT(xnames, k++, mkChar(buf));
	}
    }

    /* Allocate and compute the design matrix. */

    PROTECT(x = allocMatrix(REALSXP, n, nc));

    /* a) Begin with a column of 1s for the intercept. */

    if ((jnext = jstart = intrcept) != 0) {
	for (i = 0; i < n; i++) {
	    REAL(x)[i] = 1.0;
	}
    }

    /* b) Now loop over the model terms */

    contrast = R_NilValue;	/* -Wall */
    for (k = 0; k < nterms; k++) {
	if (k == rhs_response) continue;
	for (i = 0; i < nVar; i++) {
	    if (INTEGER(columns)[i] == 0)
		continue;
	    var_i = VECTOR_ELT(variable, i);
	    fik = INTEGER(factors)[i + k * nVar];
	    if (fik) {
		switch(fik) {
		case 1:
		    contrast = coerceVector(VECTOR_ELT(contr1, i), REALSXP);
		    break;
		case 2:
		    contrast = coerceVector(VECTOR_ELT(contr2, i), REALSXP);
		    break;
		}
		PROTECT(contrast);
		if (jnext == jstart) {
		    if (INTEGER(nlevs)[i] > 0) {
			int adj = isLogical(var_i)?1:0;
			firstfactor(&REAL(x)[jstart * n], n, jnext - jstart,
				    REAL(contrast), nrows(contrast),
				    ncols(contrast), INTEGER(var_i)+adj);
			jnext = jnext + ncols(contrast);
		    }
		    else {
			firstvar(&REAL(x)[jstart * n], n, jnext - jstart,
				 REAL(var_i), n, ncols(var_i));
			jnext = jnext + ncols(var_i);
		    }
		}
		else {
		    if (INTEGER(nlevs)[i] > 0) {
			int adj = isLogical(var_i)?1:0;
			addfactor(&REAL(x)[jstart * n], n, jnext - jstart,
				  REAL(contrast), nrows(contrast),
				  ncols(contrast), INTEGER(var_i)+adj);
			jnext = jnext + (jnext - jstart)*(ncols(contrast) - 1);
		    }
		    else {
			addvar(&REAL(x)[jstart * n], n, jnext - jstart,
			       REAL(var_i), n, ncols(var_i));
			jnext = jnext + (jnext - jstart) * (ncols(var_i) - 1);
		    }
		}
		UNPROTECT(1);
	    }
	}
	jstart = jnext;
    }
    PROTECT(tnames = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(tnames, 0, rnames);
    SET_VECTOR_ELT(tnames, 1, xnames);
    setAttrib(x, R_DimNamesSymbol, tnames);
    setAttrib(x, install("assign"), assign);
    UNPROTECT(14);
    return x;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_model[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"terms.formula",do_termsform,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"update.formula",do_updateform,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"model.frame",	do_modelframe,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"~",		do_tilde,	0,	1000,	2,	{PP_BINARY,  PREC_TILDE,  0}},
{"model.matrix",do_modelmatrix,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
