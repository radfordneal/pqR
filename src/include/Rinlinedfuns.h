/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in util.c) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   See http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

/* Probably not able to use C99 semantics in gcc < 4.3.0 but who knows what
   unofficial versions Debian or RedHat will distribute */
#if __GNUC__ == 4 && __GNUC_MINOR__ >= 3 && defined(__GNUC_STDC_INLINE__) && !defined(C99_INLINE_SEMANTICS)
#define C99_INLINE_SEMANTICS 1
#endif

/* Apple's gcc build >5400 (since Xcode 3.0) doesn't support GNU inline in C99 mode */
#if __APPLE_CC__ > 5400 && !defined(C99_INLINE_SEMANTICS) && __STDC_VERSION__ >= 199901L
#define C99_INLINE_SEMANTICS 1
#endif

#ifdef COMPILING_R
/* defined only in inlined.c: this emits standalone code there */
# define INLINE_FUN
#else
/* This section is normally only used for versions of gcc which do not
   support C99 semantics.  __GNUC_STDC_INLINE__ is defined if
   GCC is following C99 inline semantics by default: we
   switch R's usage to the older GNU semantics via attributes.
   Do this even for __GNUC_GNUC_INLINE__ to shut up warnings in 4.2.x.
   __GNUC_STDC_INLINE__ and __GNUC_GNU_INLINE__ were added in gcc 4.2.0.
*/
# if defined(__GNUC_STDC_INLINE__) || defined(__GNUC_GNU_INLINE__)
#  define INLINE_FUN extern __attribute__((gnu_inline)) inline
# else
#  define INLINE_FUN extern R_INLINE
# endif
#endif /* ifdef COMPILING_R */

#if C99_INLINE_SEMANTICS
# undef INLINE_FUN
# ifdef COMPILING_R
/* force exported copy */
#  define INLINE_FUN extern inline
# else
/* either inline or link to extern version at compiler's choice */
#  define INLINE_FUN inline
# endif /* ifdef COMPILING_R */
#endif /* C99_INLINE_SEMANTICS */


#include <string.h> /* for strlen, strcmp */


/* Sets of SEXTYPES, for fast testing with if ((set >> type) & 1) ... */

#define CONS_TYPES ( \
  (1<<LISTSXP) + (1<<LANGSXP) + (1<<DOTSXP) \
)

#define PAIRLIST_TYPES ( \
  (1<<NILSXP) + (1<<LISTSXP) + (1<<LANGSXP) \
)

#define ATOMIC_VECTOR_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) + \
  (1<<RAWSXP) + (1<<STRSXP) + (1<<CPLXSXP) \
)

#define NONPOINTER_VECTOR_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) + \
  (1<<RAWSXP) + (1<<CPLXSXP) \
)

#define NONATOMIC_VECTOR_TYPES ( \
  (1<<VECSXP) + (1<<EXPRSXP) \
)

#define VECTOR_TYPES ( \
  ATOMIC_VECTOR_TYPES + NONATOMIC_VECTOR_TYPES \
)

#define VECTOR_OR_CHAR_TYPES ( \
  VECTOR_TYPES + (1<<CHARSXP) \
)

#define PRIMITIVE_FUN_TYPES ( \
  (1<<BUILTINSXP) + (1<<SPECIALSXP) \
)

#define FUNCTION_TYPES ( \
  PRIMITIVE_FUN_TYPES + (1<<CLOSXP) \
)

#define NUMERIC_TYPES ( \
  (1<<LGLSXP) + (1<<INTSXP) + (1<<REALSXP) \
)

#define NUMBER_TYPES ( \
  NUMERIC_TYPES + (1<<CPLXSXP) \
)


/* define inline-able functions */

/* from list.c */

/* Get the i-th element of a list */
INLINE_FUN SEXP elt(SEXP list, int i)
{
    int j;
    SEXP result = list;

    if ((i < 0) || (i > length(list)))
	return R_NilValue;
    else
	for (j = 0; j < i; j++)
	    result = CDR(result);

    return CAR(result);
}


/* Return the last element of a list */
INLINE_FUN SEXP lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue) {
	result = list;
	list = CDR(list);
    }
    return result;
}


/* Shorthands for creating small lists */

INLINE_FUN SEXP list1(SEXP s)
{
    return CONS(s, R_NilValue);
}


INLINE_FUN SEXP list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, CONS(t, R_NilValue));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    PROTECT(t);
    s = CONS(s, CONS(t, CONS(u, R_NilValue)));
    UNPROTECT(2);
    return s;
}


INLINE_FUN SEXP list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    PROTECT(t);
    PROTECT(u);
    s = CONS(s, CONS(t, CONS(u, CONS(v, R_NilValue))));
    UNPROTECT(3);
    return s;
}

INLINE_FUN SEXP list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    PROTECT(t);
    PROTECT(u);
    PROTECT(v);
    s = CONS(s, CONS(t, CONS(u, CONS(v, CONS(w, R_NilValue)))));
    UNPROTECT(4);
    return s;
}


/* Destructive list append : See also ``append'' */

INLINE_FUN SEXP listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
	return t;
    r = s;
    while (CDR(r) != R_NilValue)
	r = CDR(r);
    SETCDR(r, t);
    return s;
}


/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/* Return a (language) dotted pair with the given car and cdr */

INLINE_FUN SEXP lcons(SEXP car, SEXP cdr)
{
    SEXP e = cons(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

/* Define the langN functions so that the nodes are allocated in order,
   not reverse order, so that access will be sequential if node allocation
   is sequential. */

INLINE_FUN SEXP lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

INLINE_FUN SEXP lang2(SEXP s, SEXP t)
{
    PROTECT (s = LCONS(s,R_NilValue));
    SETCDR (s, CONS(t,R_NilValue));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT (s = LCONS(s,R_NilValue));
    SETCDR (s, t = CONS(t,R_NilValue));
    SETCDR (t, CONS(u,R_NilValue));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT (s = LCONS(s,R_NilValue));
    SETCDR (s, t = CONS(t,R_NilValue));
    SETCDR (t, u = CONS(u,R_NilValue));
    SETCDR (u, CONS(v,R_NilValue));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT (s = LCONS(s,R_NilValue));
    SETCDR (s, t = CONS(t,R_NilValue));
    SETCDR (t, u = CONS(u,R_NilValue));
    SETCDR (u, v = CONS(v,R_NilValue));
    SETCDR (v, CONS(w,R_NilValue));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT (s = LCONS(s,R_NilValue));
    SETCDR (s, t = CONS(t,R_NilValue));
    SETCDR (t, u = CONS(u,R_NilValue));
    SETCDR (u, v = CONS(v,R_NilValue));
    SETCDR (v, w = CONS(w,R_NilValue));
    SETCDR (w, CONS(x,R_NilValue));
    UNPROTECT(1);
    return s;
}

/* from util.c */

/* Check to see if the arrays "x" and "y" have the identical extents */

INLINE_FUN Rboolean conformable(SEXP x, SEXP y)
{
    int i, n;
    PROTECT(x = getAttrib(x, R_DimSymbol));
    y = getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = LENGTH(x)) != LENGTH(y))
	return FALSE;
    for (i = 0; i < n; i++)
	if (INTEGER(x)[i] != INTEGER(y)[i])
	    return FALSE;
    return TRUE;
}

/* NOTE: R's inherits() is based on inherits3() in ../main/objects.c
 * Here, use char / CHAR() instead of the slower more general translateChar()
 */
INLINE_FUN Rboolean inherits(SEXP s, const char *name)
{
    SEXP klass;
    int i, nclass;
    if (OBJECT(s)) {
	klass = getAttrib(s, R_ClassSymbol);
        if (klass == R_NilValue)
            return FALSE;
	nclass = LENGTH(klass);
	for (i = 0; i < nclass; i++) {
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), name))
		return TRUE;
	}
    }
    return FALSE;
}

INLINE_FUN Rboolean isValidString(SEXP x)
{
    return TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP;
}

/* non-empty ("") valid string :*/
INLINE_FUN Rboolean isValidStringF(SEXP x)
{
    return isValidString(x) && CHAR(STRING_ELT(x, 0))[0];
}

INLINE_FUN Rboolean isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP) {
	const char *str = CHAR(PRINTNAME(s));
	if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str)-1] == '%')
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isFunction(SEXP s)
{
    return (FUNCTION_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isPrimitive(SEXP s)
{
    return (PRIMITIVE_FUN_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}


INLINE_FUN Rboolean isNewList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == VECSXP);
}

INLINE_FUN Rboolean isPairList(SEXP s)
{
    return (PAIRLIST_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isVectorList(SEXP s)
{
    return (NONATOMIC_VECTOR_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isVectorAtomic(SEXP s)
{
    return (ATOMIC_VECTOR_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isVectorNonpointer(SEXP s)
{
    return (NONPOINTER_VECTOR_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isVector(SEXP s)/* === isVectorList() or isVectorAtomic() */
{
    return (VECTOR_TYPES >> TYPEOF(s)) & 1;
}

INLINE_FUN Rboolean isFrame(SEXP s)
{
    return inherits (s, "data.frame");
}

INLINE_FUN Rboolean isLanguage(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LANGSXP);
}

INLINE_FUN Rboolean isMatrix(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
	if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isArray(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
	if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isTs(SEXP s)
{
    return (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue);
}


INLINE_FUN Rboolean isInteger(SEXP s)
{
    return (TYPEOF(s) == INTSXP && !inherits(s, "factor"));
}

INLINE_FUN Rboolean isFactor(SEXP s)
{
    return (TYPEOF(s) == INTSXP  && inherits(s, "factor"));
}

INLINE_FUN int nlevels(SEXP f)
{
    return isFactor(f) ? length(getAttrib(f, R_LevelsSymbol)) : 0;
}

/* Is an object of numeric type. */
/* FIXME:  the LGLSXP case should be excluded here
 * (really? in many places we affirm they are treated like INTs)*/

INLINE_FUN Rboolean isNumeric(SEXP s)
{
    return TYPEOF(s) == INTSXP ? !inherits(s,"factor")
                               : ((NUMERIC_TYPES >> TYPEOF(s)) & 1);
}

/** Is an object "Numeric" or  complex */
INLINE_FUN Rboolean isNumber(SEXP s)
{
    return TYPEOF(s) == INTSXP ? !inherits(s,"factor")
                               : ((NUMBER_TYPES >> TYPEOF(s)) & 1);
}

/* As from R 2.4.0 we check that the value is allowed. */
INLINE_FUN SEXP ScalarLogical(int x)
{
    return x == 0 ? R_ScalarLogicalFALSE
         : x == NA_LOGICAL ? R_ScalarLogicalNA
         : R_ScalarLogicalTRUE;
}

INLINE_FUN SEXP ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, 1);
    REAL(ans)[0] = x;
    return ans;
}


INLINE_FUN SEXP ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, 1);
    COMPLEX(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, 1);
    SET_STRING_ELT(ans, 0, x);
    UNPROTECT(1);
    return ans;
}

INLINE_FUN SEXP ScalarRaw(Rbyte x)
{
    SEXP ans = allocVector(RAWSXP, 1);
    RAW(ans)[0] = x;
    return ans;
}

/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */
/* BUT it does not exclude 0! */

INLINE_FUN Rboolean isVectorizable(SEXP s)
{
    if (s == R_NilValue) return TRUE;
    else if (isNewList(s)) {
	int i, n;

	n = LENGTH(s);
	for (i = 0 ; i < n; i++)
	    if (!isVector(VECTOR_ELT(s, i)) || LENGTH(VECTOR_ELT(s, i)) > 1)
		return FALSE;
	return TRUE;
    }
    else if (isList(s)) {
	for ( ; s != R_NilValue; s = CDR(s))
	    if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1) return FALSE;
	return TRUE;
    }
    else return FALSE;
}


/**
 * Create a named vector of type TYP
 *
 * @example const char *nms[] = {"xi", "yi", "zi", ""};
 *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
 *
 * @param TYP a vector SEXP type (e.g. REALSXP)
 * @param names names of list elements with null string appended
 *
 * @return (pointer to a) named vector of type TYP
 */
INLINE_FUN SEXP mkNamed(SEXPTYPE TYP, const char **names)
{
    SEXP ans, nms;
    int i, n;

    for (n = 0; strlen(names[n]) > 0; n++) {}
    ans = PROTECT(allocVector(TYP, n));
    nms = PROTECT(allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
	SET_STRING_ELT(nms, i, mkChar(names[i]));
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}


/* from gram.y */

/* short cut for  ScalarString(mkChar(s)) : */
INLINE_FUN SEXP mkString(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkChar(s));
    UNPROTECT(1);
    return t;
}

#endif /* R_INLINES_H_ */
