/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011  The R Core Team.
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

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#define NEED_SGGC_FUNCTIONS
#include <Defn.h>
#include <Print.h>
#include <R_ext/Rdynload.h>


/* Enable the redefinition of "error" below to abort on invalid API arguments */

#if 0
#undef error
#define error abort_on_error
static R_NORETURN void abort_on_error(char *s,...)
{ abort();
}
#endif

#undef NOT_LVALUE          /* Allow CAR, etc. on left of assignment here, */
#define NOT_LVALUE(x) (x)  /* since it's needed to implement SETCAR, etc. */


#define CHECK_OLD_TO_NEW(x,y) \
    sggc_old_to_new_check(CPTR_FROM_SEXP(x),CPTR_FROM_SEXP(y))


#ifdef TESTING_WRITE_BARRIER
#ifndef PROTECTCHECK
# define PROTECTCHECK
#endif
#endif

#ifdef PROTECTCHECK

/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and SEXP
       outputs with Rf_chk_valid_SEXP() to see if a reachable node is a
       FREESXP and signal an error if a FREESXP is found.

   Combined with GC torture this can help locate where an unprotected
   SEXP is being used.

   This approach will miss cases where an unprotected node has been
   re-allocated.  For these cases it is possible to set
   gc_inhibit_release to TRUE.  FREESXP nodes will not be reallocated,
   or large ones released, until gc_inhibit_release is set to FALSE
   again.  This will of course result in memory growth and should be
   used with care and typically in combination with OS mechanisms to
   limit process memory usage.  LT */

/* Before a node is marked as a FREESXP by the collector the previous
   type is recorded.  For now using the LEVELS field seems
   reasonable.  */

#define OLDTYPE(s) LEVELS(s)
#define SETOLDTYPE(s, t) SETLEVELS(s, t)

/* slight modification of typename() from install.c -- should probably merge */
static const char *sexptype2char(SEXPTYPE type) {
    switch (type) {
    case NILSXP:	return "NILSXP";
    case SYMSXP:	return "SYMSXP";
    case LISTSXP:	return "LISTSXP";
    case CLOSXP:	return "CLOSXP";
    case ENVSXP:	return "ENVSXP";
    case PROMSXP:	return "PROMSXP";
    case LANGSXP:	return "LANGSXP";
    case SPECIALSXP:	return "SPECIALSXP";
    case BUILTINSXP:	return "BUILTINSXP";
    case CHARSXP:	return "CHARSXP";
    case LGLSXP:	return "LGLSXP";
    case INTSXP:	return "INTSXP";
    case REALSXP:	return "REALSXP";
    case CPLXSXP:	return "CPLXSXP";
    case STRSXP:	return "STRSXP";
    case DOTSXP:	return "DOTSXP";
    case ANYSXP:	return "ANYSXP";
    case VECSXP:	return "VECSXP";
    case EXPRSXP:	return "EXPRSXP";
    case BCODESXP:	return "BCODESXP";
    case EXTPTRSXP:	return "EXTPTRSXP";
    case WEAKREFSXP:	return "WEAKREFSXP";
    case S4SXP:		return "S4SXP";
    case RAWSXP:	return "RAWSXP";
    case NEWSXP:	return "NEWSXP"; /* should never happen */
    case FREESXP:	return "FREESXP";
    default:	 	return "<unknown>";
    }
}

SEXP Rf_chk_valid_SEXP(SEXP x)
{
    if (x != R_NoObject && TYPEOF(x) == FREESXP) abort();
    return x;
}

#endif


void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(Rf_chk_valid_SEXP(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return Rf_chk_valid_SEXP(EXTPTR_TAG(Rf_chk_valid_SEXP(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return Rf_chk_valid_SEXP(EXTPTR_PROT(Rf_chk_valid_SEXP(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    EXTPTR_PTR(s) = NULL;
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    EXTPTR_PTR(s) = p;
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    CHECK_OLD_TO_NEW(s, tag);
    EXTPTR_TAG(s) = tag;
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    CHECK_OLD_TO_NEW(s, p);
    EXTPTR_PROT(s) = p;
}

/* Work around casting issues: works where it is needed */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

attribute_hidden
DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    fn_ptr tmp;
    tmp.p =  EXTPTR_PTR(Rf_chk_valid_SEXP(s));
    return tmp.fn;
}


/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x) { return Rf_chk_valid_SEXP(ATTRIB(Rf_chk_valid_SEXP(x))); }
int (OBJECT)(SEXP x) { return OBJECT(Rf_chk_valid_SEXP(x)); }
int (TYPEOF)(SEXP x) { return TYPEOF(Rf_chk_valid_SEXP(x)); }
int (NAMED)(SEXP x) { return NAMED(Rf_chk_valid_SEXP(x)); }
int (RTRACE)(SEXP x) { return RTRACE(Rf_chk_valid_SEXP(x)); }
int (LEVELS)(SEXP x) { return LEVELS(Rf_chk_valid_SEXP(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if (ATTRIB_W(x) != v) {
        if (v == R_NoObject || TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
            error(
             "value for 'SET_ATTRIB' must be pairlist or R_NilValue, not '%s'",
              v == R_NoObject ? "R_NoObject" : type2char(TYPEOF(x)));
        if (TYPEOF(x) == NILSXP || TYPEOF(x) == CHARSXP)
            abort();
        if (TYPEOF(x) == SYMSXP) 
            return;  /* silently ignore attempt to set attribute on symbol */
        if (v == R_NilValue) {
            ATTRIB_W(x) = R_NilValue;
            HAS_ATTRIB(x) = 0;
        }
        else {
            CHECK_OLD_TO_NEW(x, v);
            ATTRIB_W(x) = v;
            HAS_ATTRIB(x) = 1;
        }
    }
}

void SET_ATTRIB_TO_ANYTHING(SEXP x, SEXP v) {
    if (ATTRIB_W(x) != v) {
        if (v == R_NoObject) abort();
        if (TYPEOF(x) == NILSXP || TYPEOF(x) == CHARSXP)
            abort();
        if (TYPEOF(x) == SYMSXP) 
            return;  /* silently ignore attempt to set attribute on symbol */
        if (v == R_NilValue) {
            ATTRIB_W(x) = R_NilValue;
            HAS_ATTRIB(x) = 0;
        }
        else {
            CHECK_OLD_TO_NEW(x, v);
            ATTRIB_W(x) = v;
            HAS_ATTRIB(x) = 1;
        }
    }
}

void (SET_OBJECT)(SEXP x, int v) { SET_OBJECT(Rf_chk_valid_SEXP(x), v); }
void (SET_TYPEOF)(SEXP x, int v) { SET_TYPEOF(Rf_chk_valid_SEXP(x), v); }
void (SET_NAMED)(SEXP x, int v) { SET_NAMED(Rf_chk_valid_SEXP(x), v); }
void (SET_RTRACE)(SEXP x, int v) { SET_RTRACE(Rf_chk_valid_SEXP(x), v); }
int (SETLEVELS)(SEXP x, int v) { return SETLEVELS(Rf_chk_valid_SEXP(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB (Rf_chk_valid_SEXP(to), 
                duplicate(Rf_chk_valid_SEXP(ATTRIB(Rf_chk_valid_SEXP(from)))));
    SET_OBJECT(Rf_chk_valid_SEXP(to), OBJECT(from));
    if (IS_S4_OBJECT(from)) SET_S4_OBJECT(to); else UNSET_S4_OBJECT(to);
}

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(Rf_chk_valid_SEXP(x)); }
void (SET_S4_OBJECT)(SEXP x){ SET_S4_OBJECT(Rf_chk_valid_SEXP(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ UNSET_S4_OBJECT(Rf_chk_valid_SEXP(x)); }

/* Vector Accessors */
int (LENGTH)(SEXP x) { return LENGTH(Rf_chk_valid_SEXP(x)); }

void (SETLENGTH)(SEXP x, int v) 
{ 
    if (!isVector(Rf_chk_valid_SEXP(x)))
        abort();
    if (v < 0)
        abort();
    if (Rf_nchunks(TYPEOF(x),v) > sggc_nchunks_allocated(CPTR_FROM_SEXP(x)))
        abort();

    LENGTH(x) = v;
}

int (TRUELENGTH)(SEXP x) { return TRUELENGTH(Rf_chk_valid_SEXP(x)); }
void (SET_TRUELENGTH)(SEXP x, int v) { SET_TRUELENGTH(Rf_chk_valid_SEXP(x), v); }

const char *(R_CHAR)(SEXP x) {
    if(TYPEOF(x) != CHARSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "CHAR", "CHARSXP", type2char(TYPEOF(x)));
    return (const char *)CHAR(x);
}

SEXP (STRING_ELT)(SEXP x, int i) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_ELT", "character vector", type2char(TYPEOF(x)));
    return Rf_chk_valid_SEXP(STRING_ELT(x, i));
}

SEXP (VECTOR_ELT)(SEXP x, int i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
    return Rf_chk_valid_SEXP(VECTOR_ELT(x, i));
}

SEXP *(STRING_PTR)(SEXP x) { return STRING_PTR(Rf_chk_valid_SEXP(x)); }

SEXP *(VECTOR_PTR)(SEXP x) { error(_("not safe to return vector pointer")); }


/* The functions below also have macro versions, which also make use of the 
   "error" functions */

void R_NORETURN Rf_LOGICAL_error(SEXP x)
{   error("%s() can only be applied to a '%s', not a '%s'",
          "LOGICAL",  "logical", type2char(TYPEOF(x)));
}
int *(LOGICAL)(SEXP x)
{   if(TYPEOF(x) != LGLSXP) Rf_LOGICAL_error(x);
    return LOGICAL(x);
}

/* Maybe this should exclude logicals, but it is widely used */
void R_NORETURN Rf_INTEGER_error(SEXP x)
{   error("%s() can only be applied to a '%s', not a '%s'",
          "INTEGER",  "integer", type2char(TYPEOF(x)));
}
int *(INTEGER)(SEXP x)
{   if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP) Rf_INTEGER_error(x);
    return INTEGER(x);
}

void R_NORETURN Rf_RAW_error(SEXP x)
{   error("%s() can only be applied to a '%s', not a '%s'",
          "RAW",  "raw", type2char(TYPEOF(x)));
}
Rbyte *(RAW)(SEXP x)
{   if(TYPEOF(x) != RAWSXP) Rf_RAW_error(x);
    return RAW(x);
}

void R_NORETURN Rf_REAL_error(SEXP x)
{   error("%s() can only be applied to a '%s', not a '%s'",
          "REAL",  "numeric", type2char(TYPEOF(x)));
}
double *(REAL)(SEXP x)
{   if(TYPEOF(x) != REALSXP) Rf_REAL_error(x);
    return REAL(x);
}

void R_NORETURN Rf_COMPLEX_error(SEXP x)
{   error("%s() can only be applied to a '%s', not a '%s'",
          "COMPLEX",  "complex", type2char(TYPEOF(x)));
}
Rcomplex *(COMPLEX)(SEXP x)
{   if(TYPEOF(x) != CPLXSXP) Rf_COMPLEX_error(x);
    return COMPLEX(x);
}


void (SET_STRING_ELT)(SEXP x, int i, SEXP v) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_STRING_ELT", "character vector", type2char(TYPEOF(x)));
    if(TYPEOF(v) != CHARSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'",
	     type2char(TYPEOF(v)));
    CHECK_OLD_TO_NEW(x, v);
    STRING_ELT(x, i) = v;
}

/* Copy n string elements from v (starting at j) to x (starting at i). 
   Copied sequentially; x and v can be the same object, but note the
   consequences of this. */
void copy_string_elements(SEXP x, int i, SEXP v, int j, int n) 
{
    SEXP e;
    int k;

    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
         "copy_string_elements", "character vector", type2char(TYPEOF(x)));

    if (sggc_youngest_generation(CPTR_FROM_SEXP(x)) || x == v) {
        /* x can't be older than anything, or just copying within x */
        for (k = 0; k<n; k++) {
            e = STRING_ELT(v,j+k);
            STRING_ELT(x,i+k) = e;
        }
    }
    else {  
        /* need to check each time if x is older */
        for (k = 0; k<n; k++) {
            e = STRING_ELT(v,j+k);
            CHECK_OLD_TO_NEW(x, e);
            STRING_ELT(x,i+k) = e;
        }
    }
}

/* Store repeated copies of the string elements of v in x (starting at i),
   until n elements have been stored. */
void rep_string_elements(SEXP x, int i, SEXP v, int n) 
{
    R_len_t lenv = LENGTH(v);
    R_len_t k;

    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
         "rep_string_elements", "character vector", type2char(TYPEOF(x)));

    if (sggc_youngest_generation(CPTR_FROM_SEXP(x))) {
        /* x can't be older than anything */
        k = 0;
        while (n > 0) {
            STRING_ELT(x,i) = STRING_ELT(v,k);
            i += 1;
            n -= 1;
            k += 1;
            if (k >= lenv) k = 0;
        }
    }
    else {  
        /* need to check each time if x is older */
        SEXP e;
        k = 0;
        while (n > 0) {
            e = STRING_ELT(v,k);
            CHECK_OLD_TO_NEW(x, e);
            STRING_ELT(x,i) = e;
            i += 1;
            n -= 1;
            k += 1;
            if (k >= lenv) k = 0;
        }
    }
}

SEXP (SET_VECTOR_ELT)(SEXP x, int i, SEXP v) {
    if (v == R_NoObject) abort();
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_VECTOR_ELT", "list", type2char(TYPEOF(x)));
    }
    CHECK_OLD_TO_NEW(x, v);
    return VECTOR_ELT(x, i) = v;
}

/* Copy n vector elements from v (starting at j) to x (starting at i). */
void copy_vector_elements(SEXP x, int i, SEXP v, int j, int n) 
{
    SEXP e;
    int k;

    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "copy_vector_elements", "list", type2char(TYPEOF(x)));
    }

    if (sggc_youngest_generation(CPTR_FROM_SEXP(x))) {
        /* x can't be older than anything */
        for (k = 0; k<n; k++) {
            e = VECTOR_ELT(v,j+k);
            VECTOR_ELT(x,i+k) = e;
        }
    }
    else {  
        /* need to check each time if x is older */
        for (k = 0; k<n; k++) {
            e = VECTOR_ELT(v,j+k);
            CHECK_OLD_TO_NEW(x, e);
            VECTOR_ELT(x,i+k) = e;
        }
    }
}


/* List Accessors */
SEXP (TAG)(SEXP e) { return Rf_chk_valid_SEXP(TAG(Rf_chk_valid_SEXP(e))); }
SEXP (CAR)(SEXP e) { return Rf_chk_valid_SEXP(CAR(Rf_chk_valid_SEXP(e))); }
SEXP (CDR)(SEXP e) { return Rf_chk_valid_SEXP(CDR(Rf_chk_valid_SEXP(e))); }
SEXP (CAAR)(SEXP e) { return Rf_chk_valid_SEXP(CAAR(Rf_chk_valid_SEXP(e))); }
SEXP (CDAR)(SEXP e) { return Rf_chk_valid_SEXP(CDAR(Rf_chk_valid_SEXP(e))); }
SEXP (CADR)(SEXP e) { return Rf_chk_valid_SEXP(CADR(Rf_chk_valid_SEXP(e))); }
SEXP (CDDR)(SEXP e) { return Rf_chk_valid_SEXP(CDDR(Rf_chk_valid_SEXP(e))); }
SEXP (CADDR)(SEXP e) { return Rf_chk_valid_SEXP(CADDR(Rf_chk_valid_SEXP(e))); }
SEXP (CADDDR)(SEXP e) { return Rf_chk_valid_SEXP(CADDDR(Rf_chk_valid_SEXP(e))); }
SEXP (CAD4R)(SEXP e) { return Rf_chk_valid_SEXP(CAD4R(Rf_chk_valid_SEXP(e))); }
int (MISSING)(SEXP x) { return MISSING(Rf_chk_valid_SEXP(x)); }

void (SET_TAG)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); TAG(x) = v; }

SEXP (SETCAR)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    CHECK_OLD_TO_NEW(x, y);
    CAR(x) = y;
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    CHECK_OLD_TO_NEW(x, y);
    CDR(x) = y;
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    SEXP cell;
    cell = CDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    SEXP cell;
    cell = CDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CDDDR(x) CDR(CDR(CDR(x)))

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    SEXP cell;
    cell = CDDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CD4R(x) CDR(CDR(CDR(CDR(x))))

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    if (y == R_NoObject) abort();
    SEXP cell;
    cell = CD4R(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

void (SET_MISSING)(SEXP x, int v) { SET_MISSING(Rf_chk_valid_SEXP(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return Rf_chk_valid_SEXP(FORMALS(Rf_chk_valid_SEXP(x))); }
SEXP (BODY)(SEXP x) { return Rf_chk_valid_SEXP(BODY(Rf_chk_valid_SEXP(x))); }
SEXP (CLOENV)(SEXP x) { return Rf_chk_valid_SEXP(CLOENV(Rf_chk_valid_SEXP(x))); }
int (RDEBUG)(SEXP x) { return RDEBUG(Rf_chk_valid_SEXP(x)); }
int (RSTEP)(SEXP x) { return RSTEP(Rf_chk_valid_SEXP(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FORMALS(x) = v; }
void (SET_BODY)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); BODY(x) = v; }
void (SET_CLOENV)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); CLOENV(x) = v; }
void (SET_RDEBUG)(SEXP x, int v) { SET_RDEBUG(Rf_chk_valid_SEXP(x), v); }
void (SET_RSTEP)(SEXP x, int v) { SET_RSTEP(Rf_chk_valid_SEXP(x), v); }

/* Primitive Accessors */
attribute_hidden int (PRIMOFFSET)(SEXP x) { return PRIMOFFSET(x); }
attribute_hidden void (SET_PRIMOFFSET)(SEXP x, int v) { SET_PRIMOFFSET(x, v); }

/* Symbol Accessors */

SEXP (PRINTNAME)(SEXP x) { return Rf_chk_valid_SEXP(PRINTNAME(Rf_chk_valid_SEXP(x))); }
SEXP (SYMVALUE)(SEXP x) { return Rf_chk_valid_SEXP(SYMVALUE(Rf_chk_valid_SEXP(x))); }
SEXP (INTERNAL)(SEXP x) { return Rf_chk_valid_SEXP(INTERNAL(Rf_chk_valid_SEXP(x))); }
int (DDVAL)(SEXP x) { return DDVAL(Rf_chk_valid_SEXP(x)); }

/* Don't do old-to-new check when setting fields in symbols, since they are
   always scanned anyway. */

void (SET_SYMVALUE)(SEXP x, SEXP v) 
{
    /* No old-to-new check is needed, since symbols are scanned specially. */

    SYMVALUE(x) = v;  
}
void (SET_INTERNAL)(SEXP x, SEXP v) 
{
    /* No old-to-new check is needed, since primitives are uncollected. */

    sggc_cptr_t s = CPTR_FROM_SEXP(x);
    if (TYPEOF(v)!=BUILTINSXP && TYPEOF(v)!=SPECIALSXP) abort(); 
    if (s < R_first_internal || s > R_max_internal) abort();
    R_internal_table[s-R_first_internal] = v;    
}
void (SET_DDVAL)(SEXP x, int v) { SET_DDVAL(Rf_chk_valid_SEXP(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return Rf_chk_valid_SEXP(FRAME(Rf_chk_valid_SEXP(x))); }
SEXP (ENCLOS)(SEXP x) { return Rf_chk_valid_SEXP(ENCLOS(Rf_chk_valid_SEXP(x))); }
SEXP (HASHTAB)(SEXP x) { return Rf_chk_valid_SEXP(HASHTAB(Rf_chk_valid_SEXP(x))); }
int (ENVFLAGS)(SEXP x) { return ENVFLAGS(Rf_chk_valid_SEXP(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FRAME(x) = v; }
void (SET_ENCLOS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); ENCLOS(x) = v; }
void (SET_HASHTAB)(SEXP x, SEXP v) 
{ 
    CHECK_OLD_TO_NEW(x, v);
    HASHTAB(x) = v; 
    SET_HASHLEN(x,LENGTH(v));
}
void (SET_ENVFLAGS)(SEXP x, int v) { SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return Rf_chk_valid_SEXP(PRCODE(Rf_chk_valid_SEXP(x))); }
SEXP (PRENV)(SEXP x) { return Rf_chk_valid_SEXP(PRENV(Rf_chk_valid_SEXP(x))); }
SEXP (PRVALUE)(SEXP x) { return Rf_chk_valid_SEXP(PRVALUE(Rf_chk_valid_SEXP(x))); }
int (PRSEEN)(SEXP x) { return PRSEEN(Rf_chk_valid_SEXP(x)); }

void (SET_PRENV)(SEXP x, SEXP v){ CHECK_OLD_TO_NEW(x, v); PRENV(x) = v; }
void (SET_PRVALUE)(SEXP x, SEXP v) 
  { CHECK_OLD_TO_NEW(x, v); UPTR_FROM_SEXP(x)->u.promsxp.value = v; }
void (SET_PRCODE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); PRCODE(x) = v; }
void (SET_PRSEEN)(SEXP x, int v) { SET_PRSEEN(Rf_chk_valid_SEXP(x), v); }

/* Test functions */
Rboolean Rf_isNull(SEXP s) { return isNull(s); }
Rboolean Rf_isRaw(SEXP s) { return isRaw(s); }
Rboolean Rf_isSymbol(SEXP s) { return isSymbol(s); }
Rboolean Rf_isLogical(SEXP s) { return isLogical(s); }
Rboolean Rf_isReal(SEXP s) { return isReal(s); }
Rboolean Rf_isComplex(SEXP s) { return isComplex(s); }
Rboolean Rf_isExpression(SEXP s) { return isExpression(s); }
Rboolean Rf_isEnvironment(SEXP s) { return isEnvironment(s); }
Rboolean Rf_isString(SEXP s) { return isString(s); }
Rboolean Rf_isObject(SEXP s) { return isObject(s); }

/* Bindings accessors */
Rboolean attribute_hidden
(IS_ACTIVE_BINDING)(SEXP b) {return IS_ACTIVE_BINDING(b);}
Rboolean attribute_hidden
(BINDING_IS_LOCKED)(SEXP b) {return BINDING_IS_LOCKED(b);}
void attribute_hidden
(SET_ACTIVE_BINDING_BIT)(SEXP b) {SET_ACTIVE_BINDING_BIT(b);}
void attribute_hidden (LOCK_BINDING)(SEXP b) {LOCK_BINDING(b);}
void attribute_hidden (UNLOCK_BINDING)(SEXP b) {UNLOCK_BINDING(b);}

/* R_FunTab accessors */
int (PRIMVAL)(SEXP x) { return PRIMVAL(x); }
CCODE (PRIMFUN)(SEXP x) { return PRIMFUN(x); }
void (SET_PRIMFUN)(SEXP x, CCODE f) { SET_PRIMFUN(x,f); }

/* for use when testing the write barrier */
int  attribute_hidden (IS_BYTES)(SEXP x) { return IS_BYTES(x); }
int  attribute_hidden (IS_LATIN1)(SEXP x) { return IS_LATIN1(x); }
int  attribute_hidden (IS_ASCII)(SEXP x) { return IS_ASCII(x); }
int  attribute_hidden (IS_UTF8)(SEXP x) { return IS_UTF8(x); }
void attribute_hidden (SET_BYTES)(SEXP x) { SET_BYTES(x); }
void attribute_hidden (SET_LATIN1)(SEXP x) { SET_LATIN1(x); }
void attribute_hidden (SET_UTF8)(SEXP x) { SET_UTF8(x); }
void attribute_hidden (SET_ASCII)(SEXP x) { SET_ASCII(x); }
int  attribute_hidden (ENC_KNOWN)(SEXP x) { return ENC_KNOWN(x); }


/* ------------------------------------------------------------------------
   Function (plus global variables) not intended to ever be called, and 
   normally not defined, whose code can be examined to see how the compiler
   is implementing facilities such as the NAMEDCNT macros. 
*/

#if 1

SEXP R_tobj_a, R_tobj_b, R_tobj_c, R_tobj_d, R_tobj_e, R_tobj_f, R_tobj_g,
     R_tobj_h, R_tobj_i, R_tobj_j, R_tobj_k, R_tobj_l, R_tobj_m, R_tobj_n,
     R_tobj_o, R_tobj_p, R_tobj_q, R_tobj_r, R_tobj_s, R_tobj_t, R_tobj_u,
     R_tobj_v, R_tobj_w, R_tobj_x, R_tobj_y, R_tobj_z;

int  R_tint_a, R_tint_b, R_tint_c, R_tint_d, R_tint_e, R_tint_f, R_tint_g,
     R_tint_h, R_tint_i, R_tint_j, R_tint_k, R_tint_l, R_tint_m, R_tint_n,
     R_tint_o, R_tint_p, R_tint_q, R_tint_r, R_tint_s, R_tint_t, R_tint_u,
     R_tint_v, R_tint_w, R_tint_x, R_tint_y, R_tint_z;

void Rf_code_gen_test_func1 (void)
{
  R_tint_a = NAMEDCNT(R_tobj_a);
  R_tint_b = NAMED(R_tobj_b);

  if (NAMEDCNT_EQ_0(R_tobj_c)) REprintf("xx\n");
  if (NAMEDCNT_GT_0(R_tobj_d)) REprintf("xx\n");
  if (NAMEDCNT_GT_1(R_tobj_e)) REprintf("xx\n");

  SET_NAMEDCNT(R_tobj_f,R_tint_f);
  SET_NAMEDCNT_0(R_tobj_g);
  SET_NAMEDCNT_1(R_tobj_h);
  SET_NAMEDCNT_MAX(R_tobj_i);

  SET_NAMED(R_tobj_j,R_tint_j);

  INC_NAMEDCNT(R_tobj_k);
  DEC_NAMEDCNT(R_tobj_l);

  R_tint_m = LEVELS(R_tobj_m);
  R_tint_n = TRUELENGTH(R_tobj_n);

  SETLEVELS(R_tobj_o,R_tint_o);
  SET_TRUELENGTH(R_tobj_p,R_tint_p);

  R_tint_q = TYPEOF(R_tobj_q);
}

void Rf_code_gen_test_func2 (void)
{
  R_tint_a = TYPEOF(R_tobj_a);
  R_tint_b = LENGTH(R_tobj_a);
  R_tint_c = INTEGER(R_tobj_a)[2];

  R_tobj_w = ATTRIB(R_tobj_b);
  R_tobj_x = CAR(R_tobj_b);
  R_tobj_y = CDR(R_tobj_b);
  R_tobj_z = TAG(R_tobj_b);

  if (ATTRIB(R_tobj_c) == R_NilValue) REprintf("yy1\n");
  if (HAS_ATTRIB(R_tobj_c)) REprintf("yy2\n");

  if (LENGTH(R_tobj_c) == 1) REprintf("yy3\n");
}

#endif
