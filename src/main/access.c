/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016 by Radford M. Neal
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
#include <Defn.h>
#include <Print.h>
#include <R_ext/Rdynload.h>


#undef NOT_LVALUE          /* Allow CAR, etc. on left of assignment here, */
#define NOT_LVALUE(x) (x)  /* since it's needed to implement SETCAR, etc. */


#define CHECK_OLD_TO_NEW(x,y) sggc_old_to_new_check(CPTR(x),CPTR(y))


#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
#endif

#ifdef PROTECTCHECK

/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and
       SEXP outputs with CHK() to see if a reachable node is a
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

static R_INLINE SEXP CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x != NULL && TYPEOF(x) == FREESXP)
	error("unprotected object (%p) encountered (was %s)",
	      x, sexptype2char(OLDTYPE(x)));
    return x;
}

#else /* PROTECTCHECK not defined */

#define CHK(x) (x)

#endif


void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return CHK(EXTPTR_PROT(CHK(s)));
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
    tmp.p =  EXTPTR_PTR(CHK(s));
    return tmp.fn;
}


/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x) { return CHK(ATTRIB(CHK(x))); }
int (OBJECT)(SEXP x) { return OBJECT(CHK(x)); }
int (TYPEOF)(SEXP x) { return TYPEOF(CHK(x)); }
int (NAMED)(SEXP x) { return NAMED(CHK(x)); }
int (RTRACE)(SEXP x) { return RTRACE(CHK(x)); }
int (LEVELS)(SEXP x) { return LEVELS(CHK(x)); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if (v == NULL || TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'",
	      v == NULL ? "C null pointer" : type2char(TYPEOF(x)));
    if (TYPEOF(x) == NILSXP || TYPEOF(x) == CHARSXP) abort();
    if (ATTRIB(x) != v) {
        CHECK_OLD_TO_NEW(x, v);
        ATTRIB(x) = v;
    }
}

void SET_ATTRIB_TO_ANYTHING(SEXP x, SEXP v) {
    if (v == NULL)
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'",
	      "C null pointer");
    if (TYPEOF(x) == NILSXP || TYPEOF(x) == CHARSXP) abort();
    if (ATTRIB(x) != v) {
        CHECK_OLD_TO_NEW(x, v);
        ATTRIB(x) = v;
    }
}

void (SET_OBJECT)(SEXP x, int v) { SET_OBJECT(CHK(x), v); }
void (SET_TYPEOF)(SEXP x, int v) { SET_TYPEOF(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v) { SET_NAMED(CHK(x), v); }
void (SET_RTRACE)(SEXP x, int v) { SET_RTRACE(CHK(x), v); }
int (SETLEVELS)(SEXP x, int v) { return SETLEVELS(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    if (IS_S4_OBJECT(from)) SET_S4_OBJECT(to); else UNSET_S4_OBJECT(to);
}

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ SET_S4_OBJECT(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ UNSET_S4_OBJECT(CHK(x)); }

/* Vector Accessors */
int (LENGTH)(SEXP x) { return LENGTH(CHK(x)); }
int (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK(x)); }
void (SETLENGTH)(SEXP x, int v) { SETLENGTH(CHK(x), v); }
void (SET_TRUELENGTH)(SEXP x, int v) { SET_TRUELENGTH(CHK(x), v); }

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
    return CHK(STRING_ELT(x, i));
}

SEXP (VECTOR_ELT)(SEXP x, int i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
    return CHK(VECTOR_ELT(x, i));
}

SEXP *(STRING_PTR)(SEXP x) { return STRING_PTR(CHK(x)); }

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

/* Copy n string elements from v (starting at j) to x (starting at i). */
void copy_string_elements(SEXP x, int i, SEXP v, int j, int n) 
{
    SEXP e;
    int k;

    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
         "copy_string_elements", "character vector", type2char(TYPEOF(x)));

    if (sggc_youngest_generation(CPTR(x))) {
        /* x can't be older than anything */
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

SEXP (SET_VECTOR_ELT)(SEXP x, int i, SEXP v) {
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

    if (sggc_youngest_generation(CPTR(x))) {
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
SEXP (TAG)(SEXP e) { return CHK(TAG(CHK(e))); }
SEXP (CAR)(SEXP e) { return CHK(CAR(CHK(e))); }
SEXP (CDR)(SEXP e) { return CHK(CDR(CHK(e))); }
SEXP (CAAR)(SEXP e) { return CHK(CAAR(CHK(e))); }
SEXP (CDAR)(SEXP e) { return CHK(CDAR(CHK(e))); }
SEXP (CADR)(SEXP e) { return CHK(CADR(CHK(e))); }
SEXP (CDDR)(SEXP e) { return CHK(CDDR(CHK(e))); }
SEXP (CADDR)(SEXP e) { return CHK(CADDR(CHK(e))); }
SEXP (CADDDR)(SEXP e) { return CHK(CADDDR(CHK(e))); }
SEXP (CAD4R)(SEXP e) { return CHK(CAD4R(CHK(e))); }
int (MISSING)(SEXP x) { return MISSING(CHK(x)); }

void (SET_TAG)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); TAG(x) = v; }

SEXP (SETCAR)(SEXP x, SEXP y)
{
    CHECK_OLD_TO_NEW(x, y);
    CAR(x) = y;
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    CHECK_OLD_TO_NEW(x, y);
    CDR(x) = y;
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CDDDR(x) CDR(CDR(CDR(x)))

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CDDDR(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

#define CD4R(x) CDR(CDR(CDR(CDR(x))))

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    SEXP cell;
    cell = CD4R(x);
    CHECK_OLD_TO_NEW(cell, y);
    CAR(cell) = y;
    return y;
}

void (SET_MISSING)(SEXP x, int v) { SET_MISSING(CHK(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return CHK(FORMALS(CHK(x))); }
SEXP (BODY)(SEXP x) { return CHK(BODY(CHK(x))); }
SEXP (CLOENV)(SEXP x) { return CHK(CLOENV(CHK(x))); }
int (RDEBUG)(SEXP x) { return RDEBUG(CHK(x)); }
int (RSTEP)(SEXP x) { return RSTEP(CHK(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FORMALS(x) = v; }
void (SET_BODY)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); BODY(x) = v; }
void (SET_CLOENV)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); CLOENV(x) = v; }
void (SET_RDEBUG)(SEXP x, int v) { SET_RDEBUG(CHK(x), v); }
void (SET_RSTEP)(SEXP x, int v) { SET_RSTEP(CHK(x), v); }

/* Primitive Accessors */
attribute_hidden int (PRIMOFFSET)(SEXP x) { return PRIMOFFSET(x); }
attribute_hidden void (SET_PRIMOFFSET)(SEXP x, int v) { SET_PRIMOFFSET(x, v); }

/* Symbol Accessors */
SEXP (PRINTNAME)(SEXP x) { return CHK(PRINTNAME(CHK(x))); }
SEXP (SYMVALUE)(SEXP x) { return CHK(SYMVALUE(CHK(x))); }
SEXP (INTERNAL)(SEXP x) { return CHK(INTERNAL(CHK(x))); }
int (DDVAL)(SEXP x) { return DDVAL(CHK(x)); }

void (SET_PRINTNAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); PRINTNAME(x) = v; }
void (SET_SYMVALUE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); SYMVALUE(x) = v; }
void (SET_INTERNAL)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); INTERNAL(x) = v; }
void (SET_DDVAL)(SEXP x, int v) { SET_DDVAL(CHK(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return CHK(FRAME(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { return CHK(ENCLOS(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { return CHK(HASHTAB(CHK(x))); }
int (ENVFLAGS)(SEXP x) { return ENVFLAGS(CHK(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); FRAME(x) = v; }
void (SET_ENCLOS)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); ENCLOS(x) = v; }
void (SET_HASHTAB)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); HASHTAB(x) = v; }
void (SET_ENVFLAGS)(SEXP x, int v) { SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return CHK(PRCODE(CHK(x))); }
SEXP (PRENV)(SEXP x) { return CHK(PRENV(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { return CHK(PRVALUE(CHK(x))); }
int (PRSEEN)(SEXP x) { return PRSEEN(CHK(x)); }

void (SET_PRENV)(SEXP x, SEXP v){ CHECK_OLD_TO_NEW(x, v); PRENV(x) = v; }
void (SET_PRVALUE)(SEXP x, SEXP v) 
  { CHECK_OLD_TO_NEW(x, v); x->u.promsxp.value = v; }
void (SET_PRCODE)(SEXP x, SEXP v) { CHECK_OLD_TO_NEW(x, v); PRCODE(x) = v; }
void (SET_PRSEEN)(SEXP x, int v) { SET_PRSEEN(CHK(x), v); }

/* Hashing Accessors */
int (HASHASH)(SEXP x) { return HASHASH(CHK(x)); }
int (HASHVALUE)(SEXP x) { return HASHVALUE(CHK(x)); }

void (SET_HASHASH)(SEXP x, int v) { SET_HASHASH(CHK(x), v); }
void (SET_HASHVALUE)(SEXP x, int v) { SET_HASHVALUE(CHK(x), v); }

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
void attribute_hidden (SET_CACHED)(SEXP x) { SET_CACHED(x); }
int  attribute_hidden (IS_CACHED)(SEXP x) { return IS_CACHED(x); }



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

void Rf_code_gen_test_func (void)
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

#endif
