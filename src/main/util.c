/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
#include <ctype.h>		/* for isspace */

#undef COMPILING_R

#define imax2(x, y) ((x < y) ? y : x)
#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef Win32
void R_UTF8fixslash(char *s);
static void R_wfixslash(wchar_t *s);
#endif

#ifdef __cplusplus
#include "Clinkage.h"

extern "C" {
#endif
void F77_SYMBOL(rwarnc)(char *msg, int *nchar);
void F77_SYMBOL(rexitc)(char *msg, int *nchar);

#ifdef __cplusplus
}
#endif

/* Many small functions are included from ../include/Rinlinedfuns.h */


/* Find the higher of two atomic types, with respect to coercion ordering: 

                  RAW < LGL < INT < REAL < CPLX < STR

   NIL is before all of these.  Other types result in an error.
*/

SEXPTYPE Rf_higher_atomic_type (SEXPTYPE a, SEXPTYPE b)
{
    const short order[32] = { 
       0, -1, -1, -1, -1, -1, -1, -1,   -1, -1,  2, -1, -1,  3,  4,  5,
       6, -1, -1, -1, -1, -1, -1, -1,    1, -1, -1, -1, -1, -1, -1, -1
    };

    if (order[a] < 0 || order[b] < 0) abort();

    return order[a] > order[b] ? a : b;
}

/* Copy one string from "from" to "to", with 0 terminator.  Returns FALSE 
   if the length of "from" is greater than "size" minus 1, in which case the 
   copy will be incomplete, and TRUE otherwise.  "size" must be at least 1. */

Rboolean copy_1_string(char *to, int size, const char *from)
{
    while (*to = *from) {
        from += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    return TRUE;
}

/* Copy concatenation of strings from "from1" and "from2" to "to", with 0 
   terminator.  Returns FALSE if the resulting length is greater than "size" 
   minus 1, in which case the copy will be incomplete, and TRUE otherwise.  
   "size" must be at least 1. */

Rboolean copy_2_strings(char *to, int size, const char *from1, const char *from2)
{
    while (*to = *from1) {
        from1 += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    while (*to = *from2) {
        from2 += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    return TRUE;
}

/* Copy concatenation of strings from "from1", "from2", and "from3" to "to", 
   with 0 terminator.  Returns FALSE if the resulting length is greater than 
   "size" minus 1, in which case the copy will be incomplete, and TRUE 
   otherwise.  "size" must be at least 1. */

Rboolean copy_3_strings(char *to, int size, const char *from1, 
                        const char *from2, const char *from3)
{
    while (*to = *from1) {
        from1 += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    while (*to = *from2) {
        from2 += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    while (*to = *from3) {
        from3 += 1;
        to += 1;
        size -= 1;
        if (size<=0) return FALSE;
    }

    return TRUE;
}

/* Put the ASCII representation of a double in a string, with 0 termination.
   There must be space for at least 32 characters in the string (no extra
   needed for 0 termination).  Uses the global variable OutDec for the 
   decimal point. */

void double_to_string (char *s, double x)
{
    int w, d, e;
    const char *p;

    /* Handle small integers quickly.  The size limit derives from the 
       requirement that 100000 be converted to "1e+05". */

    if (x < 100000 && x > -100000 && (int)x == x) {
        integer_to_string (s, (int)x);
        return;
    }

    formatReal(&x, 1, &w, &d, &e, 0);
    p = EncodeReal (x, w, d, e, OutDec);

    /* Copy, removing trailing zeros in digits after decimal point. */

    if (OutDec=='0') OutDec = 0; /* don't crash in this ridiculous situation */
    while (*p != 0 && *p != OutDec) *s++ = *p++;
    if (*p != 0) {
        *s++ = *p++;  /* copy OutDec */
        while (*p >= '0' && *p <= '9') *s++ = *p++;
        while (*(s-1) == '0') s -= 1;
        while (*p != 0) *s++ = *p++;
    }
    *s = 0;
}

/* Put the ASCII representation of an integer in a string, with 0 termination.
   There must be space for at least 12 characters in the string.  NA is put
   in as "NA". */

#if 0  /* can select one of three implementations here */

void integer_to_string (char *s, int i)
{
    int m;

    if (i == 0)
        *s++ = '0';
    else if (i == NA_INTEGER) {
        *s++ = 'N'; 
        *s++ = 'A';
    }
    else {
        if (i < 0) {
            *s++ = '-';
            i = -i;
        }
        m = i >= 100000 ? 1000000000 : 10000;
        while (m > i) {
            m /= 10;
        }
        while (m > 1) {
            *s++ = '0' + i/m;
            i %= m;
            m /= 10;
        }
        *s++ = '0' + i;
    }

    *s = 0;
}

#elif 0

void integer_to_string (char *s, int i)
{
    static int pow10[10] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 
                             10000000, 100000000, 1000000000 };
    int m, j;

    if (i == NA_INTEGER) {
        *s++ = 'N'; 
        *s++ = 'A';
        *s++ = 0;
        return;
    }
    
    if (i < 0) {
        *s++ = '-';
        i = -i;
    }

    if (i >= 10) {

        j = 10;
        do {
            j -= 1;
            m = pow10[j];
        } while (m > i);

        for (;;) {
            char c = '0';
            int M = m << 2;
            if (i >= M) 
            { c += 4; 
              i -= M;
              if (i >= M) 
              { c += 4; 
                i -= M; 
              }
            }
            M >>= 1;
            if (i >= M) 
            { c += 2; 
              i -= M; 
            }
            if (i >= m)
            { c += 1;
              i -= m;
            }
            *s++ = c;
            j -= 1;
            if (j == 0) break;
            m = pow10[j];
        }
    }

    *s++ = '0' + i;
    *s = 0;
}

#else

/* The implementation below assumes that integer multiplication is
   much faster than integer division, and that division by a constant
   can be converted by the compiler to a multiplication. */

void integer_to_string (char *s, int i)
{
    int d;

    if (i == NA_INTEGER) {
        *s++ = 'N'; 
        *s++ = 'A';
        *s++ = 0;
        return;
    }
    
    if (i < 0) {
        *s++ = '-';
        i = -i;
    }

    if (i >= 100000) {
        if (i >= 1000000000) goto GE_1000000000;
        if (i >= 100000000) goto GE_100000000;
        if (i >= 10000000) goto GE_10000000;
        if (i >= 1000000) goto GE_1000000;
        goto GE_100000;
    }
    else {
        if (i >= 10000) goto GE_10000;
        if (i >= 1000) goto GE_1000;
        if (i >= 100) goto GE_100;
        if (i >= 10) goto GE_10;
        goto GE_0;
    }

  GE_1000000000:
    d = i / 1000000000;
    *s++ = '0' + d;
    i -= d*1000000000;
  GE_100000000:
    d = i / 100000000;
    *s++ = '0' + d;
    i -= d*100000000;
  GE_10000000:
    d = i / 10000000;
    *s++ = '0' + d;
    i -= d*10000000;
  GE_1000000:
    d = i / 1000000;
    *s++ = '0' + d;
    i -= d*1000000;
  GE_100000:
    d = i / 100000;
    *s++ = '0' + d;
    i -= d*100000;
  GE_10000:
    d = i / 10000;
    *s++ = '0' + d;
    i -= d*10000;
  GE_1000:
    d = i / 1000;
    *s++ = '0' + d;
    i -= d*1000;
  GE_100:
    d = i / 100;
    *s++ = '0' + d;
    i -= d*100;
  GE_10:
    d = i / 10;
    *s++ = '0' + d;
    i -= d*10;
  GE_0:
    *s++ = '0' + i;
    *s = 0;
}

#endif



Rboolean tsConform(SEXP x, SEXP y)
{
    if ((x = getAttrib(x, R_TspSymbol)) != R_NilValue &&
	(y = getAttrib(y, R_TspSymbol)) != R_NilValue) {
	/* tspgets should enforce this, but prior to 2.4.0
	   had INTEGER() here */
	if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP)
	    return REAL(x)[0] == REAL(x)[0] &&
		REAL(x)[1] == REAL(x)[1] &&
		REAL(x)[2] == REAL(x)[2];
	/* else fall through */
    }
    return FALSE;
}

int nrows(SEXP s)
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getDimAttrib(s);
	if (t == R_NilValue) return LENGTH(s);
	return INTEGER(t)[0];
    }
    else if (isFrame(s)) {
	return nrows(CAR(s));
    }
    else 
        error(_("object is not a matrix"));
}


int ncols(SEXP s)
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getDimAttrib(s);
	if (t == R_NilValue) return 1;
	if (LENGTH(t) >= 2) return INTEGER(t)[1];
	/* This is a 1D (or possibly 0D array) */
	return 1;
    }
    else if (isFrame(s)) {
	return length(s);
    }
    else 
        error(_("object is not a matrix"));
}

const static char type_msg[] = "invalid type passed to internal function\n";


void attribute_hidden internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    errorcall(call, type_msg);
	else
	    error(type_msg);
    }
}

const static char * const truenames[] = {
    "TRUE",
    "T",
    "True",
    "true",
    (char *) NULL,
};

const static char * const falsenames[] = {
    "FALSE",
    "F",
    "False",
    "false",
    (char *) NULL,
};

SEXP asChar(SEXP x)
{
    if (LENGTH(x) >= 1) {
	if (isVectorAtomic(x)) {
	    int w, d, e, wi, di, ei;
	    char buf[MAXELTSIZE];  /* probably 100 would suffice */

	    switch (TYPEOF(x)) {
	    case LGLSXP:
		if (LOGICAL(x)[0] == NA_LOGICAL)
		    return NA_STRING;
		if (LOGICAL(x)[0])
		    strcpy(buf, "T");
		else
		    strcpy(buf, "F");
		return mkChar(buf);
	    case INTSXP:
		if (INTEGER(x)[0] == NA_INTEGER)
		    return NA_STRING;
		sprintf(buf, "%d", INTEGER(x)[0]);
		return mkChar(buf);
	    case REALSXP:
		PrintDefaults();
		formatReal(REAL(x), 1, &w, &d, &e, 0);
		return mkChar(EncodeReal(REAL(x)[0], w, d, e, OutDec));
	    case CPLXSXP:
		PrintDefaults();
		formatComplex(COMPLEX(x), 1, &w, &d, &e, &wi, &di, &ei, 0);
		return mkChar(EncodeComplex(COMPLEX(x)[0], w, d, e, wi, di, ei, OutDec));
	    case STRSXP:
		return STRING_ELT(x, 0);
	    default:
		return NA_STRING;
	    }
	} else if(TYPEOF(x) == CHARSXP) {
	    return x;
	} else if(TYPEOF(x) == SYMSXP)
	    return PRINTNAME(x);
    }
    return NA_STRING;
}

Rboolean isUnordered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits_CHAR (s, R_factor_CHARSXP)
	    && !inherits_CHAR (s, R_ordered_CHARSXP));
}

Rboolean isOrdered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits_CHAR (s, R_factor_CHARSXP)
	    && inherits_CHAR (s, R_ordered_CHARSXP));
}


/* -------------------------------------------------------------------------- */
/* TYPE ID <-> TYPE NAME CONVERSION.  Updated version taken from R-3.4.2,     */
/*                                    modified a bit for pqR.                 */

const static struct {
    const char * const str;
    const int type;
}

TypeTable[] = {
    { "NULL",		NILSXP	   },  /* real types */
    { "symbol",		SYMSXP	   },
    { "pairlist",	LISTSXP	   },
    { "closure",	CLOSXP	   },
    { "environment",	ENVSXP	   },
    { "promise",	PROMSXP	   },
    { "language",	LANGSXP	   },
    { "special",	SPECIALSXP },
    { "builtin",	BUILTINSXP },
    { "char",		CHARSXP	   },
    { "logical",	LGLSXP	   },
    { "integer",	INTSXP	   },
    { "double",		REALSXP	   }, /*-  "real", for R <= 0.61.x */
    { "complex",	CPLXSXP	   },
    { "character",	STRSXP	   },
    { "...",		DOTSXP	   },
    { "any",		ANYSXP	   },
    { "expression",	EXPRSXP	   },
    { "list",		VECSXP	   },
    { "externalptr",	EXTPTRSXP  },
    { "bytecode",	BCODESXP   },
    { "weakref",	WEAKREFSXP },
    { "raw",		RAWSXP },
    { "S4",		S4SXP },
    /* aliases : */
    { "numeric",	REALSXP	   },
    { "name",		SYMSXP	   },

    { (char *)NULL,	-1	   }
};


SEXPTYPE str2type (const char *s)
{
    int i;
    for (i = 0; TypeTable[i].str; i++) {
        if (!strcmp(s, TypeTable[i].str))
            return (SEXPTYPE) TypeTable[i].type;
    }

    /* SEXPTYPE is an unsigned int, so the compiler warns us w/o the cast. */
    return (SEXPTYPE) -1;
}

static struct {
    const char *cstrName;
    SEXP rcharName;
    SEXP rstrName;
    SEXP rsymName;
} Type2Table[MAX_NUM_SEXPTYPE];


static int findTypeInTypeTable (SEXPTYPE t)
 {
    for (int i = 0; TypeTable[i].str; i++)
        if (TypeTable[i].type == t) return i;

    return -1;
}

/* Called from main.c to initialize Type2Table. */

attribute_hidden
void InitTypeTables (void) {

    for (int type = 0; type < MAX_NUM_SEXPTYPE; type++) {

        int j = findTypeInTypeTable(type);

        if (j != -1) {
            const char *cstr = TypeTable[j].str;
            SEXP rsym = install(cstr);
            SEXP rchar = PRINTNAME(rsym);
            SEXP rstr = ScalarString(rchar);
            SET_NAMEDCNT_MAX(rstr);
            R_PreserveObject(rstr);
            Type2Table[type].cstrName = cstr;
            Type2Table[type].rcharName = rchar;
            Type2Table[type].rstrName = rstr;
            Type2Table[type].rsymName = rsym;
        }
        else {
            Type2Table[type].cstrName = NULL;
            Type2Table[type].rcharName = R_NoObject;
            Type2Table[type].rstrName = R_NoObject;
            Type2Table[type].rsymName = R_NoObject;
        }
    }
}

SEXP type2str_nowarn (SEXPTYPE t)   /* returns a CHARSXP */
{
    if (t < MAX_NUM_SEXPTYPE) {     /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rcharName;
        if (res != R_NoObject)
            return res;
    }

    return R_NilValue;
}

SEXP type2str (SEXPTYPE t)          /* returns a CHARSXP */
{
    SEXP s = type2str_nowarn(t);
    if (s != R_NilValue)
        return s;

    warning(_("type %d is unimplemented in '%s'"), t, "type2str");
    char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return mkChar(buf);
}

SEXP type2rstr (SEXPTYPE t)         /* returns a STRSXP */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rstrName;
        if (res != R_NoObject)
            return res;
    }

    error(_("type %d is unimplemented in '%s'"), t, "type2rstr");
}

const char *type2char (SEXPTYPE t)  /* returns a C char * */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        const char * res = Type2Table[t].cstrName;
        if (res != NULL)
            return res;
    }

    warning(_("type %d is unimplemented in '%s'"), t, "type2char");
    static char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return buf;
}

attribute_hidden
SEXP type2symbol (SEXPTYPE t)
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rsymName;
        if (res != R_NoObject)
            return res;
    }

    error(_("type %d is unimplemented in '%s'"), t, "type2symbol");
}

attribute_hidden
void R_NORETURN UNIMPLEMENTED_TYPEt (const char *s, SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
        if (TypeTable[i].type == t)
            error(_("unimplemented type '%s' in '%s'\n"), TypeTable[i].str, s);
    }

    error(_("unimplemented type (%d) in '%s'\n"), t, s);
}

void R_NORETURN UNIMPLEMENTED_TYPE (const char *s, SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}

/* -------------------------------------------------------------------------- */

void attribute_hidden check_stack_balance (SEXP op, int save)
{
    /* NEEDED: A fixup is needed in browser, because it can trap errors,
       and currently does not reset the limit to the right value. */

    if (save == R_PPStackTop) 
        return;

    REprintf ("Warning: stack imbalance in '%s', %d then %d\n",
	       PRIMNAME(op), save, R_PPStackTop);
}


R_NORETURN void too_deep_error(void)
{
    R_Expressions = R_Expressions_keep + 500;
    errorcall (R_NilValue /* avoids deparsing call in the error handler */,
   _("evaluation nested too deeply: infinite recursion / options(expressions=)?"
    ));
}


R_NORETURN void attribute_hidden dotdotdot_error(void)
{ 
    error(_("'...' used in an incorrect context"));
}

R_NORETURN void attribute_hidden arg_missing_error(SEXP sym)
{
    if (TYPEOF(sym) == SYMSXP && *CHAR(PRINTNAME(sym)))
        error(_("argument \"%s\" is missing, with no default"),
              CHAR(PRINTNAME(sym)));
    else
        error(_("argument is missing, with no default"));
}

R_NORETURN void attribute_hidden unbound_var_error(SEXP sym)
{
    error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));
}

R_NORETURN void attribute_hidden nonsubsettable_error(SEXP call, SEXP x)
{
    errorcall (call, _("object of type '%s' is not subsettable"),
               type2char(TYPEOF(x)));
}

R_NORETURN void attribute_hidden out_of_bounds_error(SEXP call)
{
    errorcall(call, _("subscript out of bounds"));
}

R_NORETURN void attribute_hidden apply_non_function_error(void)
{
    error(_("attempt to apply non-function"));
}

R_NORETURN void attribute_hidden PRSEEN_error(SEXP e)
{
    errorcall (R_GlobalContext->call,
     _("promise already under evaluation: recursive default argument reference or earlier problems?"));
}

R_NORETURN void attribute_hidden Rf_asLogicalNoNA_error (SEXP s, SEXP call)
{
    PROTECT(s);
    errorcall (call, 
      length(s) == 0 ? _("argument is of length zero") :
      isLogical(s) ?   _("missing value where TRUE/FALSE needed") :
                       _("argument is not interpretable as logical"));
}

void attribute_hidden Rf_asLogicalNoNA_warning (SEXP s, SEXP call)
{
    PROTECT(s);
    warningcall (call,
     _("the condition has length > 1 and only the first element will be used"));
    UNPROTECT(1);
}


# include <R_ext/Riconv.h>
# include <sys/param.h>
# include <errno.h>


/* Previous versions of R (< 2.3.0) assumed wchar_t was in Unicode
   (and it commonly is).  These functions do not. */
# ifdef WORDS_BIGENDIAN
static const char UCS2ENC[] = "UCS-2BE";
# else
static const char UCS2ENC[] = "UCS-2LE";
# endif


/*
 * out=NULL returns the number of the MBCS chars
 */
/* Note: this does not terminate out, as all current uses are to look
 * at 'out' a wchar at a time, and sometimes just one char.
 */
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc)
{
    void   *cd = NULL ;
    const char *i_buf;
    char *o_buf;
    size_t  i_len, o_len, status, wc_len;
    /* out length */
    wc_len = (enc == CE_UTF8)? utf8towcs(NULL, in, 0) : mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open(UCS2ENC, (enc == CE_UTF8) ? "UTF-8": "")))
	return (size_t) -1;

    i_buf = (char *)in;
    i_len = strlen(in); /* not including terminator */
    o_buf = (char *)out;
    o_len = ((size_t) nout) * sizeof(ucs2_t);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);
    int serrno = errno;
    Riconv_close(cd);
    if (status == (size_t)-1) {
	switch(serrno){
	case EINVAL:
	    return (size_t) -2;
	case EILSEQ:
	    return (size_t) -1;
	case E2BIG:
	    break;
	default:
	    errno = EILSEQ;
	    return (size_t) -1;
	}
    }
    return wc_len; /* status would be better? */
}


#include <wctype.h>

/* This one is not in Rinternals.h, but is used in internet module */
Rboolean isBlankString(const char *s)
{
    if(mbcslocale) {
	wchar_t wc; size_t used; mbstate_t mb_st;
	mbs_init(&mb_st);
	while( (used = Mbrtowc(&wc, s, MB_CUR_MAX, &mb_st)) ) {
	    if(!iswspace((wint_t) wc)) return FALSE;
	    s += used;
	}
    } else
	while (*s)
	    if (!isspace((int)*s++)) return FALSE;
    return TRUE;
}

Rboolean StringBlank(SEXP x)
{
    if (x == R_NilValue) return TRUE;
    else return CHAR(x)[0] == '\0';
}

/* Function to test whether a string is a true value */

Rboolean StringTrue(const char *name)
{
    int i;
    if (*name != 'T' && *name != 't') 
        return FALSE;
    for (i = 0; truenames[i] != NULL; i++)
	if (strcmp (name, truenames[i]) == 0)
	    return TRUE;
    return FALSE;
}

Rboolean StringFalse(const char *name)
{
    int i;
    if (*name != 'F' && *name != 'f')
        return FALSE;
    for (i = 0; falsenames[i] != NULL; i++)
	if (strcmp (name, falsenames[i]) == 0)
	    return TRUE;
    return FALSE;
}

/* Character hashing done in memory.c and names.c; also here to stop compiler 
   from inlining it.  Four forms, one for null-terminated string, one with
   specified length, and one for a null-terminated string with given starting
   hash from previous characters, one same but with specified length. */

attribute_hidden int Rf_char_hash (const char *s)
{
    /* Hash function due to Dan Bernstein, called "djb2" at
       http://www.cse.yorku.ca/~oz/hash.html */

    unsigned int h, t;

    h = 5381;

    while ((t = *s++) != 0) {
        h = (h << 5) + h + t;
    }

    return h & 0x7fffffff;
}

attribute_hidden int Rf_char_hash_len (const char *s, int len)
{
    /* Hash function due to Dan Bernstein, called "djb2" at
       http://www.cse.yorku.ca/~oz/hash.html 

       Basic idea is to iterate h = ((h << 5) + h) + *s++, but here
       this is unrolled, allowing some merging of operations to be
       done (though we actually end up doing more shifts and adds),
       and more scope for instruction-level parallelism. */

    unsigned int h = 5381;
    if (len & 1) {
        h = (5381*33) + *s++;
        len -= 1;
    }
    while (len > 0) {
        unsigned int t;
        t = *s++;
        t = (t << 5) + t + *s++;
        h = (h << 10) + (h << 6) + h + t;
        len -= 2;
    }

    return h & 0x7fffffff;
}

attribute_hidden int Rf_char_hash_more (unsigned h, const char *s)
{
    unsigned int t;

    while ((t = *s++) != 0) {
        h = (h << 5) + h + t;
    }

    return h & 0x7fffffff;
}

attribute_hidden int Rf_char_hash_more_len (unsigned h, const char *s, int len)
{
    if (len & 1) {
        h = (h << 5) + h + *s++;
        len -= 1;
    }
    while (len > 0) {
        unsigned int t;
        t = *s++;
        t = (t << 5) + t + *s++;
        h = (h << 10) + (h << 6) + h + t;
        len -= 2;
    }

    return h & 0x7fffffff;
}

/* used in bind.c and options.c */
SEXP attribute_hidden EnsureString(SEXP s)
{
    switch(TYPEOF(s)) {
    case SYMSXP:
	s = PRINTNAME(s);
	break;
    case STRSXP:
	s = STRING_ELT(s, 0);
	break;
    case CHARSXP:
	break;
    case NILSXP:
	s = R_BlankString;
	break;
    default:
	error(_("invalid tag in name extraction"));
    }
    return s;
}

/* Allocate space for the result of an operation, or reuse the space for
   one of its operands, if it has NAMEDCNT of zero. Attributes are assumed 
   to be taken from the operands, with the first operand's attributes taking 
   precedence.  The length of the result is assumed to be the maximum of the 
   lengths of the operands (unless the result length is zero).  The two
   local_assign[12] arguments override the requirement that NAMEDCNT be 0. */

SEXP attribute_hidden alloc_or_reuse (SEXP s1, SEXP s2, SEXPTYPE typ, int n,
                                      int local_assign1, int local_assign2)
{
    int n1 = LENGTH(s1);
    int n2 = LENGTH(s2);

    /* Try to use space for 2nd arg if both same length, so 1st argument's
       attributes will then take precedence when copied. */

    if (n2==n) {
        if (TYPEOF(s2)==typ && (local_assign2 || NAMEDCNT_EQ_0(s2))) {
            /* Must remove any "names" attribute of s2 to match action of
               copyMostAttrib.  Any "dim" and "dimnames" attributes are allowed
               to stay, since they will be overwritten anyway. */
            if (HAS_ATTRIB(s2)) 
                setAttrib (s2, R_NamesSymbol, R_NilValue);
            return s2;
        }
        else {
            /* Can use 1st arg's space only if 2nd arg has no attributes, else
               we may not get attributes of result right. */
            if (n1==n && TYPEOF(s1)==typ && (local_assign1 || NAMEDCNT_EQ_0(s1))
                      && !HAS_ATTRIB(s2))
                return s1;
        }
    } 
    else if (n1==n) {
        if (TYPEOF(s1)==typ && (local_assign1 || NAMEDCNT_EQ_0(s1)))
            return s1;
    }

    return allocVector (typ, n);
}

/* used in modules */
void Rf_checkArityCall(SEXP op, SEXP args, SEXP call)
{
    int len, arity;
    SEXP rest;

    arity = PRIMARITY(op);
    if (arity < 0) 
        return;

    /* For speed, check number of args with code here rather than call length */
    rest = args;
    while (rest != R_NilValue) {
        arity -= 1;
        rest = CDR(rest);
    }
    if (arity == 0) 
        return;

    len = length(args);
    if (PRIMINTERNAL(op))
        error(ngettext("%d argument passed to .Internal(%s) which requires %d",
                 "%d arguments passed to .Internal(%s) which requires %d",
                 (unsigned long) len),
              len, PRIMNAME(op), PRIMARITY(op));
    else
        errorcall(call,
                  ngettext("%d argument passed to '%s' which requires %d",
                           "%d arguments passed to '%s' which requires %d",
                           (unsigned long) len),
                  len, PRIMNAME(op), PRIMARITY(op));
}

void attribute_hidden Rf_check1arg(SEXP arg, SEXP call, const char *formal)
{
    SEXP tag = TAG(arg);
    if (tag != R_NilValue) {
        if (ep_match_strings(formal,CHAR(PRINTNAME(tag))) == 0) 
            errorcall (call,
              _("supplied argument name '%s' does not match '%s'"),
              CHAR(PRINTNAME(TAG(arg))), formal);
    }
}

/* Called in the check1arg_x macro */
R_NORETURN void attribute_hidden Rf_check1arg_x_error (SEXP arg, SEXP call)
{
    errorcall (call, _("supplied argument name '%s' does not match '%s'"),
                     CHAR(PRINTNAME(TAG(arg))), "x");
}


SEXP nthcdr(SEXP s, int n)
{
    if ((CONS_TYPES >> TYPEOF(s)) & 1) {
	for (int i = 0; i<n; i++) {
	    if (s == R_NilValue)
		error(_("'nthcdr' list shorter than %d"), n);
	    s = CDR(s);
	}
	return s;
    }
    else error(_("'nthcdr' needs a list to CDR down"));
}


/* Find the index (from one) of the first element in a pairlist having a 
   given tag.  Returns zero if the tag isn't present. */

int tag_index (SEXP s, SEXP tag)
{
    int n = 1;

    for (;;) {
        if (s == R_NilValue)
            return 0;
        if (TAG(s) == tag)
            return n;
        s = CDR(s);
        n += 1;
    }
}


#define DUP_CONS(dst,src) do { \
    dst = cons_with_tag (CAR(src), R_NilValue, TAG(src)); \
    SET_TYPEOF (dst, TYPEOF(src)); \
    SET_ATTRIB (dst, ATTRIB(src)); \
    SET_OBJECT (dst, OBJECT(src)); \
    SETLEVELS  (dst, LEVELS(src)); \
} while (0)
  

/* Create a dst pairlist (of same type as first arg) with the nth item
   (counting from one) changed to val.  Silently returns the same list if the
   list isn't at least n long.  The new list will share CONS cells with the 
   old after the point of deletion.  No existing CONS cells are altered. 
   The arguments needn't be protected by the caller. */

SEXP with_changed_nth (SEXP s, int n, SEXP val)
{
    SEXP original = s;
    SEXP head, tail, new;

    if (s == R_NilValue)
        return R_NilValue;

    PROTECT2(s,val);
    DUP_CONS(head,s);
    PROTECT(head);
    tail = head;

    while (n > 1) {
        if (s == R_NilValue) {
            UNPROTECT(3);
            return original;
        }
        s = CDR(s);
        DUP_CONS(new,s);
        SETCDR (tail, new);
        tail = new;
        n -= 1;
    }

    SETCAR(tail,val);
    SETCDR(tail,CDR(s));

    UNPROTECT(3);
    return head;
}


/* Create a new pairlist (of same type as first arg, or of the second if the
   first is R_NilValue) with a pairlist appended at the end.  No existing CONS 
   cells are altered.  The arguments needn't be protected by the caller. */

SEXP with_pairlist_appended (SEXP s, SEXP t)
{
    SEXP head, tail, new;

    if (s == R_NilValue)
        return t;

    PROTECT2(s,t);
    DUP_CONS(head,s);
    PROTECT(head);
    tail = head;

    for (;;) {
        s = CDR(s);
        if (s == R_NilValue)
            break;
        DUP_CONS(new,s);
        SETCDR (tail, new);
        tail = new;
    }

    SETCDR (tail, t);

    UNPROTECT(3);
    return head;
}


/* Create a new pairlist/language (of the same type and attributes as
   s, or of its CDR if the first element of s is the one deleted) with
   the nth item (counting from one) deleted.  Silently returns the
   same list if the list isn't at least n long.  The new list will
   share CONS cells with the old after the point of deletion.  No
   existing CONS cells are altered.  The first argument needn't be
   protected by the caller. */

SEXP with_no_nth (SEXP s, int n)
{
    SEXP original = s;
    SEXP head, tail, new;

    if (s == R_NilValue)
        return R_NilValue;
    if (n == 1)
        return CDR(s);

    PROTECT(s);

    DUP_CONS(head,s);
    PROTECT(head);
    tail = head;

    for (;;) {
        s = CDR(s);
        n -= 1;
        if (n <= 1)
            break;
        if (s == R_NilValue) {
            UNPROTECT(2);
            return original;
        }
        DUP_CONS(new,s);
        SETCDR (tail, new);
        tail = new;
    }

    SETCDR(tail,CDR(s));

    UNPROTECT(2);
    return head;
}

/* TODO: a  Length(.) {say} which is  length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.c
*/

R_xlen_t xlength(SEXP s) { return length(s); }

R_len_t length(SEXP s)
{
    extern int Rf_envlength(SEXP);
    SEXPTYPE type = TYPEOF(s);

    if (type == NILSXP)
        return 0;
    else if ((CONS_TYPES >> type) & 1)
    {   /* Loop below relies on CDR(R_NilValue) == R_NilValue */
        SEXP t;
        int i;
        i = 0;
        do {
            i += 2;
            t = CDR(s);
            s = CDR(t);
        } while (s != R_NilValue);
        return t == R_NilValue ? i-1 : i;
    }
    else if (type == ENVSXP)
        return Rf_envlength(s);
#if USE_AUX_FOR_ATTRIB
    else if ( ! ((VECTOR_OR_CHAR_TYPES >> type) & 1))
        return 1;
#endif
    else
        return LENGTH(s);  /* Always exists unless USE_AUX_FOR_ATTRIB */
}

/* This is a primitive (with no arguments) */
static SEXP do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    int nargs = NA_INTEGER;

    checkArity(op, args);
    for (cptr = R_GlobalContext; cptr != NULL; cptr = cptr->nextcontext) {
	if ((cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == rho) {
	    nargs = length(cptr->promargs);
	    break;
	}
    }
    return ScalarIntegerMaybeConst(nargs);
}


void attribute_hidden setIVector(int * vec, int len, int val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


void attribute_hidden setRVector(double * vec, int len, double val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}

/* unused in R, in Rinternals.h */
void setSVector(SEXP * vec, int len, SEXP val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


Rboolean isFree(SEXP val)
{
    SEXP t;
    for (t = R_FreeSEXP; t != R_NilValue; t = CAR(t))
	if (val == t)
	    return TRUE;
    return FALSE;
}


/* Debugging functions (hence the d-prefix). */
/* These are intended to be called interactively from */
/* a debugger such as gdb, so you don't have to remember */
/* the names of the data structure components. */

int dtype(SEXP q)
{
    return((int)TYPEOF(q));
}


SEXP dcar(SEXP l)
{
    return(CAR(l));
}


SEXP dcdr(SEXP l)
{
    return(CDR(l));
}


static void isort_with_index(int *x, int *indx, int n)
{
    int i, j, h, iv, v;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && x[j - h] > v)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}


/* merge(xinds, yinds, all.x, all.y) */
/* xinds, yinds are along x and y rows matching into the (numeric)
   common indices, with 0 for non-matches.

   all.x and all.y are boolean.

   The return value is a list with 4 elements (xi, yi, x.alone, y.alone),
   which are index vectors for rows of x or y.
*/
static SEXP do_merge(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP xi, yi, ansx, ansy, ans, x_lone, y_lone;
    int nx = 0, ny = 0, i, j, k, nans = 0, nx_lone = 0, ny_lone = 0;
    int all_x = 0, all_y = 0, ll = 0/* "= 0" : for -Wall */;
    int *ix, *iy, tmp, nnx, nny, i0, j0;
    const char *nms[] = {"xi", "yi", "x.alone", "y.alone", ""};

    checkArity(op, args);
    xi = CAR(args);
    if ( !isInteger(xi) || !(nx = LENGTH(xi)) )
	error(_("invalid '%s' argument"), "xinds");
    yi = CADR(args);
    if ( !isInteger(yi) || !(ny = LENGTH(yi)) )
	error(_("invalid '%s' argument"), "yinds");
    if(!LENGTH(ans = CADDR(args)) || NA_LOGICAL == (all_x = asLogical(ans)))
	error(_("'all.x' must be TRUE or FALSE"));
    if(!LENGTH(ans = CADDDR(args))|| NA_LOGICAL == (all_y = asLogical(ans)))
	error(_("'all.y' must be TRUE or FALSE"));

    /* 0. sort the indices */
    ix = (int *) R_alloc((size_t) nx, sizeof(int));
    iy = (int *) R_alloc((size_t) ny, sizeof(int));
    for(i = 0; i < nx; i++) ix[i] = i+1;
    for(i = 0; i < ny; i++) iy[i] = i+1;
    isort_with_index(INTEGER(xi), ix, nx);
    isort_with_index(INTEGER(yi), iy, ny);

    /* 1. determine result sizes */
    for (i = 0; i < nx; i++) if (INTEGER(xi)[i] > 0) break; nx_lone = i;
    for (i = 0; i < ny; i++) if (INTEGER(yi)[i] > 0) break; ny_lone = i;
    for (i = nx_lone, j = ny_lone; i < nx; i = nnx, j = nny) {
	tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	/* the next is not in theory necessary,
	   since we have the common values only */
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	/* printf("i %d nnx %d j %d nny %d\n", i, nnx, j, nny); */
	nans += (nnx-i)*(nny-j);
    }


    /* 2. allocate and store result components */

    PROTECT(ans = mkNamed(VECSXP, nms));
    ansx = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);

    if(all_x) {
	x_lone = allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	for (i = 0, ll = 0; i < nx_lone; i++)
	    INTEGER(x_lone)[ll++] = ix[i];
    }

    if(all_y) {
	y_lone = allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	for (i = 0, ll = 0; i < ny_lone; i++)
	    INTEGER(y_lone)[ll++] = iy[i];
    }

    for (i = nx_lone, j = ny_lone, k = 0; i < nx; i = nnx, j = nny) {
	tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	for(i0 = i; i0 < nnx; i0++)
	    for(j0 = j; j0 < nny; j0++) {
		INTEGER(ansx)[k]   = ix[i0];
		INTEGER(ansy)[k++] = iy[j0];
	    }
    }

    UNPROTECT(1);
    return ans;
}


/* Functions for getting and setting the working directory. */
#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#endif

SEXP static intern_getwd(void)
{
    SEXP rval = R_NilValue;
    char buf[PATH_MAX+1];

#ifdef Win32
    {
	wchar_t wbuf[PATH_MAX+1];
	int res = GetCurrentDirectoryW(PATH_MAX, wbuf);
	if(res > 0) {
	    wcstoutf8(buf, wbuf, PATH_MAX+1);
	    R_UTF8fixslash(buf);
	    PROTECT(rval = allocVector(STRSXP, 1));
	    SET_STRING_ELT(rval, 0, mkCharCE(buf, CE_UTF8));
	    UNPROTECT(1);
	}
    }
#else
    char *res = getcwd(buf, PATH_MAX); /* can return NULL */
    if(res) rval = mkString(buf);
#endif
    return(rval);
}

static SEXP do_getwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return(intern_getwd());
}


#if defined(Win32) && defined(_MSC_VER)
# include <direct.h> /* for chdir, via io.h */
#endif

static SEXP do_setwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, wd;

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	error(_("character argument expected"));
    if (STRING_ELT(s, 0) == NA_STRING)
	error(_("missing value is invalid"));

    /* get current directory to return */
    PROTECT(wd = intern_getwd());

#ifdef Win32
    {
	const wchar_t *path = filenameToWchar(STRING_ELT(s, 0), TRUE);
	if(_wchdir(path) < 0)
	    error(_("cannot change working directory"));
    }
#else
    {
	const char *path
	    = R_ExpandFileName(translateChar(STRING_ELT(s, 0)));
    if(chdir(path) < 0)
	error(_("cannot change working directory"));
    }
#endif
    UNPROTECT(1); /* wd */
    return(wd);
}

/* remove portion of path before file separator if one exists */

#ifdef Win32
static SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s;
    char sp[PATH_MAX];
    wchar_t  buf[PATH_MAX], *p;
    const wchar_t *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT_NA(ans, i);
	else {
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1) error(_("path too long"));
	    wcscpy(buf, pp);
	    R_wfixslash(buf);
	    /* remove trailing file separator(s) */
	    if (*buf) {
		p = buf + wcslen(buf) - 1;
		while (p >= buf && *p == L'/') *(p--) = L'\0';
	    }
	    if ((p = wcsrchr(buf, L'/'))) p++; else p = buf;
	    memset(sp, 0, PATH_MAX); /* safety */
	    wcstoutf8(sp, p, 4*wcslen(p) + 1);
	    SET_STRING_ELT(ans, i, mkCharCE(sp, CE_UTF8));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
static SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s;
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT_NA(ans, i);
	else {
	    pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    strcpy (buf, pp);
	    if (*buf) {
		p = buf + strlen(buf) - 1;
		while (p >= buf && *p == fsp) *(p--) = '\0';
	    }
	    if ((p = Rf_strrchr(buf, fsp)))
		p++;
	    else
		p = buf;
	    SET_STRING_ELT(ans, i, mkChar(p));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif

/* remove portion of path after last file separator if one exists, else
   return "."
   */

#ifdef Win32
SEXP attribute_hidden do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s;
    wchar_t buf[PATH_MAX], *p;
    const wchar_t *pp;
    char sp[4*PATH_MAX];
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT_NA(ans, i);
	else {
	    memset(sp, 0, 4*PATH_MAX);
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    if (wcslen(pp)) {
		wcscpy (buf, pp);
		R_wfixslash(buf);
		/* remove trailing file separator(s) */
		while ( *(p = buf + wcslen(buf) - 1) == L'/'  && p > buf
			&& (p > buf+2 || *(p-1) != L':')) *p = L'\0';
		p = wcsrchr(buf, L'/');
		if(p == NULL) wcscpy(buf, L".");
		else {
		    while(p > buf && *p == L'/'
			  /* this covers both drives and network shares */
			  && (p > buf+2 || *(p-1) != L':')) --p;
		    p[1] = L'\0';
		}
		wcstoutf8(sp, buf, 4*wcslen(buf)+1);
	    }
	    SET_STRING_ELT(ans, i, mkCharCE(sp, CE_UTF8));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
SEXP attribute_hidden do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s;
    char buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT_NA(ans, i);
	else {
	    pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    size_t ll = strlen(pp);
	    if (ll) { // svMisc calls this with ""
		strcpy (buf, pp);
		/* remove trailing file separator(s) */
		while ( *(p = buf + ll - 1) == fsp  && p > buf) *p = '\0';
		p = Rf_strrchr(buf, fsp);
		if(p == NULL)
		    strcpy(buf, ".");
		else {
		    while(p > buf && *p == fsp) --p;
		    p[1] = '\0';
		}
	    } else buf[0] = '\0';
	    SET_STRING_ELT(ans, i, mkChar(buf));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif


#ifdef Win32 /* Windows version is in src/gnuwin32/extra.c */
extern SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho);
#else
#ifndef HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif

static SEXP do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args);
    int i, n = LENGTH(paths);
    const char *path;
    char abspath[PATH_MAX+1];

    checkArity(op, args);
    if (!isString(paths))
	error(_("'path' must be a character vector"));

    int mustWork = asLogical(CADDR(args)); /* 1, NA_LOGICAL or 0 */

/* Does any platform not have this? */
#ifdef HAVE_REALPATH
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = translateChar(STRING_ELT(paths, i));
	char *res = realpath(path, abspath);
	if (res) 
	    SET_STRING_ELT(ans, i, mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#else
    Rboolean OK;
    warning("this platform does not have realpath so the results may not be canonical");
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = translateChar(STRING_ELT(paths, i));
	OK = strlen(path) <= PATH_MAX;
	if (OK) {
	    if (path[0] == '/') strncpy(abspath, path, PATH_MAX);
	    else {
		OK = getcwd(abspath, PATH_MAX) != NULL;
		OK = OK && (strlen(path) + strlen(abspath) + 1 <= PATH_MAX);
		if (OK) {strcat(abspath, "/"); strcat(abspath, path);}
	    }
	}
	/* we need to check that this exists */
	if (OK) OK = (access(abspath, 0 /* F_OK */) == 0);
	if (OK) SET_STRING_ELT(ans, i, mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#endif
    UNPROTECT(1);
    return ans;
}
#endif


/* encodeString(x, w, quote, justify) */
static SEXP do_encodeString(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, s;
    int i, len, w, quote = 0, justify, na;
    const char *cs;
    Rboolean findWidth;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    if(isNull(CADR(args))) w = NA_INTEGER;
    else {
	w = asInteger(CADR(args));
	if(w != NA_INTEGER && w < 0)
	    error(_("invalid '%s' value"), "width");
    }
    findWidth = (w == NA_INTEGER);
    s = CADDR(args);
    if(LENGTH(s) != 1 || TYPEOF(s) != STRSXP)
	error(_("invalid '%s' value"), "quote");
    cs = translateChar(STRING_ELT(s, 0));
    if(strlen(cs) > 0) quote = cs[0];
    if(strlen(cs) > 1)
	warning(_("only the first character of 'quote' will be used"));
    justify = asInteger(CADDDR(args));
    if(justify == NA_INTEGER || justify < 0 || justify > 3)
	error(_("invalid '%s' value"), "justify");
    if(justify == 3) w = 0;
    na = asLogical(CAD4R(args));
    if(na == NA_LOGICAL) error(_("invalid '%s' value"), "na.encode");

    len = LENGTH(x);
    if(findWidth && justify < 3) {
	w  = 0;
	for(i = 0; i < len; i++) {
	    s = STRING_ELT(x, i);
	    if(na || s != NA_STRING)
		w = imax2(w, Rstrlen(s, quote));
	}
	if(quote) w +=2; /* for surrounding quotes */
    }
    PROTECT(ans = duplicate(x));
    for(i = 0; i < len; i++) {
	s = STRING_ELT(x, i);
	if(na || s != NA_STRING)
	    SET_STRING_ELT(ans, i, mkChar(EncodeString(s, w, quote, (Rprt_adj) justify)));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP do_encoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    int i, n;
    char *tmp;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    n = LENGTH(x);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if(IS_BYTES(STRING_ELT(x, i))) tmp = "bytes";
	else if(IS_LATIN1(STRING_ELT(x, i))) tmp = "latin1";
	else if(IS_UTF8(STRING_ELT(x, i))) tmp = "UTF-8";
	else tmp = "unknown";
	SET_STRING_ELT(ans, i, mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP do_setencoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, enc, tmp;
    int i, m, n;
    const char *this;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    if (TYPEOF(enc = CADR(args)) != STRSXP)
	error(_("a character vector 'value' expected"));
    m = LENGTH(enc);
    if(m == 0)
	error(_("'value' must be of positive length"));
    if(NAMEDCNT_GT_0(x)) x = duplicate(x);
    PROTECT(x);
    n = LENGTH(x);
    for(i = 0; i < n; i++) {
	cetype_t ienc = CE_NATIVE;
	this = CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if(streql(this, "latin1")) ienc = CE_LATIN1;
	else if(streql(this, "UTF-8")) ienc = CE_UTF8;
	else if(streql(this, "bytes")) ienc = CE_BYTES;
	tmp = STRING_ELT(x, i);
	if(tmp == NA_STRING) continue;
	if (! ((ienc == CE_LATIN1 && IS_LATIN1(tmp)) ||
	       (ienc == CE_UTF8 && IS_UTF8(tmp)) ||
	       (ienc == CE_BYTES && IS_BYTES(tmp)) ||
	       (ienc == CE_NATIVE && ! IS_LATIN1(tmp) && ! IS_UTF8(tmp))))
	    SET_STRING_ELT(x, i, mkCharLenCE(CHAR(tmp), LENGTH(tmp), ienc));
    }
    UNPROTECT(1);
    return x;
}

SEXP attribute_hidden markKnown(const char *s, SEXP ref)
{
    cetype_t ienc = CE_NATIVE;
    if(ENC_KNOWN(ref)) {
	if(known_to_be_latin1) ienc = CE_LATIN1;
	if(known_to_be_utf8) ienc = CE_UTF8;
    }
    return mkCharCE(s, ienc);
}

Rboolean strIsASCII(const char *str)
{
    const char *p;
    for(p = str; *p; p++)
	if((unsigned int)*p > 0x7F) return FALSE;
    return TRUE;
}

/* Number of additional bytes */
static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

int attribute_hidden utf8clen(char c)
{
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}

/* utf8toucs and utf8towcs return the result in wchar_t, but
   assume wchar_t is UCS-2/4 and so are for internal use only. */

int attribute_hidden /* Returns number of bytes used, or -1 or -2 on error */
utf8toucs(wchar_t *wc, const char *s)
{
    unsigned int byte;
    wchar_t local, *w;
    byte = *((unsigned char *)s);
    w = wc ? wc: &local;

    if (byte == 0) {
	*w = (wchar_t) 0;
	return 0;
    } else if (byte < 0xC0) {
	*w = (wchar_t) byte;
	return 1;
    } else if (byte < 0xE0) {
	if(strlen(s) < 2) return -2;
	if ((s[1] & 0xC0) == 0x80) {
	    *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
	    return 2;
	} else return -1;
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return -2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = (wchar_t) (((byte & 0x0F) << 12)
			    | (unsigned int) ((s[1] & 0x3F) << 6)
			    | (s[2] & 0x3F));
	    byte = (unsigned int) *w;
	    /* Surrogates range */
	    if(byte >= 0xD800 && byte <= 0xDFFF) return -1;
	    if(byte == 0xFFFE || byte == 0xFFFF) return -1;
	    return 3;
	} else return -1;
    }
    if(sizeof(wchar_t) < 4) return -2;
    /* So now handle 4,5.6 byte sequences with no testing */
    if (byte < 0xf8) {
	if(strlen(s) < 4) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 18)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 6)
			| (s[3] & 0x3F));
	return 4;
    } else if (byte < 0xFC) {
	if(strlen(s) < 5) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 24)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 12)
			| (unsigned int) ((s[3] & 0x3F) << 6)
			| (s[4] & 0x3F));
	return 5;
    } else {
	if(strlen(s) < 6) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 30)
			| (unsigned int) ((s[1] & 0x3F) << 24)
			| (unsigned int) ((s[2] & 0x3F) << 18)
			| (unsigned int) ((s[3] & 0x3F) << 12)
			| (unsigned int) ((s[4] & 0x3F) << 6)
			| (s[5] & 0x3F));
	return 6;
    }
}

size_t
utf8towcs(wchar_t *wc, const char *s, size_t n)
{
    int m;
    size_t res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    m  = utf8toucs(p, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (res >= n) break;
	}
    else
	for(t = s; ; res++, t += m) {
	    m  = utf8toucs(&local, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	}
    return res;
}

/* based on pcre.c */
static const unsigned int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const unsigned int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

static size_t Rwcrtomb(char *s, const wchar_t wc)
{
    register size_t i, j;
    unsigned int cvalue = (unsigned int) wc;
    char buf[10], *b;

    b = s ? s : buf;
    if(cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
	if (cvalue <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
	*b-- = (char) (0x80 | (cvalue & 0x3f));
	cvalue >>= 6;
    }
    *b = (char) (utf8_table2[i] | cvalue);
    return i + 1;
}

/* attribute_hidden? */
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n)
{
    ssize_t m, res=0;
    char *t;
    const wchar_t *p;
    if(s) {
	for(p = wc, t = s; ; p++) {
	    m  = (ssize_t) Rwcrtomb(t, *p);
	    if(m <= 0) break;
	    res += m;
	    if(res >= n) break;
	    t += m;
	}
    } else {
	for(p = wc; ; p++) {
	    m  = (ssize_t) Rwcrtomb(NULL, *p);
	    if(m <= 0) break;
	    res += m;
	}
    }
    return (size_t) res;
}


/* A version that reports failure as an error */
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return (size_t)0;
    used = mbrtowc(wc, s, n, ps);
    if((int) used < 0) {
	/* This gets called from the menu setup in RGui */
	if (!R_Is_Running) return (size_t)-1;
	/* let's try to print out a readable version */
	char err[4*strlen(s) + 1], *q;
	const char *p;
	R_CHECKSTACK();
	for(p = s, q = err; *p; ) {
	    /* don't do the first to keep ps state straight */
	    if(p > s) used = mbrtowc(NULL, p, n, ps);
	    if(used == 0) break;
	    else if((int) used > 0) {
		memcpy(q, p, used);
		p += used;
		q += used;
		n -= used;
	    } else {
		sprintf(q, "<%02x>", (unsigned char) *p++);
		q += 4;
		n--;
	    }
	}
	*q = '\0';
	error(_("invalid multibyte string at '%s'"), err);
    }
    return used;
}

Rboolean mbcsValid(const char *str)
{
    return  ((int)mbstowcs(NULL, str, 0) >= 0);
}


#include "valid_utf8.h"
Rboolean utf8Valid(const char *str)
{
    return valid_utf8(str, strlen(str)) == 0;
}


/* MBCS-aware versions of common comparisons.  Only used for ASCII c */
char *Rf_strchr(const char *s, int c)
{
    char *p = (char *)s;
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return strchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) return p;
	p += used;
    }
    return (char *)NULL;
}

char *Rf_strrchr(const char *s, int c)
{
    char *p = (char *)s, *plast = NULL;
    mbstate_t mb_st;
    size_t used;

    if(!mbcslocale || utf8locale) return strrchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) plast = p;
	p += used;
    }
    return plast;
}

#ifdef Win32
void R_fixslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *p = '/';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '\\') *p = '/';
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

void R_UTF8fixslash(char *s)
{
    char *p = s;

	for (; *p; p++) if (*p == '\\') *p = '/';
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

static void R_wfixslash(wchar_t *s)
{
    wchar_t *p = s;

    for (; *p; p++) if (*p == L'\\') *p = L'/';
    /* preserve network shares */
    if(s[0] == L'/' && s[1] == L'/') s[0] = s[1] = L'\\';
}


void R_fixbackslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '/') *p = '\\';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '/') *p = '\\';
}
#endif

void F77_SYMBOL(rexitc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	warning(_("error message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, (size_t) nc);
    buf[nc] = '\0';
    error("%s", buf);
}

void F77_SYMBOL(rwarnc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
	warning(_("warning message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, (size_t) nc);
    buf[nc] = '\0';
    warning("%s", buf);
}

void F77_SYMBOL(rchkusr)(void)
{
    R_CheckUserInterrupt();
}

/* Return a copy of a string using memory from R_alloc */
char *acopy_string(const char *in)
{
    char *out;
    size_t len = strlen(in);
    if (len > 0) {
	out = (char *) R_alloc(1 + len, sizeof(char));
	strcpy(out, in);
    } else
	out = "";
    return out;
}




/* Table from
http://unicode.org/Public/MAPPINGS/VENDORS/ADOBE/symbol.txt
*/

static int s2u[224] = {
    0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
    0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
    0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
    0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
    0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
    0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
    0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
    0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
    0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
    0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
    0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
    0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5,
    0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
    0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
    0x2220, 0x2207, 0xF6DA, 0xF6D9, 0xF6DB, 0x220F, 0x221A, 0x22C5,
    0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
    0x25CA, 0x2329, 0xF8E8, 0xF8E9, 0xF8EA, 0x2211, 0xF8EB, 0xF8EC,
    0xF8ED, 0xF8EE, 0xF8EF, 0xF8F0, 0xF8F1, 0xF8F2, 0xF8F3, 0xF8F4,
    0x0020, 0x232A, 0x222B, 0x2320, 0xF8F5, 0x2321, 0xF8F6, 0xF8F7,
    0xF8F8, 0xF8F9, 0xF8FA, 0xF8FB, 0xF8FC, 0xF8FD, 0xF8FE, 0x0020
};

void *Rf_AdobeSymbol2utf8(char *work, const char *c0, int nwork)
{
    const unsigned char *c = (unsigned char *) c0;
    unsigned char *t = (unsigned char *) work;
    while (*c) {
	if (*c < 32) *t++ = ' ';
	else {
	    unsigned int u = (unsigned int) s2u[*c - 32];
	    if (u < 128) *t++ = (unsigned char) u;
	    else if (u < 0x800) {
		*t++ = (unsigned char) (0xc0 | (u >> 6));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
	    } else {
		*t++ = (unsigned char) (0xe0 | (u >> 12));
		*t++ = (unsigned char) (0x80 | ((u >> 6) & 0x3f));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
	    }
	}
	if (t+6 > (unsigned char *)(work + nwork)) break;
	c++;
    }
    *t = '\0';
    return (char*) work;
}

int attribute_hidden Rf_AdobeSymbol2ucs2(int n)
{
    if(n >= 32 && n < 256) return s2u[n-32];
    else return 0;
}

#define MAX_EXPONENT_PREFIX 9999

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA)
{
    double ans;  /* NOT long double - try to avoid double rounding */
    int n, sign, expn;
    const char *p;

    /* Skip any initial whitespace. */

    for (p = str; isspace(*p); p++) ;

    /* Look for a sign. */

    sign = 1;
    switch (*p) {
    case '-': 
        sign = -1;
        /* fall through */
    case '+': 
        p += 1;
        NA = FALSE;  /* don't allow NA after sign */
        break;
    default:
        break;
    }

    /* Look for special "NA", "NaN", "infinity", "Inf" possibilities.  Also for
       hex values. */

    switch (*p) {

    case 'n': case 'N':
        if (NA && strncmp(p,"NA",2) == 0) {
            ans = NA_REAL;
            p += 2;
            goto done;
        }
        if (strncasecmp(p,"NaN",3) == 0) {
            ans = R_NaN;
            p += 3;
            goto done;
        }
        break;

    case 'i': case 'I':
        /* C99 specifies this; must come first to avoid 'inf' match */
        if (strncasecmp(p,"infinity",8) == 0) {
            ans = R_PosInf;
            p += 8;
            goto done;
        } 
        if (strncasecmp(p,"Inf",3) == 0) {
            ans = R_PosInf;
            p += 3;
            goto done;
        }
        break;

    case '0':  /* possible hex value */

        if (p[1] != 'x' && p[1] != 'X' || p[2] == 0)  /* not hex after all... */
            break;

        /* This should produce the correctly-rounded result, and overflow to
           Inf if appropriate */

        int exph = -1;
        ans = 0;
        for (p = p+2; ; p++) {
            if (*p == dec) {
                if (exph >= 0) break;  /* second decimal point */
                exph = 0;
                continue;
            }
            if ('0' <= *p && *p <= '9') 
                ans = 16*ans + (*p -'0');
            else if ('a' <= *p && *p <= 'f')
                ans = 16*ans + (*p -'a' + 10);
            else if ('A' <= *p && *p <= 'F')
                ans = 16*ans + (*p -'A' + 10);
            else 
                break;
            if (exph >= 0) exph += 4;
        }
        if (*p == 'p' || *p == 'P') {
            int expsign = 1;
            switch(*++p) {
            case '-': expsign = -1;
            case '+': p++;
            default: ;
            }
            /* The test for n is in response to PR#16358; for large exponents,
               later underflow or overflow will produce 0 or Inf. */
            for (n = 0; *p >= '0' && *p <= '9'; p++) {
                n = n * 10 + (*p - '0');
                if (n > MAX_EXPONENT_PREFIX) n = MAX_EXPONENT_PREFIX;
            }
            expn = expsign * n;
        }
        else
            expn = 0;
        if (exph > 0) expn -= exph;
        while (expn >= 10)  { ans *= 1<<10;       expn -= 10; }
        while (expn <= -10) { ans *= 1.0/(1<<10); expn += 10; }
        while (expn >= 1)   { ans *= 2;           expn -= 1; }
        while (expn <= -1)  { ans *= 0.5;         expn += 1; }
        goto done;

    default:
        break;
    }

    /* NEEDS FIXING.  Code below doesn't always do exactly-correct rounding. */

    long double lans = 0.0;
    int ndigits = 0;

    for ( ; *p >= '0' && *p <= '9'; p++) {
        lans = 10*lans + (*p - '0');
        ndigits += 1;
    }

    if (*p == 0 && ndigits > 0) {  /* quick exit for integers */
        ans = (double) lans;
        goto done;
    }

    expn = 0;
    if (*p == dec) {
        p += 1;
        for ( ; *p >= '0' && *p <= '9'; p++) {
            lans = 10*lans + (*p - '0');
            ndigits += 1;
            expn -= 1;
        }
    }

    if (ndigits == 0) {
        p = str; /* back out */
        ans = NA_REAL;
        goto done;
    }

    if (*p == 'e' || *p == 'E') {
        p += 1;
        int expsign = 1;
        switch (*p) {
        case '-': 
            expsign = -1;
            /* fall through */
        case '+': 
            p += 1;
            break;
        default:
            break;
        }
        n = 0;
        for ( ; *p >= '0' && *p <= '9'; p++) {
            n = n * 10 + (*p - '0');
            if (n > MAX_EXPONENT_PREFIX) n = MAX_EXPONENT_PREFIX;
        }
        expn += expsign * n;
    }

    /* avoid unnecessary underflow for large negative exponents */
    long double p10 = 10.0, fac = 1.0;
    if (expn + ndigits < -300) {
        for (n = 0; n < ndigits; n++) lans /= 10.0;  /* inaccurate */
        expn += ndigits;
    }
    if (expn < -307) { /* use underflow, not overflow */
        for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
            if (n & 1) fac /= p10;  /* inaccurate */
        lans *= fac;
    } else if (expn < 0) { /* positive powers are exact (up to a point...) */
        for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10)
            if (n & 1) fac *= p10;
        lans /= fac;
    } else if (lans != 0.0) {  /* PR#15976:  allow big exponents on 0 */
        for (n = expn, fac = 1.0; n; n >>= 1, p10 *= p10)
            if (n & 1) fac *= p10;
        lans *= fac;
    }
    ans = (double) lans;  /* double rounding problem here */

done:
    if (endptr) *endptr = (char *) p;
    return sign * ans;
}

double R_strtod(const char *str, char **endptr)
{
    return R_strtod4(str, endptr, '.', FALSE);
}

double R_atof(const char *str)
{
    return R_strtod4(str, NULL, '.', FALSE);
}

/* enc2native and enc2utf8, but they are the same in a UTF-8 locale */
/* primitive */
static SEXP do_enc2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, el;
    int i;
    Rboolean duped = FALSE;

    checkArity(op, args);
    check1arg_x (args, call);

    if (!isString(CAR(args)))
	errorcall(call, "argumemt is not a character vector");
    ans = CAR(args);
    for (i = 0; i < LENGTH(ans); i++) {
	el = STRING_ELT(ans, i);
	if (el == NA_STRING) { /* do nothing */ }
	else if(PRIMVAL(op) && !known_to_be_utf8) { /* enc2utf8 */
	    if(!IS_UTF8(el) && !IS_ASCII(el)) {
		if (!duped) { PROTECT(ans = duplicate(ans)); duped = TRUE; }
		SET_STRING_ELT(ans, i, 
			       mkCharCE(translateCharUTF8(el), CE_UTF8));
	    }
	} else { /* enc2native */
	    if((known_to_be_latin1 && IS_UTF8(el)) ||
	       (known_to_be_utf8 && IS_LATIN1(el)) ||
	       ENC_KNOWN(el)) {
		if (!duped) { PROTECT(ans = duplicate(ans)); duped = TRUE; }
		SET_STRING_ELT(ans, i, mkChar(translateChar(el)));
	    }
	}
    }
    if(duped) UNPROTECT(1);
    return ans;
}

#ifdef USE_ICU
# include <locale.h>
#ifdef USE_ICU_APPLE
/* Mac OS X is missing the headers */
typedef int UErrorCode; /* really an enum these days */
struct UCollator;
typedef struct UCollator UCollator;

typedef enum {
  UCOL_EQUAL    = 0,
  UCOL_GREATER    = 1,
  UCOL_LESS    = -1
} UCollationResult ;

typedef enum {
  UCOL_DEFAULT = -1,
  UCOL_PRIMARY = 0,
  UCOL_SECONDARY = 1,
  UCOL_TERTIARY = 2,
  UCOL_DEFAULT_STRENGTH = UCOL_TERTIARY,
  UCOL_CE_STRENGTH_LIMIT,
  UCOL_QUATERNARY=3,
  UCOL_IDENTICAL=15,
  UCOL_STRENGTH_LIMIT,
  UCOL_OFF = 16,
  UCOL_ON = 17,
  UCOL_SHIFTED = 20,
  UCOL_NON_IGNORABLE = 21,
  UCOL_LOWER_FIRST = 24,
  UCOL_UPPER_FIRST = 25,
  UCOL_ATTRIBUTE_VALUE_COUNT
} UColAttributeValue;

typedef UColAttributeValue UCollationStrength;

typedef enum {
      UCOL_FRENCH_COLLATION,
      UCOL_ALTERNATE_HANDLING,
      UCOL_CASE_FIRST,
      UCOL_CASE_LEVEL,
      UCOL_NORMALIZATION_MODE,
      UCOL_DECOMPOSITION_MODE = UCOL_NORMALIZATION_MODE,
      UCOL_STRENGTH,
      UCOL_HIRAGANA_QUATERNARY_MODE,
      UCOL_NUMERIC_COLLATION,
      UCOL_ATTRIBUTE_COUNT
} UColAttribute;

/* UCharIterator struct has to be defined since we use its instances as
   local variables, but we don't actually use any of its members. */
typedef struct UCharIterator {
  const void *context;
  int32_t length, start, index, limit, reservedField;
  void *fns[16]; /* we overshoot here (there is just 10 fns in ICU 3.6),
		    but we have to make sure that enough stack space
		    is allocated when used as a local var in future
		    versions */
} UCharIterator;

UCollator* ucol_open(const char *loc, UErrorCode *status);
void ucol_close(UCollator *coll);
void ucol_setAttribute(UCollator *coll, UColAttribute attr,
		       UColAttributeValue value, UErrorCode *status);
void ucol_setStrength(UCollator *coll, UCollationStrength strength);
UCollationResult ucol_strcollIter(const UCollator *coll,
				  UCharIterator *sIter,
				  UCharIterator *tIter,
				  UErrorCode *status);
void uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length);

void uloc_setDefault(const char* localeID, UErrorCode* status);

#define U_ZERO_ERROR 0
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)

#else
#include <unicode/utypes.h>
#include <unicode/ucol.h>
#include <unicode/uloc.h>
#include <unicode/uiter.h>
#endif

/* NULL if not using ICU. */
static UCollator *collator = NULL;

/* 0 - not set, 1 = use strcoll if not ICU, 2 = use strcmp if not ICU. */
static int collationLocaleSet = 0;

/* called from platform.c */
void attribute_hidden resetICUcollator(void)
{
    if (collator) ucol_close(collator);
    collator = NULL;
    collationLocaleSet = 0;
}

static const struct {
    const char * const str;
    int val;
} ATtable[] = {
    { "case_first", UCOL_CASE_FIRST },
    { "upper", UCOL_UPPER_FIRST },
    { "lower", UCOL_LOWER_FIRST },
    { "default ", UCOL_DEFAULT },
    { "strength", 999 },
    { "primary ", UCOL_PRIMARY },
    { "secondary ", UCOL_SECONDARY },
    { "teritary ", UCOL_TERTIARY },
    { "guaternary ", UCOL_QUATERNARY },
    { "identical ", UCOL_IDENTICAL },
    { "french_collation", UCOL_FRENCH_COLLATION },
    { "on", UCOL_ON },
    { "off", UCOL_OFF },
    { "normalization", UCOL_NORMALIZATION_MODE },
    { "alternate_handling", UCOL_ALTERNATE_HANDLING },
    { "non_ignorable", UCOL_NON_IGNORABLE },
    { "shifted", UCOL_SHIFTED },
    { "case_level", UCOL_CASE_LEVEL },
    { "hiragana_quaternary", UCOL_HIRAGANA_QUATERNARY_MODE },
    { NULL,  0 }
};


#ifdef Win32
#define BUFFER_SIZE 512
typedef int (WINAPI *PGSDLN)(LPWSTR, int);

static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    if (p && p[0]) return p;

    // This call is >= Vista/Server 2008
    // ICU should accept almost all of these, e.g. en-US and uz-Latn-UZ
    PGSDLN pGSDLN = (PGSDLN)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
                       "GetSystemDefaultLocaleName");
    if(pGSDLN) {
        WCHAR wcBuffer[BUFFER_SIZE];
        pGSDLN(wcBuffer, BUFFER_SIZE);
        static char locale[BUFFER_SIZE];
        WideCharToMultiByte(CP_ACP, 0, wcBuffer, -1,
                            locale, BUFFER_SIZE, NULL, NULL);
        return locale;
    } else return "root";
}
#else
static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    return (p && p[0]) ? p : setlocale(LC_COLLATE, NULL);
}
#endif

static SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    UErrorCode  status = U_ZERO_ERROR;

    for (; args != R_NilValue; args = CDR(args)) {
        if (isNull(TAG(args))) error(_("all arguments must be named"));
        const char *this = CHAR(PRINTNAME(TAG(args)));
        const char *s;

        x = CAR(args);
        if (!isString(x) || LENGTH(x) != 1)
            error(_("invalid '%s' argument"), this);
        s = CHAR(STRING_ELT(x, 0));
        if (streql(this, "locale")) {
            if (collator) {
                ucol_close(collator);
                collator = NULL;
            }
            if (streql(s, "ASCII")) {
                collationLocaleSet = 2;
            } 
            else {
                const char *loc = getLocale();
                if (strcmp(loc,"C") == 0 || strcmp(loc,"POSIX") == 0)
                    collationLocaleSet = 2;
                else
                    collationLocaleSet = 1;
                if (strcmp(s,"none") != 0) {
                    collationLocaleSet = 1;
                    if (streql(s, "default"))
                        uloc_setDefault(getLocale(), &status);
                    else 
                        uloc_setDefault(s, &status);
                    if (U_FAILURE(status))
                        error("failed to set ICU locale %s (%d)", s, status);
                    collator = ucol_open(NULL, &status);
                    if (U_FAILURE(status)) {
                        collator = NULL;
                        error("failed to open ICU collator (%d)", status);
                    }
                }
            }
        }
        else {
            int i, at = -1, val = -1;
            for (i = 0; ATtable[i].str; i++)
                if (streql(this, ATtable[i].str)) {
                    at = ATtable[i].val;
                    break;
                }
            for (i = 0; ATtable[i].str; i++)
                if (streql(s, ATtable[i].str)) {
                    val = ATtable[i].val;
                    break;
                }
            if (collator && at == 999 && val >= 0) {
                ucol_setStrength(collator, val);
            } else if (collator && at >= 0 && val >= 0) {
                ucol_setAttribute(collator, at, val, &status);
                if (U_FAILURE(status))
                    error("failed to set ICU collator attribute");
            }
        }
    }

    return R_NilValue;
}

static SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *ans = "unknown", *res;
    checkArity(op, args);

    if (collationLocaleSet == 2)
        ans = "ASCII";
    else if (collator == NULL)
        ans = "ICU not in use";
    else {
        UErrorCode  status = U_ZERO_ERROR;
        int type = asInteger(CAR(args));
        if (type < 1 || type > 2)
            error(_("invalid '%s' value"), "type");

        res = ucol_getLocaleByType (collator,
                  type == 1 ? ULOC_ACTUAL_LOCALE : ULOC_VALID_LOCALE,
                  &status);
        if(!U_FAILURE(status) && res) ans = res;
    } 

    return mkString(ans);
}

/* Calls 'error' if can't collate.

   NB: strings can have equal collation weight without being identical */

attribute_hidden int Scollate(SEXP a, SEXP b)
{
    void *vmax = VMAXGET();
    int result;

    if (collationLocaleSet == 0) {
        const char *loc = getLocale();
        collator = NULL;
        if (strcmp(loc,"C") == 0 || strcmp(loc,"POSIX") == 0)
            collationLocaleSet = 2;
        else {
            collationLocaleSet = 1;
#ifdef Win32
            const char *p = getenv("R_ICU_LOCALE");
            if (p && p[0])
#endif
            {
                UErrorCode status = U_ZERO_ERROR;
                uloc_setDefault(getLocale(), &status);
                if (U_FAILURE(status))
                    error("failed to set ICU locale (%d)", status);
                collator = ucol_open(NULL, &status);
                if (U_FAILURE(status)) {
                    collator = NULL;
                    error("failed to open ICU collator (%d)", status);
               }
            }
        }
    }

    if (collator == NULL) {
        const char *at = translateChar(a);
        const char *bt = translateChar(b);
        if (collationLocaleSet != 1)
            result = strcmp (at, bt);
        else {
            errno = 0;
            result = strcoll (at, bt);
            if (errno != 0)
                error("could not collate using strcoll");
        }
    }
    else {    
        UCharIterator aIter, bIter;
        const char *as = translateCharUTF8(a), *bs = translateCharUTF8(b);
        int len1 = (int) strlen(as), len2 = (int) strlen(bs);
        uiter_setUTF8(&aIter, as, len1);
        uiter_setUTF8(&bIter, bs, len2);
        UErrorCode status = U_ZERO_ERROR;
        result = ucol_strcollIter(collator, &aIter, &bIter, &status);
        if (U_FAILURE(status)) 
            error("could not collate using ICU");
    }

    VMAXSET(vmax);
    return result;
}

#else /* not USE_ICU */

static SEXP do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning(_("ICU is not supported on this build"));
    return R_NilValue;
}

static SEXP do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString("ICU not in use");
}

void attribute_hidden resetICUcollator(void) {}

# ifdef Win32

static int Rstrcoll(const char *s1, const char *s2)
{
    wchar_t w1[strlen(s1)+1], w2[strlen(s2)+1];
    R_CHECKSTACK();
    utf8towcs(w1, s1, strlen(s1));
    utf8towcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

int Scollate(SEXP a, SEXP b)
{
    if(getCharCE(a) == CE_UTF8 || getCharCE(b) == CE_UTF8)
	return Rstrcoll(translateCharUTF8(a), translateCharUTF8(b));
    else
	return strcoll(translateChar(a), translateChar(b));
}

# else
int Scollate(SEXP a, SEXP b)
{
    return strcoll(translateChar(a), translateChar(b));
}

# endif
#endif

#include <lzma.h>

SEXP crc64ToString(SEXP in)
{
    uint64_t crc = 0;
    char ans[17];
    if (!isString(in)) error("input must be a character string");
    const char *str = CHAR(STRING_ELT(in, 0));

    /* Seems this is realy 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return mkString(ans);
}

#ifdef CHECK_VEC_NONVEC

struct sxpinfo_struct *Rf_verify_vec (void *x)
{
    if (! ((VECTOR_OR_CHAR_TYPES >> TYPEOF((SEXP)x)) & 1)) abort();
    return &((SEXP)x)->sxpinfo;
}

struct sxpinfo_struct *Rf_verify_nonvec (void *x)
{
    if ((VECTOR_OR_CHAR_TYPES >> TYPEOF((SEXP)x)) & 1) abort();
    return &((SEXP)x)->sxpinfo;
}

#endif

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_util[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"nargs",	do_nargs,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"merge",	do_merge,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"getwd",	do_getwd,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setwd",	do_setwd,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	do_basename,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dirname",	do_dirname,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"normalizePath",do_normalizepath,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"encodeString",do_encodeString,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"Encoding",	do_encoding,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"setEncoding",	do_setencoding,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2native",	do_enc2,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2utf8",	do_enc2,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuGetCollate",do_ICUget,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
