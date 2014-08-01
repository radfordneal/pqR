/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Development Core Team
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
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

/* The character functions in this file are

nzchar nchar substr substr<- abbreviate tolower toupper chartr strtrim

and the utility 

make.names

The regex functions

strsplit grep [g]sub [g]regexpr agrep

here prior to 2.10.0 are now in grep.c and agrep.c

make.unique, duplicated, unique, match, pmatch, charmatch are in unique.c
iconv is in sysutils.c


Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

Comparison is done directly unless you happen to be comparing the same
string in different encodings.

nzchar and nchar(, "bytes") are independent of the encoding
nchar(, "char") nchar(, "width") handle UTF-8 directly, translate Latin-1
substr substr<-  handle UTF-8 and Latin-1 directly
tolower toupper chartr  translate UTF-8 to wchar, rest to current charset
  which needs Unicode wide characters
abbreviate strtrim  translate

All the string matching functions handle UTF-8 directly, otherwise
translate (latin1 to UTF-8, otherwise to native).

Support for "bytes" marked encoding
===================================

nzchar and nchar(, "bytes") are independent of the encoding.

nchar(, "char") nchar(, "width") give NA (if allowed) or error.
substr substr<-  work in bytes

abbreviate chartr make.names strtrim tolower toupper give error.

*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>
#include <errno.h>

#include <R_ext/RS.h>  /* for Calloc/Free */

#include <R_ext/rlocale.h>

/* We use a shared buffer here to avoid reallocing small buffers, and
   keep a standard-size (MAXELTSIZE = 8192) buffer allocated shared
   between the various functions.

   If we want to make this thread-safe, we would need to initialize an
   instance non-statically in each using function, but this would add
   to the overhead.
 */

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

/* primitive */
static SEXP do_nzchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    int i, len;

    checkArity(op, args);
    check1arg_x (args, call);

    if (isFactor(CAR(args)))
	error(_("'%s' requires a character vector"), "nzchar()");
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	error(_("'%s' requires a character vector"), "nzchar()");
    len = LENGTH(x);
    PROTECT(ans = allocVector(LGLSXP, len));
    for (i = 0; i < len; i++)
	LOGICAL(ans)[i] = LENGTH(STRING_ELT(x, i)) > 0;
    UNPROTECT(2);
    return ans;
}


static SEXP do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x, stype;
    int i, len, allowNA;
    size_t ntype;
    int nc;
    const char *type;
    const char *xi;
    wchar_t *wc;
    const void *vmax;

    checkArity(op, args);
    if (isFactor(CAR(args)))
	error(_("'%s' requires a character vector"), "nchar()");
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	error(_("'%s' requires a character vector"), "nchar()");
    len = LENGTH(x);
    stype = CADR(args);
    if (!isString(stype) || LENGTH(stype) != 1)
	error(_("invalid '%s' argument"), "type");
    type = CHAR(STRING_ELT(stype, 0)); /* always ASCII */
    ntype = strlen(type);
    if (ntype == 0) error(_("invalid '%s' argument"), "type");
    allowNA = asLogical(CADDR(args));
    if (allowNA == NA_LOGICAL) allowNA = 0;

    PROTECT(s = allocVector(INTSXP, len));
    vmax = VMAXGET();
    for (i = 0; i < len; i++) {
	SEXP sxi = STRING_ELT(x, i);
	if (sxi == NA_STRING) {
	    INTEGER(s)[i] = 2;
	    continue;
	}
	if (strncmp(type, "bytes", ntype) == 0) {
	    INTEGER(s)[i] = LENGTH(sxi);
	} else if (strncmp(type, "chars", ntype) == 0) {
	    if (IS_UTF8(sxi)) { /* assume this is valid */
		const char *p = CHAR(sxi);
		nc = 0;
		for( ; *p; p += utf8clen(*p)) nc++;
		INTEGER(s)[i] = nc;
	    } else if (IS_BYTES(sxi)) {
		if (!allowNA) /* could do chars 0 */
		    error(_("number of characters is not computable for element %d in \"bytes\" encoding"), i+1);
		INTEGER(s)[i] = NA_INTEGER;
	    } else if (mbcslocale) {
		nc = mbstowcs(NULL, translateChar(sxi), 0);
		if (!allowNA && nc < 0)
		    error(_("invalid multibyte string %d"), i+1);
		INTEGER(s)[i] = nc >= 0 ? nc : NA_INTEGER;
	    } else
		INTEGER(s)[i] = strlen(translateChar(sxi));
	} else if (strncmp(type, "width", ntype) == 0) {
	    if (IS_UTF8(sxi)) { /* assume this is valid */
		const char *p = CHAR(sxi);
		wchar_t wc1;
		nc = 0;
		for( ; *p; p += utf8clen(*p)) {
		    utf8toucs(&wc1, p);
		    nc += Ri18n_wcwidth(wc1);
		}
		INTEGER(s)[i] = nc;
	    } else if (IS_BYTES(sxi)) {
		if (!allowNA) /* could do width 0 */
		    error(_("width is not computable for element %d in \"bytes\" encoding"), i+1);
		INTEGER(s)[i] = NA_INTEGER;
	    } else if (mbcslocale) {
		xi = translateChar(sxi);
		nc = mbstowcs(NULL, xi, 0);
		if (nc >= 0) {
		    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);

		    mbstowcs(wc, xi, nc + 1);
		    INTEGER(s)[i] = Ri18n_wcswidth(wc, 2147483647);
		    if (INTEGER(s)[i] < 1) INTEGER(s)[i] = nc;
		} else if (allowNA)
		    error(_("invalid multibyte string %d"), i+1);
		else
		    INTEGER(s)[i] = NA_INTEGER;
	    } else
		INTEGER(s)[i] = strlen(translateChar(sxi));
	} else
	    error(_("invalid '%s' argument"), "type");
	VMAXSET(vmax);
    }
    R_FreeStringBufferL(&cbuff);
    if ((d = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	setAttrib(s, R_NamesSymbol, d);
    if ((d = getAttrib(x, R_DimSymbol)) != R_NilValue)
	setAttrib(s, R_DimSymbol, d);
    if ((d = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	setAttrib(s, R_DimNamesSymbol, d);
    UNPROTECT(2);
    return s;
}

/* Find the beginning and end (indexes start at 0) of the substring in str
   from sa to so (indexes start at 1).  If so is beyond the end of str, 
   the end is just past the last character of str.  Returns the number of
   characters (not necessarily same as bytes) in the substring (which will
   be so - sa + 1 or fewer).

   Tries to fudge something if the last character extends beyond slen
   (only possible in a string with an invalid multibyte character). */

static int find_substr (const char *str, size_t slen, int ienc, 
                        int sa, int so, size_t *beginning, size_t *end)
{
    int i, n; 
    size_t j;

    *beginning = *end = slen;

    if (ienc == CE_UTF8) {
        for (i = 1, j = 0; i < sa; i++, j += utf8clen(str[j])) {
            if (j >= slen) return 0;
        }
        *beginning = j;
        while (i <= so) {
            j += utf8clen(str[j]);
            if (j > slen) return i - sa;
            i += 1;
            if (j == slen) return i - sa;
        }
        *end = j;
    }
    else if (ienc!=CE_LATIN1 && ienc!=CE_BYTES 
              && mbcslocale && !strIsASCII(str)) {
        mbstate_t mb_st;
        mbs_init(&mb_st);
        for (i = 1, j = 0; i < sa; 
                           i++, j += Mbrtowc(NULL,str+j,MB_CUR_MAX,&mb_st)) {
            if (j >= slen) return 0;
        }
        *beginning = j;
        while (i <= so) {
            j += Mbrtowc(NULL,str+j,MB_CUR_MAX,&mb_st);
            if (j > slen) return i - sa;
            i += 1;
            if (j == slen) return i - sa;
        }
        *end = j;
    }
    else {
        if (sa > slen) return 0;
        *beginning = sa-1;
        if (so > slen) return slen - sa + 1;
        *end = so;
    }

    return so - sa + 1; 
}

static SEXP do_substr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so, el;
    int i, len, start, stop, k, l;
    size_t slen;
    cetype_t ienc;
    const char *ss;
    char *buf;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if (!isString(x))
	error(_("extracting substrings from a non-character object"));
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if (len > 0) {
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    error(_("invalid substring argument(s)"));

	for (i = 0; i < len; i++) {
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    el = STRING_ELT(x,i);
	    if (el == NA_STRING || start == NA_INTEGER || stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    ienc = getCharCE(el);
	    ss = CHAR(el);
	    slen = LENGTH(el);
	    if (start < 1) start = 1;
	    if (start > stop)
                buf = "";
	    else {
                size_t beginning, end;
		(void) find_substr (ss, slen, ienc, start, stop, 
                                    &beginning, &end);
                buf = R_AllocStringBuffer (end-beginning, &cbuff);
                memcpy (buf, ss+beginning, end-beginning);
                buf[end-beginning] = 0;
            }
	    SET_STRING_ELT(s, i, mkCharCE(buf, ienc));
	}
	R_FreeStringBufferL(&cbuff);
    }
    DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

static SEXP do_substrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so, value, el, v_el;
    int i, len, start, stop, k, l, v;
    size_t slen;
    cetype_t ienc, venc;
    const char *ss, *v_ss;
    char *buf;
    const void *vmax;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    value = CADDDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if (!isString(x))
	error(_("replacing substrings in a non-character object"));
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if (len > 0) {
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    error(_("invalid substring argument(s)"));

	v = LENGTH(value);
	if (!isString(value) || v == 0) error(_("invalid value"));
	
	vmax = VMAXGET();
	for (i = 0; i < len; i++) {
	    el = STRING_ELT(x, i);
	    v_el = STRING_ELT(value, i % v);
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    if (el == NA_STRING || v_el == NA_STRING ||
		start == NA_INTEGER || stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    ienc = getCharCE(el);
	    ss = CHAR(el);
	    slen = LENGTH(el);
	    if (start < 1) start = 1;
	    if (start > stop) {
                SET_STRING_ELT(s, i, STRING_ELT(x, i));
                continue;
            }
            int ienc2 = ienc;
            v_ss = CHAR(v_el);
            /* is the value in the same encoding?
               FIXME: could prefer UTF-8 here
             */
            venc = getCharCE(v_el);
            if (venc != ienc && !strIsASCII(v_ss)) {
                ss = translateChar(el);
                slen = strlen(ss);
                v_ss = translateChar(v_el);
                ienc2 = CE_NATIVE;
            }
            size_t ss_b, ss_e, v_ss_l, v_ss_b, v_ss_e;
            int n, v_n;
            v_ss_l = strlen(v_ss);
            n = find_substr (ss, slen, ienc2, start, stop, &ss_b, &ss_e);
            v_n = find_substr (v_ss, v_ss_l, ienc2, 1, n, &v_ss_b, &v_ss_e);
                  /* note: v_ss_b will be set to zero */
            if (v_n != n)
                n = find_substr (ss, slen, ienc2, start, start+v_n-1, 
                                 &ss_b, &ss_e);
            size_t new_len = slen - (ss_e-ss_b) + v_ss_e;
            if (new_len > INT_MAX) 
                error(_("new string is too long"));
            buf = R_AllocStringBuffer (new_len, &cbuff);
            if (ss_b > 0) memcpy (buf, ss, ss_b);
            memcpy (buf+ss_b, v_ss, v_ss_e);
            if (ss_e < slen) memcpy (buf+ss_b+v_ss_e, ss+ss_e, slen-ss_e);
            buf[new_len] = 0;
            SET_STRING_ELT(s, i, mkCharCE(buf, ienc2));
            VMAXSET(vmax);
        }
	R_FreeStringBufferL(&cbuff);
    }
    UNPROTECT(1);
    return s;
}

/* Abbreviate
   long names in the S-designated fashion:
   1) spaces
   2) lower case vowels
   3) lower case consonants
   4) upper case letters
   5) special characters.

   Letters are dropped from the end of words
   and at least one letter is retained from each word.

   If unique abbreviations are not produced letters are added until the
   results are unique (duplicated names are removed prior to entry).
   names, minlength, use.classes, dot
*/


#define FIRSTCHAR(i) (isspace((int)buff1[i-1]))
#define LASTCHAR(i) (!isspace((int)buff1[i-1]) && (!buff1[i+1] || isspace((int)buff1[i+1])))
#define LOWVOW(i) (buff1[i] == 'a' || buff1[i] == 'e' || buff1[i] == 'i' || \
		   buff1[i] == 'o' || buff1[i] == 'u')


/* memmove does allow overlapping src and dest */
static void mystrcpy(char *dest, const char *src)
{
    memmove(dest, src, strlen(src)+1);
}


/* abbreviate(inchar, minlen) */
static SEXP stripchars(const char * const inchar, int minlen)
{
/* This routine used to use strcpy with overlapping dest and src.
   That is not allowed by ISO C.
 */
    int i, j, nspace = 0, upper;
    char *buff1 = cbuff.data;

    mystrcpy(buff1, inchar);
    upper = strlen(buff1)-1;

    /* remove leading blanks */
    j = 0;
    for (i = 0 ; i < upper ; i++)
	if (isspace((int)buff1[i]))
	    j++;
	else
	    break;

    mystrcpy(buff1, &buff1[j]);
    upper = strlen(buff1) - 1;

    if (strlen(buff1) < minlen)
	goto donesc;

    for (i = upper, j = 1; i > 0; i--) {
	if (isspace((int)buff1[i])) {
	    if (j)
		buff1[i] = '\0' ;
	    else
		nspace++;
	}
	else
	    j = 0;
	/*strcpy(buff1[i],buff1[i+1]);*/
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (LOWVOW(i) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (LOWVOW(i) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    /* all else has failed so we use brute force */

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (!FIRSTCHAR(i) && !isspace((int)buff1[i]))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

donesc:

    upper = strlen(buff1);
    if (upper > minlen)
	for (i = upper - 1; i > 0; i--)
	    if (isspace((int)buff1[i]))
		mystrcpy(&buff1[i], &buff1[i + 1]);

    return(mkChar(buff1));
}


static SEXP do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    int i, len, minlen;
    Rboolean warn = FALSE;
    const char *s;
    const void *vmax;

    checkArity(op,args);
    x = CAR(args);

    if (!isString(x))
	error(_("the first argument must be a character vector"));
    len = length(x);

    PROTECT(ans = allocVector(STRSXP, len));
    minlen = asInteger(CADR(args));
    vmax = VMAXGET();
    for (i = 0 ; i < len ; i++) {
	if (STRING_ELT(x, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    s = translateChar(STRING_ELT(x, i));
	    warn = warn | !strIsASCII(s);
	    R_AllocStringBuffer(strlen(s), &cbuff);
	    SET_STRING_ELT(ans, i, stripchars(s, minlen));
	}
	VMAXSET(vmax);
    }
    if (warn) warning(_("abbreviate used with non-ASCII chars"));
    DUPLICATE_ATTRIB(ans, x);
    /* This copied the class, if any */
    R_FreeStringBufferL(&cbuff);
    UNPROTECT(1);
    return(ans);
}

static SEXP do_makenames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, ans;
    int i, l, n, allow_;
    char *p, *tmp = NULL, *cbuf;
    const char *This;
    Rboolean need_prefix;
    const void *vmax;

    checkArity(op ,args);
    arg = CAR(args);
    if (!isString(arg))
	error(_("non-character names"));
    n = length(arg);
    allow_ = asLogical(CADR(args));
    if (allow_ == NA_LOGICAL)
	error(_("invalid '%s' value"), "allow_");
    PROTECT(ans = allocVector(STRSXP, n));
    vmax = VMAXGET();
    for (i = 0 ; i < n ; i++) {
	This = translateChar(STRING_ELT(arg, i));
	l = strlen(This);
	/* need to prefix names not beginning with alpha or ., as
	   well as . followed by a number */
	need_prefix = FALSE;
	if (mbcslocale && This[0]) {
	    int nc = l, used;
	    wchar_t wc;
	    mbstate_t mb_st;
	    const char *pp = This;
	    mbs_init(&mb_st);
	    used = Mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st);
	    pp += used; nc -= used;
	    if (wc == L'.') {
		if (nc > 0) {
		    Mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st);
		    if (iswdigit(wc))  need_prefix = TRUE;
		}
	    } else if (!iswalpha(wc)) need_prefix = TRUE;
	} else {
	    if (This[0] == '.') {
		if (l >= 1 && isdigit(0xff & (int) This[1])) need_prefix = TRUE;
	    } else if (!isalpha(0xff & (int) This[0])) need_prefix = TRUE;
	}
	if (need_prefix) {
	    tmp = Calloc(l+2, char);
	    strcpy(tmp, "X");
	    strcat(tmp, translateChar(STRING_ELT(arg, i)));
	} else {
	    tmp = Calloc(l+1, char);
	    strcpy(tmp, translateChar(STRING_ELT(arg, i)));
	}
	if (mbcslocale) {
	    /* This cannot lengthen the string, so safe to overwrite it.
	       Would also be possible a char at a time.
	     */
	    int nc = mbstowcs(NULL, tmp, 0);
	    wchar_t *wstr = Calloc(nc+1, wchar_t), *wc;
	    if (nc >= 0) {
		mbstowcs(wstr, tmp, nc+1);
		for (wc = wstr; *wc; wc++) {
		    if (*wc == L'.' || (allow_ && *wc == L'_'))
			/* leave alone */;
		    else if (!iswalnum((int)*wc)) *wc = L'.';
		    /* If it changes into dot here,
		     * length will become short on mbcs.
		     * The name which became short will contain garbage.
		     * cf.
		     *   >  make.names(c("\u30fb"))
		     *   [1] "X.\0"
		     */
		}
		wcstombs(tmp, wstr, strlen(tmp)+1);
		Free(wstr);
	    } else error(_("invalid multibyte string %d"), i+1);
	} else {
	    for (p = tmp; *p; p++) {
		if (*p == '.' || (allow_ && *p == '_')) /* leave alone */;
		else if (!isalnum(0xff & (int)*p)) *p = '.';
		/* else leave alone */
	    }
	}
	l = strlen(tmp);        /* needed? */
	SET_STRING_ELT(ans, i, mkChar(tmp));
	/* do we have a reserved word?  If so the name is invalid */
	if (!isValidName(tmp)) {
	    /* FIXME: could use R_Realloc instead */
	    cbuf = CallocCharBuf(strlen(tmp) + 1);
	    strcpy(cbuf, tmp);
	    strcat(cbuf, ".");
	    SET_STRING_ELT(ans, i, mkChar(cbuf));
	    Free(cbuf);
	}
	Free(tmp);
	VMAXSET(vmax);
    }
    UNPROTECT(1);
    return ans;
}


static SEXP do_tolower(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    int i, n, ul;
    char *p;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    checkArity(op, args);
    ul = PRIMVAL(op); /* 0 = tolower, 1 = toupper */

    x = CAR(args);
    /* coercion is done in wrapper */
    if (!isString(x)) error(_("non-character argument"));
    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE_CC__)
    /* utf8towcs is really to UCS-4/2 */
    for (i = 0; i < n; i++)
	if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;
    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int nb, nc, j;
	wctrans_t tr = wctrans(ul ? "toupper" : "tolower");
	wchar_t * wc;
	char * cbuf;

	vmax = VMAXGET();
	/* the translated string need not be the same length in bytes */
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x, i);
	    if (el == NA_STRING) SET_STRING_ELT(y, i, NA_STRING);
	    else {
		const char *xi;
		ienc = getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = CHAR(el);
		    nc = utf8towcs(NULL, xi, 0);
		} else {
		    xi = translateChar(el);
		    nc = mbstowcs(NULL, xi, 0);
		    ienc = CE_NATIVE;
		}
		if (nc >= 0) {
		    /* FIXME use this buffer for new string as well */
		    wc = (wchar_t *)
			R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
		    if (ienc == CE_UTF8) {
			utf8towcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = wcstoutf8(NULL, wc, 0);
			cbuf = CallocCharBuf(nb);
			wcstoutf8(cbuf, wc, nb + 1);
			SET_STRING_ELT(y, i, mkCharCE(cbuf, CE_UTF8));
		    } else {
			mbstowcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = wcstombs(NULL, wc, 0);
			cbuf = CallocCharBuf(nb);
			wcstombs(cbuf, wc, nb + 1);
			SET_STRING_ELT(y, i, markKnown(cbuf, el));
		    }
		    Free(cbuf);
		} else {
		    error(_("invalid multibyte string %d"), i+1);
		}
	    }
	    VMAXSET(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	char *xi;
	vmax = VMAXGET();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x, i) == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		xi = CallocCharBuf(strlen(CHAR(STRING_ELT(x, i))));
		strcpy(xi, translateChar(STRING_ELT(x, i)));
		for (p = xi; *p != '\0'; p++)
		    *p = ul ? toupper(*p) : tolower(*p);
		SET_STRING_ELT(y, i, markKnown(xi, STRING_ELT(x, i)));
		Free(xi);
	    }
	    VMAXSET(vmax);
	}
    }
    DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}


typedef enum { WTR_INIT, WTR_CHAR, WTR_RANGE } wtr_type;
struct wtr_spec {
    wtr_type type;
    struct wtr_spec *next;
    union {
	wchar_t c;
	struct {
	    wchar_t first;
	    wchar_t last;
	} r;
    } u;
};

static void
wtr_build_spec(const wchar_t *s, struct wtr_spec *trs) {
    int i, len = wcslen(s);
    struct wtr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = NULL;
	if (s[i + 1] == L'-') {
	    _new->type = WTR_RANGE;
	    if (s[i] > s[i + 2])
		error(_("decreasing range specification ('%lc-%lc')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = WTR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = NULL;
	_new->type = WTR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
wtr_free_spec(struct wtr_spec *trs) {
    struct wtr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static wchar_t
wtr_get_next_char_from_spec(struct wtr_spec **p) {
    wchar_t c;
    struct wtr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the WTR_INIT case. */
    case WTR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case WTR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = L'\0';
	break;
    }
    return(c);
}

typedef enum { TR_INIT, TR_CHAR, TR_RANGE } tr_spec_type;
struct tr_spec {
    tr_spec_type type;
    struct tr_spec *next;
    union {
	unsigned char c;
	struct {
	    unsigned char first;
	    unsigned char last;
	} r;
    } u;
};

static void
tr_build_spec(const char *s, struct tr_spec *trs) {
    int i, len = strlen(s);
    struct tr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct tr_spec);
	_new->next = NULL;
	if (s[i + 1] == '-') {
	    _new->type = TR_RANGE;
	    if (s[i] > s[i + 2])
		error(_("decreasing range specification ('%c-%c')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = TR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct tr_spec);
	_new->next = NULL;
	_new->type = TR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
tr_free_spec(struct tr_spec *trs) {
    struct tr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static unsigned char
tr_get_next_char_from_spec(struct tr_spec **p) {
    unsigned char c;
    struct tr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the TR_INIT case. */
    case TR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case TR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = '\0';
	break;
    }
    return(c);
}

typedef struct { wchar_t c_old, c_new; } xtable_t;

static R_INLINE int xtable_comp(const void *a, const void *b)
{
    return ((xtable_t *)a)->c_old - ((xtable_t *)b)->c_old;
}

static R_INLINE int xtable_key_comp(const void *a, const void *b)
{
    return *((wchar_t *)a) - ((xtable_t *)b)->c_old;
}

#define SWAP(_a, _b, _TYPE)                                    \
{                                                              \
    _TYPE _t;                                                  \
    _t    = *(_a);                                             \
    *(_a) = *(_b);                                             \
    *(_b) = _t;                                                \
}

#define ISORT(_base,_num,_TYPE,_comp)                          \
{                                                              \
/* insert sort */                                              \
/* require stable data */                                      \
    int _i, _j ;                                               \
    for ( _i = 1 ; _i < _num ; _i++ )                          \
	for ( _j = _i; _j > 0 &&                               \
		      (*_comp)(_base+_j-1, _base+_j)>0; _j--)  \
	   SWAP(_base+_j-1, _base+_j, _TYPE);                  \
}

#define COMPRESS(_base,_num,_TYPE,_comp)                       \
{                                                              \
/* supress even c_old. last use */                             \
    int _i,_j ;                                                \
    for ( _i = 0 ; _i < (*(_num)) - 1 ; _i++ ){                \
	int rc = (*_comp)(_base+_i, _base+_i+1);               \
	if (rc == 0){                                          \
	   for ( _j = _i, _i-- ; _j < (*(_num)) - 1; _j++ )     \
		*((_base)+_j) = *((_base)+_j+1);               \
	    (*(_num))--;                                       \
	}                                                      \
    }                                                          \
}

#define BSEARCH(_rc,_key,_base,_nmemb,_TYPE,_comp)             \
{                                                              \
    size_t l, u, idx;                                          \
    _TYPE *p;                                                  \
    int comp;                                                  \
    l = 0;                                                     \
    u = _nmemb;                                                \
    _rc = NULL;                                                \
    while (l < u)                                              \
    {                                                          \
	idx = (l + u) / 2;                                     \
	p =  (_base) + idx;                                    \
	comp = (*_comp)(_key, p);                              \
	if (comp < 0)                                          \
	    u = idx;                                           \
	else if (comp > 0)                                     \
	    l = idx + 1;                                       \
	else{                                                  \
	  _rc = p;                                             \
	  break;                                               \
	}                                                      \
    }                                                          \
}

static SEXP do_chartr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP old, _new, x, y;
    int i, n;
    char *cbuf;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    checkArity(op, args);
    old = CAR(args); args = CDR(args);
    _new = CAR(args); args = CDR(args);
    x = CAR(args);
    n = LENGTH(x);
    if (!isString(old) || length(old) < 1 || STRING_ELT(old, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "old");
    if (length(old) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "old");
    if (!isString(_new) || length(_new) < 1 || STRING_ELT(_new, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "new");
    if (length(_new) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "new");
    if (!isString(x)) error("invalid '%s' argument", "x");

    /* utf8towcs is really to UCS-4/2 */
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE_CC__)
    for (i = 0; i < n; i++)
	if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;

    if (getCharCE(STRING_ELT(old, 0)) == CE_UTF8) use_UTF8 = TRUE;
    if (getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) use_UTF8 = TRUE;

    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int j, nb, nc;
	xtable_t *xtable, *tbl;
	int xtable_cnt;
	struct wtr_spec *trs_cnt, **trs_cnt_ptr;
	wchar_t c_old, c_new, *wc;
	const char *xi, *s;
	struct wtr_spec *trs_old, **trs_old_ptr;
	struct wtr_spec *trs_new, **trs_new_ptr;

	/* Initialize the old and new wtr_spec lists. */
	trs_old = Calloc(1, struct wtr_spec);
	trs_old->type = WTR_INIT;
	trs_old->next = NULL;
	trs_new = Calloc(1, struct wtr_spec);
	trs_new->type = WTR_INIT;
	trs_new->next = NULL;
	/* Build the old and new wtr_spec lists. */
	if (use_UTF8 && getCharCE(STRING_ELT(old, 0)) == CE_UTF8) {
	    s = CHAR(STRING_ELT(old, 0));
	    nc = utf8towcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid UTF-8 string 'old'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    utf8towcs(wc, s, nc + 1);
	} else {
	    s = translateChar(STRING_ELT(old, 0));
	    nc = mbstowcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid multibyte string 'old'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_old);
	trs_cnt = Calloc(1, struct wtr_spec);
	trs_cnt->type = WTR_INIT;
	trs_cnt->next = NULL;
	wtr_build_spec(wc, trs_cnt); /* use count only */

	if (use_UTF8 && getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) {
	    s = CHAR(STRING_ELT(_new, 0));
	    nc = utf8towcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid UTF-8 string 'new'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    utf8towcs(wc, s, nc + 1);
	} else {
	    s = translateChar(STRING_ELT(_new, 0));
	    nc = mbstowcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid multibyte string 'new'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_new);

	/* Initialize the pointers for walking through the old and new
	   wtr_spec lists and retrieving the next chars from the lists.
	*/

	trs_cnt_ptr = Calloc(1, struct wtr_spec *);
	*trs_cnt_ptr = trs_cnt->next;
	for (xtable_cnt = 0 ; wtr_get_next_char_from_spec(trs_cnt_ptr); 
	      xtable_cnt++) ;
	wtr_free_spec(trs_cnt);
	Free(trs_cnt_ptr);
	xtable = (xtable_t *) R_alloc(xtable_cnt+1, sizeof(xtable_t));

	trs_old_ptr = Calloc(1, struct wtr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct wtr_spec *);
	*trs_new_ptr = trs_new->next;
	for (i = 0; ; i++) {
	    c_old = wtr_get_next_char_from_spec(trs_old_ptr);
	    c_new = wtr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		error(_("'old' is longer than 'new'"));
	    else {
		xtable[i].c_old = c_old;
		xtable[i].c_new = c_new;
	    }
	}

	/* Free the memory occupied by the wtr_spec lists. */
	wtr_free_spec(trs_old);
	wtr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	ISORT(xtable, xtable_cnt, xtable_t , xtable_comp);
	COMPRESS(xtable, &xtable_cnt, xtable_t, xtable_comp);

	PROTECT(y = allocVector(STRSXP, n));
	vmax = VMAXGET();
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x,i);
	    if (el == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		ienc = getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = CHAR(el);
		    nc = utf8towcs(NULL, xi, 0);
		} else {
		    xi = translateChar(el);
		    nc = mbstowcs(NULL, xi, 0);
		    ienc = CE_NATIVE;
		}
		if (nc < 0)
		    error(_("invalid input multibyte string %d"), i+1);
		wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t),
						     &cbuff);
		if (ienc == CE_UTF8) utf8towcs(wc, xi, nc + 1);
		else mbstowcs(wc, xi, nc + 1);
		for (j = 0; j < nc; j++){
		    BSEARCH(tbl,&wc[j], xtable, xtable_cnt,
			    xtable_t, xtable_key_comp);
		    if (tbl) wc[j] = tbl->c_new;
		}
		if (ienc == CE_UTF8) {
		    nb = wcstoutf8(NULL, wc, 0);
		    cbuf = CallocCharBuf(nb);
		    wcstoutf8(cbuf, wc, nb + 1);
		    SET_STRING_ELT(y, i, mkCharCE(cbuf, CE_UTF8));
		} else {
		    nb = wcstombs(NULL, wc, 0);
		    cbuf = CallocCharBuf(nb);
		    wcstombs(cbuf, wc, nb + 1);
		    SET_STRING_ELT(y, i, markKnown(cbuf, el));
		}
		Free(cbuf);
	    }
	    VMAXSET(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	unsigned char xtable[UCHAR_MAX + 1], *p, c_old, c_new;
	struct tr_spec *trs_old, **trs_old_ptr;
	struct tr_spec *trs_new, **trs_new_ptr;

	for (i = 0; i <= UCHAR_MAX; i++) xtable[i] = i;

	/* Initialize the old and new tr_spec lists. */
	trs_old = Calloc(1, struct tr_spec);
	trs_old->type = TR_INIT;
	trs_old->next = NULL;
	trs_new = Calloc(1, struct tr_spec);
	trs_new->type = TR_INIT;
	trs_new->next = NULL;
	/* Build the old and new tr_spec lists. */
	tr_build_spec(translateChar(STRING_ELT(old, 0)), trs_old);
	tr_build_spec(translateChar(STRING_ELT(_new, 0)), trs_new);
	/* Initialize the pointers for walking through the old and new
	   tr_spec lists and retrieving the next chars from the lists.
	*/
	trs_old_ptr = Calloc(1, struct tr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct tr_spec *);
	*trs_new_ptr = trs_new->next;
	for (;;) {
	    c_old = tr_get_next_char_from_spec(trs_old_ptr);
	    c_new = tr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		error(_("'old' is longer than 'new'"));
	    else
		xtable[c_old] = c_new;
	}
	/* Free the memory occupied by the tr_spec lists. */
	tr_free_spec(trs_old);
	tr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	n = LENGTH(x);
	PROTECT(y = allocVector(STRSXP, n));
	vmax = VMAXGET();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x,i) == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		const char *xi = translateChar(STRING_ELT(x, i));
		cbuf = CallocCharBuf(strlen(xi));
		strcpy(cbuf, xi);
		for (p = (unsigned char *) cbuf; *p != '\0'; p++)
		    *p = xtable[*p];
		SET_STRING_ELT(y, i, markKnown(cbuf, STRING_ELT(x, i)));
		Free(cbuf);
	    }
	}
	VMAXSET(vmax);
    }

    DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}

static SEXP do_strtrim(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, width;
    int i, len, nw, w, nc;
    const char *This;
    char *buf;
    const char *p; char *q;
    int w0, wsum, k, nb;
    wchar_t wc;
    mbstate_t mb_st;
    const void *vmax;

    checkArity(op, args);
    /* as.character happens at R level now */
    if (!isString(x = CAR(args)))
	error(_("strtrim() requires a character vector"));
    len = LENGTH(x);
    PROTECT(width = coerceVector(CADR(args), INTSXP));
    nw = LENGTH(width);
    if (!nw || (nw < len && len % nw))
	error(_("invalid '%s' argument"), "width");
    for (i = 0; i < nw; i++)
	if (INTEGER(width)[i] == NA_INTEGER ||
	   INTEGER(width)[i] < 0)
	    error(_("invalid '%s' argument"), "width");
    PROTECT(s = allocVector(STRSXP, len));
    vmax = VMAXGET();
    for (i = 0; i < len; i++) {
	if (STRING_ELT(x, i) == NA_STRING) {
	    SET_STRING_ELT(s, i, STRING_ELT(x, i));
	    continue;
	}
	w = INTEGER(width)[i % nw];
	This = translateChar(STRING_ELT(x, i));
	nc = strlen(This);
	buf = R_AllocStringBuffer(nc, &cbuff);
	wsum = 0;
	mbs_init(&mb_st);
	for (p = This, w0 = 0, q = buf; *p ;) {
	    nb =  Mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
	    w0 = Ri18n_wcwidth(wc);
	    if (w0 < 0) { p += nb; continue; } /* skip non-printable chars */
	    wsum += w0;
	    if (wsum <= w) {
		for (k = 0; k < nb; k++) *q++ = *p++;
	    } else break;
	}
	*q = '\0';
	SET_STRING_ELT(s, i, markKnown(buf, STRING_ELT(x, i)));
	VMAXSET(vmax);
    }
    if (len > 0) R_FreeStringBufferL(&cbuff);
    DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(2);
    return s;
}

static int strtoi(SEXP s, int base)
{
    long res;
    char *endp;

    /* strtol might return extreme values on error */
    errno = 0;

    if(s == NA_STRING) return(NA_INTEGER);
    res = strtol(CHAR(s), &endp, base); /* ASCII */
    if(errno || *endp != '\0') res = NA_INTEGER;
    if(res > INT_MAX || res < INT_MIN) res = NA_INTEGER;
    return(res);
}

static SEXP do_strtoi(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, b;
    int i, n, base;

    checkArity(op, args);

    x = CAR(args); args = CDR(args);
    b = CAR(args);
    
    if(!isInteger(b) || (length(b) < 1))
	error(_("invalid '%s' argument"), "base");
    base = INTEGER(b)[0];
    if((base != 0) && ((base < 2) || (base > 36)))
	error(_("invalid '%s' argument"), "base");

    PROTECT(ans = allocVector(INTSXP, n = LENGTH(x)));
    for(i = 0; i < n; i++)
	INTEGER(ans)[i] = strtoi(STRING_ELT(x, i), base);
    UNPROTECT(1);
    
    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_character[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"nzchar",	do_nzchar,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"nchar",	do_nchar,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	do_substr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"substr<-",	do_substrgets,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	do_abbrev,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.names",	do_makenames,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	do_strtrim,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strtoi",	do_strtoi,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
