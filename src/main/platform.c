/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011 The R Core Team
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


/* Notes on so-called 'Large File Support'

   The 'stat' structure returns a file size as 'off_t'.  On some
   32-bit systems this will fail if called on a file > 2GB.  On
   systems with LFS selected (see the notes in connections.c) the call
   is re-mapped to *stat64, which uses off64_t for the file size.

   file.info() returns file sizes as an R double.

   On Windows we need to remap for ourselves.  There are various
   versions of the 'stat' structure (some with 64-bit times and not
   available in the original MSVCRT.dll): we use _stati64 that simply
   replaces off_t by __int64_t.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* Don't enable this, since many instances, but probably not time critical */
/* #define USE_FAST_PROTECT_MACROS */ 
#include <Defn.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <R_ext/Applic.h>		/* machar */
#include <ctype.h>			/* toupper */
#include <time.h>			/* for ctime */

# include <errno.h>

/* Machine Constants */

static void Init_R_Machine(SEXP rho)
{
    SEXP ans, nms;

    machar(&R_AccuracyInfo.ibeta,
	   &R_AccuracyInfo.it,
	   &R_AccuracyInfo.irnd,
	   &R_AccuracyInfo.ngrd,
	   &R_AccuracyInfo.machep,
	   &R_AccuracyInfo.negep,
	   &R_AccuracyInfo.iexp,
	   &R_AccuracyInfo.minexp,
	   &R_AccuracyInfo.maxexp,
	   &R_AccuracyInfo.eps,
	   &R_AccuracyInfo.epsneg,
	   &R_AccuracyInfo.xmin,
	   &R_AccuracyInfo.xmax);

    /* Check that this is consistent with 64-bit IEEE floating point. */
    
    if (R_AccuracyInfo.irnd != 5
     || R_AccuracyInfo.eps  != 0x0.0000000000001p0
     || R_AccuracyInfo.xmax != 0x1.fffffffffffffp1023) {
        R_Suicide(
         "Floating-point arithmetic does not match 64-bit IEEE standard\n");
    }

    /* Check that double rounding doesn't happen. */

    volatile static double val1 = 0x1.0000000000001p0,
                           val2 = 0x0.00000000000007ffffff8p0;

    if (val1 + val2 != val1) {
        R_Suicide(
         "Floating-point arithmetic exhibits double rounding (not IEEE)\n");
    }

    /* Check that denormalized numbers exist. */

    if ((((val1 * 0x1p-515) * 0x1p-515) * 0x1p515) * 0x1p515 != 1) {
        R_Suicide(
         "Floating-point arithmetic lacks denormalized numbers (not IEEE)\n");
    }

    /* Check that fused multiply-add is not done. */

    volatile static double a = 1.0+((long long)1<<52);
    volatile static double b = 3.0;
    volatile static double c = -(double)((long long)2<<52);
    volatile static double d = 4.0+((long long)1<<52);

    double r = a*b+c;
    if (r != d) {
        R_Suicide(
         "Fused multiply-add is being done (not reproducible)\n");
    }

    /* Try to stop the arithmetic above from being done at compile time. */

    val1 = val2 = a = b = c = d = 0;

    R_dec_min_exponent = floor(log10(R_AccuracyInfo.xmin)); /* smallest decimal exponent */
    PROTECT(ans = allocVector(VECSXP, 18));
    PROTECT(nms = allocVector(STRSXP, 18));
    SET_STRING_ELT(nms, 0, mkChar("double.eps"));
    SET_VECTOR_ELT(ans, 0, ScalarReal(R_AccuracyInfo.eps));

    SET_STRING_ELT(nms, 1, mkChar("double.neg.eps"));
    SET_VECTOR_ELT(ans, 1, ScalarReal(R_AccuracyInfo.epsneg));

    SET_STRING_ELT(nms, 2, mkChar("double.xmin"));
    SET_VECTOR_ELT(ans, 2, ScalarReal(R_AccuracyInfo.xmin));

    SET_STRING_ELT(nms, 3, mkChar("double.xmax"));
    SET_VECTOR_ELT(ans, 3, ScalarReal(R_AccuracyInfo.xmax));

    SET_STRING_ELT(nms, 4, mkChar("double.base"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(R_AccuracyInfo.ibeta));

    SET_STRING_ELT(nms, 5, mkChar("double.digits"));
    SET_VECTOR_ELT(ans, 5, ScalarInteger(R_AccuracyInfo.it));

    SET_STRING_ELT(nms, 6, mkChar("double.rounding"));
    SET_VECTOR_ELT(ans, 6, ScalarInteger(R_AccuracyInfo.irnd));

    SET_STRING_ELT(nms, 7, mkChar("double.guard"));
    SET_VECTOR_ELT(ans, 7, ScalarInteger(R_AccuracyInfo.ngrd));

    SET_STRING_ELT(nms, 8, mkChar("double.ulp.digits"));
    SET_VECTOR_ELT(ans, 8, ScalarInteger(R_AccuracyInfo.machep));

    SET_STRING_ELT(nms, 9, mkChar("double.neg.ulp.digits"));
    SET_VECTOR_ELT(ans, 9, ScalarInteger(R_AccuracyInfo.negep));

    SET_STRING_ELT(nms, 10, mkChar("double.exponent"));
    SET_VECTOR_ELT(ans, 10, ScalarInteger(R_AccuracyInfo.iexp));

    SET_STRING_ELT(nms, 11, mkChar("double.min.exp"));
    SET_VECTOR_ELT(ans, 11, ScalarInteger(R_AccuracyInfo.minexp));

    SET_STRING_ELT(nms, 12, mkChar("double.max.exp"));
    SET_VECTOR_ELT(ans, 12, ScalarInteger(R_AccuracyInfo.maxexp));

    SET_STRING_ELT(nms, 13, mkChar("integer.max"));
    SET_VECTOR_ELT(ans, 13, ScalarInteger(INT_MAX));

    SET_STRING_ELT(nms, 14, mkChar("sizeof.long"));
    SET_VECTOR_ELT(ans, 14, ScalarInteger(SIZEOF_LONG));

    SET_STRING_ELT(nms, 15, mkChar("sizeof.longlong"));
    SET_VECTOR_ELT(ans, 15, ScalarInteger(SIZEOF_LONG_LONG));

    SET_STRING_ELT(nms, 16, mkChar("sizeof.longdouble"));
    SET_VECTOR_ELT(ans, 16, ScalarInteger(SIZEOF_LONG_DOUBLE));

    SET_STRING_ELT(nms, 17, mkChar("sizeof.pointer"));
    SET_VECTOR_ELT(ans, 17, ScalarInteger(sizeof(SEXP)));
    setAttrib(ans, R_NamesSymbol, nms);
    defineVar(install(".Machine"), ans, rho);
    UNPROTECT(2);
}


/*  Platform
 *
 *  Return various platform dependent strings.  This is similar to
 *  "Machine", but for strings rather than numerical values.  These
 *  two functions should probably be amalgamated.
 */
static const char  * const R_OSType = OSTYPE;
static const char  * const R_FileSep = FILESEP;

static void Init_R_Platform(SEXP rho)
{
    SEXP value, names;

    PROTECT(value = allocVector(VECSXP, 8));
    PROTECT(names = allocVector(STRSXP, 8));
    SET_STRING_ELT(names, 0, mkChar("OS.type"));
    SET_STRING_ELT(names, 1, mkChar("file.sep"));
    SET_STRING_ELT(names, 2, mkChar("dynlib.ext"));
    SET_STRING_ELT(names, 3, mkChar("GUI"));
    SET_STRING_ELT(names, 4, mkChar("endian"));
    SET_STRING_ELT(names, 5, mkChar("pkgType"));
    SET_STRING_ELT(names, 6, mkChar("path.sep"));
    SET_STRING_ELT(names, 7, mkChar("r_arch"));
    SET_VECTOR_ELT(value, 0, mkString(R_OSType));
    SET_VECTOR_ELT(value, 1, mkString(R_FileSep));
    SET_VECTOR_ELT(value, 2, mkString(SHLIB_EXT));
    SET_VECTOR_ELT(value, 3, mkString(R_GUIType));
#ifdef WORDS_BIGENDIAN
    SET_VECTOR_ELT(value, 4, mkString("big"));
#else
    SET_VECTOR_ELT(value, 4, mkString("little"));
#endif
/* pkgType should be "mac.binary" for CRAN build *only*, not for all
   AQUA builds. Also we want to be able to use "mac.binary.leopard"
   and similar for special builds. */
#ifdef PLATFORM_PKGTYPE 
    SET_VECTOR_ELT(value, 5, mkString(PLATFORM_PKGTYPE));
#else /* unix default */
    SET_VECTOR_ELT(value, 5, mkString("source"));
#endif
#ifdef Win32
    SET_VECTOR_ELT(value, 6, mkString(";"));
#else /* not Win32 */
    SET_VECTOR_ELT(value, 6, mkString(":"));
#endif
#ifdef R_ARCH
    SET_VECTOR_ELT(value, 7, mkString(R_ARCH));
#else
    SET_VECTOR_ELT(value, 7, mkString(""));
#endif
    setAttrib(value, R_NamesSymbol, names);
    defineVar(install(".Platform"), value, rho);
    UNPROTECT(2);
}

void attribute_hidden Init_R_Variables(SEXP rho)
{
    Init_R_Machine(rho);
    Init_R_Platform(rho);
}

#ifdef HAVE_LANGINFO_CODESET
/* case-insensitive string comparison (needed for locale check) */
int static R_strieql(const char *a, const char *b)
{
    while (*a && *b && toupper(*a) == toupper(*b)) { a++; b++; }
    return (*a == 0 && *b == 0);
}
#endif

#include <locale.h>
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

/* retrieves information about the current locale and
   sets the corresponding variables (known_to_be_utf8,
   known_to_be_latin1, utf8locale, latin1locale and mbcslocale) */
void attribute_hidden R_check_locale(void)
{
    known_to_be_utf8 = utf8locale = FALSE;
    known_to_be_latin1 = latin1locale = FALSE;
    mbcslocale = FALSE;
#ifdef HAVE_LANGINFO_CODESET
    {
	char  *p = nl_langinfo(CODESET);
	/* more relaxed due to Darwin: CODESET is case-insensitive and
	   latin1 is ISO8859-1 */
	if (R_strieql(p, "UTF-8")) known_to_be_utf8 = utf8locale = TRUE;
	if (streql(p, "ISO-8859-1")) known_to_be_latin1 = latin1locale = TRUE;
	if (R_strieql(p, "ISO8859-1")) known_to_be_latin1 = latin1locale = TRUE;
# if __APPLE__
	/* On Darwin 'regular' locales such as 'en_US' are UTF-8 (hence
	   MB_CUR_MAX == 6), but CODESET is "" */
	if (*p == 0 && MB_CUR_MAX == 6)
	    known_to_be_utf8 = utf8locale = TRUE;
# endif
    }
#endif
    mbcslocale = MB_CUR_MAX > 1;
#ifdef Win32
    {
	char *ctype = setlocale(LC_CTYPE, NULL), *p;
	p = strrchr(ctype, '.');
	if (p && isdigit(p[1])) localeCP = atoi(p+1); else localeCP = 0;
	/* Not 100% correct, but CP1252 is a superset */
	known_to_be_latin1 = latin1locale = (localeCP == 1252);
    }
#endif
#if defined(SUPPORT_UTF8_WIN32) /* never at present */
    utf8locale = mbcslocale = TRUE;
#endif
}

/*  date
 *
 *  Return the current date in a standard format.  This uses standard
 *  POSIX calls which should be available on each platform.  We should
 *  perhaps check this in the configure script.
 */
/* BDR 2000/7/20.
 *  time and ctime are in fact ANSI C calls, so we don't check them.
 */
static char *R_Date(void)
{
    time_t t;
    static char s[26];		/* own space */

    time(&t);
    strcpy(s, ctime(&t));
    s[24] = '\0';		/* overwriting the final \n */
    return s;
}

static SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString(R_Date());
}

/*  file.show
 *
 *  Display file(s) so that a user can view it.  The function calls
 *  "R_ShowFiles" which is a platform-dependent hook that arranges
 *  for the file(s) to be displayed.
 */

static SEXP do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl, hd, pg;
    const char **f, **h, *t, *pager = NULL /* -Wall */;
    Rboolean dl;
    int i, n;

    checkArity(op, args);
    fn = CAR(args); args = CDR(args);
    hd = CAR(args); args = CDR(args);
    tl = CAR(args); args = CDR(args);
    dl = (Rboolean) asLogical(CAR(args)); args = CDR(args);
    pg = CAR(args);
    if (!isString(fn) || (n = length(fn)) < 1)
	error(_("invalid filename specification"));
    if (!isString(hd) || length(hd) != n)
	error(_("invalid '%s' argument"), "headers");
    if (!isString(tl))
	error(_("invalid '%s' argument"), "title");
    if (!isString(pg))
	error(_("invalid '%s' argument"), "pager");
    f = (const char**) R_alloc(n, sizeof(char*));
    h = (const char**) R_alloc(n, sizeof(char*));
    for (i = 0; i < n; i++) {
	SEXP el = STRING_ELT(fn, i);
	if (!isNull(el) && el != NA_STRING)
#ifdef Win32
	    f[i] = acopy_string(reEnc(CHAR(el), getCharCE(el), CE_UTF8, 1));
#else
	    f[i] = acopy_string(translateChar(el));
#endif
	else
            error(_("invalid filename specification"));
	if (STRING_ELT(hd, i) != NA_STRING)
	    h[i] = acopy_string(translateChar(STRING_ELT(hd, i)));
	else
            error(_("invalid '%s' argument"), "headers");
    }
    if (isValidStringF(tl))
	t = acopy_string(translateChar(STRING_ELT(tl, 0)));
    else
	t = "";
    if (isValidStringF(pg)) {
	SEXP pg0 = STRING_ELT(pg, 0);
        if (pg0 != NA_STRING)
            pager = acopy_string(CHAR(pg0));
        else
            error(_("invalid '%s' argument"), "pager");
    } else
	pager = "";
    R_ShowFiles(n, f, h, t, dl, pager);
    return R_NilValue;
}

/*  file.edit
 *
 *  Open a file in a text editor. The function calls
 *  "R_EditFiles" which is a platform dependent hook that invokes
 *  the given editor.
 *
 */


static SEXP do_fileedit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ti, ed;
    const char **f, **title, *editor;
    int i, n;

    checkArity(op, args);
    fn = CAR(args); args = CDR(args);
    ti = CAR(args); args = CDR(args);
    ed = CAR(args);

    n = length(fn);
    if (!isString(ed) || length(ed) != 1)
	error(_("invalid '%s' specification"), "editor");
    if (n > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' specification"), "filename");
	f = (const char**) R_alloc(n, sizeof(char*));
	title = (const char**) R_alloc(n, sizeof(char*));
	/* FIXME convert to UTF-8 on Windows */
	for (i = 0; i < n; i++) {
	    SEXP el = STRING_ELT(fn, 0);
	    if (!isNull(el))
#ifdef Win32
		f[i] = acopy_string(reEnc(CHAR(el), getCharCE(el), CE_UTF8, 1));
#else
		f[i] = acopy_string(translateChar(el));
#endif
	    else
		f[i] = "";
	    if (!isNull(STRING_ELT(ti, i)))
		title[i] = acopy_string(translateChar(STRING_ELT(ti, i)));
	    else
		title[i] = "";
	}
    }
    else {  /* open a new file for editing */
	n = 1;
	f = (const char**) R_alloc(1, sizeof(char*));
	f[0] = "";
	title = (const char**) R_alloc(1, sizeof(char*));
	title[0] = "";
    }
    SEXP ed0 = STRING_ELT(ed, 0);
#ifdef Win32
    editor = acopy_string(reEnc(CHAR(ed0), getCharCE(ed0), CE_UTF8, 1));
#else
    editor = acopy_string(translateChar(ed0));
#endif
    R_EditFiles(n, f, title, editor);
    return R_NilValue;
}


/*  file.append
 *
 *  Given two file names as arguments and arranges for
 *  the second file to be appended to the second.
 *  op = 1 is codeFiles.append, used in tools:::.file_append_ensuring_LFs
 */

#if defined(BUFSIZ) && (BUFSIZ > 512)
/* OS's buffer size in stdio.h, probably.
   Windows has 512, Solaris 1024, glibc 8192
 */
# define APPENDBUFSIZE BUFSIZ
#else
# define APPENDBUFSIZE 512
#endif

static int R_AppendFile(SEXP file1, SEXP file2)
{
    FILE *fp1, *fp2;
    char buf[APPENDBUFSIZE];
    int nchar, status = 0;
    if ((fp1 = RC_fopen(file1, "ab", TRUE)) == NULL) {
	return 0;
    }
    if ((fp2 = RC_fopen(file2, "rb", TRUE)) == NULL) {
	fclose(fp1);
	return 0;
    }
    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
	if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE) {
	    goto append_error;
	}
    if (fwrite(buf, 1, nchar, fp1) != nchar) {
	goto append_error;
    }
    status = 1;
 append_error:
    if (status == 0)
	warning(_("write error during file append"));
    fclose(fp1);
    fclose(fp2);
    return status;
}

static SEXP do_fileappend(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n, n1, n2;
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "file1");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "file2");
    if (n1 < 1)
	error(_("nothing to append to"));
    if (PRIMVAL(op) > 0 && n1 > 1)
	error(_("'outFile' must be a single file"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) LOGICAL(ans)[i] = 0;  /* all FALSE */
    if (n1 == 1) { /* common case */
	FILE *fp1, *fp2;
	char buf[APPENDBUFSIZE];
	int nchar, status = 0;
	if (STRING_ELT(f1, 0) == NA_STRING ||
	    !(fp1 = RC_fopen(STRING_ELT(f1, 0), "ab", TRUE)))
	   goto done;
	for (i = 0; i < n; i++) {
	    status = 0;
	    if (STRING_ELT(f2, i) == NA_STRING ||
	       !(fp2 = RC_fopen(STRING_ELT(f2, i), "rb", TRUE))) continue;
	    if (PRIMVAL(op) == 1) { /* codeFiles.append */
	    	snprintf(buf, APPENDBUFSIZE, "#line 1 \"%s\"\n",
			 CHAR(STRING_ELT(f2, i)));
	    	if(fwrite(buf, 1, strlen(buf), fp1) != strlen(buf))
		    goto append_error;
	    }
	    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE)
		    goto append_error;
	    if (fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
	    if (PRIMVAL(op) == 1 && buf[nchar - 1] != '\n') {
		if (fwrite("\n", 1, 1, fp1) != 1) goto append_error;
	    }

	    status = 1;
	append_error:
	    if (status == 0)
		warning(_("write error during file append"));
	    LOGICAL(ans)[i] = status;
	    fclose(fp2);
	}
	fclose(fp1);
    } else {
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(f1, i%n1) == R_NilValue ||
		STRING_ELT(f2, i%n2) == R_NilValue)
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] =
		    R_AppendFile(STRING_ELT(f1, i%n1), STRING_ELT(f2, i%n2));
	}
    }
done:
    UNPROTECT(1);
    return ans;
}

static SEXP do_filecreate(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    FILE *fp;
    int i, n, show;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid filename argument"));
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    n = length(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(fn, i) == NA_STRING) continue;
	if ((fp = RC_fopen(STRING_ELT(fn, i), "w", TRUE)) != NULL) {
	    LOGICAL(ans)[i] = 1;
	    fclose(fp);
	} else if (show) {
	    warning(_("cannot create file '%s', reason '%s'"),
		    translateChar(STRING_ELT(fn, i)), strerror(errno));
	}
    }
    UNPROTECT(1);
    return ans;
}

static SEXP do_fileremove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f, ans;
    int i, n;
    checkArity(op, args);
    f = CAR(args);
    if (!isString(f))
	error(_("invalid first filename"));
    n = length(f);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f, i) != NA_STRING) {
	    LOGICAL(ans)[i] =
#ifdef Win32
		(_wremove(filenameToWchar(STRING_ELT(f, i), TRUE)) == 0);
#else
		(remove(R_ExpandFileName(translateChar(STRING_ELT(f, i)))) == 0);
#endif
	    if(!LOGICAL(ans)[i])
		warning(_("cannot remove file '%s', reason '%s'"),
			translateChar(STRING_ELT(f, i)), strerror(errno));
	} else LOGICAL(ans)[i] = FALSE;
    }
    UNPROTECT(1);
    return ans;
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for symlink, getpid */
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef Win32
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0500 /* for CreateHardLink */
#endif
#include <windows.h>
typedef BOOLEAN (WINAPI *PCSL)(LPWSTR, LPWSTR, DWORD);
static PCSL pCSL = NULL;
const char *formatError(DWORD res);  /* extra.c */
/* Windows does not have link(), but it does have CreateHardLink() on NTFS */
#undef HAVE_LINK
#define HAVE_LINK 1
/* Windows does not have symlink(), but >= Vista does have 
   CreateSymbolicLink() on NTFS */
#undef HAVE_SYMLINK
#define HAVE_SYMLINK 1
#endif

/* the Win32 stuff here is not ready for release:

   (i) It needs Windows >= Vista
   (ii) It matters whether 'from' is a file or a dir, and we could only 
   know if it exists already.
   (iii) This needs specific privileges which in general only Adminstrators 
   have, and which many people report granting in the Policy Editor 
   fails to work.
*/
static SEXP do_filesymlink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_SYMLINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    if (n1 < 1)
	error(_("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;

#ifdef Win32
    pCSL = (PCSL) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
				 "CreateSymbolicLinkW");
    if(!pCSL) 
	error(_("symbolic links are not supported on this version of Windows"));
#endif

#ifdef HAVE_SYMLINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f1, i%n1) == NA_STRING ||
	    STRING_ELT(f2, i%n2) == NA_STRING)
	    LOGICAL(ans)[i] = 0;
	else {
#ifdef Win32
	    wchar_t *from, *to;
	    struct _stati64 sb;
	    from = filenameToWchar(STRING_ELT(f1, i%n1), TRUE);
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    _wstati64(from, &sb);
	    int isDir = (sb.st_mode & S_IFDIR) > 0;
	    LOGICAL(ans)[i] = pCSL(to, from, isDir) != 0;
	    if(!LOGICAL(ans)[i])
		warning(_("cannot symlink '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
#else
	    char from[PATH_MAX+1], to[PATH_MAX+1];
	    const char *p;
	    p = R_ExpandFileName(translateChar(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);
	    p = R_ExpandFileName(translateChar(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(to, p);
	    /* Rprintf("linking %s to %s\n", from, to); */
	    LOGICAL(ans)[i] = symlink(from, to) == 0;
	    if(!LOGICAL(ans)[i])
		warning(_("cannot symlink '%s' to '%s', reason '%s'"),
			from, to, strerror(errno));
#endif
	}
    }
    UNPROTECT(1);
    return ans;
#else
    warning(_("symlinks are not supported on this platform"));
    return allocVector(LGLSXP, n);
#endif
}


static SEXP do_filelink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_LINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    if (n1 < 1)
	error(_("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
#ifdef HAVE_LINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f1, i%n1) == NA_STRING ||
	    STRING_ELT(f2, i%n2) == NA_STRING)
	    LOGICAL(ans)[i] = 0;
	else {
#ifdef Win32
	    wchar_t *from, *to;
	    
	    from = filenameToWchar(STRING_ELT(f1, i%n1), TRUE);
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    LOGICAL(ans)[i] = CreateHardLinkW(to, from, NULL) != 0;
	    if(!LOGICAL(ans)[i]) {
		warning(_("cannot link '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
	    }
#else
	    char from[PATH_MAX], to[PATH_MAX];
	    const char *p;
	    p = R_ExpandFileName(translateChar(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);
	    p = R_ExpandFileName(translateChar(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(to, p);
	    LOGICAL(ans)[i] = link(from, to) == 0;
	    if(!LOGICAL(ans)[i]) {
		warning(_("cannot link '%s' to '%s', reason '%s'"),
			from, to, strerror(errno));
	    }
#endif
	}
    }
    UNPROTECT(1);
    return ans;
#else
    warning(_("(hard) links are not supported on this platform"));
    return allocVector(LGLSXP, n);
#endif
}

#ifdef Win32
int Rwin_rename(char *from, char *to);  /* in src/gnuwin32/extra.c */
int Rwin_wrename(const wchar_t *from, const wchar_t *to);
#endif

static SEXP do_filerename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n1, n2;
#ifdef Win32
    wchar_t from[PATH_MAX], to[PATH_MAX];
    const wchar_t *w;
#else
    char from[PATH_MAX], to[PATH_MAX];
    const char *p;
    int res;
#endif

    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "from");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "to");
    if (n2 != n1)
	error(_("'from' and 'to' are of different lengths"));
    PROTECT(ans = allocVector(LGLSXP, n1));
    for (i = 0; i < n1; i++) {
	if (STRING_ELT(f1, i) == NA_STRING ||
	    STRING_ELT(f2, i) == NA_STRING) {
	    LOGICAL(ans)[i] = 0;
	    continue;
	}
#ifdef Win32
	w = filenameToWchar(STRING_ELT(f1, i), TRUE);
	if (wcslen(w) >= PATH_MAX - 1)
	    error(_("expanded 'from' name too long"));
	wcsncpy(from, w, PATH_MAX - 1);
	w = filenameToWchar(STRING_ELT(f2, i), TRUE);
	if (wcslen(w) >= PATH_MAX - 1)
	    error(_("expanded 'to' name too long"));
	wcsncpy(to, w, PATH_MAX - 1);
	LOGICAL(ans)[i] = (Rwin_wrename(from, to) == 0);
#else
	p = R_ExpandFileName(translateChar(STRING_ELT(f1, i)));
	if (strlen(p) >= PATH_MAX - 1)
	    error(_("expanded 'from' name too long"));
	strncpy(from, p, PATH_MAX - 1);
	p = R_ExpandFileName(translateChar(STRING_ELT(f2, i)));
	if (strlen(p) >= PATH_MAX - 1)
	    error(_("expanded 'to' name too long"));
	strncpy(to, p, PATH_MAX - 1);
	res = rename(from, to);
	if(res) {
	    warning(_("cannot rename file '%s' to '%s', reason '%s'"),
		    from, to, strerror(errno));
	}
	LOGICAL(ans)[i] = (res == 0);
#endif
    }
    UNPROTECT(1);
    return ans;
}

# if defined(Unix) && defined(HAVE_PWD_H) && defined(HAVE_GRP_H) \
  && defined(HAVE_GETPWUID) && defined(HAVE_GETGRGID)
#  include <pwd.h>
#  include <grp.h>
#  define UNIX_EXTRAS 1
# endif

#ifdef Win32
# ifndef SCS_64BIT_BINARY
#  define SCS_64BIT_BINARY 6
# endif
#endif

#if defined HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC
# ifdef TYPEOF_STRUCT_STAT_ST_ATIM_IS_STRUCT_TIMESPEC
#  define STAT_TIMESPEC(st, st_xtim) ((st).st_xtim)
# else
#  define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim.tv_nsec)
# endif
#elif defined HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC
# define STAT_TIMESPEC(st, st_xtim) ((st).st_xtim##espec)
#elif defined HAVE_STRUCT_STAT_ST_ATIMENSEC
# define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim##ensec)
#elif defined HAVE_STRUCT_STAT_ST_ATIM_ST__TIM_TV_NSEC
# define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim.st__tim.tv_nsec)
#endif

static SEXP do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans, ansnames, fsize, mtime, ctime, atime, isdir,
	mode, xxclass;
#ifdef UNIX_EXTRAS
    SEXP uid, gid, uname, grname;
    struct passwd *stpwd;
    struct group *stgrp;
#endif
    int i, n;
#ifdef Win32
    SEXP exe;
    struct _stati64 sb;
#else
    struct stat sb;
#endif

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid filename argument"));
    n = length(fn);
#ifdef UNIX_EXTRAS
    PROTECT(ans = allocVector(VECSXP, 10));
    PROTECT(ansnames = allocVector(STRSXP, 10));
#elif defined(Win32)
    PROTECT(ans = allocVector(VECSXP, 7));
    PROTECT(ansnames = allocVector(STRSXP, 7));
#else
    PROTECT(ans = allocVector(VECSXP, 6));
    PROTECT(ansnames = allocVector(STRSXP, 6));
#endif
    fsize = SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 0, mkChar("size"));
    isdir = SET_VECTOR_ELT(ans, 1, allocVector(LGLSXP, n));
    SET_STRING_ELT(ansnames, 1, mkChar("isdir"));
    mode  = SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, n));
    SET_STRING_ELT(ansnames, 2, mkChar("mode"));
    mtime = SET_VECTOR_ELT(ans, 3, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 3, mkChar("mtime"));
    ctime = SET_VECTOR_ELT(ans, 4, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 4, mkChar("ctime"));
    atime = SET_VECTOR_ELT(ans, 5, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 5, mkChar("atime"));
#ifdef UNIX_EXTRAS
    uid = SET_VECTOR_ELT(ans, 6, allocVector(INTSXP, n));
    SET_STRING_ELT(ansnames, 6, mkChar("uid"));
    gid = SET_VECTOR_ELT(ans, 7, allocVector(INTSXP, n));
    SET_STRING_ELT(ansnames, 7, mkChar("gid"));
    uname = SET_VECTOR_ELT(ans, 8, allocVector(STRSXP, n));
    SET_STRING_ELT(ansnames, 8, mkChar("uname"));
    grname = SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, n));
    SET_STRING_ELT(ansnames, 9, mkChar("grname"));
#endif
#ifdef Win32
    exe = SET_VECTOR_ELT(ans, 6, allocVector(STRSXP, n));
    SET_STRING_ELT(ansnames, 6, mkChar("exe"));
#endif
    for (i = 0; i < n; i++) {
#ifdef Win32
	wchar_t *wfn = filenameToWchar(STRING_ELT(fn, i), TRUE);
	/* 'Sharpie' and fellow ignorami use trailing / on Windows,
	   where it is not valid */
	wchar_t *p = wfn + (wcslen(wfn) - 1);
	if (*p == L'/' || *p == L'\\') *p = 0;
#else
	const char *efn = R_ExpandFileName(translateChar(STRING_ELT(fn, i)));
#endif
	if (STRING_ELT(fn, i) != NA_STRING &&
#ifdef Win32
	    _wstati64(wfn, &sb)
#else
	    /* Target not link */
	    stat(efn, &sb)
#endif
	    == 0) {
	    REAL(fsize)[i] = (double) sb.st_size;
	    LOGICAL(isdir)[i] = (sb.st_mode & S_IFDIR) > 0;
	    INTEGER(mode)[i]  = (int) sb.st_mode & 0007777;

#if defined STAT_TIMESPEC
	    /* POSIX 2008 changed this to a struct timespec st_mtim etc
	       Not all OSes (e.g. Darwin) agree on this. */
	    REAL(mtime)[i] = (double) STAT_TIMESPEC(sb, st_mtim).tv_sec
		+ 1e-9 * STAT_TIMESPEC(sb, st_mtim).tv_nsec;
	    REAL(ctime)[i] = (double) STAT_TIMESPEC(sb, st_ctim).tv_sec
		+ 1e-9 * STAT_TIMESPEC(sb, st_ctim).tv_nsec;
	    REAL(atime)[i] = (double) STAT_TIMESPEC(sb, st_atim).tv_sec
		+ 1e-9 * STAT_TIMESPEC(sb, st_atim).tv_nsec;
#else
	    /* FIXME: there are higher-resolution ways to do this on Windows */
	    REAL(mtime)[i] = (double) sb.st_mtime;
	    REAL(ctime)[i] = (double) sb.st_ctime;
	    REAL(atime)[i] = (double) sb.st_atime;
# ifdef STAT_TIMESPEC_NS
	    REAL(mtime)[i] += STAT_TIMESPEC_NS (st, st_mtim);
	    REAL(ctime)[i] += STAT_TIMESPEC_NS (st, st_ctim);
	    REAL(atime)[i] += STAT_TIMESPEC_NS (st, st_atim);
# endif
#endif
#ifdef UNIX_EXTRAS
	    INTEGER(uid)[i] = (int) sb.st_uid;
	    INTEGER(gid)[i] = (int) sb.st_gid;
	    stpwd = getpwuid(sb.st_uid);
	    if (stpwd) SET_STRING_ELT(uname, i, mkChar(stpwd->pw_name));
	    else SET_STRING_ELT_NA(uname, i);
	    stgrp = getgrgid(sb.st_gid);
	    if (stgrp) SET_STRING_ELT(grname, i, mkChar(stgrp->gr_name));
	    else SET_STRING_ELT_NA(grname, i);
#endif
#ifdef Win32
	    {
		char *s="no";
		DWORD type;
		if (GetBinaryTypeW(wfn, &type))
		    switch(type) {
		    case SCS_64BIT_BINARY:
			s = "win64";
			break;
		    case SCS_32BIT_BINARY:
			s = "win32";
			break;
		    case SCS_DOS_BINARY:
		    case SCS_PIF_BINARY:
			s = "msdos";
			break;
		    case SCS_WOW_BINARY:
			s = "win16";
			break;
		    default:
			s = "unknown";
		    }
		SET_STRING_ELT(exe, i, mkChar(s));
	    }
#endif
	} else {
	    REAL(fsize)[i] = NA_REAL;
	    LOGICAL(isdir)[i] = NA_INTEGER;
	    INTEGER(mode)[i]  = NA_INTEGER;
	    REAL(mtime)[i] = NA_REAL;
	    REAL(ctime)[i] = NA_REAL;
	    REAL(atime)[i] = NA_REAL;
#ifdef UNIX_EXTRAS
	    INTEGER(uid)[i] = NA_INTEGER;
	    INTEGER(gid)[i] = NA_INTEGER;
	    SET_STRING_ELT_NA(uname, i);
	    SET_STRING_ELT_NA(grname, i);
#endif
#ifdef Win32
	    SET_STRING_ELT_NA(exe, i);
#endif
	}
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(xxclass = mkString("octmode"));
    classgets(mode, xxclass);
    UNPROTECT(3);
    return ans;
}

/* No longer required by POSIX, but maybe on earlier OSes */
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
#elif HAVE_SYS_NDIR_H
# include <sys/ndir.h>
#elif HAVE_SYS_DIR_H
# include <sys/dir.h>
#elif HAVE_NDIR_H
# include <ndir.h>
#endif

#define CBUFSIZE 2*PATH_MAX+1
static SEXP filename(const char *dir, const char *file)
{
    SEXP ans;
    char cbuf[CBUFSIZE];
    if (dir) {
#ifdef Win32
	if ((strlen(dir) == 2 && dir[1] == ':') ||
	    dir[strlen(dir) - 1] == '/' ||  dir[strlen(dir) - 1] == '\\')
	    snprintf(cbuf, CBUFSIZE, "%s%s", dir, file);
	else
	    snprintf(cbuf, CBUFSIZE, "%s%s%s", dir, R_FileSep, file);
#else
	snprintf(cbuf, CBUFSIZE, "%s%s%s", dir, R_FileSep, file);
#endif
	ans = mkChar(cbuf);
    } else {
	snprintf(cbuf, CBUFSIZE, "%s", file);
	ans = mkChar(cbuf);
    }
    return ans;
}

#include <tre/tre.h>

static void
list_files(const char *dnp, const char *stem, int *count, SEXP *pans,
	   Rboolean allfiles, Rboolean recursive,
	   const regex_t *reg, int *countmax, PROTECT_INDEX idx, Rboolean idirs)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX], stem2[PATH_MAX];
#ifdef Windows
    /* > 2GB files might be skipped otherwise */
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    R_CheckUserInterrupt();
    if ((dir = opendir(dnp)) != NULL) {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
		if (recursive) {
#ifdef Win32
		    if (strlen(dnp) == 2 && dnp[1] == ':')
			snprintf(p, PATH_MAX, "%s%s", dnp, de->d_name);
		    else
			snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#else
		    snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#endif
#ifdef Windows
		    _stati64(p, &sb);
#else
		    stat(p, &sb);
#endif
		    if ((sb.st_mode & S_IFDIR) > 0) {
			if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) {
			    if (idirs && 
				(!reg || tre_regexec(reg, de->d_name, 0, NULL, 0) == 0)) {
				if (*count == *countmax - 1) {
				    *countmax *= 2;
				    REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
				}
				SET_STRING_ELT(*pans, (*count)++,
					       filename(stem, de->d_name));
			    }
			    if (stem) {
#ifdef Win32
				if(strlen(stem) == 2 && stem[1] == ':')
				    snprintf(stem2, PATH_MAX, "%s%s", stem,
					     de->d_name);
				else
				    snprintf(stem2, PATH_MAX, "%s%s%s", stem,
					     R_FileSep, de->d_name);
#else
				snprintf(stem2, PATH_MAX, "%s%s%s", stem,
					 R_FileSep, de->d_name);
#endif
			    } else
				strcpy(stem2, de->d_name);
			    list_files(p, stem2, count, pans, allfiles,
				       recursive, reg, countmax, idx, idirs);
			}
			continue;
		    }
		}
		if (!reg || tre_regexec(reg, de->d_name, 0, NULL, 0) == 0) {
                    if (*count == *countmax - 1) {
                        *countmax *= 2;
                        REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
                    }
                    SET_STRING_ELT(*pans, (*count)++,
                                   filename(stem, de->d_name));
                }
	    }
	}
	closedir(dir);
    }
}

static SEXP do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX idx;
    SEXP d, p, ans;
    int i, allfiles, fullnames, count, pattern, recursive, igcase, flags, idirs;
    const char *dnp;
    regex_t reg;
    int countmax = 128;

    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d)) error(_("invalid '%s' argument"), "directory");
    p = CAR(args); args = CDR(args);
    pattern = 0;
    if (isString(p) && length(p) >= 1 && STRING_ELT(p, 0) != NA_STRING)
	pattern = 1;
    else if (!isNull(p) && !(isString(p) && length(p) < 1))
	error(_("invalid '%s' argument"), "pattern");
    allfiles = asLogical(CAR(args)); args = CDR(args);
    if (allfiles == NA_LOGICAL) 
	error(_("invalid '%s' argument"), "all.files");
    fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    recursive = asLogical(CAR(args)); args = CDR(args);
    if (recursive == NA_LOGICAL)
	error(_("invalid '%s' argument"), "recursive");
    igcase = asLogical(CAR(args)); args = CDR(args);
    if (igcase == NA_LOGICAL)
	error(_("invalid '%s' argument"), "ignore.case");
    idirs = asLogical(CAR(args));
    if (idirs == NA_LOGICAL) 
	error(_("invalid '%s' argument"), "include.dirs");
    flags = REG_EXTENDED;
    if (igcase) flags |= REG_ICASE;

    if (pattern && tre_regcomp(&reg, translateChar(STRING_ELT(p, 0)), flags))
	error(_("invalid 'pattern' regular expression"));
    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, countmax), &idx);
    count = 0;
    for (i = 0; i < LENGTH(d) ; i++) {
	if (STRING_ELT(d, i) == NA_STRING) continue;
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	list_files(dnp, fullnames ? dnp : NULL, &count, &ans, allfiles,
		   recursive, pattern ? &reg : NULL, &countmax, idx, idirs);
    }
    REPROTECT(ans = lengthgets(ans, count), idx);
    if (pattern) tre_regfree(&reg);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

static void list_dirs(const char *dnp, const char *stem, int *count, 
		      SEXP *pans, int *countmax, PROTECT_INDEX idx,
		      Rboolean recursive)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX], stem2[PATH_MAX];
#ifdef Windows
    /* > 2GB files might be skipped otherwise */
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    R_CheckUserInterrupt();
    if ((dir = opendir(dnp)) != NULL) {
	if (recursive) {
	    if (*count == *countmax - 1) {
		*countmax *= 2;
		REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
	    }
	    SET_STRING_ELT(*pans, (*count)++, mkChar(dnp));
	}
	while ((de = readdir(dir))) {
#ifdef Win32
	    if (strlen(dnp) == 2 && dnp[1] == ':')
		snprintf(p, PATH_MAX, "%s%s", dnp, de->d_name);
	    else
		snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#else
	    snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#endif
#ifdef Windows
	    _stati64(p, &sb);
#else
	    stat(p, &sb);
#endif
	    if ((sb.st_mode & S_IFDIR) > 0) {
		if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) {
		    if(recursive) {
			if (stem) {
#ifdef Win32
			    if(strlen(stem) == 2 && stem[1] == ':')
				snprintf(stem2, PATH_MAX, "%s%s", stem,
					 de->d_name);
			    else
				snprintf(stem2, PATH_MAX, "%s%s%s", stem,
					 R_FileSep, de->d_name);
#else
			    snprintf(stem2, PATH_MAX, "%s%s%s", stem,
				     R_FileSep, de->d_name);
#endif
			} else strcpy(stem2, de->d_name);
			list_dirs(p, stem2, count, pans, countmax, idx, recursive);
			
		    } else {
			if (*count == *countmax - 1) {
			    *countmax *= 2;
			    REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
			}
			SET_STRING_ELT(*pans, (*count)++, mkChar(p));
		    }
		}
	    }
	}
	closedir(dir);
    }
}

static SEXP do_listdirs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX idx;
    SEXP d, ans;
    int fullnames, count, i, recursive;
    const char *dnp;
    int countmax = 128;

    checkArity(op, args);
    d = CAR(args); args = CDR(args);    
    if (!isString(d)) error(_("invalid '%s' argument"), "directory");
    fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    recursive = asLogical(CAR(args)); args = CDR(args);
    if (recursive == NA_LOGICAL)
	error(_("invalid '%s' argument"), "recursive");
    
    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, countmax), &idx);
    count = 0;
    for (i = 0; i < LENGTH(d) ; i++) {
	if (STRING_ELT(d, i) == NA_STRING) continue;
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	list_dirs(dnp, fullnames ? dnp : NULL, &count, &ans, &countmax,
		  idx, recursive);
    }
    REPROTECT(ans = lengthgets(ans, count), idx);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

static SEXP do_Rhome(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *path;
    checkArity(op, args);
    if (!(path = R_HomeDir()))
	error(_("unable to determine R home location"));
    return mkString(path);
}

#ifdef Win32
static Rboolean attribute_hidden R_WFileExists(const wchar_t *path)
{
    struct _stati64 sb;
    return _wstati64(path, &sb) == 0;
}
#endif

static SEXP do_fileexists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, ans;
    int i, nfile;
    checkArity(op, args);
    if (!isString(file = CAR(args)))
	error(_("invalid '%s' argument"), "file");
    nfile = length(file);
    ans = PROTECT(allocVector(LGLSXP, nfile));
    for (i = 0; i < nfile; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(file, i) != NA_STRING) {
#ifdef Win32
	    /* Package XML sends arbitrarily long strings to file.exists! */
	    int len = strlen(CHAR(STRING_ELT(file, i)));
	    if (len > MAX_PATH)
		LOGICAL(ans)[i] = FALSE;
	    else
		LOGICAL(ans)[i] =
		    R_WFileExists(filenameToWchar(STRING_ELT(file, i), TRUE));
#else
	    LOGICAL(ans)[i] = R_FileExists(translateChar(STRING_ELT(file, i)));
#endif
	} else LOGICAL(ans)[i] = FALSE;
    }
    UNPROTECT(1); /* ans */
    return ans;
}

#define CHOOSEBUFSIZE 1024

#ifdef Win32 /* Windows version is in src/gnuwin32/extra.c */
extern SEXP do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho);
#else
static SEXP do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int _new, len;
    char buf[CHOOSEBUFSIZE];
    checkArity(op, args);
    _new = asLogical(CAR(args));
    if ((len = R_ChooseFile(_new, buf, CHOOSEBUFSIZE)) == 0)
	error(_("file choice cancelled"));
    if (len >= CHOOSEBUFSIZE - 1)
	error(_("file name too long"));
    return mkString(R_ExpandFileName(buf));
}
#endif

/* needed for access, and perhaps for realpath */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef Win32
extern int winAccessW(const wchar_t *path, int mode);
#endif

/* we require 'access' as from 2.12.0 */
static SEXP do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n, mode, modemask;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "names");
    n = length(fn);
    mode = asInteger(CADR(args));
    if (mode < 0 || mode > 7) error(_("invalid '%s' argument"), "mode");
    modemask = 0;
    if (mode & 1) modemask |= X_OK;
    if (mode & 2) modemask |= W_OK;
    if (mode & 4) modemask |= R_OK;
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++)
	if (STRING_ELT(fn, i) != NA_STRING) {
	    INTEGER(ans)[i] =
#ifdef Win32
		winAccessW(filenameToWchar(STRING_ELT(fn, i), TRUE), modemask);
#else
		access(R_ExpandFileName(translateChar(STRING_ELT(fn, i))),
		       modemask);
#endif
	} else INTEGER(ans)[i] = FALSE;
    UNPROTECT(1);
    return ans;
}

#ifdef Win32

static int R_rmdir(const wchar_t *dir)
{
    wchar_t tmp[MAX_PATH];
    GetShortPathNameW(dir, tmp, MAX_PATH);
    //printf("removing directory %ls\n", tmp);
    return _wrmdir(tmp);
}

/* Junctions and symbolic links are fundamentally reparse points, so
   apparently this is the way to detect them. */
static int isReparsePoint(const wchar_t *name)
{
    DWORD res = GetFileAttributesW(name);
    if(res == INVALID_FILE_ATTRIBUTES) {
	warning("cannot get info on '%ls', reason '%s'",
		name, formatError(GetLastError()));
	return 0;
    }
    // printf("%ls: %x\n", name, res);
    return res & FILE_ATTRIBUTE_REPARSE_POINT;
}

static int delReparsePoint(const wchar_t *name)
{
    HANDLE hd = 
	CreateFileW(name, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING,
		    FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
		    0);
    if(hd == INVALID_HANDLE_VALUE) {
	warning("cannot open reparse point '%ls', reason '%s'",
		name, formatError(GetLastError()));
	return 1;
    }    
    REPARSE_GUID_DATA_BUFFER rgdb = {0};
    rgdb.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
    DWORD dwBytes;
    BOOL res = DeviceIoControl(hd, FSCTL_DELETE_REPARSE_POINT, &rgdb,
			       REPARSE_GUID_DATA_BUFFER_HEADER_SIZE,
			       NULL, 0, &dwBytes, 0);
    CloseHandle(hd);
    if(res == 0)
	warning("cannot delete reparse point '%ls', reason '%s'",
		name, formatError(GetLastError()));
    else /* This may leave an empty dir behind */
	R_rmdir(name);
    return res == 0;
}

static int R_unlink(wchar_t *name, int recursive, int force)
{
    if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) return 0;
    //printf("R_unlink(%ls)\n", name);
    if (!R_WFileExists(name)) return 0;
    if (force) _wchmod(name, _S_IWRITE);

    if (recursive) {
	_WDIR *dir;
	struct _wdirent *de;
	wchar_t p[PATH_MAX];
	struct _stati64 sb;
	int n, ans = 0;

	_wstati64(name, &sb);
	/* We need to test for a junction first, as junctions
	   are detected as directories. */
	if (isReparsePoint(name)) ans += delReparsePoint(name);
	else if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	    if ((dir = _wopendir(name)) != NULL) {
		while ((de = _wreaddir(dir))) {
		    if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
			continue;
		    /* On Windows we need to worry about trailing seps */
		    n = wcslen(name);
		    if (name[n] == L'/' || name[n] == L'\\') {
			wcscpy(p, name); wcscat(p, de->d_name);
		    } else {
			wcscpy(p, name); wcscat(p, L"/"); wcscat(p, de->d_name);
		    }
		    /* printf("stat-ing %ls\n", p); */
		    _wstati64(p, &sb);
		    if (isReparsePoint(name)) ans += delReparsePoint(name);
		    else if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
			/* printf("is a directory\n"); */
			if (force) _wchmod(p, _S_IWRITE);
			ans += R_unlink(p, recursive, force);
		    } else {
			if (force) _wchmod(p, _S_IWRITE);
			ans += (_wunlink(p) == 0) ? 0 : 1;
		    }
		}
		_wclosedir(dir);
	    } else { /* we were unable to read a dir */
		ans++;
	    }
	    ans += (R_rmdir(name) == 0) ? 0 : 1;
	    return ans;
	}
	/* drop through */
    } else if (isReparsePoint(name)) return delReparsePoint(name);
    
    return _wunlink(name) == 0 ? 0 : 1;
}

void R_CleanTempDir(void)
{
    if (Sys_TempDir) {
	int n = strlen(Sys_TempDir);
	/* Windows cannot delete the current working directory */
	SetCurrentDirectory(R_HomeDir());
	wchar_t w[2*(n+1)];
	mbstowcs(w, Sys_TempDir, n+1);
	R_unlink(w, 1, 1); /* recursive=TRUE, force=TRUE */
    }
}
#else
static int R_unlink(const char *name, int recursive, int force)
{
    struct stat sb;
    int res, res2;

    if (streql(name, ".") || streql(name, "..")) return 0;
    /* We cannot use R_FileExists here since it is false for broken
       symbolic links 
       if (!R_FileExists(name)) return 0; */
    res  = lstat(name, &sb);  /* better to be lstat */
    if (!res && force) chmod(name, sb.st_mode | S_IWUSR);

    if (!res && recursive) {
	DIR *dir;
	struct dirent *de;
	char p[PATH_MAX];
	int n, ans = 0;

	if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	    if ((dir = opendir(name)) != NULL) {
		while ((de = readdir(dir))) {
		    if (streql(de->d_name, ".") || streql(de->d_name, ".."))
			continue;
		    n = strlen(name);
		    if (name[n] == R_FileSep[0])
			snprintf(p, PATH_MAX, "%s%s", name, de->d_name);
		    else
			snprintf(p, PATH_MAX, "%s%s%s", name, R_FileSep,
				 de->d_name);
		    lstat(p, &sb);
		    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
			if (force) chmod(p, sb.st_mode | S_IWUSR | S_IXUSR);
			ans += R_unlink(p, recursive, force);
		    } else {
			if (force) chmod(p, sb.st_mode | S_IWUSR);
			ans += (unlink(p) == 0) ? 0 : 1;
		    }
		}
		closedir(dir);
	    } else { /* we were unable to read a dir */
		ans++;
	    }
	    ans += (rmdir(name) == 0) ? 0 : 1;
	    return ans;
	}
	/* drop through */
    }
    res2 = unlink(name);
    /* We want to return 0 if either unlink succeeded or 'name' did not exist */
    return (res2 == 0 || res != 0) ? 0 : 1;
}
#endif


/* Note that wildcards are allowed in 'names' */
#ifdef Win32
# include <dos_wglob.h>
static SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, j, nfiles, res, failures = 0, recursive, force;
    const wchar_t *names;
    wglob_t globbuf;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "x");
	recursive = asLogical(CADR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	force = asLogical(CADDR(args));
	if (force == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "force");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		names = filenameToWchar(STRING_ELT(fn, i), TRUE);
		//Rprintf("do_unlink(%ls)\n", names);
		res = dos_wglob(names, GLOB_NOCHECK, NULL, &globbuf);
		if (res == GLOB_NOSPACE)
		    error(_("internal out-of-memory condition"));
		for (j = 0; j < globbuf.gl_pathc; j++)
		    failures += R_unlink(globbuf.gl_pathv[j], recursive, force);
		dos_wglobfree(&globbuf);
	    } else failures++;
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#else
# if defined(HAVE_GLOB) && defined(HAVE_GLOB_H)
#  include <glob.h>
# endif

static SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, nfiles, failures = 0, recursive, force;
    const char *names;
#if defined(HAVE_GLOB)
    int j, res;
    glob_t globbuf;
#endif

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "x");
	recursive = asLogical(CADR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	force = asLogical(CADDR(args));
	if (force == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "force");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		names = R_ExpandFileName(translateChar(STRING_ELT(fn, i)));
#if defined(HAVE_GLOB)
		res = glob(names, GLOB_NOCHECK, NULL, &globbuf);
# ifdef GLOB_ABORTED
		if (res == GLOB_ABORTED)
		    warning(_("read error on '%s'"), names);
# endif
# ifdef GLOB_NOSPACE
		if (res == GLOB_NOSPACE)
		    error(_("internal out-of-memory condition"));
# endif
		for (j = 0; j < globbuf.gl_pathc; j++)
		    failures += R_unlink(globbuf.gl_pathv[j], recursive, force);
		globfree(&globbuf);
	    } else failures++;
#else /* HAVE_GLOB */
	        failures += R_unlink(names, recursive, force);
	    } else failures++;
#endif
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#endif

static void chmod_one(const char *name)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX];
#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    int n;
#ifndef Win32
    mode_t mask = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR, /* 0644 */
	dirmask = mask | S_IXUSR | S_IXGRP | S_IXOTH; /* 0755 */
#endif

    if (streql(name, ".") || streql(name, "..")) return;
    if (!R_FileExists(name)) return;
#ifdef Win32
    _stati64(name, &sb);
    chmod(name, _S_IWRITE);
#else
    stat(name, &sb);
    chmod(name, (sb.st_mode | mask) & dirmask);
#endif
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
#ifndef Win32
	chmod(name, dirmask);
#endif
	if ((dir = opendir(name)) != NULL) {
	    while ((de = readdir(dir))) {
		if (streql(de->d_name, ".") || streql(de->d_name, ".."))
		    continue;
		n = strlen(name);
		if (name[n-1] == R_FileSep[0])
		    snprintf(p, PATH_MAX, "%s%s", name, de->d_name);
		else
		    snprintf(p, PATH_MAX, "%s%s%s", name, R_FileSep, de->d_name);
		chmod_one(p);
	    }
	    closedir(dir);
	} else { 
	    /* we were unable to read a dir */
	}
    }
}

/* recursively fix up permissions: used for R CMD INSTALL and build.
   NB: this overrides umask. */
static SEXP do_dirchmod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP dr;
    checkArity(op, args);
    dr = CAR(args);
    if(!isString(dr) || length(dr) != 1)
	error(_("invalid '%s' argument"), "dir");
    chmod_one(translateChar(STRING_ELT(dr, 0)));

    return R_NilValue;
}


static SEXP do_getlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int cat;
    char *p = NULL;

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if (cat == NA_INTEGER || cat < 0)
	error(_("invalid '%s' argument"), "category");
    switch(cat) {
    case 1: cat = LC_ALL; break;
    case 2: cat = LC_COLLATE; break;
    case 3: cat = LC_CTYPE; break;
    case 4: cat = LC_MONETARY; break;
    case 5: cat = LC_NUMERIC; break;
    case 6: cat = LC_TIME; break;
#ifdef LC_MESSAGES
    case 7: cat = LC_MESSAGES; break;
#endif
#ifdef LC_PAPER
    case 8: cat = LC_PAPER; break;
#endif
#ifdef LC_MEASUREMENT
    case 9: cat = LC_MEASUREMENT; break;
#endif
    default: cat = NA_INTEGER;
    }
    if (cat != NA_INTEGER) p = setlocale(cat, NULL);
    return mkString(p ? p : "");
}

extern void invalidate_cached_recodings(void);  /* from sysutils.c */

extern void resetICUcollator(void); /* from util.c */

/* Locale specs are always ASCII */
static SEXP do_setlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP locale = CADR(args), ans;
    int cat;
    const char *p;

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if (cat == NA_INTEGER || cat < 0)
	error(_("invalid '%s' argument"), "category");
    if (!isString(locale) || LENGTH(locale) != 1)
	error(_("invalid '%s' argument"), "locale");
    switch(cat) {
    case 1:
    {
	const char *l = CHAR(STRING_ELT(locale, 0));
	cat = LC_ALL;
	/* assume we can set LC_CTYPE iff we can set the rest */
	if ((p = setlocale(LC_CTYPE, l))) {
	    setlocale(LC_COLLATE, l);
	    resetICUcollator();
	    setlocale(LC_MONETARY, l);
	    setlocale(LC_TIME, l);
	    /* Need to return value of LC_ALL */
	    p = setlocale(cat, NULL);
	}
	break;
    }
    case 2:
	cat = LC_COLLATE;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	resetICUcollator();
	break;
    case 3:
	cat = LC_CTYPE;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 4:
	cat = LC_MONETARY;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 5:
	cat = LC_NUMERIC;
	warning(_("setting 'LC_NUMERIC' may cause R to function strangely"));
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 6:
	cat = LC_TIME;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#if defined LC_MESSAGES
    case 7:
	cat = LC_MESSAGES;
#ifdef Win32
/* this seems to exist in MinGW, but it does not work in Windows */
	warning(_("LC_MESSAGES exists on Windows but is not operational"));
	p = NULL;
#else
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
#endif
	break;
#endif
#ifdef LC_PAPER
    case 8:
	cat = LC_PAPER;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#endif
#ifdef LC_MEASUREMENT
    case 9:
	cat = LC_MEASUREMENT;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#endif
    default:
	p = NULL; /* -Wall */
	error(_("invalid '%s' argument"), "category");
    }
    PROTECT(ans = allocVector(STRSXP, 1));
    if (p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  {
	SET_STRING_ELT(ans, 0, mkChar(""));
	warning(_("OS reports request to set locale to \"%s\" cannot be honored"),
		CHAR(STRING_ELT(locale, 0)));
    }
    UNPROTECT(1);
    R_check_locale();
    invalidate_cached_recodings();
    return ans;
}



static SEXP do_localeconv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    struct lconv *lc = localeconv();
    int i = 0;
    char buff[20];

    PROTECT(ans = allocVector(STRSXP, 18));
    PROTECT(ansnames = allocVector(STRSXP, 18));
    SET_STRING_ELT(ans, i, mkChar(lc->decimal_point));
    SET_STRING_ELT(ansnames, i++, mkChar("decimal_point"));
    SET_STRING_ELT(ans, i, mkChar(lc->thousands_sep));
    SET_STRING_ELT(ansnames, i++, mkChar("thousands_sep"));
    SET_STRING_ELT(ans, i, mkChar(lc->grouping));
    SET_STRING_ELT(ansnames, i++, mkChar("grouping"));
    SET_STRING_ELT(ans, i, mkChar(lc->int_curr_symbol));
    SET_STRING_ELT(ansnames, i++, mkChar("int_curr_symbol"));
    SET_STRING_ELT(ans, i, mkChar(lc->currency_symbol));
    SET_STRING_ELT(ansnames, i++, mkChar("currency_symbol"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_decimal_point));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_decimal_point"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_thousands_sep));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_thousands_sep"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_grouping));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_grouping"));
    SET_STRING_ELT(ans, i, mkChar(lc->positive_sign));
    SET_STRING_ELT(ansnames, i++, mkChar("positive_sign"));
    SET_STRING_ELT(ans, i, mkChar(lc->negative_sign));
    SET_STRING_ELT(ansnames, i++, mkChar("negative_sign"));
    sprintf(buff, "%d", (int)lc->int_frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("int_frac_digits"));
    sprintf(buff, "%d", (int)lc->frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("frac_digits"));
    sprintf(buff, "%d", (int)lc->p_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_cs_precedes"));
    sprintf(buff, "%d", (int)lc->p_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sep_by_space"));
    sprintf(buff, "%d", (int)lc->n_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_cs_precedes"));
    sprintf(buff, "%d", (int)lc->n_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sep_by_space"));
    sprintf(buff, "%d", (int)lc->p_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sign_posn"));
    sprintf(buff, "%d", (int)lc->n_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sign_posn"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

/* .Internal function for path.expand */
static SEXP do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "path");
    n = length(fn);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
        SEXP tmp = STRING_ELT(fn, i);
        if (tmp != NA_STRING) {
            tmp = markKnown(R_ExpandFileName(translateChar(tmp)), tmp);
        }
	SET_STRING_ELT(ans, i, tmp);
    }
    UNPROTECT(1);
    return ans;
}

#ifdef Unix
static int var_R_can_use_X11 = -1;

extern Rboolean R_access_X11(void); /* from src/unix/X11.c */

static Rboolean R_can_use_X11(void)
{
    if (var_R_can_use_X11 < 0) {
#ifdef HAVE_X11
	if (strcmp(R_GUIType, "none") != 0) {
	    /* At this point we have permission to use the module, so try it */
	    var_R_can_use_X11 = R_access_X11();
	} else {
	    var_R_can_use_X11 = 0;
	}
#else
	var_R_can_use_X11 = 0;
#endif
    }

    return var_R_can_use_X11 > 0;
}
#endif

/* only actually used on Unix */
static SEXP do_capabilitiesX11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef Unix
    return ScalarLogical(R_can_use_X11());
#else
    return ScalarLogical(FALSE);
#endif
}

#define N_CAPABILITIES 16  /* must be increased as necessary */

static SEXP do_capabilities(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    int i = 0;
#ifdef Unix
# ifdef HAVE_X11
    int X11 = NA_LOGICAL;
# else
    int X11 = FALSE;
# endif
#endif

    checkArity(op, args);

    PROTECT(ans = allocVector(LGLSXP, N_CAPABILITIES));
    PROTECT(ansnames = allocVector(STRSXP, N_CAPABILITIES));

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("jpeg"));
#ifdef HAVE_JPEG
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("png"));
#ifdef HAVE_PNG
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("tiff"));
#ifdef HAVE_TIFF
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("tcltk"));
#ifdef HAVE_TCLTK
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("X11"));
#ifdef HAVE_X11
# if defined(Unix) /*  && !defined(__APPLE_CC__) removed in 2.11.0 */
    LOGICAL(ans)[i++] = X11;
# else
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("aqua"));
#ifdef HAVE_AQUA
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("http/ftp"));
#if HAVE_INTERNET
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("sockets"));
#ifdef HAVE_SOCKETS
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("libxml"));
#ifdef SUPPORT_LIBXML
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("fifo"));
#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    /* This one is complex.  Set it to be true only in interactive use,
       with the Windows and GNOME GUIs (but not Tk GUI) or under Unix
       if readline is available and in use. */
    SET_STRING_ELT(ansnames, i, mkChar("cledit"));
    LOGICAL(ans)[i] = FALSE;
#if defined(Win32)
    if (R_Interactive) LOGICAL(ans)[i] = TRUE;
#endif
#ifdef Unix
    if (strcmp(R_GUIType, "GNOME") == 0) {  /* always interactive */
	LOGICAL(ans)[i] = TRUE;  /* also AQUA ? */
    } else {
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
	extern Rboolean UsingReadline;
	if (R_Interactive && UsingReadline) LOGICAL(ans)[i] = TRUE;
#endif
    }
#endif
    i++;

    if (i >= N_CAPABILITIES) abort();
    /* always true as from R 2.10.0 */
    SET_STRING_ELT(ansnames, i, mkChar("iconv"));
    LOGICAL(ans)[i++] = TRUE;

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("NLS"));
#ifdef ENABLE_NLS
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("profmem"));
    LOGICAL(ans)[i++] = FALSE;  /* always - memory profiling no longer exists */

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("cairo"));
#ifdef HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = TRUE;
#elif defined(Win32)
{
    /* This is true iff winCairo.dll is available */
    struct stat sb;
    char path[1000];
    snprintf(path, 1000, "%s/library/grDevices/libs/%s/winCairo.dll", 
	     R_HomeDir(), R_ARCH);
    LOGICAL(ans)[i++] = stat(path, &sb) == 0;
}
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    if (i >= N_CAPABILITIES) abort();
    SET_STRING_ELT(ansnames, i, mkChar("ICU"));
    #ifdef USE_ICU
        LOGICAL(ans)[i++] = TRUE;
    #else
        LOGICAL(ans)[i++] = FALSE;
    #endif

    if (i != N_CAPABILITIES) abort();

    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

#if defined(HAVE_BSD_NETWORKING) && defined(HAVE_ARPA_INET_H)
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static SEXP do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    const char *name; char ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	error(_("'hostname' must be a character vector of length 1"));
    name = translateChar(STRING_ELT(CAR(args), 0));

    hp = gethostbyname(name);

    if (hp == NULL) {		/* cannot resolve the address */
	warning(_("nsl() was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warning(_("unknown format returned by gethostbyname"));
	}
	ans = mkString(ip);
    }
    return ans;
}
#else
static SEXP do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning(_("nsl() is not supported on this platform"));
    return R_NilValue;
}
#endif

static SEXP do_sysgetpid(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarInteger(getpid());
}


/* NB: we save errno immediately after the call here.  This should not
  be necessary on a POSIX OS, but it is on Windows, where it seems
  that on some versions strerror itself changes errno (something
  allowed in C99 but disallowed in POSIX).  Also, something under
  warning() might set errno in a future version.
*/
#ifndef Win32
/* mkdir is defined in <sys/stat.h> */
static SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP path;
    int res, show, recursive, mode, serrno = 0;
    char *p, dir[PATH_MAX];

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    mode = asInteger(CADDDR(args));
    if (mode == NA_LOGICAL) mode = 0777;
    strcpy(dir, R_ExpandFileName(translateChar(STRING_ELT(path, 0))));
    /* remove trailing slashes */
    p = dir + strlen(dir) - 1;
    while (*p == '/' && strlen(dir) > 1) *p-- = '\0';
    if (recursive) {
	p = dir;
	while ((p = Rf_strchr(p+1, '/'))) {
	    *p = '\0';
	    res = mkdir(dir, mode);
	    /* Solaris 10 returns ENOSYS on automount, PR#13834
	       EROFS is allowed by POSIX, so we skip that too */
	    serrno = errno;
	    if (res && serrno != EEXIST && serrno != ENOSYS && serrno != EROFS) 
		goto end;
	    *p = '/';
	}
    }
    res = mkdir(dir, mode);
    serrno = errno;
    if (show && res && serrno == EEXIST)
	warning(_("'%s' already exists"), dir);
end:
    if (show && res && serrno != EEXIST)
	warning(_("cannot create dir '%s', reason '%s'"), dir,
		strerror(serrno));
    return ScalarLogical(res == 0);
}
#else /* Win32 */
#include <io.h> /* mkdir is defined here */
static SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path;
    wchar_t *p, dir[MAX_PATH];
    int res, show, recursive, serrno = 0;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    wcscpy(dir, filenameToWchar(STRING_ELT(path, 0), TRUE));
    for (p = dir; *p; p++) if (*p == L'/') *p = L'\\';
    /* remove trailing slashes */
    p = dir + wcslen(dir) - 1;
    while (*p == L'\\' && wcslen(dir) > 1 && *(p-1) != L':') *p-- = L'\0';
    if (recursive) {
	p = dir;
	/* skip leading \\share */
	if (*p == L'\\' && *(p+1) == L'\\') {
	    p += 2;
	    p = wcschr(p, L'\\');
	}
	while ((p = wcschr(p+1, L'\\'))) {
	    *p = L'\0';
	    if (*(p-1) != L':') {
		res = _wmkdir(dir);
		serrno = errno;
		if (res && serrno != EEXIST) goto end;
	    }
	    *p = L'\\';
	}
    }
    res = _wmkdir(dir);
    serrno = errno;
    if (show && res && serrno == EEXIST)
	warning(_("'%ls' already exists"), dir);
    return ScalarLogical(res == 0);
end:
    if (show && res && serrno != EEXIST)
	warning(_("cannot create dir '%ls', reason '%s'"), dir, 
		strerror(serrno));
    return ScalarLogical(res == 0);
}
#endif

/* take file/dir 'name' in dir 'from' and copy it to 'to' 
   'from', 'to' should have trailing path separator if needed.
*/
#ifdef Win32
static int do_copy(const wchar_t* from, const wchar_t* name,
		   const wchar_t* to, int over, int recursive, int perms)
{
    struct _stati64 sb;
    int nc, nfail = 0, res;
    wchar_t dest[PATH_MAX], this[PATH_MAX];

    wsprintfW(this, L"%ls%ls", from, name);
    _wstati64(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	_WDIR *dir;
	struct _wdirent *de;
	wchar_t p[PATH_MAX];

	if (!recursive) return 1;
	nc = wcslen(to);
	wsprintfW(dest, L"%ls%ls", to, name);
	/* We could set the mode (only the 200 part matters) later */
	res = _wmkdir(dest);
	if (res && errno != EEXIST) {
	    warning(_("problem creating directory %ls: %s"), 
		    this, strerror(errno));
	    return 1;
	}
	// NB Windows' mkdir appears to require \ not /.
	wcscat(dest, L"\\");
	if ((dir = _wopendir(this)) != NULL) {
	    while ((de = _wreaddir(dir))) {
		if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
		    continue;
		wsprintfW(p, L"%ls%\\%ls", name, de->d_name);
		do_copy(from, p, to, over, recursive, perms);
	    }
	    _wclosedir(dir);
	} else {
	    warning(_("problem reading dir %ls: %s"), this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;
	wchar_t buf[APPENDBUFSIZE];

	nfail = 0;
	nc = wcslen(to);
	wsprintfW(dest, L"%ls%ls", to, name);
	if (over || !R_WFileExists(dest)) { /* FIXME */
	    if ((fp1 = _wfopen(this, L"rb")) == NULL ||
		(fp2 = _wfopen(dest, L"wb")) == NULL) {
		warning(_("problem copying %ls to %ls: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp2) != APPENDBUFSIZE) {
		    nfail++;
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		goto copy_error;
	    }
	}
	/* FIXME: perhaps manipulate mode as we do in Sys.chmod? */
	if(perms) _wchmod(dest, sb.st_mode & 0777);
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    return nfail;
}

/* file.copy(files, dir, over, recursive=TRUE, perms), only */
static SEXP do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, to, ans;
    wchar_t *p, dir[PATH_MAX+1], from[PATH_MAX+1], name[PATH_MAX+1];
    int i, nfiles, over, recursive, perms, nfail;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    PROTECT(ans = allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	to = CADR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	over = asLogical(CADDR(args));
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "over");
	recursive = asLogical(CADDDR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	perms = asLogical(CAD4R(args));
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	wcsncpy(dir,
		filenameToWchar(STRING_ELT(to, 0), TRUE),
		PATH_MAX-1);
        dir[PATH_MAX-1] = L'\0';
	if (dir[0] == L'\0' || dir[wcslen(dir)-1] !=  L'\\')
	    wcscat(dir, L"\\");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		wcsncpy(from, 
			filenameToWchar(STRING_ELT(fn, i), TRUE),
			PATH_MAX-1);
                from[PATH_MAX-1] = L'\0';
		/* If there was a trailing sep, this is a mistake */
		if (from[0] != L'\0' && from[wcslen(from)-1] == L'\\') 
                    from[wcslen(from)-1] = L'\0';
		p = wcsrchr(from, L'\\') ;
		if (p) {
		    wcscpy(name, p+1);
		    *(p+1) = L'\0';
		} 
                else if (wcslen(from) > 2 && from[1] == L':') {
		    wcscpy(name, from+2);
                    from[2] = L'\0';
                }
		else {
		    wcscpy(name, from);
		    wcscpy(from, L".\\");
		}
		nfail = do_copy(from, name, dir, over, recursive, perms);
	    } else nfail = 1;
	    LOGICAL(ans)[i] = (nfail == 0);
	}
    }
    UNPROTECT(1);
    return ans;
}

#else

static int do_copy(const char* from, const char* name, const char* to,
		   int over, int recursive, int perms)
{
    struct stat sb;
    int nc, nfail = 0, res, mask;
    char dest[PATH_MAX], this[PATH_MAX];

#ifdef HAVE_UMASK
    int um = umask(0); umask(um);
    mask = 0777 & ~um;
#else
    mask = 0777;
#endif
    /* REprintf("from: %s, name: %s, to: %s\n", from, name, to); */
    snprintf(this, PATH_MAX, "%s%s", from, name);
    /* Here we want the target not the link */
    stat(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	DIR *dir;
	struct dirent *de;
	char p[PATH_MAX];

	if (!recursive) return 1;
	snprintf(dest, PATH_MAX, "%s%s", to, name);
	/* If a directory does not have write permission for the user,
	   we will fail to create files in that directory, so defer
	   setting mode */
	res = mkdir(dest, 0700);
	if (res && errno != EEXIST) {
	    warning(_("problem creating directory %s: %s"), 
		    this, strerror(errno));
	    return 1;
	}
	strcat(dest, "/");
	if ((dir = opendir(this)) != NULL) {
	    while ((de = readdir(dir))) {
		if (streql(de->d_name, ".") || streql(de->d_name, ".."))
		    continue;
		snprintf(p, PATH_MAX, "%s/%s", name, de->d_name);
		do_copy(from, p, to, over, recursive, perms);
	    }
	    closedir(dir);
	} else {
	    warning(_("problem reading directory %s: %s"), 
		    this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
	chmod(dest, perms ? (sb.st_mode & mask): mask);
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;
	char buf[APPENDBUFSIZE];

	nfail = 0;
	snprintf(dest, PATH_MAX, "%s%s", to, name);
	if (over || !R_FileExists(dest)) {
	    /* REprintf("copying %s to %s\n", this, dest); */
	    if ((fp1 = R_fopen(this, "rb")) == NULL ||
		(fp2 = R_fopen(dest, "wb")) == NULL) {
		warning(_("problem copying %s to %s: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp2) != APPENDBUFSIZE) {
		    nfail++;
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		goto copy_error;
	    }
	}
	if(perms) chmod(dest, sb.st_mode & mask);
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    return nfail;
}

/* file.copy(files, dir, recursive), only */
static SEXP do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, to, ans;
    char *p, dir[PATH_MAX+1], from[PATH_MAX+1], name[PATH_MAX+1];
    int i, nfiles, over, recursive, perms, nfail;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    PROTECT(ans = allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	to = CADR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	over = asLogical(CADDR(args));
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "over");
	recursive = asLogical(CADDDR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	perms = asLogical(CAD4R(args));
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	strncpy(dir, 
		R_ExpandFileName(translateChar(STRING_ELT(to, 0))),
		PATH_MAX-1);
        dir[PATH_MAX-1] = 0;
	if (dir[0] == '\0' || dir[strlen(dir)-1] !=  '/')
	    strcat(dir, "/");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		strncpy(from, 
			R_ExpandFileName(translateChar(STRING_ELT(fn, i))),
			PATH_MAX-1);
                from[PATH_MAX-1] = 0;
		/* If there is a trailing sep, this is a mistake */
		if (from[0] != '\0' && from[strlen(from)-1] == '/') 
                    from[strlen(from)-1] = 0;
		p = strrchr(from, '/') ;
		if (p) {
		    strcpy(name, p+1);
		    *(p+1) = '\0';
		} 
                else {
		    strcpy(name, from);
		    strcpy(from, "./");
		}
		nfail = do_copy(from, name, dir, over, recursive, perms);
	    } else nfail = 1;
	    LOGICAL(ans)[i] = (nfail == 0);
	}
    }
    UNPROTECT(1);
    return ans;
}
#endif

static SEXP do_l10n_info(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef Win32
    int len = 4;
#else
    int len = 3;
#endif
    SEXP ans, names;
    checkArity(op, args);
    PROTECT(ans = allocVector(VECSXP, len));
    PROTECT(names = allocVector(STRSXP, len));
    SET_STRING_ELT(names, 0, mkChar("MBCS"));
    SET_STRING_ELT(names, 1, mkChar("UTF-8"));
    SET_STRING_ELT(names, 2, mkChar("Latin-1"));
    SET_VECTOR_ELT(ans, 0, ScalarLogical(mbcslocale));
    SET_VECTOR_ELT(ans, 1, ScalarLogical(utf8locale));
    SET_VECTOR_ELT(ans, 2, ScalarLogical(latin1locale));
#ifdef Win32
    SET_STRING_ELT(names, 3, mkChar("codepage"));
    SET_VECTOR_ELT(ans, 3, ScalarInteger(localeCP));
#endif
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return ans;
}

/* do_normalizepath moved to util.c in R 2.13.0 */

static SEXP do_syschmod(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_CHMOD
    SEXP paths, smode, ans;
    int i, m, n, *modes, mode, res, um = 0;

    checkArity(op, args);
    paths = CAR(args);
    if (!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    PROTECT(smode = coerceVector(CADR(args), INTSXP));
    modes = INTEGER(smode);
    m = LENGTH(smode);
    if(!m && n) error(_("'mode' must be of length at least one"));
    int useUmask = asLogical(CADDR(args));
    if (useUmask == NA_LOGICAL)
	error(_("invalid '%s' argument"), "use_umask");
#ifdef HAVE_UMASK
    um = umask(0); umask(um);
#endif
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	mode = modes[i % m];
	if (mode == NA_INTEGER) mode = 0777;
#ifdef HAVE_UMASK
	if(useUmask) mode = mode & ~um;
#endif
#ifdef Win32
	/* Windows' _[w]chmod seems only to support read access
	   or read-write access.  _S_IWRITE is 0200.
	*/
	mode = (mode & 0200) ? (_S_IWRITE | _S_IREAD): _S_IREAD;
#endif
	if (STRING_ELT(paths, i) != NA_STRING) {
#ifdef Win32
	    res = _wchmod(filenameToWchar(STRING_ELT(paths, i), TRUE), mode);
#else
	    res = chmod(R_ExpandFileName(translateChar(STRING_ELT(paths, i))),
			mode);
#endif
	} else res = 1;
	LOGICAL(ans)[i] = (res == 0);
    }
    UNPROTECT(2);
    return ans;
#else
    SEXP paths, ans;
    int i, n;

    checkArity(op, args);
    paths = CAR(args);
    if (!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    warning("insufficient OS support on this platform");
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) LOGICAL(ans)[i] = 0;
    UNPROTECT(1);
    return ans;
#endif
}

static SEXP do_sysumask(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    int mode;
    mode_t res = 0;

    checkArity(op, args);
    mode = asInteger(CAR(args));
#ifdef HAVE_UMASK
    if (mode == NA_INTEGER) {
	res = umask(0);
	umask(res);
	R_Visible = TRUE;
    } else {
	res = umask(mode);
	R_Visible = FALSE;
    }
#else
    warning(_("insufficient OS support on this platform"));
    R_Visible = FALSE;
#endif
    PROTECT(ans = ScalarInteger(res));
    setAttrib(ans, R_ClassSymbol, mkString("octmode"));
    UNPROTECT(1);
    return ans;
}

static SEXP do_readlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP paths, ans;
    int n;
#ifdef HAVE_READLINK
    char buf[PATH_MAX+1];
    ssize_t res;
    int i;
#endif

    checkArity(op, args);
    paths = CAR(args);
    if(!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    PROTECT(ans = allocVector(STRSXP, n));
#ifdef HAVE_READLINK
    for (i = 0; i < n; i++) {
	memset(buf, 0, PATH_MAX+1);
	res = readlink(R_ExpandFileName(translateChar(STRING_ELT(paths, i))),
		       buf, PATH_MAX);
	if (res >= 0) SET_STRING_ELT(ans, i, mkChar(buf));
	else if (errno == EINVAL) SET_STRING_ELT(ans, i, mkChar(""));
	else SET_STRING_ELT_NA(ans, i);
    }
#endif
    UNPROTECT(1);
    return ans;
}


static SEXP do_Cstack_info(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, nms;

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 4));
    PROTECT(nms = allocVector(STRSXP, 4));
    INTEGER(ans)[0] = R_CStackLimit == -1 ? NA_INTEGER : R_CStackLimit;
    INTEGER(ans)[1] = R_CStackLimit == -1 ? NA_INTEGER :
                      R_CStackDir < 0     ? (uintptr_t) &ans - R_CStackStart
                                          : R_CStackStart - (uintptr_t) &ans;
    INTEGER(ans)[2] = R_CStackDir < 0 ? -1 : 1;
    INTEGER(ans)[3] = R_EvalDepth;
    SET_STRING_ELT(nms, 0, mkChar("size"));
    SET_STRING_ELT(nms, 1, mkChar("current"));
    SET_STRING_ELT(nms, 2, mkChar("direction"));
    SET_STRING_ELT(nms, 3, mkChar("eval_depth"));

    UNPROTECT(2);
    setAttrib(ans, R_NamesSymbol, nms);
    return ans;
}

#ifdef Win32
static int winSetFileTime(const char *fn, time_t ftime)
{
    SYSTEMTIME st;
    FILETIME modft;
    struct tm *utctm;
    HANDLE hFile;

    utctm = gmtime(&ftime);
    if (!utctm) return 0;

    st.wYear         = (WORD) utctm->tm_year + 1900;
    st.wMonth        = (WORD) utctm->tm_mon + 1;
    st.wDayOfWeek    = (WORD) utctm->tm_wday;
    st.wDay          = (WORD) utctm->tm_mday;
    st.wHour         = (WORD) utctm->tm_hour;
    st.wMinute       = (WORD) utctm->tm_min;
    st.wSecond       = (WORD) utctm->tm_sec;
    st.wMilliseconds = (WORD) 0;
    if (!SystemTimeToFileTime(&st, &modft)) return 0;

    hFile = CreateFile(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		       FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return 0;
    int res  = SetFileTime(hFile, NULL, NULL, &modft);
    CloseHandle(hFile);
    return res != 0; /* success is non-zero */
}
#else
# ifdef HAVE_UTIMES
#  include <sys/time.h>
# elif defined(HAVE_UTIME)
#  include <utime.h>
# endif
#endif

SEXP attribute_hidden R_setFileTime(SEXP name, SEXP time)
{
    const char *fn = translateChar(STRING_ELT(name, 0));
    int ftime = asInteger(time), res;

#ifdef Win32
    res  = winSetFileTime(fn, (time_t)ftime);
#elif defined(HAVE_UTIMES)
    struct timeval times[2];

    times[0].tv_sec = times[1].tv_sec = ftime;
    times[0].tv_usec = times[1].tv_usec = 0;
    res = utimes(fn, times) == 0;
#elif defined(HAVE_UTIME)
    struct utimbuf settime;

    settime.actime = settime.modtime = ftime;
    res = utime(fn, &settime) == 0;
#endif
    return ScalarLogical(res);
}

#ifdef Win32
/* based on ideas in
   http://www.codeproject.com/KB/winsdk/junctionpoints.aspx
*/
typedef struct TMN_REPARSE_DATA_BUFFER
{
    DWORD  ReparseTag;
    WORD   ReparseDataLength;
    WORD   Reserved;
    WORD   SubstituteNameOffset;
    WORD   SubstituteNameLength;
    WORD   PrintNameOffset;
    WORD   PrintNameLength;
    WCHAR  PathBuffer[1024];
} TMN_REPARSE_DATA_BUFFER;

static SEXP do_mkjunction(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    wchar_t from[10000];
    const wchar_t *to;

    checkArity(op, args);
    /* from and to are both directories: and to exists */
    wcscpy(from, filenameToWchar(STRING_ELT(CAR(args), 0), FALSE));
    to = filenameToWchar(STRING_ELT(CADR(args), 0), TRUE);
    // printf("ln %ls %ls\n", from, to);
    
    HANDLE hd = 
	CreateFileW(to, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING,
		    FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
		    0);
    if(hd == INVALID_HANDLE_VALUE) {
	warning("cannot open reparse point '%ls', reason '%s'",
		to, formatError(GetLastError()));
	return ScalarLogical(1);
    }
    TMN_REPARSE_DATA_BUFFER rdb;
    const size_t nbytes = wcslen(from) * 2;
    rdb.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
    rdb.ReparseDataLength = nbytes + 12;
    wcscpy(rdb.PathBuffer, from);
    rdb.Reserved = 0;
    rdb.SubstituteNameOffset = 0;
    rdb.SubstituteNameLength = nbytes;
    rdb.PrintNameOffset = nbytes + 2;
    rdb.PrintNameLength = 0;
    DWORD dwBytes;
    const BOOL bOK =
	DeviceIoControl(hd, FSCTL_SET_REPARSE_POINT, &rdb, 
			8 /* header */ + rdb.ReparseDataLength,
			NULL, 0, &dwBytes, 0);
    CloseHandle(hd);
    if(!bOK)
	warning("cannot set reparse point '%ls', reason '%s'",
		to, formatError(GetLastError()));
    return ScalarLogical(bOK != 0);
}
#endif

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_platform[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"date",	do_date,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"file.show",	do_fileshow,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.edit",	do_fileedit,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.append",	do_fileappend,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"codeFiles.append",do_fileappend,1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	do_filecreate,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.remove",	do_fileremove,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.symlink",do_filesymlink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.link",	do_filelink,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.rename",	do_filerename,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.info",	do_fileinfo,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"R.home",	do_Rhome,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", do_fileexists,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.choose", do_filechoose,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.access",	do_fileaccess,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"unlink",	do_unlink,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"dirchmod",	do_dirchmod,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",do_getlocale,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setlocale",do_setlocale,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.localeconv",do_localeconv,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"path.expand",	do_pathexpand,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilitiesX11",do_capabilitiesX11,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilities",do_capabilities,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"nsl",		do_nsl,		0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"Sys.getpid",	do_sysgetpid,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.create",	do_dircreate,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"file.copy",	do_filecopy,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"l10n_info",	do_l10n_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.chmod",	do_syschmod,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.umask",	do_sysumask,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.readlink", do_readlink,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Cstack_info", do_Cstack_info,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

#ifdef Win32
{"mkjunction", do_mkjunction,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
