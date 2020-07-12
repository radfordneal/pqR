/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2019   The R Core Team.
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
 */

/* <UTF8> OK since this is intended to treat chars as byte streams */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#undef _

#include "tools.h"
#define ROL_UNUSED
#include "md5.h"

/* .Call so manages R_alloc stack */
SEXP Rmd5(SEXP files)
{
    SEXP ans;
    int i, j, nfiles = length(files), res;
#ifdef _WIN32
    const wchar_t *wpath;
#else
    const char *path;
#endif
    char out[33];
    FILE *fp;
    unsigned char resblock[16];

    if(!isString(files)) error(_("argument 'files' must be character"));
    PROTECT(ans = allocVector(STRSXP, nfiles));
    for(i = 0; i < nfiles; i++) {
#ifdef _WIN32
	wpath = filenameToWchar(STRING_ELT(files, i), FALSE);
	fp = _wfopen(wpath, L"rb");
#else
	path = translateChar(STRING_ELT(files, i));
	fp = fopen(path, "r");
#endif
	if(!fp) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    res = md5_stream(fp, &resblock);
	    if(res) {
#ifdef _WIN32
		warning(_("md5 failed on file '%ls'"), wpath);
#else
		warning(_("md5 failed on file '%s'"), path);
#endif
		SET_STRING_ELT(ans, i, NA_STRING);
	    } else {
		for(j = 0; j < 16; j++)
		    sprintf (out+2*j, "%02x", resblock[j]);
		SET_STRING_ELT(ans, i, mkChar(out));
	    }
	    fclose(fp);
	}
    }
    UNPROTECT(1);
    return ans;
}
