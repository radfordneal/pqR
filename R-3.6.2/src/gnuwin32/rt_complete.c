/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file rt_complete.c
 *  Copyright (C) 2007-2017 The R Core Team.
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <getline/getline.h>
#include <string.h>
#include <stdlib.h> /* for getenv */

#ifndef min
/* in stdlib.h in Win64 headers */
# define min(a, b) (a < b ? a : b)
#endif

#include <Rinternals.h>
#include <R_ext/Parse.h>

static int completion_available = -1;

static int gl_tab(char *buf, int offset, int *loc)
/* default tab handler, acts like tabstops every 8 cols */
{
    int i, count, len;

    len = strlen(buf);
    count = 8 - (offset + *loc) % 8;
    for (i=len; i >= *loc; i--)
	buf[i+count] = buf[i];
    for (i=0; i < count; i++)
	buf[*loc+i] = ' ';
    i = *loc;
    *loc = i + count;
    return i;
}

static int rt_completion(char *buf, int offset, int *loc)
{
    int i, alen, cursor_position = *loc;
    char *partial_line = buf;
    const char *additional_text;
    SEXP cmdSexp, cmdexpr, ans = R_NilValue;
    ParseStatus status;

    if(!completion_available) return gl_tab(buf, offset, loc);

    if(completion_available < 0) {
	char *p = getenv("R_COMPLETION");
	if(p && strcmp(p, "FALSE") == 0) {
	    completion_available = 0;
	    return gl_tab(buf, offset, loc);
	}
	/* First check if namespace is loaded */
	if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	   != R_UnboundValue) completion_available = 1;
	else { /* Then try to load it */
	    char *p = "try(loadNamespace('utils'), silent=TRUE)";
	    PROTECT(cmdSexp = mkString(p));
	    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
	    if(status == PARSE_OK) {
		for(i = 0; i < length(cmdexpr); i++)
		    eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
	    }
	    UNPROTECT(2);
	    if(findVarInFrame(R_NamespaceRegistry, install("utils"))
	       != R_UnboundValue) completion_available = 1;
	    else {
		completion_available = 0;
		return -1; /* no change */
	    }
	}
    }

    alen = strlen(partial_line);
    char orig[alen + 1], pline[2*alen + 1],
            *pchar = pline, achar;
    strcpy(orig, partial_line);
    for (i = 0; i < alen; i++) {
        achar = orig[i];
	if (achar == '"' || achar == '\\') *pchar++ = '\\';
	*pchar++ = achar;
    }
    *pchar = 0;
    size_t len = strlen(pline) + 100; 
    char cmd[len];
    snprintf(cmd, len,
	     "utils:::.win32consoleCompletion(\"%s\", %d)",
	     pline, cursor_position);
    PROTECT(cmdSexp = mkString(cmd));
    cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
    if (status != PARSE_OK) {
	UNPROTECT(2);
	/* Uncomment next line to debug */
	/* Rprintf("failed: %s \n", cmd); */
	/* otherwise pretend that nothing happened and return */
	return -1; /* no change */
    }
    /* Loop is needed here as EXPRSEXP will be of length > 1 */
    for(i = 0; i < length(cmdexpr); i++)
	ans = eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
    UNPROTECT(2);

    /* ans has the form list(addition, possible), where 'addition' is
       unique additional text if any, and 'possible' is a character
       vector holding possible completions if any (already formatted
       for linewise printing in the current implementation).  If
       'possible' has any content, we want to print those (or show in
       status bar or whatever).  Otherwise add the 'additional' text
       at the cursor */

#define ADDITION 0
#define POSSIBLE 1

    alen = length(VECTOR_ELT(ans, POSSIBLE));
    if (alen) {
	int max_show = 10;
	printf("\n"); /* finish current line */
	for (i = 0; i < min(alen, max_show); i++) {
	    printf("%s\n", CHAR(STRING_ELT(VECTOR_ELT(ans, POSSIBLE), i)));
	}
	if (alen > max_show)
	    printf("\n[...truncated]\n");
	cursor_position = -2; /* Need to redisplay whole line */
    }
    additional_text = CHAR(STRING_ELT( VECTOR_ELT(ans, ADDITION), 0 ));
    alen = strlen(additional_text);
    if (alen) {
	int cp = *loc;
	memcpy(buf+cp, additional_text, alen+1);
	*loc = cp + alen;
    }
    return cursor_position;
}


void R_gl_tab_set(void)
{
    gl_tab_hook = rt_completion;
}
