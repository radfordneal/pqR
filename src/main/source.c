/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-12     The R Core Team
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
#include <Internal.h>
#include <Fileio.h>
#include <IOStuff.h>
#include <Parse.h>
#include <Rconnections.h>

extern IoBuffer R_ConsoleIob;

/* Return a vector of string swith the recent context of a parse error. */

SEXP attribute_hidden getParseContext(void)
{
    int i, last = PARSE_CONTEXT_SIZE;
    char context[PARSE_CONTEXT_SIZE+1];

    SEXP ans = R_NilValue, ans2;
    int nn, nread;
    char c;

    context[last] = '\0';
    for (i=R_ParseContextLast; last>0 ; i += PARSE_CONTEXT_SIZE - 1) {
	i = i % PARSE_CONTEXT_SIZE;
	context[--last] = R_ParseContext[i];
	if (!context[last]) {
	    last++;
	    break;
	}
    }

    /* Initially allocate space for 16 lines.  (Do we really ever want more?) */
    nn = 16; 
    PROTECT(ans = allocVector(STRSXP, nn));
    c = context[last];
    nread = 0;
    while(c) {
	nread++;
	if(nread >= nn) {
	    ans2 = allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	i = last;
	while((c = context[i++])) {
	    if(c == '\n') break;
	}
	context[i-1] = '\0';
	SET_STRING_ELT(ans, nread-1, mkChar(context + last));
	last = i;
    }
    /* get rid of empty line after last newline */
    if (nread && !length(STRING_ELT(ans, nread-1))) {
    	nread--;
    	R_ParseContextLine--;
    }
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    return ans2;
}

static void getParseFilename(char* buffer, size_t buflen)
{
    buffer[0] = '\0';

    if (R_ParseErrorFile && R_ParseErrorFile != R_NilValue) {

        if (isEnvironment(R_ParseErrorFile)) {
            SEXP filename;
            PROTECT(filename = findVar(install("filename"), R_ParseErrorFile));
            if (isString(filename) && length(filename))
                strncpy(buffer, CHAR(STRING_ELT(filename, 0)), buflen - 1);
            UNPROTECT(1);
        } 
        else if (isString(R_ParseErrorFile) && length(R_ParseErrorFile)) 
            strncpy(buffer, CHAR(STRING_ELT(R_ParseErrorFile, 0)), buflen - 1);

        buffer[buflen-1] = 0;
    }
}

static SEXP tabExpand(SEXP strings)
{
    int i;
    char buffer[200], *b;
    const char *input;
    int len = length(strings);
    SEXP result;
    PROTECT(strings);
    PROTECT(result = allocVector(STRSXP, len));
    for (i = 0; i < len; i++) {
    	input = CHAR(STRING_ELT(strings, i));
    	for (b = buffer; *input && (b-buffer < 192); input++) {
    	    if (*input == '\t') do {
    	    	*b++ = ' ';
    	    } while (((b-buffer) & 7) != 0);
    	    else *b++ = *input;
    	}
    	*b = '\0';
    	SET_STRING_ELT(result, i, mkCharCE(buffer, Rf_getCharCE(STRING_ELT(strings, i))));
    }
    UNPROTECT(2);
    return result;
}
    	
void parseError(SEXP call, int linenum)
{
    SEXP context;
    int len, width;
    char filename[128], buffer[10];
    PROTECT(context = tabExpand(getParseContext()));
    len = length(context);
    if (linenum) {
	getParseFilename(filename, sizeof(filename)-2);
	if (strlen(filename)) strcpy(filename + strlen(filename), ":");

	switch (len) {
	case 0:
	    error(_("%s%d:%d: %s"),
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg);
	    break;
	case 1: // replaces use of %n
	    width = sprintf(buffer, "%d: ", R_ParseContextLine); 
	    error(_("%s%d:%d: %s\n%d: %s\n%*s"),
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg,
		  R_ParseContextLine, CHAR(STRING_ELT(context, 0)), 
		  width+R_ParseErrorCol, "^");
	    break;
	default:
	    width = sprintf(buffer, "%d: ", R_ParseContextLine);
	    error(_("%s%d:%d: %s\n%d: %s\n%d: %s\n%*s"),
		  filename, linenum, R_ParseErrorCol, R_ParseErrorMsg,
		  R_ParseContextLine-1, CHAR(STRING_ELT(context, len-2)),
		  R_ParseContextLine, CHAR(STRING_ELT(context, len-1)), 
		  width+R_ParseErrorCol, "^");
	    break;
	}
    } else {
	switch (len) {
	case 0:
	    error(_("%s"), R_ParseErrorMsg);
	    break;
	case 1:
	    error(_("%s in \"%s\""),
		  R_ParseErrorMsg, CHAR(STRING_ELT(context, 0)));
	    break;
	default:
	    error(_("%s in:\n\"%s\n%s\""),
		  R_ParseErrorMsg, CHAR(STRING_ELT(context, len-2)),
		  CHAR(STRING_ELT(context, len-1)));
	    break;
	}
    }
    UNPROTECT(1);
}

/* "do_parse" - the user interface for parsing

   .Internal( parse(file, n, text, prompt, srcfile, encoding) )
   If there is text then that is read and the other arguments are ignored. */

static void conn_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

static unsigned char console_buf[CONSOLE_BUFFER_SIZE+1];
static unsigned char *console_bufp;

static int console_getc (void *prompt_string)
{
    while (*console_bufp == 0) {
        if (R_ReadConsole ((const char *) prompt_string, console_buf, 
                           CONSOLE_BUFFER_SIZE, 1) == 0)
            return EOF;
        console_buf[CONSOLE_BUFFER_SIZE] = 0;  /* just in case... */
        console_bufp = console_buf;
    }

    return *console_bufp++;
}

/* .Internal( parse(file, n, text, prompt, srcfile, encoding) ) */

static SEXP do_parse(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file, text, prompt, s, source, encarg;
    const char *prompt_string;
    Rconnection con;
    Rboolean old_latin1 = known_to_be_latin1, old_utf8 = known_to_be_utf8;
    Rboolean wasopen;
    int ifile, num, i;
    const char *encoding;
    ParseStatus status;
    RCNTXT cntxt;

    checkArity(op, args);
    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    file = CAR(args); args = CDR(args);
    num = asInteger(CAR(args)); args = CDR(args);
    text = CAR(args); args = CDR(args);
    prompt = CAR(args); args = CDR(args);
    source = CAR(args); args = CDR(args);
    encarg = CAR(args);

    if (text != R_NilValue && TYPEOF(text) != STRSXP)
        errorcall (call, _("coercion of 'text' to character was unsuccessful"));

    if (num == 0 || TYPEOF(text) == STRSXP && LENGTH(text) == 0)
        return allocVector (EXPRSXP, 0);

    if (prompt != R_NilValue) prompt = coerceVector (prompt, STRSXP);
    PROTECT(prompt);

    if (!isString(encarg) || LENGTH(encarg) != 1)
        error(_("invalid '%s' value"), "encoding");
    encoding = CHAR(STRING_ELT(encarg,0));

    /* allow 'encoding' to override declaration on 'text'. */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;
    Rboolean allKnown = TRUE;
    if (streql(encoding, "latin1")) {
        known_to_be_latin1 = TRUE;
        allKnown = FALSE;
    }
    else if (streql(encoding, "UTF-8"))  {
        known_to_be_utf8 = TRUE;
        allKnown = FALSE;
    }
    else if (!streql(encoding, "unknown") && !streql(encoding, "native.enc")) 
            warning(_("argument '%s = \"%s\"' will be ignored"), 
                "encoding", encoding);

    if (text != R_NilValue) {

        /* If 'text' has known encoding then we can be sure it will be
           correctly re-encoded to the current encoding by
           translateChar in the parser and so could mark the result in
           a Latin-1 or UTF-8 locale.

           A small complication is that different elements could have
           different encodings, but all that matters is that all
           non-ASCII elements have known encoding. */

        for (i = 0; i < LENGTH(text); i++) {
            if (!ENC_KNOWN(STRING_ELT(text, i))
                 && !IS_ASCII(STRING_ELT(text, i))) {
                allKnown = FALSE;
                break;
            }
        }

        if (allKnown) {
            known_to_be_latin1 = old_latin1;
            known_to_be_utf8 = old_utf8;
        }

        if (num == NA_INTEGER) num = -1;

        s = R_ParseVector (text, num, &status, source);
    }
    else {

        ifile = asInteger(file);
        con = getConnection(ifile);
        wasopen = con->isopen;

        if (ifile >= 3) { /* file != "" */

            if (num == NA_INTEGER) num = -1;

            if(!wasopen) {
                if (!con->open(con)) error(_("cannot open the connection"));
                /* Set up a context which will close the connection on error */
                begincontext (&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, 
                              R_BaseEnv, R_NilValue, R_NilValue);
                cntxt.cend = &conn_cleanup;
                cntxt.cenddata = con;
            }
            if (!con->canread) error(_("cannot read from this connection"));

            s = R_ParseConn (con, num, &status, source);

            if (!wasopen) {
                PROTECT(s);
                endcontext(&cntxt);
                con->close(con);
                UNPROTECT(1);
            }
        }
        else {  /* reading from standard input */

            if (num == NA_INTEGER) num = 1;

            prompt_string = isString(prompt) && LENGTH(prompt) > 0
                            ? CHAR(STRING_ELT(prompt,0))
                            : CHAR(STRING_ELT(GetOption1(install("prompt")),0));

            console_bufp = console_buf;  /* empty buffer initially */
            *console_bufp = 0;

            s = R_ParseStream (console_getc, (void *) prompt_string, num, 
                               &status, source);
        }
    }

    if (status != PARSE_OK) parseError (call, R_ParseError);

    UNPROTECT(1);
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;

    return s;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_source[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"parse",	do_parse,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
