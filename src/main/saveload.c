/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2011  The R Core Team
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

/* <UTF8> byte-level access is only to compare with chars <= 0x7F */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define NEED_CONNECTION_PSTREAMS
#define USE_FAST_PROTECT_MACROS
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Rinterface.h>
#include <Rmath.h>
#include <Fileio.h>
#include <R_ext/RS.h>
#include <errno.h>
#include <ctype.h>		/* for isspace */

/* From time to time changes in R, such as the addition of a new SXP,
 * may require changes in the save file format.  Here are some
 * guidelines on handling format changes:
 *
 *    Starting with R 1.4.0 there is a version number associated with
 *    save file formats.  This version number should be incremented
 *    when the format is changed so older versions of R can recognize
 *    and reject the new format with a meaningful error message.
 *
 *    To allow older versions of R to give useful error messages, the
 *    header now contains the version of R that wrote the workspace
 *    and the oldest version that can read the workspace.  These
 *    versions are stored as an integer packed by the R_Version macro
 *    from Rversion.h.  Some workspace formats may only exist
 *    temporarily in the development stage.  If readers are not
 *    provided in a release version, then these should specify the
 *    oldest reader R version as -1.
 *
 * Currently, only version 2 may be read or written.
 */

static int R_DefaultSaveFormatVersion = 2;

#define R_MAGIC_ASCII_V2   2001
#define R_MAGIC_BINARY_V2  2002
#define R_MAGIC_XDR_V2     2003
#define R_MAGIC_EMPTY      999
#define R_MAGIC_CORRUPT    998
#define R_MAGIC_MAYBE_TOO_OLD_NEW 997


#ifndef INT_32_BITS
/* The way XDR is used pretty much assumes that int is 32 bits and
   maybe even 2's complement representation--without that, NA_INTEGER
   is not likely to be preserved properly.  Since 32 bit ints (and 2's
   complement) are pretty much universal, we can worry about that when
   the need arises.  To be safe, we signal a compiler error if int is
   not 32 bits. There may be similar issues with doubles. */
*/
# error code requires that int have 32 bits
#endif

#include <rpc/types.h>
#include <rpc/xdr.h>


/* ----- F i l e -- M a g i c -- N u m b e r s ----- */

static void R_WriteMagic(FILE *fp, int number)
{
    unsigned char buf[5];
    size_t res;

    number = abs(number);
    switch (number) {
    case R_MAGIC_ASCII_V2:   /* Version >=2 - R Data, ASCII Format */
	strcpy((char*)buf, "RDA2");
	break;
    case R_MAGIC_BINARY_V2:  /* Version >=2 - R Data, Binary Format */
	strcpy((char*)buf, "RDB2");
	break;
    case R_MAGIC_XDR_V2:     /* Version >=2 - R Data, XDR Binary Format */
	strcpy((char*)buf, "RDX2");
	break;
    default:
	buf[0] = (number/1000) % 10 + '0';
	buf[1] = (number/100) % 10 + '0';
	buf[2] = (number/10) % 10 + '0';
	buf[3] = number % 10 + '0';
    }
    buf[4] = '\n';
    res = fwrite((char*)buf, sizeof(char), 5, fp);
    if(res != 5) error(_("write failed"));
}

static int R_ReadMagic(FILE *fp)
{
    unsigned char buf[6];
    int d1, d2, d3, d4, count;

    count = fread((char*)buf, sizeof(char), 5, fp);
    if (count != 5) {
	if (count == 0)
	    return R_MAGIC_EMPTY;
	else
	    return R_MAGIC_CORRUPT;
    }

    if (strncmp((char*)buf, "RDA2\n", 5) == 0) {
	return R_MAGIC_ASCII_V2;
    }
    else if (strncmp((char*)buf, "RDB2\n", 5) == 0) {
	return R_MAGIC_BINARY_V2;
    }
    else if (strncmp((char*)buf, "RDX2\n", 5) == 0) {
	return R_MAGIC_XDR_V2;
    }
    else if (strncmp((char *)buf, "RD", 2) == 0)
	return R_MAGIC_MAYBE_TOO_OLD_NEW;

    /* Intel gcc seems to screw up a single expression here */
    d1 = (buf[3]-'0') % 10;
    d2 = (buf[2]-'0') % 10;
    d3 = (buf[1]-'0') % 10;
    d4 = (buf[0]-'0') % 10;
    return d1 + 10 * d2 + 100 * d3 + 1000 * d4;
}

/* ----- E x t e r n a l -- I n t e r f a c e s ----- */

void attribute_hidden R_SaveToFileV(SEXP obj, FILE *fp, int ascii, int version)
{
    if (version == 1) {
        abort();  /* no longer supported */
    }
    else {
	struct R_outpstream_st out;
	R_pstream_format_t type;
	int magic;
	if (ascii) {
	    magic = R_MAGIC_ASCII_V2;
	    type = R_pstream_ascii_format;
	}
	else {
	    magic = R_MAGIC_XDR_V2;
	    type = R_pstream_xdr_format;
	}
	R_WriteMagic(fp, magic);
	R_InitFileOutPStream(&out, fp, type, version, NULL, R_NoObject);
	R_Serialize(obj, &out);
    }
}

void attribute_hidden R_SaveToFile(SEXP obj, FILE *fp, int ascii)
{
    R_SaveToFileV(obj, fp, ascii, R_DefaultSaveFormatVersion);
}

/* different handling of errors */

SEXP attribute_hidden R_LoadFromFile(FILE *fp, int startup)
{
    struct R_inpstream_st in;
    int magic;

    magic = R_ReadMagic(fp);
    switch(magic) {
    case R_MAGIC_ASCII_V2:
	R_InitFileInPStream(&in, fp, R_pstream_ascii_format, NULL, R_NoObject);
	return R_Unserialize(&in);
    case R_MAGIC_BINARY_V2:
	R_InitFileInPStream(&in, fp, R_pstream_binary_format, NULL, R_NoObject);
	return R_Unserialize(&in);
    case R_MAGIC_XDR_V2:
	R_InitFileInPStream(&in, fp, R_pstream_xdr_format, NULL, R_NoObject);
	return R_Unserialize(&in);
    default:
	switch (magic) {
	case R_MAGIC_EMPTY:
	    error(_("restore file may be empty -- no data loaded"));
	case R_MAGIC_MAYBE_TOO_OLD_NEW:
	    error(_("restore file may be from a newer or much older version of R -- no data loaded"));
	default:
	    error(_("bad restore file magic number (file may be corrupted) -- no data loaded"));
	}
    }
}

static SEXP RestoreToEnv(SEXP ans, SEXP aenv)
{
    SEXP a, names, obj;
    int cnt = 0;
    /* Store the components of the list in aenv.  We either replace
     * the existing objects in aenv or establish new bindings for
     * them.
     */

    /* allow ans to be a vector-style list */
    if (TYPEOF(ans) == VECSXP) {
	int i;
	PROTECT(ans);
	PROTECT(names = getAttrib(ans, R_NamesSymbol)); /* PROTECT needed?? */
	if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(ans))
	    error(_("not a valid named list"));
	for (i = 0; i < LENGTH(ans); i++) {
	    SEXP sym = installChar(STRING_ELT(names, i));
	    obj = VECTOR_ELT(ans, i);
	    defineVar(sym, obj, aenv);
	    if(R_seemsOldStyleS4Object(obj))
		warningcall(R_NilValue,
			    _("'%s' looks like a pre-2.4.0 S4 object: please recreate it"),
			    CHAR(STRING_ELT(names, i)));
	}
	UNPROTECT(2);
	return names;
    }

    if (! isList(ans))
	error(_("loaded data is not in pair list form"));

    PROTECT(ans);
    a = ans;
    while (a != R_NilValue) {a = CDR(a); cnt++;}
    PROTECT(names = allocVector(STRSXP, cnt));
    cnt = 0;
    a = ans;
    while (a != R_NilValue) {
	SET_STRING_ELT(names, cnt++, PRINTNAME(TAG(a)));
	defineVar(TAG(a), CAR(a), aenv);
	if(R_seemsOldStyleS4Object(CAR(a)))
	    warningcall(R_NilValue,
			_("'%s' looks like a pre-2.4.0 S4 object: please recreate it"),
			CHAR(PRINTNAME(TAG(a))));
	a = CDR(a);
    }
    UNPROTECT(2);
    return names;
}

static SEXP R_LoadSavedData(FILE *fp, SEXP aenv)
{
    return RestoreToEnv(R_LoadFromFile(fp, 0), aenv);
}

void attribute_hidden R_XDREncodeDouble(double d, void *buf)
{
    XDR xdrs;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_DOUBLE_SIZE, XDR_ENCODE);
    success = xdr_double(&xdrs, &d);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR write failed"));
}

double attribute_hidden R_XDRDecodeDouble(void *buf)
{
    XDR xdrs;
    double d;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_DOUBLE_SIZE, XDR_DECODE);
    success = xdr_double(&xdrs, &d);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR read failed"));
    return d;
}

void attribute_hidden R_XDREncodeInteger(int i, void *buf)
{
    XDR xdrs;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_INTEGER_SIZE, XDR_ENCODE);
    success = xdr_int(&xdrs, &i);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR write failed"));
}

int attribute_hidden R_XDRDecodeInteger(void *buf)
{
    XDR xdrs;
    int i, success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_INTEGER_SIZE, XDR_DECODE);
    success = xdr_int(&xdrs, &i);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR read failed"));
    return i;
}

/* Next two were used in gnomeGUI package, are in Rinterface.h  */
void R_SaveGlobalEnvToFile(const char *name)
{
    SEXP sym = install("sys.save.image");
    if (findVar(sym, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	FILE *fp = R_fopen(name, "wb"); /* binary file */
	if (!fp) {
	    error(_("cannot save data -- unable to open '%s': %s"),
		  name, strerror(errno));
	}
	R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
	fclose(fp);
    }
    else {
	SEXP args, call;
	args = LCONS(ScalarString(mkChar(name)), R_NilValue);
	PROTECT(call = LCONS(sym, args));
	eval(call, R_GlobalEnv);
	UNPROTECT(1);
    }
}

void R_RestoreGlobalEnvFromFile(const char *name, Rboolean quiet)
{
    SEXP sym = install("sys.load.image");
    if (findVar(sym, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	FILE *fp = R_fopen(name, "rb"); /* binary file */
	if(fp != NULL) {
	    R_LoadSavedData(fp, R_GlobalEnv);
	    if(! quiet)
		Rprintf("[Previously saved workspace restored]\n\n");
	    fclose(fp);
	}
    }
    else {
	SEXP args, call, sQuiet;
	sQuiet = quiet ? mkTrue() : mkFalse();
	PROTECT(args = LCONS(sQuiet, R_NilValue));
	args = LCONS(ScalarString(mkChar(name)), args);
	PROTECT(call = LCONS(sym, args));
	eval(call, R_GlobalEnv);
	UNPROTECT(2);
    }
}


#include <Rconnections.h>

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}


/* Ideally it should be possible to do this entirely in R code with
   something like

	magic <- if (ascii) "RDA2\n" else ...
	writeChar(magic, con, eos = NULL)
	val <- lapply(list, get, envir = envir)
	names(val) <- list
	invisible(serialize(val, con, ascii = ascii))

   Unfortunately, this will result in too much duplication in the lapply
   (and any other way of doing this).  Hence we need an internal version. 

   In case anyone wants to do this another way, in fact it is a
   pairlist of objects that is serialized, but RestoreToEnv copes
   with either a pairlist or list.
*/

static SEXP do_saveToConn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* saveToConn(list, conn, ascii, version, environment) */

    SEXP s, t, source, list, tmp;
    Rboolean ascii, wasopen;
    int len, j, version, ep;
    Rconnection con;
    struct R_outpstream_st out;
    R_pstream_format_t type;
    char *magic;
    RCNTXT cntxt;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP)
	error(_("first argument must be a character vector"));
    list = CAR(args);

    con = getConnection(asInteger(CADR(args)));

    if (TYPEOF(CADDR(args)) != LGLSXP)
	error(_("'ascii' must be logical"));
    ascii = INTEGER(CADDR(args))[0];

    if (CADDDR(args) == R_NilValue)
	version = R_DefaultSaveFormatVersion;
    else
	version = asInteger(CADDDR(args));
    if (version == NA_INTEGER || version <= 0)
	error(_("invalid '%s' argument"), "version");
    if (version != 2)
	error(_("cannot save in version %d format"), version);
    source = CAR(nthcdr(args,4));
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error(_("invalid '%s' argument"), "environment");
    ep = asLogical(CAR(nthcdr(args,5)));
    if (ep == NA_LOGICAL)
	error(_("invalid '%s' argument"), "eval.promises");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];	
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* set up a context which will close the connection
 	   if there is an error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canwrite)
	error(_("connection not open for writing"));

    if (ascii) {
	magic = "RDA2\n";
	type = R_pstream_ascii_format;
    }
    else {
	if (con->text)
	    error(_("cannot save XDR format to a text-mode connection"));
	magic = "RDX2\n";
	type = R_pstream_xdr_format;
    }

    if (con->text)
	Rconn_printf(con, "%s", magic);
    else {
	int len = strlen(magic);
	if (len != con->write(magic, 1, len, con))
	    error(_("error writing to connection"));
    }

    R_InitConnOutPStream(&out, con, type, version, NULL, R_NoObject);

    len = length(list);
    PROTECT(s = allocList(len));

    t = s;
    for (j = 0; j < len; j++, t = CDR(t)) {
	SET_TAG(t, installChar(STRING_ELT(list, j)));
	SETCAR(t, findVar(TAG(t), source));
	tmp = findVar(TAG(t), source);
	if (tmp == R_UnboundValue)
            unbound_var_error(TAG(t));
	if(ep && TYPEOF(tmp) == PROMSXP) {
	    PROTECT(tmp);
	    tmp = eval(tmp, source);
	    UNPROTECT(1);
	}
	SETCAR(t, tmp);
    }

    R_Serialize(s, &out);
    if (!wasopen) con->close(con);
    UNPROTECT(1);
    return R_NilValue;
}

/* Read and checks the magic number, open the connection if needed */

static SEXP do_loadFromConn2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* loadFromConn2(conn, environment) */

    struct R_inpstream_st in;
    Rconnection con;
    SEXP aenv, res = R_NilValue;
    unsigned char buf[6];
    int count;
    Rboolean wasopen;
    RCNTXT cntxt;

    checkArity(op, args);

    con = getConnection(asInteger(CAR(args)));

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];	
	strcpy(mode, con->mode);
	strcpy(con->mode, "rb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* set up a context which will close the connection
 	   if there is an error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canread) error(_("connection not open for reading"));
    if(con->text) error(_("can only load() from a binary connection"));

    aenv = CADR(args);
    if (TYPEOF(aenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    else if (TYPEOF(aenv) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");

    /* check magic */
    memset(buf, 0, 6);
    count = con->read(buf, sizeof(char), 5, con);
    if (count == 0) error(_("no input is available"));
    if (strncmp((char*)buf, "RDA2\n", 5) == 0 ||
	strncmp((char*)buf, "RDB2\n", 5) == 0 ||
	strncmp((char*)buf, "RDX2\n", 5) == 0) {
	R_InitConnInPStream(&in, con, R_pstream_any_format, NULL, R_NoObject);
	/* PROTECT is paranoia: some close() method might allocate */
	PROTECT(res = RestoreToEnv(R_Unserialize(&in), aenv));
	if(!wasopen) {endcontext(&cntxt); con->close(con);}
	UNPROTECT(1);
    } else
	error(_("the input does not start with a magic number compatible with loading from a connection"));
    return res;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_saveload[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"saveToConn",	do_saveToConn,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"loadFromConn2",do_loadFromConn2,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
