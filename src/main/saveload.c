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

#define R_MAGIC_ASCII_V2   2001
#define R_MAGIC_BINARY_V2  2002
#define R_MAGIC_XDR_V2     2003
#define R_MAGIC_EMPTY      999
#define R_MAGIC_CORRUPT    998
#define R_MAGIC_MAYBE_TOO_OLD_NEW 997


/* Static Globals, DIE, DIE, DIE! */


#include "RBufferUtils.h"

/* These are used by OffsetToNode & DataLoad.
 OffsetToNode is called by DataLoad() and RestoreSEXP()
 which itself is only called by RestoreSEXP.
 */
typedef struct {
 int NSymbol;		/* Number of symbols */
 int NSave;		/* Number of non-symbols */
 int NTotal;		/* NSymbol + NSave */
 int NVSize;		/* Number of vector cells */

 int *OldOffset;        /* Offsets in previous incarnation */

 SEXP NewAddress;       /* Addresses in this incarnation */
} NodeInfo;


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

typedef struct {
/* These variables are accessed in the
   InInteger, InComplex, InReal, InString
   methods for Ascii, Binary, XDR.
   bufsize is only used in XdrInString!

The Ascii* routines could declare their own local
copy of smbuf and use that (non-static). That would
mean some of them wouldn't need the extra argument.
*/

    R_StringBuffer buffer;
    char smbuf[512];		/* Small buffer for temp use */
				/* smbuf is only used by Ascii. */
    XDR xdrs;
} SaveLoadData;


/* ----- I / O -- F u n c t i o n -- P o i n t e r s ----- */

typedef struct {
 void	(*OutInit)(FILE*, SaveLoadData *d);
 void	(*OutInteger)(FILE*, int, SaveLoadData *);
 void	(*OutReal)(FILE*, double, SaveLoadData *);
 void	(*OutComplex)(FILE*, Rcomplex, SaveLoadData *);
 void	(*OutString)(FILE*, const char*, SaveLoadData *);
 void	(*OutSpace)(FILE*, int, SaveLoadData *);
 void	(*OutNewline)(FILE*, SaveLoadData *);
 void	(*OutTerm)(FILE*, SaveLoadData *);
} OutputRoutines;

typedef struct {
 void	(*InInit)(FILE*, SaveLoadData *d);
 int	(*InInteger)(FILE*, SaveLoadData *);
 double	(*InReal)(FILE*, SaveLoadData *);
 Rcomplex	(*InComplex)(FILE*, SaveLoadData *);
 char*	(*InString)(FILE*, SaveLoadData *);
 void	(*InTerm)(FILE*, SaveLoadData *d);
} InputRoutines;

typedef struct {
  FILE *fp;
  OutputRoutines *methods;
  SaveLoadData *data;
} OutputCtxtData;

typedef struct {
  FILE *fp;
  InputRoutines *methods;
  SaveLoadData *data;
} InputCtxtData;


/* ----- D u m m y -- P l a c e h o l d e r -- R o u t i n e s ----- */

static void DummyInit(FILE *fp, SaveLoadData *d)
{
}

static void DummyOutSpace(FILE *fp, int nspace, SaveLoadData *d)
{
}

static void DummyOutNewline(FILE *fp, SaveLoadData *d)
{
}

static void DummyTerm(FILE *fp, SaveLoadData *d)
{
}


/* ----- L o w l e v e l -- B i n a r y -- I / O ----- */

static int BinaryInInteger(FILE * fp, SaveLoadData *unused)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error(_("a read error occurred"));
    return i;
}

static double BinaryInReal(FILE * fp, SaveLoadData *unused)
{
    double x;
    if (fread(&x, sizeof(double), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static Rcomplex BinaryInComplex(FILE * fp, SaveLoadData *unused)
{
    Rcomplex x;
    if (fread(&x, sizeof(Rcomplex), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static char *BinaryInString(FILE *fp, SaveLoadData *d)
{
    char *bufp = d->buffer.data;
    do {
	*bufp = R_fgetc(fp);
    }
    while (*bufp++);
    return d->buffer.data;
}

static SEXP OffsetToNode(int offset, NodeInfo *node)
{
    int l, m, r;

    if (offset == -1) return R_NilValue;
    if (offset == -2) return R_GlobalEnv;
    if (offset == -3) return R_UnboundValue;
    if (offset == -4) return R_MissingArg;

    /* binary search for offset */

    l = 0;
    r = node->NTotal - 1;
    do {
	m = (l + r) / 2;
	if (offset < node->OldOffset[m])
	    r = m - 1;
	else
	    l = m + 1;
    }
    while (offset != node->OldOffset[m] && l <= r);
    if (offset == node->OldOffset[m]) return VECTOR_ELT(node->NewAddress, m);

    /* Not supposed to happen: */
    warning(_("unresolved node during restore"));
    return R_NilValue;
}

static void RemakeNextSEXP(FILE *fp, NodeInfo *node, int version, InputRoutines *m, SaveLoadData *d)
{
    unsigned int j, idx, type;
    int len;
    SEXP s;

    idx = m->InInteger(fp, d);
    type = m->InInteger(fp, d);

    /* skip over OBJECT, LEVELS, and ATTRIB */
    /* OBJECT(s) = */ m->InInteger(fp, d);
    /* LEVELS(s) = */ m->InInteger(fp, d);
    /* ATTRIB(s) = */ m->InInteger(fp, d);
    switch (type) {
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case ENVSXP:
	s = allocSExp(type);
	/* skip over CAR, CDR, and TAG */
	/* CAR(s) = */ m->InInteger(fp, d);
	/* CDR(s) = */ m->InInteger(fp, d);
	/* TAG(s) = */ m->InInteger(fp, d);
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	s = allocSExp(type);
	/* skip over length and name fields */
	/* length = */ m->InInteger(fp, d);
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	/* name = */ m->InString(fp, d);
	break;
    case CHARSXP:
	len = m->InInteger(fp, d);
	s = allocCharsxp(len); /* This is not longer correct */
	R_AllocStringBuffer(len, &(d->buffer));
	/* skip over the string */
	/* string = */ m->InString(fp, d);
	break;
    case REALSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /*REAL(s)[j] = */ m->InReal(fp, d);
	break;
    case CPLXSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /* COMPLEX(s)[j] = */ m->InComplex(fp, d);
	break;
    case INTSXP:
    case LGLSXP:
	len = m->InInteger(fp, d);;
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /* INTEGER(s)[j] = */ m->InInteger(fp, d);
	break;
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++) {
	    /* VECTOR(s)[j] = */ m->InInteger(fp, d);
	}
	break;
    default: error(_("bad SEXP type in data file"));
    }

    /* install the new SEXP */
    SET_VECTOR_ELT(node->NewAddress, idx, s);
}

static void RestoreSEXP(SEXP s, FILE *fp, InputRoutines *m, NodeInfo *node,
                        int version, SaveLoadData *d)
{
    unsigned int j, type;
    int len;

    type = m->InInteger(fp, d);
    if (type != TYPEOF(s))
      error(_("mismatch on types"));

    SET_OBJECT(s, m->InInteger(fp, d));
    SETLEVELS(s, m->InInteger(fp, d));
    SET_ATTRIB(s, OffsetToNode(m->InInteger(fp, d), node));
    switch (TYPEOF(s)) {
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case ENVSXP:
	SETCAR(s, OffsetToNode(m->InInteger(fp, d), node));
	SETCDR(s, OffsetToNode(m->InInteger(fp, d), node));
	SET_TAG(s, OffsetToNode(m->InInteger(fp, d), node));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	len = m->InInteger(fp, d);
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	SET_PRIMOFFSET(s, StrToInternal(m->InString(fp, d)));
	break;
    case CHARSXP:
	len = m->InInteger(fp, d);
	R_AllocStringBuffer(len, &(d->buffer));
	/* Better to use a fresh copy in the cache */
	strcpy(CHAR_RW(s), m->InString(fp, d));
	break;
    case REALSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    REAL(s)[j] = m->InReal(fp, d);
	break;
    case CPLXSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    COMPLEX(s)[j] = m->InComplex(fp, d);
	break;
    case INTSXP:
    case LGLSXP:
	len = m->InInteger(fp, d);;
	for (j = 0; j < len; j++)
	    INTEGER(s)[j] = m->InInteger(fp, d);
	break;
    case STRSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    SET_STRING_ELT(s, j, OffsetToNode(m->InInteger(fp, d), node));
	break;
    case VECSXP:
    case EXPRSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    SET_VECTOR_ELT(s, j, OffsetToNode(m->InInteger(fp, d), node));
	break;
    default: error(_("bad SEXP type in data file"));
    }
}

static void RestoreError(/* const */ char *msg, int startup)
{
    if(startup)
	R_Suicide(msg);
    else
	error("%s", msg);
}

/* ----- L o w l e v e l -- A s c i i -- I / O ------ */

static void OutSpaceAscii(FILE *fp, int nspace, SaveLoadData *unused)
{
    while(--nspace >= 0)
	fputc(' ', fp);
}
static void OutNewlineAscii(FILE *fp, SaveLoadData *unused)
{
    fputc('\n', fp);
}

static void OutIntegerAscii(FILE *fp, int x, SaveLoadData *unused)
{
    if (x == NA_INTEGER) fprintf(fp, "NA");
    else fprintf(fp, "%d", x);
}

static int InIntegerAscii(FILE *fp, SaveLoadData *unused)
{
    char buf[128];
    int x, res;
    res = fscanf(fp, "%s", buf);
    if(res != 1) error(_("read error"));
    if (strcmp(buf, "NA") == 0)
	return NA_INTEGER;
    else {
	res = sscanf(buf, "%d", &x);
	if(res != 1) error(_("read error"));
    }
    return x;
}

static void OutStringAscii(FILE *fp, const char *x, SaveLoadData *unused)
{
    int i, nbytes;
    nbytes = strlen(x);
    fprintf(fp, "%d ", nbytes);
    for (i = 0; i < nbytes; i++) {
	switch(x[i]) {
	case '\n': fprintf(fp, "\\n");  break;
	case '\t': fprintf(fp, "\\t");  break;
	case '\v': fprintf(fp, "\\v");  break;
	case '\b': fprintf(fp, "\\b");  break;
	case '\r': fprintf(fp, "\\r");  break;
	case '\f': fprintf(fp, "\\f");  break;
	case '\a': fprintf(fp, "\\a");  break;
	case '\\': fprintf(fp, "\\\\"); break;
	case '\?': fprintf(fp, "\\?");  break;
	case '\'': fprintf(fp, "\\'");  break;
	case '\"': fprintf(fp, "\\\""); break;
	default  :
	    /* cannot print char in octal mode -> cast to unsigned
	       char first */
	    /* actually, since x is signed char and '\?' == 127
	       is handled above, x[i] > 126 can't happen, but
	       I'm superstitious...  -pd */
	    if (x[i] <= 32 || x[i] > 126)
		fprintf(fp, "\\%03o", (unsigned char) x[i]);
	    else
		fputc(x[i], fp);
	}
    }
}

static char *InStringAscii(FILE *fp, SaveLoadData *unused)
{
    static char *buf = NULL;
    static int buflen = 0;
    int c, d, i, j;
    int nbytes, res;
    res = fscanf(fp, "%d", &nbytes);
    if(res != 1) error(_("read error"));
    /* FIXME : Ultimately we need to replace */
    /* this with a real string allocation. */
    /* All buffers must die! */
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL) /* buf remains allocated */
	    error(_("out of memory reading ascii string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    while(isspace(c = fgetc(fp)))
	;
    ungetc(c, fp);
    for (i = 0; i < nbytes; i++) {
	if ((c =  fgetc(fp)) == '\\') {
	    switch(c = fgetc(fp)) {
	    case 'n' : buf[i] = '\n'; break;
	    case 't' : buf[i] = '\t'; break;
	    case 'v' : buf[i] = '\v'; break;
	    case 'b' : buf[i] = '\b'; break;
	    case 'r' : buf[i] = '\r'; break;
	    case 'f' : buf[i] = '\f'; break;
	    case 'a' : buf[i] = '\a'; break;
	    case '\\': buf[i] = '\\'; break;
	    case '?' : buf[i] = '\?'; break;
	    case '\'': buf[i] = '\''; break;
	    case '\"': buf[i] = '\"'; break;
	    case '0': case '1': case '2': case '3':
	    case '4': case '5': case '6': case '7':
		d = 0; j = 0;
		while('0' <= c && c < '8' && j < 3) {
		    d = d * 8 + (c - '0');
		    c = fgetc(fp);
		    j++;
		}
		buf[i] = d;
		ungetc(c, fp);
		break;
	    default  : buf[i] = c;
	    }
	}
	else buf[i] = c;
    }
    buf[i] = '\0';
    return buf;
}

static void OutDoubleAscii(FILE *fp, double x, SaveLoadData *unused)
{
    if (!R_FINITE(x)) {
	if (ISNAN(x)) fprintf(fp, "NA");
	else if (x < 0) fprintf(fp, "-Inf");
	else fprintf(fp, "Inf");
    }
    /* 16: full precision; 17 gives 999, 000 &c */
    else fprintf(fp, "%.16g", x);
}

static double InDoubleAscii(FILE *fp, SaveLoadData *unused)
{
    char buf[128];
    double x;
    int res;
    res = fscanf(fp, "%s", buf);
    if(res != 1) error(_("read error"));
    if (strcmp(buf, "NA") == 0)
	x = NA_REAL;
    else if (strcmp(buf, "Inf") == 0)
	x = R_PosInf;
    else if (strcmp(buf, "-Inf") == 0)
	x = R_NegInf;
    else {
	res = sscanf(buf, "%lg", &x);
	if(res != 1) error(_("read error"));
    }
    return x;
}

static void OutComplexAscii(FILE *fp, Rcomplex x, SaveLoadData *unused)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	fprintf(fp, "NA NA");
    else {
	OutDoubleAscii(fp, x.r, unused);
	OutSpaceAscii(fp, 1, unused);
	OutDoubleAscii(fp, x.i, unused);
    }
}

static Rcomplex InComplexAscii(FILE *fp, SaveLoadData *unused)
{
    Rcomplex x;
    x.r = InDoubleAscii(fp, unused);
    x.i = InDoubleAscii(fp, unused);
    return x;
}

/* ----- L o w l e v e l -- B i n a r y -- I / O ----- */

static int InIntegerBinary(FILE * fp, SaveLoadData *unused)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error(_("a binary read error occurred"));
    return i;
}

static char *InStringBinary(FILE *fp, SaveLoadData *unused)
{
    static char *buf = NULL;
    static int buflen = 0;
    int nbytes = InIntegerBinary(fp, unused);
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL)
	    error(_("out of memory reading binary string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    if (fread(buf, sizeof(char), nbytes, fp) != nbytes)
	error(_("a binary string read error occurred"));
    buf[nbytes] = '\0';
    return buf;
}

static double InRealBinary(FILE * fp, SaveLoadData *unused)
{
    double x;
    if (fread(&x, sizeof(double), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static Rcomplex InComplexBinary(FILE * fp, SaveLoadData *unused)
{
    Rcomplex x;
    if (fread(&x, sizeof(Rcomplex), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}


/* ----- L o w l e v e l -- X D R -- I / O ----- */

static void InInitXdr(FILE *fp, SaveLoadData *d)
{
    xdrstdio_create(&d->xdrs, fp, XDR_DECODE);
}

static void OutInitXdr(FILE *fp, SaveLoadData *d)
{
    xdrstdio_create(&d->xdrs, fp, XDR_ENCODE);
}

static void InTermXdr(FILE *fp, SaveLoadData *d)
{
    xdr_destroy(&d->xdrs);
}

static void OutTermXdr(FILE *fp, SaveLoadData *d)
{
    xdr_destroy(&d->xdrs);
}

static void OutIntegerXdr(FILE *fp, int i, SaveLoadData *d)
{
    if (!xdr_int(&d->xdrs, &i))
	error(_("an xdr integer data write error occurred"));
}

static int InIntegerXdr(FILE *fp, SaveLoadData *d)
{
    int i;
    if (!xdr_int(&d->xdrs, &i))
	error(_("an xdr integer data read error occurred"));
    return i;
}

static void OutStringXdr(FILE *fp, const char *s, SaveLoadData *d)
{
    unsigned int n = strlen(s);
    char *t = CallocCharBuf(n);
    bool_t res;
    /* This copy may not be needed, will xdr_bytes ever modify 2nd arg? */
    strcpy(t, s);
    OutIntegerXdr(fp, n, d);
    res = xdr_bytes(&d->xdrs, &t, &n, n);
    Free(t);
    if (!res)
	error(_("an xdr string data write error occurred"));
}

static char *InStringXdr(FILE *fp, SaveLoadData *d)
{
    static char *buf = NULL;
    static int buflen = 0;
    unsigned int nbytes = InIntegerXdr(fp, d);
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL)
	    error(_("out of memory reading binary string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    if (!xdr_bytes(&d->xdrs, &buf, &nbytes, nbytes))
	error(_("an xdr string data write error occurred"));
    buf[nbytes] = '\0';
    return buf;
}

static void OutRealXdr(FILE *fp, double x, SaveLoadData *d)
{
    if (!xdr_double(&d->xdrs, &x))
	error(_("an xdr real data write error occurred"));
}

static double InRealXdr(FILE * fp, SaveLoadData *d)
{
    double x;
    if (!xdr_double(&d->xdrs, &x))
	error(_("an xdr real data read error occurred"));
    return x;
}

static void OutComplexXdr(FILE *fp, Rcomplex x, SaveLoadData *d)
{
    if (!xdr_double(&d->xdrs, &(x.r)) || !xdr_double(&d->xdrs, &(x.i)))
	error(_("an xdr complex data write error occurred"));
}

static Rcomplex InComplexXdr(FILE * fp, SaveLoadData *d)
{
    Rcomplex x;
    if (!xdr_double(&d->xdrs, &(x.r)) || !xdr_double(&d->xdrs, &(x.i)))
	error(_("an xdr complex data read error occurred"));
    return x;
}


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

static int R_DefaultSaveFormatVersion = 2;

/* ----- E x t e r n a l -- I n t e r f a c e s ----- */

void attribute_hidden R_SaveToFileV(SEXP obj, FILE *fp, int ascii, int version)
{
    SaveLoadData data = {{NULL, 0, MAXELTSIZE}};

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

#define return_and_free(X) {r = X; R_FreeStringBuffer(&data.buffer); return r;}

SEXP attribute_hidden R_LoadFromFile(FILE *fp, int startup)
{
    struct R_inpstream_st in;
    int magic;
    SaveLoadData data = {{NULL, 0, MAXELTSIZE}};
    SEXP r;

    magic = R_ReadMagic(fp);
    switch(magic) {
    case R_MAGIC_ASCII_V2:
	R_InitFileInPStream(&in, fp, R_pstream_ascii_format, NULL, R_NoObject);
	return_and_free(R_Unserialize(&in));
    case R_MAGIC_BINARY_V2:
	R_InitFileInPStream(&in, fp, R_pstream_binary_format, NULL, R_NoObject);
	return_and_free(R_Unserialize(&in));
    case R_MAGIC_XDR_V2:
	R_InitFileInPStream(&in, fp, R_pstream_xdr_format, NULL, R_NoObject);
	return_and_free(R_Unserialize(&in));
    default:
	R_FreeStringBuffer(&data.buffer);
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

static void saveload_cleanup(void *data)
{
    FILE *fp = (FILE *) data;
    fclose(fp);
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
