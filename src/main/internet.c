/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2020   The R Core Team.
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

#include <Defn.h>
#include <Internal.h>

#include <Rconnections.h>
#include <Rdynpriv.h>
#include <R_ext/R-ftp-http.h>
#include <Rmodules/Rinternet.h>

static R_InternetRoutines routines, *ptr = &routines;


/*
SEXP Rdownload(SEXP args);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, int serverfd, char *mode, int timeout);
Rconnection R_newservsock(int port);


Next 6 are for use by libxml, only

void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, char *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, char *dest, int len);
void  R_FTPClose(void *ctx);

int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)

int R_HTTPDCreate(const char *ip, int port);
void R_HTTPDStop(void);
 */

static int initialized = 0;

R_InternetRoutines *
R_setInternetRoutines(R_InternetRoutines *routines)
{
    R_InternetRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}

static void internet_Init(void)
{
    int res;
    res = R_moduleCdynload("internet", 1, 1);
    initialized = -1;
    if(!res) return;
    if(!ptr->download)
	error(_("internet routines cannot be accessed in module"));
    initialized = 1;
    return;
}

SEXP Rdownload(SEXP args)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->download)(args);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

Rconnection attribute_hidden 
R_newurl(const char *description, const char * const mode, SEXP headers, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newurl)(description, mode, headers, type);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

Rconnection attribute_hidden
R_newsock(const char *host, int port, int server, int serverfd,
          const char * const mode, int timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newsock)(host, port, server, serverfd, mode, timeout);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}

Rconnection attribute_hidden R_newservsock(int port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newservsock)(port);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
}


void *R_HTTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPOpen)(url, NULL, NULL, 0);
    else {
	error(_("internet routines cannot be loaded"));
	return NULL;
    }
}

int   R_HTTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_HTTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

void *R_FTPOpen(const char *url)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPOpen)(url);
    else {
	error(_("internet routines cannot be loaded"));
	return NULL;
    }
}

int   R_FTPRead(void *ctx, char *dest, int len)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->FTPRead)(ctx, dest, len);
    else {
	error(_("internet routines cannot be loaded"));
	return 0;
    }
}

void  R_FTPClose(void *ctx)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->FTPClose)(ctx);
    else
	error(_("internet routines cannot be loaded"));
}

int extR_HTTPDCreate(const char *ip, int port)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->HTTPDCreate)(ip, port);
    else
	error(_("internet routines cannot be loaded"));
    return -1;
}

void extR_HTTPDStop(void)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->HTTPDStop)();
    else
	error(_("internet routines cannot be loaded"));
}


SEXP Rsockconnect(SEXP sport, SEXP shost)
{
    if (length(sport) != 1) error("invalid 'socket' argument");
    int port = asInteger(sport);
    char *host[1];
    host[0] = (char *) translateCharFP(STRING_ELT(shost, 0));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockconnect)(&port, host);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

SEXP Rsockread(SEXP ssock, SEXP smaxlen)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock);
    int maxlen = asInteger(smaxlen);
    if (maxlen < 0) /* also catches NA_INTEGER */
	error(_("maxlen must be non-negative"));
    SEXP rbuf = allocVector(RAWSXP, maxlen + 1);
    PROTECT(rbuf);
    char *buf = (char *) RAW(rbuf), *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockread)(&sock, abuf, &maxlen);
    else
	error(_("socket routines cannot be loaded"));
    if (maxlen < 0) // presumably -1, error from recv
	error("Error reading data in Rsockread");
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkCharLen(buf, maxlen));
    UNPROTECT(2); /* rbuf, ans */
    return ans;
		       
}

SEXP Rsockclose(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock);
    if (sock <= 0) error(_("attempt to close invalid socket"));
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockclose)(&sock);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarLogical(sock);
}

SEXP Rsockopen(SEXP sport)
{
    if (length(sport) != 1) error("invalid 'port' argument");
    int port = asInteger(sport);
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockopen)(&port);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(port); // The socket number
}

SEXP Rsocklisten(SEXP ssock)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), len = 256;
    char buf[257], *abuf[1];
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->socklisten)(&sock, abuf, &len);
    else
	error(_("socket routines cannot be loaded"));
    SEXP ans = PROTECT(ScalarInteger(sock)); // The socket being listened on
    SEXP host = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(host, 0, mkChar(buf));
    setAttrib(ans, install("host"), host);
    UNPROTECT(2);
    return ans;
}

SEXP Rsockwrite(SEXP ssock, SEXP sstring)
{
    if (length(ssock) != 1) error("invalid 'socket' argument");
    int sock = asInteger(ssock), start = 0, end, len;
    char *buf = (char *) translateCharFP(STRING_ELT(sstring, 0)), *abuf[1];
    end = len = (int) strlen(buf);
    abuf[0] = buf;
    if(!initialized) internet_Init();
    if(initialized > 0)
	(*ptr->sockwrite)(&sock, abuf, &start, &end, &len);
    else
	error(_("socket routines cannot be loaded"));
    return ScalarInteger(len);
}


attribute_hidden
int Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		double timeout)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->sockselect)(nsock, insockfd, ready, write, timeout);
    else {
	error(_("socket routines cannot be loaded"));
	return 0;
    }
}

SEXP attribute_hidden do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlVersion)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlGetHeaders)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

SEXP attribute_hidden do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->curlDownload)(call, op, args, rho);
    else {
	error(_("internet routines cannot be loaded"));
	return R_NilValue;
    }
}

Rconnection attribute_hidden
R_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type)
{
    if(!initialized) internet_Init();
    if(initialized > 0)
	return (*ptr->newcurlurl)(description, mode, headers, type);
    else {
	error(_("internet routines cannot be loaded"));
	return (Rconnection)0;
    }
    return (Rconnection)0; /* -Wall */
}

