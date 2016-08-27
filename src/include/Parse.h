/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005 R Core Team
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

#ifndef R_PARSE_H
#define R_PARSE_H

#include <R_ext/Parse.h>
#include <IOStuff.h>

/* Public interface */
/* SEXP R_ParseVector(SEXP, int, ParseStatus *, SEXP); in R_ext/Parse.h */

/* Private interface */

typedef struct {

    Rboolean keepSrcRefs;	/* Attach srcrefs to objects as parsed? */
    Rboolean didAttach;		/* Was a srcref attached? */
    SEXP SrcFile;		/* The srcfile object currently being parsed */
    SEXP Original;		/* The underlying srcfile object */
    PROTECT_INDEX SrcFileProt;	/* The SrcFile may change */
    PROTECT_INDEX OriginalProt; /* ditto */
    /* Position information about the current parse... */
    int xxlineno;		/* Line number according to #line directives */
    int xxcolno;		/* Character number on line */
    int xxbyteno;		/* Byte number on line */
    int xxparseno;              /* Line number ignoring #line directives */

    SEXP ParseData;		/* Parse data in non-matrix form or R_NilValue;
                                   this first record links to later ones */
    PROTECT_INDEX ParseDataProt;/* ParseData will often change */
    SEXP containing_parse_rec;  /* Parse data record of containing expression */
    int unattached_comment_id;  /* Id of earliest unattached comment, or 0 */
    int next_id;                /* Next id number of an element in parseData */
    PROTECT_INDEX TokenValProt; /* Index for protecting token value */
} SrcRefState;

void R_InitSrcRefState(SrcRefState *state, int kepSource);
void R_TextForSrcRefState (SrcRefState *state, const char *text);
void R_FinalizeSrcRefState(SrcRefState *state);

SEXP R_Parse1Stream (int (*)(void *), void *, ParseStatus *, SrcRefState *);

/* Report a parse error */
	
void parseError(SEXP call, int linenum);

/* Operator precedence functions.  Used in deparse.c.  Defined in parse.c,
   where the documentation is located. */

#define unary_prec  Rf_unary_prec
#define binary_prec Rf_binary_prec
#define misc_prec   Rf_misc_prec

int Rf_unary_prec (SEXP);
int Rf_binary_prec (SEXP);
int Rf_misc_prec (SEXP);

#define NON_ASSOC(p)   (((p)&3) == 0)
#define LEFT_ASSOC(p)  (((p)&3) == 1)
#define RIGHT_ASSOC(p) (((p)&3) == 2)

/* Functions defined in deparse.c and used in parse.c. */

#define needsparens_postfix  Rf_needsparens_postfix
#define needsparens_unary    Rf_needsparens_unary
#define needsparens_binary   Rf_needsparens_binary
#define needsparens_arg      Rf_needsparens_arg

Rboolean needsparens_postfix (SEXP, SEXP);
Rboolean needsparens_unary   (SEXP, SEXP);
Rboolean needsparens_binary  (SEXP, SEXP, int);
Rboolean needsparens_arg     (SEXP);

#endif /* not R_PARSE_H */
