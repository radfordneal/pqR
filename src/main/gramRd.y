%{
/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2017 by Radford M. Neal
 *
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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


/** When changed, do:  bison -l gramRd.y; mv gramRd.tab.c gramRd.c **/


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Don't enable this, since many instances, but not time critical */
/* #define USE_FAST_PROTECT_MACROS */ 
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Parse.h>
#define STRICT_R_HEADERS
#include <R_ext/RS.h>           /* for R_chk_* allocation */
#include <ctype.h>
#include <Rmath.h> /* for imax2(.),..*/

/* bison creates a non-static symbol yylloc in both gramLatex.o and gramRd.o,
   so remap */

#define yylloc yyllocR

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

static Rboolean wCalls = TRUE;

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
static int yylex();
static int yyparse(void);

#define yyconst const

typedef struct yyltype
{
  int first_line;
  int first_column;
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
} yyltype;

#ifndef YYID
#define YYID(x) (x)  /* some silly thing about suppressing lint warnings... */
#endif

# define YYLTYPE yyltype
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))							\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	  (Current).first_byte   = (Current).last_byte =		\
	    YYRHSLOC (Rhs, 0).last_byte;				\
	}								\
    while (YYID (0))

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static SEXP	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	UserMacroLookup(const char *);
static SEXP	InstallKeywords();
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);

/* Flags used to mark need for postprocessing in the dynamicFlag attribute */

#define STATIC 0
#define HAS_IFDEF 1
#define HAS_SEXPR 2

/* Internal lexer / parser state variables */

static int 	xxinRString, xxQuoteLine, xxQuoteCol;
static int	xxinEqn;
static int	xxNewlineInString;
static int	xxgetc();
static int	xxungetc(int);
static int	xxlineno, xxbyteno, xxcolno;
static int	xxmode, xxitemType, xxbraceDepth;  /* context for lexer */
static int	xxDebugTokens;  /* non-zero causes debug output to R console */
static const char* xxBasename;     /* basename of file for error messages */
static SEXP	Value;
static int	xxinitvalue;
static char const yyunknown[] = "unknown macro"; /* our message, not bison's */
static SEXP	xxMacroList;/* A hashed environment containing all the standard and user-defined macro names */



#define RLIKE 1		/* Includes R strings; xxinRString holds the opening quote char, or 0 outside a string */
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4
#define COMMENTMODE 5   /* only used in deparsing */
#define UNKNOWNMODE 6   /* ditto */

static SEXP     SrcFile;  /* parse_Rd will *always* supply a srcfile */

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxnewlist2(SEXP, SEXP);
static SEXP	xxnewlist3(SEXP, SEXP, SEXP);
static SEXP	xxnewlist4(SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist5(SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist7(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist8(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist9(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, int, int, YYLTYPE *);
static SEXP	xxmarkup3(SEXP, SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void	xxsavevalue(SEXP, YYLTYPE *);
static void	xxWarnNewline();
static SEXP	xxnewcommand(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxusermacro(SEXP, SEXP, YYLTYPE *);
static int	mkMarkup(int);
static int      mkIfdef(int);
static int	mkCode(int);
static int	mkText(int);
static int	mkVerb(int);
static int 	mkComment(int);

#define YYSTYPE		SEXP

%}

%debug

%token		END_OF_INPUT ERROR
%token		SECTIONHEADER RSECTIONHEADER VSECTIONHEADER
%token		SECTIONHEADER2
%token		RCODEMACRO SEXPR RDOPTS LATEXMACRO VERBMACRO OPTMACRO ESCAPE
%token		LISTSECTION ITEMIZE DESCRIPTION NOITEM
%token		LATEXMACRO2 VERBMACRO2 VERBLATEX
%token		LATEXMACRO3
%token		NEWCOMMAND USERMACRO USERMACRO1 USERMACRO2 USERMACRO3 USERMACRO4
%token		USERMACRO5 USERMACRO6 USERMACRO7 USERMACRO8 USERMACRO9
%token		IFDEF ENDIF
%token		TEXT RCODE VERB COMMENT UNKNOWN
%token		STARTFILE STARTFRAGMENT	/* fake tokens to have two entry points */

/* Recent bison has <> to represent all of the destructors below, but we don't assume it */

/* I think we need to list everything here which occurs before the last item in a
   pattern, just in case the last item is unmatched and we need to back out.  But
   it is safe to list more, so we do. */

%destructor { UNPROTECT_PTR($$); } SECTIONHEADER RSECTIONHEADER
VSECTIONHEADER SECTIONHEADER2 RCODEMACRO SEXPR LATEXMACRO VERBMACRO
OPTMACRO ESCAPE LISTSECTION ITEMIZE DESCRIPTION NOITEM LATEXMACRO2
VERBMACRO2 VERBLATEX LATEXMACRO3 IFDEF ENDIF TEXT RCODE VERB COMMENT UNKNOWN
NEWCOMMAND USERMACRO USERMACRO1 USERMACRO2 USERMACRO3 USERMACRO4
USERMACRO5 USERMACRO6 USERMACRO7 USERMACRO8 USERMACRO9
STARTFILE STARTFRAGMENT goLatexLike goRLike goRLike2 goOption
goVerbatim goVerbatim1 goVerbatim2 goItem0 goItem2 LatexArg RLikeArg2
VerbatimArg1 VerbatimArg2 IfDefTarget ArgItems Option

%%

Init:		STARTFILE RdFile END_OF_INPUT		{ xxsavevalue($2, &@$); UNPROTECT_PTR($1); return 0; }
	|	STARTFRAGMENT RdFragment END_OF_INPUT	{ xxsavevalue($2, &@$); UNPROTECT_PTR($1); return 0; }
	|	error					{ PROTECT(Value = R_NilValue);  YYABORT; }
	;

RdFragment :    goLatexLike ArgItems  		{ $$ = $2; UNPROTECT_PTR($1); }
	;
	
RdFile	:	SectionList			{ $$ = $1; }
	;

SectionList:	Section				{ $$ = xxnewlist($1); }
	|	SectionList Section		{ $$ = xxlist($1, $2); }
	
Section:	VSECTIONHEADER VerbatimArg	{ $$ = xxmarkup($1, $2, STATIC, &@$); }	
	|	RDOPTS VerbatimArg		{ $$ = xxmarkup($1, $2, HAS_SEXPR, &@$); }
	|	RSECTIONHEADER RLikeArg		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	SECTIONHEADER  LatexArg  	{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	LISTSECTION    Item2Arg		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	SECTIONHEADER2 LatexArg LatexArg2 { $$ = xxmarkup2($1, $2, $3, 2, STATIC, &@$); }
	|	IFDEF IfDefTarget SectionList ENDIF { $$ = xxmarkup2($1, $2, $3, 2, HAS_IFDEF, &@$); UNPROTECT_PTR($4); } 
	|	IFDEF IfDefTarget SectionList error { $$ = xxmarkup2($1, $2, $3, 2, HAS_IFDEF, &@$); }
	|	SEXPR       goOption RLikeArg2   { $$ = xxmarkup($1, $3, HAS_SEXPR, &@$); xxpopMode($2); }
	|	SEXPR       goOption Option RLikeArg2 { $$ = xxOptionmarkup($1, $3, $4, HAS_SEXPR, &@$); xxpopMode($2); }
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	TEXT				{ $$ = xxtag($1, TEXT, &@$); } /* must be whitespace */
	|	UserMacro			{ $$ = $1; }
	|	error Section			{ $$ = $2; }

ArgItems:	Item				{ $$ = xxnewlist($1); }
	|	ArgItems Item			{ $$ = xxlist($1, $2); }
	
Item:		TEXT				{ $$ = xxtag($1, TEXT, &@$); }
	|	RCODE				{ $$ = xxtag($1, RCODE, &@$); }
	|	VERB				{ $$ = xxtag($1, VERB, &@$); }
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	UNKNOWN				{ $$ = xxtag($1, UNKNOWN, &@$); yyerror(yyunknown); }
	|	Arg				{ $$ = xxmarkup(R_NilValue, $1, STATIC, &@$); }
	|	Markup				{ $$ = $1; }	
	|	UserMacro			{ $$ = $1; }
	|	error Item			{ $$ = $2; }

Markup:		LATEXMACRO  LatexArg 		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	LATEXMACRO2 LatexArg LatexArg2  { $$ = xxmarkup2($1, $2, $3, 2, STATIC, &@$); }
	|	LATEXMACRO3 LatexArg LatexArg2 LatexArg2 { $$ = xxmarkup3($1, $2, $3, $4, STATIC, &@$); }
	|	ITEMIZE     Item0Arg		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	DESCRIPTION Item2Arg		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	OPTMACRO    goOption LatexArg  	{ $$ = xxmarkup($1, $3, STATIC, &@$); xxpopMode($2); }
	|	OPTMACRO    goOption Option LatexArg { $$ = xxOptionmarkup($1, $3, $4, STATIC, &@$); xxpopMode($2); }
	|	RCODEMACRO  RLikeArg     	{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	SEXPR       goOption RLikeArg2   { $$ = xxmarkup($1, $3, HAS_SEXPR, &@$); xxpopMode($2); }
	|	SEXPR       goOption Option RLikeArg2 { $$ = xxOptionmarkup($1, $3, $4, HAS_SEXPR, &@$); xxpopMode($2); }
	|	VERBMACRO   VerbatimArg		{ $$ = xxmarkup($1, $2, STATIC, &@$); }
	|	VERBMACRO2  VerbatimArg1	{ $$ = xxmarkup2($1, $2, R_NilValue, 1, STATIC, &@$); }
	|       VERBMACRO2  VerbatimArg1 VerbatimArg2 { $$ = xxmarkup2($1, $2, $3, 2, STATIC, &@$); }
	|	ESCAPE				{ $$ = xxmarkup($1, R_NilValue, STATIC, &@$); }
	|	IFDEF IfDefTarget ArgItems ENDIF { $$ = xxmarkup2($1, $2, $3, 2, HAS_IFDEF, &@$); UNPROTECT_PTR($4); }
	|	IFDEF IfDefTarget ArgItems error { $$ = xxmarkup2($1, $2, $3, 2, HAS_IFDEF, &@$); }
	|	VERBLATEX   VerbatimArg1 LatexArg2 { $$ = xxmarkup2($1, $2, $3, 2, STATIC, &@$); }
	
UserMacro:	NEWCOMMAND  VerbatimArg1 VerbatimArg { $$ = xxnewcommand($1, $2, $3, &@$); }
	|	USERMACRO			{ $$ = xxusermacro($1, xxnewlist(R_NoObject), &@$); }
	|	USERMACRO1  VerbatimArg		{ $$ = xxusermacro($1, xxnewlist($2), &@$); }
	|	USERMACRO2  VerbatimArg VerbatimArg
						{ $$ = xxusermacro($1, xxnewlist2($2, $3), &@$); }
	|	USERMACRO3  VerbatimArg VerbatimArg VerbatimArg
						{ $$ = xxusermacro($1, xxnewlist3($2, $3, $4), &@$); }
	|	USERMACRO4  VerbatimArg VerbatimArg VerbatimArg VerbatimArg 
						{ $$ = xxusermacro($1, xxnewlist4($2, $3, $4, $5), &@$); }
	|	USERMACRO5  VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg
						{ $$ = xxusermacro($1, xxnewlist5($2, $3, $4, $5, $6), &@$); }
	|	USERMACRO6  VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg 
			    VerbatimArg		{ $$ = xxusermacro($1, xxnewlist6($2, $3, $4, $5, $6, $7), &@$); }
	|	USERMACRO7  VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg
			    VerbatimArg VerbatimArg 
			    			{ $$ = xxusermacro($1, xxnewlist7($2, $3, $4, $5, $6, $7, $8), &@$); }
	|	USERMACRO8  VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg
			    VerbatimArg VerbatimArg VerbatimArg
			    			{ $$ = xxusermacro($1, xxnewlist8($2, $3, $4, $5, $6, $7, $8, $9), &@$); }
	|	USERMACRO9  VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg VerbatimArg
			    VerbatimArg VerbatimArg VerbatimArg VerbatimArg
			    			{ $$ = xxusermacro($1, xxnewlist9($2, $3, $4, $5, $6, $7, $8, $9, $10), &@$); }
						
	
LatexArg:	goLatexLike Arg		 	{ xxpopMode($1); $$ = $2; }

LatexArg2:	goLatexLike Arg			{ xxpopMode($1); $$ = $2; }
	|	goLatexLike TEXT		{ xxpopMode($1); $$ = xxnewlist($2); 
     						  if(wCalls)
    	    					      warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, @2.first_line, @2.first_column); 
     						  else
    	    					      warningcall(R_NilValue, _("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, @2.first_line, @2.first_column); 
						}	

Item0Arg:	goItem0 Arg		 	{ xxpopMode($1); $$ = $2; }

Item2Arg:	goItem2 Arg			{ xxpopMode($1); $$ = $2; }

RLikeArg:	goRLike Arg			{ xxpopMode($1); $$ = $2; }

/* This one is like VerbatimArg2 below:  it does the push after seeing the brace */

RLikeArg2:	'{' goRLike2 ArgItems '}'	{ xxpopMode($2); $$ = $3; }
	|	'{' goRLike2 '}'		{ xxpopMode($2); $$ = xxnewlist(R_NoObject); }

VerbatimArg:	goVerbatim Arg		 	{ xxpopMode($1); $$ = $2; }

VerbatimArg1:	goVerbatim1 Arg			{ xxpopMode($1); $$ = $2; }

/* This one executes the push after seeing the brace starting the optional second arg */

VerbatimArg2:   '{' goVerbatim2 ArgItems '}'    { xxpopMode($2); $$ = $3; }
	|	'{' goVerbatim2 '}'		{ xxpopMode($2); $$ = xxnewlist(R_NoObject); }

IfDefTarget:	goLatexLike TEXT	{ xxpopMode($1); $$ = xxnewlist(xxtag($2, TEXT, &@$)); }


goLatexLike:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); }

goRLike:	/* empty */			{ $$ = xxpushMode(RLIKE, UNKNOWN, FALSE); }

goRLike2:	/* empty */			{ xxbraceDepth--; $$ = xxpushMode(RLIKE, UNKNOWN, FALSE); xxbraceDepth++; }

goOption:	/* empty */			{ $$ = xxpushMode(INOPTION, UNKNOWN, FALSE); }

goVerbatim:	/* empty */			{ $$ = xxpushMode(VERBATIM, UNKNOWN, FALSE); }

goVerbatim1:	/* empty */			{ $$ = xxpushMode(VERBATIM, UNKNOWN, TRUE); }

goVerbatim2:    /* empty */			{ xxbraceDepth--; $$ = xxpushMode(VERBATIM, UNKNOWN, FALSE); xxbraceDepth++; }

goItem0:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, ESCAPE, FALSE); }

goItem2:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); }

Arg:		'{' ArgItems  '}'		{ $$ = $2; }
	|	'{' '}'				{ $$ = xxnewlist(R_NoObject); }
	|	'{' ArgItems error '}'		{ $$ = $2; }
	|	'{' error '}'			{ $$ = xxnewlist(R_NoObject); }
	|	'{' ArgItems error END_OF_INPUT { $$ = $2; }

Option:		'[' Item ']'			{ $$ = $2; }	
		
%%

static SEXP xxpushMode(int newmode, int newitem, int neweqn)
{
    SEXP ans;
    PROTECT(ans = allocVector(INTSXP, 7));
    
    INTEGER(ans)[0] = xxmode;		/* Lexer mode */
    INTEGER(ans)[1] = xxitemType;	/* What is \item? */
    INTEGER(ans)[2] = xxbraceDepth;	/* Brace depth used in RCODE and VERBATIM */
    INTEGER(ans)[3] = xxinRString;      /* Quote char that started a string */
    INTEGER(ans)[4] = xxQuoteLine;      /* Where the quote was */
    INTEGER(ans)[5] = xxQuoteCol;       /*           "         */
    INTEGER(ans)[6] = xxinEqn;          /* In the first arg to \eqn or \deqn:  no escapes */
    
#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = newmode;
    xxitemType = newitem;
    xxbraceDepth = 0;
    xxinRString = 0;
    xxinEqn = neweqn;
    
    return ans;
}

static void xxpopMode(SEXP oldmode) 
{
#if DEBUGVALS
    Rprintf("xxpopMode(%d, %s, %d) replaces %d, %s, %d\n", INTEGER(oldmode)[0], yytname[YYTRANSLATE(INTEGER(oldmode)[1])], INTEGER(oldmode)[2], 
    					xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = INTEGER(oldmode)[0];
    xxitemType = INTEGER(oldmode)[1]; 
    xxbraceDepth = INTEGER(oldmode)[2];
    xxinRString = INTEGER(oldmode)[3];
    xxQuoteLine = INTEGER(oldmode)[4];
    xxQuoteCol  = INTEGER(oldmode)[5];
    xxinEqn	= INTEGER(oldmode)[6];
    
    UNPROTECT_PTR(oldmode);
}

static int getDynamicFlag(SEXP item)
{
    SEXP flag = getAttrib(item, install("dynamicFlag"));
    if (isNull(flag)) return 0;
    else return INTEGER(flag)[0];
}

static void setDynamicFlag(SEXP item, int flag)
{
    if (flag) {
        SEXP dynamicFlag_install = install("dynamicFlag"); /* prot by sym tbl */
    	setAttrib(item, dynamicFlag_install, ScalarInteger(flag));
    }
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans, tmp;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PROTECT(tmp = NewList());
    if (item != R_NoObject) {
    	int flag = getDynamicFlag(item);
    	PROTECT(ans = GrowList(tmp, item));
    	setDynamicFlag(ans, flag);
    	UNPROTECT_PTR(tmp);
    	UNPROTECT_PTR(item);
    } else ans = tmp;
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxnewlist2(SEXP item1, SEXP item2)
{
    return xxlist(xxnewlist(item1), item2);
}

static SEXP xxnewlist3(SEXP item1, SEXP item2, SEXP item3)
{
    return xxlist(xxnewlist2(item1, item2), item3);
}

static SEXP xxnewlist4(SEXP item1, SEXP item2, SEXP item3, SEXP item4)
{
    return xxlist(xxnewlist3(item1, item2, item3), item4);
}

static SEXP xxnewlist5(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5)
{
    return xxlist(xxnewlist4(item1, item2, item3, item4), item5);
}

static SEXP xxnewlist6(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6)
{
    return xxlist(xxnewlist5(item1, item2, item3, item4, item5), item6);
}

static SEXP xxnewlist7(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7)
{
    return xxlist(xxnewlist6(item1, item2, item3, item4, item5, item6), item7);
}

static SEXP xxnewlist8(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8)
{
    return xxlist(xxnewlist7(item1, item2, item3, item4, item5, item6, item7), item8);
}

static SEXP xxnewlist9(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8, SEXP item9)
{
    return xxlist(xxnewlist8(item1, item2, item3, item4, item5, item6, item7, item8), 
                  item9);
}

static SEXP xxlist(SEXP oldlist, SEXP item)
{
    SEXP ans;
    int flag = getDynamicFlag(oldlist) | getDynamicFlag(item);
#if DEBUGVALS
    Rprintf("xxlist(oldlist=%p, item=%p)", oldlist, item);
#endif
    PROTECT(ans = GrowList(oldlist, item));
    UNPROTECT_PTR(item);
    UNPROTECT_PTR(oldlist);
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxmarkup(SEXP header, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup(header=%p, body=%p)", header, body);    
#endif
    if (isNull(body)) 
        PROTECT(ans = allocVector(VECSXP, 0));
    else {
        flag |= getDynamicFlag(body);
	PROTECT(ans = PairToVectorList(CDR(body)));
    	UNPROTECT_PTR(body);	
    }
    if (isNull(header))
    	PROTECT(header = mkString("LIST"));
    	
    setAttrib(ans, install("Rd_tag"), header);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    UNPROTECT_PTR(header);
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxnewcommand(SEXP cmd, SEXP name, SEXP defn, YYLTYPE *lloc)
{
    SEXP ans, prev, thename, thedefn;
    char buffer[128];
    const char *c;
    int maxarg = 0;
#if DEBUGVALS
    Rprintf("xxnewcommand(cmd=%p, name=%p, defn=%p)", cmd, name, defn);
#endif
    thename = CADR(name);
    thedefn = CADR(defn);
    if (TYPEOF(thedefn) == STRSXP)
    	PROTECT(thedefn = mkString(CHAR(STRING_ELT(thedefn,0))));
    else
    	PROTECT(thedefn = mkString(""));
    prev = findVar(installChar(STRING_ELT(thename, 0)), xxMacroList);
    if (prev != R_UnboundValue && !strcmp(CHAR(STRING_ELT(cmd,0)), "\renewcommand")) {
        snprintf(buffer, sizeof(buffer), _("Macro '%s' previously defined."), 
                 CHAR(STRING_ELT(thename, 0)));
        yyerror(buffer);
    }
    for (c = CHAR(STRING_ELT(thedefn, 0)); *c; c++) {
    	if (*c == '#' && isdigit(*(c+1))) 
    	    maxarg = imax2(maxarg, *(c+1) - '0');
    }
    if (maxarg > 4) {
    	snprintf(buffer, sizeof(buffer), _("At most 4 arguments are allowed for user defined macros."));
	yyerror(buffer);
    }
    PROTECT(ans = ScalarInteger(USERMACRO + maxarg));
    setAttrib(ans, install("Rd_tag"), cmd);
    setAttrib(ans, install("definition"), thedefn);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    defineVar(installChar(STRING_ELT(thename, 0)), ans, xxMacroList);

    UNPROTECT_PTR(thedefn);
    UNPROTECT_PTR(cmd);
    UNPROTECT_PTR(name);
    UNPROTECT_PTR(defn); 
    return ans;
}

#define START_MACRO -2
#define END_MACRO -3
    	
static SEXP xxusermacro(SEXP macro, SEXP args, YYLTYPE *lloc)
{
    SEXP ans, value, nextarg;
    int i,len;
    const char *c, *start ;
    
#if DEBUGVALS
    Rprintf("xxusermacro(macro=%p, args=%p)", macro, args);
#endif
    len = length(args)-1;
    PROTECT(ans = allocVector(STRSXP, len + 1));
    value = UserMacroLookup(CHAR(STRING_ELT(macro,0)));
    if (TYPEOF(value) == STRSXP)
    	SET_STRING_ELT(ans, 0, STRING_ELT(value, 0));
    else
    	error(_("No macro definition for '%s'."), CHAR(STRING_ELT(macro,0)));
/*    Rprintf("len = %d", len); */
    for (i = 0, nextarg=args; i < len; i++, nextarg = CDR(nextarg)) {
/*        Rprintf("arg i is");
        PrintValue(CADR(CADR(nextarg))); */
	SET_STRING_ELT(ans, i+1, STRING_ELT(CADR(CADR(nextarg)), 0));
    }	
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(macro);    
    /* Now push the expanded macro onto the input stream, in reverse order */
    xxungetc(END_MACRO);
    start = CHAR(STRING_ELT(ans, 0));
    for (c = start + strlen(start); c > start; c--) {
    	if (c > start + 1 && *(c-2) == '#' && isdigit(*(c-1))) {
    	    int which = *(c-1) - '0';
    	    const char *arg = CHAR(STRING_ELT(ans, which));
    	    for (i = strlen(arg); i > 0; i--) {
    	    	xxungetc(arg[i-1]);
    	    }
    	    c--;
    	} else {
    	    xxungetc(*(c-1));
    	}
    }
    xxungetc(START_MACRO);

    SEXP Rd_tag_install = install("Rd_tag"); /* protected by symbol table */
    setAttrib(ans, Rd_tag_install, mkString("USERMACRO"));
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);
#endif
    return ans;
}
    
static SEXP xxOptionmarkup(SEXP header, SEXP option, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxOptionmarkup(header=%p, option=%p, body=%p)", header, option, body);    
#endif
    flag |= getDynamicFlag(body);
    PROTECT(ans = PairToVectorList(CDR(body)));
    UNPROTECT_PTR(body);	
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);
    flag |= getDynamicFlag(option);
    setAttrib(ans, install("Rd_option"), option);
    UNPROTECT_PTR(option);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);    
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup2(SEXP header, SEXP body1, SEXP body2, int argcount, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p)", header, body1, body2);        
#endif
    
    PROTECT(ans = allocVector(VECSXP, argcount));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	if (argcount < 2) error("internal error: inconsistent argument count");
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    	UNPROTECT_PTR(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup3(SEXP header, SEXP body1, SEXP body2, SEXP body3, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p, body3=%p)", header, body1, body2, body3);        
#endif
    
    PROTECT(ans = allocVector(VECSXP, 3));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    	UNPROTECT_PTR(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    if (!isNull(body3)) {
    	int flag3;
	flag3 = getDynamicFlag(body3);
    	SET_VECTOR_ELT(ans, 2, PairToVectorList(CDR(body3)));    
    	UNPROTECT_PTR(body3);
    	setDynamicFlag(VECTOR_ELT(ans, 2), flag3);
    	flag |= flag3;
    }    
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static void xxsavevalue(SEXP Rd, YYLTYPE *lloc)
{
    int flag = getDynamicFlag(Rd);
    PROTECT(Value = PairToVectorList(CDR(Rd)));
    if (!isNull(Value)) {
    	setAttrib(Value, R_ClassSymbol, mkString("Rd"));
    	setAttrib(Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    	setDynamicFlag(Value, flag);
    }
    UNPROTECT_PTR(Rd);
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    SEXP Rd_tag_install = install("Rd_tag"); /* protected by symbol table */
    setAttrib(item, Rd_tag_install, mkString(yytname[YYTRANSLATE(type)]));
    setAttrib(item, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    return item;
}

static void xxWarnNewline()
{
    if (xxNewlineInString) {
	if(wCalls)
	    warning(_("newline within quoted string at %s:%d"), 
		    xxBasename, xxNewlineInString);
	else
	    warningcall(R_NilValue,
			_("newline within quoted string at %s:%d"), 
			xxBasename, xxNewlineInString);
    }
}

  
/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need arbitrarily large size, since this is how macros are expanded. */
   
#define PUSH_BACK(c) do {                  \
	if (npush >= pushsize - 1) {             \
	    int *old = pushbase;              \
            pushsize *= 2;                    \
	    pushbase = malloc(pushsize*sizeof(int));         \
	    if(!pushbase) error(_("unable to allocate buffer for long macro at line %d"), xxlineno);\
	    memmove(pushbase, old, npush*sizeof(int));        \
	    if(old != pushback) free(old); }	    \
	pushbase[npush++] = (c);                        \
} while(0)

   

#define PUSHBACK_BUFSIZE 32

static int pushback[PUSHBACK_BUFSIZE];
static int *pushbase;
static unsigned int npush, pushsize;
static int macrolevel;
static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];


static int xxgetc(void)
{
    int c, oldpos;
    
    do {
    	if(npush) {    	
    	    c = pushbase[--npush]; 
    	    if (c == START_MACRO) {
    	    	macrolevel++;
    	    	if (macrolevel > 1000) 
    	    	    error(_("macros nested too deeply: infinite recursion?"));
    	    } else if (c == END_MACRO) macrolevel--;
    	} else  c = ptr_getc();
    } while (c == START_MACRO || c == END_MACRO);
    
    if (!macrolevel) {
	oldpos = prevpos;
	prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
	prevbytes[prevpos] = xxbyteno;
	prevlines[prevpos] = xxlineno;    
	/* We only advance the column for the 1st byte in UTF-8, so handle later bytes specially */
	if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF) {
	    xxcolno--;   
	    prevcols[prevpos] = prevcols[oldpos];
	} else 
	    prevcols[prevpos] = xxcolno;

	if (c == EOF) return R_EOF;

	R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
	R_ParseContext[R_ParseContextLast] = c;

	if (c == '\n') {
	    xxlineno += 1;
	    xxcolno = 1;
	    xxbyteno = 1;
	} else {
	    xxcolno++;
	    xxbyteno++;
	}

	if (c == '\t') xxcolno = ((xxcolno + 6) & ~7) + 1;

	R_ParseContextLine = xxlineno;
    }
    /* Rprintf("get %c\n", c); */
    return c;
}

static int xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    if (c == END_MACRO) macrolevel++;
    if (!macrolevel) {
    	xxlineno = prevlines[prevpos];
    	xxbyteno = prevbytes[prevpos];
    	xxcolno  = prevcols[prevpos];
    	prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;
    
    	R_ParseContextLine = xxlineno;
    
    	R_ParseContext[R_ParseContextLast] = '\0';
    	/* Mac OS X requires us to keep this non-negative */
    	R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
		% PARSE_CONTEXT_SIZE;
    }
    if (c == START_MACRO) macrolevel--;
    PUSH_BACK(c);
    /* Rprintf("unget %c;", c); */
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile)
{
    SEXP val;
    
    PROTECT(val = allocVector(INTSXP, 6));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
    setAttrib(val, R_SrcfileSymbol, srcfile);
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1);
    return val;
}

static SEXP mkString2(const char *s, int len)
{
    SEXP t;
    cetype_t enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
}


/* Stretchy List Structures : Lists are created and grown using a special */
/* dotted pair.  The CAR of the list points to the last cons-cell in the */
/* list and the CDR points to the first.  The list can be extracted from */
/* the pair by taking its CDR, while the CAR gives fast access to the end */
/* of the list. */


/* Create a stretchy-list dotted pair */

static SEXP NewList(void)
{
    SEXP s = CONS(R_NilValue, R_NilValue);
    SETCAR(s, s);
    return s;
}

/* Add a new element at the end of a stretchy list */

static SEXP GrowList(SEXP l, SEXP s)
{
    SEXP tmp;
    PROTECT(s);
    tmp = CONS(s, R_NilValue);
    UNPROTECT(1);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
    return l;
}

/*--------------------------------------------------------------------------*/

/*
 *  Parsing Entry Points:
 *
 *  The Following entry points provide Rd parsing facilities.
 *
 *	SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile)
 *
 */
 
static SEXP ParseRd(ParseStatus *status, SEXP srcfile, Rboolean fragment)
{
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
    
    xxlineno = 1;
    xxcolno = 1; 
    xxbyteno = 1;
    
    SrcFile = srcfile;
    
    npush = 0;
    pushbase = pushback;
    pushsize = PUSHBACK_BUFSIZE;
    macrolevel = 0;
    
    xxmode = LATEXLIKE; 
    xxitemType = UNKNOWN;
    xxbraceDepth = 0;
    xxinRString = 0;
    xxNewlineInString = 0;
    xxinEqn = 0;
    if (fragment) xxinitvalue = STARTFRAGMENT;
    else	  xxinitvalue = STARTFILE;
    
    xxMacroList = InstallKeywords();
    
    Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", Value);    
#endif    
    UNPROTECT_PTR(Value);
    UNPROTECT_PTR(xxMacroList);
    
    if (pushbase != pushback) free(pushbase);
    
    return Value;
}

#include "Rconnections.h"
static Rconnection con_parse;

/* need to handle incomplete last line */
static int con_getc(void)
{
    int c;
    static int last=-1000;
    
    c = Rconn_fgetc(con_parse);
    if (c == EOF && last != '\n') c = '\n';
    return (last = c);
}

attribute_hidden
SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile, Rboolean fragment)
{
    con_parse = con;
    ptr_getc = con_getc;
    return ParseRd(status, srcfile, fragment);
}

/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  
 *
 */


/* Special Symbols */
/* Section and R code headers */

struct {
    char *name;
    int token;
}

/* When adding keywords here, make sure all the handlers 
   are also modified:  checkRd, Rd2HTML, Rd2latex, Rd2txt, any other new ones... */
   
static keywords[] = {
    /* These sections contain Latex-like text */
    
    { "\\author",  SECTIONHEADER },
    { "\\concept", SECTIONHEADER },
    { "\\description",SECTIONHEADER },
    { "\\details", SECTIONHEADER },
    { "\\docType", SECTIONHEADER },
    
    { "\\encoding",SECTIONHEADER },
    { "\\format",  SECTIONHEADER },
    { "\\keyword", SECTIONHEADER },
    { "\\note",    SECTIONHEADER },    
    { "\\references", SECTIONHEADER },
    
    { "\\section", SECTIONHEADER2 },    
    { "\\seealso", SECTIONHEADER },
    { "\\source",  SECTIONHEADER },
    { "\\title",   SECTIONHEADER },

    /* These sections contain R-like text */
    
    { "\\examples",RSECTIONHEADER },
    { "\\usage",   RSECTIONHEADER },
    
    /* These sections contain verbatim text */
    
    { "\\alias",   VSECTIONHEADER }, 
    { "\\name",    VSECTIONHEADER },
    { "\\synopsis",VSECTIONHEADER }, 
    { "\\Rdversion",VSECTIONHEADER },
    
    /* These macros take no arguments.  One character non-alpha escapes get the
       same token value */

    { "\\cr",      ESCAPE },
    { "\\dots",    ESCAPE },
    { "\\ldots",   ESCAPE },
    { "\\R",       ESCAPE },    
    { "\\tab",     ESCAPE },
    
    /* These macros take one LaTeX-like argument. */
    
    { "\\acronym", LATEXMACRO },
    { "\\bold",    LATEXMACRO },
    { "\\cite",    LATEXMACRO },
    { "\\command", LATEXMACRO },
    { "\\dfn",     LATEXMACRO },
    { "\\dQuote",  LATEXMACRO },
    { "\\email",   LATEXMACRO },
    
    { "\\emph",    LATEXMACRO },    
    { "\\file",    LATEXMACRO },
    { "\\linkS4class", LATEXMACRO },
    { "\\pkg",	   LATEXMACRO },
    { "\\sQuote",  LATEXMACRO },
    
    { "\\strong",  LATEXMACRO },
    
    { "\\var",     LATEXMACRO },
    
    /* These are like SECTIONHEADER/LATEXMACRO, but they change the interpretation of \item */

    { "\\arguments",LISTSECTION },
    { "\\value",   LISTSECTION },
    
    { "\\describe",DESCRIPTION },
    { "\\enumerate",ITEMIZE },
    { "\\itemize", ITEMIZE },

    { "\\item",    NOITEM }, /* will change to UNKNOWN, ESCAPE, or LATEXMACRO2 depending on context */
    
    /* These macros take two LaTeX-like arguments. */
    
    { "\\enc",     LATEXMACRO2 },
    { "\\if",      LATEXMACRO2 },
    { "\\method",  LATEXMACRO2 },
    { "\\S3method",LATEXMACRO2 },
    { "\\S4method",LATEXMACRO2 },
    { "\\tabular", LATEXMACRO2 },
    { "\\subsection", LATEXMACRO2 },
    
    /* This macro takes one verbatim and one LaTeX-like argument. */
    
    { "\\href",    VERBLATEX },
    
    /* This macro takes three LaTeX-like arguments. */
    
    { "\\ifelse",  LATEXMACRO3 },
    
    /* These macros take one optional bracketed option and always take 
       one LaTeX-like argument */
       
    { "\\link",    OPTMACRO },
       
    /* These markup macros require an R-like text argument */
    
    { "\\code",    RCODEMACRO },
    { "\\dontshow",RCODEMACRO },
    { "\\donttest",RCODEMACRO },
    { "\\testonly",RCODEMACRO },
    
    /* This macro takes one optional bracketed option and one R-like argument */
    
    { "\\Sexpr",   SEXPR },
    
    /* This is just like a VSECTIONHEADER, but it needs SEXPR processing */
    
    { "\\RdOpts",   RDOPTS },
    
    /* These macros take one verbatim arg and ignore everything except braces */
    
    { "\\dontrun", VERBMACRO }, /* at least for now */    
    { "\\env",     VERBMACRO },
    { "\\kbd", 	   VERBMACRO },	
    { "\\option",  VERBMACRO },
    { "\\out",     VERBMACRO },
    { "\\preformatted", VERBMACRO },
    
    { "\\samp",    VERBMACRO },
    { "\\special", VERBMACRO },
    { "\\url",     VERBMACRO },
    { "\\verb",    VERBMACRO },
    
    /* These ones take one or two verbatim args */
    
    { "\\eqn",     VERBMACRO2 },
    { "\\deqn",    VERBMACRO2 },
    { "\\figure",  VERBMACRO2 },
    
    /* We parse IFDEF/IFNDEF as markup, not as a separate preprocessor step */ 
    
    { "#ifdef",    IFDEF },
    { "#ifndef",   IFDEF },
    { "#endif",    ENDIF },
    
    /* These allow user defined macros */
    { "\\newcommand", NEWCOMMAND },
    { "\\renewcommand", NEWCOMMAND },
    
    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7   

static SEXP InstallKeywords()
{
    int i, num;
    SEXP result, name, val;
    num = sizeof(keywords)/sizeof(keywords[0]);
    PROTECT(result = R_NewHashedEnv(R_EmptyEnv, ScalarInteger(num)));
    for (i = 0; keywords[i].name; i++) {
        PROTECT(name = install(keywords[i].name));
        PROTECT(val = ScalarInteger(keywords[i].token));
    	defineVar(name, val, result);
    	UNPROTECT(2);
    }
    return result;
}
    	
static int KeywordLookup(const char *s)
{
    SEXP rec = findVar(install(s), xxMacroList);
    if (rec == R_UnboundValue) return UNKNOWN;
    else return INTEGER(rec)[0];
}

static SEXP UserMacroLookup(const char *s)
{
    SEXP rec = findVar(install(s), xxMacroList);
    if (rec == R_UnboundValue) error(_("Unable to find macro %s"), s);
    PROTECT(rec);
    SEXP res = getAttrib(rec, install("definition"));
    UNPROTECT(1);
    return res;
}

static void yyerror(const char *s)
{
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */    
#define YYENGLISH 17
	"$undefined",	"input", 	
	"SECTIONHEADER","section header",
	"RSECTIONHEADER","section header",
	"VSECTIONHEADER","section header",
	"LISTSECTION",	"section header",
	
	"LATEXMACRO",	"macro",
	"LATEXMACRO2",  "macro",
	"LATEXMACRO3",  "macro",
	"RCODEMACRO",	"macro",
	"VERBMACRO",    "macro",
	"VERBMACRO2",	"macro",
	
	"ESCAPE",	"macro",
	"ITEMIZE",	"macro",
	"IFDEF",	"conditional",
	"SECTIONHEADER2","section header",
	"OPTMACRO",	"macro",
	
	"DESCRIPTION",	"macro",
	"VERB",		"VERBATIM TEXT",
	0,		0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    static char const yyshortunexpected[] = "unexpected %s";
    static char const yylongunexpected[] = "unexpected %s '%s'";
    char *expecting;
    char ParseErrorMsg[PARSE_ERROR_SIZE];
    SEXP filename;
    char ParseErrorFilename[PARSE_ERROR_SIZE];
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("macro");
    _("conditional");
    _("section header");
#endif 
   
    xxWarnNewline();	/* post newline warning if necessary */
    
    /*
    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = SrcFile;
    */
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i, translated = FALSE;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i = 0; yytname_translations[i]; i += 2) {
    	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	    	if (yychar < 256)
    	    	    sprintf(ParseErrorMsg, _(yyshortunexpected), 
    	    	        i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                        : yytname_translations[i+1]);
    	    	else
    	    	    sprintf(ParseErrorMsg, _(yylongunexpected), 
    	    	        i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                        : yytname_translations[i+1], 
    	    	        CHAR(STRING_ELT(yylval, 0)));
    	    	translated = TRUE;
    	    	break;
    	    }
    	}
    	if (!translated) {
    	    if (yychar < 256) 
    		sprintf(ParseErrorMsg, _(yyshortunexpected),
    	            s + sizeof yyunexpected - 1);
    	    else
    	    	sprintf(ParseErrorMsg, _(yylongunexpected),
    	            s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
	}
    	if (expecting) {
 	    translated = FALSE;
    	    for (i = 0; yytname_translations[i]; i += 2) {
    	    	if (!strcmp(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
    	    	    strcat(ParseErrorMsg, _(yyexpecting));
    	    	    strcat(ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	    translated = TRUE;
		    break;
		}
	    }
	    if (!translated) {
	    	strcat(ParseErrorMsg, _(yyexpecting));
	    	strcat(ParseErrorMsg, expecting + sizeof yyexpecting - 1);
	    }
	}
    } else if (!strncmp(s, yyunknown, sizeof yyunknown-1)) {
    	sprintf(ParseErrorMsg, "%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
    } else {
    	sprintf(ParseErrorMsg, "%s", s);
    }
    filename = findVar(install("filename"), SrcFile);
    if (isString(filename) && length(filename))
    	strncpy(ParseErrorFilename, CHAR(STRING_ELT(filename, 0)), PARSE_ERROR_SIZE - 1);
    else
        ParseErrorFilename[0] = '\0';
    if (wCalls) {
	if (yylloc.first_line != yylloc.last_line)
	    warning("%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warning("%s:%d: %s", 
		    ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
    } else {
	if (yylloc.first_line != yylloc.last_line)
	    warningcall(R_NilValue, "%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warningcall(R_NilValue, "%s:%d: %s", 
			ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
    }
}

#define TEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
            nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)

static void setfirstloc(void)
{
    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;
    yylloc.first_byte = xxbyteno;
}

static void setlastloc(void)
{
    yylloc.last_line = prevlines[prevpos];
    yylloc.last_column = prevcols[prevpos];
    yylloc.last_byte = prevbytes[prevpos];
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c, lookahead;
    int outsideLiteral = xxmode == LATEXLIKE || xxmode == INOPTION || xxbraceDepth == 0;

    if (xxinitvalue) {
        yylloc.first_line = 0;
        yylloc.first_column = 0;
        yylloc.first_byte = 0;
        yylloc.last_line = 0;
        yylloc.last_column = 0;
        yylloc.last_byte = 0;
    	PROTECT(yylval = mkString(""));
        c = xxinitvalue;
    	xxinitvalue = 0;
    	return(c);
    }
    
    setfirstloc();    
    c = xxgetc();

    switch (c) {
    	case '%': if (!xxinEqn) return mkComment(c);
    	    break;
	case '\\':
	    if (!xxinEqn) {
		lookahead = xxungetc(xxgetc());
		if (isalpha(lookahead) && xxmode != VERBATIM 
		    /* In R strings, only link or var is allowed as markup */
		    && (lookahead == 'l' || lookahead == 'v' || !xxinRString)) 
		    return mkMarkup(c);
	    }
	    break;
        case R_EOF:
            if (xxinRString) {
       		xxWarnNewline();
       		error(_("Unexpected end of input (in %c quoted string opened at %s:%d:%d)"), 
 			xxinRString, xxBasename, xxQuoteLine, xxQuoteCol);
    	    }
    	    return END_OF_INPUT; 
    	case '#':
    	    if (!xxinEqn && yylloc.first_column == 1) return mkIfdef(c);
    	    break;
    	case LBRACE:
    	    if (!xxinRString) {
    	    	xxbraceDepth++;
    	    	if (outsideLiteral) return c;
    	    }
    	    break;
    	case RBRACE:
    	    if (!xxinRString) {
    	    	xxbraceDepth--;
    	    	if (outsideLiteral || xxbraceDepth == 0) return c;
    	    }
    	    break;
    	case '[':
    	case ']':
    	    if (xxmode == INOPTION ) return c; 
    	    break;
    } 	    
	
    switch (xxmode) {
	case RLIKE:     return mkCode(c);
	case INOPTION:
	case LATEXLIKE: return mkText(c);
	case VERBATIM:  return mkVerb(c);
    }
 
    return ERROR; /* We shouldn't get here. */
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0, lookahead;
    
    while(1) {
    	switch (c) {
    	case '\\': 
    	    lookahead = xxgetc();
    	    if (lookahead == LBRACE || lookahead == RBRACE ||
    	        lookahead == '%' || lookahead == '\\') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    if (isalpha(lookahead)) goto stop;
    	case ']':
    	    if (xxmode == INOPTION) goto stop;
            break;
    	case '%':
    	case LBRACE:
    	case RBRACE:
    	case R_EOF:
    	    goto stop;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') goto stop;
    	c = xxgetc();
    };
stop:
    if (c != '\n') xxungetc(c); /* newline causes a break, but we keep it */
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);
    return TEXT;
}

static int mkComment(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    do TEXT_PUSH(c);
    while ((c = xxgetc()) != '\n' && c != R_EOF);
    
    xxungetc(c);
    
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);    
    return COMMENT;
}

static int mkCode(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE && !xxinRString) xxbraceDepth--;
    if (c == RBRACE && !xxinRString) xxbraceDepth++; 
    
    while(1) {
	int escaped = 0;
    	if (c == '\\') {
    	    int lookahead = xxgetc();
    	    if (lookahead == '\\' || lookahead == '%') {
    	         c = lookahead;
    	         escaped = 1;
    	    } else xxungetc(lookahead);
    	}
    	if ((!escaped && c == '%') || c == R_EOF) break;
    	if (xxinRString) {
    	    /* This stuff is messy, because there are two levels of escaping:
    	       The Rd escaping and the R code string escaping. */
    	    if (c == '\\') {
    		int lookahead = xxgetc();
    		if (lookahead == '\\') { /* This must be the 3rd backslash */
    		    lookahead = xxgetc();
    		    if (lookahead == xxinRString || lookahead == '\\') {	
    	    	    	TEXT_PUSH(c);
    	    	    	c = lookahead;
    	    	    	escaped = 1;
    	    	    } else {
    	    	    	xxungetc(lookahead); /* put back the 4th char */
    	    	    	xxungetc('\\');	     /* and the 3rd */
    	    	    }
    	    	} else if (lookahead == xxinRString) { /* There could be one or two before this */
    	    	    TEXT_PUSH(c);
    	    	    c = lookahead;
    	    	    escaped = 1;
    	    	} else if (!escaped && (lookahead == 'l' || lookahead == 'v')) { 
    	    	    /* assume \link or \var; this breaks vertical tab, but does anyone ever use that? */
    	    	    xxungetc(lookahead);
    	    	    break;
    	    	} else xxungetc(lookahead);
    	    }
    	    if (!escaped && c == xxinRString)
    	    	xxinRString = 0;
    	} else {
    	    if (c == '#') {
    	    	do {
    	    	    int escaped = 0;
    	    	    TEXT_PUSH(c);
    	    	    c = xxgetc();
    	    	    if (c == '\\') {
		        int lookahead = xxgetc();
		        if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		            c = lookahead;
		            escaped = 1;
		        } else xxungetc(lookahead);
    		    }
    	    	    if (c == LBRACE && !escaped) xxbraceDepth++;
    	    	    else if (c == RBRACE && !escaped) xxbraceDepth--;
    	    	} while (c != '\n' && c != R_EOF && xxbraceDepth > 0);
    	    	if (c == RBRACE && !escaped) xxbraceDepth++; /* avoid double counting */
    	    }
    	    if (c == '\'' || c == '"' || c == '`') {
    	    	xxinRString = c;
    	    	xxQuoteLine = xxlineno;
    	    	xxQuoteCol  = xxcolno;
    	    } else if (c == '\\' && !escaped) {
    	    	int lookahead = xxgetc();
    	    	if (lookahead == LBRACE || lookahead == RBRACE) {
		    c = lookahead;
		} else if (isalpha(lookahead)) {
    	    	    xxungetc(lookahead);
    	    	    c = '\\';
    	    	    break;
    	    	} else {
    	    	    TEXT_PUSH('\\');
    	    	    c = lookahead;
    	    	}
    	    } else if (c == LBRACE) {
    	    	xxbraceDepth++;
    	    } else if (c == RBRACE) {
    	    	if (xxbraceDepth == 1) break;
    	    	else xxbraceDepth--;
    	    } else if (c == R_EOF) break;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') {
    	    if (xxinRString && !xxNewlineInString) 
    	    	xxNewlineInString = xxlineno-1;
    	    break;
    	}
    	c = xxgetc();
    }
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);
    return RCODE; 
}

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval = 0, attempt = 0;
    
    TEXT_PUSH(c);
    while (isalnum((c = xxgetc()))) TEXT_PUSH(c);
    
    while (attempt++ < 2) {
    	/* character escapes are processed as text, not markup */
    	if (bp == stext+1) {
    	    TEXT_PUSH(c);
    	    TEXT_PUSH('\0');
    	    retval = TEXT;
    	    c = xxgetc();
    	    break;
    	} else {
    	    TEXT_PUSH('\0');
    	    retval = KeywordLookup(stext);
    	    if (retval == UNKNOWN && attempt == 1) { /* try again, non-digits only */
    	    	bp--; 				     /* pop the \0 */
    	        while (isdigit(*(bp-1))) {
            	    xxungetc(c);
    	            c = *(--bp);                     /* pop the last letter into c */
            	}
            } else {
            	if (retval == NOITEM) 
    	    	    retval = xxitemType;
    	    	break;
    	    }
        }
    }
    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
    if(stext != st0) free(stext);
    xxungetc(c);
    return retval;
}

static int mkIfdef(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval;
    
    TEXT_PUSH(c);
    while (isalpha((c = xxgetc())) && bp - stext <= DIRECTIVE_LEN) TEXT_PUSH(c);
    TEXT_PUSH('\0');
    xxungetc(c);
    
    retval = KeywordLookup(stext);
    PROTECT(yylval = mkString2(stext, bp - stext - 1));
    
    switch (retval) {
    case ENDIF:  /* eat chars to the end of the line */
    	do { c = xxgetc(); }
    	while (c != '\n' && c != R_EOF);
    	break;
    case UNKNOWN:
    	UNPROTECT(1);
    	bp--; bp--;
    	for (; bp > stext; bp--) 
    	    xxungetc(*bp);
    	switch (xxmode) {
    	case RLIKE:     
    	    retval = mkCode(*bp);
    	    break;
    	case INOPTION:
    	case LATEXLIKE:
    	    retval = mkText(*bp);
    	    break;
    	case VERBATIM:
    	    retval = mkVerb(*bp);
    	    break;
	}
	break;
    }
    if(stext != st0) free(stext);
    return retval;
}

static int mkVerb(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE) xxbraceDepth--;
    if (c == RBRACE) xxbraceDepth++;     
    
    while(1) {
    	int escaped = 0;
        if (c == '\\') {
            int lookahead = xxgetc();
            if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		escaped = 1;
		if (xxinEqn) TEXT_PUSH(c);
		c = lookahead;
	    } else xxungetc(lookahead);
        }
        if (c == R_EOF) break;
        if (!escaped) {
    	    if (c == '%' && !xxinEqn) break;
	    else if (c == LBRACE) xxbraceDepth++;
    	    else if (c == RBRACE) {
	    	if (xxbraceDepth == 1) break;
	    	else xxbraceDepth--;
	    }
	}
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    };
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);
    return VERB;  
}

static int yylex(void)
{
    int tok = token();
    
    if (xxDebugTokens) {
        Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
    	if (xxinRString) Rprintf("(in %c%c)", xxinRString, xxinRString);
    	if (tok > 255 && tok != END_OF_INPUT) 
    	    Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

/* "do_parseRd" 

 .Internal( parseRd(file, srcfile, encoding, verbose, basename, warningCalls) )
 If there is text then that is read and the other arguments are ignored.
*/

static SEXP do_parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s = R_NilValue, source;
    Rconnection con;
    Rboolean wasopen, fragment;
    int ifile, wcall;
    ParseStatus status;
    RCNTXT cntxt;

#if DEBUGMODE
    yydebug = 1;
#endif 

    checkArity(op, args);
    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    source = CAR(args);					args = CDR(args);
    /* encoding is unused */
    args = CDR(args);
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    xxDebugTokens = asInteger(CAR(args));		args = CDR(args);
    xxBasename = CHAR(STRING_ELT(CAR(args), 0));	args = CDR(args);
    fragment = asLogical(CAR(args));			args = CDR(args);
    wcall = asLogical(CAR(args));
    if (wcall == NA_LOGICAL)
    	error(_("invalid '%s' value"), "warningCalls");
    wCalls = wcall;

    if (ifile >= 3) {/* file != "" */
	if(!wasopen) {
	    if(!con->open(con)) error(_("cannot open the connection"));
	    /* Set up a context which will close the connection on error */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			 R_NilValue, R_NilValue);
	    cntxt.cend = &con_cleanup;
	    cntxt.cenddata = con;
	}
	if(!con->canread) error(_("cannot read from this connection"));
	s = R_ParseRd(con, &status, source, fragment);
	if(!wasopen) endcontext(&cntxt);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else error(_("invalid Rd file"));
    return s;
}

/* "do_deparseRd" 

 .Internal( deparseRd(element, state) )
*/

static SEXP do_deparseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP e, state, result;
    int  outlen, *statevals, quoteBraces, inRComment;
    const char *c;
    char *outbuf, *out, lookahead;
    Rboolean escape;

    checkArity(op, args);
    
    e = CAR(args);                       args = CDR(args);
    if(!isString(e) || length(e) != 1) 
    	error(_("deparseRd only supports deparsing character elements"));
    e = STRING_ELT(e, 0);
    
    state = CAR(args);
    if(!isInteger(state) || length(state) != 5) error(_("bad state"));
    xxbraceDepth = INTEGER(state)[0];
    xxinRString = INTEGER(state)[1];
    xxmode = INTEGER(state)[2];
    xxinEqn = INTEGER(state)[3];
    quoteBraces = INTEGER(state)[4];
    
    
    if (xxmode != LATEXLIKE && xxmode != RLIKE && xxmode != VERBATIM && xxmode != COMMENTMODE 
     && xxmode != INOPTION  && xxmode != UNKNOWNMODE)
    	error(_("bad text mode %d in deparseRd"), xxmode);
    
    for (c = CHAR(e), outlen=0; *c; c++) {
    	outlen++;
    	/* any special char might be escaped */
    	if (*c == '{' || *c == '}' || *c == '%' || *c == '\\') outlen++;
    }
    out = outbuf = R_chk_calloc(outlen+1, sizeof(char));
    inRComment = FALSE;
    for (c = CHAR(e); *c; c++) {
    	escape = FALSE;
    	if (xxmode != UNKNOWNMODE) {
	    switch (*c) {
	    case '\\':
		if (xxmode == RLIKE && xxinRString) {
		    lookahead = *(c+1);
		    if (lookahead == '\\' || lookahead == xxinRString || lookahead == 'l') 
		    	escape = TRUE;
		    break;
		}          /* fall through to % case for non-strings... */    
	    case '%':
		if (xxmode != COMMENTMODE && !xxinEqn)
		    escape = TRUE;
		break;
	    case LBRACE:
	    case RBRACE:
		if (quoteBraces)
		    escape = TRUE;
		else if (!xxinRString && !xxinEqn && (xxmode == RLIKE || xxmode == VERBATIM)) {
		    if (*c == LBRACE) xxbraceDepth++;
		    else if (xxbraceDepth <= 0) escape = TRUE;
		    else xxbraceDepth--;
		}
		break;
	    case '\'':
	    case '"':
	    case '`':
	    	if (xxmode == RLIKE) {
		    if (xxinRString) {
			if (xxinRString == *c) xxinRString = 0;
		    } else if (!inRComment) xxinRString = *c;
		}
		break;
	    case '#':
	    	if (xxmode == RLIKE && !xxinRString) 
	    	    inRComment = TRUE;
	    	break;
	    case '\n':
	    	inRComment = FALSE;
	    	break;
	    }
	}
    	if (escape)
    	    *out++ = '\\';
    	*out++ = *c;
    }
    *out = '\0';
    PROTECT(result = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarString(mkChar(outbuf)));
    SET_VECTOR_ELT(result, 1, duplicate(state));
    R_chk_free(outbuf);

    statevals = INTEGER( VECTOR_ELT(result, 1) );
    statevals[0] = xxbraceDepth;
    statevals[1] = xxinRString;
    UNPROTECT(1);
    return result;
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_gramRd[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"parse_Rd", 	do_parseRd,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"deparseRd", 	do_deparseRd, 	0, 	11, 	2,	{PP_FUNCALL, PREC_FN, 	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
