%{
/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Development Core Team
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
#include "IOStuff.h"		/*-> Defn.h */
#include "Fileio.h"
#include "Parse.h"

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
static int yylex();
static int yyparse(void);

#define R_ParseVector R_NewParseVector
#define R_Parse1File R_NewParse1File
#define R_Parse1Buffer R_NewParse1Buffer
#define R_ParseFile R_NewParseFile
#define R_ParseConn R_NewParseConn
#define R_ParseBuffer R_NewParseBuffer
#define R_InitSrcRefState R_NewInitSrcRefState
#define R_FinalizeSrcRefState R_NewFinalizeSrcRefState

#undef isValidName
#define isValidName NewIsValidName

/* alloca.h inclusion is now covered by Defn.h */

#define yyconst const

typedef struct yyltype
{
  int first_line;
  int first_column;
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
  
  int first_parsed;
  int last_parsed;
} yyltype;

static yyltype prev_yylloc;

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
	  (Current).first_parsed = YYRHSLOC (Rhs, 1).first_parsed;      \
	  (Current).last_parsed  = YYRHSLOC (Rhs, N).last_parsed;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	  (Current).first_byte   = (Current).last_byte =		\
	    YYRHSLOC (Rhs, 0).last_byte;				\
	  (Current).first_parsed = (Current).last_parsed =		\
	    YYRHSLOC (Rhs, 0).last_parsed;				\
	}								\
    while (YYID (0))

/* Functions used in the parsing process */

static void	IfPush(void);
static int	KeywordLookup(const char *);
static int 	processLineDirective();

/* These routines allocate constants */

static SEXP	mkComplex(const char *);
static SEXP     mkFloat(const char *);
static SEXP	mkInt(const char *s);

SEXP		mkFalse(void);  /* not used here, but are used elsewhere */
SEXP		mkTrue(void);

/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	EndOfFile = 0;
static int	xxgetc();
static int	xxungetc(int);
static int	xxcharcount, xxcharsave;
static int	xxlinesave, xxbytesave, xxcolsave, xxparsesave;

static SrcRefState ParseState;
static PROTECT_INDEX srindex;

#include <R_ext/rlocale.h>
/* # include <sys/param.h> what was this for? */
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif


static int mbcs_get_next(int c, wchar_t *wc)
{
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
	*wc = (wchar_t) c;
	return 1;
    }
    if(utf8locale) {
	clen = utf8clen(c);
	for(i = 1; i < clen; i++) {
	    s[i] = xxgetc();
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), ParseState.xxlineno);
	}
	s[clen] ='\0'; /* x86 Solaris requires this */
	res = mbrtowc(wc, s, clen, NULL);
	if(res == -1) error(_("invalid multibyte character in parser at line %d"), ParseState.xxlineno);
    } else {
	/* This is not necessarily correct for stateful MBCS */
	while(clen <= MB_CUR_MAX) {
	    mbs_init(&mb_st);
	    res = mbrtowc(wc, s, clen, &mb_st);
	    if(res >= 0) break;
	    if(res == -1)
		error(_("invalid multibyte character in parser at line %d"), ParseState.xxlineno);
	    /* so res == -2 */
	    c = xxgetc();
	    if(c == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), ParseState.xxlineno);
	    s[clen++] = c;
	} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--) xxungetc(s[i]);
    return clen;
}

/* Soon to be defunct entry points */

void		R_SetInput(int);
int		R_fgetc(FILE*);


#define YYSTYPE		SEXP

%}

%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION 
%token		INCOMPLETE_STRING
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR AND2 OR2
%token		NS_GET NS_GET_INT

/* This is the precedence table, low to high */
%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR OR2
%left		AND AND2
%left		UNOT NOT
%nonassoc   	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$' '@'
%left		NS_GET NS_GET_INT
%nonassoc	'(' '[' LBB

%%

prog	:	END_OF_INPUT			{ return (&@1, 0); }
	|	error	 			{ YYABORT; }
	;

%%

/* GRAMMAR FROM THE OLD BISON VERSION OF THE PARSER, WITHOUT THE ACTIONS. */

#if 0

prog	:	END_OF_INPUT			
	|	'\n'				
	|	expr_or_assign '\n'		
	|	expr_or_assign ';'		
	|	error	 			
	;

expr_or_assign  :    expr                     
                |    equal_assign               
                ;

equal_assign    :    expr EQ_ASSIGN expr_or_assign  
                ;

expr	: 	NUM_CONST			
	|	STR_CONST			
	|	NULL_CONST			
	|	SYMBOL				

	|	'{' exprlist '}'		
	|	'(' expr_or_assign ')'		

	|	'-' expr %prec UMINUS		
	|	'+' expr %prec UMINUS		
	|	'!' expr %prec UNOT		
	|	'~' expr %prec TILDE		
	|	'?' expr			

	|	expr ':'  expr			
	|	expr '+'  expr			
	|	expr '-' expr			
	|	expr '*' expr			
	|	expr '/' expr			
	|	expr '^' expr 			
	|	expr SPECIAL expr		
	|	expr '%' expr			
	|	expr '~' expr			
	|	expr '?' expr			
	|	expr LT expr			
	|	expr LE expr			
	|	expr EQ expr			
	|	expr NE expr			
	|	expr GE expr			
	|	expr GT expr			
	|	expr AND expr			
	|	expr OR expr			
	|	expr AND2 expr			
	|	expr OR2 expr			

	|	expr LEFT_ASSIGN expr 		
	|	expr RIGHT_ASSIGN expr 		
	|	FUNCTION '(' formlist ')' cr expr_or_assign %prec LOW
	|	expr '(' sublist ')'		
	|	IF ifcond expr_or_assign 	
	|	IF ifcond expr_or_assign ELSE expr_or_assign	
	|	FOR forcond expr_or_assign %prec FOR 	
	|	WHILE cond expr_or_assign	
	|	REPEAT expr_or_assign		
	|	expr LBB sublist ']' ']'	
	|	expr '[' sublist ']'		
	|	SYMBOL NS_GET SYMBOL		
	|	SYMBOL NS_GET STR_CONST		
	|	STR_CONST NS_GET SYMBOL		
	|	STR_CONST NS_GET STR_CONST	
	|	SYMBOL NS_GET_INT SYMBOL	
	|	SYMBOL NS_GET_INT STR_CONST	
	|	STR_CONST NS_GET_INT SYMBOL	
	|	STR_CONST NS_GET_INT STR_CONST	
	|	expr '$' SYMBOL			
	|	expr '$' STR_CONST		
	|	expr '@' SYMBOL			
	|	expr '@' STR_CONST		
	|	NEXT				
	|	BREAK				
	;


cond	:	'(' expr ')'			
	;

ifcond	:	'(' expr ')'			
	;

forcond :	'(' SYMBOL IN expr ')' 		
	;


exprlist:					
	|	expr_or_assign			
	|	exprlist ';' expr_or_assign	
	|	exprlist ';'			
	|	exprlist '\n' expr_or_assign	
	|	exprlist '\n'			
	;

sublist	:	sub				
	|	sublist cr ',' sub		
	;

sub	:					
	|	expr				
	|	SYMBOL EQ_ASSIGN 		
	|	SYMBOL EQ_ASSIGN expr		
	|	STR_CONST EQ_ASSIGN 		
	|	STR_CONST EQ_ASSIGN expr	
	|	NULL_CONST EQ_ASSIGN 		
	|	NULL_CONST EQ_ASSIGN expr	
	;

formlist:					
	|	SYMBOL				
	|	SYMBOL EQ_ASSIGN expr		
	|	formlist ',' SYMBOL		
	|	formlist ',' SYMBOL EQ_ASSIGN expr 
	;

cr	:	
	;

#endif

static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth */

#define PUSHBACK_BUFSIZE 16
static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];
static int prevparse[PUSHBACK_BUFSIZE];

static int xxgetc(void)
{
    int c, oldpos;

    c = npush ? pushback[--npush] : ptr_getc();

    oldpos = prevpos;
    prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
    prevbytes[prevpos] = ParseState.xxbyteno;
    prevlines[prevpos] = ParseState.xxlineno;  
    prevparse[prevpos] = ParseState.xxparseno;

    /* We only advance the column for the 1st byte in UTF-8, so handle later 
       bytes specially */

    if (0x80 <= (unsigned char) c && (unsigned char) c <= 0xBF 
                                  && known_to_be_utf8) {
    	ParseState.xxcolno--;   
    	prevcols[prevpos] = prevcols[oldpos];
    } else 
    	prevcols[prevpos] = ParseState.xxcolno;
    	
    if (c == EOF) {
	EndOfFile = 1;
	return R_EOF;
    }
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = c;

    if (c == '\n') {
	ParseState.xxlineno += 1;
	ParseState.xxcolno = 0;
    	ParseState.xxbyteno = 0;
    	ParseState.xxparseno += 1;
    } else {
        ParseState.xxcolno++;
    	ParseState.xxbyteno++;
    }

    if (c == '\t') ParseState.xxcolno = ((ParseState.xxcolno + 7) & ~7);
    
    R_ParseContextLine = ParseState.xxlineno;    

    xxcharcount++;
    return c;
}

static int xxungetc(int c)
{
    /* This assumes that c was the result of xxgetc; if not, some edits will 
       be needed */

    ParseState.xxlineno = prevlines[prevpos];
    ParseState.xxbyteno = prevbytes[prevpos];
    ParseState.xxcolno  = prevcols[prevpos];
    ParseState.xxparseno = prevparse[prevpos];
    
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    R_ParseContextLine = ParseState.xxlineno;

    xxcharcount--;
    R_ParseContext[R_ParseContextLast] = '\0';
    /* precaution as to how % is implemented for < 0 numbers */
    R_ParseContextLast 
      = (R_ParseContextLast + PARSE_CONTEXT_SIZE -1) % PARSE_CONTEXT_SIZE;
    if(npush >= PUSHBACK_BUFSIZE) return EOF;
    pushback[npush++] = c;
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile)
{
    SEXP val;

    PROTECT(val = allocVector(INTSXP, 8));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
    INTEGER(val)[6] = lloc->first_parsed;
    INTEGER(val)[7] = lloc->last_parsed;
    setAttrib(val, R_SrcfileSymbol, srcfile);
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1);
    return val;
}

static void attachSrcrefs(SEXP val, SEXP t)
{
    SEXP srval;
    int n;

    PROTECT(val);
    PROTECT(srval = allocVector(VECSXP, length(t)));
    for (n = 0 ; n < LENGTH(srval) ; n++, t = CDR(t))
	SET_VECTOR_ELT(srval, n, CAR(t));
    setAttrib(val, R_SrcrefSymbol, srval);
    setAttrib(val, R_SrcfileSymbol, ParseState.SrcFile);
    {
	YYLTYPE wholeFile;
	wholeFile.first_line = 1;
	wholeFile.first_byte = 0;
	wholeFile.first_column = 0;
	wholeFile.last_line = ParseState.xxlineno;
	wholeFile.last_byte = ParseState.xxbyteno;
	wholeFile.last_column = ParseState.xxcolno;
	wholeFile.first_parsed = 1;
	wholeFile.last_parsed = ParseState.xxparseno;
	setAttrib(val, R_WholeSrcrefSymbol, 
                       makeSrcref(&wholeFile, ParseState.SrcFile));
    }
    UNPROTECT(2);
}


/*--------------------------------------------------------------------------*/

/*  Parsing Entry Points:
 *
 *  The Following entry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IoBuffers
 *  (i.e. interactve use), files and R character strings.
 *
 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *  The following routines parse a single expression:
 *
 *	SEXP R_Parse1File(FILE *fp, int gencode, ParseStatus *status, 
 *                        Rboolean first)
 *        (used for R_ReplFile in main.c)
 *
 *	SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status,
 *                          Rboolean first)
 *        (used for ReplIteration and R_ReplDLLdo1 in main.c)
 *
 *  The success of the parse is indicated as folllows:
 *
 *	status = PARSE_NULL       - there was no statement to parse
 *		 PARSE_OK	  - complete statement
 *		 PARSE_INCOMPLETE - incomplete statement
 *		 PARSE_ERROR      - syntax error
 *		 PARSE_EOF	  - end of file
 *
 *  The following routines parse several expressions and return
 *  their values in a single expression vector.
 *
 *	SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
 *        (used for do_edit in file edit.c)
 *
 *	SEXP R_ParseVector(SEXP *text, int n, ParseStatus *status, SEXP srcfile)
 *        (public, and used by parse(text=) in file source.c)
 *
 *	SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, 
 *                         SEXP prompt, SEXP srcfile)
 *        (used by parse(file="") in file source.c)
 *
 *      SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status, 
 *                       SEXP srcfile)
 *        (used by parse(file=) in file source.c)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing failed
 *  for some reason.
 */

#define CONTEXTSTACK_SIZE 50
static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[CONTEXTSTACK_SIZE], *contextp;

void R_InitSrcRefState(SrcRefState *state)
{
    state->keepSrcRefs = FALSE;
    state->didAttach = FALSE;
    PROTECT_WITH_INDEX(state->SrcFile = R_NilValue, &(state->SrcFileProt));
    PROTECT_WITH_INDEX(state->Original = R_NilValue, &(state->OriginalProt));
    state->xxlineno = 1;
    state->xxcolno = 0;
    state->xxbyteno = 0;
    state->xxparseno = 1;
}

void R_FinalizeSrcRefState(SrcRefState *state)
{
    UNPROTECT_PTR(state->SrcFile);
    UNPROTECT_PTR(state->Original);
}

static void UseSrcRefState(SrcRefState *state)
{
    if (state) {
	ParseState.keepSrcRefs = state->keepSrcRefs;
	ParseState.SrcFile = state->SrcFile;
	ParseState.Original = state->Original;
	ParseState.SrcFileProt = state->SrcFileProt;
	ParseState.xxlineno = state->xxlineno;
	ParseState.xxcolno = state->xxcolno;
	ParseState.xxbyteno = state->xxbyteno;
	ParseState.xxparseno = state->xxparseno;
    } else 
    	R_InitSrcRefState(&ParseState);
}

static void PutSrcRefState(SrcRefState *state)
{
    if (state) {
	state->keepSrcRefs = ParseState.keepSrcRefs;
	state->SrcFile = ParseState.SrcFile;
	state->Original = ParseState.Original;
	state->SrcFileProt = ParseState.SrcFileProt;
	state->xxlineno = ParseState.xxlineno;
	state->xxcolno = ParseState.xxcolno;
	state->xxbyteno = ParseState.xxbyteno;
	state->xxparseno = ParseState.xxparseno;
    } else 
    	R_FinalizeSrcRefState(&ParseState);
}

static void ParseInit(void)
{
    contextp = contextstack;
    *contextp = ' ';
    SavedToken = 0;
    SavedLval = R_NilValue;
    EatLines = 0;
    EndOfFile = 0;
    xxcharcount = 0;
    npush = 0;
}

static void ParseContextInit(void)
{
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
}

/* -------------------------------------------------------------------------- */
/* PARSING VARIABLES, MACROS, AND FUNCTIONS                                   */

#define BGN_PARSE_FUN \
    int nprotect = 0

#define PARSE_SUB(w) \
    do { \
        SEXP _sub_ = (w); \
        if (_sub_ == NULL) goto error; \
        PROTECT_N(_sub_); \
    } while (0)

#define PARSE_ERROR_MSG(s) \
    do { \
        yyerror(s); \
        goto error; \
    } while (0)

#define PARSE_UNEXPECTED() \
    do { \
        char s[100] = "syntax error, unexpected"; \
        if (next_token < 256) { \
            char t[2] = { next_token, 0 }; \
            copy_3_strings(s, sizeof s, "syntax error, unexpected '", t, "'"); \
        } \
        else { \
            copy_2_strings (s, sizeof s, "syntax error, unexpected ", \
                            yytname[next_token-255]); \
        } \
        PARSE_ERROR_MSG(s); \
    } while (0)

#define NEXT_TOKEN() (next_token = yylex())

#define EAT_LINES() while (next_token == '\n') NEXT_TOKEN()

#define EXPECT(tk) \
    do { \
        if (next_token != (tk)) \
            PARSE_UNEXPECTED(); \
        NEXT_TOKEN(); \
    } while (0)

#define PROTECT_N(w) (nprotect++, PROTECT(w))

#define TOKEN_VALUE() (PROTECT_N(yylval))

#define END_PARSE_FUN \
    UNPROTECT(nprotect); \
    goto end; \
  error: \
    UNPROTECT(nprotect); \
    return NULL; \
  end:

static void start_location (yyltype *loc)
{
    loc->first_line   = loc->last_line   = yylloc.first_line;
    loc->first_column = loc->last_column = yylloc.first_column;
    loc->first_byte   = loc->last_byte   = yylloc.first_byte;
    loc->first_parsed = loc->last_parsed = yylloc.first_parsed;
}

static void end_location (yyltype *loc)
{
    loc->last_line   = prev_yylloc.last_line;
    loc->last_column = prev_yylloc.last_column;
    loc->last_byte   = prev_yylloc.last_byte;
    loc->last_parsed = prev_yylloc.last_parsed;
}

static int next_token;

/* -------------------------------------------------------------------------- */
/* THE RECURSIVE DESCENT PARSER                                               */

static SEXP parse_expr(void), parse_expr_or_assign(void);

/* Paarse the formals list of a function definiton. */

static SEXP parse_formlist(void)
{
    BGN_PARSE_FUN;
    SEXP res;

    if (next_token == ')')
        res = R_NilValue;
    else {
        SEXP last;
        res = PROTECT_N (CONS(R_MissingArg,R_NilValue));
        last = res;
        for (;;) {
            SEXP tag, f;
            if (next_token != SYMBOL)
                PARSE_UNEXPECTED();
            tag = TOKEN_VALUE();
            for (f = res; f != R_NilValue; f = CDR(f)) {
                if (TAG(f) == tag) {
                    YYLTYPE loc;
                    start_location(&loc);
                    error(_("Repeated formal argument '%s' on line %d"), 
                            CHAR(PRINTNAME(tag)), loc.first_line);
                }
            }
            SET_TAG (last, tag);
            NEXT_TOKEN();
            if (next_token == EQ_ASSIGN) {
                SEXP def;
                NEXT_TOKEN();
                PARSE_SUB(def = parse_expr());
                SETCAR (last, def);
            }
            if (next_token != ',')
                break;
            NEXT_TOKEN();
            SETCDR (last, CONS(R_MissingArg,R_NilValue));
            last = CDR(last);
        }
    }

    END_PARSE_FUN;
    return res;
}

/* Parse a list of subscripts or of function arguments.  An attempt is 
   made to make the last part of the list be a constant object.  Note
   that NULL is allowed as a tag (converted to `NULL`), presumably for 
   compatibility, though it's not allowed for a formal name.  Strings
   are also allowed for tags, though again they aren't for formal names. */

static SEXP parse_sublist(void)
{
    BGN_PARSE_FUN;
    SEXP res, last, last2;

    res = R_NilValue;
    if (next_token != ')') {
        SEXP next;
        for (;;) {
            SEXP arg;
            if (next_token == ',' || next_token == ')' || next_token == ']')
                next = MaybeConstList1(R_MissingArg);
            else {
                PARSE_SUB(arg = parse_expr());
                if (next_token == EQ_ASSIGN) {
                    SEXP tag, val;
                    if (TYPEOF(arg) == SYMSXP)
                        tag = arg;
                    else if (TYPEOF(arg) == STRSXP)
                        tag = install (translateChar (STRING_ELT(arg,0)));
                    else if (arg == R_NilValue)
                        tag = install("NULL");
                    else
                        PARSE_UNEXPECTED();
                    NEXT_TOKEN();
                    if (next_token == ',' || next_token == ')' 
                                          || next_token == ']')
                        val = R_MissingArg;
                    else
                        PARSE_SUB(val = parse_expr());
                    next = cons_with_tag(val,R_NilValue,tag);
                }
                else {
                    next = MaybeConstList1(arg);
                }
            }
            if (res == R_NilValue) {
                PROTECT_N (res = next);
                last = res;
                last2 = R_NilValue;
            }
            else {
                if (IS_CONSTANT(last)) {
                    last = cons_with_tag(CAR(last),CDR(last),TAG(last));
                    if (last2 == R_NilValue)
                        PROTECT_N (res = last);
                    else
                        SETCDR(last2,last);
                }
                SETCDR(last,next);
                last2 = last;
                last = next;
            }
            if (next_token != ',')
                break;
            NEXT_TOKEN();
        }
    }

    END_PARSE_FUN;
    return res;
}

/* Parse an element that acts as a unit for the operator precedence.
   Such an element consists of an initial part followed by possible postfix
   parts.  The initial part may be a constant, symbol, paren expression,
   or curly expression, which may be followed by postfix parts, or a function 
   closure, while, repeat, for, or if, which would absorb any postfix parts
   themselves, or break, or next, which can be followed by postfix parts
   although they make no sense.  Postfix parts may be argument lists for
   function calls, or subsetting operators [, [[, $, or @.

   Curly expressions and function closures may have source references. */

static SEXP parse_element(void)
{
    BGN_PARSE_FUN;
    YYLTYPE loc;
    SEXP res;

    /* Symbols, string constants, and namespace references built from
       one or the other or both of these. */

    if (next_token == SYMBOL || next_token == STR_CONST) {
        SEXP op, sym;
        res = TOKEN_VALUE();
        NEXT_TOKEN();
        if (next_token == NS_GET || next_token == NS_GET_INT) {
            op = TOKEN_VALUE();
            NEXT_TOKEN();
            if (next_token != SYMBOL && next_token != STR_CONST)
                PARSE_UNEXPECTED();
            sym = TOKEN_VALUE();
            res = PROTECT_N (LCONS (op, CONS (res, MaybeConstList1(sym))));
            NEXT_TOKEN();
        }
    }

    /* Numeric, logical, and NULL constants. */

    else if (next_token == NUM_CONST || next_token == NULL_CONST) {
        res = TOKEN_VALUE();
        NEXT_TOKEN();
    }

    /* Paren expressions. */

    else if (next_token == '(') {
        SEXP op, inside;
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (inside = parse_expr_or_assign());
        res = PROTECT_N (LCONS (op, MaybeConstList1(inside)));
        EXPECT(')');
    }

    /* Curly expressions. */

    else if (next_token == '{') {
        SEXP next, last, op, refs, last_ref;
        op = TOKEN_VALUE();
        res = PROTECT_N (LCONS(op,R_NilValue));
        start_location(&loc);
        NEXT_TOKEN();
        end_location(&loc);
        if (ParseState.keepSrcRefs) {
            PROTECT_N (refs = CONS (makeSrcref(&loc,ParseState.SrcFile),
                                    R_NilValue));
            last_ref = refs;
        }
        last = res;
        for (;;) {
            while (next_token == ';' || next_token == '\n')
                NEXT_TOKEN();
            if (next_token == '}')
                break;
            start_location(&loc);
            PARSE_SUB (next = parse_expr_or_assign());
            end_location(&loc);
            if (ParseState.keepSrcRefs) {
                SETCDR (last_ref, CONS (makeSrcref(&loc,ParseState.SrcFile),
                                        R_NilValue));
                last_ref = CDR(last_ref);
            }
            SETCDR (last, CONS(next,R_NilValue));
            last = CDR(last);
        }
        if (ParseState.keepSrcRefs) {
            attachSrcrefs(res,refs);
            ParseState.didAttach = TRUE;
        }
        NEXT_TOKEN();
    }

    /* Function closures. */

    else if (next_token == FUNCTION) {
        SEXP op, args, body, srcref;
        start_location(&loc);
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        EXPECT('(');
        PARSE_SUB(args = parse_formlist());
        EXPECT(')');
        EAT_LINES();
        PARSE_SUB(body = parse_expr_or_assign());
        end_location(&loc);
        if (ParseState.keepSrcRefs) {
            srcref = makeSrcref(&loc, ParseState.SrcFile);
            ParseState.didAttach = TRUE;
        } 
        else
            srcref = R_NilValue;
        res = PROTECT_N (lang4 (op, args, body, srcref));
    }

    /* Repeat statements. */

    else if (next_token == REPEAT) {
        SEXP op, body;
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB(body = parse_expr_or_assign());
        res = PROTECT_N (lang2 (op, body));
    }

    /* While statements. */

    else if (next_token == WHILE) {
        SEXP op, cond, body;
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        EXPECT('(');
        PARSE_SUB(cond = parse_expr());
        EXPECT(')');
        EAT_LINES();
        PARSE_SUB(body = parse_expr_or_assign());
        res = PROTECT_N (lang3 (op, cond, body));
    }

    /* If statements. */

    else if (next_token == IF) {
        SEXP op, cond, true_stmt, false_stmt;
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        EXPECT('(');
        PARSE_SUB(cond = parse_expr());
        EXPECT(')');
        EAT_LINES();
        PARSE_SUB(true_stmt = parse_expr_or_assign());
        if (next_token == ELSE) {
            NEXT_TOKEN();
            PARSE_SUB(false_stmt = parse_expr_or_assign());
            res = PROTECT_N (LCONS (op, CONS (cond, CONS (true_stmt,
                              MaybeConstList1(false_stmt)))));
        }
        else
            res = PROTECT_N (LCONS (op, CONS (cond,
                              MaybeConstList1(true_stmt))));
    }

    /* For statements. */

    else if (next_token == FOR) {
        SEXP op, sym, vec, body;
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        EXPECT('(');
        if (next_token != SYMBOL)
            PARSE_UNEXPECTED();
        sym = TOKEN_VALUE();
        NEXT_TOKEN();
        EXPECT(IN);
        PARSE_SUB(vec = parse_expr());
        EXPECT(')');
        EAT_LINES();
        PARSE_SUB(body = parse_expr_or_assign());
        res = PROTECT_N (lang4 (op, sym, vec, body));
    }

    /* Next and break statements. */

    else if (next_token == NEXT || next_token == BREAK) {
        SEXP op;
        op = TOKEN_VALUE();
        res = PROTECT_N (LCONS (op, R_NilValue));
        NEXT_TOKEN();
    }

    else
        PARSE_UNEXPECTED();

    /* Now parse any postfix parts. */

    for (;;) {

        /* Function call. */

        if (next_token == '(') {
            SEXP subs;
            NEXT_TOKEN();
            PARSE_SUB(subs = parse_sublist());
            if (isString(res))
                res = installChar(STRING_ELT(res,0));
            res = LCONS(res,subs);
            EXPECT(')');
        }

        /* Subscripting with [. */
        else if (next_token == '[') {
            SEXP op, subs;
            op = TOKEN_VALUE();
            NEXT_TOKEN();
            PARSE_SUB(subs = parse_sublist());
            res = LCONS (op, CONS (res, subs));
            EXPECT(']');
        }

        /* Subscripting with [[.  Note that it may be terminated with ] ]. */

        else if (next_token == LBB) {
            SEXP op, subs;
            op = TOKEN_VALUE();
            NEXT_TOKEN();
            PARSE_SUB(subs = parse_sublist());
            res = LCONS (op, CONS (res, subs));
            EXPECT(']');
            EXPECT(']');
        }

        /* Subsetting with $ or @. */

        else if (next_token == '$' || next_token == '@') {
            SEXP op, sym;
            op = TOKEN_VALUE();
            NEXT_TOKEN();
            if (next_token != SYMBOL && next_token != STR_CONST)
                PARSE_UNEXPECTED();
            sym = TOKEN_VALUE();
            res = LCONS (op, CONS (res, MaybeConstList1(sym)));
            NEXT_TOKEN();
        }

        else
            break;
    }

    END_PARSE_FUN;
    return res;
}

/* Binary and unary operators, arranged in recursive hierarchy by precedence.
   Highest precedence is first below. */

static SEXP parse_power(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_element());

    if (next_token == '^') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_power());
        res = LCONS (op, CONS (res, MaybeConstList1(right)));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_unary_plus_minus(void)
{
    BGN_PARSE_FUN;
    SEXP res, op;

    if (next_token == '+' || next_token == '-') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (res = parse_unary_plus_minus());
        res = LCONS (op, MaybeConstList1(res));
    }
    else
        PARSE_SUB (res = parse_power());

    END_PARSE_FUN;
    return res;
}

static SEXP parse_colon(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_unary_plus_minus());

    while (next_token == ':') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_unary_plus_minus());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_special(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_colon());

    while (next_token == SPECIAL) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_colon());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_mul_div(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_special());

    while (next_token == '*' || next_token == '/') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_special());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_plus_minus(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_mul_div());

    while (next_token == '+' || next_token == '-') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_mul_div());
        PROTECT_N (res = res ? LCONS (op, CONS (res, MaybeConstList1(right)))
                             : LCONS (op, MaybeConstList1(right)));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_relation(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_plus_minus());

    if (next_token == GT || next_token == GE || next_token == EQ
     || next_token == LE || next_token == LT || next_token == NE) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_plus_minus());
        res = LCONS (op, CONS (res, MaybeConstList1(right)));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_not(void)
{
    BGN_PARSE_FUN;
    SEXP res, op;

    if (next_token == '!') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (res = parse_not());
        res = LCONS (op, MaybeConstList1(res));
    }
    else
        PARSE_SUB (res = parse_relation());

    END_PARSE_FUN;
    return res;
}

static SEXP parse_and(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_not());

    while (next_token == AND || next_token == AND2) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_not());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_or(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_and());

    while (next_token == OR || next_token == OR2) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_and());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_unary_tilde(void)
{
    BGN_PARSE_FUN;
    SEXP res, op;

    if (next_token == '~') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (res = parse_unary_tilde());
        res = LCONS (op, MaybeConstList1(res));
    }
    else
        PARSE_SUB (res = parse_or());

    END_PARSE_FUN;
    return res;
}

static SEXP parse_tilde(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_unary_tilde());

    while (next_token == '~') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_unary_tilde());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_right_assign(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_tilde());

    while (next_token == RIGHT_ASSIGN) {
        op = TOKEN_VALUE();  /* already switched to left assignment */
        NEXT_TOKEN();
        PARSE_SUB (right = parse_tilde());
        PROTECT_N (res = LCONS (op, CONS (right, MaybeConstList1(res))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_left_assign(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_right_assign());

    if (next_token == LEFT_ASSIGN) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_left_assign());
        res = LCONS (op, CONS (res, MaybeConstList1(right)));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_unary_query(void)
{
    BGN_PARSE_FUN;
    SEXP res, op;

    if (next_token == '?') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (res = parse_unary_query());
        res = LCONS (op, MaybeConstList1(res));
    }
    else
        PARSE_SUB (res = parse_left_assign());

    END_PARSE_FUN;
    return res;
}

static SEXP parse_expr(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_unary_query());

    while (next_token == '?') {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_unary_query());
        PROTECT_N (res = LCONS (op, CONS (res, MaybeConstList1(right))));
    }

    END_PARSE_FUN;
    return res;
}

static SEXP parse_expr_or_assign(void)
{
    BGN_PARSE_FUN;
    SEXP res, right, op;

    PARSE_SUB (res = parse_expr());

    if (next_token == EQ_ASSIGN) {
        op = TOKEN_VALUE();
        NEXT_TOKEN();
        PARSE_SUB (right = parse_expr_or_assign());
        res = LCONS (op, CONS (res, MaybeConstList1(right)));
    }

    END_PARSE_FUN;
    return res;
}

/* Top level parse function, parsing and expression or assignments with =,
   that is followed by newline or ';'. */

static SEXP parse_prog(void)
{
    BGN_PARSE_FUN;
    SEXP res;

    PARSE_SUB (res = parse_expr_or_assign());

    if (next_token != '\n' && next_token != ';') {
        PARSE_UNEXPECTED();
    }

    END_PARSE_FUN;
    return res;
}

/* -------------------------------------------------------------------------- */

/* R_Parse1 currently sets R_CurrentExpr, but probably shouldn't. */

static SEXP R_Parse1(ParseStatus *status, YYLTYPE *loc)
{
    SEXP res;

    next_token = yylex();

    if (next_token == END_OF_INPUT) {
        *status = EndOfFile==2 ? PARSE_INCOMPLETE : PARSE_EOF;
        return R_CurrentExpr = R_NilValue;
    }

    if (next_token == '\n') {
        *status = PARSE_NULL;
        return R_CurrentExpr = R_NilValue;
    }

    start_location(loc);
    res = parse_prog();
    end_location(loc);

    if (res == NULL) {
        *status = EndOfFile ? PARSE_INCOMPLETE : PARSE_ERROR;
        return R_CurrentExpr = R_NilValue;
    }

    *status = PARSE_OK;
    return R_CurrentExpr = res;
}

/* -------------------------------------------------------------------------- */

static FILE *fp_parse;

static int file_getc(void)
{
    return R_fgetc(fp_parse);
}

/* used in main.c */
attribute_hidden
SEXP R_Parse1File(FILE *fp, int gencode, ParseStatus *status, SrcRefState *state)
{
    YYLTYPE loc;
    SEXP res;
    UseSrcRefState(state);
    ParseInit();
    ParseContextInit();
    fp_parse = fp;
    ptr_getc = file_getc;
    res = R_Parse1(status,&loc);
    PutSrcRefState(state);
    return gencode ? res : R_NilValue;
}

static IoBuffer *iob;

static int buffer_getc(void)
{
    return R_IoBufferGetc(iob);
}

/* Used only in main.c */
attribute_hidden
SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status)
{
    Rboolean keepSource = FALSE; 
    YYLTYPE loc;
    SEXP res;

    R_InitSrcRefState(&ParseState);
    if (gencode) {
    	keepSource = asLogical(GetOption1(install("keep.source")));
    	if (keepSource) {
    	    ParseState.keepSrcRefs = TRUE;
    	    REPROTECT(ParseState.SrcFile = NewEnvironment(R_NilValue, R_NilValue, R_EmptyEnv), ParseState.SrcFileProt);
	    REPROTECT(ParseState.Original = ParseState.SrcFile, ParseState.OriginalProt);
	}
    }

    ParseInit();
    ParseContextInit();
    iob = buffer;
    ptr_getc = buffer_getc;

    res = R_Parse1(status,&loc);
    if (!gencode) res = R_NilValue;

    if (gencode && keepSource) {
    	if (ParseState.didAttach) {
            SEXP filename_install = install("filename");  /* protected by the */
            SEXP lines_install = install("lines");        /*   symbol table   */
   	    int buflen = R_IoBufferReadOffset(buffer);
   	    char buf[buflen+1];
   	    SEXP class;
   	    R_IoBufferReadReset(buffer);
   	    for (int i=0; i<buflen; i++)
   	    	buf[i] = R_IoBufferGetc(buffer);

   	    buf[buflen] = 0;
    	    defineVar (filename_install, ScalarString(mkChar("")), 
                       ParseState.Original);
    	    defineVar (lines_install, ScalarString(mkChar(buf)),
                       ParseState.Original);
    	    PROTECT(class = allocVector(STRSXP, 2));
            SET_STRING_ELT(class, 0, mkChar("srcfilecopy"));
            SET_STRING_ELT(class, 1, mkChar("srcfile"));
	    setAttrib(ParseState.Original, R_ClassSymbol, class);
	    UNPROTECT(1);
	}
    }

    R_FinalizeSrcRefState(&ParseState);
    return res;
}

static TextBuffer *txtb;

static int text_getc(void)
{
    int c = R_TextBufferGetc(txtb);
    return c;
}

static SEXP R_Parse(int n, ParseStatus *status, SEXP srcfile)
{
    volatile int savestack;
    SEXP rval, tval, tlast, cur, refs, last_ref;
    YYLTYPE loc;
    int i;

    R_InitSrcRefState(&ParseState);
    
    ParseContextInit();
    savestack = R_PPStackTop;

    REPROTECT(ParseState.SrcFile = srcfile, ParseState.SrcFileProt);
    REPROTECT(ParseState.Original = srcfile, ParseState.OriginalProt);

    PROTECT(tval = CONS(R_NilValue,R_NilValue));
    tlast = tval;

    if (!isNull(ParseState.SrcFile)) {
    	ParseState.keepSrcRefs = TRUE;
        PROTECT(refs = CONS(R_NilValue,R_NilValue));
        last_ref = refs;
    }
    
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;

	ParseInit();
	cur = R_Parse1(status,&loc);
        if (ParseState.keepSrcRefs) {
            SETCDR (last_ref, 
                    CONS (makeSrcref(&loc, ParseState.SrcFile), R_NilValue));
            last_ref = CDR(last_ref);
        }

	switch(*status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
            SETCDR (tlast, CONS (cur, R_NilValue));
            tlast = CDR(tlast);
	    i++;
	    break;
	case PARSE_INCOMPLETE:
	case PARSE_ERROR:
	    R_PPStackTop = savestack;
	    R_FinalizeSrcRefState(&ParseState);
	    return R_NilValue;
	case PARSE_EOF:
	    goto finish;
	}
    }

finish:
    tval = CDR(tval);
    PROTECT(rval = allocVector(EXPRSXP, length(tval)));
    for (i = 0 ; i < LENGTH(rval) ; i++, tval = CDR(tval))
	SET_VECTOR_ELT(rval, i, CAR(tval));
    if (ParseState.keepSrcRefs) {
	attachSrcrefs(rval,CDR(refs));
        ParseState.didAttach = TRUE;
    }
    R_PPStackTop = savestack;    
    R_FinalizeSrcRefState(&ParseState);

    *status = PARSE_OK;
    return rval;
}

/* used in edit.c */
attribute_hidden
SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
{
    fp_parse = fp;
    ptr_getc = file_getc;
    return R_Parse(n, status, srcfile);
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

/* used in source.c */
attribute_hidden
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status, SEXP srcfile)
{
    con_parse = con;
    ptr_getc = con_getc;
    return R_Parse(n, status, srcfile);
}

/* This one is public, and used in source.c */
SEXP R_ParseVector(SEXP text, int n, ParseStatus *status, SEXP srcfile)
{
    SEXP rval;
    TextBuffer textb;
    R_TextBufferInit(&textb, text);
    txtb = &textb;
    ptr_getc = text_getc;
    rval = R_Parse(n, status, srcfile);
    R_TextBufferFree(&textb);
    return rval;
}

static const char *Prompt(SEXP prompt, int type)
{
    if(type == 1) {
	if(length(prompt) <= 0) {
	    return CHAR(STRING_ELT(GetOption1(install("prompt")), 0));
	}
	else
	    return CHAR(STRING_ELT(prompt, 0));
    }
    else {
	return CHAR(STRING_ELT(GetOption1(install("continue")), 0));
    }
}

/* used in source.c */
attribute_hidden
SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt, 
		   SEXP srcfile)
{
    SEXP rval, tval, tlast, cur, refs, last_ref;
    char *bufp, buf[CONSOLE_BUFFER_SIZE];
    int c, i, prompt_type = 1;
    volatile int savestack;
    YYLTYPE loc;

    R_IoBufferWriteReset(buffer);
    buf[0] = '\0';
    bufp = buf;
    R_InitSrcRefState(&ParseState);    
    savestack = R_PPStackTop;
    
    iob = buffer;
    ptr_getc = buffer_getc;

    REPROTECT(ParseState.SrcFile = srcfile, ParseState.SrcFileProt);
    REPROTECT(ParseState.Original = srcfile, ParseState.OriginalProt);

    PROTECT(tval = CONS(R_NilValue,R_NilValue));
    tlast = tval;
    
    if (!isNull(ParseState.SrcFile)) {
    	ParseState.keepSrcRefs = TRUE;
        PROTECT(refs = CONS(R_NilValue,R_NilValue));
        last_ref = refs;
    }
    
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;
	if (!*bufp) {
	    if(R_ReadConsole((char *) Prompt(prompt, prompt_type),
			     (unsigned char *)buf, CONSOLE_BUFFER_SIZE, 1) == 0)
		goto finish;
	    bufp = buf;
	}
	while ((c = *bufp++)) {
	    R_IoBufferPutc(c, buffer);
	    if (c == ';' || c == '\n') break;
	}

	/* Was a call to R_Parse1Buffer, but we don't want to reset
	   xxlineno and xxcolno */
	ParseInit();
	ParseContextInit();
	cur = R_Parse1(status,&loc);
        if (ParseState.keepSrcRefs) {
            SETCDR (last_ref, 
                    CONS (makeSrcref(&loc, ParseState.SrcFile), R_NilValue));
            last_ref = CDR(last_ref);
        }

	switch(*status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
            SETCDR (tlast, CONS(cur,R_NilValue));
            tlast = CDR(tlast);
	    i++;
	    break;
	case PARSE_INCOMPLETE:
	case PARSE_ERROR:
	    R_IoBufferWriteReset(buffer);
	    R_PPStackTop = savestack;
	    R_FinalizeSrcRefState(&ParseState);
	    return R_NilValue;
	    break;
	case PARSE_EOF:
	    goto finish;
	}
    }
finish:
    R_IoBufferWriteReset(buffer);
    tval = CDR(tval);
    rval = allocVector(EXPRSXP, length(tval));
    for (i = 0 ; i < LENGTH(rval) ; i++, tval = CDR(tval))
	SET_VECTOR_ELT(rval, n, CAR(tval));
    if (ParseState.keepSrcRefs) {
	attachSrcrefs(rval,CDR(refs));
        ParseState.didAttach = TRUE;
    }
    R_PPStackTop = savestack;
    R_FinalizeSrcRefState(&ParseState);    
    *status = PARSE_OK;
    return rval;
}


/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  Input is read a line at a time, and, if the
 *  program is in batch mode, each input line is echoed to
 *  standard output after it is read.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  The lexical
 *  analyser maintains a symbol table (in a very messy fashion).
 *
 *  The fact that if statements need to parse differently
 *  depending on whether the statement is being interpreted or
 *  part of the body of a function causes the need for ifpop
 *  and IfPush.  When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 *
 */

static void IfPush(void)
{
    if (*contextp=='{' ||
	*contextp=='['    ||
	*contextp=='('    ||
	*contextp == 'i') {
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow"));
	*++contextp = 'i';
    }

}

static void ifpop(void)
{
    if (*contextp=='i')
	*contextp-- = 0;
}

/* This is only called following ., so we only care if it is
   an ANSI digit or not */
static int typeofnext(void)
{
    int k, c;

    c = xxgetc();
    if (isdigit(c)) k = 1; else k = 2;
    xxungetc(c);
    return k;
}

static int nextchar(int expect)
{
    int c = xxgetc();
    if (c == expect)
	return 1;
    else
	xxungetc(c);
    return 0;
}

/* Special Symbols */
/* Syntactic Keywords + Symbolic Constants */

struct {
    char *name;
    int token;
}
static keywords[] = {
    { "NULL",	    NULL_CONST },
    { "NA",	    NUM_CONST  },
    { "TRUE",	    NUM_CONST  },
    { "FALSE",	    NUM_CONST  },
    { "Inf",	    NUM_CONST  },
    { "NaN",	    NUM_CONST  },
    { "NA_integer_", NUM_CONST  },
    { "NA_real_",    NUM_CONST  },
    { "NA_character_", NUM_CONST  },
    { "NA_complex_", NUM_CONST  },
    { "function",   FUNCTION   },
    { "while",	    WHILE      },
    { "repeat",	    REPEAT     },
    { "for",	    FOR	       },
    { "if",	    IF	       },
    { "in",	    IN	       },
    { "else",	    ELSE       },
    { "next",	    NEXT       },
    { "break",	    BREAK      },
    { "...",	    SYMBOL     },
    { 0,	    0	       }
};

/* KeywordLookup has side effects, it sets yylval */

static int KeywordLookup(const char *s)
{
    int i;
    for (i = 0; keywords[i].name; i++) {
	if (strcmp(keywords[i].name, s) == 0) {
	    switch (keywords[i].token) {
	    case NULL_CONST:
		yylval = R_NilValue;
		break;
	    case NUM_CONST:
                switch(i) {
                case 1:
                    yylval = ScalarLogicalMaybeConst(NA_LOGICAL);
                    break;
                case 2:
                    yylval = ScalarLogicalMaybeConst(1);
                    break;
                case 3:
                    yylval = ScalarLogicalMaybeConst(0);
                    break;
                case 4:
                    yylval = allocVector1REAL();
                    REAL(yylval)[0] = R_PosInf;
                    break;
                case 5:
                    yylval = allocVector1REAL();
                    REAL(yylval)[0] = R_NaN;
                    break;
                case 6:
                    yylval = ScalarIntegerMaybeConst(NA_INTEGER);
                    break;
                case 7:
                    yylval = allocVector1REAL();
                    REAL(yylval)[0] = NA_REAL;
                    break;
                case 8:
                    yylval = allocVector(STRSXP, 1);
                    SET_STRING_ELT(yylval, 0, NA_STRING);
                    break;
                case 9:
                    yylval = allocVector(CPLXSXP, 1);
                    COMPLEX(yylval)[0].r = COMPLEX(yylval)[0].i = NA_REAL;
                    break;
                }
		break;
	    case FUNCTION:
	    case WHILE:
	    case REPEAT:
	    case FOR:
	    case IF:
	    case NEXT:
	    case BREAK:
		yylval = install(s);
		break;
	    case IN:
	    case ELSE:
		break;
	    case SYMBOL:
		yylval = install(s);
		break;
	    }
	    return keywords[i].token;
	}
}
    return 0;
}

static SEXP mkFloat(const char *s)
{
    return ScalarRealMaybeConst(R_atof(s));
}

static SEXP mkInt(const char *s)
{
    double f = R_atof(s);  /* or R_strtol? */
    return ScalarIntegerMaybeConst((int) f);
}

static SEXP mkComplex(const char *s)
{
    SEXP t = R_NilValue;
    double f;
    f = R_atof(s); /* FIXME: make certain the value is legitimate. */

    t = allocVector(CPLXSXP, 1);
    COMPLEX(t)[0].r = 0;
    COMPLEX(t)[0].i = f;

    return t;
}

#if 0 

SEXP mkTrue(void)
{
    SEXP s = allocVector1LGL();
    LOGICAL(s)[0] = 1;
    return s;
}

SEXP mkFalse(void)
{
    SEXP s = allocVector1LGL();
    LOGICAL(s)[0] = 0;
    return s;
}

#endif

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
#define YYENGLISH 8
	"$undefined",	"input",
	"END_OF_INPUT",	"end of input",
	"ERROR",	"input",
	"STR_CONST",	"string constant",
	"NUM_CONST",	"numeric constant",
	"SYMBOL",	"symbol",
	"LEFT_ASSIGN",	"assignment",
	"'\\n'",	"end of line",
	"NULL_CONST",	"'NULL'",
	"FUNCTION",	"'function'",
	"EQ_ASSIGN",	"'='",
	"RIGHT_ASSIGN",	"'->'",
	"LBB",		"'[['",
	"FOR",		"'for'",
	"IN",		"'in'",
	"IF",		"'if'",
	"ELSE",		"'else'",
	"WHILE",	"'while'",
	"NEXT",		"'next'",
	"BREAK",	"'break'",
	"REPEAT",	"'repeat'",
	"GT",		"'>'",
	"GE",		"'>='",
	"LT",		"'<'",
	"LE",		"'<='",
	"EQ",		"'=='",
	"NE",		"'!='",
	"AND",		"'&'",
	"OR",		"'|'",
	"AND2",		"'&&'",
	"OR2",		"'||'",
	"NS_GET",	"'::'",
	"NS_GET_INT",	"':::'",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
 #if 0
 /* these are just here to trigger the internationalization */
    _("input");
    _("end of input");
    _("string constant");
    _("numeric constant");
    _("symbol");
    _("assignment");
    _("end of line");
#endif

    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = ParseState.SrcFile;

    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i;
	/* Edit the error message */
	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
	if (expecting) *expecting = '\0';
	for (i = 0; yytname_translations[i]; i += 2) {
	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
		snprintf(R_ParseErrorMsg,  PARSE_ERROR_SIZE, _("unexpected %s"),
		    i/2 < YYENGLISH ? _(yytname_translations[i+1])
				    : yytname_translations[i+1]);
		return;
	    }
	}
	snprintf(R_ParseErrorMsg, PARSE_ERROR_SIZE, _("unexpected %s"), 
                s + sizeof yyunexpected - 1);
    } else {
	strncpy(R_ParseErrorMsg, s, PARSE_ERROR_SIZE - 1);
	R_ParseErrorMsg[PARSE_ERROR_SIZE - 1] = '\0';
    }
}


/* This is used as the buffer for NumericValue, SpecialValue and
   SymbolValue.  None of these could conceivably need 8192 bytes.

   It has not been used as the buffer for input character strings
   since Oct 2007 (released as 2.7.0), and for comments since 2.8.0
 */
static char yytext[MAXELTSIZE];

#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext >= sizeof(yytext) - 1) \
	error(_("input buffer overflow at line %d"), ParseState.xxlineno); \
	*(bp)++ = (c); \
} while(0)

static int SkipSpace(void)
{
    int c;

#ifdef Win32
    if(!mbcslocale) { /* 0xa0 is NBSP in all 8-bit Windows locales */
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f' ||
	       (unsigned int) c == 0xa0) ;
	return c;
    } else {
	int i, clen;
	wchar_t wc;
	while (1) {
	    c = xxgetc();
	    if (c == ' ' || c == '\t' || c == '\f') continue;
	    if (c == '\n' || c == R_EOF) break;
	    if ((unsigned int) c < 0x80) break;
	    clen = mbcs_get_next(c, &wc);  /* always 2 */
	    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
	    for(i = 1; i < clen; i++) c = xxgetc();
	}
	return c;
    }
#endif
#if defined(__STDC_ISO_10646__)
    if(mbcslocale) { /* wctype functions need Unicode wchar_t */
	int i, clen;
	wchar_t wc;
	while (1) {
	    c = xxgetc();
	    if (c == ' ' || c == '\t' || c == '\f') continue;
	    if (c == '\n' || c == R_EOF) break;
	    if ((unsigned int) c < 0x80) break;
	    clen = mbcs_get_next(c, &wc);
	    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
	    for(i = 1; i < clen; i++) c = xxgetc();
	}
    } else
#endif
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f') ;
    return c;
}

/* Note that with interactive use, EOF cannot occur inside */
/* a comment.  However, semicolons inside comments make it */
/* appear that this does happen.  For this reason we use the */
/* special assignment EndOfFile=2 to indicate that this is */
/* going on.  This is detected and dealt with in Parse1Buffer. */

static int SkipComment(void)
{
    int c='#', i;
    Rboolean maybeLine = (ParseState.xxcolno == 1);
    if (maybeLine) {
    	char lineDirective[] = "#line";
    	for (i=1; i<5; i++) {
    	    c = xxgetc();
  	    if (c != (int)(lineDirective[i])) {
  	    	maybeLine = FALSE;
  	    	break;
  	    }
  	}
  	if (maybeLine)     
	    c = processLineDirective();
    }
    while (c != '\n' && c != R_EOF) 
	c = xxgetc();
    if (c == R_EOF) EndOfFile = 2;
    return c;
}

static int NumericValue(int c)
{
    int seendot = (c == '.');
    int seenexp = 0;
    int last = c;
    int nd = 0;
    int asNumeric = 0;
    int count = 1; /* The number of characters seen */

    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E'
	   || c == 'x' || c == 'X' || c == 'L')
    {
	count++;
	if (c == 'L') /* must be at the end.  Won't allow 1Le3 (at present). */
	    break;

	if (c == 'x' || c == 'X') {
	    if (count > 2 || last != '0') break;  /* 0x must be first */
	    YYTEXT_PUSH(c, yyp);
	    while(isdigit(c = xxgetc()) || ('a' <= c && c <= 'f') ||
		  ('A' <= c && c <= 'F') || c == '.') {
		YYTEXT_PUSH(c, yyp);
		nd++;
	    }
	    if (nd == 0) return ERROR;
	    if (c == 'p' || c == 'P') {
		YYTEXT_PUSH(c, yyp);
		c = xxgetc();
		if (!isdigit(c) && c != '+' && c != '-') return ERROR;
		if (c == '+' || c == '-') {
		    YYTEXT_PUSH(c, yyp);
		    c = xxgetc();
		}
		for(nd = 0; isdigit(c); c = xxgetc(), nd++)
		    YYTEXT_PUSH(c, yyp);
		if (nd == 0) return ERROR;
	    }
	    break;
	}
	if (c == 'E' || c == 'e') {
	    if (seenexp)
		break;
	    seenexp = 1;
	    seendot = seendot == 1 ? seendot : 2;
	    YYTEXT_PUSH(c, yyp);
	    c = xxgetc();
	    if (!isdigit(c) && c != '+' && c != '-') return ERROR;
	    if (c == '+' || c == '-') {
		YYTEXT_PUSH(c, yyp);
		c = xxgetc();
		if (!isdigit(c)) return ERROR;
	    }
	}
	if (c == '.') {
	    if (seendot)
		break;
	    seendot = 1;
	}
	YYTEXT_PUSH(c, yyp);
	last = c;
    }
    YYTEXT_PUSH('\0', yyp);
    /* Make certain that things are okay. */
    if(c == 'L') {
	double a = R_atof(yytext);
	int b = (int) a;
	/* We are asked to create an integer via the L, so we check that the
	   double and int values are the same. If not, this is a problem and we
	   will not lose information and so use the numeric value.
	*/
	if(a != (double) b) {
            if(seendot == 1 && seenexp == 0)
		warning(_("integer literal %sL contains decimal; using numeric value"), yytext);
	    else
		warning(_("non-integer value %s qualified with L; using numeric value"), yytext);
	    asNumeric = 1;
	    seenexp = 1;
	}
    }

    if(c == 'i') {
	yylval = mkComplex(yytext);
    } else if(c == 'L' && asNumeric == 0) {
	if (seendot == 1 && seenexp == 0)
	    warning(_("integer literal %sL contains unnecessary decimal point"), yytext);
	yylval = mkInt(yytext);
#if 0  /* do this to make 123 integer not double */
    } else if(!(seendot || seenexp)) {
	if(c != 'L') xxungetc(c);
	double a = R_atof(yytext);
	int b = (int) a;
	yylval = (a != (double) b) ? mkFloat(yytext) : mkInt(yytext);
#endif
    } else {
	if(c != 'L')
	    xxungetc(c);
	yylval = mkFloat(yytext);
    }

    return NUM_CONST;
}

/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */


#define STEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
	    nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), ParseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)


/* The idea here is that if a string contains \u escapes that are not
   valid in the current locale, we should switch to UTF-8 for that
   string.  Needs Unicode wide-char support.
*/

#if defined(__APPLE_CC__)
/* This may not be 100% true (see the comment in rlocales.h),
   but it seems true in normal locales */
# define __STDC_ISO_10646__
#endif

#if defined(Win32) || defined(__STDC_ISO_10646__)
typedef wchar_t ucs_t;
# define mbcs_get_next2 mbcs_get_next
#else
typedef unsigned int ucs_t;
# define WC_NOT_UNICODE 
static int mbcs_get_next2(int c, ucs_t *wc)
{
    int i, res, clen = 1; char s[9];

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
	*wc = (wchar_t) c;
	return 1;
    }
    if(utf8locale) {
	clen = utf8clen(c);
	for(i = 1; i < clen; i++) {
	    s[i] = xxgetc();
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), ParseState.xxlineno);
	}
	s[clen] ='\0'; /* x86 Solaris requires this */
	res = mbtoucs(wc, s, clen);
	if(res == -1) error(_("invalid multibyte character in parser at line %d"), ParseState.xxlineno);
    } else {
	/* This is not necessarily correct for stateful MBCS */
	while(clen <= MB_CUR_MAX) {
	    res = mbtoucs(wc, s, clen);
	    if(res >= 0) break;
	    if(res == -1)
		error(_("invalid multibyte character in parser at line %d"), ParseState.xxlineno);
	    /* so res == -2 */
	    c = xxgetc();
	    if(c == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), ParseState.xxlineno);
	    s[clen++] = c;
	} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--) xxungetc(s[i]);
    return clen;
}
#endif

#define WTEXT_PUSH(c) do { if(wcnt < 10000) wcs[wcnt++] = c; } while(0)

static SEXP mkStringUTF8(const ucs_t *wcs, int cnt)
{
    SEXP t;
    int nb;

/* NB: cnt includes the terminator */
#ifdef Win32
    nb = cnt*4; /* UCS-2/UTF-16 so max 4 bytes per wchar_t */
#else
    nb = cnt*6;
#endif
    char s[nb];
    R_CheckStack();
    memset(s, 0, nb); /* safety */
#ifdef WC_NOT_UNICODE
    {
	char *ss;
	for(ss = s; *wcs; wcs++) ss += ucstoutf8(ss, *wcs);
    }
#else
    wcstoutf8(s, wcs, nb);
#endif
    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharCE(s, CE_UTF8));
    UNPROTECT(1);
    return t;
}

#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--


static SEXP mkString2(const char *s, int len, Rboolean escaped)
{
    SEXP t;
    cetype_t enc = CE_NATIVE;

    if(known_to_be_latin1) enc= CE_LATIN1;
    else if(!escaped && known_to_be_utf8) enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
}


/* forSymbol is true when parsing backticked symbols */
static int StringValue(int c, Rboolean forSymbol)
{
    int quote = c;
    char currtext[1010], *ct = currtext;
    char st0[MAXELTSIZE];
    unsigned int nstext = MAXELTSIZE;
    char *stext = st0, *bp = st0;
    int wcnt = 0;
    ucs_t wcs[10001];
    Rboolean oct_or_hex = FALSE, use_wcs = FALSE;

    while ((c = xxgetc()) != R_EOF && c != quote) {
	CTEXT_PUSH(c);
	if (c == '\n') {
	    xxungetc(c);
	    /* Fix suggested by Mark Bravington to allow multiline strings
	     * by pretending we've seen a backslash. Was:
	     * return ERROR;
	     */
	    c = '\\';
	}
	if (c == '\\') {
	    c = xxgetc(); CTEXT_PUSH(c);
	    if ('0' <= c && c <= '7') {
		int octal = c - '0';
		if ('0' <= (c = xxgetc()) && c <= '7') {
		    CTEXT_PUSH(c);
		    octal = 8 * octal + c - '0';
		    if ('0' <= (c = xxgetc()) && c <= '7') {
			CTEXT_PUSH(c);
			octal = 8 * octal + c - '0';
		    } else {
			xxungetc(c);
			CTEXT_POP();
		    }
		} else {
		    xxungetc(c);
		    CTEXT_POP();
		}
		c = octal;
		oct_or_hex = TRUE;
	    }
	    else if(c == 'x') {
		int val = 0; int i, ext;
		for(i = 0; i < 2; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {
			xxungetc(c);
			CTEXT_POP();
			if (i == 0) { /* was just \x */
			    *ct = '\0';
			    errorcall(R_NilValue, _("'\\x' used without hex digits in character string starting \"%s\""), currtext);
			}
			break;
		    }
		    val = 16*val + ext;
		}
		c = val;
		oct_or_hex = TRUE;
	    }
	    else if(c == 'u') {
		unsigned int val = 0; int i, ext; 
		Rboolean delim = FALSE;

		if(forSymbol) 
		    error(_("\\uxxxx sequences not supported inside backticks (line %d)"), ParseState.xxlineno);
		if((c = xxgetc()) == '{') {
		    delim = TRUE;
		    CTEXT_PUSH(c);
		} else xxungetc(c);
		for(i = 0; i < 4; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {
			xxungetc(c);
			CTEXT_POP();
			if (i == 0) { /* was just \u */
			    *ct = '\0';
			    errorcall(R_NilValue, _("'\\u' used without hex digits in character string starting \"%s\""), currtext);
			}
			break;
		    }
		    val = 16*val + ext;
		}
		if(delim) {
		    if((c = xxgetc()) != '}')
			error(_("invalid \\u{xxxx} sequence (line %d)"),
			      ParseState.xxlineno);
		    else CTEXT_PUSH(c);
		}
		WTEXT_PUSH(val); /* this assumes wchar_t is Unicode */
		use_wcs = TRUE;
		continue;
	    }
	    else if(c == 'U') {
		unsigned int val = 0; int i, ext;
		Rboolean delim = FALSE;
		if(forSymbol) 
		    error(_("\\Uxxxxxxxx sequences not supported inside backticks (line %d)"), ParseState.xxlineno);
		if((c = xxgetc()) == '{') {
		    delim = TRUE;
		    CTEXT_PUSH(c);
		} else xxungetc(c);
		for(i = 0; i < 8; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {
			xxungetc(c);
			CTEXT_POP();
			if (i == 0) { /* was just \U */
			    *ct = '\0';
			    errorcall(R_NilValue, _("'\\U' used without hex digits in character string starting \"%s\""), currtext);
			}
			break;
		    }
		    val = 16*val + ext;
		}
		if(delim) {
		    if((c = xxgetc()) != '}')
			error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), ParseState.xxlineno);
		    else CTEXT_PUSH(c);
		}
		WTEXT_PUSH(val);
		use_wcs = TRUE;
		continue;
	    }
	    else {
		switch (c) {
		case 'a':
		    c = '\a';
		    break;
		case 'b':
		    c = '\b';
		    break;
		case 'f':
		    c = '\f';
		    break;
		case 'n':
		    c = '\n';
		    break;
		case 'r':
		    c = '\r';
		    break;
		case 't':
		    c = '\t';
		    break;
		case 'v':
		    c = '\v';
		    break;
		case '\\':
		    c = '\\';
		    break;
		case '"':
		case '\'':
		case ' ':
		case '\n':
		    break;
		default:
		    *ct = '\0';
		    errorcall(R_NilValue, _("'\\%c' is an unrecognized escape in character string starting \"%s\""), c, currtext);
		}
	    }
	} else if(mbcslocale) {
	    int i, clen;
	    ucs_t wc;
	    clen = mbcs_get_next2(c, &wc);
	    WTEXT_PUSH(wc);
	    for(i = 0; i < clen - 1; i++){
		STEXT_PUSH(c);
		c = xxgetc();
		if (c == R_EOF) break;
		CTEXT_PUSH(c);
		if (c == '\n') {
		    xxungetc(c); CTEXT_POP();
		    c = '\\';
		}
	    }
	    if (c == R_EOF) break;
	    STEXT_PUSH(c);
	    continue;
	}
	STEXT_PUSH(c);
	if ((unsigned int) c < 0x80) WTEXT_PUSH(c);
	else { /* have an 8-bit char in the current encoding */
#ifdef WC_NOT_UNICODE
	    ucs_t wc;
	    char s[2] = " ";
	    s[0] = c;
	    mbtoucs(&wc, s, 2);
#else
	    wchar_t wc;
	    char s[2] = " ";
	    s[0] = c;
	    mbrtowc(&wc, s, 2, NULL);
#endif
	    WTEXT_PUSH(wc);
	}
    }
    STEXT_PUSH('\0');
    WTEXT_PUSH(0);
    if (c == R_EOF) {
        if(stext != st0) free(stext);
        yylval = R_NilValue;
    	return INCOMPLETE_STRING;
    }
    if(forSymbol) {
	yylval = install(stext);
	if(stext != st0) free(stext);
	return SYMBOL;
    } else {
	if(use_wcs) {
	    if(oct_or_hex)
		error(_("mixing Unicode and octal/hex escapes in a string is not allowed"));
	    if(wcnt < 10000)
		yylval = mkStringUTF8(wcs, wcnt); /* include terminator */
	    else
		error(_("string at line %d containing Unicode escapes not in this locale\nis too long (max 10000 chars)"), ParseState.xxlineno);
	} else
	    yylval = mkString2(stext,  bp - stext - 1, oct_or_hex);
	if(stext != st0) free(stext);
	return STR_CONST;
    }
}

static int SpecialValue(int c)
{
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    while ((c = xxgetc()) != R_EOF && c != '%') {
	if (c == '\n') {
	    xxungetc(c);
	    return ERROR;
	}
	YYTEXT_PUSH(c, yyp);
    }
    if (c == '%')
	YYTEXT_PUSH(c, yyp);
    YYTEXT_PUSH('\0', yyp);
    yylval = install(yytext);
    return SPECIAL;
}

/* return 1 if name is a valid name 0 otherwise */
static int isValidName(const char *name)
{
    const char *p = name;
    int i;

    if(mbcslocale) {
	/* the only way to establish which chars are alpha etc is to
	   use the wchar variants */
	int n = strlen(name), used;
	wchar_t wc;
	used = Mbrtowc(&wc, p, n, NULL); p += used; n -= used;
	if(used == 0) return 0;
	if (wc != L'.' && !iswalpha(wc) ) return 0;
	if (wc == L'.') {
	    /* We don't care about other than ASCII digits */
	    if(isdigit(0xff & (int)*p)) return 0;
	    /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
	}
	while((used = Mbrtowc(&wc, p, n, NULL))) {
	    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
	    p += used; n -= used;
	}
	if (*p != '\0') return 0;
    } else {
	int c = 0xff & *p++;
	if (c != '.' && !isalpha(c) ) return 0;
	if (c == '.' && isdigit(0xff & (int)*p)) return 0;
	while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
	if (c != '\0') return 0;
    }

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++)
	if (strcmp(keywords[i].name, name) == 0) return 0;

    return 1;
}


static int SymbolValue(int c)
{
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
    if(mbcslocale) {
	wchar_t wc; int i, clen;
	clen = mbcs_get_next(c, &wc);
	while(1) {
	    /* at this point we have seen one char, so push its bytes
	       and get one more */
	    for(i = 0; i < clen; i++) {
		YYTEXT_PUSH(c, yyp);
		c = xxgetc();
	    }
	    if(c == R_EOF) break;
	    if(c == '.' || c == '_') {
		clen = 1;
		continue;
	    }
	    clen = mbcs_get_next(c, &wc);
	    if(!iswalnum(wc)) break;
	}
    } else
	do {
	    YYTEXT_PUSH(c, yyp);
	} while ((c = xxgetc()) != R_EOF &&
		 (isalnum(c) || c == '.' || c == '_'));
    xxungetc(c);
    YYTEXT_PUSH('\0', yyp);
    if ((kw = KeywordLookup(yytext))) 
	return kw;
    
    yylval = install(yytext);
    return SYMBOL;
}

static void setParseFilename(SEXP newname) {
    SEXP class;
    PROTECT(newname);    
    if (isEnvironment(ParseState.SrcFile)) {
    	SEXP oldname = findVar(install("filename"), ParseState.SrcFile);
    	if (isString(oldname) && length(oldname) > 0 &&
    	    strcmp(CHAR(STRING_ELT(oldname, 0)),
    	           CHAR(STRING_ELT(newname, 0))) == 0) return;
	REPROTECT(ParseState.SrcFile = NewEnvironment(R_NilValue, R_NilValue, R_EmptyEnv), ParseState.SrcFileProt);
	defineVar(install("filename"), newname, ParseState.SrcFile);
    }
    if (ParseState.keepSrcRefs) {
	defineVar(install("original"), ParseState.Original, ParseState.SrcFile);

        PROTECT(class = allocVector(STRSXP, 2));
        SET_STRING_ELT(class, 0, mkChar("srcfilealias"));
        SET_STRING_ELT(class, 1, mkChar("srcfile"));
	setAttrib(ParseState.SrcFile, R_ClassSymbol, class);
        UNPROTECT(1);
    } 
    UNPROTECT(1);
}

static int processLineDirective()
{
    int c, tok, linenumber;
    c = SkipSpace();
    if (!isdigit(c)) return(c);
    tok = NumericValue(c);
    linenumber = atoi(yytext);
    c = SkipSpace();
    if (c == '"') 
	tok = StringValue(c, FALSE);
    else
    	xxungetc(c);
    if (tok == STR_CONST) 
	setParseFilename(yylval);
    while ((c = xxgetc()) != '\n' && c != R_EOF) /* skip */ ;
    ParseState.xxlineno = linenumber;
    /* we don't change xxparseno here:  it counts parsed lines, not official lines */
    R_ParseContext[R_ParseContextLast] = '\0';  /* Context report shouldn't show the directive */
    return(c);
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c;
    wchar_t wc;

    if (SavedToken) {
	c = SavedToken;
	yylval = SavedLval;
	SavedLval = R_NilValue;
	SavedToken = 0;
	yylloc.first_line = xxlinesave;
	yylloc.first_column = xxcolsave;
	yylloc.first_byte = xxbytesave;
	yylloc.first_parsed = xxparsesave;
	return c;
    }
    xxcharsave = xxcharcount; /* want to be able to go back one token */

    c = SkipSpace();
    if (c == '#') c = SkipComment();

    prev_yylloc = yylloc;
    yylloc.first_line = ParseState.xxlineno;
    yylloc.first_column = ParseState.xxcolno;
    yylloc.first_byte = ParseState.xxbyteno;
    yylloc.first_parsed = ParseState.xxparseno;

    if (c == R_EOF) return END_OF_INPUT;
    /* Either digits or symbols can start with a "." */
    /* so we need to decide which it is and jump to  */
    /* the correct spot. */

    if (c == '.' && typeofnext() >= 2) goto symbol;

    /* literal numbers */

    if (c == '.') return NumericValue(c);
    /* We don't care about other than ASCII digits */
    if (isdigit(c)) return NumericValue(c);

    /* literal strings */

    if (c == '\"' || c == '\'')
	return StringValue(c, FALSE);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return StringValue(c, TRUE);
 symbol:

    if (c == '.') return SymbolValue(c);
    if(mbcslocale) {
	mbcs_get_next(c, &wc);
	if (iswalpha(wc)) return SymbolValue(c);
    } else
	if (isalpha(c)) return SymbolValue(c);

    /* compound tokens */

    switch (c) {
    case '<':
	if (nextchar('=')) {
	    yylval = install("<=");
	    return LE;
	}
	if (nextchar('-')) {
	    yylval = install("<-");
	    return LEFT_ASSIGN;
	}
	if (nextchar('<')) {
	    if (nextchar('-')) {
		yylval = install("<<-");
		return LEFT_ASSIGN;
	    }
	    else
		return ERROR;
	}
	yylval = install("<");
	return LT;
    case '-':
	if (nextchar('>')) {
	    if (nextchar('>')) {
		yylval = install("<<-");
		return RIGHT_ASSIGN;
	    }
	    else {
		yylval = install("<-");
		return RIGHT_ASSIGN;
	    }
	}
	yylval = install("-");
	return '-';
    case '>':
	if (nextchar('=')) {
	    yylval = install(">=");
	    return GE;
	}
	yylval = install(">");
	return GT;
    case '!':
	if (nextchar('=')) {
	    yylval = install("!=");
	    return NE;
	}
	yylval = install("!");
	return '!';
    case '=':
	if (nextchar('=')) {
	    yylval = install("==");
	    return EQ;
	}
	yylval = install("=");
	return EQ_ASSIGN;
    case ':':
	if (nextchar(':')) {
	    if (nextchar(':')) {
		yylval = install(":::");
		return NS_GET_INT;
	    }
	    else {
		yylval = install("::");
		return NS_GET;
	    }
	}
	if (nextchar('=')) {
	    yylval = install(":=");
	    return LEFT_ASSIGN;
	}
	yylval = install(":");
	return ':';
    case '&':
	if (nextchar('&')) {
	    yylval = install("&&");
	    return AND2;
	}
	yylval = install("&");
	return AND;
    case '|':
	if (nextchar('|')) {
	    yylval = install("||");
	    return OR2;
	}
	yylval = install("|");
	return OR;
    case '{':
	yylval = install("{");
	return c;
    case '}':
	return c;
    case '(':
	yylval = install("(");
	return c;
    case ')':
	return c;
    case '[':
	if (nextchar('[')) {
	    yylval = install("[[");
	    return LBB;
	}
	yylval = install("[");
	return c;
    case ']':
	return c;
    case '?':
	strcpy(yytext, "?");
	yylval = install(yytext);
	return c;
    case '*':
	/* Replace ** by ^.  This has been here since 1998, but is
	   undocumented (at least in the obvious places).  It is in
	   the index of the Blue Book with a reference to p. 431, the
	   help for 'Deprecated'.  S-PLUS 6.2 still allowed this, so
	   presumably it was for compatibility with S. */
	if (nextchar('*'))
	    c='^';
	yytext[0] = c;
	yytext[1] = '\0';
	yylval = install(yytext);
	return c;
    case '+':
    case '/':
    case '^':
    case '~':
    case '$':
    case '@':
	yytext[0] = c;
	yytext[1] = '\0';
	yylval = install(yytext);
	return c;
    default:
	return c;
    }
}

static void setlastloc(void)
{
    yylloc.last_line = ParseState.xxlineno;
    yylloc.last_column = ParseState.xxcolno;
    yylloc.last_byte = ParseState.xxbyteno;
    yylloc.last_parsed = ParseState.xxparseno;
}

static int yylex(void)
{
    int tok;

 again:

    tok = token();

    /* Newlines must be handled in a context */
    /* sensitive way.  The following block of */
    /* deals directly with newlines in the */
    /* body of "if" statements. */

    if (tok == '\n') {

	if (EatLines || *contextp == '[' || *contextp == '(')
	    goto again;

	/* The essence of this is that in the body of */
	/* an "if", any newline must be checked to */
	/* see if it is followed by an "else". */
	/* such newlines are discarded. */

	if (*contextp == 'i') {

	    /* Find the next non-newline token */

	    while(tok == '\n')
		tok = token();

	    /* If we encounter "}", ")" or "]" then */
	    /* we know that all immediately preceding */
	    /* "if" bodies have been terminated. */
	    /* The corresponding "i" values are */
	    /* popped off the context stack. */

	    if (tok == '}' || tok == ')' || tok == ']' ) {
		while (*contextp == 'i')
		    ifpop();
		*contextp-- = 0;
		setlastloc();
		return tok;
	    }

	    /* When a "," is encountered, it terminates */
	    /* just the immediately preceding "if" body */
	    /* so we pop just a single "i" of the */
	    /* context stack. */

	    if (tok == ',') {
		ifpop();
		setlastloc();
		return tok;
	    }

	    /* Tricky! If we find an "else" we must */
	    /* ignore the preceding newline.  Any other */
	    /* token means that we must return the newline */
	    /* to terminate the "if" and "push back" that */
	    /* token so that we will obtain it on the next */
	    /* call to token.  In either case sensitivity */
	    /* is lost, so we pop the "i" from the context */
	    /* stack. */

	    if(tok == ELSE) {
		EatLines = 1;
		ifpop();
		setlastloc();
		return ELSE;
	    }
	    else {
		ifpop();
		SavedToken = tok;
		xxlinesave = yylloc.first_line;
		xxcolsave  = yylloc.first_column;
		xxbytesave = yylloc.first_byte;
		xxparsesave = yylloc.first_parsed;
		SavedLval = yylval;
		setlastloc();
		return '\n';
	    }
	}
	else {
	    setlastloc();
	    return '\n';
	}
    }

    /* Additional context sensitivities */

    switch(tok) {

	/* Any newlines immediately following the */
	/* the following tokens are discarded. The */
	/* expressions are clearly incomplete. */

    case '+':
    case '-':
    case '*':
    case '/':
    case '^':
    case LT:
    case LE:
    case GE:
    case GT:
    case EQ:
    case NE:
    case OR:
    case AND:
    case OR2:
    case AND2:
    case SPECIAL:
    case FUNCTION:
    case WHILE:
    case REPEAT:
    case FOR:
    case IN:
    case '?':
    case '!':
    case '=':
    case ':':
    case '~':
    case '$':
    case '@':
    case LEFT_ASSIGN:
    case RIGHT_ASSIGN:
    case EQ_ASSIGN:
	EatLines = 1;
	break;

	/* Push any "if" statements found and */
	/* discard any immediately following newlines. */

    case IF:
	IfPush();
	EatLines = 1;
	break;

	/* Terminate any immediately preceding "if" */
	/* statements and discard any immediately */
	/* following newlines. */

    case ELSE:
	ifpop();
	EatLines = 1;
	break;

	/* These tokens terminate any immediately */
	/* preceding "if" statements. */

    case ';':
    case ',':
	ifpop();
	break;

	/* Any newlines following these tokens can */
	/* indicate the end of an expression. */

    case SYMBOL:
    case STR_CONST:
    case NUM_CONST:
    case NULL_CONST:
    case NEXT:
    case BREAK:
	EatLines = 0;
	break;

	/* Handle brackets, braces and parentheses */

    case LBB:
	if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
	    error(_("contextstack overflow at line %d"), ParseState.xxlineno);
	*++contextp = '[';
	*++contextp = '[';
	break;

    case '[':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), ParseState.xxlineno);
	*++contextp = tok;
	break;

    case '{':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), ParseState.xxlineno);
	*++contextp = tok;
	EatLines = 1;
	break;

    case '(':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), ParseState.xxlineno);
	*++contextp = tok;
	break;

    case ']':
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	EatLines = 0;
	break;

    case '}':
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	break;

    case ')':
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	EatLines = 0;
	break;

    }
    setlastloc();
    return tok;
}
