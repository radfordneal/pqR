/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2010   The R Development Core Team.
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

 *  ------------------------------------------------------------------------
 *  Matching and Partial Matching for Strings
 *
 *  In theory all string matching code should be placed in this file
 *  At present there are still a couple of rogue matchers about.
 *
 *  Routines for general partial or exact string matching (R Neal, May 2011):
 *
 *  ep_match_strings (const char *f, const char *t)
 *      Returns 0 if f and t do not match at all, 1 if they match exactly,
 *      and -1 if t is a prefix of f, but does not match exactly.  Note that 
 *      the empty string is a prefix of any string.
 *
 *  ep_match_exprs (SEXP formal, SEXP tag)
 *      Like ep_match_strings but for strings specified as SEXPs, which
 *      may be of type SYMSXP, CHARSXP, or STRSXP (the last translated).
 *
 *  ep_match_string_expr (const char *f, SEXP tag)
 *      Like ep_match_strings but with the second string specified as a SEXP,
 *      as for ep_match_exprs.
 *
 *  psmatch is an old routine, still included in case anyone uses it.
 *  It is inferior to the new ep_match functions, which return full
 *  information about exact or partial match at no extra cost.
 *
 *  psmatch(char *, char *, int);
 *
 *  This code will perform partial matching for list tags.  When
 *  exact is 1, and exact match is required (typically after ...)
 *  otherwise partial matching is performed.
 *
 *  Examples:
 *
 *	psmatch("aaa", "aaa", 0) -> 1
 *	psmatch("aaa", "aa", 0) -> 1
 *	psmatch("aa", "aaa", 0) -> 0
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"

/* This is a horrible kludge used in logic.c and in summary.c (where 
   it previously was covertly defined as extern). */

SEXP fixup_NaRm(SEXP args)
{
    SEXP a, r, t, na_value, prev = R_NilValue;

    /* Need to make sure na.rm is last and exists */
    na_value = ScalarLogical(FALSE);
    for(a = args ; a != R_NilValue; a = CDR(a)) {
	if(TAG(a) == R_NaRmSymbol) {
	    if(CDR(a) == R_NilValue) return args;
	    na_value = CAR(a);
	    if(prev == R_NilValue) args = CDR(a);
	    else SETCDR(prev, CDR(a));
	}
	prev = a;
    }

    t = cons_with_tag (na_value, R_NilValue, R_NaRmSymbol);

    if (args == R_NilValue)
	args = t;
    else {
	r = args;
	while (CDR(r) != R_NilValue) r = CDR(r);
	SETCDR(r, t);
    }

    return args;
}

/* used in subscript.c and subassign.c */
Rboolean NonNullStringMatch(SEXP s, SEXP t)
{
    /* "" or NA string matches nothing */
    if (s == NA_STRING || t == NA_STRING) return FALSE;
    if (CHAR(s)[0] && CHAR(t)[0] && Seql(s, t))
	return TRUE;
    else
	return FALSE;
}

/*  Exact or partial string match.  Returns 0 if f and t do not match at all, 
    1 if they match exactly, and -1 if t is a prefix of f, but does not match 
    exactly.  Note that the empty string is a prefix of any string. */

int ep_match_strings (const char *f, const char *t)
{
    while (*t) {
        if (*t != *f)
            return 0;
        t++;
        f++;
    }

    return *f==0 ? 1 : -1;
}

/*  Exact or partial match for strings given by SEXPs.  Returned value
    is as for ep_match_strings above. */

int ep_match_exprs (SEXP formal, SEXP tag)
{
    const char *f, *t;

    if (formal==tag)
        return 1;

    switch (TYPEOF(formal)) {
    case SYMSXP:
	f = CHAR(PRINTNAME(formal));
	break;
    case CHARSXP:
	f = CHAR(formal);
	break;
    case STRSXP:
	f = translateChar(STRING_ELT(formal, 0));
	break;
    default:
	goto fail;
    }

    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = translateChar(STRING_ELT(tag, 0));
	break;
    default:
	goto fail;
    }

    return ep_match_strings (f, t);

 fail:
    error(_("invalid partial string match"));
    return 0;/* for -Wall */
}

/*  Exact or partial match for strings, with second given by a SEXP.  
    Returned value is as for ep_match_strings above. */

int ep_match_string_expr (const char *f, SEXP tag)
{
    const char *t;

    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = translateChar(STRING_ELT(tag, 0));
	break;
    default:
        error(_("invalid partial string match"));
        return 0;/* for -Wall */
    }

    return ep_match_strings (f, t);
}

/* currently unused outside this file */
Rboolean psmatch(const char *f, const char *t, Rboolean exact)
{
    if (exact)
	return (Rboolean)!strcmp(f, t);
    /* else */
    while (*t) {
	if (*t != *f)   return FALSE;
	t++;
	f++;
    }
    return TRUE;
}


/* Matching formals and arguments */

/* Are these are always native charset? */
Rboolean pmatch(SEXP formal, SEXP tag, Rboolean exact)
{
    const char *f, *t;
    switch (TYPEOF(formal)) {
    case SYMSXP:
	f = CHAR(PRINTNAME(formal));
	break;
    case CHARSXP:
	f = CHAR(formal);
	break;
    case STRSXP:
	f = translateChar(STRING_ELT(formal, 0));
	break;
    default:
	goto fail;
    }
    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = translateChar(STRING_ELT(tag, 0));
	break;
    default:
	goto fail;
    }
    return psmatch(f, t, exact);
 fail:
    error(_("invalid partial string match"));
    return FALSE;/* for -Wall */
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a C string. */

static SEXP matchPar_int(const char *tag, SEXP *list, Rboolean exact)
{
    if (*list == R_NilValue)
	return R_MissingArg;
    else if (TAG(*list) != R_NilValue &&
	     psmatch(tag, CHAR(PRINTNAME(TAG(*list))), exact)) {
	SEXP s = *list;
	*list = CDR(*list);
	return CAR(s);
    }
    else {
	SEXP last = *list;
	SEXP next = CDR(*list);
	while (next != R_NilValue) {
	    if (TAG(next) != R_NilValue &&
		psmatch(tag, CHAR(PRINTNAME(TAG(next))), exact)) {
		SETCDR(last, CDR(next));
		return CAR(next);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return R_MissingArg;
    }
}

/* unused outside this file */
SEXP attribute_hidden matchPar(const char *tag, SEXP * list)
{
    return matchPar_int(tag, list, FALSE);
}



/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArg(SEXP tag, SEXP * list)
{
    return matchPar(CHAR(PRINTNAME(tag)), list);
}


/* Destructively Extract A Named List Element. */
/* Returns the first exactly matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArgExact(SEXP tag, SEXP * list)
{
      return matchPar_int(CHAR(PRINTNAME(tag)), list, TRUE);
}


/* Match the supplied arguments with the formals, and return a list of the 
   matched or missing arguments.

   Two ways of calling matchArgs are supported, differing in how the names
   of formal arguments are specified.  

   Primitives that need to match arguments can pass R_NilValue for "formals" 
   and pass a pointer to an array of strings for "formal_names", along with 
   the number of formal names as "arg_count". 

   In applyClosure some other routines, matchArgs is instead called with
   "formals" being a pairlist having tags that are names of formal arguments.
   In this case, "formal_names" should be NULL, and "arg_count" should be 0.
   
   If formal names are specifed using "formals", the entries in the list of
   actual arguments returned will have tags set from the list of formal 
   arguments.  This is not done if "formal_names" is used. 

   The MISSING flag is set for actual arguments returned that are R_MissingArg,
   except that ... isn't flagged as missing even when its value is R_MissingArg 
   because no arguments were left for it. */

/* Find name of formal argument from either a C string (formal_names[arg_i]) or
   a tag from a formals list (formal_tag[arg_i]). */

#define FORMALSTR(formal_names,formal_tag,arg_i) ( \
    formal_names != NULL ? formal_names[arg_i] \
                         : CHAR(PRINTNAME(formal_tag[arg_i])) )

/* We need to leave 'supplied' unchanged in case we call UseMethod */
/* MULTIPLE_MATCHES was added by RI in Jan 2005 but never activated:
   code in R-2-8-branch */

SEXP attribute_hidden matchArgs (
    SEXP formals, char **formal_names, int arg_count, SEXP supplied, SEXP call)
{
    SEXP b, last_positional, last_potential_match, actuals_list;
    int arg_i, dots, n_supplied, n_matched;

#if 0  /* Enable for debugging output */
    { SEXP p; int c;
      if (formals!=R_NilValue) 
      { printf("--- Entering matchArgs.  Formals: ");
        for (p = formals; p!=R_NilValue; p = CDR(p))
        { printf ("%c", TAG(p)==R_NilValue ? '*' : CHAR(PRINTNAME(TAG(p)))[0]);
        }
      }
      else
      { printf("--- Entering matchArgs.  Fstrngs: ");
        for (c = 0; c<arg_count; c++)
        { printf ("%c", formal_names[c][0]);
        }
      }
      printf("  Supplied: ");
      for (p = supplied; p!=R_NilValue; p = CDR(p))
      { printf ("%c", TAG(p)==R_NilValue ? '*' : CHAR(PRINTNAME(TAG(p)))[0]);
      }
      printf(" ");
      for (p = supplied; p!=R_NilValue; p = CDR(p))
      { printf ("%c", CAR(p)==R_MissingArg ? '*' : 'x');
      }
      printf("\n");
    }
#endif

    /* We avoid modifying either the formal or the supplied arguments, since
       this could cause problems.

       The "actual" and "formal_tag" arrays below are used in preference to 
       scanning and modifying the pairlists for speed.  It is assumed that 
       they will not be excessively large (as for "fargsused"), but that 
       "supplied" might be very long (with arguments matching "..."), so 
       for the supplied arguments we use an array only for the used flag.
     */

    n_supplied = length(supplied);
    if (formal_names==NULL)
        arg_count = length(formals);

    char suppused[n_supplied], *u;
    char fargused[arg_count];
    SEXP actual[arg_count];
    /* Below is used only when formal_names==NULL; has size 1 if not used. */
    SEXP formal_tag[formal_names==NULL ? arg_count : 1]; 

    /* If "formals" specified, copy formal tags from "formals" to "formal_tags".
       Save the location of the first ... in "dots" (-1 if none).  Initialize 
       "actual" array to R_MissingArg and "fargused" to 0. */
    
    dots = -1;
    if (formal_names != NULL) {
        for (arg_i = 0; arg_i < arg_count; arg_i++) {
            if (dots==-1 && strcmp(formal_names[arg_i],"...")==0)
                dots = arg_i;
            actual[arg_i] = R_MissingArg;
            fargused[arg_i] = 0;
        }
    } else { /* formals != R_NilValue */
        SEXP fm = formals;
        for (arg_i = 0; arg_i < arg_count; arg_i++) {
            formal_tag[arg_i] = TAG(fm);
            if (dots==-1 && formal_tag[arg_i]==R_DotsSymbol)
                dots = arg_i;
            actual[arg_i] = R_MissingArg;
            fargused[arg_i] = 0;
            fm = CDR(fm);
        }
    }

    /* First pass: exact matches by tag */
    /* Grab matched arguments and check */
    /* for multiple exact matches. */

    /* Sets last_positional to the last positional argument, or to R_NilValue
       if none.  Sets last_potential_match to the last tagged argument that
       hasn't been exactly matched, or to R_NilValue if none.  Also counts
       number of supplied arguments initially matched. */

    last_positional = R_NilValue;
    last_potential_match = R_NilValue;
    n_matched = 0;

    for (b = supplied, u = suppused; b != R_NilValue; b = CDR(b), u++) {
        SEXP tag_b = TAG(b);
        *u = 0;
	if (tag_b == R_NilValue)
            last_positional = b;
        else {
            for (arg_i = 0; arg_i < arg_count; arg_i++) {
                if (formal_names != NULL) {
                    if (strcmp (formal_names[arg_i], "...") == 0
                     || ep_match_string_expr (formal_names[arg_i], tag_b) <= 0)
                        continue;
                } else { /* formals != R_NilValue */
                    if (formal_tag[arg_i] == R_DotsSymbol
                     || ep_match_exprs (formal_tag[arg_i], tag_b) <= 0)
                        continue;
                }
                if (fargused[arg_i] == 2)
		    error(_("formal argument \"%s\" matched by multiple actual arguments"),
			  FORMALSTR (formal_names, formal_tag, arg_i));
		actual[arg_i] = CAR(b);
		*u = 2;
		fargused[arg_i] = 2;
                n_matched += 1;
                break;  /* assumes no duplicate names in formals */
	    }
            if (*u==0)
                last_potential_match = b;
	}
    }

    /* Second pass: partial matches based on tags */
    /* Stop looking after ..., since only exact matches are allowed there */

    if (last_potential_match != R_NilValue) {
        for (b = supplied, u = suppused; ; b = CDR(b), u++) {
            SEXP tag_b = TAG(b);
            if (tag_b != R_NilValue && *u==0) {
                for (arg_i = 0; arg_i<arg_count && arg_i!=dots; arg_i++) {
                    if (fargused[arg_i] == 2)
                        continue;
                    if (formal_names != NULL) {
                        if (ep_match_string_expr(formal_names[arg_i],tag_b)==0)
                            continue;
                    } else { /* formals != R_NilValue */
                        if (ep_match_exprs (formal_tag[arg_i], tag_b) == 0)
                            continue;
                    }
                    if (*u)
                        error(
                          _("argument %d matches multiple formal arguments"), 
                          u-suppused+1);
                    if (fargused[arg_i] == 1)
v                        error(_("formal argument \"%s\" matched by multiple actual arguments"),
		              FORMALSTR (formal_names, formal_tag, arg_i));
                    if (R_warn_partial_match_args) 
                        warningcall(call,
                            _("partial argument match of '%s' to '%s'"),
                            CHAR(PRINTNAME(tag_b)),
		            FORMALSTR (formal_names, formal_tag, arg_i));
                    actual[arg_i] = CAR(b);
                    *u = 1;
                    fargused[arg_i] = 1;
                    n_matched += 1;
                }
            }
            if (b == last_potential_match)
                break;
        }
    }

    /* Third pass: matches based on order */
    /* All args specified in tag=value form */
    /* have now been matched.  If we find ... */
    /* we gobble up all the remaining args. */
    /* Otherwise we bind untagged values in */
    /* order to any unmatched formals. */

    if (last_positional != R_NilValue) {

        b = supplied;
        u = suppused;
        arg_i = 0;

        while (arg_i < arg_count && arg_i != dots) {

            /* If this formal already matched by tag, skip to next formal */
            if (actual[arg_i] != R_MissingArg) {
                arg_i += 1;
                continue;
            } 

            /* Find the next positional argument, which must exist */
            while (TAG(b) != R_NilValue) {
                b = CDR(b); 
                u += 1;
            }

            /* We have a positional match */
            actual[arg_i] = CAR(b);
            *u = 1;
            n_matched += 1;

            /* Move to next supplied arg and formal, unless this is the last */
            if (b == last_positional)
                break;
            b = CDR(b);
            u += 1;
            arg_i += 1;
        }
    }

    if (dots != -1) { /* Gobble up all unused actuals for ... */

        if (n_matched < n_supplied) {

            SEXP a;

	    PROTECT(a = actual[dots] = allocList(n_supplied-n_matched));
	    SET_TYPEOF(a, DOTSXP);

	    for (b = supplied, u = suppused; b != R_NilValue; b = CDR(b), u++) {
		if (*u==0) {
		    SETCAR(a, CAR(b));
		    SET_TAG(a, TAG(b));
		    a = CDR(a);
		}
            }
	}

    } else if (n_matched != n_supplied) { /* Report error */

	SEXP unused = R_NilValue, unusedForError = R_NilValue, last;

	/* Find the unused arguments */
        last = R_NilValue;
	for (b = supplied, u = suppused; b != R_NilValue; b = CDR(b), u++)
	    if (*u==0) {
		if(last == R_NilValue) {
		    PROTECT(unused = CONS(CAR(b), R_NilValue));
		    SET_TAG(unused, TAG(b));
		    last = unused;
		} else {
		    SETCDR(last, CONS(CAR(b), R_NilValue));
		    last = CDR(last);
		    SET_TAG(last, TAG(b));
		}
	    }

        /* show bad arguments in call without evaluating them */
        last = R_NilValue;
        for(b = unused ; b != R_NilValue ; b = CDR(b)) {
            SEXP tagB = TAG(b), carB = CAR(b) ;
            if (TYPEOF(carB) == PROMSXP) carB = PREXPR(carB) ;
            if (last == R_NilValue) {
                PROTECT(last = CONS(carB, R_NilValue));
                SET_TAG(last, tagB);
                unusedForError = last;
            } else {
                SETCDR(last, CONS(carB, R_NilValue));
                last = CDR(last);
                SET_TAG(last, tagB);
            }
        }
	errorcall(call /* R_GlobalContext->call */,
	   _("unused argument(s) %s"),
	   CHAR(STRING_ELT(deparse1line(unusedForError, 0), 0)) + 4);
                  /* '+ 4' is to remove 'list' from 'list(badTag1,...)' */
    }

    /* Create the pairlist of actual arguments from the "actual" array, 
       setting the MISSING flag as appropriate.  Also, set the tags in 
       "actuals_list" from "formals", unless formal_names was used instead. */

    actuals_list = R_NilValue;
    for (arg_i = arg_count-1; arg_i >= 0; arg_i--) {
        actuals_list = formal_names != NULL ? CONS (actual[arg_i], actuals_list)
               : cons_with_tag (actual[arg_i], actuals_list, formal_tag[arg_i]);
        if (actual[arg_i] == R_MissingArg && arg_i != dots)
            SET_MISSING (actuals_list, 1);
    }

    if (dots!=-1 && actual[dots]!=R_MissingArg)
        UNPROTECT(1);

#if 0  /* Enable for debugging output */
    { SEXP p;
      printf("    Leaving matchArgs.   Actuals: ");
      for (p = actuals_list; p!=R_NilValue; p = CDR(p))
      { printf ("%c", TAG(p)==R_NilValue ? '*' : CHAR(PRINTNAME(TAG(p)))[0]);
      }
      printf(" ");
      for (p = actuals_list; p!=R_NilValue; p = CDR(p))
      { printf ("%c", CAR(p)==R_MissingArg ? '*' : 'x');
      }
      printf(" ");
      for (p = actuals_list; p!=R_NilValue; p = CDR(p))
      { printf ("%d", MISSING(p));
      }
      printf("\n");
    }
#endif

    return(actuals_list);
}
