/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2016  The R Core Team
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
/* -> Rinternals.h which exports R_compute_identical() */

/* Implementation of identical(x, y) */

/* How are  R "double"s compared : */
typedef enum {
    bit_NA__num_bit = 0,// S's default - look at bit pattern, also for NA/NaN's
    bit_NA__num_eq  = 1,// bitwise comparison for NA / NaN; '==' for other numbers
 single_NA__num_bit = 2,// one kind of NA or NaN; for num, use 'bit'comparison
 single_NA__num_eq  = 3,// one kind of NA or NaN; for num, use '==' : R's DEFAULT
} ne_strictness_type;
/* NOTE:  ne_strict = NUM_EQ + (SINGLE_NA * 2)  = NUM_EQ | (SINGLE_NA << 1)   */

static Rboolean neWithNaN(double x, double y, ne_strictness_type str);


/* .Internal(identical(..)) */
SEXP attribute_hidden do_identical(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int num_eq = 1, single_NA = 1, attr_as_set = 1, ignore_bytecode = 1,
	ignore_env = 0, ignore_srcref = 1, nargs = length(args), flags;
    /* avoid problems with earlier (and future) versions captured in S4
       methods: but this should be fixed where it is caused, in
       'methods'!

       checkArity(op, args); */
    if (nargs < 5)
	error("%d arguments passed to .Internal(%s) which requires %d",
	      length(args), PRIMNAME(op), PRIMARITY(op));

    SEXP x = CAR(args); args = CDR(args);
    SEXP y = CAR(args); args = CDR(args);
    num_eq = asLogical(CAR(args)); args = CDR(args);
    single_NA = asLogical(CAR(args)); args = CDR(args);
    attr_as_set = asLogical(CAR(args)); args = CDR(args);
    if (nargs >= 6)
	ignore_bytecode = asLogical(CAR(args));
    if (nargs >= 7)
	ignore_env = asLogical(CADR(args));
    if (nargs >= 8)
	ignore_srcref = asLogical(CADDR(args));

    if(num_eq          == NA_LOGICAL) error(_("invalid '%s' value"), "num.eq");
    if(single_NA       == NA_LOGICAL) error(_("invalid '%s' value"), "single.NA");
    if(attr_as_set     == NA_LOGICAL) error(_("invalid '%s' value"), "attrib.as.set");
    if(ignore_bytecode == NA_LOGICAL) error(_("invalid '%s' value"), "ignore.bytecode");
    if(ignore_env      == NA_LOGICAL) error(_("invalid '%s' value"), "ignore.environment");
    if(ignore_srcref   == NA_LOGICAL) error(_("invalid '%s' value"), "ignore.srcref");

    flags = (num_eq ? 0 : 1) + (single_NA ? 0 : 2) + (attr_as_set ? 0 : 4) +
	(ignore_bytecode ? 0 : 8) + (ignore_env ? 0 : 16) + (ignore_srcref ? 0 : 32);
    return ScalarLogical(R_compute_identical(x, y, flags));
}

#define NUM_EQ		(!(flags & 1))
#define SINGLE_NA       (!(flags & 2))
#define ATTR_AS_SET     (!(flags & 4))
#define IGNORE_BYTECODE (!(flags & 8))
#define IGNORE_ENV      (!(flags & 16))
#define IGNORE_SRCREF   (!(flags & 32))

/* do the two objects compute as identical?
   Also used in unique.c */
Rboolean
R_compute_identical(SEXP x, SEXP y, int flags)
{
    SEXP ax, ay, atrx, atry;
    if(x == y) /* same pointer */
	return TRUE;
    if(TYPEOF(x) != TYPEOF(y) ||
       OBJECT(x) != OBJECT(y) ||
       IS_S4_OBJECT(x) != IS_S4_OBJECT(y))
	return FALSE;

    /* Skip attribute checks for CHARSXP
       -- such attributes are used for the cache.  */
    if(TYPEOF(x) == CHARSXP) {
	/* This matches NAs */
	return Seql(x, y);
    }
    if (IGNORE_SRCREF && TYPEOF(x) == CLOSXP) {
	/* Remove "srcref" attribute - and below, treat body(x), body(y) */
	SEXP x_ = PROTECT(duplicate(x)), y_ = PROTECT(duplicate(y));
	setAttrib(x_, R_SrcrefSymbol, R_NilValue);
	setAttrib(y_, R_SrcrefSymbol, R_NilValue);
	ax = ATTRIB(x_); ay = ATTRIB(y_);
	UNPROTECT(2);
    }
    else {
	ax = ATTRIB(x); ay = ATTRIB(y);
    }
    if (!ATTR_AS_SET) {
	PROTECT(ax);
	PROTECT(ay);
	Rboolean idattr = R_compute_identical(ax, ay, flags);
	UNPROTECT(2);
	if(! idattr) return FALSE;
    }
    /* Attributes are special: they should be tagged pairlists.  We
       don't test them if they are not, and we do not test the order
       if they are.

       This code is not very efficient, but then neither is using
       pairlists for attributes.  If long attribute lists become more
       common (and they are used for S4 slots) we should store them in
       a hash table.
    */
    else if(ax != R_NilValue || ay != R_NilValue) {
	if(ax == R_NilValue || ay == R_NilValue)
	    return FALSE;
	if(TYPEOF(ax) != LISTSXP || TYPEOF(ay) != LISTSXP) {
	    warning(_("ignoring non-pairlist attributes"));
	} else {
	    SEXP elx, ely;
	    if(length(ax) != length(ay)) return FALSE;
	    /* They are the same length and should have
	       unique non-empty non-NA tags */
	    /* ax, ay might be fresh allocations from duplicating into
	       x_, y_) above, so need to be protected from possible
	       allocations in getAttrib and recursive calls to
	       R_compute_identical in the loop. */
	    PROTECT(ax);
	    PROTECT(ay);
	    for(elx = ax; elx != R_NilValue; elx = CDR(elx)) {
		const char *tx = CHAR(PRINTNAME(TAG(elx)));
		for(ely = ay; ely != R_NilValue; ely = CDR(ely))
		    if(streql(tx, CHAR(PRINTNAME(TAG(ely))))) {
			/* We need to treat row.names specially here */
			if(streql(tx, "row.names")) {
			    PROTECT(atrx = getAttrib(x, R_RowNamesSymbol));
			    PROTECT(atry = getAttrib(y, R_RowNamesSymbol));
			    if(!R_compute_identical(atrx, atry, flags)) {
				UNPROTECT(4); /* atrx, atry, ax, ay */
				return FALSE;
			    } else
				UNPROTECT(2); /* atrx, atry */
			} else
			    if(!R_compute_identical(CAR(elx), CAR(ely),
						    flags)) {
				UNPROTECT(2); /* ax, ay */
				return FALSE;
			    }
			break;
		    }
		if(ely == R_NilValue) {
		    UNPROTECT(2); /* ax, ay */
		    return FALSE;
		}
	    }
	    UNPROTECT(2); /* ax, ay */    
	}
    }
    switch (TYPEOF(x)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)LOGICAL(x), (void *)LOGICAL(y),
		      xlength(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case INTSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)INTEGER(x), (void *)INTEGER(y),
		      xlength(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case REALSXP:
    {
	R_xlen_t n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	else {
	    double *xp = REAL(x), *yp = REAL(y);
	    int ne_strict = NUM_EQ | (SINGLE_NA << 1);
	    for(R_xlen_t i = 0; i < n; i++)
		if(neWithNaN(xp[i], yp[i], ne_strict)) return FALSE;
	}
	return TRUE;
    }
    case CPLXSXP:
    {
	R_xlen_t n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	else {
	    Rcomplex *xp = COMPLEX(x), *yp = COMPLEX(y);
	    int ne_strict = NUM_EQ | (SINGLE_NA << 1);
	    for(R_xlen_t i = 0; i < n; i++)
		if(neWithNaN(xp[i].r, yp[i].r, ne_strict) ||
		   neWithNaN(xp[i].i, yp[i].i, ne_strict))
		    return FALSE;
	}
	return TRUE;
    }
    case STRSXP:
    {
	R_xlen_t i, n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	for(i = 0; i < n; i++) {
	    /* This special-casing for NAs is not needed */
	    Rboolean na1 = (STRING_ELT(x, i) == NA_STRING),
		na2 = (STRING_ELT(y, i) == NA_STRING);
	    if(na1 ^ na2) return FALSE;
	    if(na1 && na2) continue;
	    if (! Seql(STRING_ELT(x, i), STRING_ELT(y, i))) return FALSE;
	}
	return TRUE;
    }
    case CHARSXP: /* Probably unreachable, but better safe than sorry... */
    {
	/* This matches NAs */
	return Seql(x, y);
    }
    case VECSXP:
    case EXPRSXP:
    {
	R_xlen_t i, n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(!R_compute_identical(VECTOR_ELT(x, i),VECTOR_ELT(y, i), flags))
		return FALSE;
	return TRUE;
    }
    case LANGSXP:
    case LISTSXP:
    {
	while (x != R_NilValue) {
	    if(y == R_NilValue)
		return FALSE;
	    if(!R_compute_identical(CAR(x), CAR(y), flags))
		return FALSE;
	    if(!R_compute_identical(PRINTNAME(TAG(x)), PRINTNAME(TAG(y)), flags))
		return FALSE;
	    x = CDR(x);
	    y = CDR(y);
	}
	return(y == R_NilValue);
    }
    case CLOSXP:
    {
	if(!R_compute_identical(FORMALS(x), FORMALS(y), flags)) return FALSE;
	if(IGNORE_BYTECODE) {
	    if(IGNORE_SRCREF) {
		SEXP x_ = PROTECT(R_body_no_src(x)),
		     y_ = PROTECT(R_body_no_src(y));
		Rboolean id_body = R_compute_identical(x_, y_, flags);
		UNPROTECT(2);
		if(!id_body) return FALSE;
	    } else if(!R_compute_identical(BODY_EXPR(x), BODY_EXPR(y), flags))
		return FALSE;
	} else { // !IGNORE_BYTECODE: use byte code for comparison of function bodies :
	    if(!R_compute_identical(BODY(x), BODY(y), flags)) return FALSE;
	}
	// now, formals and body are equal, check the enviroment(.)s:
	return (IGNORE_ENV || CLOENV(x) == CLOENV(y) ? TRUE : FALSE);
    }
    case SPECIALSXP:
    case BUILTINSXP:
	return(PRIMOFFSET(x) == PRIMOFFSET(y) ? TRUE : FALSE);
    case ENVSXP:
    case SYMSXP:
    case WEAKREFSXP: /**** is this the best approach? */
	return(x == y ? TRUE : FALSE);
    case BCODESXP:
	return R_compute_identical(BCODE_CODE(x), BCODE_CODE(y), flags) &&
	       R_compute_identical(BCODE_EXPR(x), BCODE_EXPR(y), flags) &&
	       R_compute_identical(BCODE_CONSTS(x), BCODE_CONSTS(y), flags);
    case EXTPTRSXP:
	return (EXTPTR_PTR(x) == EXTPTR_PTR(y) ? TRUE : FALSE);
    case RAWSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)RAW(x), (void *)RAW(y),
		      XLENGTH(x) * sizeof(Rbyte)) == 0 ? TRUE : FALSE;

/*  case PROMSXP: args are evaluated, so will not be seen */
	/* test for equality of the substituted expression -- or should
	   we require both expression and environment to be identical? */
	/*#define PREXPR(x)	((x)->u.promsxp.expr)
	  #define PRENV(x)	((x)->u.promsxp.env)
	  return(R_compute_identical(subsititute(PREXPR(x), PRENV(x),
				     flags),
	  subsititute(PREXPR(y), PRENV(y))));*/
    case S4SXP:
	/* attributes already tested, so all slots identical */
	return TRUE;
    default:
	/* these are all supposed to be types that represent constant
	   entities, so no further testing required ?? */
	printf("Unknown Type: %s (%x)\n", type2char(TYPEOF(x)), TYPEOF(x));
	return TRUE;
    }
}


/**
 * [N]ot [E]qual  (x, y)   <==>   x  "!="  y
 *  where the NA/NaN and "-0." / "+0." cases treatment depend on 'str'.
 *
 * @param x
 * @param y  the two "number"s to be compared
 * @param str a "strictness" indicator, one of 2*2 (one|bit)_NA__num_(eq|bit)
 *  "SINGLE_NA" means: x and y differ in the case
 *    that one, but not both are NaN.  Two NaN values are judged
 *    identical for this purpose, but NA != NaN
 *
 *  "NUM_EQ" means: (x != y) is used when both are not NA or NaN
 *  whereas "bit_NA" and "num_bit" use the bitwise memory comparison  memcmp();
 *  notably "*_num_bit" will differentiate '+0.' and '-0.'.
 *
 * @return FALSE or TRUE indicating if x or y differ
 */
static Rboolean neWithNaN(double x, double y, ne_strictness_type str)
{
    switch (str) {
    case single_NA__num_eq:
    case single_NA__num_bit:
	if(R_IsNA(x))
	    return(R_IsNA(y) ? FALSE : TRUE);
	if(R_IsNA(y))
	    return(R_IsNA(x) ? FALSE : TRUE);
	if(ISNAN(x))
	    return(ISNAN(y) ? FALSE : TRUE);

    case bit_NA__num_eq:
    case bit_NA__num_bit:
	; /* do nothing */
    }

    switch (str) {
    case single_NA__num_eq:
	return(x != y);
    case bit_NA__num_eq:
	if(!ISNAN(x) && !ISNAN(y))
	    return(x != y);
	else /* bitwise check for NA/NaN's */
	    return memcmp((const void *) &x,
			  (const void *) &y, sizeof(double)) ? TRUE : FALSE;
    case bit_NA__num_bit:
    case single_NA__num_bit:
	return memcmp((const void *) &x,
		      (const void *) &y, sizeof(double)) ? TRUE : FALSE;
    default: /* Wall */
	return FALSE;
    }
}
