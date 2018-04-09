/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2012  The R Core Team.
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
# include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#define R_USE_SIGNALS 1
#define NEED_SGGC_FUNCTIONS
#include "Defn.h"
#include <R_ext/Callbacks.h>

#include <helpers/helpers-app.h>

static SEXP NAMESPACE_name = R_NoObject;  /* Initialized when first needed */
static SEXP spec_name = R_NoObject;

/*----------------------------------------------------------------------

  get environment from a subclass if possible; else return NULL. */

#define simple_as_environment(arg) \
  (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) \
                                               : R_NilValue)
	    

/*----------------------------------------------------------------------
  do_assign : .Internal(assign(x, value, envir, inherits)) */

static SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name=R_NilValue, val, aenv;
    int ginherits = 0;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error(_("invalid first argument"));
    else {
	if (length(CAR(args)) > 1)
	    warning(_("only the first element is used as variable name"));
	name = install(translateChar(STRING_ELT(CAR(args), 0)));
    }
    PROTECT(val = CADR(args));
    aenv = CADDR(args);
    if (TYPEOF(aenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(aenv) != ENVSXP &&
	TYPEOF((aenv = simple_as_environment(aenv))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");
    if (ginherits)
	set_var_nonlocal (name, val, aenv, 3);
    else
	set_var_in_frame (name, val, aenv, TRUE, 3);
    UNPROTECT(1);
    return val;
}


/* do_list2env : .Internal(list2env(x, envir)) */

static SEXP do_list2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xnms, envir;
    int n;
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != VECSXP)
	error(_("first argument must be a named list"));
    x = CAR(args);
    n = LENGTH(x);
    xnms = getAttrib(x, R_NamesSymbol);
    if (TYPEOF(xnms) != STRSXP || LENGTH(xnms) != n)
	error(_("names(x) must be a character vector of the same length as x"));
    envir = CADR(args);
    if (TYPEOF(envir) != ENVSXP)
	error(_("'envir' argument must be an environment"));

    for(int i = 0; i < LENGTH(x) ; i++) {
	SEXP name = install(translateChar(STRING_ELT(xnms, i)));
	defineVar(name, VECTOR_ELT(x, i), envir);
    }

    return envir;
}

/*----------------------------------------------------------------------
  do_remove

  There are three arguments to do_remove; a list of names to remove,
  an optional environment (if missing set it to R_GlobalEnv) and
  inherits, a logical indicating whether to look in the parent env if
  a symbol is not found in the supplied env.  This is ignored if
  environment is not specified. */

static SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* .Internal(remove(list, envir, inherits)) */

    SEXP name, envarg, tsym, tenv, value;
    int ginherits = 0;
    int i;

    checkArity(op, args);

    name = CAR(args);
    if (!isString(name))
	error(_("invalid first argument"));
    args = CDR(args);

    envarg = CAR(args);
    if (TYPEOF(envarg) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(envarg) != ENVSXP &&
	TYPEOF((envarg = simple_as_environment(envarg))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    args = CDR(args);

    ginherits = asLogical(CAR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    for (i = 0; i < LENGTH(name); i++) {
	value = R_NoObject;
	tsym = install(translateChar(STRING_ELT(name, i)));
	tenv = envarg;
	while (tenv != R_EmptyEnv) {
	    value = RemoveVariable(tsym, tenv);
	    if (value != R_NoObject || !ginherits)
		break;
	    tenv = CDR(tenv);
	}
	if (value == R_NoObject)
	    warning (_("object '%s' not found"), CHAR(PRINTNAME(tsym)));
        else
            DEC_NAMEDCNT_AND_PRVALUE(value);
    }
    return R_NilValue;
}

/*----------------------------------------------------------------------
 do_get_rm - get value of variable and then remove the variable, decrementing
             NAMEDCNT when possible. If return of pending value is allowed, will
             pass on pending value in the variable without waiting for it. */

static SEXP do_get_rm (SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP name, value;

    checkArity(op, args);
    check1arg(args, call, "x");

    name = CAR(args);
    if (TYPEOF(name) != SYMSXP)
        error(_("invalid argument"));

    value = RemoveVariable (name, rho);

    if (value == R_NoObject)
        unbound_var_error(name);

    if (TYPEOF(value) == PROMSXP) {
        SEXP prvalue = forcePromise(value);
        DEC_NAMEDCNT_AND_PRVALUE(value);
        value = prvalue;
    }
    else
        DEC_NAMEDCNT(value);

    if (variant & VARIANT_NULL)
        return R_NilValue;

    if ( ! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_COMPUTED(value);

    return value;
}

/*----------------------------------------------------------------------
  do_get

  This function returns the SEXP associated with the character
  argument.  It needs the environment of the calling function as a
  default.

      get(x, envir, mode, inherits)
      exists(x, envir, mode, inherits)
*/

static SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, genv, t1 = R_NilValue;
    SEXPTYPE gmode;
    int ginherits = 0, where;
    checkArity(op, args);

    /* The first arg is the object name */
    /* It must be present and a non-empty string */

    if (!isValidStringF(CAR(args)))
	error(_("invalid first argument"));
    else
	t1 = install(translateChar(STRING_ELT(CAR(args), 0)));

    /* envir :	originally, the "where=" argument */

    if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
	where = asInteger(CADR(args));
	genv = R_sysframe(where, R_GlobalContext);
    }
    else if (TYPEOF(CADR(args)) == NILSXP)
	error(_("use of NULL environment is defunct"));
    else if (TYPEOF(CADR(args)) == ENVSXP)
	genv = CADR(args);
    else if(TYPEOF((genv = simple_as_environment(CADR(args)))) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");

    /* mode :  The mode of the object being sought */

    /* as from R 1.2.0, this is the *mode*, not the *typeof* aka
       storage.mode.
    */

    if (isString(CADDR(args))) {
	if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), 0)), "function")) /*ASCII*/
	    gmode = FUNSXP;
	else
	    gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), 0))); /* ASCII */
    } 
    else
	error(_("invalid '%s' argument"), "mode");

    ginherits = asLogical(CADDDR(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    /* Search for the object */
    rval = findVar1mode(t1, genv, gmode, ginherits, PRIMVAL(op));

    if (PRIMVAL(op)) { /* have get(.) */
	if (rval == R_MissingArg)
            arg_missing_error(t1);
	if (rval == R_UnboundValue) {
	    if (gmode == ANYSXP)
                unbound_var_error(t1);
	    else
		error(_("object '%s' of mode '%s' was not found"),
		      CHAR(PRINTNAME(t1)),
		      CHAR(STRING_ELT(CAR(CDDR(args)), 0))); /* ASCII */
	}

	/* We need to evaluate if it is a promise */
	if (TYPEOF(rval) == PROMSXP)
	    rval = forcePromise(rval);

	SET_NAMEDCNT_NOT_0(rval);
	return rval;
    }
    else /* exists(.) */
	return ScalarLogicalMaybeConst (rval != R_UnboundValue);
}

static SEXP gfind(const char *name, SEXP env, SEXPTYPE mode,
		  SEXP ifnotfound, int inherits, SEXP enclos)
{
    SEXP rval, t1, R_fcall, var;

    t1 = install(name);

    /* Search for the object - last arg is 1 to 'get' */
    rval = findVar1mode(t1, env, mode, inherits, 1);

    if (rval == R_UnboundValue) {
	if( isFunction(ifnotfound) ) {
	    PROTECT(var = mkString(name));
	    PROTECT(R_fcall = LCONS(ifnotfound, CONS(var, R_NilValue)));
	    rval = eval(R_fcall, enclos);
	    UNPROTECT(2);
	} else
	    rval = ifnotfound;
    }

    /* We need to evaluate if it is a promise */
    if (TYPEOF(rval) == PROMSXP) rval = forcePromise(rval);
    SET_NAMEDCNT_NOT_0(rval);
    return rval;
}

/** mget(): get multiple values from an environment
 *
 * .Internal(mget(x, envir, mode, ifnotfound, inherits))
 *
 * returns a list of the same length as x, a character vector (of names). */

static SEXP do_mget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, x, mode, ifnotfound, ifnfnd;
    SEXPTYPE gmode; /* is unsigned int */
    int ginherits = 0, nvals, nmode, nifnfnd, i;

    checkArity(op, args);

    x = CAR(args);

    nvals = length(x);

    /* The first arg is the object name */
    /* It must be present and a string */
    if (!isString(x) )
	error(_("invalid first argument"));
    for(i = 0; i < nvals; i++)
	if( isNull(STRING_ELT(x, i)) || !CHAR(STRING_ELT(x, 0))[0] )
	    error(_("invalid name in position %d"), i+1);

    /* FIXME: should we install them all?) */

    env = CADR(args);
    if (env == R_NilValue) {
	error(_("use of NULL environment is defunct"));
    } else if( !isEnvironment(env) )
	error(_("second argument must be an environment"));

    mode = CADDR(args);
    nmode = length(mode);
    if( !isString(mode) )
	error(_("invalid '%s' argument"), "mode");

    if( nmode != nvals && nmode != 1 )
	error(_("wrong length for '%s' argument"), "mode");

    PROTECT(ifnotfound = coerceVector(CADDDR(args), VECSXP));
    nifnfnd = length(ifnotfound);
    if( !isVector(ifnotfound) )
	error(_("invalid '%s' argument"), "ifnotfound");

    if( nifnfnd != nvals && nifnfnd != 1 )
	error(_("wrong length for '%s' argument"), "ifnotfound");

    ginherits = asLogical(CAD4R(args));
    if (ginherits == NA_LOGICAL)
	error(_("invalid '%s' argument"), "inherits");

    PROTECT(ans = allocVector(VECSXP, nvals));

    /* now for each element of x, we look for it, using the inherits,
       etc */

    for(i = 0; i < nvals; i++) {
	if (isString(mode)) { /* ASCII */
	    if (!strcmp(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode )), "function"))
		gmode = FUNSXP;
	    else
		gmode = str2type(CHAR(STRING_ELT(CAR(CDDR(args)), i % nmode )));
	} 
        else
	    error(_("invalid '%s' argument"), "mode");

	/* is the mode provided one of the real modes? */
	if( gmode == (SEXPTYPE) (-1))
	    error(_("invalid '%s' argument"), "mode");


	if( TYPEOF(ifnotfound) != VECSXP )
	    error(_("invalid '%s' argument"), "ifnotfound");
	if( nifnfnd == 1 ) /* length has been checked to be 1 or nvals. */
	    ifnfnd = VECTOR_ELT(ifnotfound, 0);
	else
	    ifnfnd = VECTOR_ELT(ifnotfound, i);

        SET_VECTOR_ELEMENT_TO_VALUE (ans, i, 
          gfind (translateChar(STRING_ELT(x,i % nvals)), 
                 env, gmode, ifnfnd, ginherits, rho));
    }

    setAttrib(ans, R_NamesSymbol, duplicate(x));
    UNPROTECT(2);
    return(ans);
}

/* R_isMissing is called on the not-yet-evaluated (or sometimes evaluated)
   value of an argument, if this is a symbol, as it could be a missing 
   argument that has been passed down.  So 'symbol' is the promise value, 
   and 'rho' its evaluation argument.

   It is called in do_missing and in evalList_v.

   Return 0 if not missing, 1 if missing from empty arg, 2 if missing from "_".
   Note that R_isMissing pays no attention to the MISSING field, only to
   whether things are R_MissingArg or R_MissingUnder.

   Cycles in promises checked are detected by looking at each previous one.
   This takes quadratic time, but the number of promises looked at should
   normally be very small.
*/

struct detectcycle { struct detectcycle *next; SEXP prom; };

static int isMissing_recursive (SEXP, SEXP, struct detectcycle *);

int attribute_hidden R_isMissing(SEXP symbol, SEXP rho)
{
    return isMissing_recursive (symbol, rho, NULL);
}

static int isMissing_recursive(SEXP symbol, SEXP rho, struct detectcycle *dc)
{
    int ddv=0;
    SEXP vl, s;

    if (symbol == R_MissingArg)
	return 1;
    if (symbol == R_MissingUnder)
	return 2;

    if (DDVAL(symbol)) {
	s = R_DotsSymbol;
	ddv = ddVal(symbol);
    }
    else
	s = symbol;

    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return 0;

    vl = Rf_find_binding_in_frame(rho, s, NULL);
    if (vl != R_NilValue) {
        SEXP vlv = CAR(vl);
	if (DDVAL(symbol)) {
            if (vlv == R_MissingUnder)
                return 2;
	    if (vlv == R_UnboundValue || vlv == R_MissingArg || length(vlv)<ddv)
		return 1;
            vl = nthcdr(vlv, ddv-1);
            vlv = CAR(vl);
	}
	if (vlv==R_MissingArg)
	    return 1;
        if (vlv==R_MissingUnder)
            return 2;
	if (IS_ACTIVE_BINDING(vl))
	    return 0;
	if (TYPEOF(vlv)==PROMSXP && TYPEOF(PREXPR(vlv))==SYMSXP
             && (PRVALUE(vlv)==R_UnboundValue || PRVALUE(vlv)==R_MissingArg)) {
            for (struct detectcycle *p = dc; p != NULL; p = p->next) {
                if (p->prom == vlv) {
                    return 1;
                }
            }
            struct detectcycle dc2;
            dc2.next = dc;
            dc2.prom = vlv;
            int val;
            PROTECT(vl);
            R_CHECKSTACK();
            val = isMissing_recursive(PREXPR(vlv), PRENV(vlv), &dc2);
            UNPROTECT(1); /* vl */
            return val;
	}
	else
	    return 0;
    }
    return 0;
}


/*----------------------------------------------------------------------

  do_missing and do_missing_from_underline

  This function tests whether the symbol passed as its first argument
  is a missing argument to the current closure.  rho is the
  environment that missing was called from.

  Note that an argument with a default value is considered missing
  if the default was used, but this is NOT applied recursively to 
  arguments that are arguments in the calling function that were
  filled in from the default value.

  These are primitive and SPECIALSXP */

static SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP t, sym, s;
    int under = PRIMVAL(op);
    int ddv = 0;

    checkArity(op, args);
    check1arg_x (args, call);

    sym = CAR(args);
    if (isString(sym) && length(sym)==1)
	sym = install(translateChar(STRING_ELT(CAR(args), 0)));
    if (!isSymbol(sym))
	errorcall(call, _("invalid use of 'missing'"));

    if (DDVAL(sym)) {
	ddv = ddVal(sym);
	s = R_DotsSymbol;
    }
    else
        s = sym;

    t = Rf_find_binding_in_frame (rho, s, NULL);

    if (t == R_NilValue)  /* no error for local variables, despite msg below */
	errorcall(call, _("'missing' can only be used for arguments"));

    if (DDVAL(sym)) {
        if (CAR(t) == R_MissingUnder
             || !under && (CAR(t) == R_UnboundValue || CAR(t) == R_MissingArg
                                                    || length(CAR(t)) < ddv))
            goto true;
        t = nthcdr(CAR(t), ddv-1);
    }

    if (CAR(t) == R_MissingUnder
         || !under && (MISSING(t) || CAR(t) == R_MissingArg))
        goto true;

    t = CAR(t);
    if (TYPEOF(t)==PROMSXP && isSymbol(PREXPR(t))) { 
        PROTECT(t);
        int m = R_isMissing(PREXPR(t),PRENV(t));
        UNPROTECT(1);
        if (m == 2 || !under && m)
            goto true;
    }

    return ScalarLogicalMaybeConst(FALSE);

  true:
    return ScalarLogicalMaybeConst(TRUE);
}

/*----------------------------------------------------------------------
  do_globalenv

  Returns the current global environment. */

static SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}

/*----------------------------------------------------------------------
  do_baseenv

  Returns the current base environment. */

static SEXP do_baseenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_BaseEnv;
}

/*----------------------------------------------------------------------
  do_emptyenv

  Returns the current empty environment. */

static SEXP do_emptyenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_EmptyEnv;
}

/*----------------------------------------------------------------------
  do_search

  Print out the current search path. */

static SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name, t;
    int i, n;

    checkArity(op, args);
    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));
    /* TODO - what should the name of this be? */
    SET_STRING_ELT(ans, 0, mkChar(".GlobalEnv"));
    SET_STRING_ELT(ans, n-1, mkChar("package:base"));
    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_BaseEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if (!isString(name) || length(name) < 1)
	    SET_STRING_ELT(ans, i, mkChar("(unknown)"));
	else
	    SET_STRING_ELT(ans, i, STRING_ELT(name, 0));
	i++;
    }
    UNPROTECT(1);
    return ans;
}

/*----------------------------------------------------------------------
  do_ls

  This code implements the functionality of the "ls" and "objects"
  functions.  [ ls(envir, all.names) ] */

static int FrameSize(SEXP frame, int all)
{
    int count = 0;

    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue)
	    count += 1;
	frame = CDR(frame);
    }
    return count;
}

static void FrameNames(SEXP frame, int all, SEXP names, int *indx)
{
    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SET_STRING_ELT(names, *indx, PRINTNAME(TAG(frame)));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
{
    while (frame != R_NilValue) {
	if ((all || CHAR(PRINTNAME(TAG(frame)))[0] != '.') &&
				      CAR(frame) != R_UnboundValue) {
	    SEXP value = CAR(frame);
	    if (TYPEOF(value) == PROMSXP)
		value = forcePromise(value);
	    SET_VECTOR_ELT(values, *indx, duplicate(value));
	    (*indx)++;
	}
	frame = CDR(frame);
    }
}

static int HashTableSize(SEXP table, int all)
{
    int count = 0;
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void HashTableNames(SEXP table, int all, SEXP names, int *indx)
{
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameNames(VECTOR_ELT(table, i), all, names, indx);
}

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

#define NOT_IN_SYMBOL_TABLE(s) \
    (s == R_MissingArg || s == R_MissingUnder || s == R_RestartToken)

static int BuiltinSize(int all, int intern)
{
    sggc_cptr_t nxt;
    int count = 0;

    for (nxt = sggc_first_uncollected_of_kind(SGGC_SYM_KIND);
         nxt != SGGC_NO_OBJECT;
         nxt = sggc_next_uncollected_of_kind(nxt)) {
        SEXP s = SEXP_FROM_CPTR(nxt);
        if (NOT_IN_SYMBOL_TABLE(s)) continue;
        if (intern) {
            if (INTERNAL(s) != R_NilValue)
                count++;
        }
        else {
            if ((all || CHAR(PRINTNAME(s))[0] != '.')
                && SYMVALUE(s) != R_UnboundValue)
                count++;
        }
    }
    return count;
}

static void BuiltinNames(int all, int intern, SEXP names, int *indx)
{
    sggc_cptr_t nxt;

    for (nxt = sggc_first_uncollected_of_kind(SGGC_SYM_KIND);
         nxt != SGGC_NO_OBJECT;
         nxt = sggc_next_uncollected_of_kind(nxt)) {
        SEXP s = SEXP_FROM_CPTR(nxt);
        if (NOT_IN_SYMBOL_TABLE(s)) continue;
        if (intern) {
            if (INTERNAL(s) != R_NilValue)
                SET_STRING_ELT(names, (*indx)++, PRINTNAME(s));
        }
        else {
            if ((all || CHAR(PRINTNAME(s))[0] != '.')
                && SYMVALUE(s) != R_UnboundValue)
                SET_STRING_ELT(names, (*indx)++, PRINTNAME(s));
        }
    }
}

static void BuiltinValues(int all, int intern, SEXP values, int *indx)
{
    sggc_cptr_t nxt;

    for (nxt = sggc_first_uncollected_of_kind(SGGC_SYM_KIND);
         nxt != SGGC_NO_OBJECT;
         nxt = sggc_next_uncollected_of_kind(nxt)) {
        SEXP s = SEXP_FROM_CPTR(nxt);
        if (NOT_IN_SYMBOL_TABLE(s)) continue;
        SEXP vl;
        if (intern) {
            if (INTERNAL(s) != R_NilValue) {
                vl = SYMVALUE(s);
                if (TYPEOF(vl) == PROMSXP)
                    vl = forcePromise(vl);
                SET_VECTOR_ELT(values, (*indx)++, duplicate(vl));
            }
        }
        else {
            if ((all || CHAR(PRINTNAME(s))[0] != '.')
                && SYMVALUE(s) != R_UnboundValue) {
                vl = SYMVALUE(s);
                if (TYPEOF(vl) == PROMSXP)
                    vl = forcePromise(vl);
                SET_VECTOR_ELT(values, (*indx)++, duplicate(vl));
            }
        }
    }
}

static SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env;
    int all;
    checkArity(op, args);

    if(IS_USER_DATABASE(CAR(args))) {
	R_ObjectTable *tb = (R_ObjectTable*)
	    R_ExternalPtrAddr(HASHTAB(CAR(args)));
	return(tb->objects(tb));
    }

    env = CAR(args);

    /* if (env == R_BaseNamespace) env = R_BaseEnv; */

    all = asLogical(CADR(args));
    if (all == NA_LOGICAL) all = 0;

    return R_lsInternal(env, all);
}

/* takes an environment and a boolean indicating whether to get all names */

SEXP R_lsInternal(SEXP env, Rboolean all)
{
    int  k;
    SEXP ans;


    /* Step 1 : Compute the Vector Size */
    k = 0;
    if (env == R_BaseEnv || env == R_BaseNamespace)
	k += BuiltinSize(all, 0);
    else if (isEnvironment(env) ||
	isEnvironment(env = simple_as_environment(env))) {
	if (HASHTAB(env) != R_NilValue)
	    k += HashTableSize(HASHTAB(env), all);
	else
	    k += FrameSize(FRAME(env), all);
    }
    else
	error(_("invalid '%s' argument"), "envir");

    /* Step 2 : Allocate and Fill the Result */
    PROTECT(ans = allocVector(STRSXP, k));
    k = 0;

    if (IS_BASE(env))
        BuiltinNames(all, 0, ans, &k);
    else if (HASHTAB(env) != R_NilValue)
        HashTableNames(HASHTAB(env), all, ans, &k);
    else
        FrameNames(FRAME(env), all, ans, &k);

    sortVector(ans, FALSE);
    UNPROTECT(1);
    return ans;
}

/* transform an environment into a named list */

static SEXP do_env2list(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, names;
    int k, all;

    checkArity(op, args);

    env = CAR(args);
    if (env == R_NilValue)
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) ) {
        SEXP xdata;
	if( IS_S4_OBJECT(env) && TYPEOF(env) == S4SXP &&
	    (xdata = R_getS4DataSlot(env, ENVSXP)) != R_NilValue)
	    env = xdata;
	else
	    error(_("argument must be an environment"));
    }

    all = asLogical(CADR(args)); /* all.names = TRUE/FALSE */
    if (all == NA_LOGICAL) all = 0;

    if (IS_BASE(env))
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(names = allocVector(STRSXP, k));
    PROTECT(ans = allocVector(VECSXP, k));

    k = 0;
    if (IS_BASE(env))
	BuiltinValues(all, 0, ans, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, ans, &k);
    else
	FrameValues(FRAME(env), all, ans, &k);

    k = 0;
    if (IS_BASE(env))
	BuiltinNames(all, 0, names, &k);
    else if (HASHTAB(env) != R_NilValue)
	HashTableNames(HASHTAB(env), all, names, &k);
    else
	FrameNames(FRAME(env), all, names, &k);

    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return(ans);
}

/* apply a function to all objects in an environment and return the
   results in a list.
   Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)

   This is a special .Internal */

static SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, ans, R_fcall, FUN, tmp2, End;
    int i, k, k2;
    int all, useNms, no_dots;

    checkArity(op, args);

    PROTECT(env = eval(CAR(args), rho));
    if (env == R_NilValue)
	error(_("use of NULL environment is defunct"));
    if( !isEnvironment(env) )
	error(_("argument must be an environment"));

    FUN = CADR(args);
    if (!isSymbol(FUN))
	error(_("arguments must be symbolic"));

    SEXP dotsv = findVarInFrame3 (rho, R_DotsSymbol, 3);
    no_dots = dotsv==R_MissingArg || dotsv==R_NilValue || dotsv==R_UnboundValue;

    /* 'all.names' : */
    all = asLogical(eval(CADDR(args), rho));
    if (all == NA_LOGICAL) all = 0;

    /* 'USE.NAMES' : */
    useNms = asLogical(eval(CADDDR(args), rho));
    if (useNms == NA_LOGICAL) useNms = 0;

    if (IS_BASE(env))
	k = BuiltinSize(all, 0);
    else if (HASHTAB(env) != R_NilValue)
	k = HashTableSize(HASHTAB(env), all);
    else
	k = FrameSize(FRAME(env), all);

    PROTECT(ans  = allocVector(VECSXP, k));
    PROTECT(tmp2 = allocVector(VECSXP, k));

    k2 = 0;
    if (IS_BASE(env))
	BuiltinValues(all, 0, tmp2, &k2);
    else if (HASHTAB(env) != R_NilValue)
	HashTableValues(HASHTAB(env), all, tmp2, &k2);
    else
	FrameValues(FRAME(env), all, tmp2, &k2);

    /* fcall :=  <FUN>( `[`(<elist>, i), tmp, ... ), with ... maybe omitted 

       Don't try to reuse the cell holding the index - causes problems. */

    PROTECT(End = no_dots ? R_NilValue : CONS(R_DotsSymbol,R_NilValue));

    for(i = 0; i < k2; i++) {
        PROTECT(R_fcall = LCONS(FUN, 
                            CONS(LCONS(R_Bracket2Symbol,
                                     CONS(tmp2, 
                                        CONS(ScalarInteger(i+1), R_NilValue))),
                                 End)));
        SET_VECTOR_ELEMENT_TO_VALUE (ans, i, eval(R_fcall, rho));
        UNPROTECT(1);
    }

    if (useNms) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, k));
	k = 0;
	if (IS_BASE(env))
	    BuiltinNames(all, 0, names, &k);
	else if(HASHTAB(env) != R_NilValue)
	    HashTableNames(HASHTAB(env), all, names, &k);
	else
	    FrameNames(FRAME(env), all, names, &k);

	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(4);
    return(ans);
}

int envlength(SEXP rho)
{
    if( HASHTAB(rho) != R_NilValue)
	return HashTableSize(HASHTAB(rho), 1);
    else
	return FrameSize(FRAME(rho), 1);
}

/*----------------------------------------------------------------------
  do_builtins

  Return the names of all the built in functions.  These are fetched
  directly from the symbol table. */

static SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int intern, nelts;
    checkArity(op, args);
    intern = asLogical(CAR(args));
    if (intern == NA_INTEGER) intern = 0;
    nelts = BuiltinSize(1, intern);
    ans = allocVector(STRSXP, nelts);
    nelts = 0;
    BuiltinNames(1, intern, ans, &nelts);
    sortVector(ans, TRUE);
    return ans;
}


/*----------------------------------------------------------------------
  do_pos2env

  This function returns the environment at a specified position in the
  search path or the environment of the caller of
  pos.to.env (? but pos.to.env is usually used in arg lists and hence
  is evaluated in the calling environment so this is one higher).

  When pos = -1 the environment of the closure that pos2env is
  evaluated in is obtained. Note: this relies on pos.to.env being
  a primitive. */

static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;
    RCNTXT *cptr;

    if (pos == NA_INTEGER || pos < -1 || pos == 0)
	errorcall(call, _("invalid '%s' argument"), "pos");
    else if (pos == -1) {
	/* make sure the context is a funcall */
	cptr = R_GlobalContext;
	while( !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext
	       != NULL )
	    cptr = cptr->nextcontext;
	if( !(cptr->callflag & CTXT_FUNCTION) )
	    errorcall(call, _("no enclosing environment"));

	env = cptr->sysparent;
	if (R_GlobalEnv != R_NilValue && env == R_NilValue)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    else {
	for (env = R_GlobalEnv; env != R_EmptyEnv && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if (pos != 1)
	    errorcall(call, _("invalid '%s' argument"), "pos");
    }
    return env;
}

/* this is primitive */
static SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int i, npos;
    checkArity(op, args);
    check1arg_x (args, call);

    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
	errorcall(call, _("invalid '%s' argument"), "pos");
    PROTECT(env = allocVector(VECSXP, npos));
    for (i = 0; i < npos; i++) {
	SET_VECTOR_ELT(env, i, pos2env(INTEGER(pos)[i], call));
    }
    if (npos == 1) env = VECTOR_ELT(env, 0);
    UNPROTECT(2);
    return env;
}

static SEXP matchEnvir(SEXP call, const char *what)
{
    SEXP t, name;
    if(!strcmp(".GlobalEnv", what))
	return R_GlobalEnv;
    if(!strcmp("package:base", what))
	return R_BaseEnv;
    for (t = ENCLOS(R_GlobalEnv); t != R_EmptyEnv ; t = ENCLOS(t)) {
	name = getAttrib(t, R_NameSymbol);
	if(isString(name) && length(name) > 0 &&
	   !strcmp(translateChar(STRING_ELT(name, 0)), what))
	    return t;
    }
    errorcall(call, _("no item called \"%s\" on the search list"), what);
    return R_NilValue;
}

/* This is primitive */
static SEXP do_as_environment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = CAR(args), ans;
    checkArity(op, args);
    check1arg(args, call, "object");
    if(isEnvironment(arg))
	return arg;
    if(isObject(arg) &&
       DispatchOrEval(call, op, "as.environment", args, rho, &ans, 0, 1))
	return ans;
    switch(TYPEOF(arg)) {
    case STRSXP:
	return matchEnvir(call, translateChar(asChar(arg)));
    case REALSXP:
    case INTSXP:
	return do_pos2env(call, op, args, rho);
    case NILSXP:
	errorcall(call,_("using 'as.environment(NULL)' is defunct"));
    case S4SXP: {
	/* dispatch was tried above already */
	SEXP dot_xData = R_getS4DataSlot(arg, ENVSXP);
	if(!isEnvironment(dot_xData))
	    errorcall(call, _("S4 object does not extend class \"environment\""));
	else
	    return(dot_xData);
    }
    case VECSXP: {
	/* implement as.environment.list() {isObject(.) is false for a list} */
	SEXP call, val;
	PROTECT(call = lang4(install("list2env"), arg,
			     /* envir = */R_NilValue,
			     /* parent = */R_EmptyEnv));
	val = eval(call, rho);
	UNPROTECT(1);
	return val;
    }
    default:
	errorcall(call, _("invalid object for 'as.environment'"));
    }
}

void R_LockEnvironment(SEXP env, Rboolean bindings)
{
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */

    if (TYPEOF(env) != ENVSXP)
	error(_("not an environment"));

    if (IS_BASE(env)) {
	if (bindings) {
            sggc_cptr_t nxt;
            for (nxt = sggc_first_uncollected_of_kind(SGGC_SYM_KIND);
                 nxt != SGGC_NO_OBJECT;
                 nxt = sggc_next_uncollected_of_kind(nxt)) {
                SEXP s = SEXP_FROM_CPTR(nxt);
                if (NOT_IN_SYMBOL_TABLE(s)) continue;
                if (SYMVALUE(s) != R_UnboundValue)
                    LOCK_BINDING(s);
            }
	}
#ifdef NOT_YET
	/* causes problems with Matrix */
	LOCK_FRAME(env);
#endif
	return;
    }

    if (bindings) {
	if (IS_HASHED(env)) {
	    SEXP table, chain;
	    int i, size;
	    table = HASHTAB(env);
	    size = HASHLEN(env);
	    for (i = 0; i < size; i++)
		for (chain = VECTOR_ELT(table, i);
		     chain != R_NilValue;
		     chain = CDR(chain))
		    LOCK_BINDING(chain);
	}
	else {
	    SEXP frame;
	    for (frame = FRAME(env); frame != R_NilValue; frame = CDR(frame))
		LOCK_BINDING(frame);
	}
    }
    LOCK_FRAME(env);
    set_symbits_in_env(env);
}

Rboolean R_EnvironmentIsLocked(SEXP env)
{
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    return FRAME_IS_LOCKED(env) != 0;
}

static SEXP do_lockEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP frame;
    Rboolean bindings;
    checkArity(op, args);
    frame = CAR(args);
    bindings = asLogical(CADR(args));
    R_LockEnvironment(frame, bindings);
    return R_NilValue;
}

static SEXP do_envIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogicalMaybeConst(R_EnvironmentIsLocked(CAR(args)));
}

void R_LockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (IS_BASE(env))
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	LOCK_BINDING(sym);
    else {
	SEXP binding = Rf_find_binding_in_frame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	LOCK_BINDING(binding);
    }
}

void R_unLockBinding(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (IS_BASE(env))
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	UNLOCK_BINDING(sym);
    else {
	SEXP binding = Rf_find_binding_in_frame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	UNLOCK_BINDING(binding);
    }
}

void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (! isFunction(fun))
	error(_("not a function"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (IS_BASE(env)) {
	if (SYMVALUE(sym) != R_UnboundValue && ! IS_ACTIVE_BINDING(sym))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(sym))
	    error(_("cannot change active binding if binding is locked"));
	SET_SYMVALUE(sym, fun);
	SET_ACTIVE_BINDING_BIT(sym);
	/* we don't need to worry about the global cache here as
	   a regular binding cannot be changed */
    }
    else {
	SEXP binding = Rf_find_binding_in_frame(env, sym, NULL);
	if (binding == R_NilValue) {
	    defineVar(sym, fun, env); /* fails if env is locked */
	    binding = Rf_find_binding_in_frame(env, sym, NULL);
	    SET_ACTIVE_BINDING_BIT(binding);
	}
	else if (! IS_ACTIVE_BINDING(binding))
	    error(_("symbol already has a regular binding"));
	else if (BINDING_IS_LOCKED(binding))
	    error(_("cannot change active binding if binding is locked"));
	else
	    SETCAR(binding, fun);
    }
}

Rboolean R_BindingIsLocked(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (IS_BASE(env))
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return BINDING_IS_LOCKED(sym) != 0;
    else {
	SEXP binding = Rf_find_binding_in_frame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	return BINDING_IS_LOCKED(binding) != 0;
    }
}

Rboolean R_BindingIsActive(SEXP sym, SEXP env)
{
    if (TYPEOF(sym) != SYMSXP)
	error(_("not a symbol"));
    if (TYPEOF(env) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(env) != ENVSXP &&
	TYPEOF((env = simple_as_environment(env))) != ENVSXP)
	error(_("not an environment"));
    if (IS_BASE(env))
	/* It is a symbol, so must have a binding even if it is
	   R_UnboundSymbol */
	return IS_ACTIVE_BINDING(sym) != 0;
    else {
	SEXP binding = Rf_find_binding_in_frame(env, sym, NULL);
	if (binding == R_NilValue)
	    error(_("no binding for \"%s\""), CHAR(PRINTNAME(sym)));
	return IS_ACTIVE_BINDING(binding) != 0;
    }
}

Rboolean R_HasFancyBindings(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table, chain;
	int i, size;

	table = HASHTAB(rho);
	size = HASHLEN(rho);
	for (i = 0; i < size; i++)
	    for (chain = VECTOR_ELT(table, i);
		 chain != R_NilValue;
		 chain = CDR(chain))
		if (IS_ACTIVE_BINDING(chain) || BINDING_IS_LOCKED(chain))
		    return TRUE;
	return FALSE;
    }
    else {
	SEXP frame;

	for (frame = FRAME(rho); frame != R_NilValue; frame = CDR(frame))
	    if (IS_ACTIVE_BINDING(frame) || BINDING_IS_LOCKED(frame))
		return TRUE;
	return FALSE;
    }
}

static SEXP do_lockBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    switch(PRIMVAL(op)) {
    case 0:
	R_LockBinding(sym, env);
	break;
    case 1:
	R_unLockBinding(sym, env);
	break;
    default:
	error(_("unknown op"));
    }
    return R_NilValue;
}

static SEXP do_bndIsLocked(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogicalMaybeConst(R_BindingIsLocked(sym, env));
}

static SEXP do_mkActiveBnd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, fun, env;
    checkArity(op, args);
    sym = CAR(args);
    fun = CADR(args);
    env = CADDR(args);
    R_MakeActiveBinding(sym, fun, env);
    return R_NilValue;
}

static SEXP do_bndIsActive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sym, env;
    checkArity(op, args);
    sym = CAR(args);
    env = CADR(args);
    return ScalarLogicalMaybeConst(R_BindingIsActive(sym, env));
}

void R_RestoreHashCount(SEXP rho)
{
    if (IS_HASHED(rho)) {
	SEXP table;
	int i, count, size;

	table = HASHTAB(rho);
	size = HASHLEN(rho);
	for (i = 0, count = 0; i < size; i++)
	    if (VECTOR_ELT(table, i) != R_NilValue)
		count++;
	SET_HASHSLOTSUSED(table, count);
    }
}

SEXP R_PackageEnvName(SEXP rho)
{
    if (TYPEOF(rho) == ENVSXP) {
	SEXP name = getAttrib(rho, R_NameSymbol);
	if (isString(name) && LENGTH(name) > 0 &&
	      strncmp (CHAR(STRING_ELT(name, 0)), "package:", 8) == 0)
	    return name;
    }

    return R_NilValue;
}

Rboolean R_IsPackageEnv(SEXP rho)
{
    return R_PackageEnvName(rho) != R_NilValue;
}

SEXP R_FindPackageEnv(SEXP info)
{
    SEXP expr, val;
    SEXP findPackageEnv_install = install("findPackageEnv");
    PROTECT(info);
    PROTECT(expr = LCONS(findPackageEnv_install, CONS(info, R_NilValue)));
    val = eval(expr, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

Rboolean R_IsNamespaceEnv(SEXP rho)
{
    if (rho == R_BaseNamespace)
	return TRUE;

    if (TYPEOF(rho) == ENVSXP) {
        if (NAMESPACE_name == R_NoObject)
            NAMESPACE_name = install(".__NAMESPACE__.");
        if (spec_name == R_NoObject)
            spec_name = install("spec");
	SEXP info = findVarInFrame3 (rho, NAMESPACE_name, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
            PROTECT(info);
	    SEXP spec = findVarInFrame3 (info, spec_name, TRUE);
            UNPROTECT(1);
	    if (spec!=R_UnboundValue && TYPEOF(spec)==STRSXP && LENGTH(spec)>0)
		return TRUE;
	}
    }

    return FALSE;
}

static SEXP do_isNSEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_IsNamespaceEnv(CAR(args)) ? mkTrue() : mkFalse();
}

SEXP R_NamespaceEnvSpec(SEXP rho)
{
    /* The namespace spec is a character vector that specifies the
       namespace.  The first element is the namespace name.  The
       second element, if present, is the namespace version.  Further
       elements may be added later. */

    if (rho == R_BaseNamespace) {
        static SEXP R_BaseNamespaceName = R_NoObject;
        if (R_BaseNamespaceName == R_NoObject) {
            R_BaseNamespaceName = ScalarString(mkChar("base"));
            R_PreserveObject(R_BaseNamespaceName);
        }
	return R_BaseNamespaceName;
    }

    if (TYPEOF(rho) == ENVSXP) {
        if (NAMESPACE_name == R_NoObject)
            NAMESPACE_name = install(".__NAMESPACE__.");
        if (spec_name == R_NoObject)
            spec_name = install("spec");
	SEXP info = findVarInFrame3 (rho, NAMESPACE_name, TRUE);
	if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
            PROTECT(info);
	    SEXP spec = findVarInFrame3 (info, spec_name, TRUE);
            UNPROTECT(1);
	    if (spec!=R_UnboundValue && TYPEOF(spec)==STRSXP && LENGTH(spec)>0)
		return spec;
	}
    }

    return R_NilValue;
}

SEXP R_FindNamespace(SEXP info)
{
    SEXP expr, val;
    SEXP getNamespace_install = install("getNamespace");
    PROTECT(info);
    PROTECT(expr = LCONS(getNamespace_install, CONS(info, R_NilValue)));
    val = eval(expr, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

static SEXP checkNSname(SEXP call, SEXP name)
{
    switch (TYPEOF(name)) {
    case SYMSXP:
	break;
    case STRSXP:
	if (LENGTH(name) >= 1) {
	    name = install(translateChar(STRING_ELT(name, 0)));
	    break;
	}
	/* else fall through */
    default:
	errorcall(call, _("bad namespace name"));
    }
    return name;
}

static SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = CADR(args);
    if (findVarInFrame(R_NamespaceRegistry, name) != R_UnboundValue)
	errorcall(call, _("namespace already registered"));
    defineVar(name, val, R_NamespaceRegistry);
    return R_NilValue;
}

static SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    if (findVarInFrame(R_NamespaceRegistry, name) == R_UnboundValue)
	errorcall(call, _("namespace not registered"));
    RemoveVariable(name, R_NamespaceRegistry);
    return R_NilValue;
}

static SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name, val;
    checkArity(op, args);
    name = checkNSname(call, CAR(args));
    val = findVarInFrame(R_NamespaceRegistry, name);
    if (val == R_UnboundValue)
	return R_NilValue;
    else
	return val;
}

static SEXP do_getNSRegistry(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_NamespaceRegistry;
}

static SEXP do_importIntoEnv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* This function copies values of variables from one environment
       to another environment, possibly with different names.
       Promises are not forced and active bindings are preserved. */
    SEXP impenv, impnames, expenv, expnames;
    SEXP impsym, expsym, val;
    int i, n;

    checkArity(op, args);

    impenv = CAR(args); args = CDR(args);
    impnames = CAR(args); args = CDR(args);
    expenv = CAR(args); args = CDR(args);
    expnames = CAR(args); args = CDR(args);

    if (TYPEOF(impenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(impenv) != ENVSXP && 
	TYPEOF((impenv = simple_as_environment(impenv))) != ENVSXP)
	error(_("bad import environment argument"));
    if (TYPEOF(expenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    if (TYPEOF(expenv) != ENVSXP &&
	TYPEOF((expenv = simple_as_environment(expenv))) != ENVSXP)
	error(_("bad export environment argument"));
    if (TYPEOF(impnames) != STRSXP || TYPEOF(expnames) != STRSXP)
	error(_("invalid '%s' argument"), "names");
    if (LENGTH(impnames) != LENGTH(expnames))
	error(_("length of import and export names must match"));

    n = LENGTH(impnames);
    for (i = 0; i < n; i++) {
	impsym = install(translateChar(STRING_ELT(impnames, i)));
	expsym = install(translateChar(STRING_ELT(expnames, i)));

	/* find the binding--may be a CONS cell or a symbol */
	SEXP binding = R_NilValue;
	for (SEXP env = expenv;
	     env != R_EmptyEnv && binding == R_NilValue;
	     env = ENCLOS(env))
	    if (env == R_BaseNamespace) {
		if (SYMVALUE(expsym) != R_UnboundValue)
		    binding = expsym;
	    } else
		binding = Rf_find_binding_in_frame(env, expsym, NULL);
	if (binding == R_NilValue)
	    binding = expsym;

	/* get value of the binding; do not force promises */
	if (TYPEOF(binding) == SYMSXP) {
	    if (SYMVALUE(expsym) == R_UnboundValue)
		error(_("exported symbol '%s' has no value"),
		      CHAR(PRINTNAME(expsym)));
	    val = SYMVALUE(expsym);
	}
	else val = CAR(binding);

	/* import the binding */
	if (IS_ACTIVE_BINDING(binding))
	    R_MakeActiveBinding(impsym, val, impenv);
	/* This is just a tiny optimization */
	else if (IS_BASE(impenv))
	    gsetVar(impsym, val, impenv);
	else
	    defineVar(impsym, val, impenv);
    }
    return R_NilValue;
}


/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_envir[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"assign",	do_assign,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"list2env",	do_list2env,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"remove",	do_remove,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"get_rm",	do_get_rm,	0,	1000,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"get",		do_get,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing_from_underline",do_missing,1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"globalenv",	do_globalenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"baseenv",	do_baseenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"emptyenv",	do_emptyenv,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"search",	do_search,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		do_ls,		1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"env2list",	do_env2list,	0,   1000011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"eapply",	do_eapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"builtins",	do_builtins,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pos.to.env",	do_pos2env,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.environment",do_as_environment,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"lockEnvironment", do_lockEnv,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}},
{"environmentIsLocked",	do_envIsLocked,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"lockBinding", do_lockBnd,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", do_bndIsLocked,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"makeActiveBinding", do_mkActiveBnd,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsActive", do_bndIsActive,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"isNamespaceEnv",do_isNSEnv,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"registerNamespace",do_regNS,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",do_getNSRegistry, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}},
{"importIntoEnv",do_importIntoEnv, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
