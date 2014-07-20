/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
 *  Copyright (C) 2003	      The R Foundation
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *  This module incorporates changes made in R Core releases R-2.15.1, 
 *  R-2.15.2, and R-2.15.3.
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
#include <Defn.h>
#include <ctype.h> /* for tolower */
#include <string.h>
#include <errno.h>

#include <Rmath.h>

#include <R_ext/GraphicsEngine.h> /* needed for GEDevDesc in do_Externalgr */

#include <R_ext/Riconv.h>

#include <helpers/helpers-app.h>


/* Trim special arguments from argument list for .Call, .External, .C or
   .Fortran.  These arguments are removed (destructively) from the argument 
   list.  The second argument is 1 for .C/.Fortran, 0 for .Call/.External, 
   and controls whether NAOK, DUP, and ENCODING are allowed.  The results
   are returned in the special_args structure passed.  The number or remaining
   arguments, apart from the first, is returned as the value of trimargs.

   Note that no action regarding these arguments is taken here - they are 
   just found, removed, and returned.  NAOK defaults to FALSE, DUP to TRUE, 
   ENCODING to R_NilValue, PACKAGE to R_NilValue, and HELPER to FALSE.

   Also checks that the first argument is present and unnamed.  Note 
   that since this argument is always present, removal of later arguments
   won't change the head of the argument list. */

struct special_args { int naok, dup, helper; SEXP encoding, pkg; };

static int trimargs (SEXP args, int C_Fort, struct special_args *r, SEXP call)
{ 
    int naokused=0, dupused=0, helperused=0, encused=0, pkgused=0, nargs=0;
    const char *p;
    SEXP s, t, prev;

    if (args == R_NilValue)
        errorcall(call, _("'.NAME' is missing"));
    if (TAG(args) != R_NilValue)
        warningcall(call, "the first argument should not be named");

    r->naok = FALSE;
    r->dup = TRUE;
    r->helper = FALSE;
    r->encoding = R_NilValue;
    r->pkg = R_NilValue;

    prev = args;

    for (s = CDR(args); s != R_NilValue; s = CDR(s)) {

        if (TAG(s) == R_PkgSymbol) {
            r->pkg = CAR(s);
            if (pkgused) warningcall(call,_("PACKAGE used more than once"));
            pkgused = 1;
            goto remove;
        }

        if (C_Fort) {

            if (TAG(s) == R_EncSymbol) {
                r->encoding = CAR(s);
                if (encused) 
                    warningcall(call,_("ENCODING used more than once"));
                encused = 1;
                goto remove;
            }
     
            if (TAG(s) == R_NaokSymbol) {
                r->naok = asLogical(CAR(s));
                if (naokused) 
                    warningcall(call,_("NAOK used more than once"));
                naokused = 1;
                goto remove;
            }
    
            if (TAG(s) == R_DupSymbol) {
                r->dup = asLogical(CAR(s));
                if (dupused) 
                    warningcall(call,_("DUP used more than once"));
                dupused = 1;
                goto remove;
            }
    
            if (TAG(s) == R_HelperSymbol) {
                r->helper = asLogical(CAR(s));
                if (helperused) 
                    warningcall(call,_("HELPER used more than once"));
                helperused = 1;
                goto remove;
            }
        }

        nargs += 1;
        prev = s;
        continue;

      remove:
        CDR(prev) = CDR(s);
    }

    return nargs;
}


#include <Rdynpriv.h>
// Odd: 'type' is really this enum
enum {NOT_DEFINED, FILENAME, DLL_HANDLE, R_OBJECT};
typedef struct {
    char DLLname[PATH_MAX];
    HINSTANCE dll;
    SEXP  obj;
    int type;
} DllReference;

/* Maximum length of entry-point name, including nul terminator */
#define MaxSymbolBytes 1024

/* Maximum number of args to .C, .Fortran and .Call */
#define MAX_ARGS 65

/* This looks up entry points in DLLs in a platform specific way. */
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol, SEXP env);


/* Called from resolveNativeRoutine.

  Checks whether the specified object correctly identifies a native routine.
  op is the supplied value for .NAME.  This can be
   a) a string (when this does nothing).
   b) an external pointer giving the address of the routine
      (e.g. getNativeSymbolInfo("foo")$address)
   c) or a NativeSymbolInfo itself  (e.g. getNativeSymbolInfo("foo"))

   It copies the symbol name to buf.

   NB: in the last two cases it sets fun and symbol as well!
 */
static void processSymbolId (SEXP op, SEXP call, DL_FUNC *fun,
                             R_RegisteredNativeSymbol *symbol, char *buf)
{
    if (isValidString(op))
        return;

    if (TYPEOF(op) != EXTPTRSXP && inherits(op, "NativeSymbolInfo"))
        op = VECTOR_ELT(op,1);

    if (TYPEOF(op) == EXTPTRSXP) {
	char *p = NULL;
        *fun = NULL;
	if (R_ExternalPtrTag(op) == R_NativeSymbolSymbol)
	   *fun = R_ExternalPtrAddrFn(op);
	else if(R_ExternalPtrTag(op) == R_RegisteredNativeSymbolSymbol) {
	   R_RegisteredNativeSymbol *tmp;
	   tmp = (R_RegisteredNativeSymbol *) R_ExternalPtrAddr(op);
	   if(tmp) {
	      if(symbol->type != R_ANY_SYM && symbol->type != tmp->type)
		 errorcall(call, _("NULL value passed as symbol address"));
		/* Check the type of the symbol. */
	      switch(symbol->type) {
	      case R_C_SYM:
		  *fun = tmp->symbol.c->fun;
		  p = tmp->symbol.c->name;
		  break;
	      case R_CALL_SYM:
		  *fun = tmp->symbol.call->fun;
		  p = tmp->symbol.call->name;
		  break;
	      case R_FORTRAN_SYM:
		  *fun = tmp->symbol.fortran->fun;
		  p = tmp->symbol.fortran->name;
		  break;
	      case R_EXTERNAL_SYM:
		  *fun = tmp->symbol.external->fun;
		  p = tmp->symbol.external->name;
		  break;
	      default:
		 /* Something unintended has happened if we get here. */
		  errorcall(call, _("Unimplemented type %d in createRSymbolObject"),
			    symbol->type);
		  break;
	      }
	      *symbol = *tmp;
	   }
	}

	if(*fun == NULL)
	    errorcall(call, _("NULL value passed as symbol address"));

	/* copy the symbol name. */
	if (p) {
            if (!copy_1_string(buf,MaxSymbolBytes,p))
		error(_("symbol '%s' is too long"), p);
	}

	return;
    }

    errorcall(call,
     _("first argument must be a string (of length 1) or native symbol reference"));
}


/*
  This is the routine that is called by do_dotCode, do_dotcall and
  do_External to find the DL_FUNC to invoke. It handles processing
  the PACKAGE argument, if present, and also takes care of
  the cases where we are given a NativeSymbolInfo object, an
  address directly, and if the DLL is specified. If no PACKAGE is
  provided, we check whether the calling function is in a namespace
  and look there.
*/

//#define CHECK_NAMSPACE_RESOLUTION 1

static void
resolveNativeRoutine(SEXP args, DL_FUNC *fun, R_RegisteredNativeSymbol *symbol,
                     SEXP pkg, char *buf, SEXP call, SEXP env)
{
    SEXP op;
    char *q;
    const char *p, *name; 

    DllReference dll;
    /* Null string for DLLname is shorthand for 'all' in R_FindSymbol, but
       should never be supplied */
    dll.DLLname[0] = 0; dll.dll = NULL; dll.obj = NULL; dll.type = NOT_DEFINED;
    
    op = CAR(args);  // value of .NAME =

    processSymbolId(op, call, fun, symbol, buf); /* May set fun, symbol, buf */

    if (TYPEOF(pkg) == STRSXP) {
        if (LENGTH(pkg) != 1)
	    error(_("PACKAGE argument must be a single character string"));
        name = translateChar (STRING_ELT (pkg,0));
        /* allow the package: form of the name, as returned by find */
        if(strncmp(name, "package:", 8) == 0) name += 8;
        if (!copy_1_string (dll.DLLname, PATH_MAX, name))
            error(_("PACKAGE argument is too long"));
        dll.obj = pkg; /* ?? */
        dll.type = FILENAME;
    }
    else if (pkg != R_NilValue) {
        /* Have a DLL object, which is not something documented .... */
        if(TYPEOF(pkg) == EXTPTRSXP) {
            dll.dll = (HINSTANCE) R_ExternalPtrAddr(pkg);
            dll.obj = pkg; /* ?? */
            dll.type = DLL_HANDLE;
        } else if(TYPEOF(pkg) == VECSXP) {
            dll.obj = pkg; /* ?? */
            dll.type = R_OBJECT;
            strcpy(dll.DLLname,
                   translateChar(STRING_ELT(VECTOR_ELT(pkg, 1), 0)));
            dll.dll = (HINSTANCE) R_ExternalPtrAddr(VECTOR_ELT(pkg, 4));
        } else 
            error("incorrect type (%s) of PACKAGE argument\n",
        	  type2char(TYPEOF(pkg)));
    }

    /* We were given a symbol (or an address), so we are done. */
    if (*fun) return;

    // find if we were called from a namespace
    SEXP env2 = ENCLOS(env);
    const char *ns = "";
    if(R_IsNamespaceEnv(env2))
	ns = CHAR(STRING_ELT(R_NamespaceEnvSpec(env2), 0));
    else env2 = R_NilValue;

    /* Make up the load symbol */
    if(TYPEOF(op) == STRSXP) {
	p = translateChar(STRING_ELT(op, 0));
	if (!copy_1_string(buf,MaxSymbolBytes,p))
	    error(_("symbol '%s' is too long"), p);
        if (symbol->type == R_FORTRAN_SYM)
            for (q = buf; *q; q++) *q = (char) tolower(*q);
    }

    if (dll.type != FILENAME && *ns != 0) {
	/* no PACKAGE= arg, so see if we can identify a DLL
	   from the namespace defining the function */
	*fun = R_FindNativeSymbolFromDLL(buf, &dll, symbol, env2);
	if (*fun) return;
#ifdef CHECK_NAMSPACE_RESOLUTION
	warningcall(call, 
		    "\"%s\" not resolved from current namespace (%s)",
		    buf, ns);
#endif
	/* need to continue if the namespace search failed */
    }

    /* NB: the actual conversion to the symbol is done in
       R_dlsym in Rdynload.c.  That prepends an underscore (usually),
       and may append one or more underscores.
    */

    *fun = R_FindSymbol(buf, dll.DLLname, symbol);
    if (*fun) return;

    /* so we've failed and bail out */
    if (*dll.DLLname != 0) {
	switch(symbol->type) {
	case R_C_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".C", dll.DLLname);
	    break;
	case R_FORTRAN_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Fortran", dll.DLLname);
	    break;
	case R_CALL_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".Call", dll.DLLname);
	    break;
	case R_EXTERNAL_SYM:
	    errorcall(call,
		      _("\"%s\" not available for %s() for package \"%s\""),
		      buf, ".External", dll.DLLname);
	    break;
	case R_ANY_SYM:
	    errorcall(call,
		      _("%s symbol name \"%s\" not in DLL for package \"%s\""),
		      "C/Fortran", buf, dll.DLLname);
	    break;
	}
    } else
	errorcall(call, _("%s symbol name \"%s\" not in load table"),
		  symbol->type == R_FORTRAN_SYM ? "Fortran" : "C", buf);
}


/* Foreign Function Interface.  This code allows a user to call C */
/* or Fortran code which is either statically or dynamically linked. */


SEXP attribute_hidden do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *sym, *type="", *pkg = "";
    int val = 1, nargs = length(args);
    R_RegisteredNativeSymbol symbol = {R_ANY_SYM, {NULL}, NULL};

    if (nargs < 1) error(_("no arguments supplied"));
    if (nargs > 3) error(_("too many arguments"));

    if(!isValidString(CAR(args)))
	error(_("invalid '%s' argument"), "symbol");
    sym = translateChar(STRING_ELT(CAR(args), 0));
    if(nargs >= 2) {
	if(!isValidString(CADR(args)))
	    error(_("invalid '%s' argument"), "PACKAGE");
	pkg = translateChar(STRING_ELT(CADR(args), 0));
    }
    if(nargs >= 3) {
	if(!isValidString(CADDR(args)))
	    error(_("invalid '%s' argument"), "type");
	type = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
	if(strcmp(type, "C") == 0) symbol.type = R_C_SYM;
	else if(strcmp(type, "Fortran") == 0) symbol.type = R_FORTRAN_SYM;
	else if(strcmp(type, "Call") == 0) symbol.type = R_CALL_SYM;
	else if(strcmp(type, "External") == 0) symbol.type = R_EXTERNAL_SYM;
    }
    if(!(R_FindSymbol(sym, pkg, &symbol))) val = 0;
    return ScalarLogical(val);
}

/*   Call dynamically loaded "internal" functions.
     Original code by Jean Meloche <jean@stat.ubc.ca> */

typedef SEXP (*R_ExternalRoutine)(SEXP);

SEXP attribute_hidden do_External(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = NULL;
    R_ExternalRoutine fun = NULL;
    SEXP retval;
    R_RegisteredNativeSymbol symbol = {R_EXTERNAL_SYM, {NULL}, NULL};
    const void *vmax = VMAXGET();
    char buf[MaxSymbolBytes];
    struct special_args spa;

    (void) trimargs (args, 0, &spa, call);

    PROTECT(spa.pkg);
    resolveNativeRoutine (args, &ofun, &symbol, spa.pkg, buf, call, env);
    fun = (R_ExternalRoutine) ofun;
    UNPROTECT(1);

    /* Some external symbols that are registered may have 0 as the
       expected number of arguments.  We may want a warning
       here. However, the number of values may vary across calls and
       that is why people use the .External() mechanism.  So perhaps
       we should just kill this check.
    */
#ifdef CHECK_EXTERNAL_ARG_COUNT         /* Off by default. */
    if(symbol.symbol.external && symbol.symbol.external->numArgs > -1) {
	if(symbol.symbol.external->numArgs != length(args))
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      length(args), symbol.symbol.external->numArgs, buf);
    }
#endif

    /* Note that .NAME is passed as the first argument (and usually ignored). */
    retval = (SEXP)fun(args);
    VMAXSET(vmax);
    return retval;
}

#ifdef __cplusplus
typedef SEXP (*VarFun)(...);
#else
typedef DL_FUNC VarFun;
#endif

/* .Call(name, <args>) */
SEXP attribute_hidden do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = NULL;
    VarFun fun = NULL;
    SEXP retval, pargs;
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {NULL}, NULL};
    int nargs, i;
    const void *vmax = VMAXGET();
    char buf[MaxSymbolBytes];
    struct special_args spa;

    nargs = trimargs (args, 0, &spa, call);

    if (nargs > MAX_ARGS)
        errorcall(call, _("too many arguments in foreign function call"));

    PROTECT(spa.pkg);
    resolveNativeRoutine(args, &ofun, &symbol, spa.pkg, buf, call, env);
    fun = (VarFun) ofun;
    args = CDR(args);
    UNPROTECT(1);

    SEXP cargs [nargs>0 ? nargs : 1]; /* 0-length arrays not allowed in C99 */

    for (pargs = args, i = 0; pargs != R_NilValue; pargs = CDR(pargs), i++)
	cargs[i] = CAR(pargs);

    if(symbol.symbol.call && symbol.symbol.call->numArgs > -1) {
	if(symbol.symbol.call->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for '%s'"),
		      nargs, symbol.symbol.call->numArgs, buf);
    }

    retval = R_NilValue;	/* -Wall */
    fun = (VarFun) ofun;
    switch (nargs) {
    case 0:
	retval = (SEXP)ofun();
	break;
    case 1:
	retval = (SEXP)fun(cargs[0]);
	break;
    case 2:
	retval = (SEXP)fun(cargs[0], cargs[1]);
	break;
    case 3:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    default:
	errorcall(call, _("too many arguments, sorry"));
    }
    VMAXSET(vmax);
    return retval;
}

/*  Call dynamically loaded "internal" graphics functions
    .External.graphics (unused) and  .Call.graphics (used in grid).

    If there is an error or user-interrupt in the above
    evaluation, dd->recordGraphics is set to TRUE
    on all graphics devices (see GEonExit(); called in errors.c)

    NOTE: if someone uses try() around this call and there
    is an error, then dd->recordGraphics stays FALSE, so
    subsequent pages of graphics output are NOT saved on
    the display list.  A workaround is to deliberately
    force an error in a graphics call (e.g., a grid popViewport()
    while in the ROOT viewport) which will reset dd->recordGraphics
    to TRUE as per the comment above.
*/

SEXP attribute_hidden do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    pGEDevDesc dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_External(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) { // which is record && call != R_NilValue
	if (!GEcheckState(dd))
	    errorcall(call, _("Invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

SEXP attribute_hidden do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    pGEDevDesc dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_dotcall(call, op, args, env));
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    errorcall(call, _("Invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

static SEXP
Rf_getCallingDLL(void)
{
    SEXP e, ans;
    RCNTXT *cptr;
    SEXP rho = R_NilValue;
    Rboolean found = FALSE;

    /* First find the environment of the caller.
       Testing shows this is the right caller, despite the .C/.Call ...
     */
    for (cptr = R_GlobalContext;
	 cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	 cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_FUNCTION) {
		/* PrintValue(cptr->call); */
		rho = cptr->cloenv;
		break;
	    }
    /* Then search up until we hit a namespace or globalenv.
       The idea is that we will not find a namespace unless the caller
       was defined in one. */
    while(rho != R_NilValue) {
	if (rho == R_GlobalEnv) break;
	else if (R_IsNamespaceEnv(rho)) {
	    found = TRUE;
	    break;
	}
	rho = ENCLOS(rho);
    }
    if(!found) return R_NilValue;

    PROTECT(e = lang2(install("getCallingDLLe"), rho));
    ans = eval(e,  R_GlobalEnv);
    UNPROTECT(1);
    return(ans);
}


/*
  We are given the PACKAGE argument in dll.obj
  and we can try to figure out how to resolve this.
  0) dll.obj is NULL.  Then find the environment of the
   calling function and if it is a namespace, get the first registered DLL.

  1) dll.obj is a DLLInfo object
*/
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol, 
			  SEXP env)
{
    int numProtects = 0;
    DllInfo *info;
    DL_FUNC fun = NULL;

    if(dll->obj == NULL) {
	/* Rprintf("\nsearching for %s\n", name); */
	if (env != R_NilValue) {
	    SEXP e;
	    PROTECT(e = lang2(install("getCallingDLLe"), env));
	    dll->obj = eval(e, R_GlobalEnv);
	    UNPROTECT(1);
	} else dll->obj = Rf_getCallingDLL();
	PROTECT(dll->obj); numProtects++;
    }

    if(inherits(dll->obj, "DLLInfo")) {
	SEXP tmp;
	tmp = VECTOR_ELT(dll->obj, 4);
	info = (DllInfo *) R_ExternalPtrAddr(tmp);
	if(!info)
	    error(_("NULL value for DLLInfoReference when looking for DLL"));
	fun = R_dlsym(info, name, symbol);
    }

    if(numProtects) UNPROTECT(numProtects);

    return fun;
}



/* .C() {op=0}  or  .Fortran() {op=1}

   Use of this except for atomic vectors is not allowed for .Fortran,
   and is only kept for legacy code for .C.

   CRAN packages R2Cuba, RCALI, ars, coxme, fCopulae, locfit, nlme,
   splinesurv and survival pass functions, the case of RCALI as a list
   of two functions.

   RecordLinkage and locfit pass lists.
*/


/* Task procedure for .C and .Fortran.  This procedure may be called
   directly in the master thread, or scheduled for deferred evaluation,
   perhaps in a helper thread.

   If it is called directly, the function to call may be specified by the
   dotCode_fun variable.  If deferred, the function to call must be stored
   in the rawfun operand, which is NULL otherwise.

   If the output, ans1, is not NULL, it is the sole result of the
   computation, with the arguments in args being ignored after the
   call (they may or may not have changed).  If ans1 is NULL, this
   task procedure must be called directly, not deferred, and the
   contents of args is updated to be returned as the result (or perhaps
   the procedure is called for its side effects).  Updating args may
   involve memory allocation, not allowed for a deferred task, as some
   arguments may need to be converted to an appropriate form.  

   The args operand will be a vector list (VECSXP) containing pointers to 
   the arguments of the function to be called.  For arguments that are
   atomic vectors or vector lists, the function called is passed the DATAPTR 
   for the vector (unless this argument is marked as an out-of-the-box scalar,
   see below).  For other argument types, the function is passed the SEXP 
   for the operand itself.  If this task procedure is deferred, the arguments,
   other than ans1, must all either be unshared (NAMEDCNT of zero) or be 
   guaranteed to never change (NAMEDCNT at its maximum), since the helpers
   "in use" mechanism will not work for such objects that are inside the operand
   passed.

   A string argument needing converstion back to an R string is recognized 
   as such by ATTRIB being a RAWSXP (not a pairlist), with ATTRIB pointing 
   to itself for a Fortran string.  Single-precision arguments needing 
   to be converted to an R double vector are recognized by having ATTRIB 
   of R_CSingSymbol.  The result, ans1, must not be either of these sorts
   of argument.

   The opcode, scalars, holds a bit-vector indicating which of the
   first 64 arguments are scalars that should be stored "out of their
   boxes" - copied to a local variable before the call, and if necessary 
   copied back after the call.  Any arguments past the first 64 are never 
   treated as out-of-the-box scalars.  If ans1 is not NULL, it must not
   be marked as an out-of-the-box scalar (even if it is of length one).

   The variable dotCode_spa is consulted for the value of DUP (but only 
   when the task is not deferred).
*/

static VarFun dotCode_fun;  /* Function to call for .C or .Fortran */
static struct special_args dotCode_spa;  /* Special arguments provided */
static R_NativeArgStyle *argStyles;  /* May hold types and usages of args */

void task_dotCode (helpers_op_t scalars, SEXP ans1, SEXP rawfun, SEXP args)
{
    int nargs = LENGTH(args);
    int nargs1 = nargs>0 ? nargs : 1;  /* 0-length arrays not allowed in C99 */
    R_NativeArgStyle *arg_styles = argStyles;  /* save now, might change */

    /* Pointers to where the data is, passed to the function called. */

    void *cargs [nargs1]; 

    /* Scalar RAW, LGL, INT, and REAL arguments out of their boxes. */

    union scalar_val { 
        Rbyte r;   /* for RAW */
        int i;     /* for LGL and INT */
        double d;  /* for REAL */
    } scalar_value [scalars==0 ? 1 : nargs1];

    /* Pointer to function to call.  If this task may be deferred, we need
       to get it from the rawfun operand, otherwise it's faster from a variable
       set by the caller. */

    VarFun fun = rawfun!=NULL ? * (VarFun *) RAW(rawfun) : dotCode_fun;

    helpers_op_t sc;
    int na;

    for (na = 0, sc = scalars; na < nargs; na++, sc >>= 1) {

        SEXP arg = VECTOR_ELT(args,na);

        if (sc & 1) {  /* copy argument out of its box */
            if (arg == ans1) abort();
            switch (TYPEOF (arg)) {
            case RAWSXP:  scalar_value[na].r = RAW(arg)[0];     break;
            case LGLSXP:  scalar_value[na].i = LOGICAL(arg)[0]; break;
            case INTSXP:  scalar_value[na].i = INTEGER(arg)[0]; break;
            case REALSXP: scalar_value[na].d = REAL(arg)[0];    break;
            default: abort();
            }
            cargs[na] = &scalar_value[na];
        }
        else if (isVectorAtomic(arg) || TYPEOF(arg) == VECSXP)
            cargs[na] = (void *) DATAPTR(arg);
        else
            cargs[na] = (void *) arg;
    }

    switch (nargs) {
    case 0:
	/* Silicon graphics C chokes here */
	/* if there is no argument to fun. */
	fun(0);
	break;
    case 1:
	fun(cargs[0]);
	break;
    case 2:
	fun(cargs[0], cargs[1]);
	break;
    case 3:
	fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
 	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    }

    /* Handle case where ans1 is the only result (any change to other
       arguments ignored). */

    if (ans1) {

        /* Make sure a logical answer has valid values. Doesn't write if 
           unnecessary, since this may be faster. */

        if (TYPEOF(ans1) == LGLSXP) {
            int n = LENGTH(ans1);
            int *q = LOGICAL(ans1);
            for (int i = 0; i < n; i++) {
                if (q[i]!=0 && q[i]!=1 && q[i]!=NA_LOGICAL) q[i] = 1;
            }
        }

        return;
    }

    /* FROM HERE ON DOWN, WE WILL (AND MUST) BE RUNNING IN THE MASTER THREAD. */

    /* Convert all arguments in args to the form needed for them to be 
       returned as the result. */

    for (na = 0, sc = scalars; na < nargs; na++, sc >>= 1) {

        if (dotCode_spa.dup && arg_styles && arg_styles[na] == R_ARG_IN) {
            SET_VECTOR_ELT(args, na, R_NilValue);
            continue;
        }

        SEXP arg = VECTOR_ELT(args,na);

        /* For logical vectors, ensure validity of values.  Doesn't write
           if unnecessary, since this may be faster, and would be essential
           if the argument could be a read-only constant. */

        if (TYPEOF(arg) == LGLSXP) {
            int n = LENGTH(arg);
            int *q = (int *) cargs[na];  /* could be an out-of-box scalar */
            for (int i = 0; i < n; i++) {
                if (q[i]!=0 && q[i]!=1 && q[i]!=NA_LOGICAL) q[i] = 1;
            }
        }

        /* For an out-of-box scalar, see if it has changed, and if so
           allocate new space for its new value.  Note:  Since attributes
           might need to be attached later, we can't return a constant. */

        if (sc & 1) {
            switch (TYPEOF(arg)) {
            case RAWSXP:
                if (scalar_value[na].r != RAW(arg)[0])
                    SET_VECTOR_ELT(args, na, ScalarRaw(scalar_value[na].r));
                break;
            case LGLSXP:
                if (scalar_value[na].i != LOGICAL(arg)[0])
                    SET_VECTOR_ELT(args, na, ScalarLogical(scalar_value[na].i));
                break;
            case INTSXP:
                if (scalar_value[na].i != INTEGER(arg)[0])
                    SET_VECTOR_ELT(args, na, ScalarInteger(scalar_value[na].i));
                break;
            case REALSXP: 
                /* compare as 64-bit unsigned ints, else -0 will equal +0,
                   and it may not work for NaN, etc. */
                if (*(uint64_t*)&scalar_value[na].d != *(uint64_t*)REAL(arg))
                    SET_VECTOR_ELT(args, na, ScalarReal(scalar_value[na].d));
                break;
            default: abort();
            }
        }

        /* Convert a single-precision argument back to double. */

        if (ATTRIB(arg) == R_CSingSymbol) {
            int n = LENGTH(arg);
            SEXP s = allocVector (REALSXP, n);
            float *sptr = (float*) cargs[na];
            for (int i = 0; i < n; i++) 
                REAL(s)[i] = (double) sptr[i];
            SET_VECTOR_ELT (args, na, s);
        }

        /* Convert strings back into R string form. */

        if (TYPEOF(ATTRIB(arg)) == RAWSXP) {
            if (ATTRIB(arg) == arg) {
                char buf[256];
                /* only return one string: warned on the R -> Fortran step */
                strncpy(buf, (char*)cargs[na], 255);
                buf[255] = '\0';
                SET_VECTOR_ELT (args, na, ScalarString(mkChar(buf)));
            } 
            else {
                int n = LENGTH(arg) / sizeof (char*);
                char **cptr = (char**) cargs[na];
                SEXP s;
                PROTECT (s = allocVector (STRSXP, n));
                for (int i = 0 ; i < n ; i++) 
                    SET_STRING_ELT(s, i, mkChar(cptr[i]));
                SET_VECTOR_ELT (args, na, s);
                UNPROTECT(1);
            }
        }
    }
}


SEXP attribute_hidden do_dotCode (SEXP call, SEXP op, SEXP args, SEXP env,
                                  int variant)
{
    int na, nargs, Fort;
    int name_count, last_pos, i;
    DL_FUNC ofun = NULL;
    SEXP ans, pa, s, last_tag, last_arg;
    R_RegisteredNativeSymbol symbol = {R_C_SYM, {NULL}, NULL};
    R_NativePrimitiveArgType *checkTypes = NULL;
    void *vmax;
    char symName[MaxSymbolBytes], encname[101];

    vmax = VMAXGET();
    Fort = PRIMVAL(op);
    argStyles = NULL;
    if(Fort) symbol.type = R_FORTRAN_SYM;

    nargs = trimargs (args, 1, &dotCode_spa, call);

    if (dotCode_spa.naok == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "NAOK");
    if (dotCode_spa.helper == NA_LOGICAL)
        errorcall(call, _("invalid '%s' value"), "HELPER");
    /* Should check for DUP being NA_LOGICAL too?  But not done in R-2.15.3...*/

    if (nargs > MAX_ARGS)
        errorcall(call, _("too many arguments in foreign function call"));

    PROTECT2(dotCode_spa.pkg,dotCode_spa.encoding);

    encname[0] = 0;
    if (dotCode_spa.encoding != R_NilValue) {
        if (TYPEOF(dotCode_spa.encoding) != STRSXP 
             || LENGTH(dotCode_spa.encoding) != 1)
            error(_("ENCODING argument must be a single character string"));
        strncpy(encname, translateChar(STRING_ELT(dotCode_spa.encoding,0)),100);
        encname[100] = 0;
        warning("ENCODING is deprecated");
    }

    resolveNativeRoutine (args, &ofun, &symbol, dotCode_spa.pkg, symName, 
                          call, env);
    dotCode_fun = (VarFun) ofun;
    args = CDR(args);

    UNPROTECT(2);

    /* Count named arguments, and note last one. */

    name_count = 0;
    for (pa = args, i = 0; pa != R_NilValue; pa = CDR(pa), i++) {
	if (TAG(pa) != R_NilValue) {
            name_count += 1;
            last_tag = TAG(pa);
            last_arg = CAR(pa);
            last_pos = i;
        }
    }

    if(symbol.symbol.c && symbol.symbol.c->numArgs > -1) {
	if(symbol.symbol.c->numArgs != nargs)
	    errorcall (call,
              _("Incorrect number of arguments (%d), expecting %d for '%s'"),
              nargs, symbol.symbol.c->numArgs, symName);
	checkTypes = symbol.symbol.c->types;
	argStyles = symbol.symbol.c->styles;
    }

    /* Construct the return value */

    int return_one_named = 
          name_count == 1 && VARIANT_KIND(variant) == VARIANT_ONE_NAMED;

    PROTECT(ans = allocVector(VECSXP, nargs));

    if (name_count > 1 || name_count == 1 && !return_one_named) {
        /* not a variant return, and need names */
	SEXP names;
	PROTECT(names = allocVector(STRSXP, nargs));
        /* The names are initialized by allocVector to all R_BlankString. */
        pa = args;
        for (na = 0; na <= last_pos; na++) {
	    if (TAG(pa) != R_NilValue)
		SET_STRING_ELT(names, na, PRINTNAME(TAG(pa)));
            pa = CDR(pa);
        }
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }

    if (!return_one_named || helpers_not_multithreading_now)
        dotCode_spa.helper = 0;

    /* Convert the arguments for use in .C or .Fortran. */

    helpers_op_t scalars = 0;  /* bit vector indicating which arguments
                                  are scalars used out of their boxes */

    for (na = 0, pa = args ; pa != R_NilValue; pa = CDR(pa), na++) {

	s = CAR(pa);

	if (checkTypes && checkTypes[na] != ANYSXP) {
            int targetType = checkTypes[na];
            int typesMatch = targetType == TYPEOF(s)
                          || targetType == SINGLESXP 
                              && asLogical (getAttrib(s,R_CSingSymbol)) == TRUE;
            if (!typesMatch)
                errorcall(call, _("Wrong type for argument %d in call to %s"),
                          na+1, symName);
	}

	/* Start with return value a copy of the inputs (maybe coerced above),
           as that is what is needed for DUP = FALSE and for non-atomic-vector
           inputs. */

	SET_VECTOR_ELT(ans, na, s);

        /* Do error checks on argument contents, and make any needed copies,
           here, not in the dotCode task procedure, which may be deferred
           to run in a helper thread where such things aren't allowed. 

           We don't need to duplicate when NAMEDCNT is zero.  When it is
           greater than zero, and the argument is scalar, we can avoid an
           immediate duplication by flagging the argument to be used out
           of its box (with a duplicate made only if it will be part of
           the result, and it has been changed). */

	SEXPTYPE t = TYPEOF(s);
        int n;

	switch(t) {
	case RAWSXP:
	    n = LENGTH(s);
	    if (NAMEDCNT_GT_0(s) && (dotCode_spa.dup || n==1)) {
                if (n == 1 && na < 64 && !(return_one_named && na == last_pos))
                    scalars |= (helpers_op_t) 1 << na;
                else {
		    SEXP ss = allocVector(t, n);
		    memcpy(RAW(ss), RAW(s), n * sizeof(Rbyte));
		    SET_VECTOR_ELT(ans, na, ss);
                }
            }
	    break;
	case LGLSXP:
	case INTSXP:
	    n = LENGTH(s);
	    int *iptr = INTEGER(s);
	    if (!dotCode_spa.naok)
		for (int i = 0 ; i < n ; i++)
		    if (iptr[i] == NA_INTEGER)
			error(_("NAs in foreign function call (arg %d)"), na+1);
	    if (NAMEDCNT_GT_0(s) && (dotCode_spa.dup || n==1)) {
                if (n == 1 && na < 64 && !(return_one_named && na == last_pos))
                    scalars |= (helpers_op_t) 1 << na;
                else {
		    SEXP ss = allocVector(t, n);
		    memcpy(INTEGER(ss), INTEGER(s), n * sizeof(int));
		    SET_VECTOR_ELT(ans, na, ss);
                }
            }
	    break;
	case REALSXP:
	    n = LENGTH(s);
	    double *rptr = REAL(s);
	    if (!dotCode_spa.naok)
		for (int i = 0 ; i < n ; i++)
		    if(!R_FINITE(rptr[i]))
			error(_("NA/NaN/Inf in foreign function call (arg %d)"),
                              na+1);
	    if (asLogical(getAttrib(s, R_CSingSymbol)) == 1) {
                double nints = ((double) n * sizeof(float)) / sizeof(int);
                if (nints > INT_MAX)
                    error (_(
                    "too many elements for vector of single-precision values"));
                if (return_one_named && na == last_pos) return_one_named = 0;
                SEXP ss = allocVector (INTSXP, (int)(nints+0.99));
		float *sptr = (float*) INTEGER(ss);
		for (int i = 0 ; i < n ; i++) sptr[i] = (float) REAL(s)[i];
                SET_ATTRIB_TO_ANYTHING(ss,R_CSingSymbol);
                SET_VECTOR_ELT(ans, na, ss);
	    } 
            else if (NAMEDCNT_GT_0(s) && (dotCode_spa.dup || n==1)) {
                if (n == 1 && na < 64 && !(return_one_named && na == last_pos))
                    scalars |= (helpers_op_t) 1 << na;
                else {
		    SEXP ss  = allocVector(t, n);
		    memcpy(REAL(ss), REAL(s), n * sizeof(double));
		    SET_VECTOR_ELT(ans, na, ss);
                }
	    }
	    break;
	case CPLXSXP:
	    n = LENGTH(s);
	    Rcomplex *zptr = COMPLEX(s);
	    if (!dotCode_spa.naok)
		for (int i = 0 ; i < n ; i++)
		    if(!R_FINITE(zptr[i].r) || !R_FINITE(zptr[i].i))
			error (_("complex NA/NaN/Inf in foreign function call (arg %d)"), na+1);
	    if (NAMEDCNT_GT_0(s) && (dotCode_spa.dup || n==1)) {
		SEXP ss = allocVector(t, n);
		memcpy(COMPLEX(ss), COMPLEX(s), n * sizeof(Rcomplex));
		SET_VECTOR_ELT(ans, na, ss);
	    }
	    break;
	case STRSXP:
	    if (!dotCode_spa.dup && 0)  /* this is no longer an error in pqR */
		error(_("character variables must be duplicated in .C/.Fortran"));
            if (return_one_named && na == last_pos) return_one_named = 0;
	    n = LENGTH(s);
	    if (Fort) { /* .Fortran */
		const char *ss = n==0 ? "" : translateChar(STRING_ELT(s, 0));
		if (n > 1)
		    warning(_("only first string in char vector used in .Fortran"));
                int len;
                len = strlen(ss) + 1;
                if (len < 256) len = 256;
                SEXP st = allocVector(RAWSXP,len);
		strcpy(RAW(st), ss);
                SET_ATTRIB_TO_ANYTHING(st,st); /* marks it as pFortran string */
                SET_VECTOR_ELT(ans, na, st);
	    } 
            else { /* .C */
                if ((double)n*sizeof(char*) > INT_MAX)
                    error(_("too many strings to pass to C routine"));
                SEXP sptr = allocVector(RAWSXP,n*sizeof(char*));
                SET_VECTOR_ELT(ans, na, sptr);
		char **cptr = (char**) RAW(sptr);
		if (*encname != 0) {
		    char *outbuf;
		    const char *inbuf;
		    size_t inb, outb, outb0, res;
		    void *obj = Riconv_open("", encname); /* (to, from) */
		    if (obj == (void *)-1)
			error(_("unsupported encoding '%s'"), encname);
		    for (int i = 0 ; i < n ; i++) {
			inbuf = CHAR(STRING_ELT(s, i));
			inb = strlen(inbuf);
			outb0 = 3*inb;
		    restart_in: ;
                        SEXP new = allocVector(RAWSXP,outb0+1+1);
                        SET_ATTRIB_TO_ANYTHING (new, ATTRIB(sptr));
                        SET_ATTRIB_TO_ANYTHING (sptr, new);
			cptr[i] = outbuf = (char*) RAW(new);
			outb = 3*inb;
			Riconv(obj, NULL, NULL, &outbuf, &outb);
			errno = 0; /* precaution */
			res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
			if(res == -1 && errno == E2BIG) {
			    outb0 *= 3;
			    goto restart_in;
			}
			if(res == -1)
			    error(_("conversion problem in re-encoding to '%s'"),
				  encname);
			*outbuf = '\0';
		    }
		    Riconv_close(obj);
		} else {
		    for (int i = 0 ; i < n ; i++) {
			const char *ss = translateChar(STRING_ELT(s, i));
			int nn = strlen(ss) + 1;
                        SEXP new = allocVector (RAWSXP, nn==1 ? 128 : nn);
                        SET_ATTRIB_TO_ANYTHING (new, ATTRIB(sptr));
                        SET_ATTRIB_TO_ANYTHING (sptr, new);
                        cptr[i] = (char*) RAW(new);
			if (nn > 1)
			    strcpy(cptr[i], ss);
			else {
			    /* Protect ourselves against those who like to
			       extend "", maybe using strncpy */
			    memset(cptr[i], 0, 128);
			}
		    }
		}
                if (n == 0) {
                    /* keep it from looking like it's actually for a RAWSXP. */
                    SET_ATTRIB_TO_ANYTHING(sptr,allocVector(RAWSXP,0));
                }
	    }
	    break;
	case VECSXP:
	    if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"),
			    type2char(t), na + 1);
            dotCode_spa.helper = 0;
	    break;
	case CLOSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	case ENVSXP:
	    if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"), 
			    type2char(TYPEOF(s)), na + 1);
            dotCode_spa.helper = 0;
	    break;
	default:
	    /* Includes pairlists from R 2.15.0 */
	    if (Fort) error(_("invalid mode (%s) to pass to Fortran (arg %d)"), 
			    type2char(t), na + 1);
	    warning("passing an object of type '%s' to .C (arg %d) is deprecated", 
		    type2char(t), na + 1);
	    if (t == LISTSXP)
		warning(_("pairlists are passed as SEXP as from R 2.15.0"));
            dotCode_spa.helper = 0;
	    break;
	}
    }

    SEXP ans1 = NULL;
    if (return_one_named) {
        ans1 = VECTOR_ELT(ans,last_pos);
        if (ans1 != last_arg) DUPLICATE_ATTRIB(ans1, last_arg);
    }

    if (dotCode_spa.helper) {
        /* Ensure arguments won't change while task is not complete (the
           "in use" mechanism won't work on these inner objects). */
        for (na = 0; na < nargs; na++) {
            SEXP arg = VECTOR_ELT(args,na);
            if (NAMEDCNT_GT_0(arg)) SET_NAMEDCNT_MAX(arg);
        }
        SEXP rawfun = allocVector (RAWSXP, sizeof dotCode_fun);
        memcpy (RAW(rawfun), &dotCode_fun, sizeof dotCode_fun);
        helpers_do_task (0, task_dotCode, scalars, ans1, rawfun, ans);
    }
    else
        task_dotCode (scalars, ans1, NULL, ans);

    /* Either return just the one named element, ans1, as a pairlist,
       or the whole vector list of updated arguments, to which we must
       attach the right attributes. */

    if (return_one_named)
        ans = cons_with_tag (ans1, R_NilValue, last_tag);
    else {
        for (na = 0, pa = args ; pa != R_NilValue ; pa = CDR(pa), na++) {
            SEXP arg = CAR(pa);
            SEXP s = VECTOR_ELT (ans, na);
            if (s != arg) DUPLICATE_ATTRIB (s, arg);
        }
    }

    UNPROTECT(1);
    VMAXSET(vmax);
    return ans;
}


static const struct {
    const char *name;
    const SEXPTYPE type;
}

typeinfo[] = {
    {"logical",	  LGLSXP },
    {"integer",	  INTSXP },
    {"double",	  REALSXP},
    {"complex",	  CPLXSXP},
    {"character", STRSXP },
    {"list",	  VECSXP },
    {NULL,	  0      }
};

static int string2type(char *s)
{
    int i;
    for (i = 0 ; typeinfo[i].name ; i++) {
	if(!strcmp(typeinfo[i].name, s)) {
	    return typeinfo[i].type;
	}
    }
    error(_("type \"%s\" not supported in interlanguage calls"), s);
    return 1; /* for -Wall */
}

/* This is entirely legacy, with no known users (Mar 2012).
   So we freeze the code involved. 
 */

static void *RObjToCPtr2(SEXP s)
{
    int n;

    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	n = LENGTH(s);
	int *iptr = INTEGER(s);
	iptr = (int*) R_alloc(n, sizeof(int));
	for (int i = 0 ; i < n ; i++) iptr[i] = INTEGER(s)[i];
	return (void*) iptr;
	break;
    case REALSXP:
	n = LENGTH(s);
	double *rptr = REAL(s);
	rptr = (double*) R_alloc(n, sizeof(double));
	for (int i = 0 ; i < n ; i++) rptr[i] = REAL(s)[i];
	return (void*) rptr;
	break;
    case CPLXSXP:
	n = LENGTH(s);
	Rcomplex *zptr = COMPLEX(s);
	zptr = (Rcomplex*) R_alloc(n, sizeof(Rcomplex));
	for (int i = 0 ; i < n ; i++) zptr[i] = COMPLEX(s)[i];
	return (void*) zptr;
	break;
    case STRSXP:
	n = LENGTH(s);
	char **cptr = (char**) R_alloc(n, sizeof(char*));
	for (int i = 0 ; i < n ; i++) {
	    const char *ss = translateChar(STRING_ELT(s, i));
	    cptr[i] = (char*) R_alloc(strlen(ss) + 1, sizeof(char));
	    strcpy(cptr[i], ss);
	}
	return (void*) cptr;
	break;
	/* From here down, probably not right */
    case VECSXP:
	n = length(s);
	SEXP *lptr = (SEXP *) R_alloc(n, sizeof(SEXP));
	for (int i = 0 ; i < n ; i++) lptr[i] = VECTOR_ELT(s, i);
	return (void*) lptr;
	break;
    default:
	return (void*) s;
    }
}



void call_R(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    SEXP call, pcall, s;
    SEXPTYPE type;
    int i, j, n;

    if (!isFunction((SEXP)func))
	error("invalid function in call_R");
    if (nargs < 0)
	error("invalid argument count in call_R");
    if (nres < 0)
	error("invalid return value count in call_R");
    PROTECT(pcall = call = allocList((int) nargs + 1));
    SET_TYPEOF(call, LANGSXP);
    SETCAR(pcall, (SEXP)func);
    s = R_NilValue;		/* -Wall */
    for (i = 0 ; i < nargs ; i++) {
	pcall = CDR(pcall);
	type = string2type(modes[i]);
	switch(type) {
	case LGLSXP:
	case INTSXP:
	    n = (int) lengths[i];
	    SETCAR(pcall, allocVector(type, n));
	    memcpy(INTEGER(CAR(pcall)), arguments[i], n * sizeof(int));
	    break;
	case REALSXP:
	    n = (int) lengths[i];
	    SETCAR(pcall, allocVector(REALSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(double));
	    break;
	case CPLXSXP:
	    n = (int) lengths[i];
	    SETCAR(pcall, allocVector(CPLXSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(Rcomplex));
	    break;
	case STRSXP:
	    n = (int) lengths[i];
	    SETCAR(pcall, allocVector(STRSXP, n));
	    for (j = 0 ; j < n ; j++) {
		char *str = (char*)(arguments[i]);
		SET_STRING_ELT(CAR(pcall), i, mkChar(str));
	    }
	    break;
	default:
	    error(_("mode '%s' is not supported in call_R"), modes[i]);
	}
	if(names && names[i])
	    SET_TAG(pcall, install(names[i]));
	SET_NAMED(CAR(pcall), 2);
    }
    PROTECT(s = eval(call, R_GlobalEnv));
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	if(nres > 0)
	    results[0] = (char *) RObjToCPtr2(s);
	break;
    case VECSXP:
	n = length(s);
	if (nres < n) n = (int) nres;
	for (i = 0 ; i < n ; i++)
	    results[i] = (char *) RObjToCPtr2(VECTOR_ELT(s, i));
	break;
    case LISTSXP:
	n = length(s);
	if(nres < n) n = (int) nres;
	for(i = 0 ; i < n ; i++) {
	    results[i] = (char *) RObjToCPtr2(s);
	    s = CDR(s);
	}
	break;
    }
    UNPROTECT(2);
    return;
}

void call_S(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    call_R(func, nargs, arguments, modes, lengths, names, nres, results);
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_dotcode[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".C",		do_dotCode,	0,	1001,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode,	1,	1001,	-1,	{PP_FOREIGN, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
