/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Development Core Team.
 *  Copyright (C) 2003-4	    The R Foundation
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


/* Inline function used by operators that can take operands in static boxes.

   Evaluates two arguments that may be put in static boxes.  The two
   arguments are returned in arg1 and arg2.  A list of the evaluated
   arguments is returned as the value of the function, if one of the
   first two is an object, so that dispatch must be attempted (note
   that the argument count in this case may not be two).  If neither
   is an object, a list with the correct number of arguments is 
   returned, but they may (or may not) be the unevaluated arguments. 

   If an argument is an object, all arguments will have been computed
   before return from this function, but evaluation of arguments may 
   be pending if neither operand is an object.

   Note that if there are less than two arguments, the missing ones will
   appear here to be R_NilValue (since CAR(R_NilValue) is R_NilValue).

   The args and env arguments must be protected by the caller. */

static inline SEXP static_box_eval2 
                     (SEXP args, SEXP *arg1, SEXP *arg2, SEXP env, SEXP call)
{
    SEXP argsevald;
    SEXP x, y;

    x = CAR(args); 
    y = CADR(args);

    /* We evaluate by the general procedure if ... present or more than
       two arguments, not trying to put args in static boxes. */

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        argsevald = evalList (args, env);
        x = CAR(argsevald);
        y = CADR(argsevald);
        goto rtrn;
    }

    /* Otherwise, we try to put the first arg in a static box. */

    PROTECT(x = EVALV (x, env, VARIANT_STATIC_BOX_OK | VARIANT_PENDING_OK));

    /* If first arg is an object, we evaluate the rest of the arguments
       normally. */

    if (isObject(x)) {
        argsevald = evalList (CDR(args), env);
        y = CAR(argsevald);
        argsevald = cons_with_tag (x, argsevald, TAG(args));
        UNPROTECT(1); /* x */
        WAIT_UNTIL_COMPUTED(x);
        goto rtrn;
    }

    /* If there's no second argument, we can return now. */

    if (y == R_NilValue) {
        UNPROTECT(1);
        argsevald = args;
        goto rtrn;
    }

    /* If we are keeping x in a static box, we need to save its
       value in a local variable, and switch to the other box,
       since the second argument might use a static box too. */

    int intv; double realv;  /* for saving a boxed x value */
    if (x == R_ScalarRealBox) {
        realv = *REAL(x);
        x = R_ScalarRealBox0;
    }
    else if (x == R_ScalarIntegerBox) {
        intv = *INTEGER(x);
        x = R_ScalarIntegerBox0;
    }

    y = EVALV (y, env, VARIANT_STATIC_BOX_OK | VARIANT_PENDING_OK);

    if (x == R_ScalarRealBox0)
        *REAL(x) = realv;
    else if (x == R_ScalarIntegerBox0)
        *INTEGER(x) = intv;

    /* If the second arg is an object, we have to duplicate the first
       arg if it is in a static box, and create the list of evaluated
       arguments. */

    if (isObject(y)) {
        UNPROTECT(1); /* x */
        PROTECT(y);
        if (IS_STATIC_BOX(x))
            x = duplicate(x);
        PROTECT(x);
        argsevald = evalList (CDDR(args), env);
        argsevald = cons_with_tag (y, argsevald, TAG(CDR(args)));
        argsevald = cons_with_tag (x, argsevald, TAG(args));
        UNPROTECT(2); /* x & y */
        WAIT_UNTIL_COMPUTED_2(x,y);
        goto rtrn;
    }

    /* If neither of the first two arguments are an object, we
       don't look at any possible remaining arguments.  The caller
       is responsible for reporting an error if any are present,
       but we assist by returning the unevaluated arguments, which
       in this case (no ...) number the same as the actual arguments. */

    UNPROTECT(1); /* x */
    argsevald = args;

  rtrn:
    *arg1 = x;
    *arg2 = y;

    return argsevald;
}
