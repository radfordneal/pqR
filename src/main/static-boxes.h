/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Core Team.
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


/* Inline function used by operators that can take operands in static boxes,
   and that can handle unclassed objects (VARIANT_UNCLASS_FLAG).

   Evaluates two arguments that may be put in static boxes.  The two
   arguments are returned in arg1 and arg2, and whether they are
   objects (accounting for VARIANT_UNCLASS_FLAG) in obj1 and obj2. 
   A list of the evaluated arguments is returned as the value of the
   function, if one of the first two is an object, so that dispatch
   must be attempted (note that the argument count in this case may
   not be two).  If neither is an object, a list with the correct
   number of arguments is returned, but they may (or may not) be the
   unevaluated arguments.

   If an argument is an object, all arguments will have been computed
   before return from this function, but evaluation of arguments may 
   be pending if neither operand is an object.

   Note that if there are less than two arguments, the missing ones will
   appear here to be R_NilValue (since CAR(R_NilValue) is R_NilValue).

   The args and env arguments must be protected by the caller. */

static inline SEXP static_box_eval2 (SEXP args, SEXP *arg1, SEXP *arg2,
                     int *obj1, int *obj2, SEXP env, SEXP call, int variant)
{
    SEXP argsevald;
    SEXP x, y;
    int o1, o2;

    argsevald = evalList (args, env);
    x = CAR(argsevald);
    y = CADR(argsevald);
    o1 = isObject(x);
    o2 = isObject(y);

    *arg1 = x;
    *arg2 = y;
    *obj1 = o1;
    *obj2 = o2;

    return argsevald;
}
