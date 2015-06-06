/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2014 by Radford M. Neal
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif

#include <complex.h>


/* This module defines some commonly-used objects as shared constants.
   They may be declared as "const" so that many compilers will put them in
   a read-only area of memory.  Care must consequently be taken that they
   are not written to.  

   However, cowardice is allowed for, with them not really being "const" 
   (in case somebody insists on writing to them, we hope innocuously), if 
   the R_CONST symbol defined in Rinternals.h is empty (rather than 'const'). */


#include "Defn.h"    /* Includes Rinternals.h, which defines R_CONST */


/* Object to link to in gengc_next_node of a constant object in order to 
   identify it as a constant that should be re-created as the same constant 
   when unserializing. */

R_CONST SEXPREC R_unserialize_as_constant;


/* Header for a constant.

   Constants must be marked, and be of the oldest generation, so the garbage 
   collector won't fiddle with them. */

#define CONST_HEADER(typ) \
    .sxpinfo = { .nmcnt = 7, .type = typ, .gcgen = 1, .mark = 1 }, \
    .attrib = R_NilValue, \
    .gengc_next_node = (SEXP) &R_unserialize_as_constant,


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  */

R_CONST SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP)
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Definition of the R_EmptyEnv constant, whose address when cast to SEXP is 
   R_EmptyEnv.  */

R_CONST SEXPREC R_EmptyEnv_const = { \
    CONST_HEADER(ENVSXP)
    .u = { .envsxp = 
            { .frame = R_NilValue, .enclos = R_NilValue, .hashtab = R_NilValue }
         }
};


/* Definition of the R_UnboundValue constant, whose address when cast to SEXP
   is R_UnboundValue.  */

R_CONST SYM_SEXPREC R_UnboundValue_const = { \
    CONST_HEADER(SYMSXP)
    .symsxp = { .pname = R_NilValue, 
                .value = R_UnboundValue, 
                .internal = R_NilValue,
                .nextsym = R_NilValue
              }
};


/* Logical constants. */

#define LOGICAL_CONST(v) { \
    CONST_HEADER(LGLSXP) \
    .vecsxp = { .length = 1 }, \
    .data = { .i = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarLogicalNA_const    = LOGICAL_CONST(NA_LOGICAL);
R_CONST VECTOR_SEXPREC_C R_ScalarLogicalFALSE_const = LOGICAL_CONST(FALSE);
R_CONST VECTOR_SEXPREC_C R_ScalarLogicalTRUE_const  = LOGICAL_CONST(TRUE);


/* Integer constants. */

#define INTEGER_CONST(v) { \
    CONST_HEADER(INTSXP) \
    .vecsxp = { .length = 1 }, \
    .data = { .i = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarIntegerNA_const = INTEGER_CONST(NA_INTEGER);

R_CONST VECTOR_SEXPREC_C R_ScalarInteger0To10_const[11] = {
    INTEGER_CONST(0), INTEGER_CONST(1), INTEGER_CONST(2), INTEGER_CONST(3),
    INTEGER_CONST(4), INTEGER_CONST(5), INTEGER_CONST(6), INTEGER_CONST(7),
    INTEGER_CONST(8), INTEGER_CONST(9), INTEGER_CONST(10) 
};


/* Real constants. */

#define REAL_CONST(v) { \
    CONST_HEADER(REALSXP) \
    .vecsxp = { .length = 1 }, \
    .data = { .d = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarRealZero_const = REAL_CONST(0.0);
R_CONST VECTOR_SEXPREC_C R_ScalarRealOne_const = REAL_CONST(1.0);

R_CONST VECTOR_SEXPREC_C R_ScalarRealNA_const = {
    CONST_HEADER(REALSXP)
    .vecsxp = { .length = 1 },
#ifdef WORDS_BIGENDIAN
    .data = { .w = { 0x7ff00000, 1954 } }
#else
    .data = { .w = { 1954, 0x7ff00000 } }
#endif
};


/* 1-element pairlist constants. */

#define LIST1_CONST(car) { \
  CONST_HEADER(LISTSXP) \
  .u = { .listsxp = \
         { .carval = (SEXP) &car, .cdrval = R_NilValue, .tagval = R_NilValue } \
       } \
}

static R_CONST SEXPREC R_list1_constants[] = {
    LIST1_CONST(R_ScalarLogicalNA_const),
    LIST1_CONST(R_ScalarLogicalFALSE_const),
    LIST1_CONST(R_ScalarLogicalTRUE_const),
    LIST1_CONST(R_ScalarIntegerNA_const),
    LIST1_CONST(R_ScalarInteger0To10_const[0]),
    LIST1_CONST(R_ScalarInteger0To10_const[1]),
    LIST1_CONST(R_ScalarInteger0To10_const[2]),
    LIST1_CONST(R_ScalarInteger0To10_const[3]),
    LIST1_CONST(R_ScalarInteger0To10_const[4]),
    LIST1_CONST(R_ScalarInteger0To10_const[5]),
    LIST1_CONST(R_ScalarInteger0To10_const[6]),
    LIST1_CONST(R_ScalarInteger0To10_const[7]),
    LIST1_CONST(R_ScalarInteger0To10_const[8]),
    LIST1_CONST(R_ScalarInteger0To10_const[9]),
    LIST1_CONST(R_ScalarInteger0To10_const[10]),
    LIST1_CONST(R_ScalarRealZero_const),
    LIST1_CONST(R_ScalarRealOne_const),
    LIST1_CONST(R_ScalarRealNA_const),
    LIST1_CONST(R_NilValue_const) /* may be used, and also signals end of list*/
};


/* Return the CONS of the "car" argument with R_NilValue, as a shared 
   constant if possible.  The argument need not be protected by the caller
   before the call. */

SEXP attribute_hidden MaybeConstList1(SEXP car)
{
    for (int i = 0; ; i++) {
        SEXP c = (SEXP) &R_list1_constants[i];
        if (CAR(c) == car) 
            return c;
        if (CAR(c) == R_NilValue)
            return CONS(car,R_NilValue);
    }
}


/* Statically allocated boxes for return when VARIANT_STATIC_BOX_OK is used.
   These are not actually constant, since the data they contain is changed,
   but are allocated similarly.  These boxes must be marked, and be of the 
   oldest generation, so the garbage collector won't fiddle with them.  They
   are marked as shared boxes with a special value for genc_next_node. */

R_CONST SEXPREC R_static_box;

#define SCALAR_BOX(typ) { \
    .sxpinfo = { .nmcnt = 7, .type = typ, .gcgen = 1, .mark = 1 }, \
    .attrib = R_NilValue, \
    .gengc_next_node = (SEXP) &R_static_box, \
    .vecsxp = { .length = 1 } }

VECTOR_SEXPREC_C R_ScalarIntegerBox_space = SCALAR_BOX(INTSXP);
VECTOR_SEXPREC_C R_ScalarRealBox_space = SCALAR_BOX(REALSXP);

VECTOR_SEXPREC_C R_ScalarIntegerBox0_space = SCALAR_BOX(INTSXP);
VECTOR_SEXPREC_C R_ScalarRealBox0_space = SCALAR_BOX(REALSXP);


/* Evaluate two arguments that may be put in static boxes.  The two
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

SEXP attribute_hidden static_box_eval2 
                        (SEXP args, SEXP *arg1, SEXP *arg2, SEXP env, SEXP call)
{
    SEXP argsevald;
    SEXP x, y;

    x = CAR(args); 
    y = CADR(args);

    /* We evaluate by the general procedure if ... present or more than
       two arguments, not trying to put args in static boxes. */

    if (x==R_DotsSymbol || y==R_DotsSymbol || CDDR(args)!=R_NilValue) {
        argsevald = evalList (args, env, call);
        x = CAR(argsevald);
        y = CADR(argsevald);
        goto rtrn;
    }

    /* Otherwise, we try to put the first arg in a static box. */

    PROTECT(x = evalv (x, env, VARIANT_STATIC_BOX_OK | VARIANT_PENDING_OK));

    /* If first arg is an object, we evaluate the rest of the arguments
       normally. */

    if (isObject(x)) {
        argsevald = evalList (CDR(args), env, call);
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

    y = evalv (y, env, VARIANT_STATIC_BOX_OK | VARIANT_PENDING_OK);

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
        argsevald = evalList (CDDR(args), env, call);
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


/* Initialize variables holding constant values, for those who need them
   as variables (eg, RStudio).  Need to first undefine their macro forms,
   which were defined in Rinternals.h. */

#undef R_NilValue
SEXP R_NilValue = (SEXP) &R_NilValue_const;

#undef R_EmptyEnv
SEXP R_EmptyEnv =  (SEXP) &R_EmptyEnv_const;

#undef R_UnboundValue
SEXP R_UnboundValue =  (SEXP) &R_UnboundValue_const;
