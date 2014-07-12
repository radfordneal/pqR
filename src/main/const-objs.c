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
#include "Defn.h"		/*-> Arith.h -> math.h */


/* This module defines some commonly-used objects as shared constants.
   They are declared as "const" so that many compilers will put them in
   a read-only area of memory.  Care must consequently be taken that they
   are not written to. */


/* Object to link to in gengc_next_node of a constant object in order to 
   identify it as a constant that should be re-created as the same constant 
   when unserializing. */

const SEXPREC R_unserialize_as_constant;


/* Header for a constant.

   Constants must be marked, and be of the oldest generation, so the garbage 
   collector won't fiddle with them. */

#define CONST_HEADER(typ) \
    .sxpinfo = { .nmcnt = 7, .type = typ, .gcgen = 1, .mark = 1 }, \
    .attrib = R_NilValue, \
    .gengc_next_node = (SEXP) &R_unserialize_as_constant,


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  */

const SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP)
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Logical constants. */

#define LOGICAL_CONST(v) { \
    CONST_HEADER(LGLSXP) \
    .vecsxp = { .length = 1 }, \
    .data = { .i = v } \
}

VECTOR_SEXPREC_CONST R_ScalarLogicalNA_const    = LOGICAL_CONST(NA_LOGICAL);
VECTOR_SEXPREC_CONST R_ScalarLogicalFALSE_const = LOGICAL_CONST(FALSE);
VECTOR_SEXPREC_CONST R_ScalarLogicalTRUE_const  = LOGICAL_CONST(TRUE);


/* Integer constants. */

#define INTEGER_CONST(v) { \
    CONST_HEADER(INTSXP) \
    .vecsxp = { .length = 1 }, \
    .data = { .i = v } \
}

VECTOR_SEXPREC_CONST R_ScalarIntegerNA_const = INTEGER_CONST(NA_INTEGER);

VECTOR_SEXPREC_CONST R_ScalarInteger0To10_const[11] = {
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

VECTOR_SEXPREC_CONST R_ScalarRealZero_const = REAL_CONST(0.0);
VECTOR_SEXPREC_CONST R_ScalarRealOne_const = REAL_CONST(1.0);

VECTOR_SEXPREC_CONST R_ScalarRealNA_const = {
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
            { .carval = &car, .cdrval = R_NilValue, .tagval = R_NilValue } \
         } \
}

static const SEXPREC R_list1_constants[] = {
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
