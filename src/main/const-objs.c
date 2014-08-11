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


/* Header for a constant. */

#define CONST_HEADER(typ) \
    .sxpinfo = { .nmcnt = 7, .type = typ, .gcgen = 1, .mark = 1 }, \
    .attrib = R_NilValue,


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  Declared as "const" so that many compilers will put it in a 
   read-only area of memory. 

   Must be marked, and be of the oldest generation, so the garbage collector
   won't fiddle with it. */

const SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP)
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Definition of the R_EmptyEnv constant, whose address when cast to SEXP is 
   R_EmptyEnv.  */

const SEXPREC R_EmptyEnv_const = { \
    CONST_HEADER(ENVSXP)
    .u = { .envsxp = 
            { .frame = R_NilValue, .enclos = R_NilValue, .hashtab = R_NilValue }
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
