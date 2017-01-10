/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015, 2016 by Radford M. Neal
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif

#include <complex.h>
#include <sggc/sggc-app.h>


/* This module defines some commonly-used objects as shared constants.
   They may be declared as "const" so that many compilers will put them in
   a read-only area of memory.  Care must consequently be taken that they
   are not written to.  

   The Rf_constant_init function must be called when initializing memory
   management, after sggc_init.

   However, cowardice is allowed for, with them not really being "const" 
   (in case somebody insists on writing to them, we hope innocuously), if 
   the R_CONST symbol defined in Rinternals.h is empty (rather than 'const'). */


#include "Defn.h"    /* Includes Rinternals.h, which defines R_CONST */


/* Header for a constant. */

#define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),

#define CONST_HEADER(typ,index,offset) \
    CPTR_FIELD(index,offset) \
    .sxpinfo = { .nmcnt = 7, .type = typ, }, \
    .attrib = R_NilValue,

/* Segment indexes for constant segments. */

#define NIL_INDEX 0
#define ENV_INDEX 1
#define SYM_INDEX 2
#define LGL_INDEX 3
#define INT_INDEX 4
#define REAL_INDEX 5
#define LIST1_INDEX 6
#define INT_BOXES_INDEX 7
#define REAL_BOXES_INDEX 8


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  */

R_CONST SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP,NIL_INDEX,0)
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Definition of the R_EmptyEnv constant, whose address when cast to SEXP is 
   R_EmptyEnv.  */

#define N_ENV_CONSTS 1

R_CONST SEXPREC R_env_consts[N_ENV_CONSTS] = {
{
    CONST_HEADER(ENVSXP,ENV_INDEX,0)
    .u = { .envsxp = 
            { .frame = R_NilValue, .enclos = R_NilValue, .hashtab = R_NilValue }
         }
}
};


/* Definition of the R_UnboundValue constant, whose address when cast to SEXP
   is R_UnboundValue.  */

#define N_SYM_CONSTS 1

R_CONST SYM_SEXPREC R_sym_consts[N_SYM_CONSTS] = { 
{
    CONST_HEADER(SYMSXP,SYM_INDEX,0)
    .symsxp = { .pname = R_NilValue, 
                .value = R_UnboundValue, 
                .internal = R_NilValue,
                .nextsym = R_NilValue
              }
}
};


/* Logical constants. */

#define N_LGL_CONSTS 3

#define LOGICAL_CONST(v,offset) { \
    CONST_HEADER(LGLSXP,LGL_INDEX,offset) \
    .length = 1, \
    .data = { .i = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarLogical_consts[N_LGL_CONSTS] = {
    LOGICAL_CONST(FALSE,0),
    LOGICAL_CONST(TRUE,1),
    LOGICAL_CONST(NA_LOGICAL,2)
};


/* Integer constants. */

#define N_INT_CONSTS 12

#define INTEGER_CONST(v,offset) { \
    CONST_HEADER(INTSXP,INT_INDEX,offset) \
    .length = 1, \
    .data = { .i = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarInteger_consts[N_INT_CONSTS] = {
    INTEGER_CONST(0,0), INTEGER_CONST(1,1), INTEGER_CONST(2,2),
    INTEGER_CONST(3,3), INTEGER_CONST(4,4), INTEGER_CONST(5,5),
    INTEGER_CONST(6,6), INTEGER_CONST(7,7), INTEGER_CONST(8,8), 
    INTEGER_CONST(9,9), INTEGER_CONST(10,10),
    INTEGER_CONST(NA_INTEGER,11)
};


/* Real constants. */

#define N_REAL_CONSTS 3

#define REAL_CONST(v,offset) { \
    CONST_HEADER(REALSXP,REAL_INDEX,offset) \
    .length = 1, \
    .data = { .d = v } \
}

#ifdef WORDS_BIGENDIAN
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,REAL_INDEX,offset) \
    .length = 1, \
    .data = { .w = { 0x7ff00000, 1954 } } \
}
#else
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,REAL_INDEX,offset) \
    .length = 1, \
    .data = { .w = { 1954, 0x7ff00000 } } \
}
#endif

R_CONST VECTOR_SEXPREC_C R_ScalarReal_consts[N_REAL_CONSTS] = {
    REAL_CONST(0.0,0),
    REAL_CONST(1.0,1),
    REAL_NA_CONST(2)
};


/* 1-element pairlist constants. */

#define N_LIST1_CONSTS 19

#define LIST1_CONST(car,offset) { \
  CONST_HEADER(LISTSXP,LIST1_INDEX,offset) \
  .u = { .listsxp = \
          { .carval = car, .cdrval = R_NilValue, .tagval = R_NilValue } \
       } \
}

static R_CONST SEXPREC R_List1_consts[N_LIST1_CONSTS] = {
    LIST1_CONST(R_ScalarInteger0To10(0),0),
    LIST1_CONST(R_ScalarInteger0To10(1),1),
    LIST1_CONST(R_ScalarInteger0To10(2),2),
    LIST1_CONST(R_ScalarInteger0To10(3),3),
    LIST1_CONST(R_ScalarInteger0To10(4),4),
    LIST1_CONST(R_ScalarInteger0To10(5),5),
    LIST1_CONST(R_ScalarInteger0To10(6),6),
    LIST1_CONST(R_ScalarInteger0To10(7),7),
    LIST1_CONST(R_ScalarInteger0To10(8),8),
    LIST1_CONST(R_ScalarInteger0To10(9),9),
    LIST1_CONST(R_ScalarInteger0To10(10),10),
    LIST1_CONST(R_ScalarLogicalNA,11),
    LIST1_CONST(R_ScalarLogicalFALSE,12),
    LIST1_CONST(R_ScalarLogicalTRUE,13),
    LIST1_CONST(R_ScalarIntegerNA,14),
    LIST1_CONST(R_ScalarRealZero,15),
    LIST1_CONST(R_ScalarRealOne,16),
    LIST1_CONST(R_ScalarRealNA,17),
    LIST1_CONST(R_NilValue,18) /* may be used, and also signals end of list*/
};


/* Return the CONS of the "car" argument with R_NilValue, as a shared 
   constant if possible.  The argument need not be protected by the caller
   before the call. */

SEXP attribute_hidden MaybeConstList1(SEXP car)
{
    int list_chunks = sggc_kind_chunks[SGGC_N_TYPES+LISTSXP];
    for (int i = 0; ; i++) {
        SEXP c = SEXP_PTR (SGGC_CPTR_VAL (LIST1_INDEX, i*list_chunks));
        if (CAR(c) == car) 
            return c;
        if (CAR(c) == R_NilValue)
            return CONS(car,R_NilValue);
    }
}


/* Statically allocated boxes for return when VARIANT_STATIC_BOX_OK is used.
   These are not actually constant, since the data they contain is changed,
   but are allocated similarly.  They are marked as static boxes by a flag
   in sxpinfo. */

#define SCALAR_BOX(typ,index,offset) { \
    CPTR_FIELD(index,offset) \
    .sxpinfo = { .nmcnt = 7, .type = typ, .static_box = 1 }, \
    .attrib = R_NilValue, \
    .length = 1 }

VECTOR_SEXPREC_C R_ScalarIntegerBox_space[2] = {
    SCALAR_BOX(INTSXP,INT_BOXES_INDEX,0),
    SCALAR_BOX(INTSXP,INT_BOXES_INDEX,1)
};

VECTOR_SEXPREC_C R_ScalarRealBox_space[2] = {
    SCALAR_BOX(REALSXP,REAL_BOXES_INDEX,0),
    SCALAR_BOX(REALSXP,REAL_BOXES_INDEX,1)
};


/* Initialize constants (and static boxes). */

void Rf_constant_init(void)
{
    sggc_constant (R_type_to_sggc_type[NILSXP],
                   R_type_to_sggc_type[NILSXP]+SGGC_N_TYPES, 
                   1, (char *) &R_NilValue_const);

    sggc_constant (R_type_to_sggc_type[ENVSXP],
                   R_type_to_sggc_type[ENVSXP]+SGGC_N_TYPES,
                   N_ENV_CONSTS, (char *) R_env_consts);

    sggc_constant (R_type_to_sggc_type[SYMSXP],
                   R_type_to_sggc_type[SYMSXP]+2*SGGC_N_TYPES,
                   N_SYM_CONSTS, (char *) R_sym_consts);

    sggc_constant (R_type_to_sggc_type[LGLSXP],
                   R_type_to_sggc_type[LGLSXP]+SGGC_N_TYPES,
                   N_LGL_CONSTS, (char *) R_ScalarLogical_consts);

    sggc_constant (R_type_to_sggc_type[INTSXP],
                   R_type_to_sggc_type[INTSXP]+SGGC_N_TYPES,
                   N_INT_CONSTS, (char *) R_ScalarInteger_consts);

    sggc_constant (R_type_to_sggc_type[REALSXP],
                   R_type_to_sggc_type[REALSXP]+SGGC_N_TYPES,
                   N_REAL_CONSTS, (char *) R_ScalarReal_consts);

    sggc_constant (R_type_to_sggc_type[LISTSXP],
                   R_type_to_sggc_type[LISTSXP]+SGGC_N_TYPES,
                   N_LIST1_CONSTS, (char *) R_List1_consts);

    sggc_constant (R_type_to_sggc_type[INTSXP],
                   R_type_to_sggc_type[INTSXP]+SGGC_N_TYPES,
                   2, (char *) R_ScalarIntegerBox_space);

    sggc_constant (R_type_to_sggc_type[REALSXP],
                   R_type_to_sggc_type[REALSXP]+SGGC_N_TYPES,
                   2, (char *) R_ScalarRealBox_space);
}


/* Initialize variables holding constant values, for those who need them
   as variables (eg, RStudio).  Need to first undefine their macro forms,
   which were defined in Rinternals.h. */

#undef R_NilValue
SEXP R_NilValue = (SEXP) &R_NilValue_const;

#undef R_EmptyEnv
SEXP R_EmptyEnv =  (SEXP) &R_env_consts[0];

#undef R_UnboundValue
SEXP R_UnboundValue =  (SEXP) &R_sym_consts[0];
