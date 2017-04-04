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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif


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
#include <complex.h>


#if USE_COMPRESSED_POINTERS
#define CPTR_FIELD(index,offset) /* nothing */
#define LENGTH1 /* nothing */
#define NILATTRIB /* nothing */
#else
#define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),
#define LENGTH1 .length = 1,
#define NILATTRIB .attrib = R_NilValue,
#endif


/* Header for a constant. */

#define CONST_HEADER(typ,index,offset) \
    CPTR_FIELD(index,offset) \
    NILATTRIB \
    .sxpinfo = { .nmcnt = 7, .type = typ, } \


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  */

R_CONST SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP,R_SGGC_NIL_INDEX,0),
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Statically allocated boxes for return when VARIANT_STATIC_BOX_OK is used.
   These are not actually constant, since the data they contain is changed,
   but are allocated similarly. */

#if USE_COMPRESSED_POINTERS
#define SCALAR_BOX(typ,offset) { \
    CONST_HEADER(typ,R_SGGC_STATIC_BOXES_INDEX,offset) }
#else
#define SCALAR_BOX(typ,offset) { \
    CONST_HEADER(typ,R_SGGC_STATIC_BOXES_INDEX,offset), \
    LENGTH1 }
#endif

VECTOR_SEXPREC_C R_ScalarBox_space[4] = {
    SCALAR_BOX(INTSXP,0),
    SCALAR_BOX(INTSXP,1),
    SCALAR_BOX(REALSXP,2),
    SCALAR_BOX(REALSXP,3)
};


/* Definition of the R_EmptyEnv constant, whose address when cast to SEXP is 
   R_EmptyEnv.  */

R_CONST SEXPREC R_env_consts[1] = {
{
    CONST_HEADER(ENVSXP,R_SGGC_ENV_INDEX,0),
    .u = { .envsxp = 
            { .frame = R_NilValue, .enclos = R_NilValue, .hashtab = R_NilValue }
         }
}
};


/* Definition of the R_UnboundValue constant, whose address when cast to SEXP
   is R_UnboundValue.  Don't put in read-only memory, so won't have to special
   case it when clearing LASTSYMENV and LASTSYMENVNOTFOUND. */

SYM_SEXPREC R_sym_consts[1] = { 
{
    CONST_HEADER(SYMSXP,R_SGGC_SYM_INDEX,0),
    .symsxp = { .pname = R_NilValue, 
                .value = R_UnboundValue, 
                .nextsym = R_NilValue
              }
}
};


/* Logical, integer, and real constants. */

#define LOGICAL_CONST(v,offset) { \
    CONST_HEADER(LGLSXP,R_SGGC_NUM_INDEX,offset), \
    LENGTH1 \
    .data = { .i = v } \
}

#define INTEGER_CONST(v,offset) { \
    CONST_HEADER(INTSXP,R_SGGC_NUM_INDEX,offset), \
    LENGTH1 \
    .data = { .i = v } \
}

#define REAL_CONST(v,offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,offset), \
    LENGTH1 \
    .data = { .d = v } \
}

#ifdef WORDS_BIGENDIAN
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,offset), \
    LENGTH1 \
    .data = { .w = { 0x7ff00000, 1954 } } \
}
#else
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,offset), \
    LENGTH1 \
    .data = { .w = { 1954, 0x7ff00000 } } \
}
#endif

R_CONST VECTOR_SEXPREC_C R_ScalarNumerical_consts[R_N_NUM_CONSTS] = {

    LOGICAL_CONST(FALSE,0),
    LOGICAL_CONST(TRUE,1),
    LOGICAL_CONST(NA_LOGICAL,2),

    INTEGER_CONST(0,3), INTEGER_CONST(1,4), INTEGER_CONST(2,5),
    INTEGER_CONST(3,6), INTEGER_CONST(4,7), INTEGER_CONST(5,8),
    INTEGER_CONST(6,9), INTEGER_CONST(7,10), INTEGER_CONST(8,11), 
    INTEGER_CONST(9,12), INTEGER_CONST(10,13),
    INTEGER_CONST(NA_INTEGER,14),

    REAL_CONST(0.0,15),
    REAL_CONST(1.0,16),
    REAL_NA_CONST(17)
};


/* 1-element pairlist constants. */

#define LIST1_CONST(car,offset) { \
  CONST_HEADER(LISTSXP,R_SGGC_LIST1_INDEX,offset), \
  .u = { .listsxp = \
          { .carval = car, .cdrval = R_NilValue, .tagval = R_NilValue } \
       } \
}

static R_CONST SEXPREC R_List1_consts[] = {
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
    int list_chunks
          = sggc_kind_chunks [R_type_to_sggc_type[LISTSXP] + SGGC_N_TYPES];
    for (int i = 0; ; i++) {
        SEXP c = SEXP_PTR (SGGC_CPTR_VAL (R_SGGC_LIST1_INDEX, i*list_chunks));
        if (CAR(c) == car) 
            return c;
        if (CAR(c) == R_NilValue)
            return CONS(car,R_NilValue);
    }
}


/* Initialize constants (and static boxes). */

#if USE_COMPRESSED_POINTERS

static const SEXP nilattrib[SGGC_CHUNKS_IN_SMALL_SEGMENT] = {
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,

    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,

    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,

    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue, R_NilValue, R_NilValue
};

#endif

void Rf_constant_init(void)
{
    sggc_cptr_t p;

    /* R_NilValue (= R NULL). */

    p = sggc_constant (R_type_to_sggc_type[NILSXP],
                       R_type_to_sggc_type[NILSXP]+SGGC_N_TYPES, 
                       1, (char *) &R_NilValue_const
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length0, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_NIL_INDEX) abort();

    /* Static boxes.  Uses same segment for integers and reals. */

    if (R_type_to_sggc_type[INTSXP] != R_type_to_sggc_type[REALSXP]) abort();

    p = sggc_constant (R_type_to_sggc_type[INTSXP],
                       R_type_to_sggc_type[INTSXP]+SGGC_N_TYPES,
                       4, (char *) R_ScalarBox_space
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_STATIC_BOXES_INDEX) abort();

    /* Environment constant. */

    p = sggc_constant (R_type_to_sggc_type[ENVSXP],
                       R_type_to_sggc_type[ENVSXP]+SGGC_N_TYPES,
                       1, (char *) R_env_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_ENV_INDEX) abort();

    /* Symbol constant. */

    p = sggc_constant (R_type_to_sggc_type[SYMSXP],
                       R_type_to_sggc_type[SYMSXP]+2*SGGC_N_TYPES,
                       1, (char *) R_sym_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_SYM_INDEX) abort();

    /* Numerical/logical constants.  All use same segment. */

    if (R_type_to_sggc_type[INTSXP] != R_type_to_sggc_type[LGLSXP]) abort();
    if (R_type_to_sggc_type[REALSXP] != R_type_to_sggc_type[LGLSXP]) abort();

    p = sggc_constant (R_type_to_sggc_type[LGLSXP],
                       R_type_to_sggc_type[LGLSXP]+SGGC_N_TYPES,
                       R_N_NUM_CONSTS, (char *) R_ScalarNumerical_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_NUM_INDEX) abort();

    /* Pairlists of length 1. */

    p = sggc_constant (R_type_to_sggc_type[LISTSXP],
                       R_type_to_sggc_type[LISTSXP]+SGGC_N_TYPES,
                       sizeof R_List1_consts / sizeof R_List1_consts[0],
                       (char *) R_List1_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_LIST1_INDEX) abort();

}


/* Initialize variables holding constant values, for those who need them
   as variables (eg, RStudio).  Need to first undefine their macro forms,
   which were defined in Rinternals.h. */

#undef R_NilValue
#if USE_COMPRESSED_POINTERS
SEXP R_NilValue = SGGC_CPTR_VAL(R_SGGC_NIL_INDEX,0);  /* Should be zero */
#else
SEXP R_NilValue = (SEXP) &R_NilValue_const;
#endif

#undef R_EmptyEnv
#if USE_COMPRESSED_POINTERS
SEXP R_EmptyEnv = SGGC_CPTR_VAL(R_SGGC_ENV_INDEX,0);
#else
SEXP R_EmptyEnv = (SEXP) &R_env_consts[0];
#endif

#undef R_UnboundValue
#if USE_COMPRESSED_POINTERS
SEXP R_UnboundValue = SGGC_CPTR_VAL(R_SGGC_SYM_INDEX,0);
#else
SEXP R_UnboundValue = (SEXP) &R_sym_consts[0];
#endif
