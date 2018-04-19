/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2015, 2016, 2017, 2018 by Radford M. Neal
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


#define NEED_SGGC_FUNCTIONS
#include "Defn.h"          /* Includes Rinternals.h, which defines R_CONST */
#include <complex.h>


#if USE_COMPRESSED_POINTERS
#   define CPTR_FIELD(index,offset) /* nothing */
#   define LENGTH1 /* nothing */
#   define LENGTH1_NONVEC /* nothing */
#   define NILATTRIB /* nothing */
#   define NUM_OFFSET(o) (o)
#   define CONS_OFFSET(o) (o)
#elif SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
#   define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),
#   define LENGTH1 .length = 1,
#   define LENGTH1_NONVEC /* nothing */
#   define NILATTRIB /* nothing */
#   define NUM_OFFSET(o) (2*(o))
#   define CONS_OFFSET(o) (2*(o))
#else
#   define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),
#   define LENGTH1 .length = 1,
#   define LENGTH1_NONVEC .length = 1,
#   define NILATTRIB .attrib = R_NilValue,
#   if SIZEOF_CHAR_P == 4
#       define NUM_OFFSET(o) (2*(o))
#       define CONS_OFFSET(o) (2*(o))
#   else
#       define NUM_OFFSET(o) (2*(o))
#       define CONS_OFFSET(o) (3*(o))
#   endif
#endif


/* Header for a scalar constant. */

#define CONST_HEADER(typ,index,offset) \
    CPTR_FIELD(index,offset) \
    NILATTRIB \
    .sxpinfo = { .nmcnt = 7, .type_et_cetera = typ } \


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  */

R_CONST SEXPREC R_NilValue_const = { \
    CONST_HEADER(NILSXP,R_SGGC_NIL_INDEX,0),
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};


/* Scalar stack, on which values may be returned when VARIANT_SCALAR_STACK_OK
   is used.  These objects are not actually constant, since the data they 
   contain is changed, but are allocated similarly.  Types are initialized
   to RAWSXP, which is never used, so the high-water mark will be visible
   for debugging and tuning. */

#if USE_COMPRESSED_POINTERS
#define SCALAR_STACK_VALUE(offset) { \
    CONST_HEADER(RAWSXP,R_SGGC_SCALAR_STACK_INDEX,NUM_OFFSET(offset)) }
#else
#define SCALAR_STACK_VALUE(offset) { \
    CONST_HEADER(RAWSXP,R_SGGC_SCALAR_STACK_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 }
#endif

VECTOR_SEXPREC_C R_scalar_stack_space[SCALAR_STACK_SIZE] = {
    SCALAR_STACK_VALUE(0),   SCALAR_STACK_VALUE(1),
    SCALAR_STACK_VALUE(2),   SCALAR_STACK_VALUE(3),
    SCALAR_STACK_VALUE(4),   SCALAR_STACK_VALUE(5),
    SCALAR_STACK_VALUE(6),   SCALAR_STACK_VALUE(7),
    SCALAR_STACK_VALUE(8),   SCALAR_STACK_VALUE(9),
    SCALAR_STACK_VALUE(10),  SCALAR_STACK_VALUE(11),
    SCALAR_STACK_VALUE(12),  SCALAR_STACK_VALUE(13),
    SCALAR_STACK_VALUE(14),  SCALAR_STACK_VALUE(15),
    SCALAR_STACK_VALUE(16),  SCALAR_STACK_VALUE(17),
    SCALAR_STACK_VALUE(18),  SCALAR_STACK_VALUE(19),
    SCALAR_STACK_VALUE(20),  SCALAR_STACK_VALUE(21),
    SCALAR_STACK_VALUE(22),  SCALAR_STACK_VALUE(23),
    SCALAR_STACK_VALUE(24),  SCALAR_STACK_VALUE(25),
    SCALAR_STACK_VALUE(26),  SCALAR_STACK_VALUE(27),
    SCALAR_STACK_VALUE(28),  SCALAR_STACK_VALUE(29),
    SCALAR_STACK_VALUE(20),  SCALAR_STACK_VALUE(31)
};


/* Definition of the R_EmptyEnv constant, whose address when cast to SEXP is 
   R_EmptyEnv.  Leave LENGTH (if present) as zero. */

R_CONST ENV_SEXPREC R_env_consts[1] = {
{
    CONST_HEADER(ENVSXP,R_SGGC_ENV_INDEX,0),
    .frame = R_NilValue, 
    .enclos = R_NilValue, 
    .hashtab = R_NilValue,
    .envsymbits = ~(R_symbits_t)0     /* all 1s, so SKIP_USING_SYMBITS will */
}                                     /*   stop at empty environment        */
};


/* Definition of the R_UnboundValue constant, whose address when cast to SEXP
   is R_UnboundValue.  Don't put in read-only memory, so won't have to special
   case it when clearing LASTSYMENV and LASTENVNOTFOUND.  Leave LENGTH
   (if it exists) as zero. */

SYM_SEXPREC R_sym_consts[1] = { 
{
    CONST_HEADER(SYMSXP,R_SGGC_SYM_INDEX,0),
    .pname = R_NilValue,
    .value = R_UnboundValue,
    .symbits = 0                    /* all 0s, so will always look for this */
}                                   /*   (and presumably not find it)       */
};


/* Logical, integer, and real constants. */

#define LOGICAL_CONST(v,offset) { \
    CONST_HEADER(LGLSXP,R_SGGC_NUM_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .i = v } \
}

#define INTEGER_CONST(v,offset) { \
    CONST_HEADER(INTSXP,R_SGGC_NUM_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .i = v } \
}

#define REAL_CONST(v,offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .d = v } \
}

#ifdef WORDS_BIGENDIAN
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .w = { 0x7ff00000, 1954 } } \
}
#else
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_NUM_INDEX,NUM_OFFSET(offset)), \
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


/* 1-element pairlist constants.  Set LENGTH (if it exists) to 1. */

#define LIST1_CONST(car,offset) { \
    CONST_HEADER(LISTSXP,R_SGGC_LIST1_INDEX,CONS_OFFSET(offset)), \
    LENGTH1_NONVEC \
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
        SEXP c = SEXP_FROM_CPTR (SGGC_CPTR_VAL (R_SGGC_LIST1_INDEX, i*list_chunks));
        if (CAR(c) == car) 
            return c;
        if (CAR(c) == R_NilValue)
            return CONS(car,R_NilValue);
    }
}


/* Initialize constants (and scalar stack). */

#if USE_COMPRESSED_POINTERS || USE_AUX_FOR_ATTRIB

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
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_NIL_INDEX) abort();

    /* Environment constant. */

    p = sggc_constant (R_type_to_sggc_type[ENVSXP],
                       R_type_to_sggc_type[ENVSXP]+SGGC_N_TYPES,
                       1, (char *) R_env_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_ENV_INDEX) abort();

    /* Symbol constant. */

    p = sggc_constant (R_type_to_sggc_type[SYMSXP],
                       R_type_to_sggc_type[SYMSXP]+2*SGGC_N_TYPES,
                       1, (char *) R_sym_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
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
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
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
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_LIST1_INDEX) abort();

    /* Scalar stack space.  Uses same segment for integers and reals. */

    if (R_type_to_sggc_type[INTSXP] != R_type_to_sggc_type[REALSXP]) abort();

    p = sggc_constant (R_type_to_sggc_type[REALSXP],
                       R_type_to_sggc_type[REALSXP]+SGGC_N_TYPES,
                       32, (char *) R_scalar_stack_space
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_SCALAR_STACK_INDEX) abort();

}


/* Initialize variables holding constant values, for those who need them
   as variables (eg, RStudio).  Need to first undefine their macro forms,
   which were defined in Rinternals.h. */

#undef R_NilValue
#if USE_COMPRESSED_POINTERS
SEXP R_NilValue = SGGC_CPTR_VAL(R_SGGC_NIL_INDEX,0);
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
