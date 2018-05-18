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
#elif SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
#   define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),
#   define LENGTH1 .length = 1,
#   define LENGTH1_NONVEC /* nothing */
#   define NILATTRIB /* nothing */
#else
#   define CPTR_FIELD(index,offset) .cptr = SGGC_CPTR_VAL(index,offset),
#   define LENGTH1 .length = 1,
#   define LENGTH1_NONVEC .length = 1,
#   define NILATTRIB .attrib = R_NilValue,
#endif

#define NUM_OFFSET(o) (SGGC_SCALAR_CHUNKS*o)
#define CONS_OFFSET(o) (SGGC_CONS_CHUNKS*o)


/* Header for a scalar constant. */

#define CONST_HEADER(typ,index,offset) \
    CPTR_FIELD(index,offset) \
    NILATTRIB \
    .sxpinfo = { .nmcnt = 7, .type_et_cetera = typ }


/* Header for an ASCII CHARSXP constant. */

#define CONST_HEADER_CHAR(index,offset) \
    CPTR_FIELD(index,offset) \
    NILATTRIB \
    .sxpinfo = { .nmcnt = 7, \
                 .type_et_cetera = CHARSXP, \
                 .rstep_pname = 1,  /* may not always be, but OK to say so */ \
                 .gp = ASCII_MASK }


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
   More than one segment may be used, which must have consecutive indexes. */

#define SCALAR_STACK_VALUE(offset) { \
    CONST_HEADER (RAWSXP, R_SGGC_SCALAR_STACK_INDEX \
                   + SGGC_SCALAR_CHUNKS*offset/SGGC_CHUNKS_IN_SMALL_SEGMENT, \
                  NUM_OFFSET(offset)), LENGTH1 }

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
    SCALAR_STACK_VALUE(30),  SCALAR_STACK_VALUE(31),

    SCALAR_STACK_VALUE(32),  SCALAR_STACK_VALUE(33),
    SCALAR_STACK_VALUE(34),  SCALAR_STACK_VALUE(35),
    SCALAR_STACK_VALUE(36),  SCALAR_STACK_VALUE(37),
    SCALAR_STACK_VALUE(38),  SCALAR_STACK_VALUE(39),
    SCALAR_STACK_VALUE(40),  SCALAR_STACK_VALUE(41),
    SCALAR_STACK_VALUE(42),  SCALAR_STACK_VALUE(43),
    SCALAR_STACK_VALUE(44),  SCALAR_STACK_VALUE(45),
    SCALAR_STACK_VALUE(46),  SCALAR_STACK_VALUE(47),

    SCALAR_STACK_VALUE(48),  SCALAR_STACK_VALUE(49),
    SCALAR_STACK_VALUE(50),  SCALAR_STACK_VALUE(51),
    SCALAR_STACK_VALUE(52),  SCALAR_STACK_VALUE(53),
    SCALAR_STACK_VALUE(54),  SCALAR_STACK_VALUE(55),
    SCALAR_STACK_VALUE(56),  SCALAR_STACK_VALUE(57),
    SCALAR_STACK_VALUE(58),  SCALAR_STACK_VALUE(59),
    SCALAR_STACK_VALUE(60),  SCALAR_STACK_VALUE(61),
    SCALAR_STACK_VALUE(62),  SCALAR_STACK_VALUE(63),

    SCALAR_STACK_VALUE(64),  SCALAR_STACK_VALUE(65),
    SCALAR_STACK_VALUE(66),  SCALAR_STACK_VALUE(67),
    SCALAR_STACK_VALUE(68),  SCALAR_STACK_VALUE(69),
    SCALAR_STACK_VALUE(70),  SCALAR_STACK_VALUE(71),
    SCALAR_STACK_VALUE(72),  SCALAR_STACK_VALUE(73),
    SCALAR_STACK_VALUE(74),  SCALAR_STACK_VALUE(75),
    SCALAR_STACK_VALUE(76),  SCALAR_STACK_VALUE(77),
    SCALAR_STACK_VALUE(78),  SCALAR_STACK_VALUE(79),

    SCALAR_STACK_VALUE(80),  SCALAR_STACK_VALUE(81),
    SCALAR_STACK_VALUE(82),  SCALAR_STACK_VALUE(83),
    SCALAR_STACK_VALUE(84),  SCALAR_STACK_VALUE(85),
    SCALAR_STACK_VALUE(86),  SCALAR_STACK_VALUE(87),
    SCALAR_STACK_VALUE(88),  SCALAR_STACK_VALUE(89),
    SCALAR_STACK_VALUE(90),  SCALAR_STACK_VALUE(91),
    SCALAR_STACK_VALUE(92),  SCALAR_STACK_VALUE(93),
    SCALAR_STACK_VALUE(94),  SCALAR_STACK_VALUE(95),

    SCALAR_STACK_VALUE(96),  SCALAR_STACK_VALUE(97),
    SCALAR_STACK_VALUE(98),  SCALAR_STACK_VALUE(99),
    SCALAR_STACK_VALUE(100), SCALAR_STACK_VALUE(101),
    SCALAR_STACK_VALUE(102), SCALAR_STACK_VALUE(103),
    SCALAR_STACK_VALUE(104), SCALAR_STACK_VALUE(105),
    SCALAR_STACK_VALUE(106), SCALAR_STACK_VALUE(107),
    SCALAR_STACK_VALUE(108), SCALAR_STACK_VALUE(109),
    SCALAR_STACK_VALUE(110), SCALAR_STACK_VALUE(111),

    SCALAR_STACK_VALUE(112), SCALAR_STACK_VALUE(113),
    SCALAR_STACK_VALUE(114), SCALAR_STACK_VALUE(115),
    SCALAR_STACK_VALUE(116), SCALAR_STACK_VALUE(117),
    SCALAR_STACK_VALUE(118), SCALAR_STACK_VALUE(119),
    SCALAR_STACK_VALUE(120), SCALAR_STACK_VALUE(121),
    SCALAR_STACK_VALUE(122), SCALAR_STACK_VALUE(123),
    SCALAR_STACK_VALUE(124), SCALAR_STACK_VALUE(125),
    SCALAR_STACK_VALUE(126), SCALAR_STACK_VALUE(127)
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


/* Small integer constants. */

#define SMALL_INT_CONST(v,offset) { \
    CONST_HEADER(INTSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .i = v } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarInteger_consts[32] = {

    /* At most 32 fit in one segment (since up to 2 chunks each) */

    SMALL_INT_CONST(0,0),   SMALL_INT_CONST(1,1),   SMALL_INT_CONST(2,2),
    SMALL_INT_CONST(3,3),   SMALL_INT_CONST(4,4),   SMALL_INT_CONST(5,5),
    SMALL_INT_CONST(6,6),   SMALL_INT_CONST(7,7),   SMALL_INT_CONST(8,8), 
    SMALL_INT_CONST(9,9),   SMALL_INT_CONST(10,10), SMALL_INT_CONST(11,11),
    SMALL_INT_CONST(12,12), SMALL_INT_CONST(13,13), SMALL_INT_CONST(14,14),
    SMALL_INT_CONST(15,15), SMALL_INT_CONST(16,16), SMALL_INT_CONST(17,17),
    SMALL_INT_CONST(18,18), SMALL_INT_CONST(19,19), SMALL_INT_CONST(20,20),
    SMALL_INT_CONST(21,21), SMALL_INT_CONST(22,22), SMALL_INT_CONST(23,23),
    SMALL_INT_CONST(24,24), SMALL_INT_CONST(25,25), SMALL_INT_CONST(26,26),
    SMALL_INT_CONST(27,27), SMALL_INT_CONST(28,28), SMALL_INT_CONST(29,29),
    SMALL_INT_CONST(30,30), SMALL_INT_CONST(31,31)
};


/* Miscellaneious integer, logical, and real constants. */

#define INTEGER_CONST(v,offset) { \
    CONST_HEADER(INTSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .i = v } \
}

#define LOGICAL_CONST(v,offset) { \
    CONST_HEADER(LGLSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .i = v } \
}

#define REAL_CONST(v,offset) { \
    CONST_HEADER(REALSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .d = v } \
}

#ifdef WORDS_BIGENDIAN
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .w = { 0x7ff00000, 1954 } } \
}
#else
#define REAL_NA_CONST(offset) { \
    CONST_HEADER(REALSXP,R_SGGC_MISC_INDEX,NUM_OFFSET(offset)), \
    LENGTH1 \
    .data = { .w = { 1954, 0x7ff00000 } } \
}
#endif

R_CONST VECTOR_SEXPREC_C R_ScalarMisc_consts[7] = {

    /* At most 32 fit in one segment (since up to 2 chunks each) */

    LOGICAL_CONST(FALSE,0),
    LOGICAL_CONST(TRUE,1),
    LOGICAL_CONST(NA_LOGICAL,2),

    INTEGER_CONST(NA_INTEGER,3),

    REAL_CONST(0.0,4),
    REAL_CONST(1.0,5),
    REAL_NA_CONST(6)
};


/* ASCII CHARSXP constants.  Put in four segments (which may not 
   be full), so that the number of segments won't vary with the
   configuration.  The entry of ASCII code 0 is not used.

   These are not put in the string hash table, but do have the correct
   hash code, since it may be used for other purposes. */

#define HASH1CHAR(c) ((5381*33) + c)  /* keep in sync with Rf_char_hash */

#define CHARSXP_CONST(v,index) { \
    CONST_HEADER_CHAR(index,NUM_OFFSET(v&0x1f)), \
    LENGTH1 \
    .truelength = HASH1CHAR(v), \
    .data = { .c = v } \
}

/* Should really be declared as R_CONST, but radixsort actually
   fiddles with TRUELENGTH, so we can't do that. */

/* R_CONST */ VECTOR_SEXPREC_C R_ASCII_consts[128] = {

    CHARSXP_CONST(0x00,R_SGGC_CHAR_INDEX+0),  /* unused (and wrong) */
    CHARSXP_CONST(0x01,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x02,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x03,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x04,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x05,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x06,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x07,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x08,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x09,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0a,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0b,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0c,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0d,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0e,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x0f,R_SGGC_CHAR_INDEX+0),

    CHARSXP_CONST(0x10,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x11,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x12,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x13,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x14,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x15,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x16,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x17,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x18,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x19,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1a,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1b,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1c,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1d,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1e,R_SGGC_CHAR_INDEX+0),
    CHARSXP_CONST(0x1f,R_SGGC_CHAR_INDEX+0),

    CHARSXP_CONST(0x20,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x21,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x22,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x23,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x24,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x25,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x26,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x27,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x28,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x29,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2a,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2b,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2c,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2d,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2e,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x2f,R_SGGC_CHAR_INDEX+1),

    CHARSXP_CONST(0x30,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x31,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x32,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x33,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x34,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x35,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x36,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x37,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x38,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x39,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3a,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3b,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3c,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3d,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3e,R_SGGC_CHAR_INDEX+1),
    CHARSXP_CONST(0x3f,R_SGGC_CHAR_INDEX+1),

    CHARSXP_CONST(0x40,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x41,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x42,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x43,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x44,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x45,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x46,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x47,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x48,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x49,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4a,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4b,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4c,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4d,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4e,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x4f,R_SGGC_CHAR_INDEX+2),

    CHARSXP_CONST(0x50,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x51,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x52,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x53,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x54,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x55,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x56,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x57,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x58,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x59,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5a,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5b,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5c,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5d,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5e,R_SGGC_CHAR_INDEX+2),
    CHARSXP_CONST(0x5f,R_SGGC_CHAR_INDEX+2),

    CHARSXP_CONST(0x60,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x61,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x62,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x63,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x64,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x65,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x66,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x67,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x68,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x69,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6a,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6b,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6c,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6d,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6e,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x6f,R_SGGC_CHAR_INDEX+3),

    CHARSXP_CONST(0x70,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x71,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x72,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x73,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x74,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x75,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x76,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x77,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x78,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x79,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7a,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7b,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7c,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7d,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7e,R_SGGC_CHAR_INDEX+3),
    CHARSXP_CONST(0x7f,R_SGGC_CHAR_INDEX+3),
};


/* ASCII scalar STRSXP constants.  Put in four segments (which may not
   be full), so that the number of segments won't vary with the
   configuration.  The entry for ASCII code 0 is not used. */

#define STRSXP_CONST(v,index) { \
    CONST_HEADER(STRSXP,index,NUM_OFFSET(v&0x1f)), \
    LENGTH1 \
    .data = { .p = (SEXP) &R_ASCII_consts[v] } \
}

R_CONST VECTOR_SEXPREC_C R_ScalarString_consts[128] = {

    STRSXP_CONST(0x00,R_SGGC_STRING_INDEX+0),  /* unused */
    STRSXP_CONST(0x01,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x02,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x03,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x04,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x05,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x06,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x07,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x08,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x09,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0a,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0b,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0c,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0d,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0e,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x0f,R_SGGC_STRING_INDEX+0),

    STRSXP_CONST(0x10,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x11,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x12,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x13,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x14,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x15,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x16,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x17,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x18,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x19,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1a,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1b,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1c,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1d,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1e,R_SGGC_STRING_INDEX+0),
    STRSXP_CONST(0x1f,R_SGGC_STRING_INDEX+0),

    STRSXP_CONST(0x20,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x21,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x22,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x23,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x24,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x25,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x26,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x27,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x28,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x29,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2a,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2b,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2c,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2d,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2e,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x2f,R_SGGC_STRING_INDEX+1),

    STRSXP_CONST(0x30,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x31,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x32,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x33,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x34,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x35,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x36,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x37,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x38,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x39,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3a,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3b,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3c,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3d,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3e,R_SGGC_STRING_INDEX+1),
    STRSXP_CONST(0x3f,R_SGGC_STRING_INDEX+1),

    STRSXP_CONST(0x40,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x41,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x42,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x43,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x44,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x45,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x46,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x47,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x48,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x49,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4a,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4b,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4c,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4d,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4e,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x4f,R_SGGC_STRING_INDEX+2),

    STRSXP_CONST(0x50,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x51,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x52,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x53,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x54,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x55,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x56,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x57,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x58,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x59,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5a,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5b,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5c,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5d,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5e,R_SGGC_STRING_INDEX+2),
    STRSXP_CONST(0x5f,R_SGGC_STRING_INDEX+2),

    STRSXP_CONST(0x60,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x61,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x62,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x63,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x64,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x65,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x66,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x67,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x68,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x69,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6a,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6b,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6c,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6d,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6e,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x6f,R_SGGC_STRING_INDEX+3),

    STRSXP_CONST(0x70,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x71,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x72,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x73,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x74,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x75,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x76,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x77,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x78,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x79,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7a,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7b,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7c,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7d,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7e,R_SGGC_STRING_INDEX+3),
    STRSXP_CONST(0x7f,R_SGGC_STRING_INDEX+3),
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

    /* At most 21 fit in one segment (since up to 3 chunks each) */

    LIST1_CONST(R_ScalarInteger0To31(0),0),
    LIST1_CONST(R_ScalarInteger0To31(1),1),
    LIST1_CONST(R_ScalarInteger0To31(2),2),
    LIST1_CONST(R_ScalarInteger0To31(3),3),
    LIST1_CONST(R_ScalarInteger0To31(4),4),
    LIST1_CONST(R_ScalarInteger0To31(5),5),
    LIST1_CONST(R_ScalarInteger0To31(6),6),
    LIST1_CONST(R_ScalarInteger0To31(7),7),
    LIST1_CONST(R_ScalarInteger0To31(8),8),
    LIST1_CONST(R_ScalarInteger0To31(9),9),
    LIST1_CONST(R_ScalarInteger0To31(10),10),
    LIST1_CONST(R_ScalarInteger0To31(11),11),
    LIST1_CONST(R_ScalarInteger0To31(12),12),

    LIST1_CONST(R_ScalarLogicalNA,13),
    LIST1_CONST(R_ScalarLogicalFALSE,14),
    LIST1_CONST(R_ScalarLogicalTRUE,15),
    LIST1_CONST(R_ScalarIntegerNA,16),
    LIST1_CONST(R_ScalarRealZero,17),
    LIST1_CONST(R_ScalarRealOne,18),
    LIST1_CONST(R_ScalarRealNA,19),

    LIST1_CONST(R_NilValue,20) /* may be used, and also signals end of list*/
};


/* Return the CONS of the "car" argument with R_NilValue, as a shared 
   constant if possible.  The argument need not be protected by the caller
   before the call. */

SEXP attribute_hidden MaybeConstList1(SEXP car)
{
    int list_chunks
          = sggc_kind_chunks [R_type_to_sggc_type[LISTSXP] + SGGC_N_TYPES];
    for (int i = 0; ; i++) {
        SEXP c;
        c = SEXP_FROM_CPTR (SGGC_CPTR_VAL (R_SGGC_LIST1_INDEX, i*list_chunks));
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

    /*** R_NilValue (= R NULL). ****/

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

    /*** Environment constant. ***/

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

    /*** Symbol constant. ***/

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

    /*** Small integer constants. ***/

    p = sggc_constant (R_type_to_sggc_type[INTSXP],
                       R_type_to_sggc_type[INTSXP]+SGGC_N_TYPES,
                       32, (char *) R_ScalarInteger_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_INT_INDEX) abort();

    /*** Miscellaneous integer, logical, and real constants. ***/

    if (R_type_to_sggc_type[INTSXP] != R_type_to_sggc_type[LGLSXP]) abort();
    if (R_type_to_sggc_type[REALSXP] != R_type_to_sggc_type[LGLSXP]) abort();

    p = sggc_constant (R_type_to_sggc_type[LGLSXP],
                       R_type_to_sggc_type[LGLSXP]+SGGC_N_TYPES,
                       7, (char *) R_ScalarMisc_consts
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_MISC_INDEX) abort();

    /*** ASCII CHARSXP constants.  In four segments. ***/

    p = sggc_constant (R_type_to_sggc_type[CHARSXP],
                       R_type_to_sggc_type[CHARSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ASCII_consts + 0)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_CHAR_INDEX+0) abort();

    p = sggc_constant (R_type_to_sggc_type[CHARSXP],
                       R_type_to_sggc_type[CHARSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ASCII_consts + 32)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_CHAR_INDEX+1) abort();

    p = sggc_constant (R_type_to_sggc_type[CHARSXP],
                       R_type_to_sggc_type[CHARSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ASCII_consts + 64)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_CHAR_INDEX+2) abort();

    p = sggc_constant (R_type_to_sggc_type[CHARSXP],
                       R_type_to_sggc_type[CHARSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ASCII_consts + 96)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_CHAR_INDEX+3) abort();

    /*** String constants.  In four segments. ***/

    p = sggc_constant (R_type_to_sggc_type[STRSXP],
                       R_type_to_sggc_type[STRSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ScalarString_consts + 0)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_STRING_INDEX+0) abort();

    p = sggc_constant (R_type_to_sggc_type[STRSXP],
                       R_type_to_sggc_type[STRSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ScalarString_consts + 32)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_STRING_INDEX+1) abort();

    p = sggc_constant (R_type_to_sggc_type[STRSXP],
                       R_type_to_sggc_type[STRSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ScalarString_consts + 64)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_STRING_INDEX+2) abort();

    p = sggc_constant (R_type_to_sggc_type[STRSXP],
                       R_type_to_sggc_type[STRSXP]+SGGC_N_TYPES,
                       32, (char *) (R_ScalarString_consts + 96)
#if USE_COMPRESSED_POINTERS
                       , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                       , (char *) nilattrib
#endif
                      );

    if (SGGC_SEGMENT_INDEX(p) != R_SGGC_STRING_INDEX+3) abort();

    /*** Pairlists of length 1. ***/

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

    /*** Scalar stack space.  Uses same segments for integers and reals. ***/

    /* Assumes SGGC_SCALAR_CHUNKS is a power of two. */

    if (R_type_to_sggc_type[INTSXP] != R_type_to_sggc_type[REALSXP]) abort();
    if ((SGGC_SCALAR_CHUNKS & (SGGC_SCALAR_CHUNKS-1)) != 0) abort();

    for (int i = 0; 
         i < SCALAR_STACK_SIZE; 
         i += SGGC_CHUNKS_IN_SMALL_SEGMENT/SGGC_SCALAR_CHUNKS) {
        p = sggc_constant (R_type_to_sggc_type[REALSXP],
                           R_type_to_sggc_type[REALSXP]+SGGC_N_TYPES,
                           SGGC_CHUNKS_IN_SMALL_SEGMENT/SGGC_SCALAR_CHUNKS,
                           (char *) (R_scalar_stack_space + i)
#if USE_COMPRESSED_POINTERS
                           , (char *) sggc_length1, (char *) nilattrib
#elif USE_AUX_FOR_ATTRIB
                           , (char *) nilattrib
#endif
                          );
        if (i == 0 && SGGC_SEGMENT_INDEX(p)!=R_SGGC_SCALAR_STACK_INDEX) abort();
    }


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
