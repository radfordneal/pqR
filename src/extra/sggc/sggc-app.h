/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016, 2017 Radford M. Neal
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

#include <Defn.h>
#undef SGGC_SEGMENT_INDEX


/* LENGTH TYPES. */

typedef unsigned sggc_nchunks_t;/* Type for how many chunks are in an object */

typedef int sggc_length_t;      /* Type for holding an object length, which
                                   is the number of chunks, not the R length */

#define sggc_nchunks(type,length) (length)  /* Length is already nchunks */


/* NUMBER OF OBJECT TYPES.  The SGGC types are not the same as the R
   types, partly because of the possible use of SET_TYPE.  Instead,
   there are only five SGGC types, whose distinctions are useful for
   determining what pointers to follow in an object in find_object_ptrs.

   These SGGC types are as follows:

       0  No pointers to follow (NILSXP, CHARSXP)
       1  Only attribute pointer to follow (eg, INTSXP, BUILTINSXP)
       2  Attribute pointer plus three others (eg, LISTSXP, SYMSXP)
       3  Attribute plus vector of pointers (VECSXP, EXPRSXP, STRSXP)
       4  Attribute pointer plus one or two others (EXTPTRSXP, S4SXP)
*/

#define SGGC_N_TYPES 5

const char R_type_to_sggc_type[32];

sggc_nchunks_t Rf_nchunks (SEXPTYPE, R_len_t);


/* LAYOUT WITH UNCOMPRESSED 64-BIT POINTERS. */

#if SIZEOF_SIZE_T == 8

/*    Cons-type:          Vector:             
        info(64bits)        info(64bits)
        cptr, length        cptr, length
        attrib              attrib
        car                 data...
        cdr
        tag
        = 48 bytes          = 32 bytes if length==1 (except for CPLXSXP)
          (3 chunks)          (2 chunks)

      Symbol:             Primitive:
        info(64bits)        info(64bits)
        cptr, length        cptr, length
        attrib              attrib
        pname               C-function
        value               fast-C-function
        internal            64 bits of info
        nextsym             = 48 bytes
        lastenv             (3 chunks)
        lastbinding
        lastenvnotfound
        = 80 bytes
          (5 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (8*SGGC_N_TYPES)  /* A big kind, plus 7 small */

/* Note: chunks in non-vector types are given by second row below, except
   for BUILTINSXP, SPECIALSXP, and SYMSXP, given by third row. */

#define SGGC_KIND_CHUNKS \
{ 0,   0,   0,   0,   0,        /* Kinds for big segments, only types 1 & 3 */ \
  2,   2,   3,   2,   3,        /* Smallest sizes for the SGGC types */ \
  3,   3,   5,   3,   3,        /* 2nd smallest sizes, unused for type 4 */ \
  4,   5,   5,   5,   3,        /* 3rd smallest sizes, unused for types 2&4 */ \
  5,   8,   5,   8,   3,        /* 4th smallest sizes, unused for types 2&4 */ \
  8,  16,   5,  16,   3,        /* 5th smallest sizes, unused for types 2&4 */ \
 16,  32,   5,  32,   3,        /* 6th smallest sizes, unused for types 2&4 */ \
 32,  32,   5,  32,   3         /* 7th smallest sizes, only for type 0 */ \
}

#define SGGC_MIN_CHUNKS 2       /* Smallest size for any small kind */

#endif


/* LAYOUT WITH UNCOMPRESSED 32-BIT POINTERS. */

#if SIZEOF_SIZE_T == 4

/*    Cons-type:          Vector:             
        info(64bits)        info(64bits)
        cptr, length        cptr, length
        attrib, car         attrib, padding
        cdr, tag            data...
        = 32 bytes          = 32 bytes if length==1 (except for CPLXSXP)
        (2 chunks)          (2 chunks)

      Symbol:             Primitive:
        info(64bits)        info(64bits)
        cptr, length        cptr, length
        attrib, pname       attrib, C-function
        value, internal     fast-C-function, 32 bits of info
        nextsym, lastenv    32 bits of info, padding
        lastbinding, lastenvnotfound         padding, padding
        = 48 bytes                             = 48 bytes
          (3 chunks)                             (3 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (8*SGGC_N_TYPES)  /* A big kind, plus 7 small */


/* Note: chunks in non-vector types are given by second row below, except
   for BUILTINSXP, SPECIALSXP, and SYMSXP, given by third row. */

#define SGGC_KIND_CHUNKS \
{ 0,   0,   0,   0,   0,        /* Kinds for big segments, only types 1 & 3 */ \
  2,   2,   2,   2,   2,        /* Smallest sizes for the SGGC types */ \
  3,   3,   3,   3,   2,        /* 2nd smallest sizes, unused for type 4 */ \
  4,   5,   3,   5,   2,        /* 3rd smallest sizes, unused for types 2&4 */ \
  5,   8,   3,   8,   2,        /* 4th smallest sizes, unused for types 2&4 */ \
  8,  16,   3,  16,   2,        /* 5th smallest sizes, unused for types 2&4 */ \
 16,  32,   3,  32,   2,        /* 6th smallest sizes, unused for types 2&4 */ \
 32,  32,   3,  32,   2         /* 7th smallest sizes, only for type 0 */ \
}

#define SGGC_MIN_CHUNKS 2       /* Smallest size for any small kind */

#endif


#define SGGC_AFTER_MARKING


#define sggc_kind sggc_kind_inline

/* The sggc_find_object_ptrs procedure may be included after sggc.h,
   as a "static inline" procedure, or defined as an external procedure
   in memory.c, according to whether sggc_find_object_ptrs is defined
   as sggc_find_object_ptrs_inline here, or left undefined. */

#define sggc_find_object_ptrs sggc_find_object_ptrs_inline

#include "sggc.h"  /* Include the generic SGGC header file */

#ifdef sggc_find_object_ptrs
#include "sggc-find-ptrs.c"
#endif

static inline sggc_kind_t sggc_kind_inline (sggc_type_t type, 
                                            sggc_length_t length)
{ 
    int k = SGGC_N_TYPES + type;

    do {
        if (length <= sggc_kind_chunks[k])
            return k;
        k += SGGC_N_TYPES;
    } while (k < SGGC_N_KINDS);        

    return type;  /* kind for a big segment */
}
