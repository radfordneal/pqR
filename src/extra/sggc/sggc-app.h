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

#ifndef SGGC_APP_H_
#define SGGC_APP_H_

#ifdef HAVE_CONFIG_H
# include <config.h>
#else
# include <Rconfig.h>
#endif


#define SET_STATIC 1       /* Static set procedrues in source, not linked */


/* LENGTH TYPES. */

typedef unsigned sggc_nchunks_t;/* Type for how many chunks are in an object */

typedef int sggc_length_t;      /* Type for holding an object length, which
                                   is the number of chunks, not the R length */

#define sggc_nchunks(type,length) (length)  /* Length is already nchunks */


/* NUMBER OF OBJECT TYPES.  The SGGC types are not the same as the R
   types, partly because of the possible use of SET_TYPE.  Instead,
   there are only the SGGC types below, whose distinctions are useful for
   determining what pointers to follow in an object in find_object_ptrs,
   and to make some objected uncollected.

   These SGGC types are as follows:

       0  No pointers to follow (NILSXP, CHARSXP)
       1  Only attribute pointer to follow (eg, INTSXP)
       2  Attribute pointer plus three others (eg, LISTSXP, SYMSXP)
       3  Attribute plus vector of pointers (VECSXP, EXPRSXP, STRSXP)
       4  Attribute pointer plus one or two others (EXTPTRSXP, S4SXP)
       5  Only attribute pointer to follow, uncollected, used for primitives
          and symbols, which are distinguished by kind (even if same size)
*/

#define SGGC_N_TYPES 6

extern const char R_type_to_sggc_type[32];  /* Initialized in sggc-app.c */

sggc_nchunks_t Rf_nchunks (int /* SEXPTYPE */, int /* R_len_t */);


/* LAYOUT WITH COMPRESSED POINTERS. */

#if USE_COMPRESSED_POINTERS

/*    All:
        attrib in auxiliary information 1
        length in auxiliary information 2 (may be shared read-only constant)

      Cons-type:          Vector:             
        info, car           info, truelength
        cdr, tag            data...
        = 16 bytes          = 16 bytes if length==1 (except for CPLXSXP)
          (1 chunk)           (1 chunk)

      Symbol:             Primitive:                        External pointer:
        info, pname         info, padding                     info, unused/shift
        value, nextsym      C-function                        prot, tag
        lastenv, last-enf   fast-C-function                   external ptr
        lastbinding,padding 64 bits of info                   padding, padding
        = 32 bytes          = 32 bytes                        = 32 bytes
          (2 chunks)          (2 chunks)                        (2 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_AUX1_SIZE 4        /* Lengths of objects */
#define SGGC_AUX1_BLOCK_SIZE 4  /* So blocks are the same size as data blocks */

#define SGGC_AUX1_READ_ONLY     /* Lengths of some non-vectors are constant 1 */

#define sggc_aux1_read_only(kind) ( ( \
 ( ((uint64_t)1 << (SGGC_N_TYPES+2))   /* LISTSXP, etc. */ + \
   ((uint64_t)1 << (2*SGGC_N_TYPES+2)) /* SYMSXP */ ) >> (kind) ) & 1 \
     ? (char *) sggc_length1 : (char *) NULL )

#define SGGC_AUX2_SIZE 4        /* Attribute, as compressed pointer */
#define SGGC_AUX2_BLOCK_SIZE 4  /* So blocks are the same size as data blocks */

#define SGGC_N_KINDS (8*SGGC_N_TYPES)  /* A big kind plus 7 small; must not
                                          exceed 64, given sggc_aux1_read_only
                                          implementation above. */

/* Note: chunks in non-vector types are given by second row below, except
   for EXTPTRSXP and SYMSXP, given by third row. */

#define SGGC_KIND_CHUNKS \
{ 0,   0,   0,   0,   0,   0, /* Kinds for big segments, only types 1 & 3 */ \
  1,   1,   1,   1,   1,   2, /* Smallest sizes for the SGGC types */ \
  2,   2,   2,   2,   2,   2, /* 2nd smallest sizes, unused for type 4 */ \
  3,   4,   2,   4,   2,   2, /* 3rd smallest sizes, unused for types 2,4&5 */ \
  5,   8,   2,   8,   2,   2, /* 4th smallest sizes, unused for types 2,4&5 */ \
  8,  16,   2,  16,   2,   2, /* 5th smallest sizes, unused for types 2,4&5 */ \
 16,  32,   2,  32,   2,   2, /* 6th smallest sizes, unused for types 2,4&5 */ \
 32,  32,   2,  32,   2,   2  /* 7th smallest sizes, only for type 0 */ \
}

#define SGGC_KIND_TYPES \
{ 0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5  \
};

#define SGGC_KIND_UNCOLLECTED \
{ 0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1  \
};

#define SGGC_LIST_KIND (SGGC_N_TYPES + 2)
#define SGGC_ENV_KIND  (SGGC_N_TYPES + 2)
#define SGGC_PROM_KIND (SGGC_N_TYPES + 2)
#define SGGC_CLOS_KIND (SGGC_N_TYPES + 2)
#define SGGC_SYM_KIND  (2*SGGC_N_TYPES + 5)
#define SGGC_SMALL_VEC_KIND (SGGC_N_TYPES + 1)

#define SGGC_TOTAL_BYTES(type,length) \
    ( (R_size_t) Rf_nchunks(type,length) * SGGC_CHUNK_SIZE /* data part */ \
       + 4 /* attribute pointer */ \
       + 4 * ((VECTOR_OR_CHAR_TYPES >> type) & 1) /* length */ )
    
#endif


/* LAYOUT WITH UNCOMPRESSED 64-BIT POINTERS. */

#if !USE_COMPRESSED_POINTERS && SIZEOF_SIZE_T == 8

/*    Cons-type:          Vector:             
        info, cptr          info, cptr  
        attrib              attrib
        length, padding     length, truelength
        car                 data...
        cdr
        tag
        = 48 bytes          = 32 bytes if length==1 (except for CPLXSXP)
          (3 chunks)          (2 chunks)

      Symbol:             Primitive:                        External pointer:
        info, cptr          info, cptr                        info, cptr
        attrib              attrib                            attrib
        length, padding     length, padding                   length, padding
        pname               C-function                        external ptr
        value               fast-C-function                   prot
        nextsym             64 bits of info                   tag
        lastenv             = 48 bytes                        = 48 bytes
        last-enf            (3 chunks)                          (3 chunks)
        lastbinding
        padding
        = 80 bytes
          (5 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (8*SGGC_N_TYPES)  /* A big kind, plus 7 small */

/* Note: chunks in non-vector types are given by second row below, except
   for SYMSXP, given by third row. */

#define SGGC_KIND_CHUNKS \
{ 0,   0,   0,   0,   0,   0, /* Kinds for big segments, only types 1 & 3 */ \
  2,   2,   3,   2,   3,   3, /* Smallest sizes for the SGGC types */ \
  3,   3,   5,   3,   3,   5, /* 2nd smallest sizes, unused for type 4 */ \
  4,   5,   5,   5,   3,   5, /* 3rd smallest sizes, unused for types 2,4&5 */ \
  5,   8,   5,   8,   3,   5, /* 4th smallest sizes, unused for types 2,4&5 */ \
  8,  16,   5,  16,   3,   5, /* 5th smallest sizes, unused for types 2,4&5 */ \
 16,  32,   5,  32,   3,   5, /* 6th smallest sizes, unused for types 2,4&5 */ \
 32,  32,   5,  32,   3,   5  /* 7th smallest sizes, only for type 0 */ \
}

#define SGGC_KIND_TYPES \
{ 0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5  \
};

#define SGGC_KIND_UNCOLLECTED \
{ 0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1  \
};

#define SGGC_LIST_KIND (SGGC_N_TYPES + 2)
#define SGGC_ENV_KIND  (SGGC_N_TYPES + 2)
#define SGGC_PROM_KIND (SGGC_N_TYPES + 2)
#define SGGC_CLOS_KIND (SGGC_N_TYPES + 2)
#define SGGC_SYM_KIND  (2*SGGC_N_TYPES + 5)
#define SGGC_SMALL_VEC_KIND (SGGC_N_TYPES + 1)

#define SGGC_TOTAL_BYTES(type,length) \
    ( (R_size_t) Rf_nchunks(type,length) * SGGC_CHUNK_SIZE )

#endif


/* LAYOUT WITH UNCOMPRESSED 32-BIT POINTERS. */

#if !USE_COMPRESSED_POINTERS && SIZEOF_SIZE_T == 4

/*    Cons-type:          Vector:             
        info, cptr          info, cptr  
        attrib, length      attrib, length
        car, cdr            truelength, padding
        tag, padding        data...
        = 32 bytes          = 32 bytes if length==1 (except for CPLXSXP)
        (2 chunks)          (2 chunks)

      Symbol:             Primitive:                        External pointer:
        info, cptr          info, cptr                        info, cptr
        attrib, length      attrib, length                    attrib, length
        pname, value        C-function, fast-C-function       prot, tag
        nextsym, lastenv    64 bits of info                   xptr, padding
        last-enf, lastbinding     = 32 bytes                  = 32 bytes
        padding, padding            (2 chunks)                  (2 chunks)
        = 48 bytes          
          (3 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (8*SGGC_N_TYPES)  /* A big kind, plus 7 small */

/* Note: chunks in non-vector types are given by second row below, except
   for SYMSXP, given by third row. */

#define SGGC_KIND_CHUNKS \
{ 0,   0,   0,   0,   0,   0, /* Kinds for big segments, only types 1 & 3 */ \
  2,   2,   2,   2,   2,   2, /* Smallest sizes for the SGGC types */ \
  3,   3,   3,   3,   2,   3, /* 2nd smallest sizes, unused for type 4 */ \
  4,   5,   3,   5,   2,   3, /* 3rd smallest sizes, unused for types 2,4&5 */ \
  5,   8,   3,   8,   2,   3, /* 4th smallest sizes, unused for types 2,4&5 */ \
  8,  16,   3,  16,   2,   3, /* 5th smallest sizes, unused for types 2,4&5 */ \
 16,  32,   3,  32,   2,   3, /* 6th smallest sizes, unused for types 2,4&5 */ \
 32,  32,   3,  32,   2,   3  /* 7th smallest sizes, only for type 0 */ \
}

#define SGGC_KIND_TYPES \
{ 0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5, \
  0, 1, 2, 3, 4, 5  \
};

#define SGGC_KIND_UNCOLLECTED \
{ 0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 1  \
};

#define SGGC_LIST_KIND (SGGC_N_TYPES + 2)
#define SGGC_ENV_KIND  (SGGC_N_TYPES + 2)
#define SGGC_PROM_KIND (SGGC_N_TYPES + 2)
#define SGGC_CLOS_KIND (SGGC_N_TYPES + 2)
#define SGGC_SYM_KIND  (2*SGGC_N_TYPES + 5)
#define SGGC_SMALL_VEC_KIND (SGGC_N_TYPES + 1)

#define SGGC_TOTAL_BYTES(type,length) \
    ( (R_size_t) Rf_nchunks(type,length) * SGGC_CHUNK_SIZE )

#endif


#define SGGC_AFTER_MARKING

#define sggc_kind sggc_kind_inline


#include "sggc.h"  /* Include the generic SGGC header file */


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

extern const int32_t sggc_length0[SGGC_CHUNKS_IN_SMALL_SEGMENT];
extern const int32_t sggc_length1[SGGC_CHUNKS_IN_SMALL_SEGMENT];

#endif
