/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 Radford M. Neal
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


/* LENGTH TYPES. */

typedef int sggc_length_t;      /* Type for holding an object length - must 
                                   match R_len_t. */

typedef unsigned sggc_nchunks_t;/* Type for how many chunks are in a segment */


/* NUMBER OF OBJECT TYPES.  Allows for 5-bit type field.  This will be
   the "base" type for an object, not the full type, so that SET_TYPE
   can work. */

#define SGGC_N_TYPES 32


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

#define SGGC_N_KINDS (3*SGGC_N_TYPES)  /* A big kind, plus two small */

#define SGGC_KIND_CHUNKS \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* Kinds for big segments */ \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*  - not all used        */ \
  3, /* NILSXP */                                 /* Kinds for non-vector   */ \
  5, /* SYMSXP */                                 /*   objects and shortest */ \
  3, /* LISTSXP */                                /*   vectors              */ \
  3, /* CLOSXP */ \
  3, /* ENVSXP */ \
  3, /* PROMSXP */ \
  3, /* LANGSXP */ \
  3, /* SPECIALSXP */ \
  3, /* BUILTINSXP */ \
  2, /* CHARSXP */ \
  2, /* LGLSXP */ \
  1, /* unused */ \
  1, /* unused */ \
  2, /* INTSXP */ \
  2, /* REALSXP */ \
  3, /* CPLXSXP */ \
  2, /* STRSXP */ \
  3, /* DOTSXP */ \
  3, /* unused */ \
  2, /* VECSXP */ \
  2, /* EXPRSXP */ \
  3, /* BCODESXP */ \
  3, /* EXTPTRSXP */ \
  3, /* WEAKREFSXP */ \
  2, /* RAWSXP */ \
  3, /* S4SXP */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* NILSXP */                                 /* Kinds for 2nd-smallest */ \
  1, /* SYMSXP */                                 /*   sizes of vectors     */ \
  1, /* LISTSXP */ \
  1, /* CLOSXP */ \
  1, /* ENVSXP */ \
  1, /* PROMSXP */ \
  1, /* LANGSXP */ \
  1, /* SPECIALSXP */ \
  1, /* BUILTINSXP */ \
  3, /* CHARSXP */ \
  3, /* LGLSXP */ \
  1, /* unused */ \
  1, /* unused */ \
  3, /* INTSXP */ \
  3, /* REALSXP */ \
  4, /* CPLXSXP */ \
  3, /* STRSXP */ \
  1, /* DOTSXP */ \
  1, /* unused */ \
  3, /* VECSXP */ \
  3, /* EXPRSXP */ \
  1, /* BCODESXP */ \
  1, /* EXTPTRSXP */ \
  1, /* WEAKREFSXP */ \
  3, /* RAWSXP */ \
  1, /* S4SXP */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1  /* unused */ \
}

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

#define SGGC_N_KINDS (2*SGGC_N_TYPES)  /* For now,
  one small size plus big */

#define SGGC_KIND_CHUNKS \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* Kinds for big segments */ \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*  - not all used        */ \
  2, /* NILSXP */                                 /* Kinds for non-vector   */ \
  3, /* SYMSXP */                                 /*   objects shortest     */ \
  2, /* LISTSXP */                                /*   vectors              */ \
  2, /* CLOSXP */ \
  2, /* ENVSXP */ \
  2, /* PROMSXP */ \
  2, /* LANGSXP */ \
  3, /* SPECIALSXP */ \
  3, /* BUILTINSXP */ \
  2, /* CHARSXP */ \
  2, /* LGLSXP */ \
  1, /* unused */ \
  1, /* unused */ \
  2, /* INTSXP */ \
  2, /* REALSXP */ \
  3, /* CPLXSXP */ \
  2, /* STRSXP */ \
  2, /* DOTSXP */ \
  1, /* unused */ \
  2, /* VECSXP */ \
  2, /* EXPRSXP */ \
  2, /* BCODESXP */ \
  2, /* EXTPTRSXP */ \
  2, /* WEAKREFSXP */ \
  2, /* RAWSXP */ \
  2, /* S4SXP */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* NILSXP */                                 /* Kinds for 2nd-smallest */ \
  1, /* SYMSXP */                                 /*   sizes of vectors     */ \
  1, /* LISTSXP */ \
  1, /* CLOSXP */ \
  1, /* ENVSXP */ \
  1, /* PROMSXP */ \
  1, /* LANGSXP */ \
  1, /* SPECIALSXP */ \
  1, /* BUILTINSXP */ \
  3, /* CHARSXP */ \
  3, /* LGLSXP */ \
  1, /* unused */ \
  1, /* unused */ \
  3, /* INTSXP */ \
  3, /* REALSXP */ \
  4, /* CPLXSXP */ \
  3, /* STRSXP */ \
  1, /* DOTSXP */ \
  1, /* unused */ \
  3, /* VECSXP */ \
  3, /* EXPRSXP */ \
  1, /* BCODESXP */ \
  1, /* EXTPTRSXP */ \
  1, /* WEAKREFSXP */ \
  3, /* RAWSXP */ \
  1, /* S4SXP */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1, /* unused */ \
  1  /* unused */ \
}

#endif

#define SGGC_AFTER_MARKING

void sggc_app_init (void);

/* Include the generic SGGC header file. */

#include "sggc.h"
