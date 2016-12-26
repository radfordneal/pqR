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
        lastenv             (4 chunks)
        lastbinding
        lastenvnotfound
        = 80 bytes
          (5 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (2*SGGC_N_TYPES)  /* For now, one small size plus big */

#define SGGC_KIND_CHUNKS \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  3, 5, 3, 3, 3, 3, 3, 4, 4, 2, 2, 3, 3, 2, 2, 3, \
  2, 3, 3, 2, 2, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3  \
}

#endif


/* LAYOUT WITH UNCOMPRESSED 32-BIT POINTERS. */

#if SIZEOF_SIZE_T == 4

/*    Cons-type:          Vector:             
        info(64bits)        info(64bits)
        cptr                cptr
        length              length
        attrib              attrib
        car                 padding
        cdr                 data...
        tag                 = 32 bytes if length==1 (except for CPLXSXP)
        = 32 bytes          (2 chunks)
          (2 chunks)

      Symbol:             Primitive:
        info(64bits)        info(64bits)
        cptr                cptr
        length              length
        truelength          truelength
        attrib              attrib
        pname               C-function
        value               fast-C-function
        internal            32 bits of info
        nextsym             32 bits of info
        lastenv             padding x 3
        lastbinding         = 48 bytes
        lastenvnotfound       (3 chunks)
        = 48 bytes
          (3 chunks)
*/

#define SGGC_CHUNK_SIZE 16      /* Number of bytes in a data chunk */

#define SGGC_N_KINDS (2*SGGC_N_TYPES)  /* For now, one small size plus big */

#define SGGC_KIND_CHUNKS \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  2, 5, 2, 2, 2, 2, 2, 4, 4, 2, 2, 2, 2, 2, 2, 3, \
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  \
}

#endif

/* Include the generic SGGC header file. */

#include "sggc.h"
