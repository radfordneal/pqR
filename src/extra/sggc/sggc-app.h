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

typedef R_len_t sggc_length_t;  /* Type for holding an object length */
typedef unsigned sggc_nchunks_t;/* Type for how many chunks are in a segment */


/* NUMBER OF OBJECT TYPES.  Allows for 5-bit type field.  This will be
   the "base" type for an object, not the full type, so that SET_TYPE
   can work. */

#define SGGC_N_TYPES 32


/* USE ONLY BIG SEGMENTS FOR NOW. */

#define SGGC_N_KINDS SGGC_N_TYPES

#define SGGC_KIND_CHUNKS \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
}

#define sggc_kind(type,length) (type)


#if sizeof (void *) == 8

/* LAYOUT WITH UNCOMPRESSED 64-BIT POINTERS. 

      Cons-type:          Vector:             
        info, cptr          info, cptr
        length, truelength  length, truelength
        attrib              attrib
        car                 data...
        cdr
        tag
        = 48 bytes          = 32 bytes if length==1
          (3 chunks)          (2 chunks)

      Symbol:             Primitive:
        info, cptr          info, cptr
        length, truelength  length, truelength
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

#else 

/* LAYOUT WITH UNCOMPRESSED 32-BIT POINTERS.

      Cons-type:          Vector:             
        info                info
        cptr                cptr
        length              length
        truelength          truelength
        attrib              attrib
        car                 padding
        cdr                 data...
        tag                 = 32 bytes if length==1
        = 32 bytes          (2 chunks)
          (2 chunks)

      Symbol:             Primitive:
        info                info
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

#endif

/* Include the generic SGGC header file. */

#include "sggc.h"
