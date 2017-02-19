/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Header for sggc's use of the facility for maintaining sets of objects

   Copyright (c) 2016, 2017 Radford M. Neal.

   The SGGC library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */


/* NUMBER OF OFFSET BITS IN A (SEGMENT INDEX, OFFSET) PAIR. */

#define SET_OFFSET_BITS 6  /* Max is 6 for using 64-bit shift/mask operations */


/* CHAINS FOR LINKING SEGMENTS IN SETS.  All sets used have their own
   chain, except that the SET_UNUSED_FREE_NEW chain is shared amongst 
   a collection of sets, one set for each possible kind of object, as
   well as the unused set of segments for big objects, and SET_OLD_GEN2_CONST
   is shared between old generation 2 and constants. */

#define SET_CHAINS 5       /* Number of chains that can be used for sets */

#  define SET_UNUSED_FREE_NEW 0  /* Unused, free or newly allocated objects */

#  define SET_OLD_GEN1 1         /* Objects that survived one GC */

#  define SET_OLD_GEN2_CONST 2   /* Objects that survived more than one GC,
                                    and also constant objects */

#  define SET_OLD_TO_NEW 3       /* Objects maybe with old-to-new references */

#  define SET_TO_LOOK_AT 4       /* Objects that still need to be looked at 
                                    in order to mark objects still in use */


/* EXTRA INFORMATION STORED IN A SET_SEGMENT STRUCTURE.  Putting it
   here takes advantage of what might otherwise be 32 bits of unused
   padding, and makes the set_segment struct be exactly 64 bytes in
   size (maybe advantageous for index computation (if needed), and
   perhaps for cache behaviour). */

#define SGGC_CHUNK_BITS 31 /* Bits used to record the number of chunks */

#define SET_EXTRA_INFO \
  union \
  { struct                 /* For big segments... */ \
    { unsigned big : 1;       /* 1 for a big segment with one large object  */ \
      unsigned max_chunks : SGGC_CHUNK_BITS;/* Chunks that fit in allocated */ \
    } big;                                  /* space; 0 if too big to record*/ \
    struct                 /* For small segments... */ \
    { unsigned big : 1;       /* 0 for a segment with several small objects */ \
      unsigned unused : 6;    /* Bits not currently in use                  */ \
      unsigned constant : 1;  /* 1 for a constant segment                   */ \
      /* setting of aux1_off and aux2_off below may be disabled in sggc.c   */ \
      unsigned char aux1_off; /* Offset of aux1 info from start of block    */ \
      unsigned char aux2_off; /* Offset of aux2 info from start of block    */ \
      unsigned char kind;     /* The kind of segment (equal to type if big) */ \
    } small; \
  } x;


/* POINTER TO ARRAY OF POINTERS TO SEGMENTS.  This array of pointers
   is allocated when the GC is initialized, with the segments themselves
   allocated later, as needed, except that if SGGC_MAX_SEGMENTS is
   defined, it is allocated statically instead. */

#ifdef SGGC_EXTERN
SGGC_EXTERN 
#else
extern
#endif

#ifdef SGGC_MAX_SEGMENTS
struct set_segment *sggc_segment[SGGC_MAX_SEGMENTS];
#else
struct set_segment **sggc_segment;
#endif


/* MACRO FOR GETTING SEGMENT POINTER FROM SEGMENT INDEX. */

#define SET_SEGMENT(index) (sggc_segment[index])


/* INCLUDE THE NON-APPLICATION-SPECIFIC HEADER FOR THE SET MODULE. */

#include "set.h"
