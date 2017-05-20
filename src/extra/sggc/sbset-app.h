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

#define SBSET_OFFSET_BITS 6 /* Max is 6 for using 64-bit shift/mask operations*/


/* CHAINS FOR LINKING SEGMENTS IN SETS. */

#define SBSET_CHAINS 5       /* Number of chains that can be used for sets */

#  define SGGC_UNUSED_FREE_NEW 0  /* Unused, free or newly allocated objects */

#  define SGGC_OLD_GEN1 1         /* Objects that survived one GC */

#  define SGGC_OLD_GEN2_UNCOL 2   /* Objects that survived more than one GC,
                                    and also constant and uncollected objects */

#  define SGGC_OLD_TO_NEW 3       /* Objects maybe with old-to-new references */

#  define SGGC_LOOK_AT 4          /* Objects that still need to be looked at
                                    in order to mark objects still in use */


/* EXTRA INFORMATION STORED IN A SBSET_SEGMENT STRUCTURE.  Putting it
   here takes advantage of what might otherwise be 32 bits of unused
   padding, and makes the sbset_segment struct be exactly 64 bytes in
   size (maybe advantageous for index computation (if needed), and
   perhaps for cache behaviour).

   The info is a union of fields for small segments and for big
   segments, but the first few fields are the same for both kinds (and
   may be referenced either way). */

#define SGGC_CHUNK_BITS 31 /* Bits used to record the number of chunks */

#define SBSET_EXTRA_INFO \
  union \
  { struct                 /* For big segments... */ \
    { unsigned char kind;     /* The kind of segment (equal to type if big) */ \
      unsigned constant : 1;  /* 1 for a constant segment                   */ \
      unsigned big : 1;       /* 1 for a big segment with one large object  */ \
      unsigned huge : 1;      /* 1 if maximum cnunks not less than 2^21     */ \
      unsigned alloc_chunks : 21; /* Chunks that fit in allocated space, if */ \
    } big;                     /* huge is 0, else that >> by HUGE_SHIFT     */ \
    struct                 /* For small segments... */ \
    { unsigned char kind;     /* The kind of segment (equal to type if big) */ \
      unsigned constant : 1;  /* 1 for a constant segment                   */ \
      unsigned big : 1;       /* 1 for a big segment with one large object  */ \
      unsigned unused : 6;    /* Bits not currently in use                  */ \
      /* setting of aux1_off and aux2_off below may be disabled in sggc.c   */ \
      unsigned char aux1_off; /* Offset of aux1 info from start of block    */ \
      unsigned char aux2_off; /* Offset of aux2 info from start of block    */ \
    } small; \
  } x;


/* POINTER TO ARRAY OF SEGMENTS OR POINTERS TO SEGMENTS.  This array
   of segments or pointers to them is allocated when the GC is
   initialized, with the segments themselves allocated later, as
   needed, except that if SGGC_MAX_SEGMENTS is defined, it is
   allocated statically instead. */

#ifndef SGGC_EXTERN
#define SGGC_EXTERN extern
#endif

#ifdef SGGC_MAX_SEGMENTS
#ifdef SGGC_SEG_DIRECT
#define SBSET_DO_BEFORE_INLINE \
  SGGC_EXTERN struct sbset_segment sggc_segment[SGGC_MAX_SEGMENTS];
#else
SGGC_EXTERN struct sbset_segment *sggc_segment[SGGC_MAX_SEGMENTS];
#endif
#else
#ifdef SGGC_SEG_DIRECT
SGGC_EXTERN struct sbset_segment *sggc_segment;
#else
SGGC_EXTERN struct sbset_segment **sggc_segment;
#endif
#endif


/* MACRO FOR GETTING SEGMENT POINTER FROM SEGMENT INDEX. */

#ifdef SGGC_SEG_DIRECT
#define SBSET_SEGMENT(index) (sggc_segment+index)
#else
#define SBSET_SEGMENT(index) (sggc_segment[index])
#endif


/* INCLUDE THE NON-APPLICATION-SPECIFIC HEADER FOR THE SET MODULE. */

#include "sbset.h"
