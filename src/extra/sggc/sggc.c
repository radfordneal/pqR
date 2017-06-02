/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Segmented generational garbage collection - function definitions

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


/*   See sggc-doc for general information on the SGGC library, and for the
     documentation on the application interface to SGGC.  See sggc-imp for
     discussion of the implementation of SGGC. */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SGGC_EXTERN    /* So globals will be declared here without 'extern' */
#include "sggc-app.h"

#if SBSET_STATIC
# include "sbset.c"    /* Define set procedures here as static, not linked */
#endif


/* DEBUGGING FLAG.  Set to 1 to enable debug output.  May be set by a compiler
   flag, in which case it isn't overridden here. */

#ifndef SGGC_DEBUG
#define SGGC_DEBUG 0
#endif


/* ENABLE/DISABLE SEGMENT-AT-A-TIME OPERATIONS.  Set to 1 to enable use of
   set functions for segment-at-a-time operations.  If not defined, defaults
   to 1, which is what it should be except for debugging and timing tests. */

#ifndef SGGC_SEGMENT_AT_A_TIME
#define SGGC_SEGMENT_AT_A_TIME 1
#endif


/* BLOCKING FOR SMALL DATA AREAS. */

#ifndef SGGC_SMALL_DATA_AREA_BLOCKING
#define SGGC_SMALL_DATA_AREA_BLOCKING 1
#endif


/* ENABLE/DISABLE EXTRA CHECKS.  These are consistency checks that take
   non-negligible time. */

#define EXTRA_CHECKS 0


/* CLEARING OF FREED DATA AREAS. */

#ifdef SGGC_CLEAR_FREE

#ifndef SGGC_CLEAR_DATA_BYTE
#define SGGC_CLEAR_DATA_BYTE 0
#endif

#ifndef SGGC_CLEAR_AUX1_BYTE
#define SGGC_CLEAR_AUX1_BYTE 0
#endif

#ifndef SGGC_CLEAR_AUX2_BYTE
#define SGGC_CLEAR_AUX2_BYTE 0
#endif

#endif


/* ALLOCATE / FREE MACROS.  Defaults to the system calloc/malloc/free
   if something else is not defined in sggc-app.h.  Also defines the
   appropriate allocation routine for data areas, according to whether
   SGGC_DATA_ALLOC_ZERO is defined. */

#ifndef sggc_mem_alloc_zero
#define sggc_mem_alloc_zero(n) calloc(n,1)
#endif

#ifndef sggc_mem_alloc
#define sggc_mem_alloc(n) malloc(n)
#endif

#ifndef sggc_mem_free
#define sggc_mem_free free
#endif

#ifdef SGGC_DATA_ALLOC_ZERO
#define sggc_mem_alloc_data(n) sggc_mem_alloc_zero(n)
#else
#define sggc_mem_alloc_data(n) sggc_mem_alloc(n)
#endif


/* NUMBERS OF CHUNKS ALLOWED FOR AN OBJECT IN KINDS OF SEGMENTS.  Zero
   means that this kind of segment is big, containing one object of
   size found using sggc_chunks.  The application must define the
   initialization as SGGC_KIND_CHUNKS.  Entries must not be greater
   than SGGC_CHUNKS_IN_SMALL_SEGMENT. */

const int sggc_kind_chunks[SGGC_N_KINDS] = SGGC_KIND_CHUNKS;


/* TYPES FOR KINDS, IF PROVIDED BY APPLICATION. */

#ifdef SGGC_KIND_TYPES
static const sggc_type_t sggc_kind_types[SGGC_N_KINDS] = SGGC_KIND_TYPES;
#endif


/* MACROS TO APPLY OR UN-APPLY AN OFFSET TO A DATA/AUX POINTER. */

#if SGGC_USE_OFFSET_POINTERS

#define OFFSET(ptrs,ix,sz) ((ptrs)[ix] -= \
  ((SGGC_OFFSET_CALC) (sz) << SBSET_OFFSET_BITS) * (SGGC_OFFSET_CALC) (ix))
#define UNDO_OFFSET(ptrs,ix,sz) ((ptrs)[ix] += \
  ((SGGC_OFFSET_CALC) (sz) << SBSET_OFFSET_BITS) * (SGGC_OFFSET_CALC) (ix))

#else

#define OFFSET(ptrs,ix,sz) 0       /* nothing to do */
#define UNDO_OFFSET(ptrs,ix,sz) 0  /* nothing to do */

#endif


/* NUMBERS OF OBJECTS IN SEGMENTS OF EACH KIND, AND END OF ITS CHUNKS.  
   Computed at initialization from SGGC_CHUNKS_IN_SMALL_SEGMENT and 
   sggc_kind_chunks. */

static int kind_objects[SGGC_N_KINDS];
static int kind_chunk_end[SGGC_N_KINDS];


/* MACRO TO FIND THE NUMBER OF CHUNKS ALLOCATED FOR A BIG SEGMENT. */

#define CHUNKS_ALLOCATED(seg) \
  ((seg)->X.Big.huge ? (seg)->X.Big.alloc_chunks << SGGC_HUGE_SHIFT \
                     : (seg)->X.Big.alloc_chunks)



/* BLOCKS OF SPACE ALLOCATED FOR AUXILIARY INFORMATION. */

#ifdef SGGC_AUX1_SIZE
static char *kind_aux1_block[SGGC_N_KINDS];
static unsigned char kind_aux1_block_pos[SGGC_N_KINDS];
#endif

#ifdef SGGC_AUX2_SIZE
static char *kind_aux2_block[SGGC_N_KINDS];
static unsigned char kind_aux2_block_pos[SGGC_N_KINDS];
#endif


/* READ-ONLY AUXILIARY INFORMATION.  Filled in at initialization by calling
   the application's sggc_aux1_read_only and sggc_aux2_read_only functions. */

#ifdef SGGC_AUX1_READ_ONLY
static char *kind_aux1_read_only[SGGC_N_KINDS];
#endif

#ifdef SGGC_AUX2_READ_ONLY
static char *kind_aux2_read_only[SGGC_N_KINDS];
#endif


/* CURRENT BLOCK OF SPACE TO ALLOCATE SEGMENTS FROM.  Only present if
   SGGC_SEG_BLOCKING is defined (and greater than 1). */

#if SGGC_SEG_BLOCKING > 1
static struct sbset_segment *seg_block; /* Pointer to next segment in block */
static int seg_block_remaining;       /* # of segments remaining in seg_block */
#endif


/* CURRENT BLOCK OF SPACE TO ALLOCATE SMALL DATA AREAS FROM. */

#define SMALL_DATA_AREA_SIZE \
  ((size_t) SGGC_CHUNK_SIZE * SGGC_CHUNKS_IN_SMALL_SEGMENT)

static char *small_data_area_next;    /* Next position in small data area */
static char *small_data_area_end;     /* End of small data area */


/* BIT VECTORS FOR FULL SEGMENTS.  Computed at initialization from 
   sggc_kind_chunks and SBSET_OFFSET_BITS. */

static sbset_bits_t kind_full[SGGC_N_KINDS];


/* SETS OF OBJECTS. */

#define old_to_new sggc_old_to_new_set   /* External for inline use in sggc.h */

static struct sbset free_or_new[SGGC_N_KINDS]; /* Free or newly allocated */
static struct sbset unused;                    /* Big segments not being used */
static struct sbset old_gen1[SGGC_N_KINDS];    /* Survived collection once */
static struct sbset old_gen1_big;              /*   - for big objects */
static struct sbset old_gen2[SGGC_N_KINDS];    /* Survived collection >1 time */
static struct sbset old_gen2_big;              /*   - for big objects */
struct sbset old_to_new;                       /* May have old->new references*/
static struct sbset to_look_at;                /* Not yet looked at in sweep */
static struct sbset constants;                 /* Prealloc'd constant segments*/

#ifdef SGGC_KIND_UNCOLLECTED
#define uncollected sggc_uncollected_sets /* External for inline use in sggc.h*/
struct sbset uncollected[SGGC_N_KINDS];     /* Objects never collected */
#endif


/* INDICATORS OF WHICH KINDS ARE FOR UNCOLLECTED OBJECTS. */

#ifdef SGGC_KIND_UNCOLLECTED
const int sggc_kind_uncollected[SGGC_N_KINDS] = SGGC_KIND_UNCOLLECTED;
#endif


/* FUNCTIONS TO SOMETIMES BE CALLED FOR OBJECTS AT END OF COLLECTION. */

static int (*call_for_newly_freed[SGGC_N_KINDS]) (sggc_cptr_t);
static void (*call_for_object_in_use) (sggc_cptr_t, sggc_nchunks_t);


/* RECORDS OF NEXT FREE OBJECTS FOR EACH KIND.  These are used only
   for small kinds.  (Big kinds use 'unused'.)

   The sggc_next_free_val[k] value (unless it is SGGC_NO_OBJECT)
   points to free objects of kind k, with all objects in the
   SGGC_UNUSED_FREE_NEW chain following it also being free, unless
   sggc_next_segment_not_free[k] is 1.

   sggc_next_free_bits[k] records the free objects in the segment of
   sggc_next_free_val[k], with bits shifted right so that there is a 1
   bit in the bottom corresponding to sggc_next_free_val[k].

   These have external scope to allow use in sggc_alloc_small_kind_quickly, 
   which is declared as static inline in sggc.h. */

sggc_cptr_t sggc_next_free_val[SGGC_N_KINDS];
sbset_bits_t sggc_next_free_bits[SGGC_N_KINDS];
int sggc_next_segment_not_free[SGGC_N_KINDS];


/* MAXIMUM NUMBER OF SEGMENTS, AND INDEX OF NEXT SEGMENT TO USE. */

static sbset_index_t maximum_segments;         /* Max segments, fixed for now */
static sbset_index_t next_segment;             /* Number of segments in use */


/* GLOBAL VARIABLES USED FOR LOOKING AT OLD-NEW REFERENCES. */

static int collect_level = -1; /* Level of current garbage collection */
static int old_to_new_check;   /* Controls how old-to-new processing is done */


/* SUPPRESS MEMORY REUSE FLAG. */

static int do_not_reuse_memory;  /* Non-zero to suppress reuse */


/* MACRO TO DO SOMETHING FOR ELEMENT AND THOSE FOLLOWING IN THE SAME SEGMENT. 
   The statement references the element as 'w'. */

#define DO_FOR_SEGMENT(chain,el,stmt) \
  do { \
    for (sbset_value_t w = el; \
         w != SBSET_NO_VALUE && SBSET_VAL_INDEX(w) == SBSET_VAL_INDEX(el); \
         w = sbset_chain_next (chain, w)) \
    { stmt; \
    } \
  } while (0)


/* NUMBER OF CHUNKS FOR A "HUGE" OBJECT.  For such objects, the number
   of chunks is increased if necessary to a multiple of 2^SGGC_HUGE_SHIFT,
   so that the maximum number of chunks can be recorded in the available 
   space after shifting it right by SGGC_HUGE_SHIFT bits. */

#define HUGE_CHUNKS (1 << 21)  /* Number of chunks where object becomes huge */

#ifndef SGGC_HUGE_SHIFT
#define SGGC_HUGE_SHIFT 11     /* Amount to shift to try to get # in range */
#endif


/* ------------------------------ INITIALIZATION ---------------------------- */


/* INITIALIZE SEGMENTED MEMORY.  Allocates space for pointers for the
   specified number of segments (currently not expandable), unless
   SGGC_MAX_SEGMENTS is defined, so they are statically allocated.
   Record the specified maximum number of segments, reduced to 
   SGGC_MAX_SEGMENTS if that is defined.

   Returns zero if successful, non-zero if allocation fails. */

int sggc_init (unsigned max_segments)
{
  int j, k;

  /* Check that auxiliary block sizes aren't too big. */

# ifdef SGGC_AUX1_SIZE
    if (SGGC_AUX1_BLOCK_SIZE * SGGC_CHUNKS_IN_SMALL_SEGMENT > 256) abort();
# endif

# ifdef SGGC_AUX2_SIZE
    if (SGGC_AUX2_BLOCK_SIZE * SGGC_CHUNKS_IN_SMALL_SEGMENT > 256) abort();
# endif

  /* If not done statically, allocate space for pointers to segment
     descriptors, data, and possibly auxiliary information for
     segments.  Information for segments these point to is allocated
     later, when the segment is actually needed.  Also allocate space
     for segment types. */

#ifndef SGGC_MAX_SEGMENTS

    sggc_segment = sggc_mem_alloc (max_segments * sizeof *sggc_segment);
    if (sggc_segment == NULL)
    { return 1;
    }

    sggc_data = sggc_mem_alloc (max_segments * sizeof *sggc_data);
    if (sggc_data == NULL)
    { sggc_mem_free((void*)sggc_segment);
      return 2;
    }

    sggc_type = sggc_mem_alloc (max_segments * sizeof *sggc_type);
    if (sggc_type == NULL)
    { sggc_mem_free((void*)sggc_segment);
      sggc_mem_free((void*)sggc_data);
      return 3;
    }

#   ifdef SGGC_AUX1_SIZE
      sggc_aux1 = sggc_mem_alloc (max_segments * sizeof *sggc_aux1);
      if (sggc_aux1 == NULL)
      { sggc_mem_free((void*)sggc_segment);
        sggc_mem_free((void*)sggc_data);
        sggc_mem_free((void*)sggc_type);
        return 4;
      }
#   endif

#   ifdef SGGC_AUX2_SIZE
      sggc_aux2 = sggc_mem_alloc (max_segments * sizeof *sggc_aux2);
      if (sggc_aux2 == NULL)
      { sggc_mem_free((void*)sggc_segment);
        sggc_mem_free((void*)sggc_data);
        sggc_mem_free((void*)sggc_type);
#       ifdef SGGC_AUX1_SIZE
          sggc_mem_free((void*)sggc_aux1);
#       endif
        return 5;
      }
#   endif

#endif

  /* Compute numbers of objects in segments of each kind, and
     initialize bit vectors that indicate when segments of different
     kinds are full, and are also used to initialize segments as full.
     Along the way, check that small segments aren't too big. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { if (sggc_kind_chunks[k] == 0) /* big segment */
    { kind_full[k] = 1;
      kind_objects[k] = 1;
      kind_chunk_end[k] = 0;  /* ends when only object ends */
    }
    else /* small segment */
    { if (sggc_kind_chunks[k] > SGGC_CHUNKS_IN_SMALL_SEGMENT) abort();
      kind_full[k] = 0;
      for (j = 0;
           j + sggc_kind_chunks[k] <= SGGC_CHUNKS_IN_SMALL_SEGMENT; 
           j += sggc_kind_chunks[k])
      { kind_full[k] |= (sbset_bits_t)1 << j;
      }
      kind_objects[k] = SGGC_CHUNKS_IN_SMALL_SEGMENT / sggc_kind_chunks[k];
      kind_chunk_end[k] = kind_objects[k] * sggc_kind_chunks[k];
    }
  }

  /* Initialize tables of read-only auxiliary information. */

# ifdef SGGC_AUX1_READ_ONLY
    for (k = 0; k < SGGC_N_KINDS; k++)
    { kind_aux1_read_only[k] = sggc_kind_chunks[k] == 0 /* big segment */
                                ? NULL : sggc_aux1_read_only(k);
    }
# endif

# ifdef SGGC_AUX2_READ_ONLY
    for (k = 0; k < SGGC_N_KINDS; k++)
    { kind_aux2_read_only[k] = sggc_kind_chunks[k] == 0 /* big segment */
                                ? NULL : sggc_aux2_read_only(k);
    }
# endif

  /* Check for read-only data for big segments, and initialize all aux blocks
     to NULL. */

# ifdef SGGC_AUX1_SIZE
    for (k = 0; k < SGGC_N_KINDS; k++)
    { 
#     ifdef SGGC_AUX1_READ_ONLY
      if (kind_aux1_read_only[k] != NULL)      /* read-only aux1 info is not */
      { if (sggc_kind_chunks[k] == 0) abort(); /*   allowed for big segments */
      }
      else
#     endif
      { /* not read-only */
        kind_aux1_block[k] = NULL;
        kind_aux1_block_pos[k] = 0;
      }
    }
# endif

# ifdef SGGC_AUX2_SIZE
    for (k = 0; k < SGGC_N_KINDS; k++)
    { 
#     ifdef SGGC_AUX2_READ_ONLY
      if (kind_aux2_read_only[k] != NULL)      /* read-only aux2 info is not */
      { if (sggc_kind_chunks[k] == 0) abort(); /*   allowed for big segments */
      }
      else
#     endif
      { /* not read-only */
        kind_aux2_block[k] = NULL;
        kind_aux2_block_pos[k] = 0;
      }
    }
# endif

  /* Initialize sets of objects, as empty. */

  sbset_init(&unused,SGGC_UNUSED_FREE_NEW);
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { sbset_init(&free_or_new[k],SGGC_UNUSED_FREE_NEW);
    sbset_init(&old_gen1[k],SGGC_OLD_GEN1);
    sbset_init(&old_gen2[k],SGGC_OLD_GEN2_UNCOL);
#ifdef SGGC_KIND_UNCOLLECTED
    sbset_init(&uncollected[k],SGGC_OLD_GEN2_UNCOL);
#endif
  }
  sbset_init(&old_gen1_big,SGGC_OLD_GEN1);
  sbset_init(&old_gen2_big,SGGC_OLD_GEN2_UNCOL);
  sbset_init(&old_to_new,SGGC_OLD_TO_NEW);
  sbset_init(&to_look_at,SGGC_LOOK_AT);
  sbset_init(&constants,SGGC_OLD_GEN2_UNCOL);

  /* Initialize to no free objects of each kind. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { sggc_next_free_val[k] = SGGC_NO_OBJECT;
    sggc_next_free_bits[k] = 0;
    sggc_next_segment_not_free[k] = 0;
  }

  /* Record the maximum number of segments, adjusting it downwards if
     it exceeds SGGC_MAX_SEGMENTS, or the maximum possible. */

  maximum_segments = max_segments;
# ifdef SGGC_MAX_SEGMENTS
  if (maximum_segments > SGGC_MAX_SEGMENTS)
  { maximum_segments = SGGC_MAX_SEGMENTS;
  }
# endif
  if (maximum_segments > ((~(uint32_t)0) >> SBSET_OFFSET_BITS) + 1)
  { maximum_segments = ((~(uint32_t)0) >> SBSET_OFFSET_BITS) + 1;
  }

  /* Initialize next segment that can be used.  Skip segment 0 if
     SGGC_NO_OBJECT has all 0 bits. */

  next_segment = SGGC_NO_OBJECT==0 ? 1 : 0;

# if SGGC_SEG_BLOCKING > 1
  seg_block_remaining = 0;
# endif

  /* Initialize block of space for data areas of small segments. */

  small_data_area_next = NULL;

  /* Initialize the sggc_info structure. */

  sggc_info.gen0_count = 0;
  sggc_info.gen1_count = 0;
  sggc_info.gen2_count = 0;
  sggc_info.uncol_count = 0;

  sggc_info.gen0_big_chunks = 0;
  sggc_info.gen1_big_chunks = 0;
  sggc_info.gen2_big_chunks = 0;
  sggc_info.uncol_big_chunks = 0;

  sggc_info.n_segments = 0;
  sggc_info.total_mem_usage = 0;

  return 0;
}


/* -------------------------------- ALLOCATION ------------------------------ */


/* UPDATE THE POSITION TO USE NEXT IN A BLOCK OF AUXILIARY INFORMATION.
   For big segments (with only one object), auxiliary information is
   used sequentially for each new segment, until all of an auxiliary
   block has been used.  For small segments (with several objects),
   new segments are allocated successive positions for the auxiliary
   information for the segment's first object, until a position is
   reached where this spot is already used by the second object of a
   previous segment, at which time the position used jumps forward
   past all previously-used positions.  In either case, when no space 
   is left in the auxiliary block, the block pointer is set to NULL,
   to indicate that a new block must be allocated next time. */

static void next_aux_pos (sggc_kind_t kind, char **block, unsigned char *pos,
                          int block_size)
{
  int new_pos;  /* used to avoid overflow in operations on *pos */

  sggc_nchunks_t nch = sggc_kind_chunks[kind];

  new_pos = *pos + 1;

  if (nch == 0)  /* big segment */
  { if (new_pos >= block_size * SGGC_CHUNKS_IN_SMALL_SEGMENT)
    { *block = NULL;
      *pos = 0;  /* though should be irrelevant */
    }
    else
    { *pos = new_pos;
    }
  }
  else  /* small segment */
  { if (*pos % nch == 0) 
    { new_pos += nch * (kind_objects[kind] - 1);
    }
    if (new_pos + nch * (kind_objects[kind] - 1) 
         >= block_size * SGGC_CHUNKS_IN_SMALL_SEGMENT)
    { *block = NULL;
      *pos = 0;  /* though should be irrelevant */
    }
    else
    { *pos = new_pos;
    }
  }

}


/* ALLOCATE A NEW SEGMENT.  Returns -1 if unable to allocate (reached 
   maximum segments, or memory allocation failed).  Otherwise, returns
   the index of the new segment, which will have been initialized, but
   will not have its type or kind set yet (flags will all be 0). */

static sbset_index_t new_segment (void)
{
  struct sbset_segment *seg;

  if (next_segment == maximum_segments)
  { return -1;
  }

  /* Find or allocate memory for the segment structure. */

# ifdef SGGC_SEG_DIRECT
    seg = sggc_segment + next_segment;
# elif SGGC_SEG_BLOCKING > 1
    if (seg_block_remaining == 0)
    { char *sb;
      sb = sggc_mem_alloc_zero (SGGC_SEG_BLOCKING*sizeof(struct sbset_segment));
      if (sb == NULL)
      { return -1;
      }
      seg_block_remaining = SGGC_SEG_BLOCKING;

      /* Align to 64-byte boundary. */

      if (((uintptr_t)sb & 63) != 0)
      { if (0) /* can enable to see what's happening on this platform */
        { printf("Aligning segment block %p to 64-byte boundary\n",sb);
        }
        sb = (char *) (((uintptr_t)sb + 63) & ~(uintptr_t)63);
        seg_block_remaining -= 1;
      }

      seg_block = (struct sbset_segment *) sb;
    }
    seg = seg_block;
    sggc_segment[next_segment] = seg;
    seg_block += 1;
    seg_block_remaining -= 1;
# else
    seg = sggc_mem_alloc_zero (sizeof **sggc_segment);
    if (seg == NULL)
    { return -1;
    }
    sggc_segment[next_segment] = seg;
# endif

  /* Initialize the segment. */

  sbset_segment_init (seg);

  /* Increase total memory usage by the size of a segment structure,
     Also add the size of the pointer to the segment, if one is
     needed, plus the size of the pointers to data area and auxiliary
     information, plus the space for the type.

     Total memory usage is incremented in this way as segments are
     used regardless of whether or not the data is statically
     allocated, on the assumption that virtual memory space is not
     allocate physical memory until used.  */

  sggc_info.total_mem_usage += sizeof (struct sbset_segment);

# ifndef SGGC_SEG_DIRECT
    sggc_info.total_mem_usage += sizeof (struct sbset_segment *);
# endif

  sggc_info.total_mem_usage += sizeof (char *);
# ifdef SGGC_AUX1_SIZE
    sggc_info.total_mem_usage += sizeof (char *);
# endif
# ifdef SGGC_AUX2_SIZE
    sggc_info.total_mem_usage += sizeof (char *);
# endif

  sggc_info.total_mem_usage += sizeof (sggc_type_t);

  sggc_info.n_segments += 1;

  return next_segment++;
}


/* ARRANGE THAT A SMALL DATA AREA IS AVAILABLE.  Sets small_data_area_next 
   to NULL if it couldn't allocate space.  Otherwise, small_data_area_next 
   is set to point just past the data area that is available, which is of 
   size SMALL_DATA_AREA_SIZE.  If the data area is not used in the end, 
   SMALL_DATA_AREA_SIZE should be subtracted from small_data_area_next, so 
   that the space will be used next time. */

static void get_small_data_area (void)
{
  if (small_data_area_next == NULL 
       || small_data_area_next == small_data_area_end)
  { 
#   if SGGC_SMALL_DATA_AREA_ALIGN
      small_data_area_next = 
       sggc_mem_alloc_data (SMALL_DATA_AREA_SIZE*SGGC_SMALL_DATA_AREA_BLOCKING
                            + SGGC_SMALL_DATA_AREA_ALIGN - 1);
#   else
      small_data_area_next = 
       sggc_mem_alloc_data (SMALL_DATA_AREA_SIZE*SGGC_SMALL_DATA_AREA_BLOCKING);
#   endif
    if (small_data_area_next == NULL)
    { return;
    }
#   if SGGC_SMALL_DATA_AREA_ALIGN
      small_data_area_next = (char *) 
       (((uintptr_t)small_data_area_next + SGGC_SMALL_DATA_AREA_ALIGN - 1) 
           & ~ ((uintptr_t)SGGC_SMALL_DATA_AREA_ALIGN - 1));
#   endif
    small_data_area_end = small_data_area_next +
                           SMALL_DATA_AREA_SIZE * SGGC_SMALL_DATA_AREA_BLOCKING;
  }

  small_data_area_next += SMALL_DATA_AREA_SIZE;
}


/* ALLOCATE AN OBJECT OF SPECIFIED KIND, TYPE, AND LENGTH.  The length
   is used only for big kinds. The value returned is SGGC_NO_OBJECT if
   allocation fails (but note that it might succeed if retried after
   garbage collection is done), or if the number of chunks required 
   is greater than can be stored in alloc_chunks.

   Used to implement sggc_alloc, sggc_alloc_kind, and sggc_alloc_small_kind. 

   Uses sggc_alloc_small_kind_quickly when possible. */

static sggc_cptr_t sggc_alloc_kind_type_length (sggc_kind_t kind, 
                                                sggc_type_t type,
                                                sggc_length_t length)
{
  int big;                   /* will object go in a big segment? */

  sggc_nchunks_t nch = sggc_kind_chunks[kind]; /* number of chunks for object */

  char *data;                /* pointer to data area for segment used */
  size_t data_size = 0;      /* size of data area, if allocated here, else 0 */

  sbset_index_t index;       /* index of segment that object will be in */
  struct sbset_segment *seg; /* ptr to struct for seg object goes in */
  sggc_cptr_t v;             /* pointer to object to be returned as value */

  /* Look for an existing segment for this object to go in.  For a
     small segment, just call sggc_alloc_small_kind_quickly, and
     return the result directly if it succeeds.  For a big segment,
     take a segment from 'unused', if one is there.  For a big
     segment, or if no existing small segment is found, allocate the
     data area for the (big or small) segment.  Return with failure
     indication if this allocation fails. */

  if (nch == 0) /* big segment */
  { 
    v = sbset_first (&unused, 1); /* since removed with sbset_first, will be */
                                  /*   OK to later add it to free_or_new[k]  */
    if (v != SGGC_NO_OBJECT)
    { if (SGGC_DEBUG) printf("sggc_alloc: found %x in unused\n",(unsigned)v);
      index = SBSET_VAL_INDEX(v);
      seg = SBSET_SEGMENT(index);
      sggc_type[index] = type;  /* may reuse big segments of other types */
      seg->X.Big.kind = kind; 
    }

    nch = sggc_nchunks (type, length);

    /* Increase nch if necesary for huge objects so it can be shifted to fit. */

    if (nch >= HUGE_CHUNKS)
    { nch = ((nch + (1<<SGGC_HUGE_SHIFT) - 1) >> SGGC_HUGE_SHIFT) 
                                              << SGGC_HUGE_SHIFT;
      if ((nch >> SGGC_HUGE_SHIFT) >= HUGE_CHUNKS) 
      { return SGGC_NO_OBJECT;
      }
    }

    data_size = (size_t) SGGC_CHUNK_SIZE * nch;
    data = sggc_mem_alloc_data (data_size);
    if (SGGC_DEBUG) 
    { printf (
       "sggc_alloc: called mem_alloc_data for data (big %d, %d chunks):: %p\n", 
        kind, (int)nch, data);
    }

    big = 1;
  }

  else /* small segment */
  {
    v = sggc_alloc_small_kind_quickly(kind);
    if (v != SGGC_NO_OBJECT)
    { if (SGGC_DEBUG)
      { printf("sggc_alloc: found %x in next_free\n",(unsigned)v);
        printf("sggc_alloc: next_free_val[%d]=%x, next_free_bits[%d]=%016llx\n",
                kind, sggc_next_free_val[kind], 
                kind, (unsigned long long) sggc_next_free_bits[kind]);
      }
      return v;
    }

    get_small_data_area();
    data_size = SMALL_DATA_AREA_SIZE;
    data = small_data_area_next - SMALL_DATA_AREA_SIZE;

    big = 0;
  }

  if (data == NULL) 
  { return SGGC_NO_OBJECT;
  }

  /* Make sure we have blocks of auxiliary information 1 and 2 available 
     (if required), in case we need them (though we may not).  Return
     SGGC_NO_OBJECT if we can't allocate these blocks (freeing data
     if it was allocated).  Trying to allocate them now avoids the need to 
     back out later operations if the allocation fails, and is not really
     a waste since the amount allocated should be small, and will be
     needed sooner or later, even if not now. */

# ifdef SGGC_AUX1_SIZE
    char *const read_only_aux1 = 
#     ifdef SGGC_AUX1_READ_ONLY
        kind_aux1_read_only[kind];
#     else
        NULL;
#     endif
    if (!read_only_aux1 && kind_aux1_block[kind] == NULL)
    { kind_aux1_block[kind] = sggc_mem_alloc
                               (SGGC_CHUNKS_IN_SMALL_SEGMENT
                                 * SGGC_AUX1_BLOCK_SIZE * SGGC_AUX1_SIZE);
      if (kind_aux1_block[kind] == NULL) 
      { goto fail;
      }
      sggc_info.total_mem_usage += SGGC_CHUNKS_IN_SMALL_SEGMENT
                                    * SGGC_AUX1_BLOCK_SIZE * SGGC_AUX1_SIZE;
      kind_aux1_block_pos[kind] = 0;
      if (SGGC_DEBUG)
      { printf(
         "sggc_alloc: called alloc_zeroed for aux1 block (kind %d):: %p\n", 
          kind, kind_aux1_block[kind]);
      }
    }
# endif

# ifdef SGGC_AUX2_SIZE 
    char *const read_only_aux2 = 
#     ifdef SGGC_AUX2_READ_ONLY
        kind_aux2_read_only[kind];
#     else
        NULL;
#     endif
    if (!read_only_aux2 && kind_aux2_block[kind] == NULL)
    { kind_aux2_block[kind] = sggc_mem_alloc
                               (SGGC_CHUNKS_IN_SMALL_SEGMENT
                                 * SGGC_AUX2_BLOCK_SIZE * SGGC_AUX2_SIZE);
      if (kind_aux2_block[kind] == NULL) 
      { goto fail;
      }
      sggc_info.total_mem_usage += SGGC_CHUNKS_IN_SMALL_SEGMENT
                                    * SGGC_AUX2_BLOCK_SIZE * SGGC_AUX2_SIZE;
      kind_aux2_block_pos[kind] = 0;
      if (SGGC_DEBUG)
      { printf(
         "sggc_alloc: called alloc_zeroed for aux2 block (kind %d):: %p\n", 
          kind, kind_aux2_block[kind]);
      }
    }
# endif

  /* Try to create a new segment for this object, if none found above. */

  sggc_cptr_t u = v;  /* to remember whether it wasn't allocated before */

  if (v == SGGC_NO_OBJECT)  /* new segment, big or small */
  { 
    index = new_segment();
    if (index < 0)
    { goto fail;
    }

    sggc_type[index] = type;
    seg = SBSET_SEGMENT(index);
    seg->X.Big.big = big;
    seg->X.Big.kind = kind;  /* small.kind and big.kind are the same place */

    v = SGGC_CPTR_VAL(index,0);
    if (SGGC_DEBUG) 
    { printf("sggc_alloc: created %x in new segment\n", (unsigned)v);
    }
  }

  /* Add the object to 'uncollected[kind]' or 'free_or_new[kind]', now
     that we know there won't be a failure. */

#ifdef SGGC_KIND_UNCOLLECTED
  if (sggc_kind_uncollected[kind])
  { sbset_add (&uncollected[kind], v);
    sggc_info.uncol_count += 1;
  }
  else
#endif
  { sbset_add (&free_or_new[kind], v);
    sggc_info.gen0_count += 1;
  }

  /* For new small segments, put all other objects in the new segment
     into free_or_new (unless uncollected), and use them as the
     current set of free objects, since there were none before. */

  if (u == SGGC_NO_OBJECT && !big)  /* new small segment */
  { 
#ifdef SGGC_KIND_UNCOLLECTED
    if (!sggc_kind_uncollected[kind])
#endif
    { sbset_assign_segment_bits (&free_or_new[kind], v, kind_full[kind]);
      if (SGGC_DEBUG)
      { printf(
         "sggc_alloc: new segment has bits %016llx, %d in free_or_new[%d]\n", 
         (unsigned long long) sbset_chain_segment_bits (SGGC_UNUSED_FREE_NEW,v),
         sbset_n_elements(&free_or_new[kind]), kind);
      }
    }

    sggc_next_free_bits[kind] = kind_full[kind] >> sggc_kind_chunks[kind];
    sggc_next_free_val[kind] = sggc_next_free_bits[kind] == 0 
                                  ? SGGC_NO_OBJECT : v + sggc_kind_chunks[kind];
    sggc_next_segment_not_free[kind] = 1;

    if (SGGC_DEBUG)
    { printf("sggc_alloc: next_free_val[%d]=%x, next_free_bits[%d]=%016llx\n",
              kind, sggc_next_free_val[kind], 
              kind, (unsigned long long) sggc_next_free_bits[kind]);
    }
  }

  /* For new segments (big or small), assign auxiliary information
     We've previously guaranteed that auxiliary space is available. */

  if (u == SGGC_NO_OBJECT)
  {
#   ifdef SGGC_AUX1_SIZE
#     ifdef SGGC_AUX1_READ_ONLY
      if (read_only_aux1)
      { sggc_aux1[index] = (sggc_dptr) read_only_aux1;
        if (SGGC_DEBUG)
        { printf("sggc_alloc: used read-only aux1 for %x\n", v);
        }
      }
      else
#     endif
      { sggc_aux1[index] = (sggc_dptr) (kind_aux1_block[kind] 
                             + kind_aux1_block_pos[kind] * SGGC_AUX1_SIZE);
        if (0 && !big) /* could be enabled if aux1_off were ever actually used*/
        { seg->X.Small.aux1_off = kind_aux1_block_pos[kind];
        }
        if (SGGC_DEBUG)
        { printf(
            "sggc_alloc: aux1 block for %x has pos %d in block for kind %d\n",
             v, kind_aux1_block_pos[kind], kind);
        }
        next_aux_pos (kind, &kind_aux1_block[kind], &kind_aux1_block_pos[kind],
                      SGGC_AUX1_BLOCK_SIZE);
      }
      OFFSET(sggc_aux1,index,SGGC_AUX1_SIZE);
#   endif

#   ifdef SGGC_AUX2_SIZE
#     ifdef SGGC_AUX2_READ_ONLY
      if (read_only_aux2)
      { sggc_aux2[index] = (sggc_dptr) read_only_aux2;
        if (SGGC_DEBUG)
        { printf("sggc_alloc: used read-only aux2 for %x\n", v);
        }
      }
      else
#     endif
      { sggc_aux2[index] = (sggc_dptr) (kind_aux2_block[kind] 
                             + kind_aux2_block_pos[kind] * SGGC_AUX2_SIZE);
        if (0 && !big) /* could be enabled if aux2_off were ever actually used*/
        { seg->X.Small.aux2_off = kind_aux2_block_pos[kind];
        }
        if (SGGC_DEBUG)
        { printf(
            "sggc_alloc: aux2 block for %x has pos %d in block for kind %d\n",
             v, kind_aux2_block_pos[kind], kind);
        }
        next_aux_pos (kind, &kind_aux2_block[kind], &kind_aux2_block_pos[kind],
                      SGGC_AUX2_BLOCK_SIZE);
      }
      OFFSET(sggc_aux2,index,SGGC_AUX2_SIZE);
#   endif
  }

  if (EXTRA_CHECKS)
  { if (sbset_contains (&old_to_new, v)) abort();
    if (sbset_contains (&to_look_at, v)) abort();
  }

  /* Set up the data pointer for the segment. */

  if (big)
  { if (nch < HUGE_CHUNKS)
    { seg->X.Big.alloc_chunks = nch;
      seg->X.Big.huge = 0;
    }
    else
    { seg->X.Big.alloc_chunks = nch >> SGGC_HUGE_SHIFT;
      seg->X.Big.huge = 1;
    }
#ifdef SGGC_KIND_UNCOLLECTED
    if (sggc_kind_uncollected[kind])
    { sggc_info.uncol_big_chunks += nch;
    }
    else
#endif
    { sggc_info.gen0_big_chunks += nch;
    }
  }

  sggc_data[index] = (sggc_dptr) data;
  sggc_info.total_mem_usage += data_size;  /* will be 0 if not allocated here */

  OFFSET(sggc_data,index,SGGC_CHUNK_SIZE);

  /* Return newly allocated object. */

  return v;

fail: 

  /* Some allocation failed.  Return SGGC_NO_OBJECT, after freeing 'data', 
     and for a big segment, putting it back in 'unused'. */

  if (big)
  { if (v != SGGC_NO_OBJECT) 
    { sbset_add (&unused, v);
    }
    sggc_mem_free(data);
  }
  else
  { small_data_area_next -= SMALL_DATA_AREA_SIZE;
  }

  return SGGC_NO_OBJECT;
}


/* ALLOCATE AN OBJECT WITH GIVEN TYPE AND LENGTH. */

sggc_cptr_t sggc_alloc (sggc_type_t type, sggc_length_t length)
{
  sggc_kind_t kind = sggc_kind(type,length);

  if (SGGC_DEBUG) 
  { printf("sggc_alloc: type %u, length %u, kind %d\n",
            (unsigned) type, (unsigned) length, (int) kind);
  }

  return sggc_alloc_kind_type_length (kind, type, length);
}


/* ALLOCATE AN OBJECT WITH GIVEN KIND AND LENGTH.  
  
   Not defined if the application did not provide SGGC_KIND_TYPES. */

#ifdef SGGC_KIND_TYPES

sggc_cptr_t sggc_alloc_kind (sggc_kind_t kind, sggc_length_t length)
{
  if (SGGC_DEBUG) 
  { printf("sggc_alloc_kind: kind %d (type %u), length %u\n", 
            (int) kind, (unsigned) sggc_kind_types[kind], (unsigned) length);
  }

  return sggc_alloc_kind_type_length (kind, sggc_kind_types[kind], length);
}

#endif


/* ALLOCATE AN OBJECT WITH GIVEN KIND, WHICH MUST BE FOR A SMALL SEGMENT. 

   Not defined if the application did not provide SGGC_KIND_TYPES. */

#ifdef SGGC_KIND_TYPES

sggc_cptr_t sggc_alloc_small_kind (sggc_kind_t kind)
{
  if (SGGC_DEBUG) 
  { printf("sggc_alloc_small_kind: kind %d (type %u)\n", 
            (int) kind, (unsigned) sggc_kind_types[kind]);
  }

  return sggc_alloc_kind_type_length (kind, sggc_kind_types[kind], 0);
}

#endif


/* REGISTER A CONSTANT SEGMENT. */

sggc_cptr_t sggc_constant (sggc_type_t type, sggc_kind_t kind, int n_objects,
                           char *data
#ifdef SGGC_AUX1_SIZE
                         , char *aux1
#endif
#ifdef SGGC_AUX2_SIZE
                         , char *aux2
#endif
)
{
  sbset_bits_t bits;
  int i;

  if (n_objects < 1) abort();          /* must be at least one object */
  if (n_objects > kind_objects[kind]) abort(); /* too many objects */

  bits = 0;
  for (i = 0; i < n_objects; i++)
  { bits |= 1 << (i*sggc_kind_chunks[kind]);
  }

  sbset_index_t index = new_segment();
  if (index < 0)
  { return SGGC_NO_OBJECT;
  }

  struct sbset_segment *seg = SBSET_SEGMENT(index);
  sggc_cptr_t v = SGGC_CPTR_VAL(index,0);

  /* The seg->X.Small fields below coincide with the seg->X.Big fields. */

  seg->X.Small.big = sggc_kind_chunks[kind] == 0;
  seg->X.Small.constant = 1;
  seg->X.Small.kind = kind;

  sbset_add (&constants, v);
  sbset_assign_segment_bits (&constants, v, bits);

  sggc_type[index] = type;

  sggc_data[index] = (sggc_dptr) data;
  OFFSET(sggc_data,index,SGGC_CHUNK_SIZE);

# ifdef SGGC_AUX1_SIZE
    sggc_aux1[index] = (sggc_dptr) aux1;
    OFFSET(sggc_aux1,index,SGGC_AUX1_SIZE);
    seg->X.Small.aux1_off = 0;
# endif

# ifdef SGGC_AUX2_SIZE
    sggc_aux2[index] = (sggc_dptr) aux2;
    OFFSET(sggc_aux2,index,SGGC_AUX2_SIZE);
    seg->X.Small.aux2_off = 0;
# endif
    
  if (SGGC_DEBUG) 
  { printf("sggc_constant: first object in segment is %x\n", (unsigned)v);
  }

  return v;
}


/* ---------------------------- GARBAGE COLLECTION -------------------------- */


/* PRINT DEBUG INFORMATION FOR GARBAGE COLLECTION. */

static void collect_debug (void)
{ 
  int k;

  printf(
  "  unused: %d, old_to_new: %d, to_look_at: %d, constants: %d\n",
       sbset_n_elements(&unused), 
       sbset_n_elements(&old_to_new),
       sbset_n_elements(&to_look_at),
       sbset_n_elements(&constants));

  printf("    old gen 1");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" [%d]: %3d ",k,sbset_n_elements(&old_gen1[k]));
  }
  printf(" big: %3d ",sbset_n_elements(&old_gen1_big));
  printf("\n");

  printf("    old gen 2");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" [%d]: %3d ",k,sbset_n_elements(&old_gen2[k]));
  }
  printf(" big: %3d ",sbset_n_elements(&old_gen2_big));
  printf("\n");

#ifdef SGGC_KIND_UNCOLLECTED
  printf("  uncollected");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" [%d]: %3d ",k,sbset_n_elements(&uncollected[k]));
  }
  printf("\n");
#endif

  printf("  free_or_new");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" [%d]: %3d ",k,sbset_n_elements(&free_or_new[k]));
  }
  printf("\n");

  printf("next_free_val");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { if (sggc_next_free_val[k] == SGGC_NO_OBJECT)
    { printf(" [%d]: --- ",k);
    }
    else
    { printf(" [%d]: %3llx%c",k,(unsigned long long)sggc_next_free_val[k],
                                 sggc_next_segment_not_free[k] ? ' ' : '^');
    }
  }
  printf("\n");

  printf("             ");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" %08lx ",(unsigned long)(sggc_next_free_bits[k]>>32));
  }
  printf("\n");

  printf("             ");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" %08lx ",(unsigned long)(sggc_next_free_bits[k]&0xffffffff));
  }
  printf("\n");
}


/* PUT AN OBJECT IN THE APPROPRIATE OLD GENERATION.  The object is
   assumed to have already been removed from its 'free_or_new' set. */

static void put_in_right_old_gen (sggc_cptr_t v)
{
  const sggc_kind_t k = SGGC_KIND(v);
  struct sbset *ogen1 = sggc_kind_chunks[k]==0 ? &old_gen1_big : &old_gen1[k];

  if (collect_level > 0)
  { 
    if (sbset_remove (ogen1, v))
    { 
      /* Object is in generation 1; move it to generation 2. */

      sbset_add (sggc_kind_chunks[k]==0 ? &old_gen2_big : &old_gen2[k], v);
      if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen2\n",(unsigned)v);
      return;
    }

    if (collect_level == 2 && sbset_chain_contains (SGGC_OLD_GEN2_UNCOL, v))
    { 
      /* Object is in generation 2; leave it there. */

      return;
    }
  }  

  /* Object is in generation 0; move it to generation 1. */

  sbset_add (ogen1, v);
  if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen1\n",(unsigned)v);
}


/* DO A GARBAGE COLLECTION AT THE SPECIFIED LEVEL. 

   This is done using several sub-procedures, primarily so that profiling
   will show how much time is spent in each.  (Accordingly, most have
   external scope even though only used here, to discourage inlining.) 
 */

  /* Put objects in the old generations being collected in the 
     free_or_new set for their kind. */

void sggc_collect_put_in_free_or_new (void)
{
  sggc_kind_t k;
  sggc_cptr_t v;

  /* Put old objects of small kinds in the corresponding free_or_new set. */

  for (k = 0; k < SGGC_N_KINDS; k++) 
  {
    if (!SGGC_SEGMENT_AT_A_TIME) /* do it the old way, one object at a time */
    {
      if (collect_level == 2)
      { for (v = sbset_first(&old_gen2[k], 0);
             v != SBSET_NO_VALUE;
             v = sbset_chain_next(SGGC_OLD_GEN2_UNCOL,v))
        { sbset_add (&free_or_new[k], v);
          if (SGGC_DEBUG)
          { printf("sggc_collect: put %x from old_gen2 in free\n",(unsigned)v);
          }
        }
      }
  
      if (collect_level >= 1)
      { for (v = sbset_first(&old_gen1[k], 0);
             v != SBSET_NO_VALUE;
             v = sbset_chain_next(SGGC_OLD_GEN1,v))
        { sbset_add (&free_or_new[k], v);
          if (SGGC_DEBUG)
          { printf("sggc_collect: put %x from old_gen1 in free\n",(unsigned)v);
          }
        }
      }
    }
    else /* do it a segment at a time */
    {
      if (collect_level == 2)
      { for (v = sbset_first(&old_gen2[k], 0); 
             v != SBSET_NO_VALUE; 
             v = sbset_chain_next_segment(SGGC_OLD_GEN2_UNCOL,v))
        { sbset_add_segment (&free_or_new[k], v, SGGC_OLD_GEN2_UNCOL);
          if (SGGC_DEBUG) 
          { DO_FOR_SEGMENT (SGGC_OLD_GEN2_UNCOL, v,
              printf("sggc_collect: put %x from old_gen2 in free\n",
                      (unsigned)w));
          }
        }
      }
  
      if (collect_level >= 1)
      { for (v = sbset_first(&old_gen1[k], 0);
             v != SBSET_NO_VALUE; 
             v = sbset_chain_next_segment(SGGC_OLD_GEN1,v))
        { sbset_add_segment (&free_or_new[k], v, SGGC_OLD_GEN1);
          if (SGGC_DEBUG) 
          { DO_FOR_SEGMENT (SGGC_OLD_GEN1, v,
              printf("sggc_collect: put %x from old_gen1 in free\n",
                      (unsigned)w));
          }
        }
      }
    }
  }

  /* Put old objects of big kinds in the corresponding free_or_new set. */

  if (collect_level == 2)
  { for (v = sbset_first(&old_gen2_big, 0);
         v != SBSET_NO_VALUE;
         v = sbset_chain_next(SGGC_OLD_GEN2_UNCOL,v))
    { sbset_add (&free_or_new[SGGC_KIND(v)], v);
      if (SGGC_DEBUG)
      { printf("sggc_collect: put %x from old_gen2 in free\n",(unsigned)v);
      }
    }
  }
  
  if (collect_level >= 1)
  { for (v = sbset_first(&old_gen1_big, 0);
         v != SBSET_NO_VALUE;
         v = sbset_chain_next(SGGC_OLD_GEN1,v))
    { sbset_add (&free_or_new[SGGC_KIND(v)], v);
      if (SGGC_DEBUG)
      { printf("sggc_collect: put %x from old_gen1 in free\n",(unsigned)v);
      }
    }
  }
}

  /* Handle old-to-new references.  Done in cooperation with
     sggc_look_at, using the global variables collect_level (the level
     of collection being done) and old_to_new_check (which contains
     the generation of the referring object (1 or 2, or 3 for
     uncollected), except it is cleared to 0 to indicate that further
     special processing is unnecessary (which may also mean that the
     old-to-new entry is still needed), and to -1 to indicate that
     furthermore subsequent calls of sggc_look_at should be ignored. */

void sggc_collect_old_to_new (void)
{
  sggc_cptr_t v;

  v = sbset_first(&old_to_new, 0);

  while (v != SBSET_NO_VALUE)
  { int remove = 0;
    if (SGGC_DEBUG) 
    { printf ("sggc_collect: old->new for %x (gen%d)\n", (unsigned)v,
        sbset_chain_contains(SGGC_OLD_GEN2_UNCOL,v) ? 2 
#ifdef SGGC_KIND_UNCOLLECTED
          + sggc_kind_uncollected[SGGC_KIND(v)]
#endif
        : sbset_chain_contains(SGGC_OLD_GEN1,v) ? 1 : 0);
    }
    if (sbset_chain_contains (SGGC_OLD_GEN2_UNCOL, v)) /* v is oldgen2 or uncol */
    { old_to_new_check = 2;
#ifdef SGGC_KIND_UNCOLLECTED
      if (sggc_kind_uncollected[SGGC_KIND(v)])
      { old_to_new_check = 3;
      }
#endif
    }
    else /* v is in old generation 1 */
    { if (collect_level == 0)
      { old_to_new_check = 0;
        remove = 1;
      }
      else
      { old_to_new_check = 1;
      }
    }
#   ifdef SGGC_FIND_OBJECT_RETURN
      sggc_look_at (sggc_find_object_ptrs (v));
#   else
      sggc_find_object_ptrs (v);
#   endif
    if (old_to_new_check > 0) 
    { remove = 1;
    }
    if (SGGC_DEBUG) 
    { if (remove) 
      { printf("sggc_collect: old->new for %x no longer needed\n",(unsigned)v);
      }
      else 
      { printf("sggc_collect: old->new for %x still needed\n",(unsigned)v);
      }
    }
    v = sbset_next (&old_to_new, v, remove);
  }
}

  /* Keep looking at objects in the to_look_at set, putting them in
     the correct old generation, and getting the application to find
     any pointers they contain (which may add to the to_look_at set),
     until there are no more in the set. */

void sggc_collect_look_at (void)
{
  sggc_cptr_t v;

# ifdef SGGC_AFTER_MARKING
  int rep = 0;
# endif

  do
  { 
    while ((v = sbset_first (&to_look_at, 1)) != SGGC_NO_OBJECT)
    {
#     ifdef SGGC_FIND_OBJECT_RETURN
      { for (;;)
        { if (SGGC_DEBUG) printf("sggc_collect: looking at %x\n",(unsigned)v);
          put_in_right_old_gen (v);
          v = sggc_find_object_ptrs (v);
          if (v == SGGC_NO_OBJECT)
          { break;
          }
          if (SGGC_DEBUG)
          { printf ("sggc_collect: from find_object_ptrs: %x\n", (unsigned)v);
          }
          if (!sbset_chain_contains(SGGC_UNUSED_FREE_NEW,v))
          { break;
          }
          sbset_remove (&free_or_new[SGGC_KIND(v)], v);
        } 
      }
#     else
      { if (SGGC_DEBUG) printf("sggc_collect: looking at %x\n",(unsigned)v);
        put_in_right_old_gen (v);
        sggc_find_object_ptrs (v);
      }
#     endif
    }

#   ifdef SGGC_AFTER_MARKING
    sggc_after_marking (collect_level, rep++);
#   endif

  } while (sbset_first (&to_look_at, 0) != SGGC_NO_OBJECT);
}

  /* Remove small objects still in the free_or_new set from the old
     generations that were collected.  Also remove them from the
     old-to-new set.  Also calls any functions set up for small kinds
     with sggc_call_for_newly_freed_object. */

void sggc_collect_remove_free_small (void)
{
  sggc_kind_t k;
  sggc_cptr_t v;

  for (k = 0; k < SGGC_N_KINDS; k++)
  { 
    if (sggc_kind_chunks[k] != 0)  /* kind is for small objects */
    {
      int (*call)(sggc_cptr_t) = call_for_newly_freed[k];

      /* If a function to call for newly-freed objects has been set up
         for this kind (with sggc_call_for_newly_freed_object), we
         need to do a preliminary scan of objects in free_or_new[k] up
         to the point where we reach objects that were freed before
         this collection and weren't allocated since (which may now be
         mixed in with objects that were moved from old generations,
         if collect_level is greater than zero).  (Note that we also
         have to handle the case where we've used all of the free set
         and are now allocating from a single newly-created segment.)
         If one of these newly-freed objects is in an old generation,
         we remove it now (but note that this doesn't get all free
         objects, so we still need the scan done below). */

      if (call && (v = sbset_first (&free_or_new[k], 0)) != SGGC_NO_OBJECT)
      {
        sggc_cptr_t next_free = sggc_next_free_val[k];

        for (;;)
        { 
          if (v == SGGC_NO_OBJECT)
          { break;
          }

          if (v == next_free)
          { if (!sggc_next_segment_not_free[k])
            { break;
            }
            v = sbset_chain_next_segment (SGGC_UNUSED_FREE_NEW, v);
            if (v == SGGC_NO_OBJECT)
            { break;
            }
          }

          sggc_cptr_t ov = v;

          v = sbset_chain_next (SGGC_UNUSED_FREE_NEW, v);

          /* Call the function to call for a newly-freed object of this kind.
             If it returns a non-zero value, don't free the object after all. */
  
          if ((*call)(ov))
          { if (SGGC_DEBUG) 
            { printf ("sggc_collect: not freeing %x after all\n", (unsigned)ov);
            }
            (void) sbset_remove (&free_or_new[k], ov);
            put_in_right_old_gen(ov);
            continue;
          }
  
          /* Remove the object from any old_gen or old_to_new, if it's there.
             (Otherwise, we'd call the function again below.) */
  
          if (collect_level > 0)
          { if (sbset_remove (&old_gen1[k], ov))
            { if (SGGC_DEBUG)
              { printf("sggc_collect: %x in old_gen1 now free\n",(unsigned)ov);
              }
              (void) sbset_remove (&old_to_new, ov);
            }
            else if (collect_level > 1 && sbset_remove (&old_gen2[k], ov))
            { if (SGGC_DEBUG)
              { printf("sggc_collect: %x in old_gen2 now free\n",(unsigned)ov);
              }
              (void) sbset_remove (&old_to_new, ov);
            }
          }
        }
      }

      /* Scan the old generation sets, not the free sets (unless
         necessary above), since this is likely faster, if lots of
         objects were allocated but not used for long, and hence are
         in the free sets. */

      if (!SGGC_SEGMENT_AT_A_TIME || call_for_newly_freed[k])
      { 
        /* Do it one object at a time. */

        if (collect_level == 2)
        { v = sbset_first (&old_gen2[k], 0); 
          while (v != SBSET_NO_VALUE)
          { sggc_cptr_t ov = v;
            v = sbset_chain_next (SGGC_OLD_GEN2_UNCOL, v);
            if (sbset_chain_contains (SGGC_UNUSED_FREE_NEW, ov))
            { if (call && (*call)(ov))
              { if (SGGC_DEBUG) 
                { printf ("sggc_collect: not freeing %x after all\n",
                           (unsigned) ov);
                }
                (void) sbset_remove (&free_or_new[k], ov);
                put_in_right_old_gen(ov);
                continue;
              }
              if (SGGC_DEBUG)
              { printf("sggc_collect: %x in old_gen2 now free\n",
                        (unsigned) ov);
              }
              sbset_remove (&old_to_new, ov);
              sbset_remove (&old_gen2[k], ov);
            }
          }
        }
    
        if (collect_level >= 1)
        { v = sbset_first (&old_gen1[k], 0); 
          while (v != SBSET_NO_VALUE)
          { sggc_cptr_t ov = v;
            v = sbset_chain_next (SGGC_OLD_GEN1, v);
            if (sbset_chain_contains (SGGC_UNUSED_FREE_NEW, ov))
            { if (call && (*call)(ov))
              { if (SGGC_DEBUG) 
                { printf ("sggc_collect: not freeing %x after all\n",
                           (unsigned) ov);
                }
                (void) sbset_remove (&free_or_new[k], ov);
                put_in_right_old_gen(ov);
                continue;
              }
              if (SGGC_DEBUG)
              { printf("sggc_collect: %x in old_gen1 now free\n",
                        (unsigned) ov);
              }
              sbset_remove (&old_to_new, ov);
              sbset_remove (&old_gen1[k], ov);
            }
          }
        }
      }
      else 
      { 
        /* Do it a segment at a time. */

        if (collect_level == 2)
        { v = sbset_first(&old_gen2[k], 0); 
          while (v != SBSET_NO_VALUE)
          { if (SGGC_DEBUG)
            { DO_FOR_SEGMENT (SGGC_OLD_GEN2_UNCOL, v, 
                if (sbset_chain_contains(SGGC_UNUSED_FREE_NEW,w))
                 printf("sggc_collect: %x in old_gen2 now free\n",(unsigned)w));
            }
            sggc_cptr_t nv = sbset_chain_next_segment(SGGC_OLD_GEN2_UNCOL,v);
            sbset_remove_segment (&old_gen2[k], v, SGGC_UNUSED_FREE_NEW);
            if (sbset_chain_contains_any_in_segment (SGGC_UNUSED_FREE_NEW, v))
            { sbset_remove_segment (&old_to_new, v, SGGC_UNUSED_FREE_NEW);
            }
            v = nv;
          }
        }
    
        if (collect_level >= 1)
        { v = sbset_first(&old_gen1[k], 0); 
          while (v != SBSET_NO_VALUE)
          { if (SGGC_DEBUG)
            { DO_FOR_SEGMENT (SGGC_OLD_GEN1, v, 
                if (sbset_chain_contains(SGGC_UNUSED_FREE_NEW,w))
                 printf("sggc_collect: %x in old_gen1 now free\n",(unsigned)w));
            }
            sggc_cptr_t nv = sbset_chain_next_segment(SGGC_OLD_GEN1,v);
            sbset_remove_segment (&old_gen1[k], v, SGGC_UNUSED_FREE_NEW);
            if (sbset_chain_contains_any_in_segment (SGGC_UNUSED_FREE_NEW, v))
            { sbset_remove_segment (&old_to_new, v, SGGC_UNUSED_FREE_NEW);
            }
            v = nv;
          }
        }
      }

      /* Clear freed data and aux areas for small kinds if asked to do so. */

#     ifdef SGGC_CLEAR_FREE
      { sggc_nchunks_t nch = sggc_kind_chunks[k];
        sggc_cptr_t p;
        for (p = sbset_first (&free_or_new[k], 0); 
             p != SGGC_NO_OBJECT;
             p = sbset_next (&free_or_new[k], p, 0))
        { if (SGGC_DATA(p) != NULL)
          { memset (SGGC_DATA(p), SGGC_CLEAR_DATA_BYTE, SGGC_CHUNK_SIZE * nch);
          }
#         ifdef SGGC_AUX1_SIZE
#         ifdef SGGC_AUX1_READ_ONLY
          if (!kind_aux1_read_only[k])
#         endif
          { memset (SGGC_AUX1(p), SGGC_CLEAR_AUX1_BYTE, SGGC_AUX1_SIZE);
          }
#         endif
#         ifdef SGGC_AUX2_SIZE
#         ifdef SGGC_AUX2_READ_ONLY
          if (!kind_aux2_read_only[k])
#         endif
          { memset (SGGC_AUX2(p), SGGC_CLEAR_AUX2_BYTE, SGGC_AUX2_SIZE);
          }
#         endif
        }
      }
#     endif

      /* Remove all objects from the free set if we aren't reusing them. */

      if (do_not_reuse_memory)
      { do
        { v = sbset_first (&free_or_new[k], 1); 
        } while (v != SGGC_NO_OBJECT);
      }
    }
  }
}

  /* Remove big objects still in the free_or_new set from the old
     generations that were collected.  Also remove them from the
     old-to-new set, put them in 'unused', and free their data storage
     (but not their auxiliary information).  Also calls any functions
     set up for big kinds with sggc_call_for_newly_freed_object. */

void sggc_collect_remove_free_big (void)
{
  sggc_kind_t k;
  sggc_cptr_t v;

  for (k = 0; k < SGGC_N_KINDS; k++)
  { 
    if (sggc_kind_chunks[k] == 0)  /* kind is for big objects */
    { 
      /* Scan the free_or_new[k] set (which contains only newly-free
         objects), which we have to look at to free their data even if
         they are not in old_gen1_big or old_gen2_big.  Note that since
         big objects are stored one-per-segment, there is nothing to be
         gained by trying to do this a segment at a time. */

      while ((v = sbset_first (&free_or_new[k], 1)) != SGGC_NO_OBJECT)
      { 
        /* Call the function to call for a newly-freed object of this
           kind, if there is one.  If it returns a non-zero value, don't
           free this object after all. */

        if (call_for_newly_freed[k] && (*call_for_newly_freed[k])(v))
        { if (SGGC_DEBUG) 
          { printf ("sggc_collect: not freeing %x after all\n", (unsigned)v);
          }
          put_in_right_old_gen(v);
          continue;
        }

        /* Find the number of chunks used by the object. */

        sggc_nchunks_t nch=CHUNKS_ALLOCATED(SBSET_SEGMENT(SBSET_VAL_INDEX(v)));

        /* Remove the object from any old_gen or old_to_new, if it's there.
           Modify the sggc_info count of chunks accordingly.  (Chunk counts
           were previously updated for older generations assuming objects
           will remain in use.) */

        if (collect_level > 0 && sbset_remove (&old_gen1_big, v))
        { if (SGGC_DEBUG)
          { printf("sggc_collect: %x in old_gen1 now free (%d chunks)\n",
                    (unsigned)v, nch);
          }
          (void) sbset_remove (&old_to_new, v);
          sggc_info.gen2_big_chunks -= nch;
        }
        else if (collect_level > 1 && sbset_remove (&old_gen2_big, v))
        { if (SGGC_DEBUG)
          { printf("sggc_collect: %x in old_gen2 now free (%d chunks)\n",
                    (unsigned)v, nch);
          }
          (void) sbset_remove (&old_to_new, v);
          sggc_info.gen2_big_chunks -= nch;
        }
        else
        { if (SGGC_DEBUG)
          { printf(
              "sggc_collect: %x that was newly-allocated is free (%d chunks)\n",
               (unsigned)v, nch);
          }
          sggc_info.gen1_big_chunks -= nch;
        }

        /* Clear data and auxiliary info areas if asked to do so.  Note that
           big objects can't have read-only auxiliary information. */

#       ifdef SGGC_CLEAR_FREE
        { sggc_nchunks_t nch = sggc_kind_chunks[k];
          memset (SGGC_DATA(v), SGGC_CLEAR_DATA_BYTE, SGGC_CHUNK_SIZE * nch);
#         ifdef SGGC_AUX1_SIZE
          { memset (SGGC_AUX1(v), SGGC_CLEAR_AUX1_BYTE, SGGC_AUX1_SIZE * nch);
          }
#         endif
#         ifdef SGGC_AUX2_SIZE
          { memset (SGGC_AUX2(v), SGGC_CLEAR_AUX2_BYTE, SGGC_AUX2_SIZE * nch);
          }
#         endif
        }
#       endif

        /* Don't free memory or put in 'unused' if we're not reusing memory. */

        if (do_not_reuse_memory)
        { continue;
        }

        /* Free the object's data area, and update memory usage in sggc_info. */

        if (SGGC_DEBUG) 
        { printf ("sggc_collect: calling free for data for %x:: %p\n", 
                   v, SGGC_DATA(v));
        }
        sggc_mem_free ((char *) SGGC_DATA(v));
        sggc_info.total_mem_usage -= (size_t) SGGC_CHUNK_SIZE * nch;

        /* Put it in 'unused', for later re-use. */

        if (SGGC_DEBUG) 
        { printf("sggc_collect: putting %x in unused\n",(unsigned)v);
        }
        sbset_add(&unused,v); /* allowed since v was removed with sbset_first */
                              /*   and it was the only value in its segment   */
      }
    }
  }
}

void sggc_collect (int level)
{ 
  int k;

  if (SGGC_DEBUG) printf("sggc_collect: level %d\n",level);
  if (SGGC_DEBUG) collect_debug();

  collect_level = level;

  if (sbset_first(&to_look_at, 0) != SBSET_NO_VALUE) abort();

  /* Do preliminary update of big chunk counts, which will be modified 
     later when some big objects are found to be free. */

  if (level > 0) 
  { sggc_info.gen2_big_chunks += sggc_info.gen1_big_chunks;
    sggc_info.gen1_big_chunks = 0;
  }
  sggc_info.gen1_big_chunks += sggc_info.gen0_big_chunks;
  sggc_info.gen0_big_chunks = 0;

  /* Put collected generations in free sets. */

  sggc_collect_put_in_free_or_new();

  /* Handle old-to-new references. */

  sggc_collect_old_to_new();

  /* Get the application to take root pointers out of the free_or_new set,
     and put them in the to_look_at set. */

  old_to_new_check = 0;  /* no special old-to-new processing in sggc_look_at */
  sggc_find_root_ptrs();

  /* Look at objects until no more to see. */

  sggc_collect_look_at();

  /* Handle freed small objects. */

  sggc_collect_remove_free_small();

  /* Handle freed big objects. */

  sggc_collect_remove_free_big();

  /* For each kind, set up sggc_next_free_val, and sggc_next_free_bits to 
     use all of free_or_new.  For uncollected kinds, we just leave these as
     they were, since nothing was freed in the collection. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { if (sggc_kind_chunks[k] != 0)  /* kind uses small segments */
    {
#ifdef SGGC_KIND_UNCOLLECTED
      if (!sggc_kind_uncollected[k])
#endif
      { sbset_value_t n = sbset_first (&free_or_new[k], 0);
        sggc_next_free_val[k] = n;
        if (n == SGGC_NO_OBJECT)
        { sggc_next_free_bits[k] = 0;
        }
        else 
        { sggc_next_free_bits[k] 
            = sbset_chain_segment_bits (SGGC_UNUSED_FREE_NEW, n)
                >> SBSET_VAL_OFFSET(n);
        }
        sggc_next_segment_not_free[k] = 0;
      }
    }
  }

  /* Update counts in the info structure (big chunks already updated). */

  sggc_info.gen0_count = 0;

  sggc_info.gen1_count = sbset_n_elements(&old_gen1_big);
  sggc_info.gen2_count = sbset_n_elements(&old_gen2_big);
  for (k = 0; k < SGGC_N_KINDS; k++)
  { sggc_info.gen1_count += sbset_n_elements(&old_gen1[k]);
    sggc_info.gen2_count += sbset_n_elements(&old_gen2[k]);
  }

#ifdef SGGC_KIND_UNCOLLECTED
  if (1)  /* not expensive, so maybe keep this consistency check enabled */
  { unsigned cnt = 0;
    for (k = 0; k < SGGC_N_KINDS; k++)
    { if (sggc_kind_uncollected[k])
      { cnt += sbset_n_elements(&uncollected[k]);
      }
    }
    if (cnt != sggc_info.uncol_count) 
    { abort();
    }
  }
#endif

  collect_level = -1;

  /* Call the function registered to be called for every object still in use. */

  if (call_for_object_in_use)
  { 
    sggc_cptr_t v;

    for (k = 0; k < SGGC_N_KINDS; k++)
    { sggc_nchunks_t nch = sggc_kind_chunks[k];
      for (v = sbset_first(&old_gen1[k],0); 
           v != SGGC_NO_OBJECT;
           v = sbset_chain_next(SGGC_OLD_GEN1,v))
      { call_for_object_in_use (v, nch != 0 ? nch 
          : CHUNKS_ALLOCATED (SBSET_SEGMENT (SBSET_VAL_INDEX(v))));
      }
      for (v = sbset_first(&old_gen2[k],0); 
           v != SGGC_NO_OBJECT;
           v = sbset_chain_next(SGGC_OLD_GEN2_UNCOL,v))
      { call_for_object_in_use (v, nch != 0 ? nch 
          : CHUNKS_ALLOCATED (SBSET_SEGMENT (SBSET_VAL_INDEX(v))));
      }
#ifdef SGGC_KIND_UNCOLLECTED
      for (v = sbset_first(&uncollected[k],0); 
           v != SGGC_NO_OBJECT;
           v = sbset_chain_next(SGGC_OLD_GEN2_UNCOL,v))
      { call_for_object_in_use (v, nch != 0 ? nch 
          : CHUNKS_ALLOCATED (SBSET_SEGMENT (SBSET_VAL_INDEX(v))));
      }
#endif
    }

    for (v = sbset_first(&old_gen1_big,0); 
         v != SGGC_NO_OBJECT;
         v = sbset_chain_next(SGGC_OLD_GEN1,v))
    { call_for_object_in_use (v, 
        CHUNKS_ALLOCATED (SBSET_SEGMENT (SBSET_VAL_INDEX(v))));
    }
    for (v = sbset_first(&old_gen2_big,0); 
         v != SGGC_NO_OBJECT;
         v = sbset_chain_next(SGGC_OLD_GEN2_UNCOL,v))
    { call_for_object_in_use (v, 
        CHUNKS_ALLOCATED (SBSET_SEGMENT (SBSET_VAL_INDEX(v))));
    }
  }

  if (SGGC_DEBUG) printf("sggc_collect: done\n");
  if (SGGC_DEBUG) collect_debug();
}


/* ------------------------- APPLICATION INTERFACE -------------------------- */


/* TELL THE GARBAGE COLLECTOR THAT AN OBJECT NEEDS TO BE LOOKED AT.

   The principal use of this is to mark objects as in use.  If the
   object is presently in the free_or_new set for its kind, it is
   removed, and put in the set of objects to be looked at.  (If it is
   not in free_or_new, it has already been looked at, and so needn't
   be looked at again.)

   This procedure is also used as part of the old-to-new scheme to
   check whether an object in the old-to-new set still needs to be
   there, as well as sometimes marking the objects it points to.  Note
   that for this purpose, it is essential that for now the objects
   looked at are just put in 'to_look_at', not put in the old
   generation where they will eventually end up.  Also note that we
   cannot test for an object being in generation 0 by checking if it
   is in 'free_or_new', since it may have already been removed. */

void sggc_look_at (sggc_cptr_t cptr)
{
  if (cptr == SGGC_NO_OBJECT)
  { return;
  }

  if (SGGC_DEBUG) 
  { printf ("sggc_look_at: %x %d\n", (unsigned)cptr, old_to_new_check);
  }

  if (old_to_new_check != 0)
  { if (old_to_new_check < 0)
    { return;
    }
#ifdef SGGC_KIND_UNCOLLECTED
    else if (old_to_new_check == 3) /* reference from an uncollected object */
    { if (!sggc_is_constant(cptr)   /* not to a constant or uncollected obj */
            && !sggc_kind_uncollected[SGGC_KIND(cptr)])
      { old_to_new_check = 0;
      }
    }
#endif
    else if (collect_level == 0) /* ref won't be from generation 1 */
    { if (!sbset_chain_contains (SGGC_OLD_GEN2_UNCOL, cptr)) /* to gen 0 or 1 */
      { old_to_new_check = 0;
      }
    }
    else if (collect_level == 1 && old_to_new_check == 2)
    { if (!sbset_chain_contains (SGGC_OLD_GEN2_UNCOL, cptr) /* ref is to */
            && !sbset_chain_contains (SGGC_OLD_GEN1, cptr)) /*   gen 0   */
      { old_to_new_check = 0;
      }
    }
    else /* collect_level==2 || collect_level == 1 && old_to_new_check == 1 */
    { if (!sbset_chain_contains (SGGC_OLD_GEN2_UNCOL, cptr) /* ref is to */
            && !sbset_chain_contains (SGGC_OLD_GEN1, cptr)) /*   gen 0   */
      { old_to_new_check = -1;
      }
      return;
    }
  }

  if (sbset_chain_contains(SGGC_UNUSED_FREE_NEW,cptr)) /* faster than remove */
  { sbset_remove (&free_or_new[SGGC_KIND(cptr)], cptr);
    sbset_add (&to_look_at, cptr);
    if (SGGC_DEBUG) printf("sggc_look_at: will look at %x\n",(unsigned)cptr);
  }
}


/* MARK AN OBJECT AS IN USE, BUT DON'T FOLLOW REFERENCES WITHIN IT. */

void sggc_mark (sggc_cptr_t cptr)
{
  if (SGGC_DEBUG) printf("sggc_mark: %x\n",(unsigned)cptr);

  if (cptr != SGGC_NO_OBJECT)
  { if (sbset_chain_contains(SGGC_UNUSED_FREE_NEW,cptr)) /* faster than remove*/
    { sbset_remove (&free_or_new[SGGC_KIND(cptr)], cptr);
      put_in_right_old_gen (cptr);
    }
  }
}


/* FIND THE FIRST UNCOLLECTED OBJECT OF A GIVEN KIND. */

sggc_cptr_t sggc_first_uncollected_of_kind (sggc_kind_t kind)
{
#ifdef SGGC_KIND_UNCOLLECTED
  return sbset_first (&uncollected[kind], 0);
#else
  return SGGC_NO_OBJECT;
#endif
}


/* REGISTER A FUNCTION TO BE CALLED FOR NEWLY-FREED OBJECTS. */

void sggc_call_for_newly_freed_object (sggc_kind_t kind,
                                       int (*fun) (sggc_cptr_t))
{
  call_for_newly_freed[kind] = fun;
}


/* REGISTER A FUNCTION TO BE CALLED FOR OBJECTS IN USE. */

void sggc_call_for_object_in_use (void (*fun) (sggc_cptr_t,sggc_nchunks_t))
{
  call_for_object_in_use = fun;
}



/* ENABLE OR DISABLE SUPPRESSION OF MEMORY REUSE. */

void sggc_no_reuse (int enable)
{
  do_not_reuse_memory = enable;
}


/* TRY TO CRASH IF COMPRESSED POINTER IS NOT VALID. 

   Currently, doesn't try to distinguish free from recently-allocated
   objects in UNUSED_FREE_NEW.  This might be possible without huge
   effort using a flag in a segment saying whether any objects in it
   are free. */

sggc_cptr_t sggc_check_valid_cptr (sggc_cptr_t cptr)
{
  unsigned index = SGGC_SEGMENT_INDEX(cptr);
  if (index >= next_segment)
  { abort();
  }

  if (!sggc_is_constant(cptr)
   && !sbset_chain_contains(SGGC_UNUSED_FREE_NEW,cptr)
   && !sbset_chain_contains(SGGC_OLD_GEN1,cptr)
   && !sbset_chain_contains(SGGC_OLD_GEN2_UNCOL,cptr))
  { abort();
  }

  return cptr;
}
