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

#if SET_STATIC
#  include "set.c"     /* Define set procedures here as static, not linked */
#endif


/* DEBUGGING FLAG.  Set to 1 to enable debug output.  May be set by a compiler
   flag, in which case it isn't overridden here. */

#ifndef SGGC_DEBUG
#define SGGC_DEBUG 0
#endif


/* ENABLE/DISABLE EXTRA CHECKS.  These are consistency checks that take
   non-negligible time. */

#define EXTRA_CHECKS 0


/* ALLOCATE / FREE MACROS.  Defaults to the system calloc/free if something
   else is not defined in sggc-app.h. */

#ifndef sggc_alloc_zeroed
#define sggc_alloc_zeroed(n) calloc(n,1)
#endif

#ifndef sggc_free
#define sggc_free free
#endif


/* NUMBERS OF CHUNKS ALLOWED FOR AN OBJECT IN KINDS OF SEGMENTS.  Zero
   means that this kind of segment is big, containing one object of
   size found using sggc_chunks.  The application must define the
   initialization as SGGC_KIND_CHUNKS.  Entries must not be greater
   than SGGC_CHUNKS_IN_SMALL_SEGMENT. */

const int sggc_kind_chunks[SGGC_N_KINDS] = SGGC_KIND_CHUNKS;


/* NUMBERS OF OBJECTS IN SEGMENTS OF EACH KIND, AND END OF ITS CHUNKS.  
   Computed at initialization from SGGC_CHUNKS_IN_SMALL_SEGMENT and 
   sggc_kind_chunks. */

static int kind_objects[SGGC_N_KINDS];
static int kind_chunk_end[SGGC_N_KINDS];


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


/* BIT VECTORS FOR FULL SEGMENTS.  Computed at initialization from 
   sggc_kind_chunks and SET_OFFSET_BITS. */

static set_bits_t kind_full[SGGC_N_KINDS];


/* SETS OF OBJECTS. */

static struct set unused;                     /* Segment exists, but not used */
static struct set free_or_new[SGGC_N_KINDS];  /* Free or newly allocated */
static struct set old_gen1;                   /* Survived collection once */
static struct set old_gen2;                   /* Survived collection >1 time */
static struct set old_to_new;                 /* May have old->new references */
static struct set to_look_at;                 /* Not yet looked at in sweep */
static struct set constants;                  /* Prealloc'd constant segments */


/* NEXT FREE OBJECT AND END OF FREE OBJECTS FOR EACH KIND.  The next_free[k]
   pointer is to a free object, unless it is equal to end_free[k].  These
   may be SGGC_NO_OBJECT. */

static sggc_cptr_t next_free[SGGC_N_KINDS];   /* Next free object of kind */
static sggc_cptr_t end_free[SGGC_N_KINDS];    /* Indicator when past free obj */


/* MAXIMUM NUMBER OF SEGMENTS, AND INDEX OF NEXT SEGMENT TO USE. */

static set_offset_t maximum_segments;         /* Max segments, fixed for now */
static set_offset_t next_segment;             /* Number of segments in use */


/* GLOBAL VARIABLES USED FOR LOOKING AT OLD-NEW REFERENCES. */

static int collect_level = -1; /* Level of current garbage collection */
static int old_to_new_check;   /* 1 if should look for old-to-new reference */


/* ------------------------------ INITIALIZATION ---------------------------- */


/* INITIALIZE SEGMENTED MEMORY.  Allocates space for pointers for the
   specified number of segments (currently not expandable).  Returns
   zero if successful, non-zero if allocation fails. */

int sggc_init (int max_segments)
{
  int i, j, k;

  /* Check that auxiliary block sixes aren't too big. */

# ifdef SGGC_AUX1_SIZE
    if (SGGC_AUX1_BLOCK_SIZE * SGGC_CHUNKS_IN_SMALL_SEGMENT > 256) abort();
# endif

# ifdef SGGC_AUX2_SIZE
    if (SGGC_AUX2_BLOCK_SIZE * SGGC_CHUNKS_IN_SMALL_SEGMENT > 256) abort();
# endif

  /* Allocate space for pointers to segment descriptors, data, and
     possibly auxiliary information for segments.  Information for
     segments these point to is allocated later, when the segment is
     actually needed. */

  sggc_segment = sggc_alloc_zeroed (max_segments * sizeof *sggc_segment);
  if (sggc_segment == NULL)
  { return 1;
  }

  sggc_data = sggc_alloc_zeroed (max_segments * sizeof *sggc_data);
  if (sggc_data == NULL)
  { return 2;
  }

# ifdef SGGC_AUX1_SIZE
    sggc_aux1 = sggc_alloc_zeroed (max_segments * sizeof *sggc_aux1);
    if (sggc_aux1 == NULL)
    { return 3;
    }
# endif

# ifdef SGGC_AUX2_SIZE
    sggc_aux2 = sggc_alloc_zeroed (max_segments * sizeof *sggc_aux2);
    if (sggc_aux2 == NULL)
    { return 4;
    }
# endif

  /* Allocate space for holding the types of segments. */

  sggc_type = sggc_alloc_zeroed (max_segments * sizeof *sggc_type);
  if (sggc_type == NULL)
  { return 5;
  }

  /* Compute numbers of objects in segments of each kind, and
     initialize bit vectors that indicate when segments of different
     kinds are full, and are also used to initialize segments as full.
     Along the way, check that all big kinds correspond to types, and
     that small segments aren't too big. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { if (sggc_kind_chunks[k] == 0) /* big segment */
    { if (k >= SGGC_N_TYPES) abort();
      kind_full[k] = 1;
      kind_objects[k] = 1;
      kind_chunk_end[k] = 0;  /* ends when only object ends */
    }
    else /* small segment */
    { if (sggc_kind_chunks[k] > SGGC_CHUNKS_IN_SMALL_SEGMENT) abort();
      kind_full[k] = 0;
      for (j = 0;
           j + sggc_kind_chunks[k] <= SGGC_CHUNKS_IN_SMALL_SEGMENT; 
           j += sggc_kind_chunks[k])
      { kind_full[k] |= (set_bits_t)1 << j;
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

  set_init(&unused,SET_UNUSED_FREE_NEW);
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { set_init(&free_or_new[k],SET_UNUSED_FREE_NEW);
  }
  set_init(&old_gen1,SET_OLD_GEN1);
  set_init(&old_gen2,SET_OLD_GEN2_CONST);
  set_init(&old_to_new,SET_OLD_TO_NEW);
  set_init(&to_look_at,SET_TO_LOOK_AT);
  set_init(&constants,SET_OLD_GEN2_CONST);

  /* Initialize to no free objects of each kind. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { next_free[k] = SGGC_NO_OBJECT;
    end_free[k] = SGGC_NO_OBJECT;
  }

  /* Record maximum segments, and initialize to no segments in use. */

  maximum_segments = max_segments;
  next_segment = 0;

  /* Initialize the sggc_info structure. */

  sggc_info.gen0_count = 0;
  sggc_info.gen1_count = 0;
  sggc_info.gen2_count = 0;
  sggc_info.big_chunks = 0;

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

/* *block = NULL; *pos = 0; return; */

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


/* ALLOCATE AN OBJECT OF SPECIFIED TYPE AND LENGTH.  The length is
   (possibly) used in determining the kind for the object, as
   determined by the application's sggc_kind function, and the number
   of chunks of storage it requires, as determined by the
   application's sggc_nchunks function.  

   The value returned is SGGC_NO_OBJECT if allocation fails, but note
   that it might succeed if retried after garbage collection is done
   (but this is left to the application to do if it wants to). */

sggc_cptr_t sggc_alloc (sggc_type_t type, sggc_length_t length)
{
  if (SGGC_DEBUG) 
  { printf("sggc_alloc: type %u, length %u\n",(unsigned)type,(unsigned)length);
  }

  sggc_kind_t kind = sggc_kind(type,length);
  sggc_index_t index;
  sggc_cptr_t v;
  int from_free;

  /* Look for an existing segment for this object to go in (and offset
     within).  For a small segment, the object found will be in
     free_or_new, but will not be between next_free and end_free, and
     so won't be taken again (at least before the next collection).
     For a big segment, the segment will be taken from 'unused', if
     one is there, and will be added to free_or_new for this kind. */

  from_free = 0;
  if (sggc_kind_chunks[kind] == 0) /* uses big segments */
  { v = set_first (&unused, 1);
    if (v != SGGC_NO_OBJECT)
    { if (SGGC_DEBUG) printf("sggc_alloc: found %x in unused\n",(unsigned)v);
      set_add (&free_or_new[kind], v);
    }
  }
  else /* uses small segments */
  { v = next_free[kind]; 
    if (v == end_free[kind])
    { v = SGGC_NO_OBJECT;
    }
    else
    { next_free[kind] = set_next (&free_or_new[kind], v, 0);
      if (SGGC_DEBUG) printf("sggc_alloc: found %x in next_free\n",(unsigned)v);
      from_free = 1;
    }
  }

  /* Create a new segment for this object, if none found above.  Also set
     'index' to the new or old segment being used.  May return SGGC_NO_OBJECT
     if a new segment can't be created. */

  if (v != SGGC_NO_OBJECT)
  { index = SET_VAL_INDEX(v);
  }
  else
  { 
    if (next_segment == maximum_segments)
    { return SGGC_NO_OBJECT;
    }

    sggc_segment[next_segment] = sggc_alloc_zeroed (sizeof **sggc_segment);
    if (sggc_segment[next_segment] == NULL)
    { return SGGC_NO_OBJECT;
    }

    index = next_segment; 
    next_segment += 1;

    set_segment_init (SET_SEGMENT(index));
    sggc_data[index] = NULL;
#   ifdef SGGC_AUX1_SIZE
      sggc_aux1[index] = NULL;
#   endif
#   ifdef SGGC_AUX2_SIZE
      sggc_aux2[index] = NULL;
#   endif
    
    v = SGGC_CPTR_VAL(index,0);
    if (SGGC_DEBUG) 
    { printf("sggc_alloc: created %x in new segment\n", (unsigned)v);
    }
  }

  /* Set up type and flags for the segment, or (if enabled) check that they 
     are already what they should be. */

  struct set_segment *seg = SET_SEGMENT(index);

  if (from_free) /* should be a small segment of the right kind already */
  { if (EXTRA_CHECKS) 
    { if (sggc_type[index] != type) abort();
      if (seg->x.small.big) abort();
      if (seg->x.small.constant) abort();
      if (seg->x.small.kind != kind) abort();
    }
  }
  else
  { sggc_type[index] = type;
    if (sggc_kind_chunks[kind] == 0) /* big segment */
    { seg->x.big.big = 1;
    }
    else /* small segment */
    { seg->x.small.big = 0;
      seg->x.small.constant = 0;
      seg->x.small.kind = kind;
    }
  }

  /* Add the object to the free_or_new set, if not already there.  For
     small segments not from free_or_new, we also put all other
     objects in the segment into free_or_new, and use them as the
     current set of free objects if there were none before. */

  if (sggc_kind_chunks[kind] == 0) /* big segment, can't be from free_or_new */
  {
    set_add (&free_or_new[kind], v);
  }
  else if (!from_free) /* small segment not already in new_or_free */
  { 
    if (next_free[kind]!=end_free[kind]) abort(); /* should be none free now */

    end_free[kind] = set_first (&free_or_new[kind], 0);

    set_add (&free_or_new[kind], v);
  
    set_assign_segment_bits (&free_or_new[kind], v, kind_full[kind]);
    if (SGGC_DEBUG)
    { printf("sggc_alloc: new segment has bits %016llx\n", 
              (unsigned long long) set_segment_bits (&free_or_new[kind], v));
    }

    next_free[kind] = set_next (&free_or_new[kind], v, 0);
  }

  if (EXTRA_CHECKS)
  { if (set_contains (&old_gen1, v)) abort();
    if (set_contains (&old_gen2, v)) abort();
    if (set_contains (&old_to_new, v)) abort();
    if (set_contains (&to_look_at, v)) abort();
  }

  /* Allocate auxiliary information for segment, if not already there. */

# ifdef SGGC_AUX1_SIZE
    if (sggc_aux1[index] == NULL)
    {
#     ifdef SGGC_AUX1_READ_ONLY
        sggc_aux1[index] = kind_aux1_read_only[kind];
        if (SGGC_DEBUG)
        { if (sggc_aux1[index] != NULL)
          { printf("sggc_alloc: used read-only aux1 for %x\n", v);
          }
        }
#     endif
      if (sggc_aux1[index] == NULL)
      { if (kind_aux1_block[kind] == NULL)
        { kind_aux1_block[kind] = sggc_alloc_zeroed
                                   (SGGC_CHUNKS_IN_SMALL_SEGMENT
                                     * SGGC_AUX1_BLOCK_SIZE * SGGC_AUX1_SIZE);
          if (SGGC_DEBUG)
          { printf(
             "sggc_alloc: called alloc_zeroed for aux1 block (kind %d):: %p\n", 
              kind, kind_aux1_block[kind]);
          }
          kind_aux1_block_pos[kind] = 0;
          if (kind_aux1_block[kind] == NULL) 
          { goto fail;
          }
        }
        sggc_aux1[index] = kind_aux1_block[kind] 
                            + kind_aux1_block_pos[kind] * SGGC_AUX1_SIZE;
        if (!seg->x.small.big)
        { seg->x.small.aux1_off = kind_aux1_block_pos[kind];
        }
        if (SGGC_DEBUG)
        { printf(
            "sggc_alloc: aux1 block for %x has pos %d in block for kind %d\n",
             v, kind_aux1_block_pos[kind], kind);
        }
        next_aux_pos (kind, &kind_aux1_block[kind], &kind_aux1_block_pos[kind],
                      SGGC_AUX1_BLOCK_SIZE);
      }
    }
# endif

# ifdef SGGC_AUX2_SIZE
    if (sggc_aux2[index] == NULL)
    {
#     ifdef SGGC_AUX2_READ_ONLY
        sggc_aux2[index] = kind_aux2_read_only[kind];
        if (SGGC_DEBUG)
        { if (sggc_aux2[index] != NULL)
          { printf("sggc_alloc: used read-only aux2 for %x\n", v);
          }
        }
#     endif
      if (sggc_aux2[index] == NULL)
      { if (kind_aux2_block[kind] == NULL)
        { kind_aux2_block[kind] = sggc_alloc_zeroed
                                    (SGGC_CHUNKS_IN_SMALL_SEGMENT
                                      * SGGC_AUX2_BLOCK_SIZE * SGGC_AUX2_SIZE);
          if (SGGC_DEBUG)
          { printf(
             "sggc_alloc: called alloc_zeroed for aux2 block (kind %d):: %p\n", 
              kind, kind_aux2_block[kind]);
          }
          kind_aux2_block_pos[kind] = 0;
          if (kind_aux2_block[kind] == NULL) 
          { goto fail;
          }
        }
        sggc_aux2[index] = kind_aux2_block[kind] 
                            + kind_aux2_block_pos[kind] * SGGC_AUX2_SIZE;
        if (!seg->x.small.big)
        { seg->x.small.aux2_off = kind_aux2_block_pos[kind];
        }
        if (SGGC_DEBUG)
        { printf(
            "sggc_alloc: aux2 block for %x has pos %d in block for kind %d\n",
             v, kind_aux2_block_pos[kind], kind);
        }
        next_aux_pos (kind, &kind_aux2_block[kind], &kind_aux2_block_pos[kind],
                      SGGC_AUX2_BLOCK_SIZE);
      }
    }
# endif

  /* Allocate data for the segment, if not already there.  Update sggc_info. */

  if (sggc_data[index] == NULL)
  {
    if (seg->x.big.big) /* big segment */
    { 
      sggc_nchunks_t nch = sggc_nchunks (type, length);
      seg->x.big.max_chunks = (nch >> SGGC_CHUNK_BITS) == 0 ? nch : 0;

      sggc_data[index] = sggc_alloc_zeroed ((size_t) SGGC_CHUNK_SIZE * nch);
      if (SGGC_DEBUG) 
      { printf (
         "sggc_alloc: called alloc_zeroed for %x (big %d, %d chunks):: %p\n", 
          v, kind, (int)nch, sggc_data[index]);
      }

      sggc_info.big_chunks += nch;
    }
    else /* small segment */
    { 
      sggc_data[index] = sggc_alloc_zeroed ((size_t) SGGC_CHUNK_SIZE 
                                             * SGGC_CHUNKS_IN_SMALL_SEGMENT);
      if (SGGC_DEBUG) 
      { printf (
         "sggc_alloc: called alloc_zeroed for %x (small %d, %d chunks):: %p\n",
          v, kind, (int)SGGC_CHUNKS_IN_SMALL_SEGMENT, sggc_data[index]);
      }
    }

    if (sggc_data[index] == NULL) 
    { goto fail;
    }
  }

  else /* Using existing data area in small segment, so just set to zeros. */
  {
    if (sggc_kind_chunks[kind] == 0) abort();
    memset (SGGC_DATA(v), 0, (size_t) sggc_kind_chunks[kind] * SGGC_CHUNK_SIZE);
  }

  sggc_info.gen0_count += 1;

  return v;

fail: 

  /* Segment obtained, but couldn't allocate aux info or data for segment. */

  return SGGC_NO_OBJECT;
}


/* REGISTER A CONSTANT SEGMENT.  Called with the type and kind of the
   segment (must not be big), the set of bits indicating which objects
   exist in the segment, and the segment's data and optionally aux1
   and aux2 storage.

   Returns a compressed pointer to the first object in the segment (at
   offset zero), or SGGC_NO_OBJECT if a segment couldn't be allocated.

   If called several times before any calls of sggc_alloc, the segments 
   will have indexes 0, 1, 2, etc., which fact may be used when setting
   up their contents if they reference each other. */

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
  set_bits_t bits;
  int i;

  if (sggc_kind_chunks[kind] == 0) abort(); /* big segments are not allowed */
  if (n_objects < 1) abort();          /* must be at least one object */
  if (n_objects > kind_objects[kind]) abort(); /* too many objects */

  bits = 0;
  for (i = 0; i < n_objects; i++)
  { bits |= 1 << (i*sggc_kind_chunks[kind]);
  }

  if (next_segment == maximum_segments)
  { return SGGC_NO_OBJECT;
  }

  sggc_segment[next_segment] = sggc_alloc_zeroed (sizeof **sggc_segment);
  if (sggc_segment[next_segment] == NULL)
  { return SGGC_NO_OBJECT;
  }

  set_index_t index = next_segment; 
  struct set_segment *seg = SET_SEGMENT(index);
  sggc_cptr_t v = SGGC_CPTR_VAL(index,0);

  next_segment += 1;

  set_segment_init (seg);

  seg->x.small.big = 0;
  seg->x.small.constant = 1;
  seg->x.small.kind = kind;

  set_add (&constants, v);
  set_assign_segment_bits (&constants, v, bits);

  sggc_type[index] = type;
  sggc_data[index] = data;
# ifdef SGGC_AUX1_SIZE
    sggc_aux1[index] = aux1;
    seg->x.small.aux1_off = 0;
# endif
# ifdef SGGC_AUX2_SIZE
    sggc_aux2[index] = aux2;
    seg->x.small.aux2_off = 0;
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
  "  unused: %d, old_gen1: %d, old_gen2: %d, old_to_new: %d, to_look_at: %d, const: %d\n",
       set_n_elements(&unused), 
       set_n_elements(&old_gen1), 
       set_n_elements(&old_gen2), 
       set_n_elements(&old_to_new),
       set_n_elements(&to_look_at),
       set_n_elements(&constants));
  printf("  free_or_new");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { printf(" [%d]: %3d ",k,set_n_elements(&free_or_new[k]));
  }
  printf("\n");
  printf("    next_free");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { if (next_free[k] == SGGC_NO_OBJECT)
    { printf(" [%d]: --- ",k);
    }
    else
    { printf(" [%d]: %3llx ",k,(unsigned long long)next_free[k]);
    }
  }
  printf("\n");
  printf("     end_free");
  for (k = 0; k < SGGC_N_KINDS; k++) 
  { if (end_free[k] == SGGC_NO_OBJECT)
    { printf(" [%d]: --- ",k);
    }
    else
    { printf(" [%d]: %3llx ",k,(unsigned long long)end_free[k]);
    }
  }
  printf("\n");
}


/* DO A GARBAGE COLLECTION AT THE SPECIFIED LEVEL.  Levels are 0 for
   collecting only newly allocated objects, 1 for also collecting
   objects that have survived one collection, and 2 for collecting all
   objects.

   This procedure is called automatically when sggc_alloc would
   otherwise fail, but should also be called by the application
   according to its heuristics for when this would be desirable. */

void sggc_collect (int level)
{ 
  sggc_cptr_t v;
  int k;

  if (SGGC_DEBUG) printf("sggc_collect: level %d\n",level);
  if (SGGC_DEBUG) collect_debug();

  collect_level = level;

  if (set_first(&to_look_at, 0) != SET_NO_VALUE) abort();

  /* Put objects in the old generations being collected in the free_or_new set.

     This could be greatly sped up with some special facility in the set
     module that would do it a segment at a time. */

  if (level == 2)
  { for (v = set_first(&old_gen2, 0); 
         v != SET_NO_VALUE; 
         v = set_next(&old_gen2,v,0))
    { set_add (&free_or_new[SGGC_KIND(v)], v);
      if (SGGC_DEBUG) 
      { printf("sggc_collect: put %x from old_gen2 in free\n",(unsigned)v);
      }
    }
  }

  if (level >= 1)
  { for (v = set_first(&old_gen1, 0);
         v != SET_NO_VALUE; 
         v = set_next(&old_gen1,v,0))
    { set_add (&free_or_new[SGGC_KIND(v)], v);
      if (SGGC_DEBUG) 
      { printf("sggc_collect: put %x from old_gen1 in free\n",(unsigned)v);
      }
    }
  }

  /* Handle old-to-new references.  Done in cooperation with
     sggc_look_at, using the global variables collect_level (the level
     of collection being done) and old_to_new_check (which contains
     the generation of the referring object, always 1 or 2, except it
     is cleared to 0 to indicate that further special processing is
     unnecessary.  (That may also mean that the old-to-new entry is 
     still needed). */

  v = set_first(&old_to_new, 0);

  while (v != SET_NO_VALUE)
  { int remove = 0;
    if (SGGC_DEBUG) 
    { printf ("sggc_collect: old->new for %x (gen%d)\n", (unsigned)v,
        set_contains(&old_gen2,v) ? 2 : set_contains(&old_gen1,v) ? 1 : 0);
    }
    if (set_contains (&old_gen2, v)) /* v is in old generation 2 */
    { old_to_new_check = 2;
    }
    else /* v is in old generation 1 */
    { if (level == 0)
      { old_to_new_check = 0;
        remove = 1;
      }
      else
      { old_to_new_check = 1;
      }
    }
    sggc_find_object_ptrs (v);
    remove |= old_to_new_check;
    if (SGGC_DEBUG) 
    { if (remove) 
      { printf("sggc_collect: old->new for %x no longer needed\n",(unsigned)v);
      }
      else 
      { printf("sggc_collect: old->new for %x still needed\n",(unsigned)v);
      }
    }
    v = set_next (&old_to_new, v, remove);
  }

  old_to_new_check = 0;  /* no special old-to-new processing in sggc_look_at */

  /* Get the application to take root pointers out of the free_or_new set,
     and put them in the to_look_at set. */

  sggc_find_root_ptrs();

  /* Keep looking at objects in the to_look_at set, putting them in
     the correct old generation, and getting the application to find
     any pointers they contain (which may add to the to_look_at set),
     until there are no more in the set. */

# ifdef SGGC_AFTER_MARKING
  int rep = 0;
# endif

  do
  { 
    while ((v = set_first (&to_look_at, 1)) != SGGC_NO_OBJECT)
    {
      if (SGGC_DEBUG) printf("sggc_collect: looking at %x\n",(unsigned)v);

      if (level == 0)
      { /* must be in generation 0 */
        set_add (&old_gen1, v);
        if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen1\n",(unsigned)v);
      }
      else if (level == 1)
      { if (set_remove (&old_gen1, v))
        { set_add (&old_gen2, v);
          if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen2\n",(unsigned)v);
        }
        else /* must be in generation 0 */
        { set_add (&old_gen1, v);
          if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen1\n",(unsigned)v);
        }
      }
      else /* level == 2 */
      { if (set_remove (&old_gen1, v))
        { set_add (&old_gen2, v);
          if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen2\n",(unsigned)v);
        }
        else if (!set_contains (&old_gen2, v)) /* must be in generation 0 */
        { set_add (&old_gen1, v);
          if (SGGC_DEBUG) printf("sggc_collect: %x now old_gen1\n",(unsigned)v);
        }
      }  
  
      sggc_find_object_ptrs (v);
    }

#   ifdef SGGC_AFTER_MARKING
    sggc_after_marking (level, rep++);
#   endif

  } while (set_first (&to_look_at, 0) != SGGC_NO_OBJECT);

  /* Remove objects that are still in the free_or_new set from the old 
     generations that were collected.  Also remove them from the old-to-new
     set.

     This is done by scanning the old generation sets, not the free sets,
     since this is easier, and likely faster, if lots of objects were 
     allocated but not used for long, and hence are in the free sets.

     This could be greatly sped up with some special facility in the set
     module that would do it a segment at a time. */

  if (level == 2)
  { v = set_first (&old_gen2, 0); 
    while (v != SET_NO_VALUE)
    { int remove = set_chain_contains (SET_UNUSED_FREE_NEW, v);
      if (SGGC_DEBUG && remove) 
      { printf("sggc_collect: %x in old_gen2 now free\n",(unsigned)v);
      }
      if (remove)
      { set_remove (&old_to_new, v);
      }
      v = set_next (&old_gen2, v, remove);
    }
  }

  if (level >= 1)
  { v = set_first (&old_gen1, 0); 
    while (v != SET_NO_VALUE)
    { int remove = set_chain_contains (SET_UNUSED_FREE_NEW, v);
      if (SGGC_DEBUG && remove) 
      { printf("sggc_collect: %x in old_gen1 now free\n",(unsigned)v);
      }
      if (remove)
      { set_remove (&old_to_new, v);
      }
      v = set_next (&old_gen1, v, remove);
    }
  }

  /* Move big segments to the 'unused' set, while freeing their data
     storage.  Auxiliary information is not freed (and should not be
     read-only).

     Note that all big kinds are equal to their types, so we stop the
     loop at SGGC_N_TYPES. */

  for (k = 0; k < SGGC_N_TYPES; k++)
  { if (sggc_kind_chunks[k] == 0)
    { while ((v = set_first (&free_or_new[k], 1)) != SGGC_NO_OBJECT)
      { set_index_t index = SET_VAL_INDEX(v);
        if (SGGC_DEBUG) 
        { printf ("sggc_collect: calling free for data for %x:: %p\n", 
                   v, SGGC_DATA(v));
        }
        sggc_free (sggc_data[index]);
        sggc_data [index] = NULL;
        if (SGGC_DEBUG) 
        { printf("sggc_collect: putting %x in unused\n",(unsigned)v);
        }
        set_add (&unused, v); /* allowed because v was removed with set_first */
      }
    }
  }

  /* Set up new next_free and end_free pointers, to use all of free_or_new. */

  for (k = 0; k < SGGC_N_KINDS; k++)
  { if (sggc_kind_chunks[k] != 0)
    { next_free[k] = set_first (&free_or_new[k], 0);
      end_free[k] = SGGC_NO_OBJECT;
    }
  }

  /* Code below can be enabled to wipe out data in free objects, storing
     up to two copies of SGGC_NO_OBJECT at the front, to help debugging. */

  if (EXTRA_CHECKS)
  { for (k = 0; k < SGGC_N_KINDS; k++)
    { if (sggc_kind_chunks[k] != 0)
      { sggc_cptr_t p;
        for (p = set_first (&free_or_new[k], 0); 
             p != SGGC_NO_OBJECT;
             p = set_next (&free_or_new[k], p, 0))
        { if (SGGC_DATA(p) != NULL)
          { sggc_cptr_t *d =  (sggc_cptr_t *) SGGC_DATA(p);
            if (sizeof (sggc_cptr_t) < SGGC_CHUNK_SIZE) *d++ = SGGC_NO_OBJECT;
            if (2 * sizeof (sggc_cptr_t) < SGGC_CHUNK_SIZE) *d = SGGC_NO_OBJECT;
          }
        }
      }
    }
  }

  sggc_info.gen0_count = 0;
  sggc_info.gen1_count = set_n_elements(&old_gen1);
  sggc_info.gen2_count = set_n_elements(&old_gen2);
  sggc_info.big_chunks = 0;

  collect_level = -1;

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
   there, as well as sometimes marking the objects it points to.  In
   this context, sggc_look_at will return 0 to indicate looking at
   further references is unnecessary, since a reference requiring that
   the object remain in the old-to-new set has been seen. */

int sggc_look_at (sggc_cptr_t ptr)
{
  if (SGGC_DEBUG) 
  { printf ("sggc_look_at: %x %d\n", (unsigned)ptr, old_to_new_check);
  }

  if (ptr != SGGC_NO_OBJECT)
  { if (old_to_new_check != 0)
    { if (collect_level == 0)
      { if (!set_chain_contains (SET_OLD_GEN2_CONST, ptr))
        { old_to_new_check = 0;
        }
      }
      else if (collect_level == 1 && old_to_new_check == 2)
      { if (!set_chain_contains (SET_OLD_GEN2_CONST, ptr) 
              && !set_contains (&old_gen1, ptr))
        { old_to_new_check = 0;
        }
      }
      else /* collect_level==2 || collect_level == 1 && old_to_new_check == 1 */
      { if (!set_chain_contains (SET_OLD_GEN2_CONST, ptr) 
              && !set_contains (&old_gen1, ptr))
        { old_to_new_check = 0;
          return 0;
        }
        return 1;
      }
    }
    if (set_remove (&free_or_new[SGGC_KIND(ptr)], ptr))
    { set_add (&to_look_at, ptr);
      if (SGGC_DEBUG) printf("sggc_look_at: will look at %x\n",(unsigned)ptr);
    }
  }

  return 1;
}


/* RECORD AN OLD-TO-NEW REFERENCE IF NECESSARY. */

void sggc_old_to_new_check (sggc_cptr_t from_ptr, sggc_cptr_t to_ptr)
{
  /* If from_ptr is youngest generation, no need to check anything else. */

  if (set_chain_contains (SET_UNUSED_FREE_NEW, from_ptr))
  { return;
  }

  /* Can quit now if from_ptr is already in the old-to-new set. */

  if (set_contains (&old_to_new, from_ptr))
  { return;
  }

  if (set_contains (&old_gen2, from_ptr))
  { 
    /* If from_ptr is in old generation 2, only others in old generation 2
       and constants can be referenced without using old-to-new. */

    if (set_chain_contains (SET_OLD_GEN2_CONST, to_ptr))
    { return;
    }
  }
  else
  { 
    /* If from_ptr is in old generation 1, only references to newly 
       allocated objects require using old-to-new. */

    if (!set_chain_contains (SET_UNUSED_FREE_NEW, to_ptr))
    { return;
    }
  }

  /* If we get here, we need to record the existence of an old-to-new
     reference in from_ptr. */

  set_add (&old_to_new, from_ptr);
}


/* CHECK WHETHER AN OBJECT IS IN THE YOUNGEST GENERATION.  */

int sggc_youngest_generation (sggc_cptr_t from_ptr)
{
  return set_chain_contains (SET_UNUSED_FREE_NEW, from_ptr);
}


/* CHECK WHETHER AN OBJECT IS IN THE OLDEST GENERATION, OR IS A CONSTANT. */

int sggc_oldest_generation (sggc_cptr_t to_ptr)
{
  return set_chain_contains (SET_OLD_GEN2_CONST, to_ptr);
}


/* TEST WHETHER AN OBJECT IS NOT (YET) MARKED AS IN USE.  May only be 
   called during a garbage collection. */

int sggc_not_marked (sggc_cptr_t cptr)
{
  if (collect_level < 0) abort();  /* not in middle of a garbage collection */

  return set_chain_contains (SET_UNUSED_FREE_NEW, cptr);
}


/* TEST WHETHER AN OBJECT IS A CONSTANT. */

int sggc_is_constant (sggc_cptr_t cptr)
{
  struct set_segment *set = SET_SEGMENT(SET_VAL_INDEX(cptr));

  return !set->x.small.big && set->x.small.constant;
}
