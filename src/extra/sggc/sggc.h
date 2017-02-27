/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Segmented generational garbage collection - header file

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


/* CONTROL EXTERN DECLARATIONS FOR GLOBAL VARIABLES.  SGGC_EXTERN will
   be defined as nothing in sggc.c, where globals will actually be
   defined, but will be "extern" elsewhere. */

#ifndef SGGC_EXTERN
#define SGGC_EXTERN extern
#endif


#include "set-app.h"

#ifdef SGGC_USE_MEMSET
#include <string.h>
#endif


/* COMPRESSED POINTER (INDEX, OFFSET) TYPE, AND NO OBJECT CONSTANT. */

typedef set_index_t sggc_index_t; /* Type of segment index */
typedef set_value_t sggc_cptr_t;  /* Type of compressed pointer (index,offset)*/

#define SGGC_CPTR_VAL(i,o) SET_VAL((i),(o))
#define SGGC_SEGMENT_INDEX(p) SET_VAL_INDEX(p)
#define SGGC_SEGMENT_OFFSET(p) SET_VAL_OFFSET(p)

#define SGGC_NO_OBJECT SET_NO_VALUE   /* Special "no object" pointer */


/* ARRAYS OF POINTERS TO SPACE FOR MAIN AND AUXILIARY DATA .  The sggc_data
   and (perhaps) sggc_aux[0], sggc_aux[1], etc. arrays have length equal to
   the maximum number of segments, and are either allocated at initialization,
   or statically allocated, if the maximum number of segments is specified
   at compile time with SGGC_MAX_SEGMENTS.
   
   Pointers in these arrays are set when segments are needed, by the 
   application, since the application may do tricks like making some of
   of these be shared between segments, if it knows this is possible.

   Main data is for one object in a 'big' segment, and for several in a
   'small' segment, having total number of chunks no more than given by
   SGGC_CHUNKS_IN_SMALL_SEGMENT. */

#ifdef SGGC_USE_OFFSET_POINTERS
typedef uintptr_t sggc_dptr;       /* So out-of-bounds arithmetic well-defined*/
#else
typedef char *sggc_dptr;           /* Ordinary pointer arithmetic */
#endif

#ifdef SGGC_MAX_SEGMENTS

SGGC_EXTERN sggc_dptr sggc_data[SGGC_MAX_SEGMENTS]; /* Pointers to arrays of 
                                                       blocks for objs in seg */

#ifdef SGGC_AUX1_SIZE
SGGC_EXTERN sggc_dptr sggc_aux1[SGGC_MAX_SEGMENTS]; /* Pointers to aux1 data */
#endif

#ifdef SGGC_AUX2_SIZE
SGGC_EXTERN sggc_dptr sggc_aux2[SGGC_MAX_SEGMENTS]; /* Pointers to aux2 data */
#endif

#else  /* max number of segments determined at run time */

SGGC_EXTERN sggc_dptr *sggc_data;  /* Pointer to array of pointers to arrays of 
                                      data blocks for objects within segments */

#ifdef SGGC_AUX1_SIZE
SGGC_EXTERN sggc_dptr *sggc_aux1;  /* Pointer to array of pointers to arrays of 
                                      auxiliary info for objects in segments */
#endif

#ifdef SGGC_AUX2_SIZE
SGGC_EXTERN sggc_dptr *sggc_aux2;  /* Pointer to array of pointers to arrays of 
                                      auxiliary info for objects in segments */
#endif

#endif

#define SGGC_CHUNKS_IN_SMALL_SEGMENT (1 << SET_OFFSET_BITS)


/* INLINE FUNCTION TO GET DATA POINTER FOR AN OBJECT, and similarly
   for auxiliary information (if present). */

#if SGGC_USE_OFFSET_POINTERS

#ifndef SGGC_OFFSET_CAST
#define SGGC_OFFSET_CAST (uintptr_t) /* either (uint32_t) or (uintptr_t) */
#endif

static inline char *SGGC_DATA (sggc_cptr_t cptr)
{ return ((char *) (sggc_data[SET_VAL_INDEX(cptr)] 
                     + SGGC_CHUNK_SIZE * SGGC_OFFSET_CAST cptr));
}

#ifdef SGGC_AUX1_SIZE
static inline char *SGGC_AUX1 (sggc_cptr_t cptr)
{ return ((char *) (sggc_aux1[SET_VAL_INDEX(cptr)] 
                     + SGGC_AUX1_SIZE * SGGC_OFFSET_CAST cptr));
}
#endif

#ifdef SGGC_AUX2_SIZE
static inline char *SGGC_AUX2 (sggc_cptr_t cptr)
{ return ((char *) (sggc_aux2[SET_VAL_INDEX(cptr)] 
                     + SGGC_AUX2_SIZE * SGGC_OFFSET_CAST cptr));
}
#endif

#else /* not using offset pointers */

#ifndef SGGC_OFFSET_CAST
#define SGGC_OFFSET_CAST (uint32_t) /* may be (uint32_t), (uintptr_t), (int) */
#endif

static inline char *SGGC_DATA (sggc_cptr_t cptr)
{ return sggc_data[SET_VAL_INDEX(cptr)] 
          + SGGC_CHUNK_SIZE * SGGC_OFFSET_CAST SET_VAL_OFFSET(cptr);
}

#ifdef SGGC_AUX1_SIZE
static inline char *SGGC_AUX1 (sggc_cptr_t cptr)
{ return sggc_aux1[SET_VAL_INDEX(cptr)] 
           + SGGC_AUX1_SIZE * SGGC_OFFSET_CAST SET_VAL_OFFSET(cptr);
}
#endif

#ifdef SGGC_AUX2_SIZE
static inline char *SGGC_AUX2 (sggc_cptr_t cptr)
{ return sggc_aux2[SET_VAL_INDEX(cptr)] 
           + SGGC_AUX2_SIZE * SGGC_OFFSET_CAST SET_VAL_OFFSET(cptr);
}
#endif

#endif


/* TYPES AND KINDS OF SEGMENTS.  Types and kinds must fit in 8 bits,
   with the kind being equal to the type if it is for a "big" segment.
   The kind of a small segment is recorded in the segment description.
   The array of types for segments is allocated at initialization, or
   statically if SGGC_MAX_SEGMENTS is defined. */

typedef unsigned char sggc_type_t;
typedef unsigned char sggc_kind_t;

#ifdef SGGC_MAX_SEGMENTS
SGGC_EXTERN sggc_type_t sggc_type[SGGC_MAX_SEGMENTS];  /* Types for segments */
#else
SGGC_EXTERN sggc_type_t *sggc_type;  /* Types of objects in each segment */
#endif

/* Macro to access type of object, using the index of its segment. */

#define SGGC_TYPE(cptr) (sggc_type[SET_VAL_INDEX(cptr)])

/* Inline function to find the kind of the segment containing an object. */

static inline sggc_kind_t SGGC_KIND (sggc_cptr_t cptr) 
{ return SET_SEGMENT(SET_VAL_INDEX(cptr))->x.big.big ? SGGC_TYPE(cptr)
          : SET_SEGMENT(SET_VAL_INDEX(cptr))->x.small.kind;
}

/* Numbers of chunks for the various kinds (zero for kinds for big segments). */

SGGC_EXTERN const int sggc_kind_chunks[SGGC_N_KINDS];


/* STRUCTURE HOLDING INFORMATION ON CURRENT SPACE USAGE.  This structure
   is kept up-to-date after calls to sggc_alloc and sggc_collect. */

SGGC_EXTERN struct sggc_info
{ 
  unsigned gen0_count;       /* Number of newly-allocated objects */
  unsigned gen1_count;       /* Number of objects in old generation 1 */
  unsigned gen2_count;       /* Number of objects in old generation 2.
                                Does not include constants */

  sggc_nchunks_t big_chunks; /* # of chunks in newly-allocated big objects */

} sggc_info;


/* FUNCTIONS PROVIDED BY THE APPLICATION.  Prototypes are declared here only
   if they haven't been defined as macros. */

#ifndef sggc_kind
sggc_kind_t sggc_kind (sggc_type_t type, sggc_length_t length);
#endif

#ifndef sggc_nchunks
sggc_nchunks_t sggc_nchunks (sggc_type_t type, sggc_length_t length);
#endif

#ifndef sggc_find_root_ptrs
void sggc_find_root_ptrs (void);
#endif

#ifndef sggc_find_object_ptrs
void sggc_find_object_ptrs (sggc_cptr_t cptr);
#endif

#ifdef SGGC_AUX1_READ_ONLY
#ifndef sggc_aux1_read_only
char *sggc_aux1_read_only (sggc_kind_t kind);
#endif
#endif

#ifdef SGGC_AUX1_READ_ONLY
#ifndef sggc_aux2_read_only
char *sggc_aux2_read_only (sggc_kind_t kind);
#endif
#endif

#ifdef SGGC_AFTER_MARKING
#ifndef sggc_after_marking
void sggc_after_marking (int level, int rep);
#endif
#endif


/* FUNCTIONS USED BY THE APPLICATION. */

int sggc_init (int max_segments);
sggc_cptr_t sggc_alloc (sggc_type_t type, sggc_length_t length);
#ifdef SGGC_KIND_TYPES
sggc_cptr_t sggc_alloc_small_kind (sggc_kind_t kind);
#endif
void sggc_collect (int level);
void sggc_look_at (sggc_cptr_t cptr);

sggc_cptr_t sggc_constant (sggc_type_t type, sggc_kind_t kind, int n_objects,
                           char *data
#ifdef SGGC_AUX1_SIZE
                         , char *aux1
#endif
#ifdef SGGC_AUX2_SIZE
                         , char *aux2
#endif
);


/**** Functions below are defined here as "static inline" for speed. ****/


/* CHECK WHETHER AN OBJECT IS IN THE YOUNGEST GENERATION.  */

static inline int sggc_youngest_generation (sggc_cptr_t from_ptr)
{
  return set_chain_contains (SET_UNUSED_FREE_NEW, from_ptr);
}


/* CHECK WHETHER AN OBJECT IS IN THE OLDEST GENERATION, OR IS A CONSTANT. */

static inline int sggc_oldest_generation (sggc_cptr_t to_ptr)
{
  return set_chain_contains (SET_OLD_GEN2_CONST, to_ptr);
}


/* TEST WHETHER AN OBJECT IS NOT (YET) MARKED AS IN USE.  May only be 
   called during a garbage collection. */

static inline int sggc_not_marked (sggc_cptr_t cptr)
{
  return set_chain_contains (SET_UNUSED_FREE_NEW, cptr);
}


/* TEST WHETHER AN OBJECT IS A CONSTANT. */

static inline int sggc_is_constant (sggc_cptr_t cptr)
{
  struct set_segment *set = SET_SEGMENT(SET_VAL_INDEX(cptr));

  return !set->x.small.big && set->x.small.constant;
}


/* QUICKLY ALLOCATE AN OBJECT WITH GIVEN KIND, WHICH MUST BE FOR SMALL SEGMENT. 
   Returns SGGC_NO_OBJECT if can't allocate quickly in an existing segment. */

static inline sggc_cptr_t sggc_alloc_small_kind_quickly (sggc_kind_t kind)
{
  extern sggc_cptr_t sggc_next_free_val[SGGC_N_KINDS];
  extern set_bits_t sggc_next_free_bits[SGGC_N_KINDS];
  extern int sggc_next_segment_not_free[SGGC_N_KINDS];

  sggc_cptr_t nfv = sggc_next_free_val[kind];

  if (nfv == SGGC_NO_OBJECT)
  { return SGGC_NO_OBJECT;
  }

  set_bits_t nfb = sggc_next_free_bits[kind];

  nfb >>= 1;
  if (nfb != 0)
  { int o = set_first_bit_pos(nfb);
    nfb >>= o;
    sggc_next_free_val[kind] = nfv + o + 1;
  }
  else if (!sggc_next_segment_not_free[kind])
  { sggc_cptr_t n = set_chain_next_segment (SET_UNUSED_FREE_NEW, nfv);
    sggc_next_free_val[kind] = n;
    if (n != SGGC_NO_OBJECT)
    { nfb = set_chain_segment_bits (SET_UNUSED_FREE_NEW, n) >> SET_VAL_OFFSET(n);
    }
  }
  else
  { sggc_next_free_val[kind] = SGGC_NO_OBJECT;
  }

  sggc_next_free_bits[kind] = nfb;

  sggc_nchunks_t nch = sggc_kind_chunks[kind]; /* number of chunks for object */
  uint64_t *p = (uint64_t *) SGGC_DATA(nfv);   /* should be aligned properly  */

#ifdef SGGC_USE_MEMSET
  memset (p, 0, (size_t) SGGC_CHUNK_SIZE * nch);
#else
  do 
  { int i;
    for (i = 0; i <SGGC_CHUNK_SIZE; i += 8) *p++ = 0;
    nch -= 1;
  } while (nch > 0);
#endif

  sggc_info.gen0_count += 1;

  return nfv;
}


/* RECORD AN OLD-TO-NEW REFERENCE IF NECESSARY. */

static inline void sggc_old_to_new_check (sggc_cptr_t from_ptr,
                                          sggc_cptr_t to_ptr)
{
  /* If from_ptr is youngest generation, no need to check anything else. */

  if (set_chain_contains (SET_UNUSED_FREE_NEW, from_ptr))
  { return;
  }

  /* Can quit now if from_ptr is already in the old-to-new set (which is
     the only one using the SET_OLD_TO_NEW chain). */

  if (set_chain_contains (SET_OLD_TO_NEW, from_ptr))
  { return;
  }

  /* Note:  from_ptr shouldn't be constant, so below can look in whole chain. */

  if (set_chain_contains (SET_OLD_GEN2_CONST, from_ptr))
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

  extern struct set sggc_old_to_new_set;
  set_add (&sggc_old_to_new_set, from_ptr);
}
