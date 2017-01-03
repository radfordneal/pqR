/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Facility for maintaining sets of objects - header file

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


/*   See set-doc for general information on this facility, and for the
 *   documentation on functions that are part of the application interface.
 */


#include <stdint.h>


/* TYPES FOR (INDEX, OFFSET) PAIRS.  The set_value_t type is for the pair,
   and is designed to be 32 bits.  The set_index_t type must be signed,
   and should also be 32 bits, to limit space used.  The set_offset_t type
   is not used in data structures, and can be int, as that is big enough
   and presumably most efficient. */

typedef int set_offset_t;
typedef int32_t set_index_t;
typedef uint32_t set_value_t;


/* MACROS TO CREATE / ACCESS (INDEX, OFFSET) PAIRS. */

#define SET_VAL(index,offset) \
  (((set_value_t)(index) << SET_OFFSET_BITS) | (offset))
#define SET_VAL_INDEX(val) \
  ((val) >> SET_OFFSET_BITS)
#define SET_VAL_OFFSET(val) \
  ((val) & (((set_value_t)1 << SET_OFFSET_BITS) - 1))


/* TYPE OF THE BIT VECTOR RECORDING SET MEMBERSHIP IN A SEGMENT.  Must
   be unsigned. */

#if SET_OFFSET_BITS == 3
  typedef uint8_t set_bits_t;
#elif SET_OFFSET_BITS == 4
  typedef uint16_t set_bits_t;
#elif SET_OFFSET_BITS == 5
  typedef uint32_t set_bits_t;
#elif SET_OFFSET_BITS == 6
  typedef uint64_t set_bits_t;
#endif


/* SPECIAL NULL VALUE.  Uses all 1s in the index and offset.  This value must 
   not be used for an actual object. */

#define SET_NO_VALUE (~(set_value_t)0)


/* SPECIAL INDEXES USED IN CHAINS.  These are not used in real (index, offset)
   pairs, in which the index is unsigned. */

#define SET_NOT_IN_CHAIN -1
#define SET_END_OF_CHAIN -2


/* DATA FOR A SEGMENT.  Records which objects in the segment are present in
   the sets using each chain, and the links to the next segments for each
   chain.  Also may include extra information used by the application, which
   may take advantage of otherwise wasted padding, and may also have the effect
   of adjusting the size of the structure to a power of two (advantageous for
   speed of indexing, and possibly cache performance). */

struct set_segment
{ set_bits_t bits[SET_CHAINS];  /* Bits indicating membership in sets */
  set_index_t next[SET_CHAINS]; /* Next / SET_NOT_IN_CHAIN / SET_END_OF_CHAIN */
# ifdef SET_EXTRA_INFO
  SET_EXTRA_INFO                /* Extra info of use to the application, or  */
# endif                         /*  padding to make struct size a power of 2 */
};


/* DESCRIPTION OF A SET.  The chain used must not be used by any other set,
   unless the two sets never contain elements from the same segment. */

struct set
{ int chain;                    /* Number of chain used for this set */
  set_index_t first;            /* First segment, or SET_END_OF_CHAIN */
  set_value_t n_elements;       /* Number of elements in set */
};


/* FUNCTIONS USED BY THE APPLICATION. */

void set_init (struct set *set, int chain);
void set_segment_init (struct set_segment *seg);
int set_chain (struct set *set);
int set_contains (struct set *set, set_value_t val);
int set_chain_contains (int chain, set_value_t val);
int set_add (struct set *set, set_value_t val);
int set_remove (struct set *set, set_value_t val);
set_value_t set_first (struct set *set, int remove);
set_value_t set_next (struct set *set, set_value_t val, int remove);
set_bits_t set_first_bits (struct set *set);
set_bits_t set_segment_bits (struct set *set, set_value_t val);
void set_assign_segment_bits (struct set *set, set_value_t val, set_bits_t b);
void set_move_first (struct set *src, struct set *dst);
void set_move_next (struct set *src, set_value_t val, struct set *dst);
set_value_t set_n_elements (struct set *set);
