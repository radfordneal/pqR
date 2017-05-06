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


#ifndef SET_USE_BUILTINS
#define SET_USE_BUILTINS (defined(__GNUC__) || defined(__clang__))
#endif


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


/* SPECIAL NO VALUE INDICATOR.  Uses either all 0s or all 1s in the
   index and offset.  This value must not be used for an actual
   element in a set. */

#ifdef SET_NO_VALUE_ZERO
#define SET_NO_VALUE ((set_value_t)0)
#else
#define SET_NO_VALUE (~(set_value_t)0)
#endif


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


/* FUNCTIONS USED BY THE APPLICATION.

   If SET_STATIC is defined as non-zero, all API procedures are
   static.  This makes sense only if this .c file is included at
   compile time in any module using it, rather than being compiled
   separately and then linked. */

#if SET_STATIC
#  define SET_PROC_CLASS static
#else
#  define SET_PROC_CLASS
#endif

SET_PROC_CLASS void set_init (struct set *set, int chain);
SET_PROC_CLASS void set_segment_init (struct set_segment *seg);
SET_PROC_CLASS set_value_t set_first (struct set *set, int remove);
SET_PROC_CLASS set_value_t set_next (struct set *set, set_value_t val, 
                                     int remove);
SET_PROC_CLASS set_bits_t set_first_bits (struct set *set);
SET_PROC_CLASS void set_move_first (struct set *src, struct set *dst);
SET_PROC_CLASS void set_move_next (struct set *src, set_value_t val,
                                   struct set *dst);
SET_PROC_CLASS void set_add_segment (struct set *set, set_value_t val, 
                                     int chain);
SET_PROC_CLASS void set_remove_segment (struct set *set, set_value_t val,
                                        int chain);


/**** Functions below are defined here as "static inline" for speed. ****/

/* First, do anything that needs doing at this point from set-app.h. */

#ifdef SET_DO_BEFORE_INLINE
SET_DO_BEFORE_INLINE
#endif


/* RETURN THE CHAIN USED BY A SET. */

static inline int set_chain (struct set *set)
{ 
  return set->chain;
}


/* CHECK WHETHER A VALUE IS AN ELEMENT OF ANY SET USING A GIVEN CHAIN.

   This is implemented by just looking at the right bit in the bits for
   the chain. */

static inline int set_chain_contains (int chain, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  return (seg->bits[chain] >> offset) & 1;
}


/* CHECK WHETHER ANY SET USING A GIVEN CHAIN CONATAINS ANY ELEMENT IN A SEGMENT.

   This is implemented by just checking whether any bits for that chain in the
   segment. */

static inline int set_chain_contains_any_in_segment(int chain, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  return seg->bits[chain] != 0;
}


/* CHECK WHETHER A SET, OR ANY SET USING THE SAME CHAIN, CONTAINS A VALUE. */

static inline int set_contains (struct set *set, set_value_t val)
{
  return set_chain_contains (set->chain, val);
}


/* RETURN THE NUMBER OF ELEMENTS IN A SET. */

static inline set_value_t set_n_elements (struct set *set)
{
  return set->n_elements;
}


/* FIND THE NUMBER OF 1 BITS IN A SET OF BITS.  

   Fast for gcc and clang, using their builtin functions. */

static inline int set_bit_count (set_bits_t b)
{ 
# if SET_USE_BUILTINS
    return sizeof b <= sizeof (unsigned) ? __builtin_popcount(b) 
         : sizeof b <= sizeof (unsigned long) ? __builtin_popcountl(b) 
         : __builtin_popcountll(b);
# else
    int cnt;
    cnt = 0;
    while (b != 0)
    { cnt += (b & 1);
      b >>= 1;
    }
    return cnt;
# endif

}


/* FIND POSITION OF LOWEST-ORDER BIT.  The position returned is from 0 up.
   The argument must not be zero.

   Fast for gcc and clang, using their builtin functions. */

static inline int set_first_bit_pos (set_bits_t b)
{ 
# if SET_USE_BUILTINS
    return sizeof b <= sizeof (unsigned) ? __builtin_ctz(b) 
         : sizeof b <= sizeof (unsigned long) ? __builtin_ctzl(b) 
         : __builtin_ctzll(b);
# else
    int pos;
    pos = 0;
    while ((b & 1) == 0)
    { pos += 1;
      b >>= 1;
    }
    return pos;
# endif

}


/* RETURN BITS INDICATING MEMBERSHIP FOR THE SEGMENT CONTAINING AN ELEMENT. */

static inline set_bits_t set_chain_segment_bits (int chain, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  return seg->bits[chain];
}


/* ASSIGN BITS INDICATING MEMBERSHIP FOR THE SEGMENT CONTAINING AN ELEMENT. */

static inline void set_assign_segment_bits (struct set *set, 
                                            set_value_t val, set_bits_t b)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  set->n_elements -= set_bit_count(seg->bits[set->chain]);
  seg->bits[set->chain] = b;
  set->n_elements += set_bit_count(b);
}


/* FIND THE NEXT ELEMENT IN A CHAIN.

   If the linked list has to be followed to a later segment, any
   unused segments that are skipped are deleted from the list, to save
   time in any future searches. */

static inline set_value_t set_chain_next (int chain, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  /* Get the bits after the one for the element we are looking after. */

  set_bits_t b = seg->bits[chain] >> offset;
  offset += 1;
  b >>= 1;

  /* If no bits are set after the one we are starting at, go to the
     next segment, removing ones that are unused.  We may discover
     that there is no next element, and return SET_NO_VALUE. */

  if (b == 0)
  { set_index_t nindex;
    struct set_segment *nseg;

    for (;;)
    { 
      nindex = seg->next[chain];
      if (nindex == SET_END_OF_CHAIN) 
      { return SET_NO_VALUE;
      }

      nseg = SET_SEGMENT(nindex);

      b = nseg->bits[chain];
      if (b != 0) 
      { break;
      }

      seg->next[chain] = nseg->next[chain];
      nseg->next[chain] = SET_NOT_IN_CHAIN;
    }

    index = nindex;
    offset = 0;
  }

  offset += set_first_bit_pos(b);

  return SET_VAL(index,offset);
}


/* FIND THE NEXT ELEMENT IN A SET THAT IS IN A DIFFERENT SEGMENT. */

static inline set_value_t set_chain_next_segment (int chain, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  set_index_t nindex;
  struct set_segment *nseg;

  /* Go to the next segment, removing any segments that are unused. If there
     is no next segment, return SET_NO_VALUE. */

  for (;;)
  { 
    nindex = seg->next[chain];
    if (nindex == SET_END_OF_CHAIN) 
    { return SET_NO_VALUE;
    }

    nseg = SET_SEGMENT(nindex);

    set_bits_t b = nseg->bits[chain];
    if (b != 0) 
    { return SET_VAL (nindex, set_first_bit_pos(b));
    }

    seg->next[chain] = nseg->next[chain];
    nseg->next[chain] = SET_NOT_IN_CHAIN;
  }
}


/* ADD A VALUE TO A SET.  The value must not be in a segment with
   members in a different set using the same chain (or previously,
   if the segment may still be in the other set's chain).

   This is implemented by setting the right bit in the bits for the
   set's chain, within the segment structure for this value's index.
   This segment is then added to the linked list of segments for this
   set if it is not there already, and the element count is updated. */

static inline int set_add (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  set_bits_t b = seg->bits[set->chain];
  set_bits_t t = (set_bits_t)1 << SET_VAL_OFFSET(val);

  if (b & t)
  { return 1;
  }

  if (seg->next[set->chain] == SET_NOT_IN_CHAIN)
  { seg->next[set->chain] = set->first;
    set->first = index;
  }

  seg->bits[set->chain] |= t;
  set->n_elements += 1;

  return 0;
}


/* REMOVE A VALUE FROM A SET.  The value must not be in any other set using
   the same chain.

   This is implemented by clearing the right bit in the bits for the set's 
   chain, within the segment structure for this value's index, and updating
   the count of elements in the set. */

static inline int set_remove (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  set_bits_t b = seg->bits[set->chain];
  set_bits_t t = (set_bits_t)1 << SET_VAL_OFFSET(val);

  if ((b & t) == 0)
  { return 0;
  }

  seg->bits[set->chain] &= ~t;
  set->n_elements -= 1;

  return 1;
}
