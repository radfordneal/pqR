/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Facility for maintaining sets of objects - function definitions

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


#include <stdlib.h>

#if !SET_STATIC
#  include "set-app.h"
#endif


#ifndef SET_USE_BUILTINS
#define SET_USE_BUILTINS (defined(__GNUC__) || defined(__clang__))
#endif


/*   See set-doc for general documentation on this facility, and for the
 *   documentation on functions that are part of the application interface.
 */


/* --------------------- UTILITIES USED IN THIS MODULE ---------------------- */


/* DEBUGGING CHECKS.  These check validity of data, calling abort if
   the check fails.  Enabled only if SET_DEBUG is defined to be 1.
   This may be done with a compiler flag, in which case it isn't
   overridden here. */

static int check_n_elements (struct set *set);

#ifndef SET_DEBUG
#define SET_DEBUG 0
#endif

#define CHK_CHAIN(chain) \
  do { \
    if (SET_DEBUG && ((chain) < 0 || (chain) >= SET_CHAINS)) abort(); \
  } while (0)

#define CHK_SET(set) \
  do { \
    CHK_CHAIN((set)->chain); \
    if (SET_DEBUG && (set)->first < 0 \
                  && (set)->first != SET_END_OF_CHAIN) abort(); \
    if (SET_DEBUG && (set)->n_elements != 0 \
                  && (set)->first == SET_END_OF_CHAIN) abort(); \
    if (SET_DEBUG && !check_n_elements((set))) abort(); \
  } while (0)

#define CHK_SET_INDEX(set,index) \
  do { \
    if (SET_DEBUG && !check_has_seg((set),(index))) abort(); \
  } while (0)

#define CHK_SEGMENT(seg,chain) \
  do { \
    CHK_CHAIN(chain); \
    if (SET_DEBUG && (seg)->next[chain] < 0 \
                  && (seg)->next[chain] != SET_NOT_IN_CHAIN \
                  && (seg)->next[chain] != SET_END_OF_CHAIN) abort(); \
    if (SET_DEBUG && (seg)->next[chain] == SET_NOT_IN_CHAIN \
                  && (seg)->bits[chain] != 0) abort(); \
  } while (0)


/* FIND POSITION OF LOWEST-ORDER BIT.  The position returned is from 0 up.
   The argument must not be zero.

   Fast for gcc and clang, using their builtin functions. */

static inline int first_bit_pos (set_bits_t b)
{ 
  if (SET_DEBUG && b == 0) abort();

# if SET_USE_BUILTINS
    return sizeof b <= sizeof (unsigned) ? __builtin_ctz(b) 
         : sizeof b <= sizeof (unsigned long) ? __builtin_ctzl(b) 
         : sizeof b <= sizeof (unsigned long long) ? __builtin_ctzll(b)
         : (abort(), 0);
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


/* FIND THE NUMBER OF BITS IN A SET OF BITS.  

   Fast for gcc and clang, using their builtin functions. */

static inline int bit_count (set_bits_t b)
{ 
# if SET_USE_BUILTINS
    return sizeof b <= sizeof (unsigned) ? __builtin_popcount(b) 
         : sizeof b <= sizeof (unsigned long) ? __builtin_popcountl(b) 
         : sizeof b <= sizeof (unsigned long long) ? __builtin_popcountll(b)
         : (abort(), 0);
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


/* REMOVE ANY EMPTY SEGMENTS AT THE FRONT OF A SET. */

static inline void remove_empty (struct set *set)
{
  struct set_segment *seg;

  CHK_SET(set);

  while (set->first != SET_END_OF_CHAIN)
  { 
    seg = SET_SEGMENT(set->first);
    CHK_SEGMENT(seg,set->chain);

    if (seg->bits[set->chain] != 0)
    { break;
    }

    set->first = seg->next[set->chain];
    seg->next[set->chain] = SET_NOT_IN_CHAIN;
  }
}


/* CHECK WHETHER THE COUNT OF NUMBER OF ELEMENTS IN A SET IS CORRECT. */

static int check_n_elements (struct set *set)
{
  struct set_segment *seg;
  set_index_t index;
  set_value_t cnt;
  int chain;

  chain = set->chain;
  cnt = set->n_elements;
  index = set->first;

  while (index != SET_END_OF_CHAIN)
  { seg = SET_SEGMENT(index);
    cnt -= bit_count (seg->bits[chain]);
    index = seg->next[chain];
  }

  return cnt == 0;
}


/* CHECK WHETHER A SET CONTAINS A SEGMENT WITH GIVEN INDEX. */

static int check_has_seg (struct set *set, set_index_t index)
{
  set_index_t ix = set->first;
  while (ix != SET_END_OF_CHAIN)
  { if (ix == index) 
    { return 1;
    }
    ix = SET_SEGMENT(ix) -> next[set->chain];
  }
  return 0;
}


/* --------------------- FUNCTIONS USED BY APPLICATIONS --------------------- */


/* INITIALIZE A SET, AS EMPTY. */

SET_PROC_CLASS void set_init (struct set *set, int chain)
{
  CHK_CHAIN(chain);

  set->chain = chain;
  set->first = SET_END_OF_CHAIN;
  set->n_elements = 0;
}


/* INITIALIZE A SEGMENT STRUCTURE. */

SET_PROC_CLASS void set_segment_init (struct set_segment *seg)
{
  int j;
  for (j = 0; j < SET_CHAINS; j++)
  { seg->bits[j] = 0;
    seg->next[j] = SET_NOT_IN_CHAIN;
  }
}


/* ADD A VALUE TO A SET.  The value must not be in a segment with
   members in a different set using the same chain (or previously,
   if the segment may still be in the other set's chain).

   This is implemented by setting the right bit in the bits for the
   set's chain, within the segment structure for this value's index.
   This segment is then added to the linked list of segments for this
   set if it is not there already, and the element count is updated. */

SET_PROC_CLASS int set_add (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);

  set_bits_t b = seg->bits[set->chain];
  set_bits_t t = (set_bits_t)1 << offset;

  if (b & t)
  { CHK_SET_INDEX(set,index);
    return 1;
  }

  if (seg->next[set->chain] == SET_NOT_IN_CHAIN)
  { seg->next[set->chain] = set->first;
    set->first = index;
  }

  seg->bits[set->chain] |= t;
  set->n_elements += 1;

  CHK_SET_INDEX(set,index);
  CHK_SET(set);

  return 0;
}


/* REMOVE A VALUE FROM A SET.  The value must not be in any other set using
   the same chain.

   This is implemented by clearing the right bit in the bits for the set's 
   chain, within the segment structure for this value's index, and updating
   the count of elements in the set. */

SET_PROC_CLASS int set_remove (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);

  set_bits_t b = seg->bits[set->chain];
  set_bits_t t = (set_bits_t)1 << offset;

  if ((b & t) == 0)
  { return 0;
  }

  CHK_SET_INDEX(set,index);

  seg->bits[set->chain] &= ~t;
  set->n_elements -= 1;

  return 1;
}


/* FIND AND POSSIBLY REMOVE THE FIRST ELEMENT IN A SET.  Removal with
   the function of the last value in a segment allows that segment to
   be added to another set using the same chain.

   The linked list of segments for this set is first trimmed of any at
   the front that have no elements set, to save time in any future
   searches.  This is also done after removing the first element, to
   ensure that if the segment no longer contains elements of this set
   it can be used in another set using the same chain.  */

SET_PROC_CLASS set_value_t set_first (struct set *set, int remove)
{ 
  struct set_segment *seg;
  set_value_t first;
  set_bits_t b;
  int o;

  CHK_SET(set);

  remove_empty(set);

  if (set->first == SET_END_OF_CHAIN) 
  { return SET_NO_VALUE;
  }

  seg = SET_SEGMENT(set->first);
  CHK_SEGMENT(seg,set->chain);

  b = seg->bits[set->chain];
  o = first_bit_pos(b);
  first = SET_VAL (set->first, o);

  if (remove) 
  { seg->bits[set->chain] &= ~ ((set_bits_t)1 << o);
    set->n_elements -= 1;
    remove_empty(set);
  }

  CHK_SET(set);

  return first;
}


/* FIND THE NEXT ELEMENT IN A SET.  

   If the linked list has to be followed to a later segment, any
   unused segments that are skipped are deleted from the list, to save
   time in any future searches.  As a consequence, if 'val' is not
   removed, the value returned (if not SET_NO_VALUE) is either in the
   segment containing 'val' or the following segment.  (But note that
   if 'val' is removed, the segment containing it is not removed from
   the list even if it no longer has any elements.) */

SET_PROC_CLASS set_value_t set_next (struct set *set, set_value_t val, 
                                     int remove)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);
  CHK_SET_INDEX(set,index);

  /* Get the bits after the one for the element we are looking after.
     Also clear the bit for 'val' if we are removing it. */

  set_bits_t b = seg->bits[set->chain] >> offset;
  if (SET_DEBUG && (b & 1) == 0) abort();  /* 'val' isn't in 'set' */
  if (remove)
  { seg->bits[set->chain] &= ~ ((set_bits_t) 1 << offset);
    set->n_elements -= 1;
  }
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
      nindex = seg->next[set->chain];
      if (nindex == SET_END_OF_CHAIN) 
      { return SET_NO_VALUE;
      }

      nseg = SET_SEGMENT(nindex);
      CHK_SEGMENT(nseg,set->chain);

      b = nseg->bits[set->chain];
      if (b != 0) 
      { break;
      }

      seg->next[set->chain] = nseg->next[set->chain];
      nseg->next[set->chain] = SET_NOT_IN_CHAIN;
    }

    index = nindex;
    offset = 0;
  }

  offset += first_bit_pos(b);

  CHK_SET(set);

  return SET_VAL(index,offset);
}


/* FIND THE NEXT ELEMENT IN A SET THAT IS IN A DIFFERENT SEGMENT. */

SET_PROC_CLASS set_value_t set_next_segment (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);
  CHK_SET_INDEX(set,index);

  set_index_t nindex;
  struct set_segment *nseg;

  /* Go to the next segment, removing any segments that are unused. If there
     is no next segment, return SET_NO_VALUE. */

  for (;;)
  { 
    nindex = seg->next[set->chain];
    if (nindex == SET_END_OF_CHAIN) 
    { return SET_NO_VALUE;
    }

    nseg = SET_SEGMENT(nindex);
    CHK_SEGMENT(nseg,set->chain);

    set_bits_t b = nseg->bits[set->chain];
    if (b != 0) 
    { return SET_VAL (nindex, first_bit_pos(b));
    }

    seg->next[set->chain] = nseg->next[set->chain];
    nseg->next[set->chain] = SET_NOT_IN_CHAIN;
  }
}


/* RETURN THE BITS INDICATING MEMBERSHIP FOR THE FIRST SEGMENT OF A SET. 
   First removes empty segments at the front. */

SET_PROC_CLASS set_bits_t set_first_bits (struct set *set)
{
  struct set_segment *seg;

  CHK_SET(set);
  remove_empty(set);

  if (set->first == SET_END_OF_CHAIN)
  { return 0;
  }

  return SET_SEGMENT(set->first) -> bits[set->chain];
}


/* RETURN BITS INDICATING MEMBERSHIP FOR THE SEGMENT CONTAINING AN ELEMENT. */

SET_PROC_CLASS set_bits_t set_segment_bits (struct set *set, set_value_t val)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);
  CHK_SET_INDEX(set,index);

  return seg->bits[set->chain];
}


/* ASSIGN BITS INDICATING MEMBERSHIP FOR THE SEGMENT CONTAINING AN ELEMENT. */

SET_PROC_CLASS void set_assign_segment_bits (struct set *set, set_value_t val,
                                             set_bits_t b)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);
  CHK_SET_INDEX(set,index);

  set->n_elements -= bit_count(seg->bits[set->chain]);
  seg->bits[set->chain] = b;
  set->n_elements += bit_count(b);

  CHK_SET(set);
}


/* MOVE THE FIRST SEGMENT OF A SET TO ANOTHER SET USING THE SAME CHAIN. */

SET_PROC_CLASS void set_move_first (struct set *src, struct set *dst)
{
  struct set_segment *seg;
  set_index_t index;
  set_value_t cnt;

  CHK_SET(src);
  CHK_SET(dst);

  if (SET_DEBUG && src->chain != dst->chain) abort();
  if (SET_DEBUG && src->first == SET_END_OF_CHAIN) abort();

  remove_empty(src);
  remove_empty(dst);

  index = src->first;
  seg = SET_SEGMENT(index);
  CHK_SEGMENT(seg,src->chain);
  if (SET_DEBUG && seg->bits[src->chain] == 0) abort();

  cnt = bit_count(seg->bits[src->chain]);
  src->n_elements -= cnt;
  dst->n_elements += cnt;

  src->first = seg->next[src->chain];

  seg->next[src->chain] = dst->first;
  dst->first = index;

  CHK_SET(src);
  CHK_SET(dst);
}


/* MOVE SEGMENT AFTER THAT CONTAINING AN ELEMENT TO ANOTHER SET IN THE CHAIN. */

SET_PROC_CLASS void set_move_next (struct set *src, set_value_t val, 
                                   struct set *dst)
{
  set_index_t index = SET_VAL_INDEX(val);
  set_offset_t offset = SET_VAL_OFFSET(val);
  struct set_segment *seg = SET_SEGMENT(index);
  set_value_t cnt;

  CHK_SET(src);
  CHK_SET_INDEX(src,index);
  CHK_SET(dst);

  if (SET_DEBUG && src->chain != dst->chain) abort();

  int chain = src->chain;
  set_index_t nindex = seg->next[chain];

  if (SET_DEBUG && nindex == SET_END_OF_CHAIN) abort();

  struct set_segment *nseg = SET_SEGMENT(nindex);
  CHK_SEGMENT(nseg,chain);
  if (SET_DEBUG && nseg->bits[chain] == 0) abort();

  cnt = bit_count(nseg->bits[chain]);
  src->n_elements -= cnt;
  dst->n_elements += cnt;

  seg->next[chain] = nseg->next[chain];
  nseg->next[chain] = dst->first;
  dst->first = nindex;

  CHK_SET(src);
  CHK_SET(dst);
}


/* ADD ELEMENTS IN ANY SET USING SOME CHAIN WITHIN SOME SEGMENT TO SOME SET. */

SET_PROC_CLASS void set_add_segment (struct set *set, set_value_t val, 
                                     int chain)
{
  CHK_SET(set);
  int dst_chain = set->chain;

  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SEGMENT(seg,dst_chain);
  CHK_SEGMENT(seg,chain);

  set_bits_t added_bits = seg->bits[chain] & ~seg->bits[dst_chain];

  if (added_bits != 0)
  { 
    seg->bits[dst_chain] |= added_bits;
    set->n_elements += bit_count(added_bits);

    if (seg->next[dst_chain] == SET_NOT_IN_CHAIN)
    { seg->next[dst_chain] = set->first;
      set->first = index;
    }
  }
}


/* REMOVE ELEMENTS IN A SET WITHIN SOME SEGMENT AND IN ANY SET IN SOME CHAIN. */

SET_PROC_CLASS void set_remove_segment (struct set *set, set_value_t val, 
                                        int chain)
{
  CHK_SET(set);
  int dst_chain = set->chain;

  set_index_t index = SET_VAL_INDEX(val);
  struct set_segment *seg = SET_SEGMENT(index);

  CHK_SEGMENT(seg,dst_chain);
  CHK_SEGMENT(seg,chain);

  set_bits_t removed_bits = seg->bits[chain] & seg->bits[dst_chain];

  if (removed_bits != 0)
  { 
    seg->bits[dst_chain] &= ~removed_bits;
    set->n_elements -= bit_count(removed_bits);
  }
}
