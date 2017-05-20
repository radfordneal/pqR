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

#if !SBSET_STATIC
#  include "sbset-app.h"
#endif


/*   See set-doc for general documentation on this facility, and for the
 *   documentation on functions that are part of the application interface.
 */


/* --------------------- UTILITIES USED IN THIS MODULE ---------------------- */


/* DEBUGGING CHECKS.  These check validity of data, calling abort if
   the check fails.  Enabled only if SBSET_DEBUG is defined to be 1.
   This may be done with a compiler flag, in which case it isn't
   overridden here. */

static void check_n_elements (struct sbset *set);

#ifndef SBSET_DEBUG
#define SBSET_DEBUG 0
#endif

#define CHK_CHAIN(chain) \
  do { \
    if (SBSET_DEBUG && ((chain) < 0 || (chain) >= SBSET_CHAINS)) abort(); \
  } while (0)

#define CHK_SET(set) \
  do { \
    CHK_CHAIN((set)->chain); \
    if (SBSET_DEBUG && (set)->first < 0 \
                  && (set)->first != SBSET_END_OF_CHAIN) abort(); \
    if (SBSET_DEBUG && (set)->n_elements != 0 \
                  && (set)->first == SBSET_END_OF_CHAIN) abort(); \
    if (SBSET_DEBUG) check_n_elements(set); \
  } while (0)

#define CHK_SBSET_INDEX(set,index) \
  do { \
    if (SBSET_DEBUG && !check_has_seg((set),(index))) abort(); \
  } while (0)

#define CHK_SEGMENT(seg,chain) \
  do { \
    CHK_CHAIN(chain); \
    if (SBSET_DEBUG && (seg)->next[chain] < 0 \
                  && (seg)->next[chain] != SBSET_NOT_IN_CHAIN \
                  && (seg)->next[chain] != SBSET_END_OF_CHAIN) abort(); \
    if (SBSET_DEBUG && (seg)->next[chain] == SBSET_NOT_IN_CHAIN \
                  && (seg)->bits[chain] != 0) abort(); \
  } while (0)


/* REMOVE ANY EMPTY SEGMENTS AT THE FRONT OF A SET. */

static inline void remove_empty (struct sbset *set)
{
  struct sbset_segment *seg;

  CHK_SET(set);

  while (set->first != SBSET_END_OF_CHAIN)
  { 
    seg = SBSET_SEGMENT(set->first);
    CHK_SEGMENT(seg,set->chain);

    if (seg->bits[set->chain] != 0)
    { break;
    }

    set->first = seg->next[set->chain];
    seg->next[set->chain] = SBSET_NOT_IN_CHAIN;
  }
}


/* ABORT IF THE COUNT OF NUMBER OF ELEMENTS IN A SET IS INCORRECT. */

static void check_n_elements (struct sbset *set)
{
  struct sbset_segment *seg;
  sbset_index_t index;
  sbset_value_t cnt;
  int chain;

  chain = set->chain;
  cnt = set->n_elements;
  index = set->first;

  while (index != SBSET_END_OF_CHAIN)
  { seg = SBSET_SEGMENT(index);
    cnt -= sbset_bit_count (seg->bits[chain]);
    index = seg->next[chain];
  }

  if (cnt != 0) abort();
}


/* CHECK WHETHER A SET CONTAINS A SEGMENT WITH GIVEN INDEX. */

static int check_has_seg (struct sbset *set, sbset_index_t index)
{
  sbset_index_t ix = set->first;
  while (ix != SBSET_END_OF_CHAIN)
  { if (ix == index) 
    { return 1;
    }
    ix = SBSET_SEGMENT(ix) -> next[set->chain];
  }
  return 0;
}


/* --------------------- FUNCTIONS USED BY APPLICATIONS --------------------- */


/* INITIALIZE A SET, AS EMPTY. */

SBSET_PROC_CLASS void sbset_init (struct sbset *set, int chain)
{
  CHK_CHAIN(chain);

  set->chain = chain;
  set->first = SBSET_END_OF_CHAIN;
  set->n_elements = 0;
}


/* INITIALIZE A SEGMENT STRUCTURE. */

SBSET_PROC_CLASS void sbset_segment_init (struct sbset_segment *seg)
{
  int j;
  for (j = 0; j < SBSET_CHAINS; j++)
  { seg->bits[j] = 0;
    seg->next[j] = SBSET_NOT_IN_CHAIN;
  }
}


/* FIND AND POSSIBLY REMOVE THE FIRST ELEMENT IN A SET.  Removal with
   this function of the last value in a segment allows that segment to
   be added to another set using the same chain.

   The linked list of segments for this set is first trimmed of any at
   the front that have no elements set, to save time in any future
   searches.  This is also done after removing the first element, to
   ensure that if the segment no longer contains elements of this set
   it can be used in another set using the same chain.  */

SBSET_PROC_CLASS sbset_value_t sbset_first (struct sbset *set, int remove)
{ 
  struct sbset_segment *seg;
  sbset_value_t first;
  sbset_bits_t b;
  int o;

  CHK_SET(set);

  remove_empty(set);

  if (set->first == SBSET_END_OF_CHAIN) 
  { return SBSET_NO_VALUE;
  }

  seg = SBSET_SEGMENT(set->first);
  CHK_SEGMENT(seg,set->chain);

  b = seg->bits[set->chain];
  o = sbset_first_bit_pos(b);
  first = SBSET_VAL (set->first, o);

  if (remove) 
  { seg->bits[set->chain] &= ~ ((sbset_bits_t)1 << o);
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
   removed, the value returned (if not SBSET_NO_VALUE) is either in the
   segment containing 'val' or the following segment.  (But note that
   if 'val' is removed, the segment containing it is not removed from
   the list even if it no longer has any elements.) */

SBSET_PROC_CLASS sbset_value_t sbset_next (struct sbset *set, 
                                           sbset_value_t val, int remove)
{
  sbset_index_t index = SBSET_VAL_INDEX(val);
  sbset_offset_t offset = SBSET_VAL_OFFSET(val);
  struct sbset_segment *seg = SBSET_SEGMENT(index);

  CHK_SET(set);
  CHK_SEGMENT(seg,set->chain);
  CHK_SBSET_INDEX(set,index);

  /* Get the bits after the one for the element we are looking after.
     Also clear the bit for 'val' if we are removing it. */

  sbset_bits_t b = seg->bits[set->chain] >> offset;
  if (SBSET_DEBUG && (b & 1) == 0) abort();  /* 'val' isn't in 'set' */
  if (remove)
  { seg->bits[set->chain] &= ~ ((sbset_bits_t) 1 << offset);
    set->n_elements -= 1;
  }
  offset += 1;
  b >>= 1;

  /* If no bits are set after the one we are starting at, go to the
     next segment, removing ones that are unused.  We may discover
     that there is no next element, and return SBSET_NO_VALUE. */

  if (b == 0)
  { sbset_index_t nindex;
    struct sbset_segment *nseg;

    for (;;)
    { 
      nindex = seg->next[set->chain];
      if (nindex == SBSET_END_OF_CHAIN) 
      { return SBSET_NO_VALUE;
      }

      nseg = SBSET_SEGMENT(nindex);
      CHK_SEGMENT(nseg,set->chain);

      b = nseg->bits[set->chain];
      if (b != 0) 
      { break;
      }

      seg->next[set->chain] = nseg->next[set->chain];
      nseg->next[set->chain] = SBSET_NOT_IN_CHAIN;
    }

    index = nindex;
    offset = 0;
  }

  offset += sbset_first_bit_pos(b);

  CHK_SET(set);

  return SBSET_VAL(index,offset);
}


/* RETURN THE BITS INDICATING MEMBERSHIP FOR THE FIRST SEGMENT OF A SET. 
   First removes empty segments at the front. */

SBSET_PROC_CLASS sbset_bits_t sbset_first_bits (struct sbset *set)
{
  CHK_SET(set);
  remove_empty(set);

  if (set->first == SBSET_END_OF_CHAIN)
  { return 0;
  }

  return SBSET_SEGMENT(set->first) -> bits[set->chain];
}


/* MOVE THE FIRST SEGMENT OF A SET TO ANOTHER SET USING THE SAME CHAIN. */

SBSET_PROC_CLASS void sbset_move_first (struct sbset *src, 
                                        struct sbset *dst)
{
  struct sbset_segment *seg;
  sbset_index_t index;
  sbset_value_t cnt;

  CHK_SET(src);
  CHK_SET(dst);

  if (SBSET_DEBUG && src->chain != dst->chain) abort();
  if (SBSET_DEBUG && src->first == SBSET_END_OF_CHAIN) abort();

  remove_empty(src);
  remove_empty(dst);

  index = src->first;
  seg = SBSET_SEGMENT(index);
  CHK_SEGMENT(seg,src->chain);
  if (SBSET_DEBUG && seg->bits[src->chain] == 0) abort();

  cnt = sbset_bit_count(seg->bits[src->chain]);
  src->n_elements -= cnt;
  dst->n_elements += cnt;

  src->first = seg->next[src->chain];

  seg->next[src->chain] = dst->first;
  dst->first = index;

  CHK_SET(src);
  CHK_SET(dst);
}


/* MOVE SEGMENT AFTER THAT CONTAINING AN ELEMENT TO ANOTHER SET IN THE CHAIN. */

SBSET_PROC_CLASS void sbset_move_next (struct sbset *src, sbset_value_t val, 
                                       struct sbset *dst)
{
  sbset_index_t index = SBSET_VAL_INDEX(val);
  struct sbset_segment *seg = SBSET_SEGMENT(index);
  sbset_value_t cnt;

  CHK_SET(src);
  CHK_SBSET_INDEX(src,index);
  CHK_SET(dst);

  if (SBSET_DEBUG && src->chain != dst->chain) abort();

  int chain = src->chain;
  sbset_index_t nindex = seg->next[chain];

  if (SBSET_DEBUG && nindex == SBSET_END_OF_CHAIN) abort();

  struct sbset_segment *nseg = SBSET_SEGMENT(nindex);
  CHK_SEGMENT(nseg,chain);
  if (SBSET_DEBUG && nseg->bits[chain] == 0) abort();

  cnt = sbset_bit_count(nseg->bits[chain]);
  src->n_elements -= cnt;
  dst->n_elements += cnt;

  seg->next[chain] = nseg->next[chain];
  nseg->next[chain] = dst->first;
  dst->first = nindex;

  CHK_SET(src);
  CHK_SET(dst);
}


/* ADD ELEMENTS IN ANY SET USING SOME CHAIN WITHIN SOME SEGMENT TO SOME SET. */

SBSET_PROC_CLASS void sbset_add_segment (struct sbset *set, 
                                         sbset_value_t val, int chain)
{
  CHK_SET(set);
  int dst_chain = set->chain;

  sbset_index_t index = SBSET_VAL_INDEX(val);
  struct sbset_segment *seg = SBSET_SEGMENT(index);

  CHK_SEGMENT(seg,dst_chain);
  CHK_SEGMENT(seg,chain);

  sbset_bits_t added_bits = seg->bits[chain] & ~seg->bits[dst_chain];

  if (added_bits != 0)
  { 
    seg->bits[dst_chain] |= added_bits;
    set->n_elements += sbset_bit_count(added_bits);

    if (seg->next[dst_chain] == SBSET_NOT_IN_CHAIN)
    { seg->next[dst_chain] = set->first;
      set->first = index;
    }
  }

  CHK_SET(set);
}


/* REMOVE ELEMENTS IN A SET WITHIN SOME SEGMENT AND IN ANY SET IN SOME CHAIN. */

SBSET_PROC_CLASS void sbset_remove_segment (struct sbset *set, 
                                            sbset_value_t val, int chain)
{
  CHK_SET(set);
  int dst_chain = set->chain;

  sbset_index_t index = SBSET_VAL_INDEX(val);
  struct sbset_segment *seg = SBSET_SEGMENT(index);

  CHK_SEGMENT(seg,dst_chain);
  CHK_SEGMENT(seg,chain);

  sbset_bits_t removed_bits = seg->bits[chain] & seg->bits[dst_chain];

  if (removed_bits != 0)
  { 
    seg->bits[dst_chain] &= ~removed_bits;
    set->n_elements -= sbset_bit_count(removed_bits);
  }

  CHK_SET(set);
}
