/* LPHASH - LOCALLY-PROBED HASH TABLE FACILITY - HEADER FILE.

   Copyright (c) 2017 Radford M. Neal.

   The lphash library is free software; you can redistribute it and/or modify
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


/* See lphash-doc for general information on the lphash library. */


#include <stdlib.h>


/* INTERNAL STRUCTURE OF A HASH TABLE.  Should not be directly
   referenced by the application, except for testing and performance
   evaluation (using the statistics fields). */

typedef struct 
{ lphash_entry_t entry;      /* Entry in this bucket, or LPHASH_NO_ENTRY */
  lphash_hash_t hash;        /* Full hash value for this entry (if present) */
} lphash_bucket_t;

typedef struct
{ 
  int size;                  /* Number of buckets in table */
  int occupied;              /* Number of occupied buckets */
  int threshold;             /* Threshold for increasing table size */
  int threshold2;            /* Threshold for declaring overflow */

  lphash_bucket_t *buckets;  /* Pointer to an array of 'size' buckets */
  int buckets_offset;        /* Offset added to align buckets */

# ifdef LPHASH_STATS
  int searches;              /* Number of searches done */
  int not_found;             /* Number of searches where entry not found */
  int probes;                /* Number of buckets probed */
  int matches;               /* Number of calls of lphash_match */
# endif

} *lphash_table_t;


/* FUNCTIONS PROVIDED BY LPHASH TO THE APPLICATION. */

lphash_table_t lphash_create (int initial_size);

lphash_entry_t lphash_lookup (lphash_table_t table, lphash_hash_t hash,
                              lphash_key_t key);

lphash_entry_t lphash_insert (lphash_table_t table, lphash_hash_t hash,
                              lphash_key_t key);

void lphash_destroy (lphash_table_t table);


/* FUNCTIONS PROVIDED BY THE APPLICATION TO LPHASH.  Not declared if 
   the application has defined them as macros (in lphash-app.h). */

#ifndef lphash_match 
int lphash_match (lphash_entry_t entry, lphash_key_t key);
#endif

#ifndef lphash_make_entry
lphash_entry_t lphash_make_entry (lphash_key_t key);
#endif

#ifndef lphash_malloc
void *lphash_malloc (size_t size);
#endif

#ifndef lphash_free
void lphash_free (void *ptr);
#endif
