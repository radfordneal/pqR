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


#ifdef LPHASH_STATIC
#define LPHASH_FUN static
#else
#define LPHASH_FUN
#endif

#include <stdlib.h>


/* INTERNAL STRUCTURE OF A HASH TABLE.  Should not be directly
   referenced by the application, except for testing and performance
   evaluation (using the statistics fields). */

typedef struct
{ 
  int size;                  /* Number of buckets in table */
  int occupied;              /* Number of occupied buckets */
  int threshold;             /* Threshold for increasing table size */
  int threshold2;            /* Threshold for declaring overflow */

  lphash_bucket_t *buckets;  /* Pointer to an array of 'size' buckets */
  int buckets_offset;        /* Offset added to align buckets */

# ifdef LPHASH_STATS
  int searches;              /* Number of key searches done */
  int not_found;             /* Number of key searches where entry not found */
  int probes;                /* Number of buckets probed */
  int matches;               /* Number of calls of lphash_match */
# endif

} lphash_table_t;


/* PROTOTYPES FOR FUNCTIONS PROVIDED BY LPHASH TO THE APPLICATION. */

LPHASH_FUN lphash_table_t *lphash_create (int initial_size);

LPHASH_FUN lphash_bucket_t *lphash_key_lookup (lphash_table_t *table, 
                                               lphash_hash_t hash,
                                               lphash_key_t key);

LPHASH_FUN lphash_bucket_t *lphash_insert (lphash_table_t *table, 
                                           lphash_hash_t hash,
                                           lphash_key_t key);

LPHASH_FUN void lphash_destroy (lphash_table_t *table);


/* INLINE FUNCTIONS PROVIDED BY LPHASH TO THE APPLICATION. */

static inline lphash_bucket_t *lphash_entry_lookup (lphash_table_t *table,
                                                    lphash_hash_t hash,
                                                    lphash_entry_t entry)
{
  int i, x;

  i = hash & (table->size-1);
  x = 0;

  /* Note:  Table should always have an empty bucket, ensuring termination. */

  for (;;)
  {
#   ifdef LPHASH_LINEAR
      int ix = (i+x) & (table->size-1);
#   else
      int ix = i^x;
#   endif

    lphash_bucket_t *b = &table->buckets[ix];

    if (b->entry == entry)
    { return b;
    }

    if (b->entry == LPHASH_NO_ENTRY)
    {  return NULL;
    }

    x += 1;
  }
}

static inline lphash_bucket_t *lphash_first_bucket (lphash_table_t *table)
{ 
  if (table->occupied == 0)
  { return NULL;
  }

  lphash_bucket_t *b = table->buckets;
  while (b->entry == LPHASH_NO_ENTRY)
  { b += 1;
  }
  return b;
}

static inline lphash_bucket_t *lphash_next_bucket (lphash_table_t *table, 
                                                   lphash_bucket_t *bucket)
{ 
  for (;;)
  { bucket += 1;
    if (bucket >= table->buckets + table->size)
    { return NULL;
    }
    if (bucket->entry != LPHASH_NO_ENTRY)
    { return bucket;
    }
  }
}


/* FUNCTIONS PROVIDED BY THE APPLICATION TO LPHASH.  Not declared if 
   the application has defined them as macros (in lphash-app.h). */

#ifndef lphash_match 
LPHASH_FUN int lphash_match (lphash_bucket_t *bucket, lphash_key_t key);
#endif

#ifndef lphash_setup_bucket
LPHASH_FUN void lphash_setup_bucket (lphash_bucket_t *bucket, lphash_key_t key);
#endif

#ifndef lphash_malloc
LPHASH_FUN void *lphash_malloc (size_t size);
#endif

#ifndef lphash_free
LPHASH_FUN void lphash_free (void *ptr);
#endif
