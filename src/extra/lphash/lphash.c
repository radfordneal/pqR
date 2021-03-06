/* LPHASH - LOCALLY-PROBED HASH TABLE FACILITY - FUNCTION DEFINITIONS

   Copyright (c) 2017, 2018 Radford M. Neal.

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


#include "lphash-app.h"
#include <stdint.h>
#include <limits.h>
#include <string.h>
#include <math.h>


/* DEFAULT VALUES FOR OPTIONS. */

#ifndef LPHASH_MAX_LOAD
#define LPHASH_MAX_LOAD 0.75
#endif

#ifndef LPHASH_ALIGN
#define LPHASH_ALIGN 64
#endif


/* ALLOCATE BUCKETS FOR A TABLE.  Returns 0 if space can't be allocated.
   Otherwise returns 1 after setting the fields in 'table' for the bucket
   array (initialized to LPHASH_NO_ENTRY for 'entry', and otherwise zeros) 
   and the size and thresholds. */

static int allocate_buckets (lphash_table_t *table, int size)
{
  char *m, *m_aligned;

  size_t tbl_size = (size_t)size * sizeof (lphash_bucket_t);

  m = lphash_malloc (tbl_size + LPHASH_ALIGN-1);
  if (m == NULL) 
  { return 0;
  }

  m_aligned = (char *) (((uintptr_t)m + LPHASH_ALIGN-1) 
                           & ~(uintptr_t)(LPHASH_ALIGN-1));

  table->buckets = (lphash_bucket_t *) m_aligned;
  table->buckets_offset = m_aligned - m;

  table->size = size;

  table->threshold = (int) (size * LPHASH_MAX_LOAD);
  if (table->threshold < 2)
  { table->threshold = 2;
  }
  if (table->threshold >= size)
  { table->threshold = size-1;
  }

  table->threshold2 = (int) (size * sqrt(LPHASH_MAX_LOAD));
  if (table->threshold2 < 2)
  { table->threshold2 = 2;
  }
  if (table->threshold2 >= size)
  { table->threshold2 = size-1;
  }

  memset (m_aligned, 0, tbl_size);

  if (LPHASH_NO_ENTRY != 0)
  { for (int i = 0; i < size; i++)
    { table->buckets[i].entry = LPHASH_NO_ENTRY;
    }
  }
 
  return 1;
}


/* CREATE A HASH TABLE. */

LPHASH_FUN lphash_table_t *lphash_create (int initial_size)
{
  int size = 8;

  if (initial_size > 8)
  { while ((unsigned)size << 1 <= (unsigned)initial_size) 
    { size <<= 1;
    }
  }

  lphash_table_t *table = lphash_malloc (sizeof *table);
  if (table == NULL)
  { return NULL;
  }

  if (!allocate_buckets(table,size))
  { lphash_free(table);
    return NULL;
  }

  table->occupied = 0;

# ifdef LPHASH_STATS
    table->searches = 0;
    table->not_found = 0;
    table->probes = 0;
    table->matches = 0;
# endif

  return table;
}


/* DESTROY A HASH TABLE. */

LPHASH_FUN void lphash_destroy (lphash_table_t *table)
{
  lphash_free ((char *)table->buckets - table->buckets_offset);
  lphash_free (table);
}


/* SEARCH FOR A BUCKET WITH GIVEN HASH AND KEY.  Returns a pointer to
   the bucket found, or if not found, a pointer to the bucket where a
   new entry should be stored. */

static inline lphash_bucket_t *key_search (lphash_table_t *table, 
                                           lphash_hash_t hash, lphash_key_t key)
{
  int i, x;

# ifdef LPHASH_STATS
    table->searches += 1;
# endif
  
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

#   ifdef LPHASH_STATS
      table->probes += 1;
#   endif

    if (b->entry == LPHASH_NO_ENTRY)
    { 
#     ifdef LPHASH_STATS
        table->not_found += 1;
#     endif
      return b;
    }

    if (b->hash == hash)
    {
#     ifdef LPHASH_STATS
        table->matches += 1;
#     endif

      if (lphash_match(b,key))
      { return b;
      }
    }

    x += 1;
  }
}


/* TRY TO EXPAND A TABLE TO DOUBLE ITS SIZE. */

static void expand_table (lphash_table_t *table)
{ 
  if (table->size > INT_MAX/2)
  { return;
  }

  lphash_bucket_t *old_buckets = table->buckets;
  int old_size = table->size;
  int old_offset = table->buckets_offset;

  if (!allocate_buckets(table,old_size*2))
  { return;
  }

  int i, j, x, ix;

  for (j = 0; j < old_size; j++)
  { 
    if (old_buckets[j].entry == LPHASH_NO_ENTRY)
    { continue;
    }

    lphash_hash_t hash = old_buckets[j].hash;

    i = hash & (table->size-1);
    x = 0;

    for (;;)
    { 
#     ifdef LPHASH_LINEAR
        ix = (i+x) & (table->size-1);
#     else
        ix = i^x;
#     endif

      if (table->buckets[ix].entry == LPHASH_NO_ENTRY)
      { break;
      }

      x += 1;
    }

    table->buckets[ix] = old_buckets[j];
  }

  lphash_free ((char *)old_buckets - old_offset);
}


/* INSERT AN ENTRY IN A HASH TABLE. */

LPHASH_FUN lphash_bucket_t *lphash_insert (lphash_table_t *table, 
                                           lphash_hash_t hash,
                                           lphash_key_t key)
{
  lphash_bucket_t *b;

  b = key_search (table, hash, key);

  if (b->entry != LPHASH_NO_ENTRY)
  { return b;
  }

  if (table->occupied==table->threshold || table->occupied==table->threshold2)
  { expand_table (table);
    b = key_search (table, hash, key);
  }

  if (table->occupied >= table->threshold2)
  { return NULL;
  }

  b->hash = hash;
  lphash_setup_bucket(b,key);

  table->occupied += 1;

  return b;
}


/* SEARCH THE HASH TABLE BY KEY. */

LPHASH_FUN lphash_bucket_t *lphash_key_lookup (lphash_table_t *table, 
                                               lphash_hash_t hash,
                                               lphash_key_t key)
{
  lphash_bucket_t *b = key_search (table, hash, key);

  return b->entry == LPHASH_NO_ENTRY ? NULL : b;
}
