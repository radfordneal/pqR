/* LPHASH - LOCALLY-PROBED HASH TABLE FACILITY - PROGRAM FOR TEST #1, STATIC.

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

#include <stdlib.h>
#include <stdio.h>

#define LPHASH_STATIC

#include "lphash.c"

int lphash_match (lphash_bucket_t *bucket, lphash_key_t key)
{
  return bucket->entry == atoi(key);
}

void lphash_setup_bucket (lphash_bucket_t *bucket, lphash_key_t key)
{
  bucket->entry = atoi(key);
}

lphash_hash_t hash (lphash_key_t key)
{
  lphash_hash_t h;
  int i;

  h = 0;
  for (i = 0; key[i] != 0; i++)
  { h = 5*h + (key[i]-'0');
  }

  return h;
}

char *tests[] = 
  { "4", "127", "10", "5", "12", "1050", "7", "1100", "1045", "132" };

#ifndef TABLE_SIZE
#define TABLE_SIZE 16
#endif

int main (int argc, char **argv)
{
  lphash_table_t *tbl;

  tbl = lphash_create (TABLE_SIZE);
  if (tbl == NULL)
  { fprintf (stderr, "Can't create table\n");
    exit(1);
  }

  int s, t;
  for (t = 0; ; t++)
  { 
    printf("\n");
    for (int ix = 0; ix < tbl->size; ix++)
    { printf(" %4d",ix);
    }
    printf("\n");
    for (int ix = 0; ix < tbl->size; ix++)
    { printf("%c",ix==0?'e':' ');
      if (tbl->buckets[ix].entry == LPHASH_NO_ENTRY)
      { printf("   -");
      }
      else
      { printf("%4d",tbl->buckets[ix].entry);
      }
    }
    printf("\n");
    for (int ix = 0; ix < tbl->size; ix++)
    { printf("%c",ix==0?'h':' ');
      if (tbl->buckets[ix].entry == LPHASH_NO_ENTRY)
      { printf("   -");
      }
      else
      { printf("%4d",tbl->buckets[ix].hash);
      }
    }
    printf("\n");

    printf("\n");
    for (s = 0; tests[s]; s++)
    { printf(" %4s",tests[s]);
    }
    printf("\n");
    for (s = 0; tests[s]; s++)
    { lphash_bucket_t *b;
      printf("%c",s==0?'e':' ');
      b = lphash_key_lookup(tbl,hash(tests[s]),tests[s]);
      printf("%4d", b ? b->entry : -1);
      if (lphash_entry_lookup(tbl,hash(tests[s]),atoi(tests[s])) != b) abort();
    }
    printf("\n");
    for (s = 0; tests[s]; s++)
    { printf("%c",s==0?'h':' ');
      printf("%4d",hash(tests[s]));
    }
    printf("\n");

    if (tests[t] == 0)
    { break;
    }

    printf ("\nInserting %s: h %d (%d), e %d\n", 
            tests[t], 
            hash(tests[t]), hash(tests[t]) & (tbl->size-1),
            lphash_insert(tbl,hash(tests[t]),tests[t])->entry);

    if (TABLE_SIZE == 8 && t+1 == 7)
    { printf("\nScan with one empty bucket:");
      lphash_bucket_t *b = lphash_first_bucket(tbl);
      while (b != NULL)
      { printf(" %d",b->entry);
        b = lphash_next_bucket(tbl,b);
      }
      printf("\n");
    }
  }

  printf("\nFinal scan:");
  lphash_bucket_t *b = lphash_first_bucket(tbl);
  while (b != NULL)
  { printf(" %d",b->entry);
    b = lphash_next_bucket(tbl,b);
  }
  printf("\n");

  printf("\nStatistics: ");
  printf("load %d/%d, %d searches (%d not found), %d probes, %d matches\n",
          tbl->occupied, tbl->size, 
          tbl->searches, tbl->not_found, 
          tbl->probes, tbl->matches);

  lphash_destroy(tbl);

  return 0;
}
