/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Wrapper for calloc/free for use in test programs

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
#include <stdio.h>


/* NUMBER OF STORAGE BLOCKS ALLOCATED AND NOT YET FREED. */

static int in_use = 0;


/* WRAPPER FOR CALLOC FOR USE IN TESTING.  Displays the address allocated,
   and the number of storage blocks in use after this allocation. */

void *test_calloc (size_t size)
{ 
  char *res;
  size_t i;

  res = calloc (size,1);
  if (res == NULL)
  { printf ("test_calloc: %d in use, calloc failed\n", in_use);
    return res;
  }

  in_use += 1;

  printf ("test_calloc: %d in use after:: %p\n", in_use, res);
  return res;
}


/* WRAPPER FOR FREE FOR USE IN TESTING.  Checks that the pointer being
   freed is not NULL, and that the in use count is not already zero,
   and displays the pointer freed and the in use count afterwards. */

void test_free (void *ptr)
{
  if (ptr == NULL) abort();
  if (in_use == 0) abort();

  free (ptr);
  in_use -= 1;

  printf ("test_free: %d in use after:: %p\n", in_use, ptr);
}
