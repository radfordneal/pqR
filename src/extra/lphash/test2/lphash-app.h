/* LPHASH - LOCALLY-PROBED HASH TABLE FACILITY - HEADER FILE FOR TEST #2.

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


#define LPHASH_STATS

typedef int lphash_entry_t;
typedef int lphash_key_t;
typedef unsigned lphash_hash_t;

typedef struct { lphash_entry_t entry; lphash_hash_t hash; } lphash_bucket_t;

#define LPHASH_NO_ENTRY (-1)

#ifndef LPHASH_MAX_LOAD
#define LPHASH_MAX_LOAD 0.95
#endif

#define lphash_malloc malloc
#define lphash_free free

#define lphash_match(b,k) ((b)->entry == (k))
#define lphash_setup_bucket(b,k) ((b)->entry = (k))

#include "lphash.h"
