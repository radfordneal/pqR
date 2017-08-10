/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2017 Radford M. Neal
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */


/* This header file is for the application of lphash for match, in unique.c. */


#ifndef LPHASH_APP_H_
#define LPHASH_APP_H_

/* #define LPHASH_STATS */

#define LPHASH_MAX_LOAD 0.9999999 /* Will figure out the desired size, so don't
                                     want it expanded automatically */

typedef int lphash_entry_t;       /* Index of first occurence of element, or
                                     0 for no entry, -index for incomparable */

typedef unsigned lphash_hash_t;   /* Hash value computed from table element */

typedef struct HashData *lphash_key_t;  /* Pointer to struct containing key */

#define LPHASH_NO_ENTRY 0         /* R_NoObject */

/* Structure of bucket in hash table. */

typedef struct {
    lphash_entry_t entry;
    lphash_hash_t hash;
} lphash_bucket_t;

#define lphash_free(ptr) ((void)0) /* Nothing need be done - freed by VMAXSET */

#include <lphash/lphash.h>

#endif
