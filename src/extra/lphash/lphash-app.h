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

#ifndef LPHASH_APP_H_
#define LPHASH_APP_H_

typedef unsigned lphash_entry_t;    /* SEXP32 pointer to symbol object */
typedef char *lphash_key_t;         /* Symbol's print name */
typedef int lphash_hash_t;          /* Character/symbol hash as stored in symbol
                                       and in PRINTNAME(symbol) */

/* Structure of bucket in hash table. */

typedef struct {
    lphash_entry_t entry;           /* SEXP32 pointer to symbol object */
    lphash_hash_t hash;             /* Character/symbol hash */
} lphash_bucket_t;

#define LPHASH_NO_ENTRY 0           /* R_NoObject */

#define lphash_free free

#include "lphash.h"

#endif
