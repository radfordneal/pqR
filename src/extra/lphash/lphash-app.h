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

typedef unsigned lphash_entry_t;    /* Compressed pointer to symbol object */
typedef char *lphash_key_t;         /* Symbol's print name */
typedef unsigned lphash_hash_t;     /* Character hash as stored in print name */

#define LPHASH_NO_ENTRY 0 /* R_NoObject */

#ifndef LPHASH_MAX_LOAD
#define LPHASH_MAX_LOAD 0.7
#endif

#define lphash_free free

#include "lphash.h"

#endif
