/* SGGC - A LIBRARY SUPPORTING SEGMENTED GENERATIONAL GARBAGE COLLECTION.
          Simple interpreter used to test SGGC - useless module to test header

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


/* Include the sggc-app header file after defining SGGC_NO_FUNCTIONS to
   test that the types and constants are defined, but not the functions.
   Compiled with -Wall to get warnings if it doesn't work. */

#define SET_STATIC 1
#define SGGC_NO_FUNCTIONS

#include "sggc-app.h"

sggc_cptr_t test_cptr_var = SGGC_NO_OBJECT;
