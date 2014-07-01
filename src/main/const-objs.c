/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Development Core Team.
 *  Copyright (C) 2003-4	    The R Foundation
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif

#include <complex.h>
#include "Defn.h"		/*-> Arith.h -> math.h */


/* Definition of the R_NilValue constant, whose address when cast to SEXP is 
   R_NilValue.  Declared as "const" so that many compilers will put it in a 
   read-only area of memory. 

   Must be marked, and be of the oldest generation, so the garbage collector
   won't fiddle with it. */

const SEXPREC R_NilValue_constant = { \
    .sxpinfo = { .nmcnt = 7, .type = NILSXP, .gcgen = 1, .mark = 1 },
    .attrib = R_NilValue,
    .u = { .listsxp = 
            { .carval = R_NilValue, .cdrval = R_NilValue, .tagval = R_NilValue }
         }
};
