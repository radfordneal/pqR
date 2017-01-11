/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016, 2017 Radford M. Neal
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


/* This source file is included in either main/memory.c or in sggc-app.h,
   not compiled on its own. */


#define LOOK_AT(x) \
  ((x) && COMPRESSED_PTR(x) /* not R_NilValue */ \
    ? sggc_look_at(COMPRESSED_PTR(x)) : 1)

#undef NOT_LVALUE          /* Allow CAR, etc. as lvalue below */
#define NOT_LVALUE(x) (x)

#ifdef sggc_find_object_ptrs
static inline
#endif
void sggc_find_object_ptrs (sggc_cptr_t cptr)
{
    sggc_type_t sggctype = SGGC_TYPE(cptr);

    if (sggctype == 0)
        return;

    SEXP n = SEXP_PTR(cptr);

    if (ATTRIB(n) != R_NilValue) {
        if (!LOOK_AT(ATTRIB(n))) return;
    }

    if (sggctype == 1)
        return;

    if (sggctype == 2) {
        if (!LOOK_AT(CAR(n))) return;
        if (!LOOK_AT(CDR(n))) return;
        LOOK_AT(TAG(n));
        return;
    }

    SEXP *strt; R_len_t cnt;
    if (sggctype == 3) {
        cnt = LENGTH(n);
        if (cnt == 0) return;
        strt = &STRING_ELT(n,0);
    }
    else {
        cnt = 2;
        strt = &CDR(n);
    }

    do {
        if (!LOOK_AT(*strt)) return;
        strt += 1;
        cnt -= 1;
    } while (cnt > 0);
}
