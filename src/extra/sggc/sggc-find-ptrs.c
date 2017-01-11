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
        sggc_cptr_t p = COMPRESSED_PTR(ATTRIB(n));
        if (p) if (!sggc_look_at(p)) return;
    }

    if (sggctype == 1)
        return;

    if (sggctype == 2) {
        sggc_cptr_t p1 = COMPRESSED_PTR(CAR(n));
        sggc_cptr_t p2 = COMPRESSED_PTR(CDR(n));
        sggc_cptr_t p3 = COMPRESSED_PTR(TAG(n));
        if (p1) if (!sggc_look_at(p1)) return;
        if (p2) if (!sggc_look_at(p2)) return;
        if (p3) (void) sggc_look_at(p3);
        return;
    }

    if (sggctype == 3) {
        SEXP *ptr = &STRING_ELT(n,0);
        R_len_t cnt = LENGTH(n);
        while (cnt > 0) {
            sggc_cptr_t p = COMPRESSED_PTR(*ptr);
            if (!sggc_look_at(p)) return;
            ptr += 1;
            cnt -= 1;
        }
        return;
    }

    if (sggctype == 4) {
        if (CDR(n))
            if (!sggc_look_at(COMPRESSED_PTR(CDR(n)))) return;
        if (TAG(n))
            (void) sggc_look_at(COMPRESSED_PTR(TAG(n)));
        return;
    }

    abort();
}
