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


#define LOOK_AT(x) ((x) ? sggc_look_at(COMPRESSED_PTR(x)) : 1)

#define no_action_types \
( (1 << NILSXP) + \
  (1 << BUILTINSXP) + \
  (1 << SPECIALSXP) + \
  (1 << CHARSXP) + \
  (1 << LGLSXP) + \
  (1 << INTSXP) + \
  (1 << REALSXP) + \
  (1 << CPLXSXP) + \
  (1 << WEAKREFSXP) + \
  (1 << RAWSXP) + \
  (1 << S4SXP) )

#define three_pointer_types \
( (1 << ENVSXP) + \
  (1 << CLOSXP) + \
  (1 << PROMSXP) + \
  (1 << LISTSXP) + \
  (1 << LANGSXP) + \
  (1 << DOTSXP) + \
  (1 << SYMSXP) + \
  (1 << BCODESXP) )

#define vector_of_pointers_types \
( (1 << VECSXP) + \
  (1 << EXPRSXP) + \
  (1 << STRSXP) )

#undef NOT_LVALUE          /* Allow CAR, etc. as lvalue below */
#define NOT_LVALUE(x) (x)

#ifdef sggc_find_object_ptrs
static inline
#endif
void sggc_find_object_ptrs (sggc_cptr_t cptr)
{
    SEXP n = SEXP_PTR(cptr);
    SEXPTYPE typ = TYPEOF(n);

    if (ATTRIB(n) != R_NilValue && typ != CHARSXP) {
        if (!LOOK_AT(ATTRIB(n))) return;
    }

    if (! ((no_action_types >> typ) & 1)) {
        SEXP *strt; R_len_t cnt;
        if ((three_pointer_types >> typ) & 1) {
            strt = &CAR(n); cnt = 3;
        }
        else if ((vector_of_pointers_types >> typ) & 1) {
            cnt = LENGTH(n);
            if (cnt == 0) return;
            strt = &STRING_ELT(n,0);
        }
        else if (typ == EXTPTRSXP) {
            strt = &CDR(n); cnt = 2;
        }
        do {
            if (!LOOK_AT(*strt)) return;
            strt += 1;
            cnt -= 1;
        } while (cnt > 0);
    }
}
