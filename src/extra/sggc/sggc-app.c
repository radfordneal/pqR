/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 Radford M. Neal
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

#include <Defn.h>


#if USE_COMPRESSED_POINTERS

/* Constant lengths in auxiliary information 1, used for constant objects
   in const-objs.c and also for many non-constant objects. */

const R_len_t sggc_length0[SGGC_CHUNKS_IN_SMALL_SEGMENT] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const R_len_t sggc_length1[SGGC_CHUNKS_IN_SMALL_SEGMENT] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};

#endif

const char R_type_to_sggc_type[32] = 
{
  0, /* NILSXP */
  5, /* SYMSXP */
  2, /* LISTSXP */
  2, /* CLOSXP */
  2, /* ENVSXP */
  2, /* PROMSXP */
  2, /* LANGSXP */
  5, /* SPECIALSXP */
  5, /* BUILTINSXP */
  0, /* CHARSXP */
  1, /* LGLSXP */
  0, /* unused */
  0, /* unused */
  1, /* INTSXP */
  1, /* REALSXP */
  1, /* CPLXSXP */
  3, /* STRSXP */
  2, /* DOTSXP */
  0, /* unused */
  3, /* VECSXP */
  3, /* EXPRSXP */
  2, /* BCODESXP */
  4, /* EXTPTRSXP */
  2, /* WEAKREFSXP */
  1, /* RAWSXP */
  4, /* S4SXP */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0  /* unused */
};


/* COMPRESSED POINTERS. */

#if USE_COMPRESSED_POINTERS

sggc_nchunks_t Rf_nchunks (int type, unsigned length)
{
    unsigned hd = 8;  /* Size of header */

    switch (type) {
    case RAWSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length) / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length+1) / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
    case STRSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)16*length) / SGGC_CHUNK_SIZE;
    case EXTPTRSXP:
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+R_type_to_sggc_type[type]];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif


/* UNCOMPRESSED, 64-BIT POINTERS, ATTRIBUTE IN DATA AREA. */

#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && !USE_AUX_FOR_ATTRIB

sggc_nchunks_t Rf_nchunks (int type, unsigned length)
{
    unsigned hd = 24;  /* Size of header */

    switch (type) {
    case RAWSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length) / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length+1) / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
    case STRSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+R_type_to_sggc_type[type]];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif


/* UNCOMPRESSED, 64-BIT POINTERS, ATTRIBUTE IN AUXILIARY INFO 1. */

#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB

sggc_nchunks_t Rf_nchunks (int type, unsigned length)
{
    unsigned hd = 16;  /* Size of header for vector types */

    switch (type) {
    case RAWSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length) / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length+1) / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
    case STRSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+R_type_to_sggc_type[type]];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif


/* UNCOMPRESSED, 32-BIT POINTERS. */

#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 4

sggc_nchunks_t Rf_nchunks (int type, unsigned length)
{
    unsigned hd = 24;  /* Size of header */

    switch (type) {
    case RAWSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length) / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + length+1) / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)8*length) / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
    case STRSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)4*length) / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (hd + SGGC_CHUNK_SIZE-1 + (uint64_t)16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+R_type_to_sggc_type[type]];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif

#undef NOT_LVALUE          /* Allow CAR, etc. as lvalue here, since */
#define NOT_LVALUE(x) (x)  /*   needed for &STRING_ELT(...) below   */

#if USE_COMPRESSED_POINTERS
#define CHK_NO_OBJECT(x) 1  /* since checked in sggc_look_at */
#else
#define CHK_NO_OBJECT(x) ((x) != R_NoObject)
#endif

void sggc_find_object_ptrs (sggc_cptr_t cptr)
{
    sggc_type_t sggctype = SGGC_TYPE(cptr);

    if (sggctype == 0)
        return;

    SEXP a, n;

#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
    a = * (SEXP *) SGGC_AUX1(cptr);  /* avoid waste of CPTR -> UPTR -> CPTR */
#else
    n = SEXP_FROM_CPTR(cptr);
    a = ATTRIB(n);
#endif

    if (CHK_NO_OBJECT(a) && a != R_NilValue) {
        sggc_cptr_t p = CPTR_FROM_SEXP(a);
        sggc_look_at(p);
    }

    if (sggctype == 1)
        return;

#if !USE_COMPRESSED_POINTERS && SIZEOF_CHAR_P == 8 && USE_AUX_FOR_ATTRIB
    n = SEXP_FROM_CPTR(cptr);
#endif

    if (sggctype == 2) {
        SEXP car = CAR(n), cdr = CDR(n), tag = TAG(n);
        if (CHK_NO_OBJECT(car))
            sggc_look_at(CPTR_FROM_SEXP(car));
        if (cdr != R_NilValue && CHK_NO_OBJECT(cdr)) 
            sggc_look_at(CPTR_FROM_SEXP(cdr));
        if (tag != R_NilValue && CHK_NO_OBJECT(tag)) 
            sggc_look_at(CPTR_FROM_SEXP(tag));
        return;
    }

    if (sggctype == 3) {
        SEXP *ptr = &STRING_ELT(n,0);
        R_len_t cnt = LENGTH(n);
        while (cnt > 0) {
            sggc_cptr_t p = CPTR_FROM_SEXP(*ptr);
            sggc_look_at(p);
            ptr += 1;
            cnt -= 1;
        }
        return;
    }

    if (sggctype == 4) {
        if (CHK_NO_OBJECT(CDR(n)))
            sggc_look_at(CPTR_FROM_SEXP(CDR(n)));
        if (CHK_NO_OBJECT(TAG(n)))
            sggc_look_at(CPTR_FROM_SEXP(TAG(n)));
        return;
    }

    if (sggctype == 5)
        return;

    abort();
}
