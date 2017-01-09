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

#include "sggc-app.h"


char R_type_to_sggc_type[32] = 
{
  0, /* NILSXP */
  2, /* SYMSXP */
  2, /* LISTSXP */
  2, /* CLOSXP */
  2, /* ENVSXP */
  2, /* PROMSXP */
  2, /* LANGSXP */
  1, /* SPECIALSXP */
  1, /* BUILTINSXP */
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
  2, /* S4SXP */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0, /* unused */
  0  /* unused */
};


/* UNCOMPRESSED, 64-BIT POINTERS. */

#if SIZEOF_SIZE_T == 8

sggc_nchunks_t R_nchunks (SEXPTYPE type, R_len_t length)
{
    switch (type) {
    case RAWSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length)    / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length+1)  / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case STRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+2];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif


/* UNCOMPRESSED, 32-BIT POINTERS. */

#if SIZEOF_SIZE_T == 4

sggc_nchunks_t R_nchunks (SEXPTYPE type, R_len_t length)
{
    switch (type) {
    case RAWSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length)    / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length+1)  / SGGC_CHUNK_SIZE;
    case INTSXP:
    case LGLSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case VECSXP:
    case EXPRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case STRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return sggc_kind_chunks[2*SGGC_N_TYPES+2];
    default:
        return sggc_kind_chunks[SGGC_N_TYPES+R_type_to_sggc_type[type]];
    }
}

#endif

sggc_kind_t sggc_kind (sggc_type_t type, sggc_length_t length)
{ 
    for (int k = SGGC_N_TYPES + type; k < SGGC_N_KINDS; k += SGGC_N_TYPES) {
        if (sggc_kind_chunks [k] >= length)
            return k;
    }

    return type;  /* kind for a big segment */
}
