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


/* UNCOMPRESSED, 64-BIT POINTERS. */

#if SIZEOF_SIZE_T == 8

sggc_nchunks_t sggc_nchunks (sggc_type_t type, sggc_length_t length)
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
    default:
        abort();
    }
}

#endif


/* UNCOMPRESSED, 32-BIT POINTERS. */

#if SIZEOF_SIZE_T == 4

sggc_nchunks_t sggc_nchunks (sggc_type_t type, sggc_length_t length)
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
    default:
        abort();
    }
}

#endif


/* Tables of kinds and allowed lengths for vector kinds. */

static int kind_chunks [SGGC_N_KINDS] = SGGC_KIND_CHUNKS;

static int kind_length [SGGC_N_KINDS-SGGC_N_TYPES];

static SEXPTYPE vectypes[] = {
    RAWSXP, CHARSXP, INTSXP, LGLSXP, VECSXP, EXPRSXP, STRSXP, CPLXSXP, NILSXP
};

void sggc_app_init (void)
{
    sggc_type_t typ;
    sggc_length_t len;
    int i, k;

    for (typ = 0; typ < SGGC_N_TYPES; typ++)
        kind_length[typ] = 1;

    for (i = 0; vectypes[i] != NILSXP; i++) {
        typ = vectypes[i];
        for (k = SGGC_N_TYPES + typ; k < SGGC_N_KINDS; k += SGGC_N_TYPES) {
            for (len = 1; sggc_nchunks(typ,len) <= kind_chunks[k]; len++)
                kind_length [k - SGGC_N_TYPES] = len;
        }
    }
}

sggc_kind_t sggc_kind (sggc_type_t type, sggc_length_t length)
{ 
    if (length == 1) /* handle non-vectors and vectors of length 1 quickly */
        return SGGC_N_TYPES + type;

    for (int k = SGGC_N_TYPES + type; k < SGGC_N_KINDS; k += SGGC_N_TYPES) {
        if (kind_length [k - SGGC_N_TYPES] >= length)
            return k;
    }

    return type;  /* kind for a big segment */
}
