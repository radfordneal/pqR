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

sggc_kind_t sggc_kind (sggc_type_t type, sggc_length_t length)
{ 
    return length > 1 ? type : SGGC_N_TYPES + type;
}

sggc_nchunks_t sggc_nchunks (sggc_type_t type, sggc_length_t length)
{
    switch (type) {
    case RAWSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length)    / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length+1)  / SGGC_CHUNK_SIZE;
    case INTSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case VECSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case STRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case CPLXSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return 5;
    case SPECIALSXP:
        return 4;
    case BUILTINSXP:
        return 4;
    default:
        return 3;
    }
}

#endif


/* UNCOMPRESSED, 32-BIT POINTERS. */

#if SIZEOF_SIZE_T == 4

sggc_kind_t sggc_kind (sggc_type_t type, sggc_length_t length)
{ 
    return length > 1 ? type : SGGC_N_TYPES + type;
}

sggc_nchunks_t sggc_nchunks (sggc_type_t type, sggc_length_t length)
{
    switch (type) {
    case RAWSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length)    / SGGC_CHUNK_SIZE;
    case CHARSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + length+1)  / SGGC_CHUNK_SIZE;
    case INTSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case REALSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 8*length)  / SGGC_CHUNK_SIZE;
    case VECSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case STRSXP:
        return (24 + SGGC_CHUNK_SIZE-1 + 4*length)  / SGGC_CHUNK_SIZE;
    case CPLXSXP
        return (24 + SGGC_CHUNK_SIZE-1 + 16*length) / SGGC_CHUNK_SIZE;
    case SYMSXP:
        return 3;
    case SPECIALSXP:
        return 3;
    case BUILTINSXP:
        return 3;
    default:
        return 2;
    }
}

#endif


