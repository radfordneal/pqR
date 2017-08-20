/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2010  The R Core Team
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

#define USE_FAST_PROTECT_MACROS
#define R_USE_SIGNALS 1
#include <Defn.h>


/* STRUCTURE WITH HASH TABLE INFORMATION. */

#define HASH_STATS 0

typedef struct HashData {
    unsigned (*hash)(void*,int);    /* function for computing hashes */
    int (*equal)(void*,int,void*,int); /* function for comparing elements */
    int *table;                     /* hash table */
    size_t size;                    /* size of hash table */
    void *matchvec;                 /* array of elements matched against */
    int nomatch;                    /* index value for "no match" */
    Rboolean useBytes;              /* some string marked as bytes? */
    Rboolean useUTF8;               /* compare strings as UTF8? */
#if HASH_STATS
    unsigned occupied;              /* number of occupied buckets */
    unsigned searches;              /* number of searches done */
    unsigned probs;                 /* number of buckets looked at in searches*/
#endif
} HashData;


/* HASH FUNCTIONS FOR VARIOUS TYPES.  Arguments are pointer to data array
   index (from 0) of element to hash. */

#define HASH32U(u) ( ((u) + ((u) >> 5)) + (((u) << 3) + ((u) >> 16)) )

static unsigned lhash (void *data, int indx)
{
    int *x = data;
    return x[indx] == NA_LOGICAL ? 2 : x[indx];
}

static unsigned ihash (void *data, int indx)
{
    int *x = data;
    unsigned u = x[indx];
    return HASH32U(u);
}

union foo { double d; unsigned int u[2]; };

static unsigned rhash (void *data, int indx)
{
    double *x = data;

    /* There is a problem with signed 0s under IEEE */
    double tmp = x[indx] == 0.0 ? 0.0 : x[indx];

    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp)) tmp = NA_REAL;
    else if (R_IsNaN(tmp)) tmp = R_NaN;

    /* need to use both 32-byte chunks or endianness is an issue */
    union foo tmpu;
    tmpu.d = tmp;
    unsigned u = tmpu.u[0] + tmpu.u[1];
    return HASH32U(u);
}

static unsigned chash (void *data, int indx)
{
    Rcomplex *x = data;

    Rcomplex tmp;
    tmp.r = x[indx].r == 0.0 ? 0.0 : x[indx].r;
    tmp.i = x[indx].i == 0.0 ? 0.0 : x[indx].i;

    /* we want all NaNs except NA equal, and all NAs equal */
    if (R_IsNA(tmp.r)) tmp.r = NA_REAL;
    else if (R_IsNaN(tmp.r)) tmp.r = R_NaN;
    if (R_IsNA(tmp.i)) tmp.i = NA_REAL;
    else if (R_IsNaN(tmp.i)) tmp.i = R_NaN;

    union foo tmpu;
    tmpu.d = tmp.r;
    unsigned u;
    u = tmpu.u[0] + tmpu.u[1];
    tmpu.d = tmp.i;
    u ^= tmpu.u[0] + tmpu.u[1];
    return HASH32U(u);
}

static unsigned shash (void *data, int indx)
{
    SEXP *x = data;
    uint32_t u;
#   if USE_COMPRESSED_POINTERS
        u = (uint32_t) x[indx];
#   elif  SIZEOF_CHAR_P == 4
        u = ((uint32_t) x[indx]) >> 5;
#   else
    {   uint64_t u64 = ((uint64_t) (uintptr_t) x[indx]) >> 5;
        u = (uint32_t) (u64 >> 32) + (uint32_t) u64;
    }
#   endif
    return HASH32U(u);
}

static unsigned shash_UTF8 (void *data, int indx)
{
    SEXP *x = data;
    const void *vmax = VMAXGET();
    const char *p = translateCharUTF8(x[indx]);
    unsigned u = Rf_char_hash(p);
    VMAXSET(vmax); /* discard any memory used by translateChar */

    return HASH32U(u);
}

static unsigned rawhash (void *data, int indx)
{
    Rbyte *x = data;
    return x[indx];
}

static unsigned vhash (void *dat, int indx)
{
    int i;
    unsigned int key;
    SEXP *x = dat;
    SEXP this = x[indx];
    void *data = DATAPTR(this);

    key = OBJECT(this) + 2*TYPEOF(this) + 100*length(this);
    /* maybe we should also look at attributes, but that slows us down */
    switch (TYPEOF(this)) {
    case LGLSXP:
	/* This is not too clever: pack into 32-bits and then scatter? */
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= lhash (data, i);
	    key *= 97;
	}
	break;
    case INTSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= ihash (data, i);
	    key *= 97;
	}
	break;
    case REALSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= rhash (data, i);
	    key *= 97;
	}
	break;
    case CPLXSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= chash (data, i);
	    key *= 97;
	}
	break;
    case STRSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= shash (data, i);
	    key *= 97;
	}
	break;
    case RAWSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= rawhash (data, i);
	    key *= 97;
	}
	break;
    case VECSXP:
    case EXPRSXP:
	for(i = 0; i < LENGTH(this); i++) {
	    key ^= vhash (data, i);
	    key *= 97;
	}
	break;
    default:
	break;
    }
    return key;
}


/* EQUALITY COMPARISON FOR VARIOUS TYPES.  Arguments are two pointers to
   data arrays and two indexes (from 0) within them of the elements to 
   compare. */

static int lequal (void *di, int i, void *dj, int j)
{
    int *x = di, *y = dj;
    return x[i] == y[j];
}


static int iequal (void *di, int i, void *dj, int j)
{
    int *x = di, *y = dj;
    return x[i] == y[j];
}

/* BDR 2002-1-17  We don't want NA and other NaNs to be equal */
static int requal (void *di, int i, void *dj, int j)
{
    double *x = di, *y = dj;
    if (!ISNAN(x[i]) && !ISNAN(y[j]))
	return x[i] == y[j];
    else if (R_IsNA(x[i]) && R_IsNA(y[j]))
        return 1;
    else if (R_IsNaN(x[i]) && R_IsNaN(y[j]))
        return 1;
    else
        return 0;
}

static int cplx_eq(Rcomplex x, Rcomplex y)
{
    if (!ISNAN(x.r) && !ISNAN(x.i) && !ISNAN(y.r) && !ISNAN(y.i))
	return x.r == y.r &&  x.i == y.i;
    else if ((R_IsNA(x.r) || R_IsNA(x.i)) && (R_IsNA(y.r) || R_IsNA(y.i)))
	return 1;
    else if ((R_IsNaN(x.r) || R_IsNaN(x.i)) && (R_IsNaN(y.r) || R_IsNaN(y.i)))
	return 1;
    else
	return 0;
}

static int cequal (void *di, int i, void *dj, int j)
{
    Rcomplex *x = di, *y = dj;
    return cplx_eq(x[i], y[j]);
}

static int sequal (void *di, int i, void *dj, int j)
{
    SEXP *x = di, *y = dj;
    SEXP xs = x[i];
    SEXP ys = y[j];
    /* Two strings which have the same address must be the same,
       so avoid looking at the contents */
    if (xs == ys)
        return 1;
    /* Then if either is NA the other cannot be */
    if (xs == NA_STRING || ys == NA_STRING)
	return 0;
    return SEQL(xs,ys);
}

static int rawequal (void *di, int i, void *dj, int j)
{
    Rbyte *x = di, *y = dj;
    return x[i] == y[j];
}

static int vequal (void *di, int i, void *dj, int j)
{
    SEXP *x = di, *y = dj;
    return R_compute_identical (x[i], y[j], 0);
}


/* HASH TABLE INITIALIZATION. */

static size_t hash_size (R_len_t len)
{
    size_t i = 32;  /* minimum size (unless overridden later) */
    while (i < len) i <<= 1;
    return (i<<1) == 0 ? i : i<<1;
}

static void HashTableSetup (SEXP x, HashData *d)
{
    if (!isVector(x))
        UNIMPLEMENTED_TYPE("HashTableSetup", x);

    d->size = hash_size(LENGTH(x));

    switch (TYPEOF(x)) {
    case LGLSXP:
        d->hash = lhash;
        d->equal = lequal;
        d->size = 4;
        break;
    case INTSXP:
        d->hash = ihash;
        d->equal = iequal;
        break;
    case REALSXP:
        d->hash = rhash;
        d->equal = requal;
        break;
    case CPLXSXP:
        d->hash = chash;
        d->equal = cequal;
        break;
    case STRSXP:
        d->hash = shash;
        d->equal = sequal;
        break;
    case RAWSXP:
        d->hash = rawhash;
        d->equal = rawequal;
        break;
    case VECSXP:
    case EXPRSXP:
        d->hash = vhash;
        d->equal = vequal;
        break;
    default:
        abort();
    }

    d->table = (int *) R_alloc (d->size, sizeof *d->table);
    for (unsigned i = 0; i < d->size; i++) d->table[i] = 0;

    d->matchvec = DATAPTR(x);
    d->nomatch = 0;
    d->useBytes = FALSE;
    d->useUTF8 = FALSE;
}


static void check_UTF8 (SEXP x, Rboolean *useBytes, Rboolean *useUTF8)
{
    if (TYPEOF(x) != STRSXP || *useBytes)
        return;

    R_len_t len_x = LENGTH(x);
    R_len_t i;

    for (i = 0; i < len_x; i++) {
        SEXP s = STRING_ELT(x, i);
        if (IS_BYTES(s)) {
            *useBytes = TRUE;
            *useUTF8 = FALSE;
            return;
        }
        if (ENC_KNOWN(s)) {
            *useUTF8 = TRUE;
        }
    }
}


/* HASH TABLE OPERATIONS. */

static inline int hash_insert (SEXP x, int indx, HashData *d)
{
    void *vec = DATAPTR(x);
    void *tbl = d->matchvec;
    unsigned i;

#   if HASH_STATS
        d->searches += 1;
#   endif
    i = d->hash (vec, indx);
    for (;;) {
        i &= d->size-1;
#       if HASH_STATS
            d->probes += 1;
            if (d->table[i] == 0) d->occupied += 1;
#       endif
        if (d->table[i] <= 0) {
            d->table[i] = indx + 1;
            return 0;
        }
        if (d->equal (tbl, d->table[i]-1, vec, indx)) {
            return d->table[i];
        }
        i += 1;
    }
}


static inline int hash_lookup (SEXP x, int indx, HashData *d)
{
    void *vec = DATAPTR(x);
    void *tbl = d->matchvec;
    unsigned i;

#   if HASH_STATS
        d->searches += 1;
#   endif
    i = d->hash (vec, indx);
    for (;;) {
        i &= d->size-1;
#       if HASH_STATS
            d->probes += 1;
#       endif
        if (d->table[i] == 0) {
            return d->nomatch;
        }
        if (d->table[i] > 0 && d->equal (tbl, d->table[i]-1, vec, indx)) {
            return d->table[i];
        }
        i += 1;
    }
}


static inline void hash_remove (SEXP x, int indx, HashData *d)
{
    void *vec = DATAPTR(x);
    void *tbl = d->matchvec;
    unsigned i;

#   if HASH_STATS
        d->searches += 1;
#   endif
    i = d->hash (vec, indx);
    for (;;) {
        i &= d->size-1;
#       if HASH_STATS
            d->probes += 1;
#       endif
        if (d->table[i] == 0)
            return;
        if (d->table[i] > 0 && d->equal (tbl, d->table[i]-1, vec, indx)) {
            d->table[i] = -d->table[i];
            return;
        }
        i += 1;
    }
}


#define DUPLICATED_INIT						\
    const void *vmax = VMAXGET();				\
    int i, n;							\
    HashData data;						\
        							\
    if (!isVector(x))						\
        error(_("'duplicated' applies only to vectors"));	\
        							\
    n = LENGTH(x);						\
    HashTableSetup(x, &data);					\
    check_UTF8(x, &data.useBytes, &data.useUTF8);		\
    if (data.useUTF8)						\
        data.hash = shash_UTF8;					\

#define DUPLICATED_FINI VMAXSET(vmax)

SEXP duplicated(SEXP x, Rboolean from_last)
{
    SEXP ans;
    int *v;

    DUPLICATED_INIT;

    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if(from_last)
	for (i = n-1; i >= 0; i--) v[i] = hash_insert (x, i, &data) != 0;
    else
	for (i = 0; i < n; i++) v[i] = hash_insert (x, i, &data) != 0;

    UNPROTECT(1);
    DUPLICATED_FINI;
    return ans;
}

/* simpler version of the above : return 1-based index of first, or 0 : */
int any_duplicated(SEXP x, Rboolean from_last)
{
    int result = 0;
    DUPLICATED_INIT;

    if (from_last) {
        for (i = n-1; i >= 0; i--) {
            if (hash_insert (x, i, &data)) { result = i+1; break; }
        }
    }
    else {
        for (i = 0; i < n; i++) {
            if (hash_insert (x, i, &data)) { result = i+1; break; }
        }
    }

    DUPLICATED_FINI;
    return result;
}

SEXP duplicated3(SEXP x, SEXP incomp, Rboolean from_last)
{
    SEXP ans;
    int *v, j, m;

    DUPLICATED_INIT;

    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    if (from_last)
        for (i = n-1; i >= 0; i--) v[i] = hash_insert (x, i, &data) != 0;
    else
        for (i = 0; i < n; i++) v[i] = hash_insert (x, i, &data) != 0;

    if (length(incomp)) {
        PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
        m = length(incomp);
        for (i = 0; i < n; i++) {
            if (v[i]) {
                for (j = 0; j < m; j++)
                    if (data.equal (DATAPTR(x), i, DATAPTR(incomp), j)) { 
                        v[i] = 0;
                        break;
                    }
            }
        }
        UNPROTECT(1);
    }
    UNPROTECT(1);
    DUPLICATED_FINI;
    return ans;
}

/* return (1-based) index of first duplication, or 0 : */
int any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last)
{
    int j, m;

    m = length(incomp);
    if (m == 0)
	error(_("any_duplicated3(., <0-length incomp>)"));

    DUPLICATED_INIT;

    PROTECT(incomp = coerceVector(incomp, TYPEOF(x)));
    m = length(incomp);

    if (from_last)
	for (i = n-1; i >= 0; i--) {
#define IS_DUPLICATED_CHECK				\
	    if (hash_insert (x, i, &data)) {		\
		Rboolean isDup = TRUE;			\
		for (j = 0; j < m; j++)			\
		    if (data.equal (DATAPTR(x), i, DATAPTR(incomp), j)) { \
			isDup = FALSE;			\
                        break;				\
		    }					\
		if (isDup) {				\
		    UNPROTECT(1);			\
                    DUPLICATED_FINI;			\
		    return i+1;				\
 	        }					\
		/* else continue */			\
	    }
	    IS_DUPLICATED_CHECK;
	}
    else {
	for (i = 0; i < n; i++)
            IS_DUPLICATED_CHECK;
    }

    UNPROTECT(1);
    DUPLICATED_FINI;
    return 0;
}

#undef IS_DUPLICATED_CHECK
#undef DUPLICATED_INIT
#undef DUPLICATED_FINI


/* .Internal(duplicated(x))       [op=0]
   .Internal(unique(x))	          [op=1]
   .Internal(anyDuplicated(x))    [op=2]
*/
static SEXP do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, incomp, dup, ans;
    int i, k, n, fromLast;

    checkArity(op, args);
    x = CAR(args);
    incomp = CADR(args);
    if (length(CADDR(args)) < 1)
	error(_("'fromLast' must be length 1"));
    fromLast = asLogical(CADDR(args));
    if (fromLast == NA_LOGICAL)
	error(_("'fromLast' must be TRUE or FALSE"));

    /* handle zero length vectors, and NULL */
    if ((n = length(x)) == 0)
	return(PRIMVAL(op) <= 1
	       ? allocVector(PRIMVAL(op) != 1 ? LGLSXP : TYPEOF(x), 0)
	       : ScalarIntegerMaybeConst(0));

    if (!isVector(x)) {
	error(_("%s() applies only to vectors"),
	      (PRIMVAL(op) == 0 ? "duplicated" :
	       (PRIMVAL(op) == 1 ? "unique" : /* 2 */ "anyDuplicated")));
    }

    if(length(incomp) && /* S has FALSE to mean empty */
       !(isLogical(incomp) && length(incomp) == 1 && LOGICAL(incomp)[0] == 0)) {
	if(PRIMVAL(op) == 2) /* return R's 1-based index :*/
	    return ScalarIntegerMaybeConst (any_duplicated3(x,incomp,fromLast));
	else
	    dup = duplicated3(x, incomp, fromLast);
    }
    else {
	if(PRIMVAL(op) == 2)
	    return ScalarIntegerMaybeConst(any_duplicated(x, fromLast));
	else
	    dup = duplicated(x, fromLast);
    }
    if (PRIMVAL(op) == 0) /* "duplicated()" */
	return dup;
    /*	ELSE
	use the results of "duplicated" to get "unique" */

    /* count unique entries */
    k = 0;
    for (i = 0; i < n; i++)
	if (LOGICAL(dup)[i] == 0)
	    k++;

    PROTECT(dup);
    PROTECT(ans = allocVector(TYPEOF(x), k));

    k = 0;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		INTEGER(ans)[k++] = INTEGER(x)[i];
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		REAL(ans)[k++] = REAL(x)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0) {
		COMPLEX(ans)[k].r = COMPLEX(x)[i].r;
		COMPLEX(ans)[k].i = COMPLEX(x)[i].i;
		k++;
	    }
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		SET_STRING_ELT(ans, k++, STRING_ELT(x, i));
	break;
    case VECSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		SET_VECTOR_ELT(ans, k++, VECTOR_ELT(x, i));
	break;
    case RAWSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		RAW(ans)[k++] = RAW(x)[i];
	break;
    default:
	UNIMPLEMENTED_TYPE("duplicated", x);
    }
    UNPROTECT(2);
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table, HashData *d)
{
    int i, n;

    n = LENGTH(table);

    for (i = 0; i < n; i++)
	(void) hash_insert (table, i, d);
}

/* invalidate entries */
static void UndoHashing (SEXP x, HashData *d)
{
    for (int i = 0; i < LENGTH(x); i++) hash_remove (x, i, d);
}

/* Now do the table lookup */
static SEXP HashLookup (SEXP x, HashData *d)
{
    R_len_t i, n;
    SEXP ans;

    n = LENGTH(x);
    PROTECT(ans = allocVector(INTSXP, n));

    for (i = 0; i < n; i++) {
	INTEGER(ans)[i] = hash_lookup (x, i, d);
    }

    UNPROTECT(1);
    return ans;
}

static SEXP match_transform(SEXP s, SEXP env)
{
    if (!OBJECT(s))
        return s;

    if (inherits_CHAR (s, R_factor_CHARSXP))
        return asCharacterFactor(s);

    if (inherits(s, "POSIXlt")) { /* and maybe more classes in the future:
       				     Call R's (generic)  as.character(s) : */
        SEXP call, r;
        PROTECT(call = lang2(install("as.character"), s));
        PROTECT(r = eval(call, env));
        UNPROTECT(2);
        return r;
    }

    return s;
}

/* Main function used to implement 'match(x,table,nomatch,imcomp)' and
   'x %in% table'.  The inop argument is 0 for match, 1 for %in% without
   variant return, 2 for all(x %in% table), 3 for any(x %in% table),
   and 4 for sum(x %in% table). */

static SEXP matchfun (SEXP itable, SEXP ix, int nomatch, SEXP incomp, SEXP env,
                      int inop)
{
    SEXPTYPE type;
    HashData data;
    int i, n;

    BEGIN_PROTECT3 (x, table, ans);
    ALSO_PROTECT1 (incomp);

    n = length(ix);

    /* handle zero length arguments */

    if (n == 0) 
        RETURN_SEXP_INSIDE_PROTECT (allocVector (inop ? LGLSXP : INTSXP, 0));

    if (length(itable) == 0) {
        if (inop) {
            ans = allocVector (LGLSXP, n);
            for (i = 0; i < n; i++) LOGICAL(ans)[i] = 0;
        }
        else {
            ans = allocVector (INTSXP, n);
            for (i = 0; i < n; i++) INTEGER(ans)[i] = nomatch;
        }
        RETURN_SEXP_INSIDE_PROTECT (ans);
    }

    x = match_transform (ix, env);
    table = match_transform (itable, env);

    /* Coerce to a common type; type == NILSXP is ok here.
     * Note that above we coerce factors and "POSIXlt", only to character.
     * Hence, coerce to character or to `higher' type
     * (given that we have "Vector" or NULL) */
    if(TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP)
	type = STRSXP;
    else type = TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
    x = coerceVector (x, type); 
    table = coerceVector (table, type);
    n = LENGTH(x);

    /* Special case scalar of x -- for speed only.  Taken from R-3.3.0
       (then cleaned up and modified). */

    if (n == 1 && incomp == R_NoObject) {
        R_len_t ilen = LENGTH(itable);
        int result = nomatch;
        switch (type) {
        case STRSXP: {
            SEXP x_val = STRING_ELT(x,0);
            for (int i = 0; i < ilen; i++) {
                if (SEQL(STRING_ELT(table,i), x_val)) {
                    result = i + 1;
                    break;
                }
            }
            break;
        }
        case LGLSXP:
        case INTSXP: {
            int x_val = INTEGER(x)[0];
            int *table_p = INTEGER(table);
            if ((ilen & 1) && table_p[0] == x_val) {
                result = 1;
                break;
            }
            for (int i = ilen & 1; i < ilen; i += 2) {
                int a = table_p[i];
                int b = table_p[i+1];
                if (a == x_val) { result = i+1; break; }
                if (b == x_val) { result = i+2; break; }
            }
            break;
        }
        case REALSXP: {
            double x_val = REAL(x)[0]==0 ? 0 : REAL(x)[0];/* handles signed 0 */
            double *table_p = REAL(table);
            /* we want all NaNs except NA equal, and all NAs equal */
            if (R_IsNA(x_val)) {
                for (int i = 0; i < ilen; i++) {
                    if (R_IsNA(table_p[i])) {
                        result = i + 1;
                        break;
                    }
                }
            }
            else if (R_IsNaN(x_val)) {
                for (int i = 0; i < ilen; i++) {
                    if (R_IsNaN(table_p[i])) {
                        result = i + 1; 
                        break;
                    }
                }
            }
            else {
                if ((ilen & 1) && table_p[0] == x_val) {
                    result = 1;
                    break;
                }
                for (int i = ilen & 1; i < ilen; i += 2) {
                    double a = table_p[i];
                    double b = table_p[i+1];
                    if (a == x_val) { result = i+1; break; }
                    if (b == x_val) { result = i+2; break; }
                }
            }
            break;
        }
        case CPLXSXP: {
            Rcomplex x_val = COMPLEX(x)[0];
            Rcomplex *table_p = COMPLEX(table);
            for (int i = 0; i < ilen; i++) {
                if (cplx_eq(table_p[i], x_val)) {
                    result = i + 1;
                    break;
                }
            }
            break;
        }
        case RAWSXP: {
            Rbyte x_val = RAW(x)[0];
            Rbyte *table_p = RAW(table);
            for (int i = 0; i < ilen; i++) {
                if (table_p[i] == x_val) {
                    result = i + 1;
                    break;
                }
            }
            break;
        }}
        if (inop == 4) /* sum (x %in% y) */
            RETURN_SEXP_INSIDE_PROTECT (ScalarInteger(result!=0));
        else if (inop) /* %in%, not variant */
            RETURN_SEXP_INSIDE_PROTECT (ScalarLogicalMaybeConst(result!=0));
        else
            RETURN_SEXP_INSIDE_PROTECT (ScalarInteger(result));
    }

    if (incomp != R_NoObject) 
        incomp = coerceVector(incomp, type);

    /* Choose between two aproaches depending on which of 'x' and 'table'
       is smaller. */

    if (LENGTH(table) < 1.2*n) {  /* 'table' is hashed, 'x' looked up */

        /* Create a hash table for 'table', then look up elements of 'x'
           in this table to create the result.  A vector for results 
           needn't be created for 'all', 'any', and 'sum' variants, and
           lookups can stop once the result of 'all' or 'any' has been
           determined. */

        HashTableSetup (table, &data);
        data.nomatch = nomatch;
        check_UTF8 (x, &data.useBytes, &data.useUTF8);
        check_UTF8 (table, &data.useBytes, &data.useUTF8);
        if (data.useUTF8)
            data.hash = shash_UTF8;
        DoHashing (table, &data);
        if (incomp != R_NoObject)
            UndoHashing (incomp, &data);
        switch (inop) {
        case 0: { /* match */
            ans = allocVector (INTSXP, n);
            int *ansi = INTEGER(ans);
            for (i = 0; i < n; i++)
                ansi[i] = hash_lookup (x, i, &data);
            break;
        }
        case 2: { /* all (x %in% y) */
            int res = 1;
            for (i = 0; i < n; i++) {
                if (hash_lookup (x, i, &data) == 0) {
                    res = 0;
                    break;
                }
            }
            ans = ScalarLogicalMaybeConst(res);
            break;
        }
        case 3: { /* any (x %in% y) */
            int res = 0;
            for (i = 0; i < n; i++) {
                if (hash_lookup (x, i, &data) != 0) {
                    res = 1;
                    break;
                }
            }
            ans = ScalarLogicalMaybeConst(res);
            break;
        }
        case 4: { /* sum (x %in% y) */
            int res = 0;
            for (i = 0; i < n; i++) {
                if (hash_lookup (x, i, &data) != 0)
                    res += 1;
            }
            ans = ScalarInteger(res);
            break;
        }
        default: { /* %in%, not variant */
            ans = allocVector (LGLSXP, n);
            int *ansl = LOGICAL(ans);
            for (i = 0; i < n; i++)
                ansl[i] = hash_lookup (x, i, &data) != 0;
            break;
        }}
    }

    else {  /* 'x' is hashed, 'table' looked up */

        /* Create a hash table for 'x', then look up elements of
           'table' to see which elements of 'x' exist in 'table'. 
           A vector the length of 'x' must be created to record for
           each duplicate element in 'x' the index of the first
           instance.  When an element of 'table' is found in 'x', the
           entry for the first instance of that element is set to
           minus the index in 'table' (distinguising it from the
           indexes for duplicates).  A final pass then links the
           duplicate instances to the result for the first instance.
           For the 'any' variant, TRUE can be returned when the first 
           lookup succeeds. */

        HashTableSetup(x, &data);
        data.nomatch = 0;  /* NOT nomatch */
        check_UTF8 (x, &data.useBytes, &data.useUTF8);
        check_UTF8 (table, &data.useBytes, &data.useUTF8);
        if (data.useUTF8)
            data.hash = shash_UTF8;
        ans = allocVector (INTSXP, n);
        int *ansi = INTEGER(ans);
        for (i = 0; i < n; i++) 
            ansi[i] = hash_insert (x, i, &data);
        if (incomp != R_NoObject)
            UndoHashing (incomp, &data);

        R_len_t tl = LENGTH(table);
        if (inop == 3) {  /* any (x %in% y) */
            for (i = 0; i < tl; i++) {
                if (hash_lookup (table, i, &data) != 0) {
                    ans = ScalarLogicalMaybeConst(TRUE);
                    goto done;
                }
            }
        }
        else {
            for (i = 0; i < tl; i++) {
                int j = hash_lookup (table, i, &data);
                if (j != 0 && ansi[j-1] == 0)
                    ansi[j-1] = -(i+1);
            }
        }

        switch (inop) {
        case 0: { /* match */
            for (i = 0; i < n; i++)
                ansi[i] = ansi[i] == 0 ? nomatch
                        : ansi[i] > 0  ? ansi[ansi[i]-1]
                        :                -ansi[i];
            break;
        }
        case 2: { /* all (x %in% y) */
            int res = 1;
            for (i = 0; i < n; i++) {
                if (ansi[i] == 0 || ansi[i] > 0 && ansi[ansi[i]-1] == 0) {
                    res = 0;
                    break;
                }
            }
            ans = ScalarLogicalMaybeConst(res);
            break;
        }
        case 3: { /* any (x %in% y) */
            /* If any are in y, TRUE would have been returned above. */
            ans = ScalarLogicalMaybeConst(FALSE);
            break;
        }
        case 4: { /* sum (x %in% y) */
            int res = 0;
            for (i = 0; i < n; i++) {
                if (ansi[i] != 0 && (ansi[i] < 0 || ansi[ansi[i]-1] != 0))
                    res += 1;
            }
            ans = ScalarInteger(res);
            break;
        }
        default: { /* %in%, not variant */
            for (i = 0; i < n; i++)
                ansi[i] = ansi[i]==0 ? 0 : ansi[i]<0 ? 1 : ansi[ansi[i]-1] != 0;
            TYPEOF(ans) = LGLSXP;
            break;
        }}
    }

  done: 
    
#   if HASH_STATS
        if (installed_already("HASH_STATS")) {
            REprintf(
              "HASH_STATS: size %d, occupied %d\, searches %d, probes %d\n",
               data.size, data.occupied, data.searches, data.probes);
        }
#   endif

    RETURN_SEXP_INSIDE_PROTECT (ans);
    END_PROTECT;
}

SEXP match5(SEXP itable, SEXP ix, int nomatch, SEXP incomp, SEXP env)
{ 
    matchfun (itable, ix, nomatch, incomp, env, 0);
}

SEXP matchE(SEXP itable, SEXP ix, int nomatch, SEXP env)
{
    return matchfun (itable, ix, nomatch, R_NoObject, env, 0);
}

/* used from other code, not here: */
SEXP match(SEXP itable, SEXP ix, int nomatch)
{
    return matchfun (itable, ix, nomatch, R_NoObject, R_BaseEnv, 0);
}


static SEXP do_match(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int nomatch, nargs = length(args);
    SEXP incomp;

    if (PRIMVAL(op) == 0) { /* match */
        /* checkArity(op, args); too many packages have 3 from R < 2.7.0 */
        if (nargs < 3 || nargs > 4)
            error("%d arguments passed to .Internal(%s) which requires %d",
                  length(args), PRIMNAME(op), PRIMARITY(op));
        if (nargs == 3)
            warning("%d arguments passed to .Internal(%s) which requires %d",
                    length(args), PRIMNAME(op), PRIMARITY(op));
    
        if ((!isVector(CAR(args)) && !isNull(CAR(args)))
            || (!isVector(CADR(args)) && !isNull(CADR(args))))
            error(_("'match' requires vector arguments"));
        nomatch = asInteger(CADDR(args));
        incomp = nargs < 4 ? R_NilValue : CADDDR(args);
    }
    else { /* %in% */
        checkArity (op, args);
        if ((!isVector(CAR(args)) && !isNull(CAR(args)))
            || (!isVector(CADR(args)) && !isNull(CADR(args))))
            error(_("'match' requires vector arguments"));
        nomatch = 0;
        incomp = R_NilValue;
    }

    int inop = PRIMVAL(op) == 1;  /* is this for %in% operator? */
    if (inop == 0 && length(incomp) 
         && /* S has FALSE to mean empty */
            !(isLogical(incomp) && length(incomp)==1 && LOGICAL(incomp)[0]==0))
        return matchfun (CADR(args), CAR(args), nomatch, incomp, env, 0);
    else {
        int vk = VARIANT_KIND(variant);
        return matchfun (CADR(args), CAR(args), nomatch, R_NoObject, env, 
                         !inop ? 0 : vk == VARIANT_AND ? 2
                                   : vk == VARIANT_OR ? 3
                                   : vk == VARIANT_SUM ? 4
                                   : 1);
    }
}

/* Partial Matching of Strings */
/* Fully S Compatible version. */

/* Hmm, this was not all S compatible!  The desired behaviour is:
 * First do exact matches, and mark elements as used as they are matched
 *   unless dup_ok is true.
 * Then do partial matching, from left to right, using up the table
 *   unless dup_ok is true.  Multiple partial matches are ignored.
 * Empty strings are unmatched                        BDR 2000/2/16
 */

static SEXP do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    int i, j,  mtch, n_input, n_target, mtch_count, dups_ok, no_match;
    size_t temp;
    int nexact = 0;
    int *used = NULL, *ians;
    const char **in, **tar;
    Rboolean no_dups;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;

    checkArity(op, args);
    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);
    no_match = asInteger(CADDR(args));
    dups_ok = asLogical(CADDDR(args));
    if (dups_ok == NA_LOGICAL)
	error(_("invalid '%s' argument"), "duplicates.ok");
    no_dups = !dups_ok;

    if (!isString(input) || !isString(target))
	error(_("argument is not of mode character"));

    if(no_dups) {
	used = (int *) R_alloc((size_t) n_target, sizeof(int));
	for (j = 0; j < n_target; j++) used[j] = 0;
    }

    check_UTF8 (input, &useBytes, &useUTF8);
    check_UTF8 (target, &useBytes, &useUTF8);

    in = (const char **) R_alloc((size_t) n_input, sizeof(char *));
    tar = (const char **) R_alloc((size_t) n_target, sizeof(char *));
    PROTECT(ans = allocVector(INTSXP, n_input));
    ians = INTEGER(ans);
    if(useBytes) {
	for(i = 0; i < n_input; i++) {
	    in[i] = CHAR(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(j = 0; j < n_target; j++)
	    tar[j] = CHAR(STRING_ELT(target, j));
    }
    else if(useUTF8) {
	for(i = 0; i < n_input; i++) {
	    in[i] = translateCharUTF8(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(j = 0; j < n_target; j++)
	    tar[j] = translateCharUTF8(STRING_ELT(target, j));
    }
    else {
	for(i = 0; i < n_input; i++) {
	    in[i] = translateChar(STRING_ELT(input, i));
	    ians[i] = 0;
	}
	for(j = 0; j < n_target; j++)
	    tar[j] = translateChar(STRING_ELT(target, j));
    }

    /* First pass, exact matching */
    if(no_dups) {
	for (i = 0; i < n_input; i++) {
	    const char *ss = in[i];
	    if (strlen(ss) == 0) continue;
	    for (j = 0; j < n_target; j++) {
		if (used[j]) continue;
		if (strcmp(ss, tar[j]) == 0) {
		    if(no_dups) used[j] = 1;
		    ians[i] = j + 1;
		    nexact++;
		    break;
		}
	    }
	}
    }
    else {
	/* only worth hashing if enough lookups will be done:
	   since the tradeoff involves memory as well as time
	   it is not really possible to optimize there.
	 */
	if ((n_target > 100) && (10*n_input > n_target)) {
	    /* <FIXME>
	       Currently no hashing when using bytes.
	       </FIXME>
	    */
            void *vmax = VMAXGET();
	    HashData data;
	    HashTableSetup(target, &data);
	    DoHashing(target, &data);
	    for (i = 0; i < n_input; i++) {
		/* we don't want to lookup "" */
		if (strlen(in[i]) == 0) continue;
		ians[i] = hash_lookup (input, i, &data);
		if(ians[i]) nexact++;
	    }
            VMAXSET(vmax);
	} else {
	    for (i = 0; i < n_input; i++) {
		const char *ss = in[i];
		if (strlen(ss) == 0) continue;
		for (j = 0; j < n_target; j++)
		    if (strcmp(ss, tar[j]) == 0) {
			ians[i] = j + 1;
			nexact++;
			break;
		    }
	    }
	}
    }

    if (nexact < n_input) {
	/* Second pass, partial matching */
	for (i = 0; i < n_input; i++) {
	    const char *ss;
	    if (ians[i]) continue;
	    ss = in[i];
	    temp = strlen(ss);
	    if (temp == 0) continue;
	    mtch = 0;
	    mtch_count = 0;
	    for (j = 0; j < n_target; j++) {
		if (no_dups && used[j]) continue;
		if (strncmp(ss, tar[j], temp) == 0) {
		    mtch = j + 1;
		    mtch_count++;
		}
	    }
	    if (mtch > 0 && mtch_count == 1) {
		if(no_dups) used[mtch - 1] = 1;
		ians[i] = mtch;
	    }
	}
	/* Third pass, set no matches */
	for (i = 0; i < n_input; i++)
	    if(ians[i] == 0) ians[i] = no_match;

    }
    UNPROTECT(1);
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

static SEXP do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    Rboolean perfect;
    int i, j, k, imatch, n_input, n_target, no_match, *ians;
    size_t temp;
    const char *ss, *st;
    Rboolean useBytes = FALSE, useUTF8 = FALSE;
    const void *vmax;

    checkArity(op, args);

    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);

    if (!isString(input) || !isString(target))
	error(_("argument is not of mode character"));
    no_match = asInteger(CADDR(args));

    check_UTF8 (input, &useBytes, &useUTF8);
    check_UTF8 (target, &useBytes, &useUTF8);

    PROTECT(ans = allocVector(INTSXP, n_input));
    ians = INTEGER(ans);

    vmax = VMAXGET();
    for(i = 0; i < n_input; i++) {
	if(useBytes)
	    ss = CHAR(STRING_ELT(input, i));
	else if(useUTF8)
	    ss = translateCharUTF8(STRING_ELT(input, i));
	else
	    ss = translateChar(STRING_ELT(input, i));
	temp = strlen(ss);
	imatch = NA_INTEGER;
	perfect = FALSE;
	/* we could reset vmax here too: worth it? */
	for(j = 0; j < n_target; j++) {
	    if(useBytes)
		st = CHAR(STRING_ELT(target, j));
	    else if(useUTF8)
		st = translateCharUTF8(STRING_ELT(target, j));
	    else
		st = translateChar(STRING_ELT(target, j));
	    k = strncmp(ss, st, temp);
	    if (k == 0) {
		if (strlen(st) == temp) {
		    if (perfect)
			imatch = 0;
		    else {
			perfect = TRUE;
			imatch = j + 1;
		    }
		}
		else if (!perfect) {
		    if (imatch == NA_INTEGER)
			imatch = j + 1;
		    else
			imatch = 0;
		}
	    }
	}
	ians[i] = (imatch == NA_INTEGER) ? no_match : imatch;
	VMAXSET(vmax);
    }
    UNPROTECT(1);
    return ans;
}


/* Functions for matching the supplied arguments to the */
/* formal arguments of functions.  The returned value */
/* is a list with all components named. */

#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)

static SEXP StripUnmatched(SEXP s)
{
    if (s == R_NilValue) return s;

    if (CAR(s) == R_MissingArg && !ARGUSED(s) ) {
	return StripUnmatched(CDR(s));
    }
    else if (CAR(s) == R_DotsSymbol ) {
	return StripUnmatched(CDR(s));
    }
    else {
	SETCDR(s, StripUnmatched(CDR(s)));
	return s;
    }
}

static SEXP ExpandDots(SEXP s, int expdots)
{
    SEXP r;
    if (s == R_NilValue)
	return s;
    if (TYPEOF(CAR(s)) == DOTSXP ) {
	SET_TYPEOF(CAR(s), LISTSXP);	/* a safe mutation */
	if (expdots) {
	    r = CAR(s);
	    while (CDR(r) != R_NilValue ) {
		SET_ARGUSED(r, 1);
		r = CDR(r);
	    }
	    SET_ARGUSED(r, 1);
	    SETCDR(r, ExpandDots(CDR(s), expdots));
	    return CAR(s);
	}
    }
    else
	SET_ARGUSED(s, 0);
    SETCDR(s, ExpandDots(CDR(s), expdots));
    return s;
}

static SEXP subDots(SEXP rho)
{
    SEXP rval, dots, a, b, t;
    int len,i;

    dots = findVar(R_DotsSymbol, rho);

    if (dots == R_UnboundValue)
	error(_("... used in a situation where it doesn't exist"));

    if (dots == R_MissingArg)
	return dots;

    len = length(dots);
    PROTECT(dots);
    PROTECT(rval=allocList(len));
    for(a=dots, b=rval, i=1; i<=len; a=CDR(a), b=CDR(b), i++) {
	SET_TAG(b, TAG(a));
	t = CAR(a);
	while (TYPEOF(t) == PROMSXP)
	    t = PREXPR(t);
	if (isSymbol(t) || isLanguage(t)) {
            char tbuf[14];
            tbuf[0] = tbuf[1] = '.';
            integer_to_string(tbuf+2,i);
	    SETCAR(b,install(tbuf));
        }
	else
	    SETCAR(b, t);
    }
    UNPROTECT(2);
    return rval;
}


static SEXP do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, b, rval, sysp, t1, t2, tail;
    RCNTXT *cptr;
    int expdots;

    checkArity(op,args);

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP && LENGTH(funcall) > 0)
	funcall = VECTOR_ELT(funcall, 0);

    if (TYPEOF(funcall) != LANGSXP)
	error(_("invalid '%s' argument"), "call");

    /* Get the function definition */
    sysp = R_GlobalContext->sysparent;

    if (TYPEOF(CAR(args)) == NILSXP) {
	/* Get the env that the function containing */
	/* matchcall was called from. */
	cptr = R_GlobalContext;
	while (cptr != NULL) {
	    if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == sysp)
		break;
	    cptr = cptr->nextcontext;
	}
	if (cptr == NULL)
	    errorcall(R_NilValue,
		      "match.call() was called from outside a function");
	sysp = cptr->sysparent;
	if (cptr != NULL)
	    /* Changed to use the function from which match.call was
	       called as recorded in the context.  This change is
	       needed in case the current function is computed in a
	       way that cannot be reproduced by a second computation,
	       or if it is a registered S3 method that is not
	       lexically visible at the call site.

	       There is one particular case where this represents a
	       change from previous semantics: The definition is NULL,
	       the call is supplied explicitly, and the function in
	       the call is NOT the current function.  The new behavior
	       is to ignore the function in the call and use the
	       current function.  This is consistent with (my reading
	       of) the documentation in both R and Splus.  However,
	       the old behavior of R was consistent with the behavior
	       of Splus (and inconsistent with the documentation in
	       both cases).

	       The previous semantics for this case can be restored by
	       having the .Internal receive an additional argument
	       that indicates whether the call was supplied explicitly
	       or missing, and using the function recorded in the
	       context only if the call was not supplied explicitly.
	       The documentation should also be changed to be
	       consistent with this behavior.  LT */
	    PROTECT(b = duplicate(cptr->callfun));
	else if ( TYPEOF(CAR(funcall)) == SYMSXP )
	    PROTECT(b = findFun(CAR(funcall), sysp));
	else
	    PROTECT(b = eval(CAR(funcall), sysp));

	if (TYPEOF(b) != CLOSXP)
	    error(_("unable to find a closure from within which 'match.call' was called"));

    }
    else {
	/* It must be a closure! */
	PROTECT(b = CAR(args));
	if (TYPEOF(b) != CLOSXP)
	    error(_("invalid '%s' argument"), "definition");
    }

    /* Do we expand ... ? */

    expdots = asLogical(CAR(CDDR(args)));
    if (expdots == NA_LOGICAL)
	error(_("invalid '%s' argument"), "expand.dots");

    /* Get the formals and match the actual args */

    formals = FORMALS(b);
    PROTECT(actuals = duplicate(CDR(funcall)));

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

    t2 = R_MissingArg;
    for (t1=actuals ; t1!=R_NilValue ; t1 = CDR(t1) ) {
	if (CAR(t1) == R_DotsSymbol) {
		t2 = subDots(sysp);
		break;
	}
    }
    /* now to splice t2 into the correct spot in actuals */
    if (t2 != R_MissingArg ) {	/* so we did something above */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = listAppend(t2, CDR(actuals));
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, t2);
		    listAppend(actuals,tail);
		    break;
		}
	    }
	}
    } else { /* get rid of it */
	if( CAR(actuals) == R_DotsSymbol ) {
	    UNPROTECT(1);
	    actuals = CDR(actuals);
	    PROTECT(actuals);
	}
	else {
	    for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
		if( CADR(t1) == R_DotsSymbol ) {
		    tail = CDDR(t1);
		    SETCDR(t1, tail);
		    break;
		}
	    }
	}
    }
    rlist = matchArgs(formals, NULL, 0, actuals, call);

#if 0  /* No longer needed, since matchArgs attaches the tags itself.  */

    /* Attach the argument names as tags */

    for (f = formals, b = rlist; b != R_NilValue; b = CDR(b), f = CDR(f)) {
	SET_TAG(b, TAG(f));
    }
#endif

    /* Handle the dots */

    PROTECT(rlist = ExpandDots(rlist, expdots));

    /* Eliminate any unmatched formals and any that match R_DotSymbol */
    /* This needs to be after ExpandDots as the DOTSXP might match ... */

    rlist = StripUnmatched(rlist);

    PROTECT(rval = allocSExp(LANGSXP));
    SETCAR(rval, duplicate(CAR(funcall)));
    SETCDR(rval, rlist);
    UNPROTECT(4);
    return rval;
}


#include <string.h>
#ifdef _AIX  /*some people just have to be different */
#    include <memory.h>
#endif
/* int and double zeros are all bits off */
#define ZEROINT(X,N,I) do{memset(INTEGER(X),0,(size_t)(N)*sizeof(int));}while(0)
#define ZERODBL(X,N,I) do{memset(REAL(X),0,(size_t)(N)*sizeof(double));}while(0)

SEXP attribute_hidden
Rrowsum_matrix(SEXP x, SEXP ncol, SEXP g, SEXP uniqueg, SEXP snarm)
{
    SEXP matches,ans;
    int i, j, n, p, ng = 0, offset, offsetg, narm;
    HashData data;
    data.nomatch = 0;

    void *vmax = VMAXGET();

    n = LENGTH(g);
    p = INTEGER(ncol)[0];
    ng = length(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup (g, &data));

    PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

    offset = 0; offsetg = 0;

    switch(TYPEOF(x)){
    case REALSXP:
	ZERODBL(ans, ng*p, i);
	for(i = 0; i < p; i++) {
	    for(j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(x)[j+offset]))
		    REAL(ans)[INTEGER(matches)[j]-1+offsetg]
			+= REAL(x)[j+offset];
	    offset += n;
	    offsetg += ng;
	}
	break;
    case INTSXP:
	ZEROINT(ans, ng*p, i);
	for(i = 0; i < p; i++) {
	    for(j = 0; j < n; j++) {
		if (INTEGER(x)[j+offset] == NA_INTEGER) {
		    if(!narm)
			INTEGER(ans)[INTEGER(matches)[j]-1+offsetg]
			    = NA_INTEGER;
		} else if (INTEGER(ans)[INTEGER(matches)[j]-1+offsetg]
			   != NA_INTEGER) {
		    /* check for integer overflows */
		    int itmp = INTEGER(ans)[INTEGER(matches)[j]-1+offsetg];
		    double dtmp = itmp;
		    dtmp += INTEGER(x)[j+offset];
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += INTEGER(x)[j+offset];
		    INTEGER(ans)[INTEGER(matches)[j]-1+offsetg] = itmp;
		}
	    }
	    offset += n;
	    offsetg += ng;
	}
	break;
    default:
	error(_("non-numeric matrix in rowsum(): this cannot happen"));
    }

    UNPROTECT(2); /* ans, matches*/
    VMAXSET(vmax);
    return ans;
}

SEXP attribute_hidden
Rrowsum_df(SEXP x, SEXP ncol, SEXP g, SEXP uniqueg, SEXP snarm)
{
    SEXP matches,ans,col,xcol;
    int i, j, n, p, ng = 0, narm;
    HashData data;
    data.nomatch = 0;

    void *vmax = VMAXGET();

    n = LENGTH(g);
    p = INTEGER(ncol)[0];
    ng = length(uniqueg);
    narm = asLogical(snarm);
    if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

    HashTableSetup(uniqueg, &data);
    DoHashing(uniqueg, &data);
    PROTECT(matches = HashLookup (g, &data));

    PROTECT(ans = allocVector(VECSXP, p));

    for(i = 0; i < p; i++) {
	xcol = VECTOR_ELT(x,i);
	if (!isNumeric(xcol))
	    error(_("non-numeric data frame in rowsum"));
	switch(TYPEOF(xcol)){
	case REALSXP:
	    PROTECT(col = allocVector(REALSXP,ng));
	    ZERODBL(col, ng, i);
	    for(j = 0; j < n; j++)
		if(!narm || !ISNAN(REAL(xcol)[j]))
		    REAL(col)[INTEGER(matches)[j]-1] += REAL(xcol)[j];
	    SET_VECTOR_ELT(ans,i,col);
	    UNPROTECT(1);
	    break;
	case INTSXP:
	    PROTECT(col = allocVector(INTSXP, ng));
	    ZEROINT(col, ng, i);
	    for(j = 0; j < n; j++) {
		if (INTEGER(xcol)[j] == NA_INTEGER) {
		    if(!narm)
			INTEGER(col)[INTEGER(matches)[j]-1] = NA_INTEGER;
		} else if (INTEGER(col)[INTEGER(matches)[j]-1] != NA_INTEGER) {
		    int itmp = INTEGER(col)[INTEGER(matches)[j]-1];
		    double dtmp = itmp;
		    dtmp += INTEGER(xcol)[j];
		    if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
		    else itmp += INTEGER(xcol)[j];
		    INTEGER(col)[INTEGER(matches)[j]-1] = itmp;
		}
	    }
	    SET_VECTOR_ELT(ans, i, col);
	    UNPROTECT(1);
	    break;

	default:
	    error(_("this cannot happen"));
	}
    }
    namesgets(ans, getAttrib(x, R_NamesSymbol));

    UNPROTECT(2); /*ans, matches*/
    VMAXSET(vmax);
    return ans;
}

static SEXP do_makeunique(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP names, sep, ans, newx;
    int i, cnt, *cnts, dp;
    R_len_t n, len, maxlen = 0;
    HashData data;
    const char *csep, *ss;

    void *vmax0 = VMAXGET();

    checkArity(op, args);
    names = CAR(args);
    if(!isString(names))
        error(_("'names' must be a character vector"));
    n = LENGTH(names);
    sep = CADR(args);
    if(!isString(sep) || LENGTH(sep) != 1)
        error(_("'sep' must be a character string"));
    csep = translateChar(STRING_ELT(sep, 0));

    PROTECT(ans = allocVector(STRSXP, n));

    for(i = 0; i < n; i++) {
        SET_STRING_ELT(ans, i, STRING_ELT(names, i));
        len = (R_len_t) strlen(translateChar(STRING_ELT(names, i)));
        if(len > maxlen) maxlen = len;
        VMAXSET(vmax0);
    }
    if (n <= 1) {
        UNPROTECT(1);
        VMAXSET(vmax0);
        return ans;
    }

    /* +20 for terminator and biggest number that could possibly be added */
    char buf[maxlen + (R_len_t) strlen(csep) + 20];
    if (n < 10000)
        cnts = (int *) alloca(((size_t) n) * sizeof(int));
    else {
        /* This is going to be slow so use expensive allocation
           that will be recovered if interrupted. */
        cnts = (int *) R_alloc((size_t) n,  sizeof(int));
    }
    R_CHECKSTACK();
    for (i = 0; i < n; i++) cnts[i] = 1;
    data.nomatch = 0;
    PROTECT(newx = allocVector(STRSXP, 1));

    SEXP dup;

    HashTableSetup(names, &data);
    PROTECT(dup = allocVector(INTSXP, n));
    int *v = INTEGER(dup);
    for (i = 0; i < n; i++) v[i] = hash_insert (names, i, &data);

    char csep_len = strlen(csep);
    void * vmax = VMAXGET();
    for (i = 1; i < n; i++) { /* first cannot be a duplicate */
        dp = INTEGER(dup)[i]; /* 1-based number of first occurrence */
        if (dp == 0)
            continue;
        ss = translateChar(STRING_ELT(names, i));
        strcpy(buf,ss);
        if (strlen(ss)<csep_len || strcmp(csep,ss+strlen(ss)-csep_len)!=0)
            strcat(buf,csep);
        char *end = buf + strlen(buf);
        /* Try appending 1,2,3, ..., n-1 until it is not already in use */
        for(cnt = cnts[dp-1]; ; cnt++) {
            sprintf(end, "%d", cnt);
            SET_STRING_ELT(newx, 0, mkChar(buf));
            if (hash_lookup (newx, 0, &data) == data.nomatch) break;
        }
        SET_STRING_ELT(ans, i, STRING_ELT(newx, 0));
        /* insert it */ (void) hash_insert (ans, i, &data);
        cnts[dp-1] = cnt+1; /* cache the first unused cnt value */
        VMAXSET(vmax);
    }

    UNPROTECT(3);
    VMAXSET(vmax0);
    return ans;
}

/* Use hashing to improve object.size. Here we want equal CHARSXPs,
   not equal contents. */

static int csequal (void *di, int i, void *dj, int j)
{
    SEXP *x = di, *y = dj;
    return x[i] == y[j];
}

static void HashTableSetup1 (SEXP x, HashData *d)
{
    d->hash = shash;
    d->equal = csequal;

    d->size = hash_size(LENGTH(x));

    d->table = (int *) R_alloc (d->size, sizeof *d->table);
    for (unsigned i = 0; i < d->size; i++) d->table[i] = 0;

    d->matchvec = DATAPTR(x);
    d->nomatch = 0;
    d->useBytes = FALSE;
    d->useUTF8 = FALSE;
}

SEXP attribute_hidden csduplicated (SEXP x)
{
    SEXP ans;
    int *h, *v;
    int i, n;
    HashData data;
    const void *vmax = VMAXGET();

    if(TYPEOF(x) != STRSXP)
        error(_("csduplicated not called on a STRSXP"));

    HashTableSetup1(x, &data);

    n = LENGTH(x);
    PROTECT(ans = allocVector(LGLSXP, n));

    v = LOGICAL(ans);

    for (i = 0; i < n; i++)
	v[i] = hash_insert (x, i, &data) != 0;

    UNPROTECT(1);
    VMAXSET(vmax);
    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_unique[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"duplicated",	do_duplicated,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match,	0,	1011,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"%in%",	do_match,	1,	1011,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	do_pmatch,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"charmatch",	do_charmatch,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match.call",	do_matchcall,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	do_makeunique,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
