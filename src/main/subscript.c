/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  https://www.R-project.org/Licenses/
 *
 * EXPORTS:
 *
 *  OneIndex()        -- used for "[[<-" in ./subassign.c
 *  get1index()       -- used for "[["   in ./subassign.c & subset.c
 *  vectorIndex()     -- used for "[[" and "[[<-" with a vector arg

 *  mat2indsub()      -- for "mat[i]"     "    "            "

 *  makeSubscript()   -- for "[" and "[<-" in ./subset.c and ./subassign.c,
 *			 and "[[<-" with a scalar in ./subassign.c
 *  arraySubscript()  -- for "[i,j,..." and "[<-..." in ./subset.c, ./subassign.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

#include <R_ext/Itermacros.h>

/* interval at which to check interrupts, a guess (~subsecond on current hw) */
#define NINTERRUPT 10000000

/* We might get a call with R_NilValue from subassignment code */
#define ECALL(call, yy)     if(call == R_NilValue) error(yy);    else errorcall(call, yy);
#define ECALL3(call, yy, A) if(call == R_NilValue) error(yy, A); else errorcall(call, yy, A);

/* This allows for the unusual case where x is of length 2,
   and x[[-m]] selects one element for m = 1, 2.
   So 'len' is only used if it is 2 and i is negative.
*/
static R_INLINE int integerOneIndex(int i, R_xlen_t len, SEXP call)
{
    int indx = -1;

    if (i > 0) /* a regular 1-based index from R */
	indx = i - 1;
    else if (i == 0 || len < 2) {
	ECALL3(call, _("attempt to select less than one element in %s"), "integerOneIndex");
    } else if (len == 2 && i > -3)
	indx = 2 + i;
    else {
	ECALL3(call, _("attempt to select more than one element in %s"), "integerOneIndex");
    }
    return indx;
}

/* Utility used (only in) do_subassign2_dflt(), i.e. "[[<-" in ./subassign.c : */
R_xlen_t attribute_hidden
OneIndex(SEXP x, SEXP s, R_xlen_t nx, int partial, SEXP *newname,
	 int pos, SEXP call)
{
    SEXP names;
    R_xlen_t i, indx;
    const void *vmax;

    if (pos < 0 && length(s) > 1) {
	ECALL3(call, _("attempt to select more than one element in %s"), "OneIndex");
    }
    if (pos < 0 && length(s) < 1) {
	ECALL3(call, _("attempt to select less than one element in %s"), "OneIndex");
    }

    if(pos < 0) pos = 0;

    indx = -1;
    *newname = R_NilValue;
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	indx = integerOneIndex(INTEGER_ELT(s, pos), nx, call);
	break;
    case REALSXP:
	indx = integerOneIndex((int)REAL_ELT(s, pos), nx, call);
	break;
    case STRSXP:
	vmax = vmaxget();
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    PROTECT(names);
	    /* Try for exact match */
	    for (i = 0; i < nx; i++) {
		const char *tmp = translateChar(STRING_ELT(names, i));
		if (!tmp[0]) continue;
		if (streql(tmp, translateChar(STRING_ELT(s, pos)))) {
		    indx = i;
		    break;
		}
	    }
	    // Try for partial match -- not ever used in current R (partial is 0)
	    if (partial && indx < 0) {
		size_t l = strlen(translateChar(STRING_ELT(s, pos)));
		for(i = 0; i < nx; i++) {
		    const char *tmp = translateChar(STRING_ELT(names, i));
		    if (!tmp[0]) continue;
		    if(!strncmp(tmp, translateChar(STRING_ELT(s, pos)), l)) {
			if(indx == -1 )
			    indx = i;
			else
			    indx = -2;
		    }
		}
	    }
	    UNPROTECT(1); /* names */
	}
	if (indx == -1)
	    indx = nx;
	*newname = STRING_ELT(s, pos);
	vmaxset(vmax);
	break;
    case SYMSXP:
	vmax = vmaxget();
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    PROTECT(names);
	    for (i = 0; i < nx; i++)
		if (streql(translateChar(STRING_ELT(names, i)),
			   translateChar(PRINTNAME(s)))) {
		    indx = i;
		    break;
		}
	    UNPROTECT(1); /* names */
	}
	if (indx == -1)
	    indx = nx;
	*newname = PRINTNAME(s);
	vmaxset(vmax);
	break;
    default:
	ECALL3(call, _("invalid subscript type '%s'"), type2char(TYPEOF(s)));
    }
    return indx;
}

/* used here and in subset.c and subassign.c */
R_xlen_t attribute_hidden
get1index(SEXP s, SEXP names, R_xlen_t len, int pok, int pos, SEXP call)
{
/* Get a single index for the [[ and [[<- operators.
   Checks that only one index is being selected.
   Returns -1 for no match.

   s is the subscript
   len is the length of the object or dimension, with names its (dim)names.
   pos is len-1 or -1 for [[, -1 for [[<-
     -1 means use the only element of length-1 s.
   pok : is "partial ok" ?
	 if pok is -1, warn if partial matching occurs, but allow.
*/
    const char *ss, *cur_name;
    const void *vmax;

    int warn_pok = (pok == -1);
    if (warn_pok)
	pok = 1;

    if (pos < 0 && length(s) != 1) {
	if (length(s) > 1) {
	    ECALL3(call, _("attempt to select more than one element in %s"), "get1index");
	} else {
	    ECALL3(call, _("attempt to select less than one element in %s"), "get1index");
	}
    } else if(pos >= length(s)) {
	ECALL(call, _("internal error in use of recursive indexing"));
    }
    if(pos < 0) pos = 0;
    R_xlen_t indx = -1;
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    {
	int i = INTEGER_ELT(s, pos);
	if (i != NA_INTEGER)
	    indx = integerOneIndex(i, len, call);
	break;
    }
    case REALSXP:
    {
	double dblind = REAL_ELT(s, pos);
	if(!ISNAN(dblind)) {
	    /* see comment above integerOneIndex */
	    if (dblind > 0) {
		if(R_FINITE(dblind)) indx = (R_xlen_t)(dblind - 1);
	    } else if (dblind == 0 || len < 2) {
		ECALL3(call,
		       _((dblind < 0) ? "invalid negative subscript in %s"
			 : "attempt to select less than one element in %s"),
		       "get1index <real>");
	    } else if (len == 2 && dblind > -3) // dblind = -2 or -1 {why exception ?}
		indx = (R_xlen_t)(2 + dblind);
	    else {
		ECALL3(call,
		       _((dblind < 0) ? "invalid negative subscript in %s"
			 : "attempt to select more than one element in %s"),
		       "get1index <real>");
	    }
	}
	break;
    }
    case STRSXP:
	/* NA matches nothing */
	if(STRING_ELT(s, pos) == NA_STRING) break;
	/* "" matches nothing: see names.Rd */
	if(!CHAR(STRING_ELT(s, pos))[0]) break;

	/* Try for exact match */
	vmax = vmaxget();
	ss = translateChar(STRING_ELT(s, pos));
	for (R_xlen_t i = 0; i < xlength(names); i++)
	    if (STRING_ELT(names, i) != NA_STRING) {
		if (streql(translateChar(STRING_ELT(names, i)), ss)) {
		    indx = i;
		    break;
		}
	    }
	/* Try for partial match */
	if (pok && indx < 0) {
	    size_t len = strlen(ss);
	    for(R_xlen_t i = 0; i < xlength(names); i++) {
		if (STRING_ELT(names, i) != NA_STRING) {
		    cur_name = translateChar(STRING_ELT(names, i));
		    if(!strncmp(cur_name, ss, len)) {
			if(indx == -1) {/* first one */
			    indx = i;
			    if (warn_pok) {
				if (call == R_NilValue)
				    warning(_("partial match of '%s' to '%s'"),
					    ss, cur_name);
				else
				    warningcall(call,
						_("partial match of '%s' to '%s'"),
						ss, cur_name);
			    }
			}
			else {
			    indx = -2;/* more than one partial match */
			    if (warn_pok) /* already given context */
				warningcall(R_NilValue,
					    _("further partial match of '%s' to '%s'"),
					    ss, cur_name);
			    break;
			}
		    }
		}
	    }
	}
	vmaxset(vmax);
	break;
    case SYMSXP:
	vmax = vmaxget();
	for (R_xlen_t i = 0; i < xlength(names); i++)
	    if (STRING_ELT(names, i) != NA_STRING &&
		streql(translateChar(STRING_ELT(names, i)),
		       CHAR(PRINTNAME(s)))) {
		indx = i;
		vmaxset(vmax);
		break;
	    }
	break;
    default:
	ECALL3(call, _("invalid subscript type '%s'"), type2char(TYPEOF(s)));
    }
    return indx;
}

/* This is used for [[ and [[<- with a vector of indices of length > 1 .
   x is a list or pairlist, and it is indexed recusively from
   level start to level stop-1.  ( 0...len-1 or 0..len-2 then len-1).
   For [[<- it needs to duplicate if substructure might be shared.
 */
SEXP attribute_hidden
vectorIndex(SEXP x, SEXP thesub, int start, int stop, int pok, SEXP call,
	    Rboolean dup)
{
    int i;
    R_xlen_t offset;
    SEXP cx;

    /* sanity check */
    if (dup && MAYBE_SHARED(x))
	error("should only be called in an assignment context.");

    for(i = start; i < stop; i++) {
	if(!isVectorList(x) && !isPairList(x)) {
	    if (i)
		errorcall(call, _("recursive indexing failed at level %d\n"), i+1);
	    else
		errorcall(call, _("attempt to select more than one element in %s"), "vectorIndex");
	}
	PROTECT(x);
	SEXP names = PROTECT(getAttrib(x, R_NamesSymbol));
	offset = get1index(thesub, names,
			   xlength(x), pok, i, call);
	UNPROTECT(2); /* x, names */
	if(offset < 0 || offset >= xlength(x))
	    errorcall(call, _("no such index at level %d\n"), i+1);
	if(isPairList(x)) {
#ifdef LONG_VECTOR_SUPPORT
	    if (offset > R_SHORT_LEN_MAX)
		error("invalid subscript for pairlist");
#endif
	    cx = nthcdr(x, (int) offset);
	    RAISE_NAMED(CAR(x), NAMED(x));
	    x = CAR(cx);
	    if (dup && MAYBE_SHARED(x)) {
		PROTECT(cx);
		x = shallow_duplicate(x);
		SETCAR(cx, x);
		UNPROTECT(1); /* cx */
	    }
	} else {
	    cx = x;
	    x = VECTOR_ELT(x, offset);
	    RAISE_NAMED(x, NAMED(cx));
	    if (dup && MAYBE_SHARED(x)) {
		PROTECT(cx);
		x = shallow_duplicate(x);
		SET_VECTOR_ELT(cx, offset, x);
		UNPROTECT(1); /* cx */
	    }
	}
    }
    return x;
}

/* Special Matrix Subscripting: Handles the case x[i] where
   x is an n-way array and i is a matrix with n columns.
   This code returns a vector containing the subscripts
   to be extracted when x is regarded as unravelled.

   Negative indices are not allowed.

   A zero/NA anywhere in a row will cause a zero/NA in the same
   position in the result.
*/


SEXP attribute_hidden mat2indsub(SEXP dims, SEXP s, SEXP call)
{
    int nrs = nrows(s);
    R_xlen_t NR = nrs;
    SEXP rvec;
    int ndim = LENGTH(dims);
    const int *pdims = INTEGER_RO(dims);

    if (ncols(s) != ndim) {
	ECALL(call, _("incorrect number of columns in matrix subscript"));
    }

#ifdef LONG_VECTOR_SUPPORT
    /* Check if it is a long vector we need to index */
    R_xlen_t len = 1;
    for (int j = 0; j < ndim; j++)  len *= pdims[j];

    if(len > R_SHORT_LEN_MAX) {
	PROTECT(rvec = allocVector(REALSXP, nrs));
	double *rv = REAL(rvec);
	for (int i = 0; i < nrs; i++) rv[i] = 1.; // 1-based.
	if (TYPEOF(s) == REALSXP) {
	    for (int i = 0; i < nrs; i++) {
		R_xlen_t tdim = 1;
		const double *ps = REAL_RO(s);
		for (int j = 0; j < ndim; j++) {
		    double k = ps[i + j * NR];
		    if(ISNAN(k)) {rv[i] = NA_REAL; break;}
		    if(k < 0) {
			ECALL(call, _("negative values are not allowed in a matrix subscript"));
		    }
		    if(k == 0.) {rv[i] = 0.; break;}
		    if (k > pdims[j]) {
			ECALL(call, _("subscript out of bounds"));
		    }
		    rv[i] += (k - 1.) * tdim;
		    tdim *= pdims[j];
		}
	    }
	} else {
	    s = coerceVector(s, INTSXP);
	    const int *ps = INTEGER_RO(s);
	    for (int i = 0; i < nrs; i++) {
		R_xlen_t tdim = 1;
		for (int j = 0; j < ndim; j++) {
		    int k = ps[i + j * NR];
		    if(k == NA_INTEGER) {rv[i] = NA_REAL; break;}
		    if(k < 0) {
			ECALL(call, _("negative values are not allowed in a matrix subscript"));
		    }
		    if(k == 0) {rv[i] = 0.; break;}
		    if (k > pdims[j]) {
			ECALL(call, _("subscript out of bounds"));
		    }
		    rv[i] += (double) ((k - 1) * tdim);
		    tdim *= pdims[j];
		}
	    }
	}
    } else
#endif
    {
	PROTECT(rvec = allocVector(INTSXP, nrs));
	int *iv = INTEGER(rvec);
	for (int i = 0; i < nrs; i++) iv[i] = 1; // 1-based.
	s = coerceVector(s, INTSXP);
	int *ps = INTEGER(s);
	for (int i = 0; i < nrs; i++) {
	    int tdim = 1;
	    for (int j = 0; j < ndim; j++) {
		int k = ps[i + j * NR];
		if(k == NA_INTEGER) {iv[i] = NA_INTEGER; break;}
		if(k < 0) {
		    ECALL(call, _("negative values are not allowed in a matrix subscript"));
		}
		if(k == 0) {iv[i] = 0; break;}
		if (k > pdims[j]) {
		    ECALL(call, _("subscript out of bounds"));
		}
		iv[i] += (k - 1) * tdim;
		tdim *= pdims[j];
	    }
	}
    }

    UNPROTECT(1);
    return rvec;
}

/*
Special Matrix Subscripting: For the case x[i] where x is an n-way
array and i is a character matrix with n columns, this code converts i
to an integer matrix by matching against the dimnames of x. NA values
in any row of i propagate to the result.  Unmatched entries result in
a subscript out of bounds error.  */

SEXP attribute_hidden strmat2intmat(SEXP s, SEXP dnamelist, SEXP call)
{
    /* XXX: assumes all args are protected */
    int nr = nrows(s), i, j, v;
    R_xlen_t idx, NR = nr;
    SEXP dnames, snames, si, sicol, s_elt;
    PROTECT(snames = allocVector(STRSXP, nr));
    PROTECT(si = allocVector(INTSXP, xlength(s)));
    dimgets(si, getAttrib(s, R_DimSymbol));
    int *psi = INTEGER(si);
    for (i = 0; i < length(dnamelist); i++) {
	dnames = VECTOR_ELT(dnamelist, i);
	for (j = 0; j < nr; j++)
	    SET_STRING_ELT(snames, j, STRING_ELT(s, j + (i * NR)));
	PROTECT(sicol = match(dnames, snames, 0));
	for (j = 0; j < nr; j++) {
	    v = INTEGER_ELT(sicol, j);
	    idx = j + (i * NR);
	    s_elt = STRING_ELT(s, idx);
	    if (s_elt == NA_STRING) v = NA_INTEGER;
	    if (!CHAR(s_elt)[0]) v = 0; /* disallow "" match */
	    if (v == 0) errorcall(call, _("subscript out of bounds"));
	    psi[idx] = v;
	}
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return si;
}

static SEXP nullSubscript(R_xlen_t n)
{
    SEXP indx;
#ifdef LONG_VECTOR_SUPPORT
    if (n > R_SHORT_LEN_MAX) {
	indx = allocVector(REALSXP, n);
	double *pindx = REAL(indx);
	for (R_xlen_t i = 0; i < n; i++)
	    pindx[i] = (double)(i + 1);
    } else
#endif
    {
	indx = allocVector(INTSXP, n);
	int *pindx = INTEGER(indx);
	for (int i = 0; i < n; i++)
	    pindx[i] = i + 1;
    }
    return indx;
}


static SEXP
logicalSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    R_xlen_t count, i, nmax, i1, i2;
    int canstretch;
    SEXP indx;
    canstretch = *stretch > 0;
    if (!canstretch && ns > nx) {
	ECALL(call, _("(subscript) logical subscript too long"));
    }
    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;
    if (ns == 0) return(allocVector(INTSXP, 0));
    const int *ps = LOGICAL_RO(s);    /* Calling LOCICAL_RO here may force a
					 large allocation, but no larger than
					 the one made by R_alloc below. This
					 could use rewriting to better handle
					 a sparse logical index. */
#ifdef LONG_VECTOR_SUPPORT
    if (nmax > R_SHORT_LEN_MAX) {
	if (ns == nmax) { /* no recycling - use fast single-index code */
	    const void *vmax = vmaxget();
	    double *buf = (double *) R_alloc(nmax, sizeof(double));
	    count = 0;
	    R_ITERATE_CHECK(NINTERRUPT, nmax, i,
		if (ps[i]) {
		    if (ps[i] == NA_LOGICAL)
			buf[count++] = NA_REAL;
		    else
			buf[count++] = (double)(i + 1);
		});
	    PROTECT(indx = allocVector(REALSXP, count));
	    memcpy(REAL(indx), buf, sizeof(double) * count);
	    vmaxset(vmax);
	    UNPROTECT(1);
	    return indx;
	}
	count = 0;
	/* we only need to scan s once even if we recycle,
	   just remember the total count as well as
	   the count for the last incomplete chunk (if any) */
	i1 = (ns < nmax) ? (nmax % ns) : 0;
	if (i1 > 0) { /* last recycling chunk is incomplete -
			 we have to get the truncated count as well */
	    R_xlen_t rem = 0;
	    for (i = 0; i < ns; i++) {
		if (i == i1) rem = count;
		if (ps[i]) count++;
	    }
	    count = count * (nmax / ns) + rem;
	} else { /* nested recycling, total is sufficient */
	    for (i = 0; i < ns; i++)
		if (ps[i]) count++;
	    count *= nmax / ns;
	}
	PROTECT(indx = allocVector(REALSXP, count));
	double *pindx = REAL(indx);
	count = 0;
	MOD_ITERATE_CHECK(NINTERRUPT, nmax, ns, nmax, i, i1, i2,
	    if (ps[i1]) {
		if (ps[i1] == NA_LOGICAL)
		    pindx[count++] = NA_REAL;
		else
		    pindx[count++] = (double)(i + 1);
	    });

	UNPROTECT(1);
	return indx;
    }
#endif
// else --- the same code for  non-long vectors --------------------------
    if (ns == nmax) {  /* no recycling - use fast single-index code */
	const void *vmax = vmaxget();
	int *buf = (int *) R_alloc(nmax, sizeof(int));
	count = 0;
	R_ITERATE_CHECK(NINTERRUPT, nmax, i,
	    if (ps[i]) {
		if (ps[i] == NA_LOGICAL)
		    buf[count++] = NA_INTEGER;
		else
		    buf[count++] = (int)(i + 1);
	    });
	PROTECT(indx = allocVector(INTSXP, count));
	memcpy(INTEGER(indx), buf, sizeof(int) * count);
	vmaxset(vmax);
	UNPROTECT(1);
	return indx;
    }

    count = 0;
    /* we only need to scan s once even if we recycle,
       just remember the total count as well as
       the count for the last incomplete chunk (if any) */
    i1 = (ns < nmax) ? (nmax % ns) : 0;
    if (i1 > 0) { /* last recycling chunk is incomplete -
		     we have to get the truncated count as well */
	R_xlen_t rem = 0;
	for (i = 0; i < ns; i++) {
	    if (i == i1) rem = count;
	    if (ps[i]) count++;
	}
	count = count * (nmax / ns) + rem;
    } else { /* nested recycling, total is sufficient */
	for (i = 0; i < ns; i++)
	    if (ps[i]) count++;
	count *= nmax / ns;
    }
    PROTECT(indx = allocVector(INTSXP, count));
    int *pindx = INTEGER(indx);
    count = 0;
    MOD_ITERATE_CHECK(NINTERRUPT, nmax, ns, nmax, i, i1, i2,
	if (ps[i1]) {
	    if (ps[i1] == NA_LOGICAL)
		pindx[count++] = NA_INTEGER;
	    else
		pindx[count++] = (int)(i + 1);
	});

    UNPROTECT(1);
    return indx;
}

static SEXP negativeSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, SEXP call)
{
    SEXP indx;
    R_xlen_t stretch = 0;
    R_xlen_t i;
    PROTECT(indx = allocVector(LGLSXP, nx));
    int *pindx = LOGICAL(indx);
    for (i = 0; i < nx; i++)
	pindx[i] = 1;
    const int *ps = INTEGER_RO(s);
    for (i = 0; i < ns; i++) {
	int ix = ps[i];
	if (ix != 0 && ix != NA_INTEGER && -ix <= nx)
	    pindx[-ix - 1] = 0;
    }
    s = logicalSubscript(indx, nx, nx, &stretch, call);
    UNPROTECT(1);
    return s;
}

static SEXP positiveSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx)
{
    SEXP indx;
    R_xlen_t i, zct = 0;
    const int *ps = INTEGER_RO(s);
    for (i = 0; i < ns; i++) if (ps[i] == 0) zct++;
    if (zct) {
	indx = allocVector(INTSXP, (ns - zct));
	int *pindx = INTEGER(indx);
	for (i = 0, zct = 0; i < ns; i++)
	    if (ps[i] != 0)
		pindx[zct++] = ps[i];
	return indx;

    } else return s;
}

static SEXP
integerSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    R_xlen_t i;
    int ii, neg, max, canstretch;
    Rboolean isna = FALSE;
    canstretch = *stretch > 0;
    *stretch = 0;
    neg = FALSE;
    max = 0;
    const int *ps = INTEGER_RO(s);
    for (i = 0; i < ns; i++) {
	ii = ps[i];
	if (ii < 0) {
	    if (ii == NA_INTEGER)
		isna = TRUE;
	    else
		neg = TRUE;
	}
	else if (ii > max)
	    max = ii;
    }
    if (max > nx) {
	if(canstretch) *stretch = max;
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (neg) {
	if (max == 0 && !isna) return negativeSubscript(s, ns, nx, call);
	else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    }
    else return positiveSubscript(s, ns, nx);
    return R_NilValue;
}

static SEXP
realSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, R_xlen_t *stretch, SEXP call)
{
    R_xlen_t i;
    int canstretch;
    double ii, min, max;
    Rboolean isna = FALSE;
    canstretch = *stretch > 0;
    *stretch = 0;
    min = 0;
    max = 0;
    const double *ps = REAL_RO(s);
    for (i = 0; i < ns; i++) {
	ii = ps[i];
	if (R_FINITE(ii)) {
	    if (ii < min) min = ii;
	    if (ii > max) max = ii;
	} else isna = TRUE;
    }
    if (max > nx) {
#ifndef LONG_VECTOR_SUPPORT
	if (max > INT_MAX) {
	    ECALL(call, _("subscript too large for 32-bit R"));
	}
#endif
	if(canstretch) *stretch = (R_xlen_t) max;
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (min < 0) {
	if (max == 0 && !isna) {
	    SEXP indx;
	    R_xlen_t stretch = 0;
	    double dx;
	    R_xlen_t i, ix;
	    PROTECT(indx = allocVector(LGLSXP, nx));
	    int *pindx = LOGICAL(indx);
	    for (i = 0; i < nx; i++) pindx[i] = 1;
	    for (i = 0; i < ns; i++) {
		dx = ps[i];
		if (R_FINITE(dx) && dx != 0  && -dx <= nx) {
		    ix = (R_xlen_t)(-dx - 1);
		    pindx[ix] = 0;
		}
	    }
	    s = logicalSubscript(indx, nx, nx, &stretch, call);
	    UNPROTECT(1);
	    return s;
	} else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    } else {
	/* Only return a REALSXP index if we need to */
	SEXP indx;
	R_xlen_t i, cnt = 0;
	Rboolean int_ok = TRUE;
	/* NB, indices will be truncated eventually,
	   so need to do that to take '0' into account */
	for (i = 0; i < ns; i++) {
	    double ds = ps[i];
#ifdef OLDCODE_LONG_VECTOR
	    if (!R_FINITE(ds)) {
		if (ds > INT_MAX) int_ok = FALSE;
		cnt++;
	    } else if ((R_xlen_t) ds != 0) cnt++;
#else
	    if (R_FINITE(ds) && ds > INT_MAX) int_ok = FALSE;
	    if (!R_FINITE(ds) || (R_xlen_t) ds != 0) cnt++;
#endif
	}
	if (int_ok) {
	    indx = allocVector(INTSXP, cnt);
	    int *pindx = INTEGER(indx);
	    for (i = 0, cnt = 0; i < ns; i++) {
		double ds = ps[i];
		int ia;
		if (!R_FINITE(ds)) ia = NA_INTEGER;
		else ia = (int) ds;
		if (ia != 0) pindx[cnt++] = ia;
	    }
	} else {
	    indx = allocVector(REALSXP, cnt);
	    double *pindx = REAL(indx);
	    for (i = 0, cnt = 0; i < ns; i++) {
		double ds = ps[i];
		if (!R_FINITE(ds) || (R_xlen_t) ds != 0) pindx[cnt++] = ds;
	    }
	}
	return indx;
    }
    return R_NilValue;
}

/* This uses a couple of horrible hacks in conjunction with
 * VectorAssign (in subassign.c).  If subscripting is used for
 * assignment, it is possible to extend a vector by supplying new
 * names, and we want to give the extended vector those names, so they
 * are returned as the use.names attribute. Also, unset elements of the vector
 * of new names (places where a match was found) are indicated by
 * setting the element of the newnames vector to NULL.
*/

/* The original code (pre 2.0.0) used a ns x nx loop that was too
 * slow.  So now we hash.  Hashing is expensive on memory (up to 32nx
 * bytes) so it is only worth doing if ns * nx is large.  If nx is
 * large, then it will be too slow unless ns is very small.
 */

static SEXP
stringSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, SEXP names,
		R_xlen_t *stretch, SEXP call)
{
    SEXP indx, indexnames = R_NilValue;
    R_xlen_t i, j, nnames, extra, sub;
    int canstretch = *stretch > 0;
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = ( ((ns > 1000 && nx) || (nx > 1000 && ns)) || (ns * nx > 15*nx + ns) );

    PROTECT(s);
    PROTECT(names);
    nnames = nx;
    extra = nnames;

    /* Process each of the subscripts. First we compare with the names
     * on the vector and then (if there is no match) with each of the
     * previous subscripts, since (if assigning) we may have already
     * added an element of that name. (If we are not assigning, any
     * nonmatch will have given an error.)
     */

    if(usehashing) {
	/* must be internal, so names contains a character vector */
	/* NB: this does not behave in the same way with respect to ""
	   and NA names: they will match */
	PROTECT(indx = match(names, s, 0)); /**** guaranteed to be fresh???*/
	/* second pass to correct this */
	int *pindx = INTEGER(indx);
	for (i = 0; i < ns; i++)
	    if(STRING_ELT(s, i) == NA_STRING || !CHAR(STRING_ELT(s, i))[0])
		pindx[i] = 0;
    } else {
	PROTECT(indx = allocVector(INTSXP, ns));
	int *pindx = INTEGER(indx);
	for (i = 0; i < ns; i++) {
	    sub = 0;
	    if (names != R_NilValue) {
		for (j = 0; j < nnames; j++) {
		    SEXP names_j = STRING_ELT(names, j);
		    if (NonNullStringMatch(STRING_ELT(s, i), names_j)) {
			sub = j + 1;
			break;
		    }
		}
	    }
	    pindx[i] = (int) sub;
	}
    }

    int *pindx = INTEGER(indx);
    SEXP sindx = NULL;
    for (i = 0; i < ns; i++) {
	sub = pindx[i];
	if (sub == 0) {
	    if (sindx == NULL) {
		sindx = PROTECT(match(s, s, 0));
		indexnames = PROTECT(allocVector(VECSXP, ns));
		for (int z = 0; z < ns; z++)
		    SET_VECTOR_ELT(indexnames, z, R_NilValue);
	    }
	    int j = INTEGER(sindx)[i] - 1;
	    if(STRING_ELT(s, i) != NA_STRING && CHAR(STRING_ELT(s, i))[0]) {
		sub = pindx[j];
		SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, j));
	    }
	}
	if (sub == 0) {
	    if (!canstretch) {
		ECALL(call, _("subscript out of bounds"));
	    }
	    extra += 1;
	    sub = extra;
	    SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, i));
	}
	pindx[i] = (int) sub;
    }
    /* We return the new names as the names attribute of the returned
       subscript vector. */
    if (extra != nnames)
	setAttrib(indx, R_UseNamesSymbol, indexnames);
    if (sindx != NULL) {
	UNPROTECT(2);
    }
    if (canstretch)
	*stretch = extra;
    UNPROTECT(3);
    return indx;
}

/* Array Subscripts.
    dim is the dimension (0 to k-1)
    s is the subscript list,
    dims is the dimensions of x
    dng is a function (usually getAttrib) that obtains the dimnames
    x is the array to be subscripted.
*/

attribute_hidden SEXP
int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call)
{
    int nd, ns;
    R_xlen_t stretch = 0;
    SEXP dnames, tmp;
    ns = length(s);
    nd = INTEGER_ELT(dims, dim);

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, call);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, call);
    case REALSXP:
	/* We don't yet allow subscripts > R_SHORT_LEN_MAX */
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, call);
	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = getAttrib(x, R_DimNamesSymbol);
	if (dnames == R_NilValue) {
	    ECALL(call, _("no 'dimnames' attribute for array"));
	}
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, &stretch, call);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	ECALL3(call, _("invalid subscript type '%s'"), type2char(TYPEOF(s)));
    }
    return R_NilValue;
}

/* This is used by packages arules, cba, proxy and seriation. */
typedef SEXP AttrGetter(SEXP x, SEXP data);
typedef SEXP (*StringEltGetter)(SEXP x, int i);

SEXP
arraySubscript(int dim, SEXP s, SEXP dims, AttrGetter dng,
	       StringEltGetter strg, SEXP x)
{
    return int_arraySubscript(dim, s, dims, x, R_NilValue);
}

/* Subscript creation.  The first thing we do is check to see */
/* if there are any user supplied NULL's, these result in */
/* returning a vector of length 0. */
/* if stretch is zero on entry then the vector x cannot be
   "stretched",
   otherwise, stretch returns the new required length for x
*/

SEXP attribute_hidden
makeSubscript(SEXP x, SEXP s, R_xlen_t *stretch, SEXP call)
{
    if (! (isVector(x) || isList(x) || isLanguage(x))) {
	ECALL(call, _("subscripting on non-vector"));
    }

    R_xlen_t nx = xlength(x);

    /* special case for simple indices -- does not duplicate */
    if (IS_SCALAR(s, INTSXP)) {
	int i = SCALAR_IVAL(s);
	if (0 < i && i <= nx) {
	    *stretch = 0;
	    return s;
	}
    }
    else if (IS_SCALAR(s, REALSXP)) {
	double di = SCALAR_DVAL(s);
	if (1 <= di && di <= nx) {
	    *stretch = 0;
	    /* We could only return a REALSXP if the value is too
	       large for an INTSXP, but, as the calling code can
	       handle REALSXP indices, returning the REALSXP
	       avoids an allocation. */
	    return s;
	}
    }

    R_xlen_t ns = xlength(s);
    SEXP ans = R_NilValue;
    switch (TYPEOF(s)) {
    case NILSXP:
	*stretch = 0;
	ans = allocVector(INTSXP, 0);
	break;
    case LGLSXP:
	ans = logicalSubscript(s, ns, nx, stretch, call);
	break;
    case INTSXP:
	ans = integerSubscript(s, ns, nx, stretch, call);
	break;
    case REALSXP:
	ans = realSubscript(s, ns, nx, stretch, call);
	break;
    case STRSXP:
    {
	SEXP names = PROTECT(getAttrib(x, R_NamesSymbol));
	/* *stretch = 0; */
	ans = stringSubscript(s, ns, nx, names, stretch, call);
	UNPROTECT(1); /* names */
	break;
    }
    case SYMSXP:
	*stretch = 0;
	if (s == R_MissingArg) {
	    ans = nullSubscript(nx);
	    break;
	}
    default:
	ECALL3(call, _("invalid subscript type '%s'"), type2char(TYPEOF(s)));
    }
    return ans;
}
