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
 *
 * EXPORTS:
 *
 *  get1index()       -- used for "[["   in ./subassign.c & subset.c
 *
 *  mat2indsub()      -- for "mat[i]"     "    "            "
 *
 *  makeSubscript()   -- for "[" and "[<-" in ./subset.c and ./subassign.c,
 *			 and "[[<-" with a scalar in ./subassign.c
 *  internalArraySubscript()  -- for "[i,j,..." and "[<-..." 
 *                               in ./subset.c, ./subassign.c
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>

/* We might get a call with R_NilValue from subassignment code */
#define ECALL(call, yy) do { \
  if (call == R_NilValue) error(yy); \
  else errorcall(call, yy); \
} while (0)

static int integerOneIndex(int i, int len, SEXP call)
{
    int indx = -1;

    if (i > 0)
	indx = i - 1;
    else if (i == 0 || len < 2) {
	ECALL(call, _("attempt to select less than one element"));
    } else if (len == 2 && i > -3)
	indx = 2 + i;
    else {
	ECALL(call, _("attempt to select more than one element"));
    }
    return(indx);
}

int attribute_hidden
get1index(SEXP s, SEXP names, int len, int pok, int pos, SEXP call)
{
/* Get a single index for the [[ operator.
   Check that only one index is being selected.
   pok : is "partial ok" ?
	 if pok is -1, warn if partial matching occurs
*/
    int indx, i, warn_pok;
    R_len_t length_s = length(s);
    double dblind;
    SEXP se;

    warn_pok = 0;
    if (pok == -1) {
	pok = 1;
	warn_pok = 1;
    }

    if (pos < 0 && length_s != 1) {
	if (length_s > 1)
	    ECALL(call, _("attempt to select more than one element"));
	else
	    ECALL(call, _("attempt to select less than one element"));
    } 
    else {
	if(pos >= length_s)
	    ECALL(call, _("internal error in use of recursive indexing"));
    }

    if (pos < 0) pos = 0;
    indx = -1;

    switch (TYPEOF(s)) {

    case LGLSXP:
        /* REALLY???  But it falls through in R-2.15.0, so keep doing so... */
    case INTSXP:
	i = INTEGER(s)[pos];
	if (i != NA_INTEGER)
	    indx = integerOneIndex(i, len, call);
	break;

    case REALSXP:
	dblind = REAL(s)[pos];
	if (!ISNAN(dblind))
            indx = dblind > R_LEN_T_MAX ? R_LEN_T_MAX
                 : dblind < -R_LEN_T_MAX ? -R_LEN_T_MAX
                 : integerOneIndex((int)dblind, len, call);
	break;

    case STRSXP:
    {
	se = STRING_ELT(s,pos);
	/* NA matches nothing */
	if (se == NA_STRING)
            break;
	/* "" matches nothing: see names.Rd */
	if (CHAR(se)[0] == 0)
            break;

        int len_names = TYPEOF(names) != STRSXP ? 0 : LENGTH(names);

	/* Try for exact match */
	for (i = 0; i < len_names; i++) {
	    if (STRING_ELT(names,i) != NA_STRING 
             && SEQL(STRING_ELT(names,i),se)) {
	     	indx = i;
		break;
	    }
	}

	/* Try for partial match */
	if (pok && indx < 0) {
	    const char *ss = translateChar(se);
	    len = strlen(ss);
	    for(i = 0; i < len_names; i++) {
		if (STRING_ELT(names, i) != NA_STRING) {
		    const char *cur_name = translateChar(STRING_ELT(names,i));
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
	break;
    }
    case SYMSXP:
    {
        int len_names = TYPEOF(names) != STRSXP ? 0 : LENGTH(names);
	for (i = 0; i < len_names; i++) {
	    if (STRING_ELT(names,i) != NA_STRING
                  && SEQL (STRING_ELT(names,i), PRINTNAME(s))) {
		indx = i;
		break;
	    }
        }
        break;
    }
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"),
		      type2char(TYPEOF(s)));
    }
    return indx;
}

/* Special Matrix Subscripting: Handles the case x[i] where */
/* x is an n-way array and i is a matrix with n columns. */
/* This code returns a vector containing the integer subscripts */
/* to be extracted when x is regarded as unravelled. */
/* Negative indices are not allowed. */
/* A zero anywhere in a row will cause a zero in the same */
/* position in the result. */

SEXP attribute_hidden mat2indsub(SEXP dims, SEXP s, SEXP call)
{
    int tdim, j, i, k, nrs = nrows(s);
    SEXP rvec;

    if (ncols(s) != LENGTH(dims))
	ECALL(call, _("incorrect number of columns in matrix subscript"));

    PROTECT(rvec = allocVector(INTSXP, nrs));
    s = coerceVector(s, INTSXP);
    setIVector(INTEGER(rvec), nrs, 0);

    for (i = 0; i < nrs; i++) {
	tdim = 1;
	/* compute 0-based subscripts for a row (0 in the input gets -1
	   in the output here) */
	for (j = 0; j < LENGTH(dims); j++) {
	    k = INTEGER(s)[i + j * nrs];
	    if (k == NA_INTEGER) {
		INTEGER(rvec)[i] = NA_INTEGER;
		break;
	    }
	    if (k < 0)
		ECALL(call, 
                  _("negative values are not allowed in a matrix subscript"));
	    if (k == 0) {
		INTEGER(rvec)[i] = -1;
		break;
	    }
	    if (k > INTEGER(dims)[j])
		ECALL(call, _("subscript out of bounds"));
	    INTEGER(rvec)[i] += (k - 1) * tdim;
	    tdim *= INTEGER(dims)[j];
	}
	/* transform to 1 based subscripting (0 in the input gets 0
	   in the output here) */
	if(INTEGER(rvec)[i] != NA_INTEGER)
	    INTEGER(rvec)[i]++;
    }
    UNPROTECT(1);
    return (rvec);
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
    int nr = nrows(s), i, j, v, idx;
    SEXP dnames, snames, si, sicol, s_elt;
    PROTECT(snames = allocVector(STRSXP, nr));
    PROTECT(si = allocVector(INTSXP, length(s)));
    dimgets(si, getDimAttrib(s));
    for (i = 0; i < length(dnamelist); i++) {
        dnames = VECTOR_ELT(dnamelist, i);
        for (j = 0; j < nr; j++) {
            SET_STRING_ELT(snames, j, STRING_ELT(s, j + (i * nr)));
        }
        PROTECT(sicol = match(dnames, snames, 0));
        for (j = 0; j < nr; j++) {
            v = INTEGER(sicol)[j];
            idx = j + (i * nr);
            s_elt = STRING_ELT(s, idx);
            if (s_elt == NA_STRING) v = NA_INTEGER;
            if (!CHAR(s_elt)[0]) v = 0; /* disallow "" match */
            if (v == 0) errorcall(call, _("subscript out of bounds"));
            INTEGER(si)[idx] = v;
        }
        UNPROTECT(1);
    }
    UNPROTECT(2);
    return si;
}

static SEXP nullSubscript (int n)
{
    SEXP indx = allocVector(INTSXP, n);
    int *ix = INTEGER(indx) - 1;

    unsigned i;     /* unsigned avoids overflow issue */
    unsigned m = n;

    for (i = 1; i <= m; i++) ix[i] = i;

    return indx;
}

static SEXP logicalSubscript (SEXP s, int ns, int nx, int *stretch, 
                              int *hasna, SEXP call)
{
    int canstretch, nmax;
    canstretch = *stretch;
    int *si = LOGICAL(s);
    R_len_t i, j, k;
    int *xi;
    SEXP x;
    int v;

    if (!canstretch && ns > nx)
	ECALL(call, _("(subscript) logical subscript too long"));

    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;

    if (ns == 0)
	return allocVector(INTSXP, 0);

    if (ns != nmax || SIZEOF_CHAR_P <= 4) {  /* small address space */

        /* TWO-PASS IMPLEMENTATION.  Avoids allocating more memory than
           necessary, hence preferred for systems with limited address space.
           Also used when short subscript is recycled for longer vector. */

        /* Count the number of TRUE or NA values in s.  Adds together all the
           values in s in a 64-bit unsigned accumulator, then adds portions of
           this sum to get the desired count.  Need to then do more if subscript
           is recycled... */
    
        unsigned *su = (unsigned *)si;
        uint64_t ucount;
        ucount = 0;
        i = 0;
        if (ns & 1) {
            ucount += su[i++];
        }
        if (ns & 2) {
            ucount += su[i++];
            ucount += su[i++];
        }
        while (i < ns) {
            ucount += su[i++];
            ucount += su[i++];
            ucount += su[i++];
            ucount += su[i++];
        }

        int NA_count = ucount >> 31;
        int TRUE_count = ucount & 0x7fffffff;
        int len = NA_count + TRUE_count;

        if (nmax > ns) {  /* adjust for replication, perhaps partial */
            len *= nmax / ns;
            int rem = nmax % ns;
            ucount = 0;
            for (i = 0; i < rem; i++)
                ucount += su[i];
            len += (int)(ucount >> 31) + (int)(ucount & 0x7fffffff);
        }
    
        /* Create index vector, x, with NA or index values. */
    
        x = allocVector (INTSXP, len);

        if (len > 0) {

            xi = INTEGER(x);
            j = 0;
    
            if (NA_count == 0) {  /* don't need to handle NA */
                i = 0;
                if (ns & 1) {
                    if ((v = si[i++]) != 0) xi[j++] = i;
                }
                if (ns & 2) {
                    if ((v = si[i++]) != 0) xi[j++] = i;
                    if ((v = si[i++]) != 0) xi[j++] = i;
                }
                while (i < ns) {
                    if ((v = si[i++]) != 0) xi[j++] = i;
                    if ((v = si[i++]) != 0) xi[j++] = i;
                    if ((v = si[i++]) != 0) xi[j++] = i;
                    if ((v = si[i++]) != 0) xi[j++] = i;
                }
                if (j <= 1) {
                    int t = xi[0];
                    while (j < len) { t += ns; xi[j++] = t; }
                }
                else {  /* unrolled loop that requires j > 1 */
                    int t0, t1;
                    k = 0;
                    if ((len-j) & 1) {
                        int t0 = xi[k++];
                        xi[j++] = t0 + ns;
                    }
                    while (j < len) {
                        int t0 = xi[k++];
                        int t1 = xi[k++];
                        xi[j++] = t0 + ns;
                        xi[j++] = t1 + ns;
                    }
                }
                *hasna = 0;
            }
            else {  /* do need to handle NA */
                i = 0;
                if (ns & 1) {
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                }
                if (ns & 2) {
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                }
                while (i < ns) {
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                    if ((v = si[i++]) != 0) xi[j++] = v < 0 ? NA_INTEGER : i;
                }
                if (ns == 1) {  /* sole index must be NA */
                    while (j < len) xi[j++] = NA_INTEGER;
                }
                if (j <= 1) {
                    int t = xi[0];
                    if (t == NA_INTEGER)
                        while (j < len) xi[j++] = NA_INTEGER;
                    else
                        while (j < len) { t += ns; xi[j++] = t; }
                }
                else {  /* unrolled loop that requires j > 1 */
                    int t0, t1;
                    k = 0;
                    if ((len-j) & 1) {
                        int t0 = xi[k++];
                        xi[j++] = t0 < 0 ? NA_INTEGER : t0 + ns;
                    }
                    while (j < len) {
                        int t0 = xi[k++];
                        int t1 = xi[k++];
                        xi[j++] = t0 < 0 ? NA_INTEGER : t0 + ns;
                        xi[j++] = t1 < 0 ? NA_INTEGER : t1 + ns;
                    }
                }
                for (i = 0; xi[i] >= 0; i++) ;  /* find first NA */
                *hasna = i+1;
            }
        }
    }

    else {  /* large address space */

        /* ONE-PASS IMPLEMENTATION.  May allocate much more memory than
           necessary, but unused portions are never accessed, and on many
           systems will not be allocated physical memory.  But the allocation
           does occupy address space, hence this is more suitable when
           there's plenty of address space.  Does not handle recycling. */

        /* Initially try to store indices in a local array, xi0, of length
           LEN0.  When that's full, or when the end of s is reached, copy
           contents to an allocated INTSXP vector, to which more indices
           may be added.  Reduce the length of this vector once done to
           give the final result (or expands it and copies if we're 
           recycling). */

#       define LEN0 300  /* Must be at least 3 */

        R_len_t i, j, first_na;
        int xi0[LEN0];
        first_na = 0;
        xi = xi0;
        j = 0;
        i = 0;

        /* Use unrolled loops. */

        if (ns & 1) {
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
        }
        if (ns & 2) {
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
        }

        while (i < ns && j < LEN0-3) {
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
        }

        x = allocVector (INTSXP, j + (ns-i));
        xi = INTEGER(x);
        memcpy (xi, xi0, j * sizeof(int));

        while (i < ns) {
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            if ((v = si[i++]) != 0) 
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
        }

        if (LENGTH(x) != j)
            x = reallocVector(x,j,1);

        *hasna = first_na;
    }

    return x;
}

static SEXP negativeSubscript(SEXP s, int ns, int nx)
{
    void *vmax = VMAXGET();

    int *si = INTEGER(s);
    char *keep;
    int dcnt;
    int i, j;

    /* Allocate 'keep' flags, and set them all to 1. */

    keep = R_alloc (nx, 1);
    memset (keep, 1, nx);

    /* Clear 'keep' flags of elements with negative indexes in subscript vector,
       ignoring 0 and NA.  Count how many will be deleted. */

    dcnt = 0;
    for (i = 0; i < ns; i++) {
        unsigned u = ~(unsigned)si[i];  /* Map negative indexes from -1 to -nx
                                           to 0 to nx-1 (unsigned) */
        if (u < nx) {
            dcnt += keep[u];
            keep[u] = 0;
        }
    }

    /* Create vector of indexes of elements not deleted. */

    R_len_t len = nx - dcnt;
    s = allocVector (INTSXP, len);
    si = INTEGER(s);

    i = 0;
    j = 0;
    if (nx & 1) {
        if (keep[j++]) si[i++] = j;
    }
    if (nx & 2) {
        if (keep[j++]) si[i++] = j;
        if (keep[j++]) si[i++] = j;
    }
    while (j < nx) {
        if (keep[j++]) si[i++] = j;
        if (keep[j++]) si[i++] = j;
        if (keep[j++]) si[i++] = j;
        if (keep[j++]) si[i++] = j;
    }

    if (i != len) abort();

    VMAXSET(vmax);
    return s;
}

static SEXP nonnegativeSubscript(SEXP s, int ns, int nx)
{
    SEXP indx;
    int i, zct = 0;
    for (i = 0; i < ns; i++) {
        if (INTEGER(s)[i] == 0)
            zct++;
    }
    if (zct) {
        indx = allocVector(INTSXP, (ns - zct));
        for (i = 0, zct = 0; i < ns; i++)
            if (INTEGER(s)[i] != 0)
                INTEGER(indx)[zct++] = INTEGER(s)[i];
        return indx;
    }
    else
        return s;
}

static SEXP integerSubscript (SEXP s, int ns, int nx, int *stretch, int *hasna,
                              SEXP call)
{
    int i, ii, min, max, canstretch;

    canstretch = *stretch;
    *stretch = 0;

    for (i = 0; i < ns; i++) {
        ii = INTEGER(s)[i];
        if (ii != NA_INTEGER) 
            break;
    }

    if (i==ns) /* all NA, or ns==0 */
        return s;

    *hasna = i>0;

    min = ii;
    max = ii;
    for (i = i+1; i < ns; i++) {
        ii = INTEGER(s)[i];
        if (ii == NA_INTEGER) {
            if (!*hasna) *hasna = i+1;
        }
        else {
            if (ii > max)  /* checked first since more common than ii < min */
                max = ii;
            else if (ii < min)
                min = ii;
        }
    }

    if (max > nx) {
        if (canstretch) 
            *stretch = max;
        else
            ECALL(call, _("subscript out of bounds"));
    }

    if (min > 0) /* All positive (or NA) */
        return s;
    else if (min < 0) {
        if (max <= 0 && !*hasna) 
            return negativeSubscript(s, ns, nx);
        else
            ECALL(call, _("only 0's may be mixed with negative subscripts"));
    }
    else /* min == 0 */
        return nonnegativeSubscript(s, ns, nx);
}

typedef SEXP (*StringEltGetter)(SEXP x, int i);

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

#define na_or_empty_string(strelt) ((strelt)==NA_STRING || CHAR((strelt))[0]==0)

static SEXP stringSubscript (SEXP s, int ns, int nx, SEXP names,
                             StringEltGetter strg, int *stretch, SEXP call)
{
    SEXP indx, indexnames;
    int i, j, k, nnames, sub, extra;
    int canstretch = *stretch;
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = names == R_NilValue ? FALSE :
        ns > 1000 ? (nx > 2) : nx > 1000 ? (ns > 15) : (ns*nx > 15*nx + 2*ns);
    /* was: (ns > 1000 && nx) || (nx > 1000 && ns) || (ns * nx > 15*nx + ns) */

    PROTECT(s);
    PROTECT(names);
    indexnames = R_NoObject;
    nnames = names==R_NilValue ? 0 : nx;
    extra = nx;

    /* Process each of the subscripts. First we compare with the names
     * on the vector and then (if there is no match) with each of the
     * previous subscripts, since (if assigning) we may have already
     * added an element of that name. (If we are not assigning, any
     * nonmatch will have given an error.)
     */

    if(usehashing) {
	/* NB: this does not behave in the same way with respect to ""
	   and NA names: they will match */
	PROTECT(indx = match(names, s, 0));
	/* second pass to correct this */
	for (i = 0; i < ns; i++) {
            SEXP sbe_i = STRING_ELT(s,i);
	    if (na_or_empty_string(sbe_i))
		INTEGER(indx)[i] = 0;
        }
    } else {
        PROTECT(indx = allocVector(INTSXP, ns));
        if (nnames == 0)
            for (i = 0; i < ns; i++) 
                INTEGER(indx)[i] = 0;
        else {
            for (i = 0; i < ns; i++) {
                SEXP sbe_i = STRING_ELT(s,i);
                sub = 0;
                if (!na_or_empty_string(sbe_i)) {
                    for (j = 0; j < nnames; j++) {
                        SEXP sbe_j = strg(names,j);
                        if (!na_or_empty_string(sbe_j) && SEQL(sbe_i,sbe_j)) {
                            sub = j + 1;
                            break;
                        }
                    }
                }
                INTEGER(indx)[i] = sub;
            }
        }
    }

    if (canstretch == 0) {
        for (i = 0; i < ns; i++) {
            if (INTEGER(indx)[i] == 0)
                ECALL(call, _("subscript out of bounds"));
        }
    }
    else if (canstretch < 0) {
        for (i = 0; i < ns; i++) {
            if (INTEGER(indx)[i] == 0) {
                SEXP sbe_i = STRING_ELT(s,i);
                if (indexnames == R_NoObject) { /* first non-matching index */
                    PROTECT (indexnames = allocVector(VECSXP, ns));
                    for (k = 0; k < ns; k++) 
                        if (INTEGER(indx)[k] != 0)
                            SET_VECTOR_ELT (indexnames, k, R_NilValue);
                }
                SET_VECTOR_ELT (indexnames, i, sbe_i);
                extra += 1;
                sub = extra;
                INTEGER(indx)[i] = sub;
                if (!na_or_empty_string(sbe_i)) {
                    for (j = i+1 ; j<ns ; j++) {
                        if (INTEGER(indx)[j] == 0) {
                            SEXP sbe_j = STRING_ELT(s,j);
                            if (!na_or_empty_string(sbe_j) 
                             && SEQL(sbe_i,sbe_j)) {
                                INTEGER(indx)[j] = sub;
                                SET_VECTOR_ELT (indexnames, j, sbe_i);
                            }
                        }
                    }
                }
            }
        }
    }
    else {
        /* We just leave any zeros in the returned index vector */
    }

    /* We return the new names as the names attribute of the returned
       subscript vector. */
    if (indexnames != R_NoObject)
        setAttrib(indx, R_UseNamesSymbol, indexnames);
    if (canstretch)
        *stretch = extra==nx ? 0 : extra;

    UNPROTECT (3+(indexnames!=R_NoObject));

    return indx;
}

/* Array Subscripts.
    dim is the dimension (0 to k-1)
    s is the subscript list,
    dims is the dimensions of x
    x is the array to be subscripted.
    hasna is set to index of first NA index, 0 if none
*/

SEXP attribute_hidden internalArraySubscript
                        (int dim, SEXP s, SEXP dims, SEXP x, int *hasna)
{
    SEXP call = R_NilValue;
    int nd, ns, stretch = 0;
    SEXP dnames, tmp;
    ns = length(s);
    nd = INTEGER(dims)[dim];

    *hasna = 0;

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, hasna, call);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, hasna, call);
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, hasna, call);
	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = getAttrib(x, R_DimNamesSymbol);
	if (dnames == R_NilValue)
	    ECALL(call, _("no 'dimnames' attribute for array"));
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, (STRING_ELT), &stretch, call);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
        /* fall through */
    default:
        error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
    }
}

/* Function similar to internalArraySubscript, used by packages
   arules and cba. Seems dangerous as the typedef is not exported.

   dng is a function (usually getAttrib) that obtains the dimnames. */

typedef SEXP AttrGetter(SEXP x, SEXP data);

SEXP arraySubscript (int dim, SEXP s, SEXP dims, AttrGetter dng,
                     StringEltGetter strg, SEXP x)
{
    SEXP call = R_NilValue;
    int nd, ns, hasna, stretch = 0;
    SEXP dnames, tmp;
    ns = length(s);
    nd = INTEGER(dims)[dim];

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, &hasna, call);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, &hasna, call);
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, &hasna, call);
	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = dng(x, R_DimNamesSymbol);
	if (dnames == R_NilValue)
	    ECALL(call, _("no 'dimnames' attribute for array"));
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, strg, &stretch, call);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"),
		      type2char(TYPEOF(s)));
    }
}

/* Subscript creation.  x is the object being subscripted; s is the 
   R subscript value.  

   The "stretch" argument is a pointer to an integer set by the caller as 
   follows:
  
      0   No out-of-bounds indexes allowed
      1   Out-of-bounds indexes allowed, for fetching only
     -1   Out-of-bounds indexes allowed, for storing
  
   This procedure will set *stretch to 0 if there are no out-of-bound
   indexes, and otherwise to the largest out-of-bounds index.

   The used_to_replace argument should be 1 if the subscript is used to 
   replace items and 0 if the subscript is only for extracting items.
   This is used to reduce how often the subscript vector needs to be
   duplicated.

   The value returned is an integer vector, which may have a
   R_UseNamesSymbol attribute containing new names to use.  The caller
   should look for this only if the subscripts were strings.

   The hasna argument is set to the index (from 1) of the first NA
   subscript, or 0 if there are none.

   The arguments x and s are protected within this function.
*/

SEXP attribute_hidden makeSubscript (SEXP x, SEXP s, int *stretch, int *hasna,
                                     SEXP call, int used_to_replace)
{
    int nx, ns;
    SEXP ans, tmp;

    *hasna = 0;

    if (!isVector(x) && !isList(x) && !isLanguage(x))
	ECALL(call, _("subscripting on non-vector"));

    nx = length(x);
    ns = length(s);

    /* Handle single positive index (real or integer), not out of bounds.
       Note that we don't have to worry about a length one subscript being
       modified in a replace operation, since even if it is, we don't use
       it anymore after the modification.  Since it is of length one, we
       can return a vector that is in use (caller shouldn't modify it). */

    if (ns == 1) {
        if (TYPEOF(s) == INTSXP) {
            int i = INTEGER(s)[0];
            if (0 < i && i <= nx) {
                *stretch = 0;
                return s;
            }
	} else if (TYPEOF(s) == REALSXP) {
            int i, warn = 0;
            i = IntegerFromReal (REAL(s)[0], &warn);
            if (0 < i && i <= nx) {
                if (warn) CoercionWarning(warn);
                *stretch = 0;
                return ScalarIntegerMaybeConst(i);
            }
        }
    }

    PROTECT2(x,s);

    switch (TYPEOF(s)) {
    case NILSXP:
	*stretch = 0;
	ans = allocVector(INTSXP, 0);
	break;
    case LGLSXP:
	ans = logicalSubscript(s, ns, nx, stretch, hasna, call);
	break;
    case INTSXP:
	ans = integerSubscript(s, ns, nx, stretch, hasna, call);
	break;
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	ans = integerSubscript(tmp, ns, nx, stretch, hasna, call);
	UNPROTECT(1);
	break;
    case STRSXP: {
	SEXP names = PROTECT(getAttrib(x, R_NamesSymbol));
	ans = stringSubscript(s, ns, nx, names, (STRING_ELT), stretch, call);
        UNPROTECT(1);
        break;
    }
    case SYMSXP:
	*stretch = 0;
	if (s == R_MissingArg) {
	    ans = nullSubscript(nx);
	    break;
	}
        /* fall through */
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"),
		      type2char(TYPEOF(s)));
    }

    /* If ans is being used for replacement, duplicate it if it is the same 
       as s, to avoid problems with assignments like p[p] <- ... */

    if (used_to_replace && ans == s)
        ans = duplicate(ans);

    UNPROTECT(2);
    return ans;
}
