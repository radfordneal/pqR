/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2007   The R Core Team
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
 *
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 *
 *
 *  Subscripts that are ranges:  "[" now asks for a variant result from
 *  a sequence operator (for first index), hoping to get a range rather
 *  than a sequence, allowing for faster extraction, and avoidance of
 *  memory use for the sequence.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"

#include "scalar-stack.h"

#include <helpers/helpers-app.h>

#if __AVX2__ && !defined(DISABLE_AVX_CODE)
#include <immintrin.h>
#endif


/* ----------------------------- SUBSCRIPTS --------------------------------- */

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

int get1index(SEXP s, SEXP names, int len, int pok, int pos, SEXP call)
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

static SEXP mat2indsub(SEXP dims, SEXP s, SEXP call)
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

/* Special Matrix Subscripting: For the case x[i] where x is an n-way
   array and i is a character matrix with n columns, this code
   converts i to an integer matrix by matching against the dimnames of
   x. NA values in any row of i propagate to the result.  Unmatched
   entries result in a subscript out of bounds error.  */

static SEXP strmat2intmat(SEXP s, SEXP dnamelist, SEXP call)
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
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
        }
        if (ns & 2) {
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
        }

        while (i < ns && j < LEN0-3) {
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
        }

        x = allocVector (INTSXP, j + (ns-i));
        xi = INTEGER(x);
        memcpy (xi, xi0, j * sizeof(int));

        while (i < ns) {
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
            if ((v = si[i++]) != 0) {
                if (v > 0) xi[j++] = i;
                else { xi[j++] = NA_INTEGER; if (first_na == 0) first_na = j; }
            }
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
                              SEXP modified_obj, SEXP call)
{
    int i, ii, min, max, canstretch;

    canstretch = *stretch;
    *stretch = 0;

    for (i = 0; i < ns; i++) {
        ii = INTEGER(s)[i];
        if (ii != NA_INTEGER) 
            break;
    }

    *hasna = i>0;

    if (i==ns) /* all NA, or ns==0 */
        return s;

    min = ii;
    max = ii;
    for (i = i+1; i < ns; i++) {
        ii = INTEGER(s)[i];
        if (ii > max)  /* checked first since more common than ii < min */
            max = ii;
        else if (ii < min) {  /* includes the possibility that ii is NA */
            if (ii == NA_INTEGER) {
                if (!*hasna) *hasna = i+1;
            }
            else
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
        return s == modified_obj ? duplicate(s) : s;
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
   slow.  So now we hash.  Hashing is only worth doing if ns * nx is large. */

#define na_or_empty_string(strelt) ((strelt)==NA_STRING || CHAR((strelt))[0]==0)

static SEXP stringSubscript (SEXP s, int ns, int nx, SEXP names,
                             StringEltGetter strg, int *stretch, SEXP call)
{
    SEXP indx, indexnames;
    int i, j, k, nnames, sub, extra;
    int canstretch = *stretch;
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = names == R_NilValue ? FALSE :
        ns > 1000 ? (nx > 3) : nx > 1000 ? (ns > 10) : (ns*nx > 10*nx + 3*ns);
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
                    if (strg == (STRING_ELT)) {  /* do specially for speed */
                        for (j = 0; j < nnames; j++) {
                            SEXP sbe_j = STRING_ELT(names,j);
                            if (!na_or_empty_string(sbe_j) 
                                  && SEQL(sbe_i,sbe_j)) {
                                sub = j + 1;
                                break;
                            }
                        }
                    }
                    else {
                        for (j = 0; j < nnames; j++) {
                            SEXP sbe_j = strg(names,j);
                            if (!na_or_empty_string(sbe_j)
                                  && SEQL(sbe_i,sbe_j)) {
                                sub = j + 1;
                                break;
                            }
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
                            SET_VECTOR_ELT_NIL (indexnames, k);
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

static SEXP internalArraySubscript
            (int dim, SEXP s, SEXP dims, SEXP x, SEXP modified_obj, int *hasna)
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
	return integerSubscript(s, ns, nd, &stretch, hasna, modified_obj, call);
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, hasna, R_NoObject, call);
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


/* Inline function to handle positive scalar real and integer
   subscripts specially, putting them on the scalar stack, and
   otherwise call internalArraySubscript. */

static inline SEXP array_sub (SEXP sb, SEXP dim, int i, SEXP x, 
                              SEXP modified_obj, int *hasna)
{
    if ( (((1<<INTSXP) + (1<<REALSXP)) >> TYPEOF(sb)) & 1 ) {
        if (LENGTH(sb) == 1) {
            R_len_t dm, ix;
            dm = INTEGER(dim)[i];
            if (TYPEOF(sb) == REALSXP) {
                if (ISNAN(*REAL(sb)) || *REAL(sb) < 1 || *REAL(sb) > dm)
                    goto fallback;
                ix = (R_len_t) *REAL(sb);
            }
            else {
                ix = *INTEGER(sb);
                if (ix < 1 || ix > dm)
                    goto fallback;
            }
            *hasna = 0;
            return SCALAR_STACK_HAS_SPACE() ? PUSH_SCALAR_INTEGER(ix)
                                            : ScalarInteger(ix);
        }
    }

  fallback:
    return internalArraySubscript (i, sb, dim, x, modified_obj, hasna);
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
	return integerSubscript(s, ns, nd, &stretch, &hasna, x, call);
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, &hasna, R_NoObject, call);
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

static SEXP makeSubscript (SEXP x, SEXP s, int *stretch, int *hasna,
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
	ans = integerSubscript(s, ns, nx, stretch, hasna, 
                               used_to_replace ? x : R_NoObject, call);
	break;
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	ans = integerSubscript(tmp, ns, nx, stretch, hasna, R_NoObject, call);
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

    UNPROTECT(2);
    return ans;
}


/* ----------------------------- SUBSCRIPTS --------------------------------- */

/* Convert range to vector. The caller must ensure that its size won't 
   overflow. */

SEXP attribute_hidden Rf_VectorFromRange (int rng0, int rng1)
{ 
    SEXP vec;
    int n;

    if (rng1<rng0)
        return allocVector (INTSXP, 0);

    n = rng1-rng0+1;
  
    vec = allocVector(INTSXP, n);

    int *p = INTEGER(vec);
    int i = 0;

#   if !__AVX2__ || defined(DISABLE_AVX_CODE)
    {
        while (i < n) {
            p[i] = rng0+i; i += 1;
        }
    }
#   else
    {
        if ((SGGC_ALIGN_FORWARD & 8) && i < n-1) {
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
        }
        if ((SGGC_ALIGN_FORWARD & 16) && i < n-3) {
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
        }
        if (i < n-7) {
            __m256i eights = _mm256_set1_epi32 (8);
            __m256i vals = _mm256_add_epi32 (_mm256_set1_epi32(rng0+i),
                                             _mm256_set_epi32(7,6,5,4,3,2,1,0));
            do {
                _mm256_store_si256 ((__m256i*)(p+i), vals);
                vals = _mm256_add_epi32 (vals, eights);
                i += 8;
            } while (i < n-7);
        }
        if (i < n-3) {
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
        }
        if (i < n-1) {
            p[i] = rng0+i; i += 1;
            p[i] = rng0+i; i += 1;
        }
        if (i < n) {
            p[i] = rng0+i;
        }
    }
#   endif
  
    return vec;
}

/* Take a range (in seq) and either extract a range (possibly empty)
   of positive subscripts into start and end and return R_NoObject, or
   convert the range to a vector of negative integer subscripts that
   is returned. */

static SEXP Rf_DecideVectorOrRange(int64_t seq, int *start, int *end, SEXP call)
{
    int from, len;

    from = seq >> 32;
    len = (seq >> 1) & 0x7fffffff;

    if (from==0) {                     /* get rid of 0 subscript at beginning */
        from = 1;
        len -= 1;
    }
    if (from < 0 && from+(len-1) == 0) /* get rid of 0 subscript at end */
        len -= 1;

    if (from < 0 && from+(len-1) > 0) 
        errorcall(call, _("only 0's may be mixed with negative subscripts"));

    if (from > 0) {
        *start = from;
        *end = from+(len-1);
        return R_NoObject;
    }
    else {
        return Rf_VectorFromRange (from, from+(len-1));
    }
}


/* ExtractRange does the transfer of elements from "x" to the
   beginning of "result", according to the range within x given by
   "start" and "end" (1-based).  The caller will have allocated
   "result" to be at least the required length, and for VECSXP and
   EXPRSXP, the entries will be R_NilValue (done by allocVector).

   Arguments x and result must be protected by the caller. */

static void ExtractRange(SEXP x, SEXP result, int start, int end, SEXP call)
{
    int nx = length(x);

    SEXP tmp, tmp2;
    int n, m, i;

    start -= 1;
    n = end-start;
    m = start>=nx ? 0 : end<=nx ? n : nx-start;

    tmp = result;

    switch (TYPEOF(x)) {
    case LGLSXP:
        memcpy (LOGICAL(result), LOGICAL(x)+start, m * sizeof *LOGICAL(x));
        break;
    case INTSXP:
        memcpy (INTEGER(result), INTEGER(x)+start, m * sizeof *INTEGER(x));
        break;
    case REALSXP:
        memcpy (REAL(result), REAL(x)+start, m * sizeof *REAL(x));
        break;
    case CPLXSXP:
        memcpy (COMPLEX(result), COMPLEX(x)+start, m * sizeof *COMPLEX(x));
        break;
    case STRSXP:
        copy_string_elements (result, 0, x, start, m);
        break;
    case VECSXP:
    case EXPRSXP:
        if (!DUPVE || NAMEDCNT_EQ_0(x)) {
            copy_vector_elements (result, 0, x, start, m);
            if (NAMEDCNT_GT_0(x))
                for (i = 0; i<m; i++)
                    INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,i));
        }
        else {
            for (i = 0; i<m; i++)
                SET_VECTOR_ELT (result, i, duplicate(VECTOR_ELT(x,start+i)));
        }
        return;  /* remaining elements already set to R_NilValue */
    case LISTSXP:
        /* cannot happen: pairlists are coerced to lists */
        abort();
    case LANGSXP:
        for (i = 0; i<m; i++) {
            tmp2 = nthcdr(x, start+i);
            SETCAR(tmp, CAR(tmp2));
            SET_TAG(tmp, TAG(tmp2));
            tmp = CDR(tmp);
        }
        for ( ; i<n; i++) {
            SETCAR_NIL(tmp);
            tmp = CDR(tmp);
        }
        return;
    case RAWSXP:
        memcpy (RAW(result), RAW(x)+start, m * sizeof *RAW(x));
        break;
    default:
        abort();
    }

    if (m < n) Rf_set_elements_to_NA (result, m, 1, n);
}


/* ExtractSubset does the transfer of elements from "x" to "result" 
   according to the integer subscripts given in "indx". The caller will
   have allocated "result" to be at least the required length, and
   for VECSXP and EXPRSXP, the entries will be R_NilValue (done by
   allocVector).  

   Out of bound indexes (including 0 and NA) give NA.

   Arguments x and result must be protected by the caller. */

static void ExtractSubset(SEXP x, SEXP result, SEXP indx, SEXP call)
{
    int *ix = INTEGER(indx);
    int n = LENGTH(indx);
    int nx = LENGTH(x);
    int i, ii;

    switch (TYPEOF(x)) {
    case LGLSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx)
                LOGICAL(result)[i] = NA_LOGICAL;
            else
                LOGICAL(result)[i] = LOGICAL(x)[ii-1];
        break;
    case INTSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx)
                INTEGER(result)[i] = NA_INTEGER;
            else
                INTEGER(result)[i] = INTEGER(x)[ii-1];
        break;
    case REALSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx)
                REAL(result)[i] = NA_REAL;
            else
                REAL(result)[i] = REAL(x)[ii-1];
        break;
    case CPLXSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx) {
                COMPLEX(result)[i].r = NA_REAL;
                COMPLEX(result)[i].i = NA_REAL; 
            }
            else
                COMPLEX(result)[i] = COMPLEX(x)[ii-1];
        break;
    case STRSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx)
                SET_STRING_ELT_NA(result, i);
            else
                SET_STRING_ELT(result, i, STRING_ELT(x, ii-1));
        break;
    case VECSXP:
    case EXPRSXP:
        if (NAMEDCNT_EQ_0(x)) {
            for (i = 0; i<n; i++)
                if ((ii = ix[i]) <= 0 || ii > nx)
                    /* nothing, already R_NilValue */ ;
                else {
                    SEXP ve = VECTOR_ELT(x, ii-1);
                    SET_VECTOR_ELT(result, i, ve);
                    if (i > 0) INC_NAMEDCNT_0_AS_1(ve);
                }
        }
        else {
            for (i = 0; i<n; i++)
                if ((ii = ix[i]) <= 0 || ii > nx)
                    /* nothing, already R_NilValue */ ;
                else 
                    SET_VECTOR_ELEMENT_FROM_VECTOR(result, i, x, ii-1);
        }
        break;
    case LISTSXP:
	    /* cannot happen: pairlists are coerced to lists */
    case LANGSXP: ;
        SEXP tmp, tmp2;
        tmp = result;
        for (i = 0; i<n; i++) {
            if ((ii = ix[i]) <= 0 || ii > nx)
                SETCAR_NIL(tmp);
            else {
                tmp2 = nthcdr(x, ii-1);
                SETCAR(tmp, CAR(tmp2));
                SET_TAG(tmp, TAG(tmp2));
            }
            tmp = CDR(tmp);
        }
        break;
    case RAWSXP:
        for (i = 0; i<n; i++)
            if ((ii = ix[i]) <= 0 || ii > nx)
                RAW(result)[i] = (Rbyte) 0;
            else
                RAW(result)[i] = RAW(x)[ii-1];
        break;
    default:
        nonsubsettable_error(call,x);
    }
}


/* Check whether subscript is such that we suppress dropping dimensions 
   when the drop argument is NA (the default).  Assumes that it's not a
   missing argument (which needs to be checked separately). */

static inline int whether_suppress_drop (SEXP sb)
{
    SEXP d;
    return TYPEOF(sb) != LGLSXP 
             && HAS_ATTRIB(sb)
             && (d = getDimAttrib(sb)) != R_NilValue
             && LENGTH(d) == 1;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP subs, int64_t seq, int drop, SEXP call)
{
    SEXP sb = subs == R_NilValue ? R_MissingArg : CAR(subs);
    SEXP indx = R_NilValue;
    SEXP result, dims, attrib, nattrib;
    int start = 1, end = 0, n = 0;
    int suppress_drop = 0;
    int spi, ndim;

    /* R_NilValue is handled specially, always returning R_NilValue. */

    if (x == R_NilValue)
        return R_NilValue;

    /* SPECIAL KLUDGE:  To avoid breaking inexplicable code in some 
       packages, just return a duplicate of x if the subscripting has 
       the form x[,drop=FALSE]. */

    if (sb == R_MissingArg && drop == FALSE)
        return duplicate(x);

    PROTECT_WITH_INDEX (sb, &spi);
    dims = getDimAttrib(x);
    ndim = dims==R_NilValue ? 0 : LENGTH(dims);

    /* Check for variant result, which will be a range rather than a vector, 
       and if we have a range, see whether it can be used directly, or must
       be converted to a vector to be handled as other vectors. */

    if (seq) {
        suppress_drop = seq & 1;  /* get flag for seq having a dim attr */
        REPROTECT(sb = Rf_DecideVectorOrRange(seq,&start,&end,call), spi);
        if (sb == R_NoObject)
            n = end - start + 1;
    }

    /* If subscript is missing, convert to range over entire vector. 
       Also, suppress dropping if it was missing from '_'. */

    if (sb == R_MissingArg) {
        suppress_drop = subs != R_NilValue && MISSING(subs) == 2;
        start = 1;
        end = length(x);
        n = end;
        sb = R_NoObject;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    if (sb != R_NoObject && isMatrix(sb) && isArray(x) && ncols(sb) == ndim) {
        if (isString(sb)) {
            sb = strmat2intmat(sb, GetArrayDimnames(x), call);
            REPROTECT(sb,spi);
        }
        if (isInteger(sb) || isReal(sb)) {
            sb = mat2indsub(dims, sb, call);
            REPROTECT(sb,spi);
        }
    }

    /* Convert sb to a vector of integer subscripts (unless we have a range).
       We suppress dropping when drop is NA when the index is not logical
       and is a 1D array. */

    if (sb != R_NoObject) {
        int stretch = 1;  /* allow out of bounds, not for assignment */
        int hasna;
        PROTECT(indx = makeSubscript(x, sb, &stretch, &hasna, call, 0));
        n = LENGTH(indx);
        if (drop == NA_LOGICAL)
            suppress_drop = whether_suppress_drop(sb);
    }

    /* Allocate and extract the result. */

    PROTECT (result = allocVector(TYPEOF(x),n));
    if (sb==R_NoObject)
        ExtractRange(x, result, start, end, call);
    else 
        ExtractSubset(x, result, indx, call);

    if (((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
        ( /* here we might have an array.  Use row names if 1D */
            ndim == 1 &&
            (attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
            (attrib = GetRowNames(attrib)) != R_NilValue
            )
        ) {
        PROTECT(attrib);
        PROTECT(nattrib = allocVector(TYPEOF(attrib), n));
        if (sb==R_NoObject)
            ExtractRange(attrib, nattrib, start, end, call);
        else
            ExtractSubset(attrib, nattrib, indx, call);
        setAttrib(result, R_NamesSymbol, nattrib);
        UNPROTECT(2);
    }
    if ((attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
        TYPEOF(attrib) == VECSXP) {
        PROTECT(attrib);
        PROTECT(nattrib = allocVector(VECSXP, n));
        if (sb==R_NoObject)
            ExtractRange(attrib, nattrib, start, end, call);
        else
            ExtractSubset(attrib, nattrib, indx, call);
        setAttrib(result, R_SrcrefSymbol, nattrib);
           UNPROTECT(2);
    }

    /* FIXME: this is wrong, because the slots are gone, so result is
       an invalid object of the S4 class! JMC 3/3/09 */

    UNPROTECT(2 + (sb!=R_NoObject));

    /* One-dimensional arrays should have their dimensions dropped only 
       if the result has length one and drop TRUE or is NA_LOGICAL without
       the drop being suppressed by the index being a 1D array. */

    if (ndim == 1) {
        int len = length(result);

        if (len > 1 || drop == FALSE || drop == NA_LOGICAL && suppress_drop) {
            SEXP attr;
            PROTECT(result);
            PROTECT(attr = allocVector1INT());
            INTEGER(attr)[0] = len;
            if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
                /* reinstate dimnames, include names of dimnames, which
                   should be in the names attribute at this point. */
                PROTECT(attrib = dup_top_level(attrib));
                SET_VECTOR_ELT(attrib, 0, getAttrib(result, R_NamesSymbol));
                setAttrib(result, R_DimSymbol, attr);
                setAttrib(result, R_DimNamesSymbol, attrib);
                setAttrib(result, R_NamesSymbol, R_NilValue);
                UNPROTECT(1);
            }
            else 
                setAttrib(result, R_DimSymbol, attr);
            UNPROTECT(2);
        }
    }

    return result;
}


/* Used in MatrixSubset when only one (valid) row is accessed. */

static void one_row_of_matrix (SEXP call, SEXP x, SEXP result, 
                               int ii, int nr, SEXP sc, int ncs, int nc)
{
    int typeofx = TYPEOF(x);
    int j, jj, st;

    st = (ii-1) - nr;

    for (j = 0; j < ncs; j++) {

        jj = INTEGER(sc)[j];

        if (jj == NA_INTEGER) {
            Rf_set_elements_to_NA (result, j, 1, j+1);
            continue;
        }

        if (jj < 1 || jj > nc)
            out_of_bounds_error(call);

        switch (typeofx) {
        case LGLSXP:
            LOGICAL(result)[j] = LOGICAL(x) [st+jj*nr];
            break;
        case INTSXP:
            INTEGER(result)[j] = INTEGER(x) [st+jj*nr];
            break;
        case REALSXP:
            REAL(result)[j] = REAL(x) [st+jj*nr];
            break;
        case CPLXSXP:
            COMPLEX(result)[j] = COMPLEX(x) [st+jj*nr];
            break;
        case STRSXP:
            SET_STRING_ELT(result, j, STRING_ELT(x, st+jj*nr));
            break;
        case VECSXP: ;
            SET_VECTOR_ELEMENT_FROM_VECTOR(result, j, x, st+jj*nr);
            break;
        case RAWSXP:
            RAW(result)[j] = RAW(x) [st+jj*nr];
            break;
        default:
            abort();
        }
    }
}


/* Used in MatrixSubset for subsetting with range of rows. */

static void range_of_rows_of_matrix (SEXP call, SEXP x, SEXP result, 
    int start, int nrs, int nr, SEXP sc, int ncs, int nc)
{
    int i, j, jj, ij, jjnr;

    start -= 1;

    if (start < 0 || start+nrs > nr)
        out_of_bounds_error(call);

    /* Loop to handle extraction, with outer loop over columns. */

    ij = 0;
    for (j = 0; j < ncs; j++) {
        jj = INTEGER(sc)[j];

        /* If column index is NA, just set column of result to NAs. */

        if (jj == NA_INTEGER) {
            Rf_set_elements_to_NA (result, ij, 1, ij+nrs);
            ij += nrs;
            continue;
        }

        /* Check for bad column index. */

        if (jj < 1 || jj > nc)
            out_of_bounds_error(call);

        /* Loops over range of rows. */

        jjnr = (jj-1) * nr + start;
        switch (TYPEOF(x)) {
        case LGLSXP:
            memcpy (LOGICAL(result)+ij, LOGICAL(x)+jjnr, 
                    nrs * sizeof *LOGICAL(x));
            break;
        case INTSXP:
            memcpy (INTEGER(result)+ij, INTEGER(x)+jjnr, 
                    nrs * sizeof *INTEGER(x));
            break;
        case REALSXP:
            memcpy (REAL(result)+ij, REAL(x)+jjnr, 
                    nrs * sizeof *REAL(x));
            break;
        case CPLXSXP:
            memcpy (COMPLEX(result)+ij, COMPLEX(x)+jjnr, 
                    nrs * sizeof *COMPLEX(x));
            break;
        case STRSXP:
            copy_string_elements (result, ij, x, jjnr, nrs);
            break;
        case VECSXP:
            if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                copy_vector_elements (result, ij, x, jjnr, nrs);
                if (NAMEDCNT_GT_0(x))
                    for (i = 0; i<nrs; i++)
                        INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,ij+i));
            }
            else {
                for (i = 0; i<nrs; i++)
                    SET_VECTOR_ELT (result, ij+i, 
                                    duplicate(VECTOR_ELT(x,jjnr+i)));
            }
            break;
        case RAWSXP:
            memcpy (RAW(result)+ij, RAW(x)+jjnr, 
                    nrs * sizeof *RAW(x));
            break;
        default:
            abort();
        }

        ij += nrs;
    }
}


/* Used in MatrixSubset for the general case of subsetting. */

static void multiple_rows_of_matrix (SEXP call, SEXP x, SEXP result, 
    SEXP sr, int nrs, int nr, SEXP sc, int ncs, int nc)
{
    int i, j, ii, jj, ij, jjnr;

    /* Set rows of result to NAs where there are NA row indexes.  Also check 
       for bad row indexes (once here rather than many times in loop). */

    for (i = 0; i < nrs; i++) {
        ii = INTEGER(sr)[i];
        if (ii == NA_INTEGER) 
            Rf_set_elements_to_NA (result, i, nrs, i+nrs*ncs);
        else if (ii < 1 || ii > nr)
            out_of_bounds_error(call);
    }

    /* Loop to handle extraction except for NAs.  Outer loop is over columns so
       writes are sequential, which is faster for indexing, and probably better
       for memory speed. */

    for (j = 0, ij = 0; j < ncs; j++) {
        jj = INTEGER(sc)[j];

        /* If column index is NA, just set column of result to NAs. */

        if (jj == NA_INTEGER) {
            Rf_set_elements_to_NA (result, j*nrs, 1, (j+1)*nrs);
            ij += nrs;
            continue;
        }

        /* Check for bad column index. */

        if (jj < 1 || jj > nc)
            out_of_bounds_error(call);

        /* Loops over row indexes, except skips NA row indexes, done above. */

        int *sri = INTEGER(sr);
        jjnr = (jj-1) * nr;
        switch (TYPEOF(x)) {
        case LGLSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    LOGICAL(result)[ij] = LOGICAL(x)[(ii-1)+jjnr];
            break;
        case INTSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    INTEGER(result)[ij] = INTEGER(x)[(ii-1)+jjnr];
            break;
        case REALSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    REAL(result)[ij] = REAL(x)[(ii-1)+jjnr];
            break;
        case CPLXSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    COMPLEX(result)[ij] = COMPLEX(x)[(ii-1)+jjnr];
            break;
        case STRSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    SET_STRING_ELT(result, ij, STRING_ELT(x, (ii-1)+jjnr));
            break;
        case VECSXP:
            if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                for (i = 0; i < nrs; i++, ij++) 
                    if ((ii = sri[i]) != NA_INTEGER) {
                        SEXP ve = VECTOR_ELT(x, (ii-1)+jjnr);
                        SET_VECTOR_ELT (result, ij, ve);
                        INC_NAMEDCNT_0_AS_1(ve);
                    }
            }
            else {
                for (i = 0; i < nrs; i++, ij++) 
                    if ((ii = sri[i]) != NA_INTEGER) 
                        SET_VECTOR_ELT (result, ij, 
                          duplicate(VECTOR_ELT(x,(ii-1)+jjnr)));
            }
            break;
        case RAWSXP:
            for (i = 0; i < nrs; i++, ij++) 
                if ((ii = sri[i]) != NA_INTEGER) 
                    RAW(result)[ij] = RAW(x)[(ii-1)+jjnr];
            break;
        default:
            abort();
        }
    }
}


/* Subset for a vector with dim attribute specifying two dimensions. */

static SEXP MatrixSubset(SEXP x, SEXP subs, SEXP call, int drop, int64_t seq)
{
    SEXP s0 = CAR(subs), s1 = CADR(subs);
    SEXP dims, result, sr, sc;
    int rhasna = 0, chasna = 0;
    int start = 1, end = 0;
    int nr, nc, nrs = 0, ncs = 0;
    int suppress_drop_row = 0, suppress_drop_col = 0;
    int ii;

    PROTECT2(x,subs);
    int nprotect = 2;

    SEXP dim = getDimAttrib(x);

    nr = INTEGER(dim)[0];
    nc = INTEGER(dim)[1];

    /* s0 is set to R_NoObject when we have a range for the first subscript */

    if (s0 == R_MissingArg) {
        suppress_drop_row = MISSING(subs)==2;
        start = 1;
        end = nr;
        nrs = nr;
        s0 = R_NoObject;
    }
    else if (seq) {
        suppress_drop_row = seq & 1;
        PROTECT(s0 = Rf_DecideVectorOrRange(seq,&start,&end,call));
        nprotect++;
        if (s0 == R_NoObject)
            nrs = end - start + 1;
    }

    SEXP sv_scalar_stack = R_scalar_stack;

    if (s0 != R_NoObject) {
        if (drop == NA_LOGICAL) 
            suppress_drop_row = whether_suppress_drop(s0);
        PROTECT (sr = array_sub (s0, dim, 0, x, R_NoObject, &rhasna));
        nprotect++;
        nrs = LENGTH(sr);
    }

    if (drop == NA_LOGICAL) 
        suppress_drop_col = s1 == R_MissingArg ? MISSING(CDR(subs)) == 2 
                                               : whether_suppress_drop(s1);

    if (drop == FALSE)
        suppress_drop_row = suppress_drop_col = 1;

    PROTECT (sc = array_sub (s1, dim, 1, x, R_NoObject, &chasna));
    nprotect++;
    ncs = LENGTH(sc);

    if (nrs < 0 || ncs < 0)
        abort();  /* shouldn't happen, but code was conditional before... */

    /* Check this does not overflow */
    if ((double)nrs * (double)ncs > R_LEN_T_MAX)
        error(_("dimensions would exceed maximum size of array"));

    PROTECT (result = allocVector(TYPEOF(x), nrs*ncs));
    nprotect++;

    /* Extract elements from matrix x to result. */

    if (s0 == R_NoObject)
        range_of_rows_of_matrix(call, x, result, start, nrs, nr, sc, ncs, nc);
    else if (nrs == 1 && (ii = INTEGER(sr)[0]) != NA_INTEGER 
                      && ii >= 0 && ii <= nr)
        one_row_of_matrix (call, x, result, ii, nr, sc, ncs, nc);
    else
        multiple_rows_of_matrix (call, x, result, sr, nrs, nr, sc, ncs, nc);

    /* Set up dimnames of the returned value.  Not attached to result yet. */

    SEXP dimnames, dimnamesnames, newdimnames;
    PROTECT(dimnames = getAttrib(x, R_DimNamesSymbol));
    nprotect++;

    if (dimnames == R_NilValue)
        newdimnames = R_NilValue;
    else {
        PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
        nprotect++;
        PROTECT(newdimnames = allocVector(VECSXP, 2));
        nprotect++;
        if (TYPEOF(dimnames) == VECSXP) {
            if (VECTOR_ELT(dimnames,0) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 0, allocVector(STRSXP, nrs));
                if (s0 == R_NoObject)
                    ExtractRange (VECTOR_ELT(dimnames,0),
                      VECTOR_ELT(newdimnames,0), start, end, call);
                else
                    ExtractSubset(VECTOR_ELT(dimnames,0),
                      VECTOR_ELT(newdimnames,0), sr, call);
            }
            if (VECTOR_ELT(dimnames,1) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 1, allocVector(STRSXP, ncs));
                ExtractSubset(VECTOR_ELT(dimnames,1),
                  VECTOR_ELT(newdimnames,1), sc, call);
            }
        }
        else {
            if (CAR(dimnames) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 0, allocVector(STRSXP, nrs));
                if (s0 == R_NoObject)
                    ExtractRange (CAR(dimnames),
                      VECTOR_ELT(newdimnames,0), start, end, call);
                else
                    ExtractSubset(CAR(dimnames),
                      VECTOR_ELT(newdimnames,0), sr, call);
            }
            if (CADR(dimnames) != R_NilValue) {
                SET_VECTOR_ELT (newdimnames, 1, allocVector(STRSXP, ncs));
                ExtractSubset(CADR(dimnames),
                  VECTOR_ELT(newdimnames,1), sc, call);
            }
        }
        setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
    }

    /* Set up dimensions attribute and attach dimnames, unless dimensions
       will be dropped (in which case names attribute may be attached). */

    if (LENGTH(result) == 1 && suppress_drop_row + suppress_drop_col != 2) {
        if (newdimnames != R_NilValue) {
            /* attach names if unambiguous which are wanted */
            SEXP rn = VECTOR_ELT(newdimnames,0);
            SEXP cn = VECTOR_ELT(newdimnames,1);
            if (rn == R_NilValue || suppress_drop_col)
                setAttrib (result, R_NamesSymbol, cn);
            else if (cn == R_NilValue || suppress_drop_row)
                setAttrib (result, R_NamesSymbol, rn);
        }
    }
    else if (nrs == 1 && !suppress_drop_row) {
        if (newdimnames != R_NilValue)
            setAttrib (result, R_NamesSymbol, VECTOR_ELT(newdimnames,1));
    }
    else if (ncs == 1 && !suppress_drop_col) {
        if (newdimnames != R_NilValue)
            setAttrib (result, R_NamesSymbol, VECTOR_ELT(newdimnames,0));
    }
    else {
        PROTECT(dims = allocVector(INTSXP, 2));
        nprotect++;
        INTEGER(dims)[0] = nrs;
        INTEGER(dims)[1] = ncs;
        setAttrib(result, R_DimSymbol, dims);
        setAttrib(result, R_DimNamesSymbol, newdimnames);
    }

    UNPROTECT(nprotect);
    R_scalar_stack = sv_scalar_stack;
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop, SEXP xdims, int k)
{
    int i, j, ii, jj, n;
    SEXP dimnames, dimnamesnames, r, result;
    int mode = TYPEOF(x);

    int *subs[k], indx[k], nsubs[k], offset[k], suppress_drop[k];
    SEXP subv[k];

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT3(x,s,xdims);

    n = 1; r = s;
    for (i = 0; i < k; i++) {
        int hasna;
        if (drop == TRUE)
            suppress_drop[i] = 0;
        else if (drop == FALSE)
            suppress_drop[i] = 1;
        else /* drop == NA_LOGICAL */ 
            suppress_drop[i] = CAR(r) == R_MissingArg ? MISSING(r) == 2
                                : whether_suppress_drop(CAR(r));
        PROTECT (subv[i] = array_sub (CAR(r), xdims, i, x, R_NoObject, &hasna));
        subs[i] = INTEGER(subv[i]);
	nsubs[i] = LENGTH(subv[i]);
        n *= nsubs[i];
        indx[i] = 0;
	r = CDR(r);
    }

    offset[1] = INTEGER(xdims)[0];  /* offset[0] is not used */
    for (i = 2; i < k; i++)
        offset[i] = offset[i-1] * INTEGER(xdims)[i-1];

    /* Check for out-of-bounds indexes.  Disabled, since it seems unnecessary,
       given that arraySubscript checks bounds. */

#if 0
    for (j = 0; j < k; j++) {
        for (i = 0; i < nsubs[j]; i++) {
            jj = subs[j][i];
            if (jj != NA_INTEGER && (jj < 1 || jj > INTEGER(xdims)[j])) {
                out_of_bounds_error(call);
            }
        }
    }
#endif

    /* Vector to contain the returned values. */

    PROTECT(result = allocVector(mode, n));

    if (n == 0) goto done;

    /* Transfer the subset elements from "x" to "a". */

    for (i = 0; ; i++) {

        jj = subs[0][indx[0]];
        if (jj != NA_INTEGER) {
            ii = jj-1;
            for (j = 1; j < k; j++) {
                jj = subs[j][indx[j]];
                if (jj == NA_INTEGER)
                    break;
                ii += (jj-1) * offset[j];
            }
        }

        if (jj != NA_INTEGER) {
            switch (mode) {
            case LGLSXP:
                LOGICAL(result)[i] = LOGICAL(x)[ii];
                break;
            case INTSXP:
                INTEGER(result)[i] = INTEGER(x)[ii];
                break;
            case REALSXP:
                REAL(result)[i] = REAL(x)[ii];
                break;
            case CPLXSXP:
                COMPLEX(result)[i] = COMPLEX(x)[ii];
                break;
            case STRSXP:
                SET_STRING_ELT(result, i, STRING_ELT(x, ii));
                break;
            case VECSXP:
                if (!DUPVE || NAMEDCNT_EQ_0(x)) {
                    SET_VECTOR_ELT (result, i, VECTOR_ELT(x, ii));
                    INC_NAMEDCNT_0_AS_1(VECTOR_ELT(result,i));
                }
                else
                    SET_VECTOR_ELT (result, i, duplicate(VECTOR_ELT(x,ii)));
                break;
            case RAWSXP:
                RAW(result)[i] = RAW(x)[ii];
                break;
            default:
                errorcall (call, 
                           _("array subscripting not handled for this type"));
                break;
            }
        }
        else { /* jj == NA_INTEGER */
            switch (mode) {
            case LGLSXP:
                LOGICAL(result)[i] = NA_LOGICAL;
                break;
            case INTSXP:
                INTEGER(result)[i] = NA_INTEGER;
                break;
            case REALSXP:
                REAL(result)[i] = NA_REAL;
                break;
            case CPLXSXP:
                COMPLEX(result)[i].r = NA_REAL;
                COMPLEX(result)[i].i = NA_REAL;
                break;
            case STRSXP:
                SET_STRING_ELT_NA(result, i);
                break;
            case VECSXP:
                SET_VECTOR_ELT_NIL(result, i);
                break;
            case RAWSXP:
                RAW(result)[i] = (Rbyte) 0;
                break;
            default:
                errorcall (call, 
                           _("array subscripting not handled for this type"));
                break;
            }
        }

        j = 0;
        while (++indx[j] >= nsubs[j]) {
            indx[j] = 0;
            if (++j >= k) goto done;
        }
    }

  done: ;

    /* Set up dimnames for result, but don't attach to result yet. */

    SEXP newdimnames;
    PROTECT(dimnames = getAttrib(x, R_DimNamesSymbol));
    PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
    if (TYPEOF(dimnames) == VECSXP) { /* broken code for others in R-2.15.0 */
        PROTECT(newdimnames = allocVector(VECSXP, k));
        for (i = 0; i < k ; i++) {
            if (nsubs[i] > 0 && VECTOR_ELT(dimnames,i) != R_NilValue) {
                SET_VECTOR_ELT(newdimnames, i, allocVector(STRSXP, nsubs[i]));
                ExtractSubset(VECTOR_ELT(dimnames,i), VECTOR_ELT(newdimnames,i),
                              subv[i], call);
            } 
            /* else leave as NULL for 0-length dims */
        }
        setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
    }
    else
        PROTECT(newdimnames = R_NilValue);

    /* See if dropping down to a vector. */

    int rdims = 0;
    for (i = 0; i < k; i++) {
        if (nsubs[i] != 1 || suppress_drop[i])
            rdims += 1;
    }

    if (rdims <= 1) { /* result is vector without dims, but maybe with names */
        if (newdimnames != R_NilValue) {
            int w = -1;   /* which dimension to take names from, -1 if none */
            for (i = 0; i < k; i++) {
                if (VECTOR_ELT(newdimnames,i) != R_NilValue) {
                    if (w < 0 || nsubs[i] != 1 || suppress_drop[i])
                        w = i;
                    else if (!suppress_drop[w]) {
                        w = -1;
                        break;
                    }
                }
            }
            if (w >= 0)
                setAttrib (result, R_NamesSymbol, VECTOR_ELT(newdimnames,w));
        }
    }

    else { /* not dropping down to a vector */
        SEXP newdims;
        PROTECT (newdims = allocVector(INTSXP, k));
        for (i = 0 ; i < k ; i++)
            INTEGER(newdims)[i] = nsubs[i];
        setAttrib (result, R_DimSymbol, newdims);
        setAttrib (result, R_DimNamesSymbol, newdimnames);
        if (drop == TRUE)
            DropDims(result);
        else if (drop == NA_LOGICAL)
            DropDimsNotSuppressed(result,suppress_drop);
        UNPROTECT(1); /* newdims */
    }

    UNPROTECT(k+7); /* ... + result, dimnames, dimnamesnames, newdimnames,
                             x, s, xdims */

    R_scalar_stack = sv_scalar_stack;
    return result;
}


/* Returns and removes a named argument from the argument list 
   pointed to by args_ptr, and updates args_ptr to account for
   the removal (when it was at the head).

   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NoObject is is returned.

   The caller does not need to protect *args_ptr before.
 */
static SEXP ExtractArg(SEXP *args_ptr, SEXP arg_sym)
{
    SEXP prev_arg = R_NoObject;
    SEXP arg;

    for (arg = *args_ptr; arg != R_NilValue; arg = CDR(arg)) {
	if(TAG(arg) == arg_sym) {
	    if (prev_arg == R_NoObject) /* found at head of args */
		*args_ptr = CDR(arg);
	    else
		SETCDR(prev_arg, CDR(arg));
	    return CAR(arg);
	}
	prev_arg = arg;
    }
    return R_NoObject;
}

/* Extracts the drop argument, if present, from the argument list.
   The argument list will be modified, and the pointer passed changed
   if the first argument is deleted.  The caller does not need to
   protect *args_ptr before.  The value is FALSE, TRUE, or NA_LOGICAL,
   with NA_LOGICAL being the default when no drop argument is present.
   When used as a C boolean, NA_LOGICAL will have the same effect as
   TRUE, but NA_LOGICAL will sometimes allow dropping to be suppressed
   when a vector index has a 1D dim attribute. */

static int ExtractDropArg(SEXP *args_ptr)
{
    SEXP drop_arg = ExtractArg(args_ptr, R_DropSymbol);
    return drop_arg != R_NoObject ? asLogical(drop_arg) : NA_LOGICAL;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used

   The argument list pointed to by args_ptr may be modified.  The
   caller does not need to protect *args_ptr before. */

static int ExtractExactArg(SEXP *args_ptr)
{
    SEXP argval = ExtractArg(args_ptr, R_ExactSymbol);
    if (argval == R_NoObject) return 1; /* Default is true as from R 2.7.0 */
    int exact = asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}


/* Returns simple (positive or negative) index, with no dim attribute, or 
   zero if not so simple. */

static inline R_len_t simple_index (SEXP s)
{
    int type_etc = TYPE_ETC(s);

    if (type_etc == REALSXP) goto real;
    if (type_etc == INTSXP) goto integer;
    
    if ((type_etc & TYPE_ET_CETERA_VEC_DOTS_TR) /* not scalar */
         || getDimAttrib(s) != R_NilValue)
        return 0;
    
    int type = type_etc & TYPE_ET_CETERA_TYPE;

    if (type == REALSXP) goto real;
    if (type == INTSXP) goto integer;

    return 0;

  real:
    if (REAL(s)[0]<=R_LEN_T_MAX && REAL(s)[0]>=-R_LEN_T_MAX) /* false if NaN */
        return (R_len_t) REAL(s)[0];
    else 
        return 0;

  integer:
    if (INTEGER(s)[0] != NA_INTEGER)
        return INTEGER(s)[0];
    else
        return 0;
}


/* Look for the simple case of subscripting an atomic vector with one 
   valid integer or real subscript that is positive or negative (not zero, 
   NA, or out of bounds), with no dim attribute.  Returns the result, or 
   R_NilValue if it's not so simple.  Return result may be on scalar stack,
   if variant allows.

   The arguments x and s do not need to be protected before this
   function is called.  It's OK for x to still be being computed. The
   variant for the return result is the last argument. */

static SEXP one_vector_subscript (SEXP x, SEXP s, int variant)
{
    R_len_t ix, n;
    int typeofx;
    SEXP r;

    typeofx = TYPEOF(x);

    if (!isVectorAtomic(x))
        return R_NilValue;

    n = LENGTH(x);
    ix = simple_index (s);

    if (ix>0) {

        if (ix > n) 
            return R_NilValue;

        R_len_t avail;
        ix -= 1;
        if (helpers_is_being_computed(x)) {
            helpers_start_computing_var(x);
            HELPERS_WAIT_IN_VAR (x, avail, ix, n);
        }
        switch (typeofx) {
        case LGLSXP:  
            return ScalarLogicalMaybeConst (LOGICAL(x)[ix]);
        case INTSXP:  
            if (CAN_USE_SCALAR_STACK(variant))
                return PUSH_SCALAR_INTEGER(INTEGER(x)[ix]);
            else
                return ScalarIntegerMaybeConst(INTEGER(x)[ix]);
        case REALSXP: 
            if (CAN_USE_SCALAR_STACK(variant))
                return PUSH_SCALAR_REAL(REAL(x)[ix]);
            else
                return ScalarRealMaybeConst(REAL(x)[ix]);
        case RAWSXP:  
            return ScalarRawMaybeConst (RAW(x)[ix]);
        case STRSXP:  
            return ScalarStringMaybeConst (STRING_ELT(x,ix));
        case CPLXSXP: 
            return ScalarComplexMaybeConst (COMPLEX(x)[ix]);
        default: abort();
        }
    }

    else if (ix < 0) {

        if (ix < -n) 
            return R_NilValue;

        R_len_t ex;

        WAIT_UNTIL_COMPUTED(x);
        PROTECT(x);
        r = allocVector (typeofx, n-1);

        ix = -ix-1;
        ex = n-ix-1;

        switch (typeofx) {
        case LGLSXP: 
            if (ix!=0) memcpy(LOGICAL(r), LOGICAL(x), ix * sizeof *LOGICAL(r));
            if (ex!=0) memcpy(LOGICAL(r)+ix, LOGICAL(x)+ix+1, 
                                                      ex * sizeof *LOGICAL(r));
            break;
        case INTSXP: 
            if (ix!=0) memcpy(INTEGER(r), INTEGER(x), ix * sizeof *INTEGER(r));
            if (ex!=0) memcpy(INTEGER(r)+ix, INTEGER(x)+ix+1, 
                                                      ex * sizeof *INTEGER(r));
            break;
        case REALSXP: 
            if (ix!=0) memcpy(REAL(r), REAL(x), ix * sizeof *REAL(r));
            if (ex!=0) memcpy(REAL(r)+ix, REAL(x)+ix+1, ex * sizeof *REAL(r));
            break;
        case RAWSXP: 
            if (ix!=0) memcpy(RAW(r), RAW(x), ix * sizeof *RAW(r));
            if (ex!=0) memcpy(RAW(r)+ix, RAW(x)+ix+1, ex * sizeof *RAW(r));
            break;
        case STRSXP: 
            if (ix!=0) copy_string_elements (r, 0, x, 0, ix);
            if (ex!=0) copy_string_elements (r, ix, x, ix+1, ex); 
            break;
        case CPLXSXP: 
            if (ix!=0) memcpy(COMPLEX(r), COMPLEX(x), ix * sizeof *COMPLEX(r));
            if (ex!=0) memcpy(COMPLEX(r)+ix, COMPLEX(x)+ix+1, 
                                                      ex * sizeof *COMPLEX(r));
            break;
        default: abort();
        }

        UNPROTECT(1);
        return r;
    }

    else  /* ix == 0 */
        return R_NilValue;
}


/* Look for the simple case of subscripting an atomic matrix with two
   valid integer or real subscript that are positive (not negative, zero, 
   NA, or out of bounds), with no dim attribute.  Returns the result, or 
   R_NilValue if it's not so simple.  The arguments x, dim, s1, and s2 do 
   not need to be protected before this function is called. It's OK for x to 
   still be being computed. The variant for the return result is the last 
   argument. */

static SEXP two_matrix_subscripts (SEXP x, SEXP dim, SEXP s1, SEXP s2, 
                                          int variant)
{
    R_len_t ix1, ix2, nrow, ncol, avail, e;

    if (!isVectorAtomic(x))
        return R_NilValue;

    nrow = INTEGER(dim)[0];
    ix1 = simple_index (s1);
    if (ix1 <= 0 || ix1 > nrow)
        return R_NilValue;

    ncol = INTEGER(dim)[1];
    ix2 = simple_index (s2);
    if (ix2 <= 0 || ix2 > ncol)
        return R_NilValue;

    e = (ix1 - 1) + nrow * (ix2 - 1);

    if (helpers_is_being_computed(x)) {
        helpers_start_computing_var(x);
        HELPERS_WAIT_IN_VAR (x, avail, e, LENGTH(x));
    }

    switch (TYPEOF(x)) {
    case LGLSXP:  
        return ScalarLogicalMaybeConst (LOGICAL(x)[e]);
    case INTSXP:  
        if (CAN_USE_SCALAR_STACK(variant))
            return PUSH_SCALAR_INTEGER(INTEGER(x)[e]);
        else
            return ScalarIntegerMaybeConst(INTEGER(x)[e]);
    case REALSXP: 
        if (CAN_USE_SCALAR_STACK(variant))
            return PUSH_SCALAR_REAL(REAL(x)[e]);
        else
            return ScalarRealMaybeConst(REAL(x)[e]);
    case RAWSXP:  
        return ScalarRawMaybeConst (RAW(x)[e]);
    case STRSXP:  
        return ScalarStringMaybeConst (STRING_ELT(x,e));
    case CPLXSXP: 
        return ScalarComplexMaybeConst (COMPLEX(x)[e]);
    default: abort();
    }
}


/* The do_subset function implementing the "[" subset operator is in eval.c. */

/* do_subset_dflt and do_subset_dflt_seq are called from there and elsewhere
   outside this module. */

/* do_subset_dflt doesn't have the "seq" argument of do_subset_dflt_seq, 
   and takes all arguments as an arg list. */

SEXP attribute_hidden do_subset_dflt (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args);
    args = CDR(args);
    
    if (args == R_NilValue || TAG(args) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, R_NoObject, R_NoObject, 
                                   args, rho, 0, 0);
    else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
        return do_subset_dflt_seq (call, op, x, CAR(args), R_NoObject,
                                   CDR(args), rho, 0, 0);
    else
        return do_subset_dflt_seq (call, op, x, CAR(args), CADR(args),
                                   CDDR(args), rho, 0, 0);
}

/* The "seq" argument below is non-zero if the first subscript is a sequence
   specification (a variant result), in which case it encodes the start, 
   length, and whether .. properties of the sequence.

   The first argument (the array, x) is passed separately rather than
   as part of an argument list, for efficiency.  If sb1 is not R_NoObject, 
   it is the first subscript, which has no tag.  Similarly for sb2.
   Remaining subscripts and other arguments are in the pairlist subs.

   May return its result on the scalar stack, depending on variant.

   Sets R_Visible to TRUE.

   Note:  x, sb1, and subs need not be protected on entry. */

SEXP attribute_hidden do_subset_dflt_seq (SEXP call, SEXP op, SEXP x, 
                                          SEXP sb1, SEXP sb2, SEXP subs, 
                                          SEXP rho, int variant, int64_t seq)
{
    int drop, i, nsubs, type;
    SEXP ans, ax, px;

    R_Visible = TRUE;

    if (seq == 0 && sb1 != R_NoObject && subs==R_NilValue) {

        if (sb2 == R_NoObject) {  /* handle simples cases with one subscript */
            SEXP attr = ATTRIB(x);
            if (attr == R_NilValue         /* no attributes except maybe dim */
                 || (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue)) {
                SEXP r = one_vector_subscript (x, sb1, variant);
                if (r != R_NilValue)
                    return r;
            }
        }

        else {  /* handle simple cases with two subscripts*/
            SEXP attr = ATTRIB(x);
            if (TAG(attr) == R_DimSymbol 
                  && CDR(attr) == R_NilValue) {        /* only has a dim attr */
                SEXP dim = CAR(attr);
                if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {  /* a matrix */
                    SEXP r = two_matrix_subscripts (x, dim, sb1, sb2, variant);
                    if (r != R_NilValue)
                        return r;
                }
            }
        }
    }

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */

    if (x == R_NilValue)
	return x;

    PROTECT3(x,sb1,sb2);

    drop = ExtractDropArg(&subs);
    if (sb2 != R_NoObject)
        subs = CONS (sb2, subs);
    if (sb1 != R_NoObject) 
        subs = CONS (sb1, subs);
    PROTECT(subs);

    WAIT_UNTIL_COMPUTED(x);

    nsubs = length(subs);
    type = TYPEOF(x);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    ax = x;
    if (isVector(x))
	PROTECT(ax);
    else if (isPairList(x)) {
	SEXP dim = getDimAttrib(x);
	int ndim = length(dim);
	if (ndim > 1) {
	    PROTECT(ax = allocArray(VECSXP, dim));
	    setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    PROTECT(ax = allocVector(VECSXP, length(x)));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
        SET_NAMEDCNT(ax,NAMEDCNT(x));
	for(px = x, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else
        nonsubsettable_error(call,x);

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    if(nsubs < 2)
	PROTECT(ans = VectorSubset(ax, subs, seq, drop, call));
    else {
        SEXP xdims = getDimAttrib(x);
	if (nsubs != length(xdims))
	    errorcall(call, _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, subs, call, drop, seq);
	else
	    ans = ArraySubset(ax, subs, call, drop, xdims, nsubs);
	PROTECT(ans);
    }

    /* Note: we do not coerce back to pair-based lists. */

    if (type == LANGSXP) {
	ax = ans;
	PROTECT(ans = allocList(LENGTH(ax)));
	if (ans != R_NilValue) {
	    SET_TYPEOF(ans, LANGSXP);
            for (px = ans, i = 0 ; px != R_NilValue ; px = CDR(px))
                SETCAR(px, VECTOR_ELT(ax, i++));
            setAttrib(ans, R_DimSymbol, getDimAttrib(ax));
            setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
            setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
            SET_NAMEDCNT_MAX(ans);
        }
        UNPROTECT(2);
        PROTECT(ans);
    }

    if (HAS_ATTRIB(ans)) { /* remove probably erroneous attr's */
	setAttrib(ans, R_TspSymbol, R_NilValue);
        setAttrib(ans, R_ClassSymbol, R_NilValue);
    }
    UNPROTECT(6);

    return ans;
}


/* The [[ subset operator is implemented by do_subset2 in eval.c.  It
   calls routines below. */

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x = CAR(args);
    args = CDR(args);
    
    if (args == R_NilValue || TAG(args) != R_NilValue)
        return do_subset2_dflt_x (call, op, x, R_NoObject, R_NoObject, 
                                  args, rho, 0);
    else if (CDR(args) == R_NilValue || TAG(CDR(args)) != R_NilValue)
        return do_subset2_dflt_x (call, op, x, CAR(args), R_NoObject,
                                  CDR(args), rho, 0);
    else
        return do_subset2_dflt_x (call, op, x, CAR(args), CADR(args),
                                  CDDR(args), rho, 0);
}

/* Sets R_Visible to TRUE. */
SEXP attribute_hidden do_subset2_dflt_x (SEXP call, SEXP op, 
                                         SEXP x, SEXP sb1, SEXP sb2,
                                         SEXP subs, SEXP rho, int variant)
{
    int offset;
    SEXP ans;

    R_Visible = TRUE;

    if (isVector(x) && sb1 != R_NoObject) {

        /* Check for one subscript, handling simple cases.  Doesn't handle
           simple cases with two subscripts here yet. */

        if (sb2 == R_NoObject && subs == R_NilValue 
              && isVectorAtomic(sb1) && LENGTH(sb1) == 1) { 
            int str_sym_sub = isString(sb1) || isSymbol(sb1);
            offset = get1index(sb1, 
                          str_sym_sub ? getAttrib(x,R_NamesSymbol) : R_NilValue,
                          LENGTH(x), 0/*exact*/, 0, call);
            if (offset >= 0 && offset < LENGTH(x)) {  /* not out of bounds */
                if (isVectorList(x)) {
                    ans = VECTOR_ELT(x, offset);
                    if (NAMEDCNT_GT_0(x))
                        SET_NAMEDCNT_NOT_0(ans);
                    if ((VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
                          || VARIANT_KIND(variant) == VARIANT_FAST_SUB)
                         && !NAMEDCNT_GT_1(x) && !NAMEDCNT_GT_1(ans))
                        R_variant_result = 1;
                }
                else if (TYPEOF(x) == INTSXP && CAN_USE_SCALAR_STACK(variant))
                    ans = PUSH_SCALAR_INTEGER (INTEGER(x)[offset]);
                else if (TYPEOF(x) == REALSXP && CAN_USE_SCALAR_STACK(variant))
                    ans = PUSH_SCALAR_REAL (REAL(x)[offset]);
                else {
                    ans = allocVector(TYPEOF(x), 1);
                    copy_elements (ans, 0, 0, x, offset, 0, 1);
                }
                return ans;
            }
        }
    }

    SEXP dims, dimnames;
    int i, drop, ndims, nsubs;
    int pok, exact = -1;

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */

    if (x == R_NilValue)
        return x;

    PROTECT(x);

    drop = ExtractDropArg(&subs);  /* a "drop" arg is tolerated, but ignored */
    exact = ExtractExactArg(&subs);
    PROTECT(subs);
    if (sb1 == R_NoObject && subs != R_NilValue) {
        sb1 = CAR(subs);
        subs = CDR(subs);
    }
    if (sb2 == R_NoObject && subs != R_NilValue) {
        sb2 = CAR(subs);
        subs = CDR(subs);
    }

    WAIT_UNTIL_COMPUTED(x);

    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    if (exact == -1)
	pok = exact;
    else
	pok = !exact;

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    nsubs = length(subs) + (sb1 != R_NoObject) + (sb2 != R_NoObject);
    if (nsubs == 0)
	errorcall(call, _("no index specified"));
    dims = getDimAttrib(x);
    ndims = length(dims);
    if(nsubs > 1 && nsubs != ndims)
	errorcall(call, _("incorrect number of subscripts"));

    /* code to allow classes to extend environment */
    if(TYPEOF(x) == S4SXP) {
        x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	  errorcall(call, _("this S4 class is not subsettable"));
        UNPROTECT(1);
        PROTECT(x);
    }

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
        if (nsubs != 1 || !isString(sb1) || length(sb1) != 1)
            errorcall(call, _("wrong arguments for subsetting an environment"));
        SEXP sym = installed_already (translateChar (STRING_ELT(sb1,0)));
        if (sym == R_NoObject)
            ans = R_NilValue;
        else {
            ans = findVarInFrame (x, sym);
            if (ans == R_UnboundValue)
                ans = R_NilValue;
            else {
                if (TYPEOF(ans) == PROMSXP)
                    ans = forcePromise(ans);
                SET_NAMEDCNT_NOT_0(ans);
            }
        }
        UNPROTECT(2);
        return(ans);
    }

    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	nonsubsettable_error(call,x);

    int max_named = NAMEDCNT(x);

    if (nsubs == 1) { /* simple or vector indexing */

        int str_sym_sub = isString(sb1) || isSymbol(sb1);
	int len = length(sb1);
        int i, lenx;

        for (i = 1; i < len; i++) {
            if (!isVectorList(x) && !isPairList(x))
                errorcall(call,_("recursive indexing failed at level %d\n"),i);
            lenx = length(x);
            offset = get1index(sb1, 
                       str_sym_sub ? getAttrib(x, R_NamesSymbol) : R_NilValue,
                       lenx, pok, i-1, call);
            if (offset < 0 || offset >= lenx)
                errorcall(call, _("no such index at level %d\n"), i);
            if (isPairList(x)) {
                x = CAR(nthcdr(x, offset));
                max_named = MAX_NAMEDCNT;
            } 
            else {
                x = VECTOR_ELT(x, offset);
                int nm = NAMEDCNT(x);
                if (nm > max_named) 
                    max_named = nm;
            }
        }

        lenx = length(x);
	offset = get1index(sb1, 
                   str_sym_sub ? getAttrib(x, R_NamesSymbol) : R_NilValue,
                   lenx, pok, len > 1 ? len-1 : -1, call);
	if (offset < 0 || offset >= lenx) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset >= lenx && PRIMVAL(op) == 0 /* .el.methods */
             || offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(2);
		return R_NilValue;
	    }
	    else
                out_of_bounds_error(call);
	}
    } else { /* matrix or array indexing */

	dimnames = getAttrib(x, R_DimNamesSymbol);

	int ndn = length(dimnames); /* Number of dimnames. Unlikely anything
                                       but or 0 or nsubs, but just in case... */
        R_len_t indx[nsubs];

	for (i = 0; i < nsubs; i++) {
            SEXP ix;
            if (i == 0)
                ix = sb1;
            else if (i == 1)
                ix = sb2;
            else {
                ix = CAR(subs);
                subs = CDR(subs);
            }
	    indx[i] = get1index(ix, i<ndn ? VECTOR_ELT(dimnames,i) : R_NilValue,
			        INTEGER(dims)[i], pok, -1, call);
	    if (indx[i] < 0 || indx[i] >= INTEGER(dims)[i])
		out_of_bounds_error(call);
	}
	offset = 0;
	for (i = nsubs-1; i > 0; i--)
	    offset = (offset + indx[i]) * INTEGER(dims)[i-1];
	offset += indx[0];
    }

    if (isPairList(x)) {
	ans = CAR(nthcdr(x, offset));
        SET_NAMEDCNT_MAX(ans);
    }
    else if (isVectorList(x)) {
	ans = VECTOR_ELT(x, offset);
	if (max_named > 0)
            SET_NAMEDCNT_NOT_0(ans);
        if ((VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
              || VARIANT_KIND(variant) == VARIANT_FAST_SUB)
             && max_named <= 1 && !NAMEDCNT_GT_1(ans))
            R_variant_result = 1;
    }
    else if (TYPEOF(x) == INTSXP && CAN_USE_SCALAR_STACK(variant))
        ans = PUSH_SCALAR_INTEGER (INTEGER(x)[offset]);
    else if (TYPEOF(x) == REALSXP && CAN_USE_SCALAR_STACK(variant))
        ans = PUSH_SCALAR_REAL (REAL(x)[offset]);
    else {
	ans = allocVector(TYPEOF(x), 1);
        copy_elements (ans, 0, 0, x, offset, 0, 1);
    }
    UNPROTECT(2);
    return ans;
}


/* The $ subset operator is implemented in do_subset3 in eval.c.  It calls
   the routine below. */

/* The field to extract is specified by either the "input" argument (a
   CHARSXP or R_NilValue) or the "name" argument (a SYMSXP or R_NilValue),
   or both.  Protects x.

   Sets R_Visible to TRUE. */

SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input, SEXP name, SEXP call,
                                     int variant)
{
    const char *cinp, *ctarg;
    int mtch;
    SEXP y;

    R_Visible = TRUE;

     /* The mechanism to allow  a class extending "environment" */
    if( IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP ){
        PROTECT(x);
        x = R_getS4DataSlot(x, ANYSXP);
        UNPROTECT(1);
	if(x == R_NilValue)
	    errorcall(call, "$ operator not defined for this S4 class");
    }

    PROTECT(x);

    if (isPairList(x)) {

	SEXP xmatch = R_NilValue;
	int havematch;
        if (name!=R_NilValue) {
            /* Quick check for exact match by name */
            for (y = x; y != R_NilValue; y = CDR(y))
                if (TAG(y)==name) {
                    y = CAR(y);
                    goto found_pairlist;
                }
        }
        cinp = input==R_NilValue ? CHAR(PRINTNAME(name)) : translateChar(input);
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
            if (TAG(y) == R_NilValue)
                continue;
            ctarg = CHAR(PRINTNAME(TAG(y)));
	    mtch = ep_match_strings(ctarg, cinp);
	    if (mtch>0) /* exact */ {
		y = CAR(y);
                goto found_pairlist;
            }
            else if (mtch<0) /* partial */ {
		havematch++;
		xmatch = y;
            }
	}
	if (havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
                ctarg = CHAR(PRINTNAME(TAG(xmatch)));
		warningcall(call, _("partial match of '%s' to '%s'"),
			    cinp, ctarg);
            }
	    y = CAR(xmatch);
            goto found_pairlist;
	}

        UNPROTECT(1);
	return R_NilValue;

      found_pairlist:
        SET_NAMEDCNT_MAX(y);
        UNPROTECT(1);
        return y;
    }

    else if (isVectorList(x)) {

	int i, n, havematch, imatch=-1;
        SEXP str_elt;
        SEXP nlist = getAttrib(x, R_NamesSymbol);
        if (nlist == R_NilValue)
            goto not_found;
	n = LENGTH(nlist);
        if (input == R_NilValue) 
            input = PRINTNAME(name);

        /* Quick check for an exact match.  This has the effect of preferring a
           match without translation to an earlier match after translation,
           which may be good... */

	for (i = 0 ; i < n ; i++) {
            if (STRING_ELT(nlist,i) == input) {
                y = VECTOR_ELT(x,i);
                goto found_veclist;
            }
        }

        /* Slower search, with possible translation and partial matching. */

        cinp = input==R_NilValue ? CHAR(PRINTNAME(name)) : translateChar(input);
	havematch = 0;
	for (i = 0 ; i < n ; i++) {
            str_elt = STRING_ELT (nlist, i);
            ctarg = translateChar(str_elt);
	    mtch = ep_match_strings(ctarg, cinp);
            if (mtch>0) /* exact */ {
		y = VECTOR_ELT(x, i);
                goto found_veclist;
            }
	    else if (mtch<0) /* partial */ {
		havematch++;
		imatch = i;
	    }
	}
	if (havematch == 1) { /* unique partial match */
	    if (R_warn_partial_match_dollar) {
                str_elt = STRING_ELT (nlist, imatch);
                ctarg = TYPEOF(str_elt)==CHARSXP ? translateChar(str_elt)
                                                 : CHAR(PRINTNAME(str_elt));
		warningcall(call, _("partial match of '%s' to '%s'"),
			    cinp, ctarg);
	    }
	    y = VECTOR_ELT(x, imatch);

            /* OLD COMMENT: Partial matches can cause aliasing in
               eval.c:evalseq This is overkill, but alternative ways
               to prevent the aliasing appear to be even worse.
               SHOULD REVISIT. */
            SET_NAMEDCNT_MAX(y);

	    goto found_veclist;
	}

      not_found:
        UNPROTECT(1);
	return R_NilValue;

      found_veclist:
        if (NAMEDCNT_GT_0(x) && NAMEDCNT_EQ_0(y))
            SET_NAMEDCNT(y,1);
        if ((VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
               || VARIANT_KIND(variant) == VARIANT_FAST_SUB) 
             && !NAMEDCNT_GT_1(x) && !NAMEDCNT_GT_1(y))
            R_variant_result = 1;

        UNPROTECT(1);
        return y;
    }
    else if (isEnvironment(x)) {
        if (name==R_NilValue) {
            name = installed_already (translateChar(input));
            if (name == R_NoObject) {
                UNPROTECT(1);
                return R_NilValue;
            }
        }

        y = findVarInFrame3 (x, name, 1);
        if (y == R_UnboundValue)
            y = R_NilValue;
        else {
             if (TYPEOF(y) == PROMSXP)
                 y = forcePromise(y);
             else {
                 SET_NAMEDCNT_NOT_0(y);
                 if ((VARIANT_KIND(variant) == VARIANT_QUERY_UNSHARED_SUBSET 
                        || VARIANT_KIND(variant) == VARIANT_FAST_SUB) 
                      && !NAMEDCNT_GT_1(y))
                     R_variant_result = R_binding_cell == R_NilValue ? 2 : 1;
             }
        }
        UNPROTECT(1);
        return y;
    }
    else if( isVectorAtomic(x) ){
	errorcall(call, "$ operator is invalid for atomic vectors");
    }
    else /* e.g. a function */
	nonsubsettable_error(call,x);

    return R_NilValue;
}


/* ------------------------- SUBSET ASSIGNMENT ------------------------------ */

/* EnlargeVector() takes a vector "x" and changes its length to
   "newlen".  This allows to assign values "past the end" of the
   vector or list.  Names are extended as well, if present.  The dim
   and dimnames attributes are deleted. Other attributes are
   preserved. */

static SEXP EnlargeVector(SEXP call, SEXP x, R_len_t new_len)
{
    if (!isVector(x))
	errorcall(call,_("attempt to enlarge non-vector"));

    R_len_t len = LENGTH(x);

    if (LOGICAL(GetOption1(install("check.bounds")))[0])
	warningcall (call,
          _("assignment outside vector/list limits (extending from %d to %d)"),
          len, new_len);

    SEXP xnames = getNamesAttrib(x);
    SEXP new_xnames = R_NilValue;
    SEXP new_x;

    if (xnames != R_NilValue) {
        R_len_t old_len = LENGTH(xnames);
        R_len_t i;
        if (NAMEDCNT_GT_1(xnames)) {
            new_xnames = allocVector (STRSXP, new_len);
            copy_string_elements (new_xnames, 0, xnames, 0, old_len);
        }
        else
            new_xnames = reallocVector (xnames, new_len, 1);
        for (i = old_len; i < new_len; i++)
            SET_STRING_ELT_BLANK (new_xnames, i);
    }

    PROTECT(new_xnames);
    PROTECT(new_x = reallocVector (x, new_len, 1));
    no_dim_attributes(new_x);
    if (xnames != R_NilValue && new_xnames != xnames)
        setAttrib (new_x, R_NamesSymbol, new_xnames);
    UNPROTECT(2);  /* new_x, new_xnames */

    return new_x;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in which coerceVector should
   fail; namely, S4SXP */
static SEXP embedInVector(SEXP v)
{
    SEXP ans;
    PROTECT(ans = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, v);
    UNPROTECT(1);
    return (ans);
}

/* Coerces the LHS or RHS to make assignment possible.

   Level 0 and 1 are used for [<-.  They coerce the RHS to a list when the
   LHS is a list.  Level 1 also coerces to avoid assignment of numeric vector 
   to string vector or raw vector to any other numeric or string vector.

   Level 2 is used for [[<-.  It does not coerce when assigning into a list.
*/

static void SubassignTypeFix (SEXP *x, SEXP *y, int stretch, int level, 
                              SEXP call)
{
    Rboolean x_is_object = OBJECT(*x);  /* coercion can lose the object bit */

    const int type_x = TYPEOF(*x), type_y = TYPEOF(*y);
    int atom_x;

    if (type_x == type_y || type_y == NILSXP) {
        /* nothing to do */
    }
    else if ((atom_x = isVectorAtomic(*x)) && isVectorAtomic(*y)) { 
        /* Follow STR > CPLX > REAL > INT > LGL > RAW conversion hierarchy, 
           which never produces warnings. */
        if (type_x == CPLXSXP
              && type_y == STRSXP
         || type_x == REALSXP
              && ((((1<<STRSXP) + (1<<CPLXSXP)) >> type_y) & 1)
         || type_x == INTSXP 
              && ((((1<<STRSXP) + (1<<CPLXSXP) + (1<<REALSXP)) >> type_y) & 1)
         || type_x == LGLSXP
              && ((((1<<STRSXP) + (1<<CPLXSXP) + (1<<REALSXP) + (1<<INTSXP))
                   >> type_y) & 1)
         || type_x == RAWSXP
              && type_y != RAWSXP)
            *x = coerceVector (*x, type_y);
        if (level == 1) { 
            /* For when later code doesn't handle these cases. */
            if (type_y == RAWSXP && type_x != RAWSXP
             || type_y != STRSXP && type_x == STRSXP)
                *y = coerceVector (*y, type_x);
        }
    }
    else if (atom_x && isVectorList(*y)) {
        *x = coerceVector (*x, type_y);
    }
    else if (isVectorList(*x)) {
        if (level != 2)
	    *y = type_y==S4SXP ? embedInVector(*y) : coerceVector (*y, type_x);
    }
    else {
	errorcall(call,
              _("incompatible types (from %s to %s) in subassignment type fix"),
	      type2char(type_y), type2char(type_x));
    }

    if (stretch) {
	PROTECT(*y);
	*x = EnlargeVector(call, *x, stretch);
	UNPROTECT(1);
    }
    SET_OBJECT(*x, x_is_object);
}

/* Returns list made from x (a LISTSXP, EXPRSXP, or NILSXP) with elements 
   from start to end (inclusive) deleted.  The start index will be positive. 
   If start>end, no elements are deleted (x returned unchanged). */

static SEXP DeleteListElementsSeq (SEXP x, R_len_t start, R_len_t end)
{
    SEXP xnew, xnames, xnewnames;
    R_len_t i, len;

    len = length(x);
    if (start < 1)
        start = 1;
    if (end > len) 
        end = len;
    if (start > end)
        return x;

    if (NAMEDCNT_GT_1(x)) {
        PROTECT(xnew = allocVector(TYPEOF(x), len-(end-start+1)));
        for (i = start; i <= end; i++) /* after we know alloc won't fail */
            DEC_NAMEDCNT (VECTOR_ELT (x, i-1));
        if (start>1) 
            copy_vector_elements (xnew, 0, x, 0, start-1);
        if (end<len) 
            copy_vector_elements (xnew, start-1, x, end, len-end);
        copyMostAttrib(x, xnew);
    }
    else {
        for (i = start; i <= end; i++)
            DEC_NAMEDCNT (VECTOR_ELT (x, i-1));
        if (end<len) 
            copy_vector_elements (x, start-1, x, end, len-end);
        PROTECT(xnew = reallocVector (x, len-(end-start+1), 1));
        no_dim_attributes(xnew);
    }

    xnames = getNamesAttrib(x);
    if (xnames != R_NilValue) {
        if (NAMEDCNT_GT_1(xnames)) {
            PROTECT(xnewnames = allocVector(STRSXP, len-(end-start+1)));
            if (start > 1)
                copy_string_elements(xnewnames, 0, xnames, 0, start-1);
            if (end < len)
                copy_string_elements(xnewnames, start-1, xnames, end, len-end);
        }
        else {
            if (end < len)
                copy_string_elements(xnames, start-1, xnames, end, len-end);
            PROTECT(xnewnames = reallocVector (xnames, len-(end-start+1), 1));
        }
        if (xnew != x || xnewnames != xnames) 
            setAttrib(xnew, R_NamesSymbol, xnewnames);
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return xnew;
}

/* Returns list made from x (a LISTSXP, EXPRSXP, or NILSXP) with elements 
   indexed by elements in "which" (an INSTSXP or NILSXP) deleted. */

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    R_len_t i, ii, len, lenw;

    if (x==R_NilValue || which==R_NilValue)
        return x;
    len = LENGTH(x);
    lenw = LENGTH(which);
    if (len==0 || lenw==0) 
        return x;

    /* handle deletion of a contiguous block (incl. one element) specially. */
    for (i = 1; i < lenw; i++)
        if (INTEGER(which)[i] != INTEGER(which)[i-1]+1)
            break;
    if (i == lenw) {
        int start = INTEGER(which)[0];
        int end = INTEGER(which)[lenw-1];
        if (start < 1) start = 1;
        return DeleteListElementsSeq (x, start, end);
    }

    /* create vector indicating which to delete */
    PROTECT(include = allocVector(INTSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(include)[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = INTEGER(which)[i];
	if (0 < ii  && ii <= len)
	    INTEGER(include)[ii - 1] = 0;
    }

    /* calculate the length of the result */
    ii = 0;
    for (i = 0; i < len; i++)
	ii += INTEGER(include)[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }

    PROTECT(xnew = allocVector(TYPEOF(x), ii));
    ii = 0;
    for (i = 0; i < len; i++) {
	if (INTEGER(include)[i] == 1) {
	    SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
	    ii++;
	}
        else
            DEC_NAMEDCNT (VECTOR_ELT (x, i));
    }

    xnames = getNamesAttrib(x);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_STRING_ELT(xnewnames, ii, STRING_ELT(xnames, i));
		ii++;
	    }
	}
	setAttrib(xnew, R_NamesSymbol, xnewnames);
	UNPROTECT(1);
    }

    copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

/* Assigns to a contiguous block of elements with indexes from start to end
   (inclusive).  The start index will be positive.  If start>end, no elements 
   are assigned, but the returned value may have been coerced to match types. 
   The y argument must have length of 1 or the size of the block (end-start+1),
   or be R_NilValue (for deletion).

   The x argument must be protected by the caller. */

static SEXP VectorAssignSeq 
              (SEXP call, SEXP x, R_len_t start, R_len_t end, SEXP y)
{
    int i, n, ny;

    if (x==R_NilValue && y==R_NilValue)
	return R_NilValue;

    n = end - start + 1;

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */

    SubassignTypeFix (&x, &y, end > length(x) ? end : 0, 0, call);

    PROTECT(x);

    ny = length(y);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (n > 0 && ny == 0)
	    errorcall(call,_("replacement has length zero"));
    }

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Do the actual assignment... */

    if (n == 0) {
        /* nothing to do */
    }
    else if (isVectorAtomic(x) && isVectorAtomic(y)) {
        copy_elements_coerced (x, start-1, 1, y, 0, ny>1, n);
    }
    else  if (isVectorList(x) && isVectorList(y)) {
        if (ny == 1) {
            SET_VECTOR_ELEMENT_FROM_VECTOR (x, start-1, y, 0);
            SEXP y0 = VECTOR_ELT (y, 0);
            for (i = 1; i < n; i++) {
                SET_VECTOR_ELT (x, start-1+i, y0);
                INC_NAMEDCNT_0_AS_1 (y0);
            }
        }
        else {
            for (i = 0; i < n; i++)
                SET_VECTOR_ELEMENT_FROM_VECTOR (x, start-1+i, y, i);
        }
    }
    else if (isVectorList(x) && y == R_NilValue) {
	x = DeleteListElementsSeq(x, start, end);
    }
    else {
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

    UNPROTECT(2);
    return x;
}

/* Remove NA indexes from indx, whose length is *n, with first NA at
   index (from 1) first_na.  Returns possibly new index vector, and
   updates *n to number of indexes. */

static SEXP NA_rem (SEXP indx, int *n, int first_na)
{
    int i, j;

    if (NAMEDCNT_GT_0(indx))
        indx = duplicate(indx);

    j = first_na - 1;
    for (i = j+1; i < *n; i++) {
        if (INTEGER(indx)[i] != NA_INTEGER) {
            INTEGER(indx)[j] = INTEGER(indx)[i];
            j += 1;
        }
    }

    *n = j;

    return indx;
}

/* If "err" is 1, raise an error if any NAs are present in "indx", and
   otherwise return "indx" unchanged.  If "err" is 0, return an index
   vector (possibly newly allocated) with any NAs removed, updating "n"
   to its new length. */

static inline SEXP NA_check_remove (SEXP call, SEXP indx, int *n, int err)
{
    int ii;

    for (ii = 0; ii < *n && INTEGER(indx)[ii] != NA_INTEGER; ii++) ;

    if (ii == *n)
        return indx;

    if (err)
        errorcall(call,_("NAs are not allowed in subscripted assignments"));

    return NA_rem (indx, n, ii+1);
}


static SEXP VectorAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    SEXP indx, newnames;
    int i, ii, iy, n, nx, ny;
    double ry;

    if (x==R_NilValue && y==R_NilValue)
        return R_NilValue;

    PROTECT(s);

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    if (isMatrix(s) && isArray(x)) {
        SEXP dim = getDimAttrib(x);
        if (ncols(s) == LENGTH(dim)) {
            if (isString(s)) {
                SEXP dnames = PROTECT(GetArrayDimnames(x));
                s = strmat2intmat(s, dnames, call);
                UNPROTECT(2); /* dnames, s */
                PROTECT(s);
            }
            if (isInteger(s) || isReal(s)) {
                s = mat2indsub(dim, s, call);
                UNPROTECT(1);
                PROTECT(s);
            }
        }
    }

    int stretch = -1; /* allow out of bounds, for assignment */
    int pindx;
    int hasna;
    PROTECT_WITH_INDEX (indx = makeSubscript(x, s, &stretch, &hasna, call, 1),
                        &pindx);
    n = LENGTH(indx);

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */
    SubassignTypeFix(&x, &y, stretch, 1, call);
    if (n == 0) {
        UNPROTECT(2);
        return x;
    }

    PROTECT(x);

    ny = length(y);
    nx = length(x);

    int oldn = n;

    if (hasna) {
        if (length(y) > 1)
            errorcall(call,_("NAs are not allowed in subscripted assignments"));
        REPROTECT (indx = NA_rem (indx, &n, hasna), pindx);
    }

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
        if (oldn > 0 && ny == 0)
            errorcall(call,_("replacement has length zero"));
        if (oldn > 0 && n % ny)
            warningcall(call,
             _("number of items to replace is not a multiple of replacement length"));
    }

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
        PROTECT(y = duplicate(y));
    else
        PROTECT(y);

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int *ixp = INTEGER(indx);
    int k;

    switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

    case (LGLSXP<<5) + LGLSXP:
    case (INTSXP<<5) + LGLSXP:
    case (INTSXP<<5) + INTSXP:
        if (ny == 1) {
            int e = INTEGER(y)[0];
            if (n & 1) {
                ii = ixp[0] - 1;
                INTEGER(x)[ii] = e;
            }
            for (i = n & 1; i < n; i += 2) {
                ii = ixp[i] - 1;
                INTEGER(x)[ii] = e;
                ii = ixp[i+1] - 1;
                INTEGER(x)[ii] = e;
            }
        }
        else if (ny >= n) {
            if (n & 1) {
                ii = ixp[0] - 1;
                INTEGER(x)[ii] = INTEGER(y)[i];
            }
            for (i = n & 1; i < n; i += 2) {
                ii = ixp[i] - 1;
                INTEGER(x)[ii] = INTEGER(y)[i];
                ii = ixp[i+1] - 1;
                INTEGER(x)[ii] = INTEGER(y)[i+1];
            }
        }
        else {
            for (i = 0, k = 0; i < n; i++) {
                ii = ixp[i] - 1;
                INTEGER(x)[ii] = INTEGER(y)[k];
                if (++k == ny) k = 0;
            }
        }
        break;

    case (REALSXP<<5) + LGLSXP:
    case (REALSXP<<5) + INTSXP:
        for (i = 0, k = 0; i < n; i++) {
            ii = ixp[i] - 1;
            iy = INTEGER(y)[k];
            if (iy == NA_INTEGER)
                REAL(x)[ii] = NA_REAL;
            else
                REAL(x)[ii] = iy;
            if (++k == ny) k = 0;
        }
        break;

    case (REALSXP<<5) + REALSXP:
        if (ny == 1) {
            double e = REAL(y)[0];
            if (n & 1) {
                ii = ixp[0] - 1;
                REAL(x)[ii] = e;
            }
            for (i = n & 1; i < n; i += 2) {
                ii = ixp[i] - 1;
                REAL(x)[ii] = e;
                ii = ixp[i+1] - 1;
                REAL(x)[ii] = e;
            }
        }
        else if (ny >= n) {
            if (n & 1) {
                ii = ixp[0] - 1;
                REAL(x)[ii] = REAL(y)[0];
            }
            for (i = n & 1; i < n; i += 2) {
                ii = ixp[i] - 1;
                REAL(x)[ii] = REAL(y)[i];
                ii = ixp[i+1] - 1;
                REAL(x)[ii] = REAL(y)[i+1];
            }
        }
        else {
            for (i = 0, k = 0; i < n; i++) {
                ii = ixp[i] - 1;
                REAL(x)[ii] = REAL(y)[k];
                if (++k == ny) k = 0;
            }
        }
        break;

    case (CPLXSXP<<5) + LGLSXP:
    case (CPLXSXP<<5) + INTSXP:
        for (i = 0, k = 0; i < n; i++) {
            ii = ixp[i] - 1;
            iy = INTEGER(y)[k];
            if (iy == NA_INTEGER) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = iy;
                COMPLEX(x)[ii].i = 0.0;
            }
            if (++k == ny) k = 0;
        }
        break;

    case (CPLXSXP<<5) + REALSXP:
        for (i = 0, k = 0; i < n; i++) {
            ii = ixp[i] - 1;
            ry = REAL(y)[k];
            if (ISNA(ry)) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = ry;
                COMPLEX(x)[ii].i = 0.0;
            }
            if (++k == ny) k = 0;
        }
        break;

    case (CPLXSXP<<5) + CPLXSXP:
        for (i = 0, k = 0; i < n; i++) {
            ii = ixp[i] - 1;
            COMPLEX(x)[ii] = COMPLEX(y)[k];
            if (++k == ny) k = 0;
        }
        break;

    case (STRSXP<<5) + STRSXP:
        if (ny == 1) {
            SEXP e = STRING_ELT(y,0);
            for (i = 0; i < n; i++) {
                ii = ixp[i] - 1;
                SET_STRING_ELT(x, ii, e);
            }
        }
        else if (ny >= n) {
            for (i = 0; i < n; i++) {
                ii = ixp[i] - 1;
                SET_STRING_ELT(x, ii, STRING_ELT(y, i));
            }
        }
        else {
            for (i = 0, k = 0; i < n; i++) {
                ii = ixp[i] - 1;
                SET_STRING_ELT(x, ii, STRING_ELT(y, k));
                if (++k == ny) k = 0;
            }
        }
        break;

    case (RAWSXP<<5) + RAWSXP:
        if (ny == 1) {
            int e = RAW(y)[0];
            for (i = 0; i < n; i++) {
                ii = ixp[i] - 1;
                RAW(x)[ii] = e;
            }
        }
        else if (ny >= n) {
            for (i = 0; i < n; i++) {
                ii = ixp[i] - 1;
                RAW(x)[ii] = RAW(y)[i];
            }
        }
        else {
            for (i = 0, k = 0; i < n; i++) {
                ii = ixp[i] - 1;
                RAW(x)[ii] = RAW(y)[k];
                if (++k == ny) k = 0;
            }
        }
        break;

    case (EXPRSXP<<5) + VECSXP:
    case (EXPRSXP<<5) + EXPRSXP:
    case (VECSXP<<5)  + EXPRSXP:
    case (VECSXP<<5)  + VECSXP:
        for (i = 0, k = 0; i < n; i++) {
            ii = ixp[i] - 1;
            if (i < ny) {
                SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, i);
            }
            else { /* first time we get here, k is and should be 0 */
                SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, k);
                if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ii)))
                    SET_NAMEDCNT(VECTOR_ELT(x,ii),2);
                if (++k == ny) k = 0;
            }
        }
        break;

    case (EXPRSXP<<5) + NILSXP:
    case (VECSXP<<5)  + NILSXP:
        x = DeleteListElements(x, indx);
        UNPROTECT(4);
        return x;
        break;

    default:
        warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

    /* Check for additional named elements, if subscripting with strings. */
    /* Note makeSubscript passes the additional names back as the use.names
       attribute (a vector list) of the generated subscript vector */
    if (TYPEOF(s)==STRSXP && 
          (newnames = getAttrib(indx, R_UseNamesSymbol)) != R_NilValue) {
        SEXP oldnames = getNamesAttrib(x);
        if (oldnames == R_NilValue) {
            PROTECT(oldnames = allocVector(STRSXP,nx)); /* all R_BlankString */
            setAttrib(x, R_NamesSymbol, oldnames);
            UNPROTECT(1);
        }
        for (i = 0; i < n; i++) {
            if (VECTOR_ELT(newnames, i) != R_NilValue) {
                ii = INTEGER(indx)[i];
                if (ii == NA_INTEGER) continue;
                ii = ii - 1;
                SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
            }
        }
    }
    UNPROTECT(4);
    return x;
}

static SEXP MatrixAssign(SEXP call, SEXP x, SEXP sb1, SEXP sb2, SEXP y)
{
    int i, j, iy;
    int rhasna, chasna;
    int_fast64_t n;
    double ry;
    int nr;
    int nrs, ncs;
    SEXP sr, sc, dim;

    dim = getDimAttrib(x);

    if (dim == R_NilValue || LENGTH(dim) != 2)
	errorcall(call,_("incorrect number of subscripts on matrix"));

    nr = INTEGER(dim)[0];

    SEXP sv_scalar_stack = R_scalar_stack;

    PROTECT (sr = array_sub (sb1, dim, 0, x, x, &rhasna));
    nrs = LENGTH(sr);

    PROTECT (sc = array_sub (sb2, dim, 1, x, x, &chasna));
    ncs = LENGTH(sc);

    /* Do assignment of a single atomic element with matching scalar type,
       not being computed, specially. */

    if (nrs == 1 && ncs == 1 && isVectorAtomic(x)
          && (TYPE_ETC(y) & ~TYPE_ET_CETERA_HAS_ATTR) == TYPEOF(x)) {
        if (*INTEGER(sr) != NA_INTEGER && *INTEGER(sc) != NA_INTEGER) {
            R_len_t isub = (*INTEGER(sr)-1) + (*INTEGER(sc)-1) * nr;
            switch (TYPEOF(x)) {
            case RAWSXP: 
                RAW(x)[isub] = *RAW(y);
                break;
            case LGLSXP: 
                LOGICAL(x)[isub] = *LOGICAL(y);
                break;
            case INTSXP: 
                INTEGER(x)[isub] = *INTEGER(y);
                break;
            case REALSXP: 
                REAL(x)[isub] = *REAL(y);
                break;
            case CPLXSXP: 
                COMPLEX(x)[isub] = *COMPLEX(y);
                break;
            case STRSXP:
                SET_STRING_ELT (x, isub, STRING_ELT(y,0));
                break;
            }
        }
        UNPROTECT(2);
        R_scalar_stack = sv_scalar_stack;
        return x;
    }

    R_len_t ny = length(y);

    if (rhasna) {
        sr = NA_check_remove (call, sr, &nrs, ny > 1);
        UNPROTECT(2);
        PROTECT2(sr,sc);
    }
    if (chasna) {
        sc = NA_check_remove (call, sc, &ncs, ny > 1);
        UNPROTECT(2);
        PROTECT2(sr,sc);
    }

    n = nrs * ncs;

    if (n > 0 && ny == 0)
	errorcall(call,_("replacement has length zero"));
    if (n > 0 && n % ny)
	errorcall(call,
       _("number of items to replace is not a multiple of replacement length"));

    SubassignTypeFix(&x, &y, 0, 1, call);
    if (n == 0) {
        UNPROTECT(2);
        R_scalar_stack = sv_scalar_stack;
        return x;
    }

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int * restrict scix = INTEGER(sc);
    int * restrict srix = INTEGER(sr);
    int colix;

    if (ny == 1) {

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP: {
            int tmp = INTEGER(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    INTEGER(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
        case (REALSXP<<5) + REALSXP: {
            double tmp = TYPEOF(y) == REALSXP ? REAL(y)[0] 
                       : INTEGER(y)[0] == NA_INTEGER ? NA_REAL : INTEGER(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    REAL(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP: 
        case (CPLXSXP<<5) + REALSXP:
        case (CPLXSXP<<5) + CPLXSXP: {
            Rcomplex tmp;
            if (TYPEOF(y) == CPLXSXP) 
                tmp = COMPLEX(y)[0];
            else if (TYPEOF(y) == REALSXP) {
                if (ISNAN(REAL(y)[0])) tmp.r = tmp.i = NA_REAL;
                else { tmp.r = REAL(y)[0]; tmp.i = 0; }
            }
            else { /* INT or LGL */
                if (INTEGER(y)[0] == NA_INTEGER) tmp.r = tmp.i = NA_REAL;
                else { tmp.r = INTEGER(y)[0]; tmp.i = 0; }
            }
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    COMPLEX(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (STRSXP<<5) + STRSXP: {
            SEXP tmp = STRING_ELT (y, 0);
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    SET_STRING_ELT (x, srix[i] - 1 + colix, tmp);
            }
            break;
        }
        case (RAWSXP<<5) + RAWSXP: {
            Rbyte tmp = RAW(y)[0];
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++)
                    RAW(x) [srix[i] - 1 + colix] = tmp;
            }
            break;
        }
        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP: {
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i]-1+colix, y, 0);
                    if (i+j>0 && NAMEDCNT_EQ_0 (VECTOR_ELT(x,srix[i]-1+colix)))
                        SET_NAMEDCNT (VECTOR_ELT (x, srix[i]-1+colix), 2);
                }
            }
            break;
        }
        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }
    else if (ny >= nrs * ncs) {

        int k = 0;

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    INTEGER(x) [srix[i] - 1 + colix] = INTEGER(y)[k];
                    k += 1;
                }
            }
            break;

        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER)
                        REAL(x) [srix[i] - 1 + colix] = NA_REAL;
                    else
                        REAL(x) [srix[i] - 1 + colix] = iy;
                    k += 1;
                }
            }
            break;

        case (REALSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    REAL(x) [srix[i] - 1 + colix] = REAL(y)[k];
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = iy;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    ry = REAL(y)[k];
                    if (ISNA(ry)) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = ry;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    k += 1;
                }
            }
            break;

        case (CPLXSXP<<5) + CPLXSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    COMPLEX(x) [srix[i] - 1 + colix] = COMPLEX(y)[k];
                    k += 1;
                }
            }
            break;

        case (STRSXP<<5) + STRSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_STRING_ELT(x, srix[i] - 1 + colix, STRING_ELT(y, k));
                    k += 1;
                }
            }
            break;

        case (RAWSXP<<5) + RAWSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    RAW(x) [srix[i] - 1 + colix] = RAW(y)[k];
                    k += 1;
                }
            }
            break;

        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    if (k < ny) {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k);
                    }
                    else {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k%ny);
                        if (NAMEDCNT_EQ_0 (VECTOR_ELT (x, srix[i] - 1 + colix)))
                            SET_NAMEDCNT(VECTOR_ELT(x, srix[i] - 1 + colix), 2);
                    }
                    k += 1;
                }
            }
            break;

        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }
    else {  /* y is recycled, and is not of length 1 */

        int k = 0;

        switch ((TYPEOF(x)<<5) + TYPEOF(y)) {

        case (LGLSXP<<5) + LGLSXP:
        case (INTSXP<<5) + LGLSXP:
        case (INTSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    INTEGER(x) [srix[i] - 1 + colix] = INTEGER(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (REALSXP<<5) + LGLSXP:
        case (REALSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER)
                        REAL(x) [srix[i] - 1 + colix] = NA_REAL;
                    else
                        REAL(x) [srix[i] - 1 + colix] = iy;
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (REALSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    REAL(x) [srix[i] - 1 + colix] = REAL(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + LGLSXP:
        case (CPLXSXP<<5) + INTSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    iy = INTEGER(y)[k];
                    if (iy == NA_INTEGER) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = iy;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + REALSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    ry = REAL(y)[k];
                    if (ISNA(ry)) {
                        COMPLEX(x) [srix[i] - 1 + colix].r = NA_REAL;
                        COMPLEX(x) [srix[i] - 1 + colix].i = NA_REAL;
                    }
                    else {
                        COMPLEX(x) [srix[i] - 1 + colix].r = ry;
                        COMPLEX(x) [srix[i] - 1 + colix].i = 0.0;
                    }
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (CPLXSXP<<5) + CPLXSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    COMPLEX(x) [srix[i] - 1 + colix] = COMPLEX(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (STRSXP<<5) + STRSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    SET_STRING_ELT(x, srix[i] - 1 + colix, STRING_ELT(y, k));
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (RAWSXP<<5) + RAWSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    RAW(x) [srix[i] - 1 + colix] = RAW(y)[k];
                    if (++k == ny) k = 0;
                }
            }
            break;

        case (EXPRSXP<<5) + VECSXP:
        case (EXPRSXP<<5) + EXPRSXP:
        case (VECSXP<<5)  + EXPRSXP:
        case (VECSXP<<5)  + VECSXP:
            for (j = 0; j < ncs; j++) {
                colix = (scix[j] - 1) * nr;
                for (i = 0; i < nrs; i++) {
                    if (k < ny) {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k);
                    }
                    else {
                        SET_VECTOR_ELEMENT_FROM_VECTOR (x, srix[i] - 1 + colix,
                                                        y, k%ny);
                        if (NAMEDCNT_EQ_0 (VECTOR_ELT (x, srix[i] - 1 + colix)))
                            SET_NAMEDCNT(VECTOR_ELT(x, srix[i] - 1 + colix), 2);
                    }
                    k += 1;
                }
            }
            break;

        default:
            warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
        }
    }

    UNPROTECT(4);
    R_scalar_stack = sv_scalar_stack;
    return x;
}


static SEXP ArrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, iy, k=0, ny;
    int rep_assign = 0; /* 1 if elements assigned repeatedly into list array */
    int any_hasna = 0;
    SEXP dims, tmp;
    double ry;

    PROTECT(dims = getDimAttrib(x));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	errorcall(call,_("incorrect number of subscripts"));

    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    SubassignTypeFix(&x, &y, 0, 1, call);

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    ny = length(y);

    int *subs[k], indx[k], bound[k], offset[k], hasna[k];
    SEXP sv_scalar_stack = R_scalar_stack;

    int nmod = ny > 1;
    int zero = 0;
    for (i = 0; i < k; i++) {
        PROTECT(tmp = array_sub (CAR(s), dims, i, x, x, &hasna[i]));
	bound[i] = LENGTH(tmp);
        if (hasna[i]) {
            any_hasna = 1;
            if (ny <= 1) {
                tmp = NA_rem (tmp, &bound[i], hasna[i]);
                UNPROTECT(1);
                PROTECT(tmp);
            }
        }
        subs[i] = INTEGER(tmp);
        if (bound[i] == 0) zero = 1;
        if (ny > 1) nmod = ((int_fast64_t)nmod * bound[i]) % ny;
        indx[i] = 0;
	s = CDR(s);
    }

    if (!zero && ny == 0)
	errorcall(call,_("replacement has length zero"));
    if (!zero && nmod != 0)
	errorcall(call,
       _("number of items to replace is not a multiple of replacement length"));

    if (any_hasna && ny > 1)
        errorcall (call, _("NAs are not allowed in subscripted assignments"));

    if (zero) {
	UNPROTECT(k+3);
        R_scalar_stack = sv_scalar_stack;
	return x;
    }

    offset[1] = INTEGER(dims)[0];
    offset[0] = offset[1] + 1;
    for (i = 2; i < k; i++) {
        offset[i] = offset[i-1] * INTEGER(dims)[i-1];
        offset[0] += offset[i];
    }

    /* Do the actual assignment... Note that assignments to string vectors
       from non-string vectors and from raw vectors to non-raw vectors are
       not handled here, but are avoided by coercion in SubassignTypeFix. */

    int which = (TYPEOF(x)<<5) + TYPEOF(y);

#   define AA_PRELUDE \
    for (;;) { \
        ii = subs[0][indx[0]] - offset[0]; \
        if (k == 3) { \
            ii += offset[1] * subs[1][indx[1]]; \
            ii += offset[2] * subs[2][indx[2]]; \
        } \
        else if (k == 4) { \
            ii += offset[1] * subs[1][indx[1]]; \
            ii += offset[2] * subs[2][indx[2]]; \
            ii += offset[3] * subs[3][indx[3]]; \
        } \
        else { \
            for (j = 1; j < k; j++) \
                ii += offset[j] * subs[j][indx[j]]; \
        }

#   define AA_POSTLUDE \
        j = 0; \
        while (++indx[j] >= bound[j]) { \
            indx[j] = 0; \
            if (++j >= k) goto done; \
        } \
        if (++i == ny) i = 0; \
    } /* end of for(;;) */

    i = 0;

    switch (which) {

    case (LGLSXP<<5) + LGLSXP:
    case (INTSXP<<5) + LGLSXP:
    case (INTSXP<<5) + INTSXP:
        AA_PRELUDE
            INTEGER(x)[ii] = INTEGER(y)[i];
        AA_POSTLUDE
        break;

    case (REALSXP<<5) + LGLSXP:
    case (REALSXP<<5) + INTSXP:
        AA_PRELUDE
            iy = INTEGER(y)[i];
            if (iy == NA_INTEGER)
                REAL(x)[ii] = NA_REAL;
            else
                REAL(x)[ii] = iy;
        AA_POSTLUDE
        break;

    case (REALSXP<<5) + REALSXP:
        AA_PRELUDE
            REAL(x)[ii] = REAL(y)[i];
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + LGLSXP:
    case (CPLXSXP<<5) + INTSXP:
        AA_PRELUDE
            iy = INTEGER(y)[i];
            if (iy == NA_INTEGER) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = iy;
                COMPLEX(x)[ii].i = 0.0;
            }
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + REALSXP:
        AA_PRELUDE
            ry = REAL(y)[i];
            if (ISNA(ry)) {
                COMPLEX(x)[ii].r = NA_REAL;
                COMPLEX(x)[ii].i = NA_REAL;
            }
            else {
                COMPLEX(x)[ii].r = ry;
                COMPLEX(x)[ii].i = 0.0;
            }
        AA_POSTLUDE
        break;

    case (CPLXSXP<<5) + CPLXSXP:
        AA_PRELUDE
            COMPLEX(x)[ii] = COMPLEX(y)[i];
        AA_POSTLUDE
        break;

    case (STRSXP<<5) + STRSXP:
        AA_PRELUDE
            SET_STRING_ELT(x, ii, STRING_ELT(y, i));
        AA_POSTLUDE
        break;

    case (RAWSXP<<5) + RAWSXP:
        AA_PRELUDE
            RAW(x)[ii] = RAW(y)[i];
        AA_POSTLUDE
        break;

    case (EXPRSXP<<5) + VECSXP:
    case (EXPRSXP<<5) + EXPRSXP:
    case (VECSXP<<5)  + EXPRSXP:
    case (VECSXP<<5)  + VECSXP:
        AA_PRELUDE
            SET_VECTOR_ELEMENT_FROM_VECTOR(x, ii, y, i);
            if (!rep_assign) {
                if (i == ny - 1) 
                    rep_assign = 1;
            }
            else {
                if (NAMEDCNT_EQ_0(VECTOR_ELT(x,ii)))
                    SET_NAMEDCNT(VECTOR_ELT(x,ii),2);
            }
        AA_POSTLUDE
        break;

    default:
        warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }

  done:
    UNPROTECT(k+3);
    R_scalar_stack = sv_scalar_stack;
    return x;
}

/* Returns 'subs' as the index list, and 'y' as the value to assign.
   May modify 'subs'. */

static void SubAssignArgs(SEXP *subs, SEXP *y, SEXP call)
{
    SEXP args = *subs;

    if (args == R_NilValue)
	errorcall(call,_("SubAssignArgs: invalid number of arguments"));

    if (CDR(args) == R_NilValue) {
	*subs = R_NilValue;
	*y = CAR(args);
    }
    else {
	while (CDDR(args) != R_NilValue)
	    args = CDR(args);
	*y = CADR(args);
	SETCDR_NIL(args);
    }
}

/* The [<- operator is implemeted in do_subassign in eval.c, which calls
   routines below. */
                           /* do_subassign_dflt is called from elsewhere too. */

SEXP attribute_hidden do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return do_subassign_dflt_seq 
      (call, CAR(args), R_NoObject, R_NoObject, CDR(args), rho, R_NoObject, 0);
}

/* The last "seq" argument below is non-zero if the first subscript is a 
   sequence spec (a variant result).  Sets R_Visible to TRUE. */

SEXP attribute_hidden do_subassign_dflt_seq (SEXP call, SEXP x, 
                                             SEXP sb1, SEXP sb2, SEXP subs,
                                             SEXP rho, SEXP y, int64_t seq)
{
    R_Visible = TRUE;

    BEGIN_PROTECT0 ();
    ALSO_PROTECT5 (x, sb1, sb2, subs, y);

    WAIT_UNTIL_COMPUTED(x);

    /* Do simple cases quickly. */

    if (!seq && sb1 != R_NoObject && subs == R_NilValue 
             && isVector(x) && !IS_S4_OBJECT(x) && !NAMEDCNT_GT_1(x)
             && y != R_NoObject && TYPEOF(y) == TYPEOF(x) && LENGTH(y) == 1) {
        R_len_t ix1 = 0;
        if (TYPE_ETC(sb1) == INTSXP) {        /* scalar integer index */
            if (*INTEGER(sb1) <= LENGTH(x))
                ix1 = *INTEGER(sb1);
        }
        else if (TYPE_ETC(sb1) == REALSXP) {  /* scalar real index */
            if (*REAL(sb1) >= 1 && *REAL(sb1) <= LENGTH(x))  /* false if NaN */
                ix1 = (R_len_t) *REAL(sb1);
        }
        R_len_t ix;
        if (sb2 == R_NoObject)
            ix = ix1;
        else {
            ix = 0;
            if (ix1 > 0) {
                SEXP dim = getDimAttrib(x);
                if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2
                                          && ix1 <= INTEGER(dim)[0]) {
                    R_len_t ix2 = 0;
                    if (TYPE_ETC(sb2) == INTSXP) {    /* scalar integer index */
                        if (*INTEGER(sb2) <= INTEGER(dim)[1])
                            ix2 = *INTEGER(sb2);
                    }
                    else if (TYPE_ETC(sb2) == REALSXP) { /* scalar real index */
                        if (*REAL(sb2) >= 1 && *REAL(sb2) <= INTEGER(dim)[1])
                            ix2 = (R_len_t) *REAL(sb2);
                    }
                    if (ix2 > 0)
                        ix = ix1 + INTEGER(dim)[0] * (ix2-1);
                }
            }
        }
        if (ix > 0) {
            ix -= 1;
            switch (TYPEOF(x)) {
                case RAWSXP: 
                    RAW(x)[ix] = *RAW(y);
                    break;
                case LGLSXP: 
                    LOGICAL(x)[ix] = *LOGICAL(y);
                    break;
                case INTSXP: 
                    INTEGER(x)[ix] = *INTEGER(y);
                    break;
                case REALSXP: 
                    REAL(x)[ix] = *REAL(y);
                    break;
                case CPLXSXP: 
                    COMPLEX(x)[ix] = *COMPLEX(y);
                    break;
                case STRSXP:
                    SET_STRING_ELT (x, ix, STRING_ELT(y,0));
                    break;
                case VECSXP: case EXPRSXP:
                    DEC_NAMEDCNT (VECTOR_ELT (x, ix));
                    SET_VECTOR_ELEMENT_FROM_VECTOR (x, ix, y, 0);
                    break;
            }
            SET_NAMEDCNT_0(x);
            RETURN_SEXP_INSIDE_PROTECT(x);
        }
    }

    if (y == R_NoObject)
        SubAssignArgs (&subs, &y, call);
    else if (ON_SCALAR_STACK(y) && !isVectorAtomic(x))
        y = DUP_STACK_VALUE(y);

    if (sb1 == R_NoObject) {
        if (subs != R_NilValue) {
            sb1 = CAR(subs);
            subs = CDR(subs);
        }
    }

    if (sb2 == R_NoObject) {
        if (subs != R_NilValue) {
            sb2 = CAR(subs);
            subs = CDR(subs);
        }
    }

    Rboolean S4 = IS_S4_OBJECT(x);
    int oldtype = NILSXP;

    if (isVector(x)) {
        if (LENGTH(x) == 0) {
            if (length(y) == 0) {
                RETURN_SEXP_INSIDE_PROTECT(x);
            }
        }
        else if (NAMEDCNT_GT_1(x))
            x = dup_top_level(x);
    }
    else if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP) {
        oldtype = TYPEOF(x);
        SEXP ox = x;
        x = PairToVectorList(x);
        setAttrib (x, R_DimSymbol, getAttrib (ox, R_DimSymbol));
        setAttrib (x, R_DimNamesSymbol, getAttrib (ox, R_DimNamesSymbol));
    }
    else if (x == R_NilValue) {
        if (length(y) == 0) {
            RETURN_SEXP_INSIDE_PROTECT(x);
        }
        x = coerceVector(x, TYPEOF(y));
    }
    else
        nonsubsettable_error(call,x);

    if (sb1 == R_NoObject) {
        /* 0 subscript arguments */
        x = VectorAssign(call, x, R_MissingArg, y);
    }
    else if (sb2 == R_NoObject) {
        /* 1 subscript argument */
        if (seq) {
            int start, end;
            sb1 = Rf_DecideVectorOrRange (seq, &start, &end, call);
            if (sb1 == R_NoObject) {
                R_len_t leny;
                if (start < 0 || y != R_NilValue && (leny = length(y)) != 1
                                                 && leny != end - start + 1)
                    sb1 = Rf_VectorFromRange (start, end);
                else
                    x = VectorAssignSeq (call, x, start, end, y);
            }
        }
        if (sb1 != R_NoObject) {
            x = VectorAssign (call, x, sb1, y);
        }
    }
    else if (subs == R_NilValue) {
        /* 2 subscript arguments */
        x = MatrixAssign(call, x, sb1, sb2, y);
    }
    else {
        /* More than 2 subscript arguments */
        subs = CONS(sb1,CONS(sb2,subs));
        x = ArrayAssign(call, x, subs, y);
    }

  out:
    if (oldtype == LANGSXP) {
        if (LENGTH(x)==0)
            errorcall(call,
              _("result is zero-length and so cannot be a language object"));
        x = VectorToPairList(x);
        SET_TYPEOF (x, LANGSXP);
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    if (!isList(x)) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);

    RETURN_SEXP_INSIDE_PROTECT(x);
    END_PROTECT;
}

/* The [[<- operator is implemented by do_subassign2 in eval.c, which calls
   routines below. */

SEXP attribute_hidden do_subassign2_dflt         /* called from elsewhere too */
                        (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return do_subassign2_dflt_int 
         (call, CAR(args), R_NoObject, R_NoObject, CDR(args), rho, R_NoObject);
}

/* Sets R_Visible to TRUE. */
SEXP attribute_hidden do_subassign2_dflt_int
         (SEXP call, SEXP x, SEXP sb1, SEXP sb2, SEXP subs, SEXP rho, SEXP y)
{
    SEXP dims, newname, xup;
    int i, ndims, nsubs, offset, off = -1 /* -Wall */, stretch;
    Rboolean S4, recursed;
    R_len_t length_x;

    R_Visible = TRUE;

    BEGIN_PROTECT2 (names, xtop);
    ALSO_PROTECT5 (x, sb1, sb2, subs, y);

    SEXP xOrig = R_NilValue;

    if (y == R_NoObject)
        SubAssignArgs (&subs, &y, call);
    else if (ON_SCALAR_STACK(y) && !isVectorAtomic(x))
        y = DUP_STACK_VALUE(y);

    nsubs = 0;

    if (sb1 != R_NoObject)
        nsubs += 1;
    else {
        if (subs != R_NilValue) {
            sb1 = CAR(subs);
            subs = CDR(subs);
            nsubs += 1;
        }
    }

    if (sb2 != R_NoObject)
        nsubs += 1;
    else {
        if (subs != R_NilValue) {
            sb2 = CAR(subs);
            subs = CDR(subs);
            nsubs += 1;
        }
    }

    if (subs != R_NilValue)
        nsubs += length(subs);

    /* At this point, nsubs will be the number of indexes, sb1 will be the first
       index, sb2 the second, subs will be a pairlist of remaining indexes. */

    WAIT_UNTIL_COMPUTED(x);

    S4 = IS_S4_OBJECT(x);

    dims = getDimAttrib(x);
    ndims = dims==R_NilValue ? 1 : LENGTH(dims);

    /* Note: below, no duplication is necessary for environments. */

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
        xOrig = x; /* will be an S4 object */
        x = R_getS4DataSlot(x, ANYSXP);
        if(TYPEOF(x) != ENVSXP)
          errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    /* ENVSXP special case first; requires that nsubs be 1. */
    if( TYPEOF(x) == ENVSXP) {
        if (nsubs != 1 || !isString(sb1) || length(sb1) != 1)
            errorcall(call,_("wrong args for environment subassignment"));
        SEXP name = install (translateChar (STRING_ELT (sb1, 0)));
        set_var_in_frame (name, y, x, TRUE, 3);
        RETURN_SEXP_INSIDE_PROTECT (S4 ? xOrig : x);
    }

    if (x == R_NilValue) {
        /* Handle NULL left-hand sides.  If the right-hand side is NULL,
           just return NULL, otherwise replace x by a zero length list 
           (VECSXP) or vector of type of y (if y of length one).  (This
           dependence on the length of y is of dubious wisdom!) */
        if (y == R_NilValue) {
            RETURN_SEXP_INSIDE_PROTECT (R_NilValue);
        }
        if (length(y) == 1)
            x = allocVector(TYPEOF(y), 0);
        else
            x = allocVector(VECSXP, 0);
    }
    else if (isPairList(x))
        x = duplicate(x);
    else if (isVectorList(x)) {
        if (NAMEDCNT_GT_1(x) || x == y)
            x = dup_top_level(x);
    }

    /* Special fast handling of one numeric or string index for a vector object,
       with no complications.  Any possible error conditions are handled by
       just falling through for the code below to handle it. */

    if (nsubs == 1 && ndims <= 1 && !S4
         && (isVectorAtomic(x) || isVectorList(x) && y != R_NilValue)) {
        int type_plus_sb1 = TYPE_ETC(sb1) & ~TYPE_ET_CETERA_HAS_ATTR;
        if ((type_plus_sb1 == REALSXP || type_plus_sb1 == INTSXP 
                || type_plus_sb1 == STRSXP) && 
              (TYPE_ETC(y) & ~TYPE_ET_CETERA_HAS_ATTR) == TYPEOF(x)) {
            R_len_t lenx = LENGTH(x);
            R_len_t ix = 0;
            if (type_plus_sb1 == INTSXP)
                ix = INTEGER(sb1)[0];
            else if (type_plus_sb1 == REALSXP) {
                double d = REAL(sb1)[0];
                if (!ISNAN(d)) {
                    ix = (int) d;
                    if ((double) ix != d)
                        ix = 0;
                }
            }
            else { /* string */
                SEXP names = getAttrib (x, R_NamesSymbol);
                if (TYPEOF(names) == STRSXP) {
                    SEXP se = STRING_ELT(sb1,0);
                    if (se != NA_STRING && CHAR(se)[0] != 0) {
                        for (int i = 0; i < lenx; i++) {
                            if (STRING_ELT(names,i) != NA_STRING 
                                 && SEQL (STRING_ELT(names,i), se)) {
                                ix = i + 1;
                                break;
                            }
                        }
                    }
                }
            }
            if (ix > 0 && ix <= lenx) {
                ix -= 1;
                switch (TYPEOF(x)) {
                    case RAWSXP:
                        RAW(x)[ix] = *RAW(y);
                        break;
                    case LGLSXP:
                        LOGICAL(x)[ix] = *LOGICAL(y);
                        break;
                    case INTSXP:
                        INTEGER(x)[ix] = *INTEGER(y);
                        break;
                    case REALSXP:
                        REAL(x)[ix] = *REAL(y);
                        break;
                    case CPLXSXP:
                        COMPLEX(x)[ix] = *COMPLEX(y);
                        break;
                    case STRSXP:
                        SET_STRING_ELT (x, ix, STRING_ELT(y,0));
                        break;
                    case VECSXP: case EXPRSXP:
                        DEC_NAMEDCNT (VECTOR_ELT (x, ix));
                        SET_VECTOR_ELEMENT_TO_VALUE (x, ix, y);
                    break;
                }
                SET_NAMEDCNT_0(x);
                RETURN_SEXP_INSIDE_PROTECT (x);
            }
        }
    }

    xtop = xup = x; /* x will contain the element which is assigned to; */
                    /*   xup may contain x; xtop is what is returned.  */ 

    recursed = FALSE;

    R_len_t len;

    if (nsubs == 1) { /* One vector index for a list. */
        len = length(sb1);
        if (len > 1) {
            for (int i = 0; i < len-1; i++) {
                if (!isVectorList(x) && !isPairList(x))
                    errorcall (call, 
                      _("recursive indexing failed at level %d\n"), i+1);
                length_x = length(x);
                off = get1index (sb1, 
                        isString(sb1) ? getNamesAttrib(x) : R_NilValue,
                        length_x, TRUE, i, call);
                if (off < 0 || off >= length_x)
                    errorcall(call, _("no such index at level %d\n"), i+1);
                xup = x;
                if (isPairList(xup))
                    x = CAR (nthcdr (xup, off));
                else {
                    x = VECTOR_ELT (xup, off);
                    if (isPairList(x)) {
                        x = duplicate(x);
                        SET_VECTOR_ELT (xup, off, x);
                    }
                    else if (isVectorList(x) && NAMEDCNT_GT_1(x)) {
                        x = dup_top_level(x);
                        SET_VECTOR_ELT (xup, off, x);
                    }
                }
            }
            recursed = TRUE;
        }
    }

    if (isVector(x)) {
        R_len_t length_y = length(y);
        if (!isVectorList(x) && length_y == 0)
            errorcall(call,_("replacement has length zero"));
        if (!isVectorList(x) && length_y > 1)
            errorcall(call,
               _("more elements supplied than there are to replace"));
        if (nsubs == 0 || sb1 == R_MissingArg)
            errorcall(call,_("[[ ]] with missing subscript"));
    }

    stretch = 0;
    newname = R_NilValue;

    length_x = length(x);

    if (nsubs == 1) {
        int str_sym_sub = isString(sb1) || isSymbol(sb1);
        offset = get1index (sb1, 
                   str_sym_sub ? getNamesAttrib(x) : R_NilValue,
                   length_x, FALSE, (recursed ? len-1 : -1), call);
        if (offset < 0) {
            if (str_sym_sub)
                offset = length_x;
            else
                errorcall(call,_("[[ ]] subscript out of bounds"));
        }
        if (offset >= length_x) {
            stretch = offset + 1;
            newname = isString(sb1) ? STRING_ELT(sb1,len-1)
                    : isSymbol(sb1) ? PRINTNAME(sb1) : R_NilValue;
        }
    }
    else {
        if (ndims != nsubs)
            errorcall(call,_("[[ ]] improper number of subscripts"));
        names = getAttrib(x, R_DimNamesSymbol);
        offset = 0;
        SEXP s = subs;
        for (i = ndims-1; i >= 0; i--) {
            SEXP ix;
            if (i == 0)
                ix = sb1;
            else if (i == 1)
                ix = sb2;
            else {
                ix = CAR(s);
                s = CDR(s);
            }
            R_len_t ii = 
             get1index(ix, names==R_NilValue ? R_NilValue : VECTOR_ELT(names,i),
                       INTEGER(dims)[i], /*partial ok*/ FALSE, -1, call);
            if (ii < 0 || ii >= INTEGER(dims)[i])
                errorcall(call,_("[[ ]] subscript out of bounds"));
            offset += ii;
            if (i > 0) offset *= INTEGER(dims)[i-1];
        }
    }

    if (isVector(x)) {

        if (nsubs == 1 && isVectorList(x) && y == R_NilValue) {
            x = DeleteListElementsSeq (x, offset+1, offset+1);
        }
        else {

            SubassignTypeFix(&x, &y, stretch, 2, call);
    
            if (NAMEDCNT_GT_1(x) || x == y)
                x = dup_top_level(x);
    
            if (isVectorAtomic(x))
                copy_elements_coerced (x, offset, 0, y, 0, 0, 1);
            else if (isVectorList(x)) {
                DEC_NAMEDCNT (VECTOR_ELT(x, offset));
                SET_VECTOR_ELEMENT_TO_VALUE (x, offset, y);
            }
            else
                errorcall(call,
                   _("incompatible types (from %s to %s) in [[ assignment"),
                      type2char(TYPEOF(y)), type2char(TYPEOF(x)));
    
            /* If we stretched, we may have a new name. */
            /* In this case we must create a names attribute */
            /* (if it doesn't already exist) and set the new */
            /* value in the names attribute. */
            if (stretch && newname != R_NilValue) {
                names = getNamesAttrib(x);
                if (names == R_NilValue) {
                    names = allocVector(STRSXP, LENGTH(x));
                    SET_STRING_ELT(names, offset, newname);
                    setAttrib(x, R_NamesSymbol, names);
                }
                else
                    SET_STRING_ELT(names, offset, newname);
            }
        }
    }

    else if (isPairList(x)) {

        SET_NAMEDCNT_MAX(y);
        if (nsubs == 1) {
            if (y == R_NilValue)
                x = with_no_nth(x,offset+1);
            else if (!stretch)
                x = with_changed_nth(x,offset+1,y);
            else {
                SEXP append = 
                  cons_with_tag (y, R_NilValue, newname == R_NilValue ? 
                                  R_NilValue : install(translateChar(newname)));
                for (i = length_x + 1; i < stretch; i++)
                    append = CONS(R_NilValue,append);
                x = with_pairlist_appended(x,append);
            }
        }
        else {
            SEXP nth = nthcdr(x,offset);
            SETCAR(nth,y);
        }
    }

    else 
        nonsubsettable_error(call,x);

    /* The modified "x" may now be a different object, due to deletion or
       extension, so we need to update the reference to it. */

    if (recursed) {
        if (isVectorList(xup))
            SET_VECTOR_ELT(xup, off, x);
        else {
            SETCAR(nthcdr(xup,off),x); /* xup was duplicated, so this is safe */
            SET_NAMEDCNT_MAX(x);
        }
    }
    else
        xtop = x;

    if (!isList(xtop)) SET_NAMEDCNT_0(xtop);
    if(S4) SET_S4_OBJECT(xtop);

    RETURN_SEXP_INSIDE_PROTECT (xtop);
    END_PROTECT;
}

/* Called from do_subassing3, and elsewhere.  Protects x and val; name should be
   a symbol and hence not needing protection.  Sets R_Visible to TRUE. */

#define na_or_empty_string(strelt) ((strelt)==NA_STRING || CHAR((strelt))[0]==0)

SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP name, SEXP val)
{
    PROTECT_INDEX pvalidx, pxidx;
    Rboolean S4; SEXP xS4 = R_NilValue;

    R_Visible = TRUE;

    if (ON_SCALAR_STACK(val)) /* currently, never puts value in atomic vector */
        val = DUP_STACK_VALUE(val); 

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = IS_S4_OBJECT(x);

    WAIT_UNTIL_COMPUTED(x);

    /* code to allow classes to extend ENVSXP */
    if (TYPEOF(x) == S4SXP) {
	xS4 = x;
        x = R_getS4DataSlot(x, ANYSXP);
	if (x == R_NilValue)
            errorcall (call, 
              _("no method for assigning subsets of this S4 class"));
    }

    switch (TYPEOF(x)) {

    case LISTSXP: case LANGSXP: ;
        int ix = tag_index(x,name);
        if (ix == 0) {
            if (val != R_NilValue) {
                x = with_pairlist_appended (x,
                      cons_with_tag (val, R_NilValue, name));
                SET_NAMEDCNT_MAX(val);
            }
        }
        else if (val == R_NilValue) {
            x = with_no_nth (x, ix);
        }
        else {
            x = with_changed_nth (x, ix, val);
            SET_NAMEDCNT_MAX(val);
        }
        break;

    case ENVSXP:
	set_var_in_frame (name, val, x, TRUE, 3);
        break;

    case SYMSXP: case CLOSXP: case SPECIALSXP: case BUILTINSXP:
        /* Used to 'work' in R < 2.8.0 */
        nonsubsettable_error(call,x);

    default:
	warningcall(call,_("Coercing LHS to a list"));
	REPROTECT(x = coerceVector(x, VECSXP), pxidx);
	/* fall through to VECSXP / EXPRSXP / NILSXP code */

    case VECSXP: case EXPRSXP: case NILSXP: ;

        SEXP pname = PRINTNAME(name);
        int type = TYPEOF(x);
        int imatch = -1;
        SEXP names;
        R_len_t nx;

        if (type == NILSXP) {
            names = R_NilValue;
            type = VECSXP;
            nx = 0;
        }
        else {
            if (NAMEDCNT_GT_1(x) || x == val)
                REPROTECT(x = dup_top_level(x), pxidx);
            names = getNamesAttrib(x);
            nx = LENGTH(x);

            /* Set imatch to the index of the selected element, stays at
               -1 if not present.  Note that NA_STRING and "" don't match 
               anything. */

            if (names != R_NilValue && !na_or_empty_string(pname)) {
                for (int i = 0; i < nx; i++) {
                    SEXP ni = STRING_ELT(names, i);
                    if (SEQL(ni,pname) && !na_or_empty_string(ni)) {
                        imatch = i;
                        break;
                    }
                }
            }
        }

        if (val == R_NilValue) {
            /* If "val" is NULL, this is an element deletion if there
               is a match to "name" otherwise "x" is unchanged. */
            if (imatch >= 0)
                x = DeleteListElementsSeq (x, imatch+1, imatch+1);
            /* else x is unchanged */
        }
        else {
            /* If "val" is non-NULL, we are either replacing an existing 
               list element or we are adding a new element. */
	    if (imatch >= 0) {
		/* We are just replacing an element */
                DEC_NAMEDCNT (VECTOR_ELT(x,imatch));
		SET_VECTOR_ELEMENT_TO_VALUE(x, imatch, val);
	    }
	    else {
		SEXP ans, ansnames;
                if (x == R_NilValue)
                    PROTECT (ans = allocVector (VECSXP, 1));
                else
                    PROTECT (ans = reallocVector (x, nx+1, 1));
                if (names == R_NilValue || NAMEDCNT_GT_1(names)) {
                    PROTECT(ansnames = allocVector (STRSXP, nx+1));
                    if (names != R_NilValue)
                        copy_string_elements (ansnames, 0, names, 0, nx);
                }
                else {
                    PROTECT(ansnames = reallocVector (names, nx+1, 1));
                }
		SET_VECTOR_ELEMENT_TO_VALUE (ans, nx, val);
		SET_STRING_ELT (ansnames, nx, pname);
                no_dim_attributes(ans);
		setAttrib (ans, R_NamesSymbol, ansnames);
		UNPROTECT(2);
		x = ans;
	    }
	}
        break;
    }

    UNPROTECT(2);
    if(xS4 != R_NilValue)
	x = xS4; /* x was an env't, the data slot of xS4 */
    if (!isList(x)) SET_NAMEDCNT_0(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_subset[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
