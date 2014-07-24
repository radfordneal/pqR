/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2011  The R Development Core Team.
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

/* The x:y primitive calls do_colon(); do_colon() calls cross_colon() if
   both arguments are factors and seq_colon() otherwise.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>
#include <float.h>  /* for DBL_EPSILON */
#include <Rmath.h>

#include <helpers/helpers-app.h>

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#define _S4_rep_keepClass
/* ==>  rep(<S4>, .) keeps class e.g., for list-like */

static SEXP cross_colon(SEXP call, SEXP s, SEXP t)
{
    SEXP a, la, ls, lt, rs, rt;
    int i, j, k, n, nls, nlt, vs, vt;
    char *cbuf;

    if (length(s) != length(t))
	errorcall(call, _("unequal factor lengths"));
    n = length(s);
    ls = getAttrib(s, R_LevelsSymbol);
    lt = getAttrib(t, R_LevelsSymbol);
    nls = LENGTH(ls);
    nlt = LENGTH(lt);
    PROTECT(a = allocVector(INTSXP, n));
    PROTECT(rs = coerceVector(s, INTSXP));
    PROTECT(rt = coerceVector(t, INTSXP));
    for (i = 0; i < n; i++) {
	vs = INTEGER(rs)[i];
	vt = INTEGER(rt)[i];
	if ((vs == NA_INTEGER) || (vt == NA_INTEGER))
	    INTEGER(a)[i] = NA_INTEGER;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    UNPROTECT(2);
    if (!isNull(ls) && !isNull(lt)) {
	PROTECT(la = allocVector(STRSXP, nls * nlt));
	k = 0;
	/* FIXME: possibly UTF-8 version */
	for (i = 0; i < nls; i++) {
	    const char *vi = translateChar(STRING_ELT(ls, i));
	    vs = strlen(vi);
	    for (j = 0; j < nlt; j++) {
		const char *vj = translateChar(STRING_ELT(lt, j));
		vt = strlen(vj);
                int len = vs + vt + 1;
		cbuf = R_AllocStringBuffer(len, &cbuff);
		(void) copy_3_strings(cbuf,len+1,vi,":",vj);
		SET_STRING_ELT(la, k, mkChar(cbuf));
		k++;
	    }
	}
	setAttrib(a, R_LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = mkString("factor"));
    setAttrib(a, R_ClassSymbol, la);
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return(a);
}

/* Create a simple integer sequence, or as variant, a description of it. 
   Sets R_variant_result to 1 if a sequence description is returned. */
static SEXP make_seq (int from, int len, int variant)
{
    SEXP ans;
    int *p;

    if (VARIANT_KIND(variant) == VARIANT_SEQ) {
        ans = allocVector (INTSXP, 2);
        p = INTEGER(ans);
        p[0] = from;
        p[1] = from + len - 1;
        R_variant_result = 1;
    }
    else {
        ans = allocVector (INTSXP, len);
        p = INTEGER(ans);
        for (int i = 0; i < len; i++) p[i] = from + i;
    }

    return ans;
}

static SEXP seq_colon(double n1, double n2, SEXP call, int variant)
{
    int i, n, in1;
    double r;
    SEXP ans;
    Rboolean useInt;

    r = fabs(n2 - n1);
    if(r >= INT_MAX) errorcall(call,_("result would be too long a vector"));

    n = r + 1 + FLT_EPSILON;

    in1 = (int)(n1);
    useInt = (n1 == in1);
    if(useInt) {
	if(n1 <= INT_MIN || n1 > INT_MAX)
	    useInt = FALSE;
	else {
	    /* r := " the effective 'to' "  of  from:to */
	    r = n1 + ((n1 <= n2) ? n-1 : -(n-1));
	    if(r <= INT_MIN || r > INT_MAX)
		useInt = FALSE;
	}
    }
    if (useInt) {
        if (n1 <= n2)
            ans = make_seq (in1, n, variant);
        else {
	    ans = allocVector(INTSXP, n);
            for (i = 0; i < n; i++) INTEGER(ans)[i] = in1 - i;
        }
    } else {
	ans = allocVector(REALSXP, n);
	if (n1 <= n2)
	    for (i = 0; i < n; i++) REAL(ans)[i] = n1 + i;
	else
	    for (i = 0; i < n; i++) REAL(ans)[i] = n1 - i;
    }
    return ans;
}

static SEXP do_fast_colon (SEXP call, SEXP op, SEXP s1, SEXP s2, SEXP rho,
                           int variant)
{   double n1, n2;

    if (inherits(s1, "factor") && inherits(s2, "factor"))
	return cross_colon (call, s1, s2);

    n1 = length(s1);
    n2 = length(s2);

    if (n1 == 0 || n2 == 0)
	errorcall(call, _("argument of length 0"));
    if (n1 > 1)
	warningcall(call, 
          _("numerical expression has %d elements: only the first used"), 
          (int) n1);
    if (n2 > 1)
	warningcall(call, 
          _("numerical expression has %d elements: only the first used"), 
          (int) n2);

    n1 = asReal(s1);
    n2 = asReal(s2);
    if (ISNAN(n1) || ISNAN(n2))
	errorcall(call, _("NA/NaN argument"));

    return seq_colon(n1, n2, call, variant);
}

static SEXP do_colon(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{   
    checkArity(op, args);
    return do_fast_colon (call, op, CAR(args), CADR(args), rho, variant);
}

/* Task procedure for rep and rep.int.  Repeats first input to the length
   of the output.  If the second input is null, repeats each element once
   in each cycle; if it is scalar, repeats each element that many times
   each cycle; if it is a vector (same length as first input), it repeats 
   each element the specified number of times each cycle. 
  
   Should be master-only if input is not numeric.  Does not pipeline input
   or output. */

void task_rep (helpers_op_t op, SEXP a, SEXP s, SEXP t)
{
    int na = length(a), ns = length(s);
    int i, j, k;
    SEXP u;

    if (TYPEOF(a) != TYPEOF(s) || t != NULL && TYPEOF(t) != INTSXP) abort();

    if (na <= 0) return;

    if (t == NULL || LENGTH(t) == 1 && INTEGER(t)[0] == 1) {
        if (ns == 1) {
            /* Repeat of a single element na times. */
            switch (TYPEOF(s)) {
            case LGLSXP: {
                int v = LOGICAL(s)[0];
                for (i = 0; i < na; i++) LOGICAL(a)[i] = v;
                break;
            }
            case INTSXP: {
                int v = INTEGER(s)[0];
                for (i = 0; i < na; i++) INTEGER(a)[i] = v;
                break;
            }
            case REALSXP: {
                double v = REAL(s)[0];
                for (i = 0; i < na; i++) REAL(a)[i] = v;
                break;
            }
            case CPLXSXP: {
                Rcomplex v = COMPLEX(s)[0];
                for (i = 0; i < na; i++) COMPLEX(a)[i] = v;
                break;
            }
            case RAWSXP: {
                Rbyte v = RAW(s)[0];
                for (i = 0; i < na; i++) RAW(a)[i] = v;
                break;
            }
            case STRSXP: {
                SEXP v = STRING_ELT(s,0);
                for (i = 0; i < na; i++) SET_STRING_ELT (a, i, v);
                break;
            }
            case LISTSXP: {
                SEXP v = CAR(s);
                for (u = a; u != R_NilValue; u = CDR(u)) 
                    SETCAR (u, duplicate(v));
                break;
            }
            case EXPRSXP:
            case VECSXP: {
                SEXP v = VECTOR_ELT (s, 0);
                SET_VECTOR_ELEMENT_FROM_VECTOR (a, 0, s, 0);
                for (i = 1; i < na; i++) {
                    SET_VECTOR_ELT (a, i, v);
                    INC_NAMEDCNT_0_AS_1 (v);
                }
        	break;
            }
            default: abort();
            }
        }
        else {
            /* Simple repeat of a vector to length na. */
            switch (TYPEOF(s)) {
            case LGLSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    LOGICAL(a)[i] = LOGICAL(s)[j];
                }
                break;
            case INTSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    INTEGER(a)[i] = INTEGER(s)[j];
                }
                break;
            case REALSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    REAL(a)[i] = REAL(s)[j];
                }
                break;
            case CPLXSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    COMPLEX(a)[i] = COMPLEX(s)[j];
                }
                break;
            case RAWSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    RAW(a)[i] = RAW(s)[j];
                }
                break;
            case STRSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    SET_STRING_ELT(a, i, STRING_ELT(s, j));
                }
                break;
            case LISTSXP:
                for (u = a, j = 0; u != R_NilValue; u = CDR(u), j++) {
                    if (j >= ns) j = 0;
                    SETCAR (u, duplicate (CAR (nthcdr (s, j))));
                }
                break;
            case EXPRSXP:
            case VECSXP:
                for (i = 0, j = 0; i < na; i++, j++) {
                    if (j >= ns) j = 0;
                    if (i < ns)
                        SET_VECTOR_ELEMENT_FROM_VECTOR (a, i, s, j);
                    else {
                        SEXP v = VECTOR_ELT (s, j);
                        SET_VECTOR_ELT (a, i, v);
                        INC_NAMEDCNT_0_AS_1 (v);
                    }
                }
        	break;
            default: abort();
            }
        }
    }

    else {
        if (LENGTH(t) == 1) {
            /* Repeat each element of s same number of times in each cycle. */
            int each = INTEGER(t)[0];
            if (each == 0) return;
            switch (TYPEOF(s)) {
            case LGLSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    int v = LOGICAL(s)[j];
                    for (k = each; k > 0; k--) {
                        LOGICAL(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case INTSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    int v = INTEGER(s)[j];
                    for (k = each; k > 0; k--) {
                        INTEGER(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case REALSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    double v = REAL(s)[j];
                    for (k = each; k > 0; k--) {
                        REAL(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case CPLXSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    Rcomplex v = COMPLEX(s)[j];
                    for (k = each; k > 0; k--) {
                        COMPLEX(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case RAWSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    Rbyte v = RAW(s)[j];
                    for (k = each; k > 0; k--) {
                        RAW(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case STRSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = STRING_ELT (s, j);
                    for (k = each; k > 0; k--) {
                        SET_STRING_ELT (a, i, v);
                        if (++i == na) return;
                    }
                }
            case LISTSXP:
                for (u = a, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = CAR (nthcdr (s, j));
                    for (k = each; k > 0; k--) {
                        SETCAR (u, duplicate(v));
                        u = CDR(u);
                        if (u == R_NilValue) return;
                    }
                }
            case EXPRSXP:
            case VECSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = VECTOR_ELT (s, j);
                    for (k = each; k > 0; k--) {
                        SET_VECTOR_ELT (a, i, v);
                        INC_NAMEDCNT_0_AS_1 (v);
                        if (++i == na) return;
                    }
                }
            default: abort();
            }
        }
        else {
            /* Repeat elements varying numbers of times in each cycle. */
            int *eachv = INTEGER(t);
            if (LENGTH(t) != ns) abort();
            switch (TYPEOF(s)) {
            case LGLSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    int v = LOGICAL(s)[j];
                    for (k = eachv[j]; k > 0; k--) {
                        LOGICAL(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case INTSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    int v = INTEGER(s)[j];
                    for (k = eachv[j]; k > 0; k--) {
                        INTEGER(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case REALSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    double v = REAL(s)[j];
                    for (k = eachv[j]; k > 0; k--) {
                        REAL(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case CPLXSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    Rcomplex v = COMPLEX(s)[j];
                    for (k = eachv[j]; k > 0; k--) {
                        COMPLEX(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case RAWSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    Rbyte v = RAW(s)[j];
                    for (k = eachv[j]; k > 0; k--) {
                        RAW(a)[i] = v;
                        if (++i == na) return;
                    }
                }
            case STRSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = STRING_ELT (s, j);
                    for (k = eachv[j]; k > 0; k--) {
                        SET_STRING_ELT (a, i, v);
                        if (++i == na) return;
                    }
                }
            case LISTSXP:
                for (u = a, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = CAR (nthcdr (s, j));
                    for (k = eachv[j]; k > 0; k--) {
                        SETCAR (u, duplicate(v));
                        u = CDR(u);
                        if (u == R_NilValue) return;
                    }
                }
            case EXPRSXP:
            case VECSXP:
                for (i = 0, j = 0; ; j++) {
                    if (j >= ns) j = 0;
                    SEXP v = VECTOR_ELT (s, j);
                    for (k = eachv[j]; k > 0; k--) {
                        SET_VECTOR_ELT (a, i, v);
                        INC_NAMEDCNT_0_AS_1 (v);
                        if (++i == na) return;
                    }
                }
            default: abort();
            }
        }
    }
}

static SEXP do_rep_int(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP s = CAR(args), ncopy = CADR(args), a;
    int na;

    if (!isVector(ncopy))
	error(_("incorrect type for second argument"));

    if (!isVector(s) && TYPEOF(s) != LISTSXP)
	error(_("attempt to replicate non-vector"));

    int ns = length(s);
    int nc = length(ncopy);

    if (nc == ns) {
        PROTECT(ncopy = coerceVector(ncopy, INTSXP));
        if (TYPEOF(ncopy) != INTSXP || LENGTH(ncopy) != nc) abort();
        na = 0;
        for (int i = 0; i < nc; i++) {
	    if (INTEGER(ncopy)[i] == NA_INTEGER || INTEGER(ncopy)[i] < 0)
	        error(_("invalid '%s' value"), "times");
            if ((double)na + INTEGER(ncopy)[i] > INT_MAX)
                error(_("invalid '%s' value"), "times");
            na += INTEGER(ncopy)[i];
        }
    }
    else {	
	if (nc != 1) error(_("invalid '%s' value"), "times");
        int ncv = asInteger(ncopy);
	if (ncv == NA_INTEGER || ncv < 0 || (double)ncv*ns > INT_MAX)
	    error(_("invalid '%s' value"), "times"); /* ncv = 0 is OK */
        na = ncv * ns;
        ncopy = NULL;
    }

    PROTECT(a = allocVector(TYPEOF(s), na));

    task_rep (0, a, s, ncopy);

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} 
        else 
            PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }

    UNPROTECT(1 + (ncopy!=NULL));
    return a;
}

/* We are careful to use evalListKeepMissing here (inside
   DispatchOrEval) to avoid dropping missing arguments so e.g.
   rep(1:3,,8) matches length.out */

/* This is a primitive SPECIALSXP with internal argument matching */

static SEXP do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, times;
    int i, len, each, nprotect = 0;
    static char *ap[5] = { "x", "times", "length.out", "each", "..." };

    if (DispatchOrEval(call, op, "rep", args, rho, &ans, 0, 0))
	return(ans);

    /* This has evaluated all the non-missing arguments into ans */
    PROTECT(args = ans);
    nprotect++;

    /* This is a primitive, and we have not dispatched to a method
       so we manage the argument matching ourselves.  We pretend this is
       rep(x, times, length.out, each, ...)
    */
    PROTECT(args = matchArgs(R_NilValue, ap, 5, args, call));
    nprotect++;

    SEXP x = CAR(args); args = CDR(args);
    SEXP times_arg = CAR(args); args = CDR(args);
    SEXP length_arg = CAR(args); args = CDR(args);
    SEXP each_arg = CAR(args);

    if (x == R_NilValue) {
        unprotect(nprotect);
        return R_NilValue;
    }

    if (!isVector(x) && !isList(x))
	error(_("attempt to replicate non-vector"));
    
    int lx = length(x);

    len = asInteger(length_arg);
    if(len != NA_INTEGER && len < 0)
	errorcall(call, _("invalid '%s' argument"), "length.out");
    if(length(length_arg) != 1)
	warningcall(call, _("first element used of '%s' argument"), 
		    "length.out");

    int le = length(each_arg);
    each = asInteger(each_arg);
    if (each == NA_INTEGER) 
        each = 1;
    if (le == 0 || each < 0)
	errorcall(call, _("invalid '%s' argument"), "each");
    if (le != 1)
	warningcall(call, _("first element used of '%s' argument"), "each");

    if (lx == 0) {
        PROTECT(x = duplicate(x));
        nprotect++;
        if (len != NA_INTEGER && len > 0)
            x = lengthgets(x,len);
	UNPROTECT(nprotect);
        return x;
    }

    if (len != NA_INTEGER) { /* takes precedence over times */
        if(len > 0 && each == 0)
            errorcall(call, _("invalid '%s' argument"), "each");
        times = NULL;
    } 
    else {  /* len == NA_INTEGER */
	int nt;
	if(times_arg == R_MissingArg) 
            PROTECT(times = ScalarIntegerMaybeConst(1));
	else 
            PROTECT(times = coerceVector(times_arg, INTSXP));
	nprotect++;
	nt = LENGTH(times);
	if (nt == 1) {
	    int it = INTEGER(times)[0];
	    if (it == NA_INTEGER || it < 0 || (double) lx * it * each > INT_MAX)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = lx * it * each;
            times = NULL;
	} 
        else {
            if (nt != (double) lx * each)
                errorcall(call, _("invalid '%s' argument"), "times");
            len = 0;
	    for(i = 0; i < nt; i++) {
		int it = INTEGER(times)[i];
		if (it == NA_INTEGER || it < 0 || (double)len + it > INT_MAX)
		    errorcall(call, _("invalid '%s' argument"), "times");
		len += it;
	    }
            /* Here, convert calls like rep(c(T,F),each=2,times=c(1,3,5,2)) 
               to rep(c(T,F),each=1,times=c(4,7)) */
            if (each != 1) {
                SEXP old_times = times;
                int j;
                times = allocVector (INTSXP, nt/each);
                UNPROTECT(1);
                PROTECT(times);
                for (j = 0; j < LENGTH(times); j++) {
                    INTEGER(times)[j] = 0;
                    for (i = 0; i < each; i++) 
                        INTEGER(times)[j] += INTEGER(old_times)[i+each*j];
                }
                each = 1;
            }
	}
    }

    SEXP xn = getAttrib(x, R_NamesSymbol);

    if (TYPEOF(x) == LISTSXP) {
        PROTECT(x = coerceVector(x,VECSXP));
        nprotect++;
    }

    SEXP each_times = each != 1 ? ScalarIntegerMaybeConst(each) : times;

    PROTECT(a = allocVector(TYPEOF(x), len));
    nprotect++;

    task_rep (0, a, x, each_times);

    if (length(xn) > 0) {
        SEXP an = allocVector (TYPEOF(xn), len);
        task_rep (0, an, xn, each_times);
        setAttrib(a, R_NamesSymbol, an);
    }

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    UNPROTECT(nprotect);
    return a;
}


/* do_seq implements seq.int, which dispatches on methods for seq. */

#define FEPS 1e-10
/* to match seq.default */
static SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    SEXP ans = R_NilValue /* -Wall */, from, to, by, len, along;
    int i, nargs = length(args), lf, lout = NA_INTEGER;
    Rboolean One = nargs == 1;
    static char *ap[6] =
        { "from", "to", "by", "length.out", "along.with", "..." };

    if (DispatchOrEval(call, op, "seq", args, rho, &ans, 0, 1))
	return(ans);

    /* This is a primitive and we manage argument matching ourselves.
       We pretend this is
       seq(from, to, by, length.out, along.with, ...)
    */

    PROTECT(args = matchArgs(R_NilValue, ap, 6, args, call));

    from = CAR(args); args = CDR(args);
    to = CAR(args); args = CDR(args);
    by = CAR(args); args = CDR(args);
    len = CAR(args); args = CDR(args);
    along = CAR(args);

    if(One && from != R_MissingArg) {
	lf = length(from);
	if(lf == 1 && (TYPEOF(from) == INTSXP || TYPEOF(from) == REALSXP))
	    ans = seq_colon(1.0, asReal(from), call, variant);
	else if (lf)
	    ans = seq_colon(1.0, (double)lf, call, variant);
	else
	    ans = allocVector(INTSXP, 0);
	goto done;
    }
    if(along != R_MissingArg) {
	lout = LENGTH(along);
	if(One) {
	    ans = lout ? seq_colon(1.0, (double)lout, call, variant) 
                       : allocVector(INTSXP, 0);
	    goto done;
	}
    } else if(len != R_MissingArg && len != R_NilValue) {
	double rout = asReal(len);
	if(ISNAN(rout) || rout <= -0.5)
	    errorcall(call, _("'length.out' must be a non-negative number"));
	if(length(len) != 1)
	    warningcall(call, _("first element used of '%s' argument"), 
			"length.out");
	lout = (int) ceil(rout);
    }

    if(lout == NA_INTEGER) {
        double rfrom = 1.0, rto = 1.0, rby, *ra;
        if (from != R_MissingArg) {
            if (length(from) != 1) error("'from' must be of length 1");
            rfrom = asReal(from);
        }
        if (to != R_MissingArg) {
            if (length(to) != 1) error("'to' must be of length 1");
            rto = asReal(to);
        }
	if(by == R_MissingArg)
	    ans = seq_colon(rfrom, rto, call, variant);
	else {
            if (length(by) != 1) error("'by' must be of length 1");
            rby = asReal(by);
	    double del = rto - rfrom, n, dd;
	    int nn;
	    if(!R_FINITE(rfrom))
		errorcall(call, _("'from' must be finite"));
	    if(!R_FINITE(rto))
		errorcall(call, _("'to' must be finite"));
	    if(del == 0.0 && rto == 0.0) {
		ans = to;
		goto done;
	    }
	    /* printf("from = %f, to = %f, by = %f\n", rfrom, rto, rby); */
	    n = del/rby;
	    if(!R_FINITE(n)) {
		if(del == 0.0 && rby == 0.0) {
		    ans = from;
		    goto done;
		} else
		    errorcall(call, _("invalid '(to - from)/by' in 'seq'"));
	    }
	    dd = fabs(del)/fmax2(fabs(rto), fabs(rfrom));
	    if(dd < 100 * DBL_EPSILON) {
		ans = from;
		goto done;
	    }
	    if(n > (double) INT_MAX)
		errorcall(call, _("'by' argument is much too small"));
	    if(n < - FEPS)
		errorcall(call, _("wrong sign in 'by' argument"));
	    if(TYPEOF(from) == INTSXP &&
	       TYPEOF(to) == INTSXP &&
	       TYPEOF(by) == INTSXP) {
		int *ia, ifrom = asInteger(from), iby = asInteger(by);
		/* With the current limits on integers and FEPS
		   reduced below 1/INT_MAX this is the same as the
		   next, so this is future-proofing against longer integers.
		*/
		nn = (int)n;
		/* seq.default gives integer result from
		   from + (0:n)*by
		*/
		ans = allocVector(INTSXP, nn+1);
		ia = INTEGER(ans);
		for(i = 0; i <= nn; i++)
		    ia[i] = ifrom + i * iby;
	    } else {
		nn = (int)(n + FEPS);
		ans = allocVector(REALSXP, nn+1);
		ra = REAL(ans);
		for(i = 0; i <= nn; i++)
		    ra[i] = rfrom + i * rby;
		/* Added in 2.9.0 */
		if (nn > 0)
		    if((rby > 0 && ra[nn] > rto) || (rby < 0 && ra[nn] < rto))
			ra[nn] = rto;
	    }
	}
    } else if (lout == 0) {
	ans = allocVector(INTSXP, 0);
    } else if (One) {
	ans = seq_colon(1.0, (double)lout, call, variant);
    } else if (by == R_MissingArg) {
	double rfrom = asReal(from), rto = asReal(to), rby;
	if(to == R_MissingArg) rto = rfrom + lout - 1;
	if(from == R_MissingArg) rfrom = rto - lout + 1;
	if(!R_FINITE(rfrom))
	    errorcall(call, _("'from' must be finite"));
	if(!R_FINITE(rto))
	    errorcall(call, _("'to' must be finite"));
	ans = allocVector(REALSXP, lout);
	if(lout > 0) REAL(ans)[0] = rfrom;
	if(lout > 1) REAL(ans)[lout - 1] = rto;
	if(lout > 2) {
	    rby = (rto - rfrom)/(double)(lout - 1);
	    for(i = 1; i < lout-1; i++) REAL(ans)[i] = rfrom + i*rby;
	}
    } else if (to == R_MissingArg) {
	double rfrom = asReal(from), rby = asReal(by), rto;
	if(from == R_MissingArg) rfrom = 1.0;
	if(!R_FINITE(rfrom))
	    errorcall(call, _("'from' must be finite"));
	if(!R_FINITE(rby))
	    errorcall(call, _("'by' must be finite"));
	rto = rfrom +(lout-1)*rby;
	if(rby == (int)rby && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++)
		INTEGER(ans)[i] = rfrom + i*rby;
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++)
		REAL(ans)[i] = rfrom + i*rby;
	}
    } else if (from == R_MissingArg) {
	double rto = asReal(to), rby = asReal(by),
	    rfrom = rto - (lout-1)*rby;
	if(!R_FINITE(rto))
	    errorcall(call, _("'to' must be finite"));
	if(!R_FINITE(rby))
	    errorcall(call, _("'by' must be finite"));
	if(rby == (int)rby && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++)
		INTEGER(ans)[i] = rto - (lout - 1 - i)*rby;
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++)
		REAL(ans)[i] = rto - (lout - 1 - i)*rby;
	}
    } else
	errorcall(call, _("too many arguments"));

done:
    UNPROTECT(1);
    return ans;
}

static SEXP do_seq_along(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    static SEXP length_op = NULL;
    SEXP arg, ans;
    int len;

    /* Store the .Primitive for 'length' for DispatchOrEval to use. */
    if (length_op == NULL) {
	SEXP R_lengthSymbol = install("length");
	length_op = eval(R_lengthSymbol, R_BaseEnv);
	if (TYPEOF(length_op) != BUILTINSXP) {
	    length_op = NULL;
	    error("'length' is not a BUILTIN");
	}
	R_PreserveObject(length_op);
    }

    checkArity(op, args);
    check1arg(args, call, "along.with");
    arg = CAR(args);

    /* Try to dispatch to S3 or S4 methods for 'length'.  For cases
       where no methods are defined this is more efficient than an
       unconditional callback to R */

    if (isObject(arg)
	  && DispatchOrEval(call, length_op, "length", args, rho, &ans, 0, 1)) {
	len = asInteger(ans);
    }
    else
	len = length(arg);

    return make_seq (1, len, variant);
}

static SEXP do_fast_seq_len (SEXP call, SEXP op, SEXP arg, SEXP rho, 
                             int variant)
{   int len = asInteger(arg);
    if(len == NA_INTEGER || len < 0)
	errorcall(call,_("argument must be coercible to non-negative integer"));
    if (length(arg) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");

    return make_seq (1, len, variant);
}

static SEXP do_seq_len(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{   
    checkArity(op, args);
    check1arg(args, call, "length.out");

    return do_fast_seq_len (call, op, CAR(args), rho, variant);
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_seq[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{":",		do_colon,	0,	1001,	2,	{PP_BINARY2, PREC_COLON,  0}},
{"rep.int",	do_rep_int,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq.int",	do_seq,		0,	1001,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_along",	do_seq_along,	0,	11001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len,	0,	1001,	1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};

/* Fast built-in functions in this file. See names.c for documentation */

attribute_hidden FASTFUNTAB R_FastFunTab_seq[] = {
/*slow func	fast func,     code or -1  uni/bi/both dsptch  variants */

{ do_colon,	do_fast_colon,	-1,		2,	0, 0,  0, 0 },
{ do_seq_len,	do_fast_seq_len,-1,		1,	0, 0,  0, 0 },
{ 0,		0,		0,		0,	0, 0,  0, 0 }
};
