/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2011   The R Core Team
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
#include <Defn.h>
#define R_MSG_type	_("invalid 'type' (%s) of argument")
#define imax2(x, y) ((x < y) ? y : x)

#define R_INT_MIN	(1+INT_MIN)
	/* since INT_MIN is the NA_INTEGER value ! */
#define Int2Real(i)	(((i) == NA_INTEGER) ? NA_REAL : (double)(i))

#ifdef DEBUG_sum
#define DbgP1(s) REprintf(s)
#define DbgP2(s,a) REprintf(s,a)
#define DbgP3(s,a,b) REprintf(s,a,b)
#else
#define DbgP1(s)
#define DbgP2(s,a)
#define DbgP3(s,a,b)
#endif

#include <stdint.h>

static int isum(int *x, int n, Rboolean narm, SEXP call)
{
    int_fast64_t s = 0;
    int i;

    if (narm) {
        for (i = 0; i < n; i++) 
	    if (x[i] != NA_INTEGER) s += x[i];
    } else { 
        for (i = 0; i < n; i++) {
            if (x[i] == NA_INTEGER) 
                return NA_INTEGER;
            s += x[i];
        }
    }

    if (s > INT_MAX || s < R_INT_MIN) {
	warningcall(call, _("Integer overflow - use sum(as.numeric(.))"));
	return NA_INTEGER;
    }

    return (int) s;
}

static double rsum (double *x, int n, Rboolean narm)
{
    long double s = 0.0;
    int i;

    if (narm) {
        for (i = 0; i < n; i++) 
	    if (!ISNAN(x[i])) s += x[i];
    } else { 
        for (i = 0; i < n; i++)
            s += x[i];
    }

    return s;
}

static Rcomplex csum (Rcomplex *x, int n, Rboolean narm)
{
    long double sr, si;
    Rcomplex s;
    int i;

    sr = si = 0.0;

    if (narm) {
        for (i = 0; i < n; i++) {
	    if (!ISNAN(x[i].r) && !ISNAN(x[i].i)) {
                sr += x[i].r;
                si += x[i].i;
            }
        }
    } else { 
        for (i = 0; i < n; i++) {
            sr += x[i].r;
            si += x[i].i;
        }
    }

    s.r = sr; 
    s.i = si;

    return s;
}

static Rboolean smin(SEXP x, SEXP *value, Rboolean narm)
{
    int i;
    SEXP s = NA_STRING; /* -Wall */
    Rboolean updated = FALSE;
    int len = length(x);

    for (i = 0; i < len; i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) > 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return(TRUE);
	}
    }
    *value = s;

    return(updated);
}

static Rboolean attribute_noinline imin_max
                      (int *x, int n, int *value, Rboolean narm, int max)
{
    int updated = FALSE;
    int s = 0;

    if (n == 0) goto ret;

    int i = 0;

    /* Look for first non-NA value (or done if !narm and there's a NA). */

    while (x[i] == NA_INTEGER) {
        if (!narm) goto na;
        i += 1;
        if (i >= n) goto ret;
    }

    /* Look at values, update max or min, skip or exit for NA value. */

    s = x[i];
    i += 1;

    if (max) {
        while (i < n) {
            if (x[i] == NA_INTEGER) {
                if (!narm) goto na;
            }
            else if (x[i] > s)
                s = x[i];
            i += 1;
        }
    }
    else { /* min */
        while (i < n) {
            if (x[i] == NA_INTEGER) {
                if (!narm) goto na;
            }
            else if (x[i] < s)
                s = x[i];
            i += 1;
        }
    }

    updated = TRUE;
    goto ret;

  na:

    updated = TRUE;
    s = NA_INTEGER;

  ret:

    *value = s;
    return updated;
}

static Rboolean attribute_noinline rmin_max
                      (double *x, int n, double *value, Rboolean narm, int max)
{
    int updated = FALSE;
    double s = 0;

    if (n == 0) goto ret;

    int i = 0;

    /* Look for first non-NaN value (or done if !narm and there's a NaN). */

    while (ISNAN(x[i])) {
        if (!narm) goto nan;
        i += 1;
        if (i >= n) goto ret;
    }

    /* Look at values, update max or min, skip or exit for NaN values. */

    s = x[i];
    i += 1;

    if (max) {
        while (i < n) {
            if (x[i] > s)        /* never true if x[i] is a NaN */
                s = x[i];
            else if (x[i] <= s)  /* if so, not a NaN */
                ;
            else { /* a NaN value */
                if (!narm) goto nan;
            }
            i += 1;
        }
    }
    else { /* min */
        while (i < n) {
            if (x[i] < s)        /* never true if x[i] is a NaN */
                s = x[i];
            else if (x[i] >= s)  /* if so, not a NaN */
                ;
            else { /* a NaN value */
                if (!narm) goto nan;
            }
            i += 1;
        }
    }

    updated = TRUE;
    goto ret;

  nan:

    updated = TRUE;
    s = x[i];

    /* Make NA trump other NaN values. */

    while (i < n) {
        if (ISNA(x[i])) {
            s = x[i];
            goto ret;
        }
        i += 1;
    }

  ret:

    *value = s;
    return updated;
}

static Rboolean smax(SEXP x, SEXP *value, Rboolean narm)
{
    int i;
    SEXP s = NA_STRING; /* -Wall */
    Rboolean updated = FALSE;
    int len = length(x);

    for (i = 0; i < len; i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) < 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return(TRUE);
	}
    }
    *value = s;

    return(updated);
}

static double iprod(int *x, int n, Rboolean narm)
{
    double s = 1.0;
    int i;

    if (narm) {
        for (i = 0; i < n; i++) 
	    if (x[i] != NA_INTEGER) s *= x[i];
    } else { 
        for (i = 0; i < n; i++) {
            if (x[i] == NA_INTEGER) 
                return NA_REAL;
            s *= x[i];
        }
    }

    return s;
}

static double rprod(double *x, int n, Rboolean narm)
{
    long double s = 1.0;
    int i;

    if (narm) {
        for (i = 0; i < n; i++) 
	    if (!ISNAN(x[i])) s *= x[i];
    } else { 
        for (i = 0; i < n; i++)
            s *= x[i];
    }

    return s;
}

static Rcomplex cprod(Rcomplex *x, int n, Rboolean narm)
{
    Rcomplex s, t;
    int i, first;

    s.r = 1;
    s.i = 0;

    first = 1;
    for (i = 0; i < n; i++) {
	if (!narm || (!ISNAN(x[i].r) && !ISNAN(x[i].i))) {
            if (first) {
                s = x[i];  /* multiplying x[i] by 1+0i may not be the same */
                first = 0;
            }
            else {
                t = s;
                R_from_C99_complex (&s, C99_from_R_complex(&t)
                                         * C99_from_R_complex(&x[i]));
            }
	}
    }

    return s;
}

static SEXP do_mean (SEXP call, SEXP op, SEXP args, SEXP env)
{
    long double s, si, t, ti;
    int_fast64_t smi;
    SEXP x, x_grad, ans, grad;
    int n, i;

    x = CAR(args);
    x_grad = HAS_GRADIENT_IN_CELL(args) ? GRADIENT_IN_CELL(args) : R_NilValue;
    grad = R_NilValue;

    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
        n = LENGTH(x);
        smi = 0;
        for (i = 0; i < n; i++) {
            if (INTEGER(x)[i] == NA_INTEGER)
                return ScalarReal(R_NaReal);
            smi += INTEGER(x)[i];
        }
        ans = ScalarReal ((double)smi / n);
        break;
    case REALSXP:
        n = LENGTH(x);
        s = 0;
        for (i = 0; i < n; i++) 
            s += REAL(x)[i];
        s /= n;
        if (R_FINITE((double)s)) {
            t = 0;
            for (i = 0; i < n; i++) 
                t += REAL(x)[i]-s;
            s += t/n;
        }
        ans = ScalarReal(s);
        if (x_grad != R_NilValue && R_FINITE(s))
            grad = mean_gradient (x_grad, n);
        break;
    case CPLXSXP:
        n = LENGTH(x);
        s = si = 0;
        for (i = 0; i < n; i++) {
            s += COMPLEX(x)[i].r;
            si += COMPLEX(x)[i].i;
        }
        s /= n; si /= n;
        if (R_FINITE((double)s) && R_FINITE((double)si)) {
            t = ti = 0;
            for (i = 0; i < n; i++) {
                t += COMPLEX(x)[i].r-s;
                ti += COMPLEX(x)[i].i-si;
            }
            s += t/n; si += ti/n;
        }
        ans = allocVector (CPLXSXP, 1);
        COMPLEX(ans)[0].r = s;
        COMPLEX(ans)[0].i = si;
        break;
    default:
        error(R_MSG_type, type2char(TYPEOF(x)));
    }

    if (grad != R_NilValue) {
        R_gradient = grad;
        R_variant_result = VARIANT_GRADIENT_FLAG;
    }

    return ans;
}

static int has_na_rm_arg (SEXP args)
{
    while (args != R_NilValue) {
        if (TAG(args) == R_NaRmSymbol)
            return 1;
        args = CDR(args);
    }

    return 0;
}

/* do_summary provides a variety of data summaries
	op :  0 = sum,  2 = min,  3 = max,  4 = prod

   NOTE: mean used to be done here, but is now separate, since it has
   nothing in common with the others (has only one arg and no na.rm, and
   dispatch is from an R-level generic). */

static SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    int iop = PRIMVAL(op);
    int vrt = iop == 0 /* sum */ && !has_na_rm_arg(args)
                ? VARIANT_ANY_ATTR | VARIANT_SUM 
                : VARIANT_ANY_ATTR;

    PROTECT (args = iop == 0 /* sum */ && (variant & VARIANT_GRADIENT)
                     ? evalList_gradient (args, env, vrt, INT_MAX, 0)
                     : evalList_v (args, env, vrt));

    SEXP ans, a, stmp = NA_STRING /* -Wall */, scum = NA_STRING, call2;
    double tmp = 0.0, s;
    Rcomplex z, ztmp, zcum={0.0, 0.0} /* -Wall */;
    int itmp = 0, icum=0, int_a, real_a, empty, warn = 0 /* dummy */;
    int first;
    SEXPTYPE ans_type;/* only INTEGER, REAL, COMPLEX or STRSXP here */

    Rboolean narm;
    int updated;
	/* updated := 1 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned */

    /* match to foo(..., na.rm=FALSE) */
    args = fixup_NaRm(args);
    UNPROTECT_PROTECT(args);
    PROTECT(call2 = LCONS(CAR(call),args));

    if (DispatchGroup("Summary", call2, op, args, env, &ans, 0)) {
	UNPROTECT(2);
	return(ans);
    }
    UNPROTECT(1);

#ifdef DEBUG_Summary
    REprintf("C do_summary(op%s, *): did NOT dispatch\n", PRIMNAME(op));
#endif

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);

    switch(iop) {
    case 0:/* sum */
        /* we need to find out if _all_ the arguments are integer or logical
           in advance, as we might overflow before we find out.  NULL is
           documented to be the same as integer(0).
        */
	ans_type = INTSXP;
	for (SEXP a = args; !isNull(a); a = CDR(a)) {
	    if(!isInteger(CAR(a)) && !isLogical(CAR(a)) && !isNull(CAR(a))) {
		ans_type = REALSXP;  /* may change to CPLXSXP later */
		break;
	    }
	}
        icum = 0;
	zcum.r = zcum.i = 0.;
	break;

    case 2:/* min */
	DbgP2("do_summary: min(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_PosInf;
	icum = INT_MAX;
	break;

    case 3:/* max */
	DbgP2("do_summary: max(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_NegInf;;
	icum = R_INT_MIN;
	break;

    case 4:/* prod */
	ans_type = REALSXP;
	zcum.r = 1.;
	zcum.i = 0.;
        first = 1;
	break;

    default:
	errorcall(call,
		  _("internal error ('op = %d' in do_summary).\t Call a Guru"),
		  iop);
    }

    /*-- now loop over all arguments.  Do the 'op' switch INSIDE : */

    SEXP grad;
    PROTECT(grad = R_NilValue);

    updated = 0;
    empty = 1; /* for min/max, 1 if only 0-length arguments, or NA with na.rm=T */
    while (args != R_NilValue) {
	a = CAR(args);

	if(length(a) > 0) {

	    switch(iop) {
	    case 2:/* min */
	    case 3:/* max */

	        updated = 0;
	        int_a = 0;/* int_a = 1	<-->	a is INTEGER */
	        real_a = 0;

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    int_a = 1;
                    updated = imin_max (INTEGER(a), LENGTH(a), 
                                        &itmp, narm, iop==3);
		    break;
		case REALSXP:
		    real_a = 1;
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
                    updated = rmin_max (REAL(a), LENGTH(a), &tmp, narm, iop==3);
		    break;
		case STRSXP:
		    if(!empty && ans_type == INTSXP)
			scum = StringFromInteger(icum, &warn);
		    else if(!empty && ans_type == REALSXP)
			scum = StringFromReal(zcum.r, &warn);
		    ans_type = STRSXP;
		    if (iop == 2) updated = smin(a, &stmp, narm);
		    else updated = smax(a, &stmp, narm);
		    break;
		default:
		    goto invalid_type;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (itmp == NA_INTEGER) goto na_answer;
			if ((iop == 2 && itmp < icum) || /* min */
			    (iop == 3 && itmp > icum))   /* max */
			    icum = itmp;
		    } else if(ans_type == REALSXP) {
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNA(zcum.r)); /* NA trumps anything */
			else if (ISNAN(tmp)) {
			    if (ISNA(tmp)) zcum.r = tmp;
			    else zcum.r += tmp;/* NA or NaN */
			} else if(
			    (iop == 2 && tmp < zcum.r) ||
			    (iop == 3 && tmp > zcum.r))	zcum.r = tmp;
		    } else if(ans_type == STRSXP) {
			if(empty) scum = stmp;
			else {
			    if(int_a)
				stmp = StringFromInteger(itmp, &warn);
			    if(real_a)
				stmp = StringFromReal(tmp, &warn);
			    if(((iop == 2 && stmp != scum && Scollate(stmp, scum) < 0)) ||
			       (iop == 3 && stmp != scum && Scollate(stmp, scum) > 0) )
				scum = stmp;
			}
		    }
		}/*updated*/ else {
		    /*-- in what cases does this happen here at all?
		      -- if there are no non-missing elements.
		     */
		    DbgP2(" NOT updated [!! RARE !!]: int_a=%d\n", int_a);
		}

		break;/*--- end of  min() / max() ---*/

	    case 0:/* sum */

                WAIT_UNTIL_COMPUTED(a);

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    itmp = isum (TYPEOF(a)==LGLSXP ? LOGICAL(a) : INTEGER(a),
                                 LENGTH(a), narm, call);
		    if (itmp == NA_INTEGER) goto na_answer;
		    if (ans_type == INTSXP) {
		        s = (double) icum + (double) itmp;
		        if (s > INT_MAX || s < R_INT_MIN) {
		            warningcall (call,
                              _("Integer overflow - use sum(as.numeric(.))"));
			    goto na_answer;
			}
                        icum += itmp;
		    } 
                    else
		        zcum.r += Int2Real(itmp);
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) { /* shouldn't happen */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    zcum.r += rsum(REAL(a), LENGTH(a), narm);
                    if (ans_type == REALSXP && HAS_GRADIENT_IN_CELL(args)) {
                        SEXP v = GRADIENT_IN_CELL(args);
                        grad = sum_gradient (grad, v, a, narm, LENGTH(a));
                        UNPROTECT_PROTECT(grad);
                    }
		    break;
		case CPLXSXP:
		    if(ans_type == INTSXP) { /* shouldn't happen */
			ans_type = CPLXSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    } else if (ans_type == REALSXP)
			ans_type = CPLXSXP;
		    ztmp = csum(COMPLEX(a), LENGTH(a), narm);
		    zcum.r += ztmp.r;
		    zcum.i += ztmp.i;
		    break;
		default:
		    goto invalid_type;
		}

		break;/* sum() part */

	    case 4:/* prod */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		    if(TYPEOF(a) == REALSXP)
			tmp = rprod(REAL(a), LENGTH(a), narm);
		    else
			tmp = iprod(INTEGER(a), LENGTH(a), narm);
		    zcum.r *= tmp;
		    zcum.i *= tmp;
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    ztmp = cprod(COMPLEX(a), LENGTH(a), narm);
                    if (first) 
                        zcum = ztmp;
                    else {
                        z = zcum;
                        R_from_C99_complex(&zcum, C99_from_R_complex(&z)
                                                   * C99_from_R_complex(&ztmp));
                    }
		    break;
		default:
		    goto invalid_type;
		}

                first = 0;
		break;/* prod() part */

	    }/* switch(iop) */

	} else { /* len(a)=0 */
	    /* Even though this has length zero it can still be invalid,
	       e.g. list() or raw() */
	    switch(TYPEOF(a)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case NILSXP:  /* OK historically, e.g. PR#1283 */
		break;
	    case CPLXSXP:
		if (iop == 2 || iop == 3) goto invalid_type;
		break;
	    case STRSXP:
		if (iop == 2 || iop == 3) {
		    if(!empty && ans_type == INTSXP)
			scum = StringFromInteger(icum, &warn);
		    else if(!empty && ans_type == REALSXP)
			scum = StringFromReal(zcum.r, &warn);
		    ans_type = STRSXP;
		    break;
		}
	    default:
		goto invalid_type;
	    }
	    if(ans_type < TYPEOF(a) && ans_type != CPLXSXP) {
		if(!empty && ans_type == INTSXP)
		    zcum.r = Int2Real(icum);
		ans_type = TYPEOF(a);
	    }
	}
	DbgP3(" .. upd.=%d, empty: old=%d", updated, empty);
	if(empty && updated) empty=0;
	DbgP2(", new=%d\n", empty);
	args = CDR(args);
    } /*-- while(..) loop over args */

    /*-------------------------------------------------------*/
    if(empty && (iop == 2 || iop == 3)) {
	if(ans_type == STRSXP) {
	    warningcall(call, _("no non-missing arguments, returning NA"));
	} else {
	    if(iop == 2)
		warningcall(call, _("no non-missing arguments to min; returning Inf"));
	    else
		warningcall(call, _("no non-missing arguments to max; returning -Inf"));
	    ans_type = REALSXP;
	}
    }

    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:   INTEGER(ans)[0] = icum; break;
    case REALSXP:  REAL(ans)[0] = zcum.r; break;
    case CPLXSXP:  COMPLEX(ans)[0].r = zcum.r; 
                   COMPLEX(ans)[0].i = zcum.i; 
                   break;
    case STRSXP:   SET_STRING_ELT(ans, 0, scum); break;
    }

    if (grad != R_NilValue) {
        R_gradient = grad;
        R_variant_result = VARIANT_GRADIENT_FLAG;
    }

    UNPROTECT(2);  /* args, grad */
    return ans;

na_answer: /* only INTSXP case currently used */
    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:   INTEGER(ans)[0] = NA_INTEGER; break;
    case REALSXP:  REAL(ans)[0] = NA_REAL; break;
    case CPLXSXP:  COMPLEX(ans)[0].r = COMPLEX(ans)[0].i = NA_REAL; break;
    case STRSXP:   SET_STRING_ELT_NA(ans, 0); break;
    }
    UNPROTECT(2);  /* args, grad */
    return ans;

invalid_type:
    errorcall(call, R_MSG_type, type2char(TYPEOF(a)));
    return R_NilValue;
}/* do_summary */


static SEXP do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, prargs, call2;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = LCONS(CAR(call),args));

    if (DispatchGroup("Summary", call2, op, args, env, &ans, 0)) {
	UNPROTECT(2);
	return(ans);
    }
    UNPROTECT(1);

    PROTECT(op = findFun(install("range.default"), env));
    /* Below should really use CDR(call) for the unevaluated expressions, 
       but it can't because args has been fiddled with by fixup_NaRm. */
    PROTECT(prargs = promiseArgsWithValues(args, R_EmptyEnv, args, 0));
    ans = applyClosure(call, op, prargs, env, NULL);
    UNPROTECT(3);
    return(ans);
}

/* which.min(x) : The index (starting at 1), of the first min(x) in x
   which.max(x) : The index (starting at 1), of the first max(x) in x

   Uses some changes from R-3.2.3, but with substantial pqR improvements. */

SEXP attribute_hidden do_first_min(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sx = CAR(args), ans;
    int nprot = 1;
    R_xlen_t i, n, indx = -1;

    checkArity(op, args);
    if (!isNumeric(sx)) {
        PROTECT(sx = coerceVector(CAR(args), REALSXP)); nprot++;
    }

    n = XLENGTH(sx);

    switch (TYPEOF(sx)) {

    case LGLSXP: 
    {
        /* We can stop once we've found FALSE for min or TRUE for max. */

        int *r = LOGICAL(sx);
        if (PRIMVAL(op) == 0) { /* which.min */
            for (i = 0; i < n && r[i] == NA_LOGICAL; i++) ;
            if (i == n) /* all NA */
                break;
            indx = i;
            for ( ; i < n && r[i] != FALSE; i++) ;
            if (i == n) /* all NA or TRUE */
                break;
            indx = i;
        }
        else { /* which.max */
            for (i = 0; i < n && r[i] == NA_LOGICAL; i++) ;
            if (i == n) /* all NA */
                break;
            indx = i;
            for ( ; i < n && r[i] != TRUE; i++) ;
            if (i == n) /* all NA or FALSE */
                break;
            indx = i;
        }
        break;
    }

    case INTSXP:
    {
        int s, *r = INTEGER(sx);
        if (PRIMVAL(op) == 0) { /* which.min */
            for (i = 0; i < n && r[i] == NA_INTEGER; i++) ;
            if (i == n) /* all NA */
                break;
            indx = i;
            s = r[i];
            for ( ; i < n; i++) {
                if (r[i] < s && r[i] != NA_INTEGER) {
                    indx = i;
                    s = r[i];
                }
            }
        }
        else { /* which.max */
            for (i = 0; i < n && r[i] == NA_INTEGER; i++) ;
            if (i == n) /* all NA */
                break;
            indx = i;
            s = r[i];
            for ( ; i < n; i++) {
                if (r[i] > s) { /* no need to check NA_INTEGER, since never > */
                    indx = i;
                    s = r[i];
                }
            }
        }
        break;
    }

    case REALSXP:
    {
        double s, *r = REAL(sx);
        if (PRIMVAL(op) == 0) { /* which.min */
            for (i = 0; i < n && ISNAN(r[i]); i++) ;
            if (i == n) /* all NA/NaN */
                break;
            indx = i;
            s = r[i];
            for ( ; i < n; i++) {
                if (r[i] < s && !ISNAN(r[i])) { /* !ISNAN unneeded in theory */
                    indx = i;
                    s = r[i]; 
                }
            }
        }
        else { /* which.max */
            for (i = 0; i < n && ISNAN(r[i]); i++) ;
            if (i == n) /* all NA/NaN */
                break;
            indx = i;
            s = r[i];
            for ( ; i < n; i++) {
                if (r[i] > s && !ISNAN(r[i])) { /* !ISNAN unneeded in theory */
                    indx = i;
                    s = r[i]; 
                }
            }
        }
        break;
    }}

    if (n > INT_MAX)  /* never happens when there are no large vectors */
        ans = indx == -1 ? allocVector(REALSXP,0) : ScalarReal((double)indx+1);
    else
        ans = indx == -1 ? allocVector(INTSXP,0) : ScalarInteger(indx+1);
    PROTECT(ans);

    if (indx != -1) {
        SEXP names = getNamesAttrib(sx);
        if (names != R_NilValue) {
            SEXP ansnam;
            PROTECT(ansnam = ScalarString(STRING_ELT(names,indx)));
            setAttrib(ans, R_NamesSymbol, ansnam);
            UNPROTECT(1);
        }
    }

    UNPROTECT(nprot);
    return ans;
}

/* which(x) : indices of TRUE values in x (ignores NA) */

static SEXP do_which(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, s_nms, x, x_nms = R_NilValue;
    int *xi;
    R_len_t len, i, j;

    checkArity(op, args);
    s = CAR(args);
    if (!isLogical(s))
        error(_("argument to 'which' is not logical"));

    R_len_t ns = LENGTH(s);
    int *si = LOGICAL(s);

    if (SIZEOF_CHAR_P <= 4) {  /* small address space */

        /* TWO-PASS IMPLEMENTATION.  Avoids allocating more memory than
           necessary, hence preferred for systems with limited address space. */

        /* Count the number of TRUE values in s.  Adds together all the
           values in s in a 32-bit unsigned accumulator, then clears the
           top bit of this sum to get the desired count. */
    
        unsigned *su = (unsigned *)si;
        uint32_t ucount;
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
        len = (int)(ucount & 0x7fffffff);
    
        /* Create index vector, x, with index values. */
    
        x = allocVector (INTSXP, len);
        xi = INTEGER(x);

        i = 0;
        j = 0;
        if (ns & 1) {
            if (si[i++] > 0) xi[j++] = i;
        }
        if (ns & 2) {
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
        }
        while (i < ns) {
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
        }
    
        if (j != len) abort();
    }

    else {  /* large address space */

        /* ONE-PASS IMPLEMENTATION.  May allocate much more memory than
           necessary, but unused portions are never accessed, and on many
           systems will not be allocated physical memory.  But the allocation
           does occupy address space, hence this is more suitable when
           there's plenty of address space. */

        /* Initially try to store indices in a local array, xi0, of length
           LEN0.  When that's full, or when the end of s is reached, copy
           contents to an allocated INTSXP vector, to which more indices
           may be added.  Reduce the length of this vector once done to
           give the final result. */

#       define LEN0 300  /* Must be at least 7 */

        R_len_t len0;
        int xi0[LEN0];
        len = ns;
        len0 = len < LEN0 ? len : LEN0;
        xi = xi0;

        i = 0;
        j = 0;

        /* Use unrolled loops. */

        if (len & 1) {
            if (si[i++] > 0) xi[j++] = i;
        }
        if (len & 2) {
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
        }
        if (len & 4) {
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
        }

        if (len0 == LEN0) {
            while (i < len && j < LEN0-7) {
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
                if (si[i++] > 0) xi[j++] = i;
            }
        }

        x = allocVector (INTSXP, j + (len-i));
        xi = INTEGER(x);
        memcpy (xi, xi0, j * sizeof(int));

        while (i < len) {
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
            if (si[i++] > 0) xi[j++] = i;
        }

        len = j;
        if (LENGTH(x) != len)
            x = reallocVector(x,len,1);
    }

    PROTECT(x);
    if ((s_nms = getNamesAttrib(s)) != R_NilValue) {
        PROTECT(x_nms = allocVector(STRSXP, len));
        for (i = 0; i < len; i++) {
            SET_STRING_ELT (x_nms, i, STRING_ELT(s_nms,xi[i]-1));
        }
        setAttrib(x, R_NamesSymbol, x_nms);
        UNPROTECT(1);
    }
    UNPROTECT(1);

    return x;
}

/* complete.cases(.) */
static SEXP do_compcases(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, t, u, rval;
    int i, len;

    /* checkArity(op, args); */
    len = -1;

    for (s = args; s != R_NilValue; s = CDR(s)) {
	if (isList(CAR(s))) {
	    for (t = CAR(s); t != R_NilValue; t = CDR(t))
		if (isMatrix(CAR(t))) {
		    u = getDimAttrib(CAR(t));
		    if (len < 0)
			len = INTEGER(u)[0];
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
		else if (isVector(CAR(t))) {
		    if (len < 0)
			len = LENGTH(CAR(t));
		    else if (len != LENGTH(CAR(t)))
			goto bad;
		}
		else
		    error(R_MSG_type, type2char(TYPEOF(CAR(t))));
	}
	/* FIXME : Need to be careful with the use of isVector() */
	/* since this includes lists and expressions. */
	else if (isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = length(t);
	    /* 0-column data frames are a special case */
	    if(nt) {
		for (it = 0 ; it < nt ; it++) {
		    if (isMatrix(VECTOR_ELT(t, it))) {
			u = getDimAttrib(VECTOR_ELT(t, it));
			if (len < 0)
			    len = INTEGER(u)[0];
			else if (len != INTEGER(u)[0])
			    goto bad;
		    }
		    else if (isVector(VECTOR_ELT(t, it))) {
			if (len < 0)
			    len = LENGTH(VECTOR_ELT(t, it));
			else if (len != LENGTH(VECTOR_ELT(t, it)))
			    goto bad;
		    }
		    else
			error(R_MSG_type, "unknown");
		}
	    } else {
		u = getAttrib(t, R_RowNamesSymbol);
		if (!isNull(u)) {
		    if (len < 0)
			len = LENGTH(u);
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
	    }
	}
	else if (isMatrix(CAR(s))) {
	    u = getDimAttrib(CAR(s));
	    if (len < 0)
		len = INTEGER(u)[0];
	    else if (len != INTEGER(u)[0])
		goto bad;
	}
	else if (isVector(CAR(s))) {
	    if (len < 0)
		len = LENGTH(CAR(s));
	    else if (len != LENGTH(CAR(s)))
		goto bad;
	}
	else
	    error(R_MSG_type, type2char(TYPEOF(CAR(s))));
    }

    if (len < 0)
	error(_("no input has determined the number of cases"));
    PROTECT(rval = allocVector(LGLSXP, len));
    for (i = 0; i < len; i++) INTEGER(rval)[i] = 1;
    /* FIXME : there is a lot of shared code here for vectors. */
    /* It should be abstracted out and optimized. */
    for (s = args; s != R_NilValue; s = CDR(s)) {
	if (isList(CAR(s))) {
	    /* Now we only need to worry about vectors */
	    /* since we use mod to handle arrays. */
	    /* FIXME : using mod like this causes */
	    /* a potential performance hit. */
	    for (t = CAR(s); t != R_NilValue; t = CDR(t)) {
		u = CAR(t);
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING_ELT(u, i) == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			error(R_MSG_type, type2char(TYPEOF(u)));
		    }
		}
	    }
	}
	if (isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = length(t);
	    for (it = 0 ; it < nt ; it++) {
		u = VECTOR_ELT(t, it);
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING_ELT(u, i) == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			error(R_MSG_type, type2char(TYPEOF(u)));
		    }
		}
	    }
	}
	else {
	    for (i = 0; i < LENGTH(CAR(s)); i++) {
		u = CAR(s);
		switch (TYPEOF(u)) {
		case INTSXP:
		case LGLSXP:
		    if (INTEGER(u)[i] == NA_INTEGER)
			INTEGER(rval)[i % len] = 0;
		    break;
		case REALSXP:
		    if (ISNAN(REAL(u)[i]))
			INTEGER(rval)[i % len] = 0;
		    break;
		case CPLXSXP:
		    if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			INTEGER(rval)[i % len] = 0;
		    break;
		case STRSXP:
		    if (STRING_ELT(u, i) == NA_STRING)
			INTEGER(rval)[i % len] = 0;
		    break;
		default:
		    UNPROTECT(1);
		    error(R_MSG_type, type2char(TYPEOF(u)));
		}
	    }
	}
    }
    UNPROTECT(1);
    return rval;

 bad:
    error(_("not all arguments have the same length"));
}

/* op = 0 is pmin, op = 1 is pmax.  
   Internal, and SPECIAL so can handle gradients.
   Passed na.rm argument followed by arguments to do min/max for.
   NULL and logicals are handled as if they had been coerced to integer. */

static SEXP do_pmin(SEXP call, SEXP op, SEXP args, SEXP rho, int variant)
{
    PROTECT (args = variant & VARIANT_GRADIENT 
                      ? evalList_gradient (args, rho, 0, INT_MAX, 1)
                      : evalList (args, rho));

    int max = PRIMVAL(op) == 1;

    SEXP a, x, ans, grad;
    int i, j, n, len, narm;
    SEXPTYPE type, anstype;

    narm = asLogical(CAR(args));
    if(narm == NA_LOGICAL)
        error(_("invalid '%s' value"), "na.rm");
    args = CDR(args);
    x = CAR(args);
    if(args == R_NilValue) error(_("no arguments"));

    anstype = TYPEOF(x);
    switch(anstype) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
        break;
    default:
        error(_("invalid input type"));
    }

    a = CDR(args);
    if (a == R_NilValue) {
        /* one input */
        if (HAS_GRADIENT_IN_CELL(args)) {
            R_gradient = GRADIENT_IN_CELL(args);
            R_variant_result = VARIANT_GRADIENT_FLAG;
        }
        UNPROTECT(1);
        return x;
    }

    len = length(x); /* not LENGTH, as NULL is allowed */
    for(; a != R_NilValue; a = CDR(a)) {
        x = CAR(a);
        type = TYPEOF(x);
        switch(type) {
        case NILSXP:
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case STRSXP:
            break;
        default:
            error(_("invalid input type"));
        }
        if (type > anstype) anstype = type;  /* RELIES ON SEXPTYPE ORDERING! */
        n = length(x);
        if ((len > 0) ^ (n > 0)) {
            // till 2.15.0:  error(_("cannot mix 0-length vectors with others"))
            len = 0;
            break;
        }
        len = imax2(len, n);
    }

    if (anstype < INTSXP) anstype = INTSXP;  /* RELIES ON SEXPTYPE ORDERING! */

    if (len == 0) {
        UNPROTECT(1);
        return allocVector(anstype, 0);
    }

    /* Check for fractional recycling (added in 2.14.0) */
    for(a = args; a != R_NilValue; a = CDR(a)) {
        n = LENGTH(CAR(a));
        if (len % n != 0) {
            warning(_("an argument will be fractionally recycled"));
            break;
        }
    }

    PROTECT(ans = allocVector(anstype, len));
    PROTECT(grad = R_NilValue);

    x = CAR(args);
    if (TYPEOF(x) != anstype) {
        x = coerceVector(CAR(args), anstype);
        SETCAR(args,x);
    }
    copy_elements_recycled (ans, 0, x, len);

    for (a = CDR(args); a != R_NilValue; a = CDR(a)) {

        x = coerceVector (CAR(a), anstype);
        n = LENGTH(x);
        SETCAR(a,x);

        switch(anstype) {
        case INTSXP: {
            int *r,  *ra = INTEGER(ans), tmp;
            r = INTEGER(x);
            for (i = 0, j = 0; i < len; i++, j++) {
                if (j >= n) j = 0;
                tmp = r[j];
                if (tmp == NA_INTEGER) {
                    if (!narm) ra[i] = NA_INTEGER;
                }
                else if (ra[i] == NA_INTEGER) {
                    if (narm) ra[i] = tmp;
                }
                else if (max ? tmp > ra[i] : tmp < ra[i])
                    ra[i] = tmp;
            }
            break;
        }
        case REALSXP: {
            double *r, *ra = REAL(ans), tmp;
            r = REAL(x);
            for (i = 0, j = 0; i < len; i++, j++) {
                if (j >= n) j = 0;
                tmp = r[j];
                if (MAY_BE_NAN2(ra[i],tmp)) {
                    if (ISNAN(ra[i])) {
                        if (ISNAN(tmp)) {
                            if (!ISNA(ra[i])) ra[i] = tmp;
                        }
                        else {
                            if (narm) ra[i] = tmp;
                        }
                        continue;
                    }
                    if (ISNAN(tmp)) {
                        if (!narm) ra[i] = tmp;
                        continue;
                    }
                }
                else if (max ? tmp > ra[i] : tmp < ra[i])
                    ra[i] = tmp;
            }
            break;
        }
        case STRSXP: {
            SEXP tmp, rai, new;
            for (i = 0, j = 0; i < len; i++, j++) {
                if (j >= n) j = 0;
                tmp = STRING_ELT (x, j);
                rai = STRING_ELT (ans, i);
                new = rai;
                if (tmp == NA_STRING) {
                    if (!narm) new = NA_STRING;
                }
                else if (rai == NA_STRING) {
                    if (narm) new = tmp;
                }
                else if (max ? Scollate(tmp,rai) > 0 : Scollate(tmp,rai) < 0)
                    new = tmp;
                if (new != rai)
                    SET_STRING_ELT (ans, i, new);
            }
            break;
        }
        default:
            break;
        }
    }

    if ((variant & VARIANT_GRADIENT) && anstype == REALSXP) {
        SEXP dup_ans = duplicate(ans);  /* so can be modified below */
        for (a = args; a != R_NilValue; a = CDR(a)) {
            SEXP v = CAR(a);
            if (HAS_GRADIENT_IN_CELL(a)) {
                grad = minmax_gradient 
                         (grad, GRADIENT_IN_CELL(a), dup_ans, v, len);
                UNPROTECT_PROTECT(grad);
            }
            n = LENGTH(v);
            j = 0;
            for (i = 0; i < len; i++) {
                if (REAL(dup_ans)[i] == REAL(v)[j])
                    REAL(dup_ans)[i] = NA_REAL;  /* so won't match again */
                j += 1;
                if (j >= n) j = 0;
            }
        }
    }

    if (grad != R_NilValue) {
        R_gradient = grad;
        R_variant_result = VARIANT_GRADIENT_FLAG;
    }

    UNPROTECT(3);
    return ans;
}

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_summary[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"mean",	do_mean,	0,	10000011,1,	{PP_FUNCALL, PREC_FN,	0}},
{"range",	do_range,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.min",	do_first_min,	0,   1000011,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min,	1,   1000011,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	do_which,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"complete.cases",do_compcases,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin,	0,	1010,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	1,	1010,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* these four are group generic and so need to eval args */
{"sum",		do_summary,	0,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"min",		do_summary,	2,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"max",		do_summary,	3,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prod",	do_summary,	4,	1000,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
