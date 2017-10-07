/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014, 2015, 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  The R Core Team
 *  Copyright (C) 2002--2005  The R Foundation
 *
 *  The changes in pqR from R-2.15.0 distributed by the R Core Team are
 *  documented in the NEWS and MODS files in the top-level source directory.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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

/* Code to handle list / vector switch */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include <Defn.h>

#include <helpers/helpers-app.h>

#define imax2(x, y) ((x < y) ? y : x)

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};


/* Task procedure for copying a vector into another vector with possible
   coercion.  The code gives the offset within the output vector to start
   copying to (in top 32 bits), and also indicates whether an initial 
   portion (low bit 0) or a final portion (low bit 1) of the input should 
   be copied, with remaining bits in low 32 giving the number of elements
   copied.  

   In-out pipelining for the output is done, but only a large-scale level
   suitable for combining several calls of this task procedure for parallel
   computation of the result.

   The coercion required must not involve allocation (not to string, no 
   warning possible. */

void task_copy_coerced (helpers_op_t code, SEXP out, SEXP in, SEXP in2)
{
    int pos = code >> 32;
    int count = (code >> 1) & 0x7fffffff;
    int start = code & 1 ? LENGTH(in)-count : 0;

    while (helpers_avail0(LENGTH(out)) < pos+count) ;
    helpers_amount_out (pos);

    copy_elements_coerced (out, pos, 1, in, start, 1, count);

    helpers_amount_out (pos+count);
    while (helpers_avail0(LENGTH(out)) < LENGTH(out)) ;
}

/* Check for case of only atomic vectors in c and unlist.  Return result, or
   R_NoObject if not so simple.  Arguments are a pointer to an array of
   pointers to objects to be concatenated, the number of objects, whether 
   names (if prsent) are to be used, and the variant (used to see if pending
   computation is OK). */

#define T_c THRESHOLD_ADJUST(80)

static SEXP simple_concatenate (SEXP *objs, R_len_t nobj, int usenames, 
                                int variant, SEXP call, SEXP env)
{
    SEXPTYPE typ = NILSXP;
    SEXP ans, ansnames;
    int add_names, realloc, largeone0, largeones;
    R_len_t i, len, len0, pos;

    /* Scan the objects to be concatenated, seeing whether this is a simple
       case, and if so, finding the type and length of the result. */

    add_names = 0;
    largeone0 = 0;
    largeones = 0;
    len = 0;
    for (i = 0; i < nobj; i++) {
        SEXP a = objs[i];
        if (a == R_NilValue)
            continue;
        if (!isVectorAtomic(a) || LENGTH(a) > INT_MAX - len) 
            return R_NoObject;
        if (usenames && HAS_ATTRIB(a) /* quick pretest */ 
                     && getNamesAttrib(a) != R_NilValue)
            add_names = 1;
        typ = Rf_higher_atomic_type (typ, TYPEOF(a));
        len += LENGTH(a);
        if (LENGTH(a) > T_c) {
            if (i == 0) largeone0 = 1;
            largeones += 1;
        }
    }

    if (typ == NILSXP)
        return R_NoObject;

    /* Allocate space for result.  May be a reallocation of the first
       object to be concatenated.  Also sets ansnames, to either R_NilValue
       or the names from the first object. */

    int local_assign1 = VARIANT_KIND(variant) == VARIANT_LOCAL_ASSIGN1 &&
                        TYPEOF(objs[0]) == typ && !NAMEDCNT_GT_1(objs[0]) &&
                        objs[0] == findVarInFrame3 (env, CADR(call), 7);

    if (TYPEOF(objs[0]) != typ 
         || LENGTH(objs[0]) <= len/2 /* guards against repeats: v <- c(v,v) */
         || NAMEDCNT_GT_0(objs[0]) && !local_assign1) {
        ans = allocVector (typ, len);
        ansnames = R_NilValue;
        local_assign1 = 0;
        realloc = 0;
    }
    else {
        if (add_names && (NAMEDCNT_EQ_0(objs[0]) || local_assign1)) {
            ansnames = getNamesAttrib(objs[0]);
            if (NAMEDCNT_GT_1(ansnames) /* Enough?  But go on for now... */
                 || NAMEDCNT_GT_0(ansnames) && !local_assign1)
                ansnames = R_NilValue;
        }
        len0 = LENGTH(objs[0]);
        ans = reallocVector (objs[0], len, 1);
        SET_ATTRIB (ans, R_NilValue);
        SETLEVELS (ans, 0);
        SET_TRUELENGTH (ans, 0);
        if (ans != objs[0]) {
            local_assign1 = 0;
            SET_NAMEDCNT_0(ans);
        }
        largeones -= largeone0;
        realloc = 1;
    }

    PROTECT(ans);

    /* Concatenate the contents of the vectors.  Done in two passes, 
       first copying vectors directly, then (perhaps) scheduling tasks 
       to copy other (big) ones. */

    int use_helpers = 
      largeones > 0 && typ != STRSXP
        && !helpers_not_multithreading_now 
        && (largeones > 1 || add_names || (variant & VARIANT_PENDING_OK));

    pos = 0;
    for (i = 0; i < nobj; i++) {
        SEXP a = objs[i];
        if (a == R_NilValue)
            continue;
        if (i == 0 && realloc)
            pos += len0;
        else {
            R_len_t ln = LENGTH(a);
            if (!use_helpers || ln <= T_c) {
                WAIT_UNTIL_COMPUTED(a);
                copy_elements_coerced (ans, pos, 1, a, 0, 1, ln);
            }
            pos += ln;
        }
    }

    if (use_helpers) {
        pos = len;
        for (i = nobj-1; i >= realloc; i--) {
            SEXP a = objs[i];
            if (a == R_NilValue)
                continue;
            R_len_t ln = LENGTH(a);
            pos -= ln;
            if (ln > T_c) {
                helpers_do_task (HELPERS_PIPE_IN0_OUT, task_copy_coerced,
                  ((helpers_op_t)pos << 32) + ((helpers_op_t)ln << 1) + 0,
                  ans, a, (helpers_var_ptr)0);
            }
        }
    }

    /* Concatenate and attach the names, if they are present and used. */

    if (add_names) {
        if (ansnames == R_NilValue) {
            ansnames = allocVector (STRSXP, len);
            realloc = 0;
        }
        else {
            ansnames = reallocVector (ansnames, len, 1);
            realloc = 1;
        }
        pos = 0;
        for (i = 0; i < nobj; i++) {
            SEXP a = objs[i];
            if (a == R_NilValue)
                continue;
            R_len_t ln = LENGTH(a);
            if (i == 0 && realloc)
                pos += len0;
            else {
                SEXP nms = getNamesAttrib(a);
                if (nms != R_NilValue) {
                    copy_elements (ansnames, pos, 1, nms, 0, 1, ln); }
                else
                    copy_elements (ansnames, pos, 1, 
                                   R_BlankScalarString, 0, 0, ln);
                pos += ln;
            }
        }
        setAttrib (ans, R_NamesSymbol, ansnames);
    }

    if (! (variant & VARIANT_PENDING_OK))
        WAIT_UNTIL_COMPUTED(ans);

    UNPROTECT(1);
    R_variant_result = local_assign1;
    return ans;
}


static SEXP cbind(SEXP, SEXP, SEXPTYPE, SEXP, int);
static SEXP rbind(SEXP, SEXP, SEXPTYPE, SEXP, int);

/* The following code establishes the return type for the */
/* functions  unlist, c, cbind, and rbind and also determines */
/* whether the returned object is to have a names attribute. */

struct BindData {
    int  ans_type;
    SEXP ans_ptr;
    int  ans_length;
    SEXP ans_names;
    int  ans_nnames;
};

static int HasNames(SEXP x)
{
    if (isVector(x))
        return getNamesAttrib(x) != R_NilValue;
    else if (isList(x)) {
	while (x != R_NilValue) {
	    if (TAG(x) != R_NilValue)
                return 1;
	    x = CDR(x);
	}
        return 0;
    }
}

static void
AnswerType(SEXP x, int recurse, int usenames, struct BindData *data, SEXP call)
{
    R_len_t len;

    if (isVectorAtomic(x)) {
        if (data->ans_type != VECSXP && data->ans_type != EXPRSXP)
            data->ans_type = Rf_higher_atomic_type (data->ans_type, TYPEOF(x));
        len = LENGTH(x);
    }
    else if (isVectorList(x)) {
	if (recurse) {
	    int i, n;
	    n = LENGTH(x);
            if (usenames && !data->ans_nnames 
                         && HAS_ATTRIB(x) /* quick pre-test */
                         && getNamesAttrib(x) != R_NilValue)
		data->ans_nnames = 1;
	    for (i = 0; i < n; i++) {
		if (usenames && !data->ans_nnames)
		    data->ans_nnames = HasNames(VECTOR_ELT(x, i));
		AnswerType(VECTOR_ELT(x, i), recurse, usenames, data, call);
	    }
            len = 0;  /* nothing extra after recursive calls */
	}
	else {
            if (data->ans_type != EXPRSXP)
                data->ans_type = VECSXP;
	    len = LENGTH(x);
	}
    }
    else if (TYPEOF(x) == LISTSXP) {
	if (recurse) {
	    while (x != R_NilValue) {
		if (usenames && !data->ans_nnames) {
		    if (!isNull(TAG(x))) data->ans_nnames = 1;
		    else data->ans_nnames = HasNames(CAR(x));
		}
		AnswerType(CAR(x), recurse, usenames, data, call);
		x = CDR(x);
	    }
            len = 0;  /* nothing extra after recursive calls */
	}
	else {
            if (data->ans_type != EXPRSXP)
                data->ans_type = VECSXP;
	    len = length(x);
	}
    }
    else if (x == R_NilValue) {
        len = 0;
    }
    else {
	data->ans_type = VECSXP;
	len = 1;
    }

    if (len > R_LEN_T_MAX - data->ans_length)
       errorcall(call,_("resulting vector exceeds vector length limit"));

    data->ans_length += len;
}


/* Add elements to a list result.  For LISTSXP, VECSXP, and EXPRSXP, the
   'recursive' argument controls how elements are copied:  0: just at top
   level, -1: one level down, 1: recursively to any depth. */

#define LIST_ASSIGN(x) do { \
  SET_VECTOR_ELT(data->ans_ptr, data->ans_length, x); \
  data->ans_length++; \
} while (0) /* apparently defined as this in case SET_VECTOR_ELT is a macro */

static void ListAnswer(SEXP x, int recursive, struct BindData *data)
{
    int i;

    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LGLSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarLogicalMaybeConst(LOGICAL(x)[i]));
	break;
    case RAWSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarRawMaybeConst(RAW(x)[i]));
	break;
    case INTSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarIntegerMaybeConst(INTEGER(x)[i]));
	break;
    case REALSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarRealMaybeConst(REAL(x)[i]));
	break;
    case CPLXSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarComplexMaybeConst(COMPLEX(x)[i]));
	break;
    case STRSXP:
	for (i = 0; i < LENGTH(x); i++)
	    LIST_ASSIGN(ScalarStringMaybeConst(STRING_ELT(x, i)));
	break;
    case VECSXP:
    case EXPRSXP:
	if (recursive != 0) {
	    for (i = 0; i < LENGTH(x); i++)
		ListAnswer (VECTOR_ELT(x, i), recursive == 1, data);
	}
	else {
	    for (i = 0; i < LENGTH(x); i++) {
                SET_VECTOR_ELEMENT_FROM_VECTOR (data->ans_ptr, data->ans_length,
                                                x, i); 
                data->ans_length += 1;
            }
	}
	break;
    case LISTSXP:
	if (recursive != 0) {
	    while (x != R_NilValue) {
		ListAnswer (CAR(x), recursive == 1, data);
		x = CDR(x);
	    }
	}
	else
	    while (x != R_NilValue) {
                SET_VECTOR_ELT (data->ans_ptr, data->ans_length, 
                                CAR(x));
                SET_NAMEDCNT_MAX (CAR(x));
                data->ans_length += 1;
		x = CDR(x);
	    }
	break;
    default:
	LIST_ASSIGN(duplicate(x));
	break;
    }
}


/* Add elements to an atomic vector result. */

static void AtomicAnswer(SEXP x, struct BindData *data)
{
    int i, n;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while (x != R_NilValue) {
	    AtomicAnswer(CAR(x), data);
	    x = CDR(x);
	}
	break;
    case EXPRSXP:
    case VECSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    AtomicAnswer(VECTOR_ELT(x, i), data);
	break;
    default:
        copy_elements_coerced (data->ans_ptr, data->ans_length, 1,
                               x, 0, 1, LENGTH(x));
        data->ans_length += LENGTH(x);
        break;
    }
}

static SEXP NewBase(SEXP base, SEXP tag)
{
    SEXP ans;
    char *cbuf;
    base = EnsureString(base);
    tag = EnsureString(tag);
    if (*CHAR(base) && *CHAR(tag)) { /* test of length */
	const void *vmax = VMAXGET();
	const char *sb = translateCharUTF8(base), *st = translateCharUTF8(tag);
        size_t alloc_len = strlen(st) + strlen(sb) + 1;
	cbuf = ALLOC_STRING_BUFF(alloc_len,&cbuff);
        (void) copy_3_strings (cbuf, alloc_len+1, sb, ".", st);
	/* This isn't strictly correct as we do not know that all the
	   components of the name were correctly translated. */
	ans = mkCharCE(cbuf, CE_UTF8);
        VMAXSET(vmax);
    }
    else if (*CHAR(tag)) {
	ans = tag;
    }
    else if (*CHAR(base)) {
	ans = base;
    }
    else ans = R_BlankString;
    return ans;
}

static SEXP NewName(SEXP base, SEXP tag, int seqno)
{
/* Construct a new Name/Tag, using
 *	base.tag
 *	base<seqno>	or
 *	tag
 *
 */

    SEXP ans;
    char *cbuf;
    const void *vmax = VMAXGET();
    base = EnsureString(base);
    tag = EnsureString(tag);
    if (*CHAR(base) && *CHAR(tag)) {
	const char *sb = translateCharUTF8(base), *st = translateCharUTF8(tag);
        size_t alloc_len = strlen(sb) + strlen(st) + 1;
	cbuf = ALLOC_STRING_BUFF(alloc_len,&cbuff);
        (void) copy_3_strings (cbuf, alloc_len+1, sb, ".", st);
	ans = mkCharCE(cbuf, CE_UTF8);
    }
    else if (*CHAR(base)) {
	const char *sb = translateChar(base);
        char sn[31];
        integer_to_string(sn,seqno);
        size_t alloc_len = strlen(sb) + strlen(sn);
	cbuf = ALLOC_STRING_BUFF(alloc_len,&cbuff);
        (void) copy_2_strings (cbuf, alloc_len+1, sb, sn);
	ans = mkCharCE(cbuf, CE_UTF8);
    }
    else if (*CHAR(tag)) {
	if(tag == NA_STRING) ans = NA_STRING;
	else {
	    const char *st = translateCharUTF8(tag);
            if (st == CHAR(tag))
                ans = tag;
            else {
                size_t alloc_len = strlen(st);
                cbuf = ALLOC_STRING_BUFF(alloc_len,&cbuff);
                strcpy(cbuf,st);
                ans = mkCharCE(cbuf, CE_UTF8);
            }
	}
    }
    else 
        ans = R_BlankString;
    VMAXSET(vmax);
    return ans;
}

/* also used in coerce.c */
SEXP attribute_hidden ItemName(SEXP names, int i)
{
  /* return  names[i]  if it is a character (>= 1 char), or NULL otherwise */
    if (names != R_NilValue &&
	STRING_ELT(names, i) != R_NilValue &&
	CHAR(STRING_ELT(names, i))[0] != '\0') /* length test */
	return STRING_ELT(names, i);
    else
	return R_NilValue;
}

/* NewExtractNames(v, base, tag, recurse):  For c() and	 unlist().
 * On entry, "base" is the naming component we have acquired by
 * recursing down from above.
 *	If we have a list and we are recursing, we append a new tag component
 * to the base tag (either by using the list tags, or their offsets),
 * and then we do the recursion.
 *	If we have a vector, we just create the tags for each element. */

struct NameData {
 int count;
 int seqno;
 int firstpos;
};


static void NewExtractNames(SEXP v, SEXP base, SEXP tag, int recurse,
			     struct BindData *data, struct NameData *nameData)
{
    SEXP names, namei;
    int i, n, savecount=0, saveseqno, savefirstpos=0;

    /* If we are beneath a new tag, we reset the index sequence and
       create the new basename string. */

    if (tag != R_NilValue) {
	PROTECT(base = NewBase(base, tag));
	savefirstpos = nameData->firstpos;
	saveseqno = nameData->seqno;
	savecount = nameData->count;
	nameData->count = 0;
	nameData->seqno = 0;
	nameData->firstpos = -1;
    }
    else saveseqno = 0;

    n = length(v);
    PROTECT(names = getNamesAttrib(v));

    switch(TYPEOF(v)) {
    case NILSXP:
	break;
    case LISTSXP:
	for (i = 0; i < n; i++) {
	    PROTECT(namei = ItemName(names, i));
	    if (recurse) {
		NewExtractNames(CAR(v), base, namei, recurse, data, nameData);
	    }
	    else {
		if (namei == R_NilValue && nameData->count == 0)
		    nameData->firstpos = data->ans_nnames;
		nameData->count++;
		namei = NewName(base, namei, ++(nameData->seqno));
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	    v = CDR(v);
	    UNPROTECT(1); /*namei*/
	}
	break;
    case VECSXP:
    case EXPRSXP:
	for (i = 0; i < n; i++) {
	    namei = ItemName(names, i);
	    if (recurse) {
		NewExtractNames(VECTOR_ELT(v, i), base, namei, recurse, data, nameData);
	    }
	    else {
		if (namei == R_NilValue && nameData->count == 0)
		    nameData->firstpos = data->ans_nnames;
		nameData->count++;
		namei = NewName(base, namei, ++(nameData->seqno));
		SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	    }
	}
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	for (i = 0; i < n; i++) {
	    namei = ItemName(names, i);
	    if (namei == R_NilValue && nameData->count == 0)
		nameData->firstpos = data->ans_nnames;
	    nameData->count++;
	    namei = NewName(base, namei, ++(nameData->seqno));
	    SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
	}
	break;
    default:
	if (nameData->count == 0)
	    nameData->firstpos = data->ans_nnames;
	nameData->count++;
	namei = NewName(base, R_NilValue, ++(nameData->seqno));
	SET_STRING_ELT(data->ans_names, (data->ans_nnames)++, namei);
    }
    if (tag != R_NilValue) {
	if (nameData->firstpos >= 0 && nameData->count == 1)
	    SET_STRING_ELT(data->ans_names, nameData->firstpos, base);
	nameData->firstpos = savefirstpos;
	nameData->count = savecount;
	UNPROTECT(1);
    }
    UNPROTECT(1); /*names*/
    nameData->seqno = nameData->seqno + saveseqno;
}

/* Code to process arguments to c().  Returns an argument list with the keyword
   arguments 'recursive' and 'use.names' removed, as well as any NULL args. 
   *recurse and *usenames are set to the keyword arg values, if they are
   present.  *anytags is set to whether any other args have tags.  *nargs is 
   set to the length of the argument list returned. */

static SEXP process_c_args (SEXP ans, SEXP call, int *recurse, int *usenames, 
                            int *anytags, R_len_t *nargs)
{
    SEXP a, n, last = R_NoObject, next = R_NoObject;
    int v, n_recurse = 0, n_usenames = 0;

    *anytags = 0;
    *nargs = 0;

    for (a = ans; a != R_NilValue; a = next) {
	n = TAG(a);
	next = CDR(a);
	if (n != R_NilValue && ep_match_exprs(R_RecursiveSymbol, n)==1) {
	    if (n_recurse++ == 1)
		errorcall(call, _("repeated formal argument 'recursive'"));
	    if ((v = asLogical(CAR(a))) != NA_INTEGER) {
		*recurse = v;
	    }
	    if (last == R_NoObject)
		ans = next;
	    else
		SETCDR(last, next);
	}
	else if (n != R_NilValue && ep_match_exprs(R_UseNamesSymbol, n)==1) {
	    if (n_usenames++ == 1)
		errorcall(call, _("repeated formal argument 'use.names'"));
	    if ((v = asLogical(CAR(a))) != NA_INTEGER) {
		*usenames = v;
	    }
	    if (last == R_NoObject)
		ans = next;
	    else
		SETCDR(last, next);
	}
        else if (CAR(a) == R_NilValue) {
	    if (last == R_NoObject)
		ans = next;
	    else
		SETCDR(last, next);
        }
	else {
            if (n != R_NilValue) 
                *anytags = 1;
            *nargs += 1;
            last = a;
        }
    }
    return ans;
}


/* The change to lists based on dotted pairs has meant that it was
   necessary to separate the internal code for "c" and "unlist".
   Although the functions are quite similar, they operate on very
   different data structures.
*/

/* The major difference between the two functions is that the value of
   the "recursive" argument is FALSE by default for "c" and TRUE for
   "unlist".  In addition, "c" takes ... while "unlist" takes a single
   argument.
*/

static SEXP do_c (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP ans;

    checkArity(op, args);

    /* Attempt method dispatch. */

    if (DispatchOrEval(call, op, "c", args, env, &ans, 1, 1))
	return(ans);

    return do_c_dflt(call, op, ans, env, variant);
}

/* function below is also called directly from eval.c */

#define OBJ_ARRAY_SIZE 30  /* size of array holdings objs to be concatenated */

SEXP attribute_hidden do_c_dflt (SEXP call, SEXP op, SEXP args, SEXP env,
                                 int variant)
{
    SEXP ans, t;
    R_len_t nobj;
    int recurse, usenames, anytags, highesttype;
    struct BindData data;
    struct NameData nameData;

    usenames = 1;
    recurse = 0;

    PROTECT(args = process_c_args (args, call, &recurse, &usenames, 
                                   &anytags, &nobj));

    if (!(usenames && anytags) && !recurse && nobj <= OBJ_ARRAY_SIZE) {
        SEXP objs[OBJ_ARRAY_SIZE];
        SEXP a;
        int i; 
        a = args;
        for (i = 0; i < nobj; i++) {
            objs[i] = CAR(a);
            a = CDR(a);
        }
        ans = simple_concatenate (objs, nobj, usenames, variant, call, env);
        if (ans != R_NoObject) {
            UNPROTECT(1);
            return ans;
        }
    }

    /* General case.  Determine the type of the returned value. 
       If a non-vector argument was encountered (perhaps a list if
       recursive is FALSE) then we must return a list. Otherwise, we
       use the natural coercion for vector types. */

    data.ans_type  = NILSXP;
    data.ans_length = 0;
    data.ans_nnames = 0;

    for (t = args; t != R_NilValue; t = CDR(t)) {
        if (usenames && !data.ans_nnames) {
            if (!isNull(TAG(t))) data.ans_nnames = 1;
            else data.ans_nnames = HasNames(CAR(t));
        }
        AnswerType(CAR(t), recurse, usenames, &data, call);
    }

    if (data.ans_type == NILSXP && data.ans_length != 0) abort();

    /* Allocate the return value and set up to pass through 
       the arguments filling in values of the returned object. */

    PROTECT(ans = allocVector(data.ans_type, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;

    if (data.ans_type == VECSXP || data.ans_type == EXPRSXP 
                                || data.ans_type == NILSXP)
        ListAnswer (args, recurse ? 1 : -1, &data);
    else
        AtomicAnswer(args, &data);

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
        PROTECT(data.ans_names = allocVector(STRSXP, data.ans_length));
        data.ans_nnames = 0;
        while (args != R_NilValue) {
            nameData.seqno = 0;
            nameData.firstpos = 0;
            nameData.count = 0;
            NewExtractNames (CAR(args), R_NilValue, TAG(args), 
                             recurse, &data, &nameData);
            args = CDR(args);
        }
        setAttrib(ans, R_NamesSymbol, data.ans_names);
        R_FreeStringBufferL(&cbuff);
        UNPROTECT(1);
    }

    UNPROTECT(2);
    return ans;
} /* do_c */


static SEXP do_unlist(SEXP call, SEXP op, SEXP args, SEXP env, int variant)
{
    SEXP ans, lst, t;
    int recurse, usenames;
    int i, n;
    struct BindData data;
    struct NameData nameData;

    checkArity(op, args);

    /* Attempt method dispatch. */

    if (DispatchOrEval(call, op, "unlist", args, env, &ans, 0, 1))
	return(ans);

    /* Method dispatch has failed; run the default code. */

    PROTECT(lst = CAR(ans));
    recurse = asLogical(CADR(ans));
    usenames = asLogical(CADDR(ans));

    SEXP topnames = usenames ? getNamesAttrib(lst) : R_NilValue;

    if (TYPEOF(lst) == VECSXP && LENGTH(lst) > 0 && topnames == R_NilValue) {
        if (NAMEDCNT_EQ_0(VECTOR_ELT(lst,0)))
            SET_NAMEDCNT_1(VECTOR_ELT(lst,0));  /* so won't be reallocated */
        ans = simple_concatenate ((SEXP*)DATAPTR(lst), LENGTH(lst), 
                                  usenames, variant, call, env);
        if (ans != R_NoObject) {
            UNPROTECT(1);
            return ans;
        }
    }

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a generic vector. */

    data.ans_type  = NILSXP;
    data.ans_length = 0;
    data.ans_nnames = 0;

    n = 0;			/* -Wall */
    if (isNewList(lst)) {
	n = length(lst);
	if (usenames && HAS_ATTRIB(lst) /* quick pre-test */
                     && getNamesAttrib(lst) != R_NilValue)
	    data.ans_nnames = 1;
	for (i = 0; i < n; i++) {
	    if (usenames && !data.ans_nnames)
		data.ans_nnames = HasNames(VECTOR_ELT(lst, i));
	    AnswerType(VECTOR_ELT(lst, i), recurse, usenames, &data, call);
	}
    }
    else if (isList(lst)) {
	for (t = lst; t != R_NilValue; t = CDR(t)) {
	    if (usenames && !data.ans_nnames) {
		if (!isNull(TAG(t))) data.ans_nnames = 1;
		else data.ans_nnames = HasNames(CAR(t));
	    }
	    AnswerType(CAR(t), recurse, usenames, &data, call);
	}
    }
    else {
	UNPROTECT(1);
	if (isVector(lst)) return lst;
	else error(_("argument not a list"));
    }

    /* Allocate the return value and set up to pass through 
       the arguments filling in values of the returned object.
       If a non-vector argument was encountered (perhaps a list if 
       recursive = F) then we must return a list.  Otherwise, we use 
       the natural coercion for vector types. */

    PROTECT(ans = allocVector(data.ans_type, data.ans_length));
    data.ans_ptr = ans;
    data.ans_length = 0;

    if (data.ans_type == VECSXP || data.ans_type == EXPRSXP 
                                || data.ans_type == NILSXP)
	ListAnswer(lst, recurse ? 1 : -1, &data);
    else
	AtomicAnswer(lst, &data);

    /* Build and attach the names attribute for the returned object. */

    if (data.ans_nnames && data.ans_length > 0) {
	PROTECT(data.ans_names = allocVector(STRSXP, data.ans_length));
	if (!recurse) {
	    if (TYPEOF(lst) == VECSXP) {
		SEXP names = getNamesAttrib(lst);
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.firstpos = 0;
		nameData.count = 0;
		for (i = 0; i < n; i++) {
		    NewExtractNames(VECTOR_ELT(lst, i), R_NilValue,
				    ItemName(names, i), recurse, &data, &nameData);
		}
	    }
	    else if (TYPEOF(lst) == LISTSXP) {
		data.ans_nnames = 0;
		nameData.seqno = 0;
		nameData.firstpos = 0;
		nameData.count = 0;
		while (lst != R_NilValue) {
		    NewExtractNames(CAR(lst), R_NilValue,
				    TAG(lst), recurse, &data, &nameData);
		    lst = CDR(lst);
		}
	    }
	}
	else {
	    data.ans_nnames = 0;
	    nameData.seqno = 0;
	    nameData.firstpos = 0;
	    nameData.count = 0;
	    NewExtractNames(lst, R_NilValue, R_NilValue, recurse, &data, &nameData);
	}
	setAttrib(ans, R_NamesSymbol, data.ans_names);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return ans;
} /* do_unlist */


/* cbind(deparse.level, ...) and rbind(deparse.level, ...) : */
/* This is a special .Internal */
static SEXP do_bind(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a, t, obj, classlist, classname, method, classmethod, rho;
    const char *generic;
    int deparse_level;
    Rboolean compatible = TRUE;
    struct BindData data;
    char buf[512];
    const char *s, *klass;

    /* since R 2.2.0: first argument "deparse.level" */
    deparse_level = asInteger(eval(CAR(args), env));
    args = CDR(args);

    /* Lazy evaluation and method dispatch based on argument types are
     * fundamentally incompatible notions.  The results here are
     * ghastly.
     *
     * We build promises to evaluate the arguments and then force the
     * promises so that if we despatch to a closure below, the closure
     * is still in a position to use "substitute" to get the actual
     * expressions which generated the argument (for naming purposes).
     *
     * The dispatch rule here is as follows:
     *
     * 1) For each argument we get the list of possible class
     *	  memberships from the class attribute.
     *
     * 2) We inspect each class in turn to see if there is an
     *	  applicable method.
     *
     * 3) If we find an applicable method we make sure that it is
     *	  identical to any method determined for prior arguments.
     *	  If it is identical, we proceed, otherwise we immediately
     *	  drop through to the default code.
     */

    PROTECT(args = promiseArgs(args, env));

    generic = ((PRIMVAL(op) == 1) ? "cbind" : "rbind");
    klass = "";
    method = R_NilValue;
    for (a = args; a != R_NilValue && compatible; a = CDR(a)) {
	PROTECT(obj = eval(CAR(a), env));
	if (isObject(obj)) {
	    int i, len_classlist;
	    classlist = getClassAttrib(obj);
            len_classlist = length(classlist);
	    for (i = 0; i < len_classlist; i++) {
		classname = STRING_ELT(classlist, i);
		s = translateChar(classname);
                if (!copy_3_strings (buf, sizeof buf, generic, ".", s))
		    error(_("class name too long in '%s'"), generic);
		classmethod = R_LookupMethod(install(buf), env, env,
					     R_BaseNamespace);
		if (classmethod != R_UnboundValue) {
		    if (klass[0] == '\0') {
			/* There is no previous class */
			/* We use this method. */
			klass = s;
			method = classmethod;
		    }
		    else {
			/* Check compatibility with the */
			/* previous class.  If the two are not */
			/* compatible we drop through to the */
			/* default method. */
			if (strcmp(klass, s)) {
			    method = R_NilValue;
			    /* need to end both loops */
			    compatible = FALSE;
			}
		    }
		    break; /* go to next parameter */
		}
	    }
	}
	UNPROTECT(1);
    }
    if (method != R_NilValue) {
	PROTECT(method);
	args = applyClosure(call, method, args, env, NULL);
	UNPROTECT(2);
	return args;
    }

    /* Dispatch based on class membership has failed. */
    /* The default code for rbind/cbind.default follows */
    /* First, extract the evaluated arguments. */

    rho = env;
    data.ans_type = NILSXP;
    data.ans_length = 0;
    data.ans_nnames = 0;
    for (t = args; t != R_NilValue; t = CDR(t))
	AnswerType (TYPEOF(CAR(t))==PROMSXP ? PRVALUE(CAR(t)) : CAR(t), 
                    0, 0, &data, call);

    /* zero-extent matrices shouldn't give NULL, but cbind(NULL) should: */

    if (data.ans_type == NILSXP) {
        if (data.ans_length != 0) abort();
	UNPROTECT(1);
	return R_NilValue;
    }

    switch (data.ans_type) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	break;
	/* we don't handle expressions: we could, but coercion of a matrix
	   to an expression is not ideal */
    default:
	error(_("cannot create a matrix from these types"));
    }

    if (PRIMVAL(op) == 1)
	a = cbind(call, args, data.ans_type, rho, deparse_level);
    else
	a = rbind(call, args, data.ans_type, rho, deparse_level);
    UNPROTECT(1);
    return a;
}


static void SetRowNames(SEXP dimnames, SEXP x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 0, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCAR(dimnames, x);
}

static void SetColNames(SEXP dimnames, SEXP x)
{
    if (TYPEOF(dimnames) == VECSXP)
	SET_VECTOR_ELT(dimnames, 1, x);
    else if (TYPEOF(dimnames) == LISTSXP)
	SETCADR(dimnames, x);
}

static SEXP cbind(SEXP call, SEXP args, SEXPTYPE mode, SEXP rho,
		  int deparse_level)
{
    int h, i, j, k, idx, nargs, n;
    Rboolean have_rnames = FALSE, have_cnames = FALSE, warned = FALSE;
    int nnames, mnames;
    int rows, cols, mrows, lenmin;
    SEXP dn, t, u, result, expr;

    nargs = length(args);

    char argkind[nargs]; /* Kind of argument: 1=vector, 2=matrix, 3=other */
    SEXP argval[nargs];  /* Values of arguments, later maybe coerced versions */
    R_len_t matrows[nargs];  /* Numbers of rows in matrices */
    R_len_t matcols[nargs];  /* Numbers of columns in matrices */
    R_len_t arg_len[nargs];  /* Lengths of non-matrix args */

    lenmin = 0;

    /* Record args, what kind they are, and their numbers of rows and columns
       or lengths for non-matrix arguments.  Also check if we are in the 
       zero-rows case. */

    for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	argval[n] = TYPEOF(CAR(t))==PROMSXP ? PRVALUE(CAR(t)) : CAR(t);
        argkind[n] = !isVector(argval[n]) && !isPairList(argval[n]) ? 3
                      : isMatrix(argval[n]) ? 2 : 1;
        if (argkind[n] == 2) {
            matrows[n] = nrows(argval[n]);
            matcols[n] = ncols(argval[n]);
            if (matrows[n] > 0) 
                lenmin = 1;
        }
        else {
            arg_len[n] = length(argval[n]);
            if (arg_len[n] > 0)
                lenmin = 1;
        }
    }

    /* check conformability of matrix arguments */

    rows = 0;
    cols = 0;
    mrows = -1;

    for (n = 0; n < nargs; n++) {
        if (argkind[n] == 2) {
            if (mrows == -1)
                mrows = matrows[n];
            else if (mrows != matrows[n])
               error(_("number of rows of matrices must match (see arg %d)"),
                     n + 1);
            cols += matcols[n];
        }
        else if (arg_len[n] >= lenmin) {
            if (arg_len[n] > rows) rows = arg_len[n];
            cols += 1;
        }
    }

    if (mrows != -1) 
        rows = mrows;

    /* Check conformability of non-matrix arguments. -- Look for dimnames. */

    nnames = 0;
    mnames = 0;

    for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	if (argkind[n] == 2) {
	    dn = getAttrib (argval[n], R_DimNamesSymbol);
	    if (length(dn) == 2) {
		if (VECTOR_ELT(dn, 1) != R_NilValue)
		    have_cnames = TRUE;
		if (VECTOR_ELT(dn, 0) != R_NilValue)
		    mnames = mrows;
	    }
	}
	else {
	    k = arg_len[n];
	    if (!warned && k>0 && (k > rows || rows % k)) {
		warned = TRUE;
		warning("number of rows of result is not a multiple of vector length (arg %d)", n + 1);
	    }
	    PROTECT(dn = getNamesAttrib(argval[n]));
	    if (k >= lenmin && (TAG(t) != R_NilValue ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 isSymbol(substitute(CAR(t),R_NilValue)))))
		have_cnames = TRUE;
	    nnames = imax2(nnames, length(dn));
            UNPROTECT(1); /* dn */
	}
    }

    if (mnames || nnames == rows)
	have_rnames = TRUE;

    /* allocate space for result. */

    PROTECT(result = allocMatrix(mode, rows, cols));

    /* Coerce data for VECSXP result.  Replace args to ignore with R_NilValue */

    if (mode == VECSXP) {
        for (n = 0; n < nargs; n++) {
            if (argkind[n] != 3)
                argval[n] = argkind[n] == 2 || arg_len[n] >= lenmin 
                             ? coerceVector(argval[n],mode) : R_NilValue;
            PROTECT(argval[n]);
        }
    }
    else {
        for (n = 0; n < nargs; n++)
            if (argkind[n] == 1 && arg_len[n] < lenmin) 
                argval[n] = R_NilValue;
    }

    /* Copy the data. */

    j = 0;

    for (n = 0; n < nargs; n++) {
        if (argval[n] != R_NilValue) {
            if (mode == VECSXP) {
                if (argkind[n] == 3) { /* something special - eg, closure */
                    idx = 1;
                    for (i = 0; i < rows; i++)
                        SET_VECTOR_ELT (result, i + j*rows,
                                                duplicate(argval[n]));
                }
                else { /* matrix or vector */
                    idx = argkind[n] == 2 ? matcols[n] : 1;
                    if (idx == 1) { /* vector, or matrix with one column */
                        k = LENGTH(argval[n]);
                        if (k >= rows) /* no repetition needed */
                            copy_elements (result, j*rows, 1,
                                           argval[n], 0, 1, rows);
                        else if (k == 1) /* repeat single element */
                            copy_elements (result, j*rows, 1,
                                           argval[n], 0, 0, rows);
                        else { /* need to repeat short vector */
                            if (k == 0) abort(); /* shouldn't happen */
                            for (h = 0; h < rows; h += k)
                                copy_elements (result, h + j*rows, 1,
                                   argval[n], 0, 1, rows-h >= k ? k : rows-h);
                        }
                    }
                    else { /* general matrix */
                        copy_elements (result, j*rows, 1,
                                       argval[n], 0, 1, idx*rows);
                    }
                }
            }
            else {
                idx = argkind[n] == 2 ? matcols[n] : 1;
                if (idx == 1) { /* vector, or matrix with one column */
                    k = LENGTH(argval[n]);
                    if (k >= rows) /* no repetition needed */
                        copy_elements_coerced (result, j*rows, 1,
                                               argval[n], 0, 1, rows);
                    else if (k == 1) /* repeat single element */
                        copy_elements_coerced (result, j*rows, 1,
                                               argval[n], 0, 0, rows);
                    else { /* need to repeat short vector */
                        if (k == 0) abort(); /* shouldn't happen */
                        for (h = 0; h < rows; h += k)
                            copy_elements_coerced (result, h + j*rows, 1,
                               argval[n], 0, 1, rows-h >= k ? k : rows-h);
                    }
                } 
                else { /* general matrix */
                    copy_elements_coerced (result, j*rows, 1,
                                           argval[n], 0, 1, idx*rows);
                }
            }
            j += idx;
        }
    }

    if (mode == VECSXP)
        UNPROTECT(nargs);

    /* Adjust dimnames attributes. */

    if (have_cnames || have_rnames) {
	SEXP nam, tnam,v;
	PROTECT(dn = allocVector(VECSXP, 2));
	if (have_cnames)
	    nam = SET_VECTOR_ELT(dn, 1, allocVector(STRSXP, cols));
	else
	    nam = R_NilValue;	/* -Wall */
	j = 0;
	for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	    u = TYPEOF(CAR(t))==PROMSXP ? PRVALUE(CAR(t)) : CAR(t);
	    if (argkind[n] == 2) {
		v = getAttrib(u, R_DimNamesSymbol);

		if (have_rnames &&
		    GetRowNames(dn) == R_NilValue &&
		    GetRowNames(v) != R_NilValue)
		    SetRowNames(dn, duplicate(GetRowNames(v)));

		/* rbind() does this only  if(have_?names) .. : */
		/* but if tnam is non-null, have_cnames = TRUE: see above */
		tnam = GetColNames(v);
		if (tnam != R_NilValue) {
                    copy_string_elements (nam, j, tnam, 0, LENGTH(tnam));
                    j += LENGTH(tnam);
		}
		else if (have_cnames) {
		    for (i = 0; i < ncols(u); i++)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    } else if (arg_len[n] >= lenmin) {
		v = getNamesAttrib(u);

		if (have_rnames && GetRowNames(dn) == R_NilValue
		    && v != R_NilValue && length(v) == rows)
		    SetRowNames(dn, duplicate(v));

		if (TAG(t) != R_NilValue)
		    SET_STRING_ELT(nam, j++, PRINTNAME(TAG(t)));
		else {
		    expr = substitute(CAR(t), R_NilValue);
		    if (deparse_level == 1 && isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
		        PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(deparse1line(expr, TRUE), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_cnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	setAttrib(result, R_DimNamesSymbol, dn);
	UNPROTECT(1);
    }

    UNPROTECT(1);
    return result;

} /* cbind */


#define RBIND_COLS 4  /* Number of columns to copy at once */

static SEXP rbind(SEXP call, SEXP args, SEXPTYPE mode, SEXP rho,
		  int deparse_level)
{
    int h, i, j, k, idx, nargs, n;
    Rboolean have_rnames = FALSE, have_cnames = FALSE, warned = FALSE;
    int nnames, mnames;
    int rows, cols, mcols, lenmin;
    SEXP dn, t, result, expr;
 
    nargs = length(args);

    char argkind[nargs]; /* Kind of argument: 1=vector, 2=matrix, 3=other */
    SEXP argval[nargs];  /* Values of arguments, later maybe coerced versions */
    R_len_t matrows[nargs];  /* Numbers of rows in matrices */
    R_len_t matcols[nargs];  /* Numbers of columns in matrices */
    R_len_t arg_len[nargs];  /* Lengths of non-matrix args */

    lenmin = 0;

    /* Record args, what kind they are, and their numbers of rows and columns
       or lengths for non-matrix arguments.  Also check if we are in the 
       zero-cols case. */

    for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	argval[n] = TYPEOF(CAR(t))==PROMSXP ? PRVALUE(CAR(t)) : CAR(t);
        argkind[n] = !isVector(argval[n]) && !isPairList(argval[n]) ? 3
                      : isMatrix(argval[n]) ? 2 : 1;
        if (argkind[n] == 2) {
            matrows[n] = nrows(argval[n]);
            matcols[n] = ncols(argval[n]);
            if (matcols[n] > 0) 
                lenmin = 1;
        }
        else {
            arg_len[n] = length(argval[n]);
            if (arg_len[n] > 0)
                lenmin = 1;
        }
    }

    /* check conformability of matrix arguments */

    rows = 0;
    cols = 0;
    mcols = -1;

    for (n = 0; n < nargs; n++) {
        if (argkind[n] == 2) {
            if (mcols == -1)
                mcols = matcols[n];
            else if (mcols != matcols[n])
               error(_("number of columns of matrices must match (see arg %d)"),
                     n + 1);
            rows += matrows[n];
        }
        else if (arg_len[n] >= lenmin) {
            if (arg_len[n] > cols) cols = arg_len[n];
            rows += 1;
        }
    }

    if (mcols != -1) 
        cols = mcols;

    /* Check conformability of non-matrix arguments. -- Look for dimnames. */

    nnames = 0;
    mnames = 0;

    for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	if (argkind[n] == 2) {
	    dn = getAttrib (argval[n], R_DimNamesSymbol);
	    if (length(dn) == 2) {
		if (VECTOR_ELT(dn, 0) != R_NilValue)
		    have_rnames = TRUE;
		if (VECTOR_ELT(dn, 1) != R_NilValue)
		    mnames = mcols;
	    }
	}
	else {
	    k = arg_len[n];
	    if (!warned && k>0 && (k > cols || cols % k)) {
		warned = TRUE;
		warning("number of columns of result is not a multiple of vector length (arg %d)", n + 1);
	    }
	    PROTECT(dn = getNamesAttrib(argval[n]));
	    if (k >= lenmin && (TAG(t) != R_NilValue ||
				(deparse_level == 2) ||
				((deparse_level == 1) &&
				 isSymbol(substitute(CAR(t),R_NilValue)))))
		have_rnames = TRUE;
	    nnames = imax2(nnames, length(dn));
            UNPROTECT(1); /* dn */
	}
    }

    if (mnames || nnames == cols)
	have_cnames = TRUE;

    /* allocate space for result. */

    PROTECT(result = allocMatrix(mode, rows, cols));

    /* Coerce data for VECSXP result.  Replace args to ignore with R_NilValue */

    if (mode == VECSXP) {
        for (n = 0; n < nargs; n++) {
            if (argkind[n] != 3)
                argval[n] = argkind[n] == 2 || arg_len[n] >= lenmin 
                             ? coerceVector(argval[n],mode) : R_NilValue;
            PROTECT(argval[n]);
        }
    }
    else {
        for (n = 0; n < nargs; n++)
            if (argkind[n] == 1 && arg_len[n] < lenmin) 
                argval[n] = R_NilValue;
    }

    /* Copy the data.  Data from all arguments is copied into succesive 
       columns of the result matrix, with RBIND_COLS being copied at once,
       to improve cache performance, and for vectors or matrices with few
       rows, reduce the overhead of calling copy_elements... */

    j = 0;

    while (j < cols) {

        int m = j+RBIND_COLS <= cols ? j+RBIND_COLS : cols;

        i = 0;
        for (n = 0; n < nargs; n++) {
            if (argval[n] != R_NilValue) {
                if (mode == VECSXP) {
                    if (argkind[n] == 3) { /* something special - eg, closure */
                        idx = 1;
                        for (h = j; h < m; h++)
                            SET_VECTOR_ELT (result, i + h*rows,
                                                    duplicate(argval[n]));
                    }
                    else { /* matrix or vector */
                        idx = argkind[n] == 2 ? matrows[n] : 1;
                        if (idx == 1) { /* vector, or matrix with one row */
                            k = LENGTH(argval[n]);
                            if (k >= cols) /* no repetition needed */
                                copy_elements (result, i + j*rows, rows,
                                               argval[n], j, 1, m-j);
                            else if (k == 1) /* repeat single element */
                                copy_elements (result, i + j*rows, rows,
                                               argval[n], 0, 0, m-j);
                            else { /* need to repeat short vector */
                                if (k == 0) abort(); /* shouldn't happen */
                                h = j;
                                while (h < m) {
                                    int s = h%k, t = k-s;
                                    copy_elements (result, i + h*rows, rows,
                                      argval[n], s, 1, t>m-h ? m-h : t);
                                    h += t;
                                }
                            }
                        }
                        else { /* general matrix */
                            for (h = j; h < m; h++)
                                copy_elements (result, i + h*rows, 1,
                                               argval[n], h*idx, 1, idx);
                        }
                    }
                }
                else {
                    idx = argkind[n] == 2 ? matrows[n] : 1;
                    if (idx == 1) { /* vector, or matrix with one row */
                        k = LENGTH(argval[n]);
                        if (k >= cols) /* no repetition needed */
                            copy_elements_coerced (result, i + j*rows, rows,
                                                   argval[n], j, 1, m-j);
                        else if (k == 1) /* repeat single element */
                            copy_elements_coerced (result, i + j*rows, rows,
                                                   argval[n], 0, 0, m-j);
                        else { /* need to repeat short vector */
                            if (k == 0) abort(); /* shouldn't happen */
                            h = j;
                            while (h < m) {
                                int s = h%k, t = k-s;
                                copy_elements_coerced (result, i + h*rows, rows,
                                   argval[n], s, 1, t>m-h ? m-h : t);
                                h += t;
                            }
                        }
                    }
                    else if (idx < m-j) { /* matrix with few rows */
                        for (h = 0; h < idx; h++)
                            copy_elements_coerced (result, i + h + j*rows, rows,
                              argval[n], h + j*idx, idx, m-j);
                    }
                    else { /* general matrix */
                        for (h = j; h < m; h++)
                            copy_elements_coerced (result, i + h*rows, 1,
                                                   argval[n], h*idx, 1, idx);
                    }
                }
                i += idx;
            }
        }

        j = m;
    }

    if (mode == VECSXP)
        UNPROTECT(nargs);

    /* adjust dimnames attributes. */

    if (have_rnames || have_cnames) {
	SEXP nam, tnam, u, v;
	PROTECT(dn = allocVector(VECSXP, 2));
	if (have_rnames)
	    nam = SET_VECTOR_ELT(dn, 0, allocVector(STRSXP, rows));
	else
	    nam = R_NilValue;	/* -Wall */
	j = 0;
	for (t = args, n = 0; t != R_NilValue; t = CDR(t), n++) {
	    u = TYPEOF(CAR(t))==PROMSXP ? PRVALUE(CAR(t)) : CAR(t);
	    if (argkind[n] == 2) {
		v = getAttrib(u, R_DimNamesSymbol);

		if (have_cnames &&
		    GetColNames(dn) == R_NilValue &&
		    GetColNames(v) != R_NilValue)
		    SetColNames(dn, duplicate(GetColNames(v)));

		/* cbind() doesn't test have_?names BEFORE tnam!=Nil..:*/
		/* but if tnam is non-null, have_rnames = TRUE: see above */
		tnam = GetRowNames(v);
		if (have_rnames) {
		    if (tnam != R_NilValue) {
                        copy_string_elements (nam, j, tnam, 0, LENGTH(tnam));
                        j += LENGTH(tnam);
                    }
		    else {
			for (i = 0; i < matrows[n]; i++)
			    SET_STRING_ELT(nam, j++, R_BlankString);
		    }
		}
	    }
	    else if (arg_len[n] >= lenmin) {
		v = getNamesAttrib(u);

		if (have_cnames && GetColNames(dn) == R_NilValue
		    && v != R_NilValue && length(v) == cols)
		    SetColNames(dn, duplicate(v));

		if (TAG(t) != R_NilValue)
		    SET_STRING_ELT(nam, j++, PRINTNAME(TAG(t)));
		else {
		    expr = substitute(CAR(t), R_NilValue);
		    if (deparse_level == 1 && isSymbol(expr))
			SET_STRING_ELT(nam, j++, PRINTNAME(expr));
		    else if (deparse_level == 2) {
		        PROTECT(expr);
			SET_STRING_ELT(nam, j++,
				       STRING_ELT(deparse1line(expr, TRUE), 0));
			UNPROTECT(1); /* expr */
		    } else if (have_rnames)
			SET_STRING_ELT(nam, j++, R_BlankString);
		}
	    }
	}
	setAttrib(result, R_DimNamesSymbol, dn);
	UNPROTECT(1);
    }

    UNPROTECT(1);
    return result;

} /* rbind */

/* FUNTAB entries defined in this source file. See names.c for documentation. */

attribute_hidden FUNTAB R_FunTab_bind[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{"c",		do_c,		0,	1001,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist,	0,	1011,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};
