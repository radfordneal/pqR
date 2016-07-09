/* C implementations used in ex1.r.

   Part of pqR.
   Copyright 2016 by Radford M. Neal.  
   Distributed under GPL version 2 or later. */

/* These functions assume that they might be called directly from C, and
   so protect their arguments, which would automatically be protected
   when called from R with .Call or .External.  They also conservatively
   protect objects that might need protection if the code was modified
   in not-unlikely ways. */

#include <R.h>
#include <Rinternals.h>


/* Implementation for use with .Call, using features of the old C API only. */

SEXP ex1_Call_old (SEXP L)
{
    SEXP res, e, e_before;

    PROTECT (L);

    /* Check single argument (list of R arguments) for validity, and
       determine the type and length of the result, stored in res_type 
       and res_length.  Also sets L_len to the number of vectors to
       concatenate. */

    if (TYPEOF(L) != VECSXP)
        error ("C argument is not a list");

    const R_len_t L_len = LENGTH(L);
    if (L_len == 0)
        error ("nothing to concatenate");

    SEXPTYPE res_type = INTSXP;  /* change to REALSXP or STRSXP if necessary  */
    R_len_t res_len;
    R_len_t i;

    for (i = 0; i < L_len; i++) {

        e = VECTOR_ELT (L, i);

        switch (TYPEOF(e)) {
        case STRSXP:
            res_type = STRSXP;
            break;
        case REALSXP:
            if (res_type != STRSXP) res_type = REALSXP;
            break;
        case INTSXP:
            break;
        default:
            error ("argument is not numeric or character");
        }

        if (LENGTH(e) == 0)
            error ("argument has zero length");

        if (i == 0)
            res_len = LENGTH(e);
        else {
            if (LENGTH(e) - 1 > R_LEN_T_MAX - res_len)
                error ("result would be too large");
            res_len += LENGTH(e) - 1;
        }
    }

    /* Allocate result vector, then coerce and copy over elements of vectors
       being concatenated, signaling an error if the first element of a vector 
       does not match the last element of the previous vector. */

    PROTECT (res = allocVector (res_type, res_len));

    R_len_t res_pos = 0;  /* Place to store next element copied */

    for (i = 0; i < L_len; i++) {

        /* Coerce this vector to the result type. */

        PROTECT (e = coerceVector (VECTOR_ELT (L, i), res_type));
        R_len_t e_len = LENGTH(e);

        /* Check that last/first elements match. */

        if (i > 0) {
            if (TYPEOF(e) == STRSXP ? 
                   strcmp (CHAR (STRING_ELT (e_before, LENGTH(e_before)-1)), 
                           CHAR (STRING_ELT (e, 0))) != 0
                : TYPEOF(e) == REALSXP ?
                   REAL(e_before)[LENGTH(e_before)-1] != REAL(e)[0]
                :
                   INTEGER(e_before)[LENGTH(e_before)-1] != INTEGER(e)[0]) 
            {
                error ("end of argument %d doesn't match start of argument %d",
                         i, i+1);
            }

            UNPROTECT(2);   /* e_before & e */
            PROTECT(e);
        }

        /* Copy elements from this vector into the result vector. */

        R_len_t j = i==0 ? 0 : 1;
        switch (TYPEOF(e)) {
        case STRSXP:
            while (j < LENGTH(e)) 
                SET_STRING_ELT (res, res_pos++, STRING_ELT(e,j++));
            break;
        case REALSXP:
            while (j < LENGTH(e)) 
                REAL(res)[res_pos++] = REAL(e)[j++];
            break;
        case INTSXP:
            while (j < LENGTH(e)) 
                INTEGER(res)[res_pos++] = INTEGER(e)[j++];
            break;
        }

        e_before = e;
    }

    if (i > 0) UNPROTECT(1);  /* e */

    UNPROTECT(2);  /* L & res */

    return res;
}


/* Implementation for use with .Call, using the new facility for pointer 
   protection. */

SEXP ex1_Call_new (SEXP L)
{
    BEGIN_PROTECT3 (res, e, e_before);
    ALSO_PROTECT1 (L);

    /* Check single argument (list of R arguments) for validity, and
       determine the type and length of the result, stored in res_type 
       and res_length.  Also sets L_len to the number of vectors to
       concatenate. */

    if (TYPEOF(L) != VECSXP)
        error ("C argument is not a list");

    const R_len_t L_len = LENGTH(L);
    if (L_len == 0)
        error ("nothing to concatenate");

    SEXPTYPE res_type = INTSXP;  /* change to REALSXP or STRSXP if necessary  */
    R_len_t res_len;
    R_len_t i;

    for (i = 0; i < L_len; i++) {

        e = VECTOR_ELT (L, i);

        switch (TYPEOF(e)) {
        case STRSXP:
            res_type = STRSXP;
            break;
        case REALSXP:
            if (res_type != STRSXP) res_type = REALSXP;
            break;
        case INTSXP:
            break;
        default:
            error ("argument is not numeric or character");
        }

        if (LENGTH(e) == 0)
            error ("argument has zero length");

        if (i == 0)
            res_len = LENGTH(e);
        else {
            if (LENGTH(e) - 1 > R_LEN_T_MAX - res_len)
                error ("result would be too large");
            res_len += LENGTH(e) - 1;
        }
    }

    /* Allocate result vector, then coerce and copy over elements of vectors
       being concatenated, signaling an error if the first element of a vector 
       does not match the last element of the previous vector. */

    res = allocVector (res_type, res_len);

    R_len_t res_pos = 0;  /* Place to store next element copied */

    for (i = 0; i < L_len; i++) {

        /* Coerce this vector to the result type. */

        e = coerceVector (VECTOR_ELT (L, i), res_type);
        R_len_t e_len = LENGTH(e);

        /* Check that last/first elements match. */

        if (i > 0) {
            if (TYPEOF(e) == STRSXP ? 
                   strcmp (CHAR (STRING_ELT (e_before, LENGTH(e_before)-1)), 
                           CHAR (STRING_ELT (e, 0))) != 0
                : TYPEOF(e) == REALSXP ?
                   REAL(e_before)[LENGTH(e_before)-1] != REAL(e)[0]
                :
                   INTEGER(e_before)[LENGTH(e_before)-1] != INTEGER(e)[0]) 
            {
                error ("end of argument %d doesn't match start of argument %d",
                         i, i+1);
            }
        }

        /* Copy elements from this vector into the result vector. */

        R_len_t j = i==0 ? 0 : 1;
        switch (TYPEOF(e)) {
        case STRSXP:
            while (j < LENGTH(e)) 
                SET_STRING_ELT (res, res_pos++, STRING_ELT(e,j++));
            break;
        case REALSXP:
            while (j < LENGTH(e)) 
                REAL(res)[res_pos++] = REAL(e)[j++];
            break;
        case INTSXP:
            while (j < LENGTH(e)) 
                INTEGER(res)[res_pos++] = INTEGER(e)[j++];
            break;
        }

        e_before = e;
    }

    RETURN_SEXP_INSIDE_PROTECT(res);
    END_PROTECT;
}


/* Implementation for use with .Call, using the new facility for pointer 
   protection, and the copy_elements function. */

SEXP ex1_Call_new2 (SEXP L)
{
    BEGIN_PROTECT3 (res, e, e_before);
    ALSO_PROTECT1 (L);

    /* Check single argument (list of R arguments) for validity, and
       determine the type and length of the result, stored in res_type 
       and res_length.  Also sets L_len to the number of vectors to
       concatenate. */

    if (TYPEOF(L) != VECSXP)
        error ("C argument is not a list");

    const R_len_t L_len = LENGTH(L);
    if (L_len == 0)
        error ("nothing to concatenate");

    SEXPTYPE res_type = INTSXP;  /* change to REALSXP or STRSXP if necessary  */
    R_len_t res_len;
    R_len_t i;

    for (i = 0; i < L_len; i++) {

        e = VECTOR_ELT (L, i);

        switch (TYPEOF(e)) {
        case STRSXP:
            res_type = STRSXP;
            break;
        case REALSXP:
            if (res_type != STRSXP) res_type = REALSXP;
            break;
        case INTSXP:
            break;
        default:
            error ("argument is not numeric or character");
        }

        if (LENGTH(e) == 0)
            error ("argument has zero length");

        if (i == 0)
            res_len = LENGTH(e);
        else {
            if (LENGTH(e) - 1 > R_LEN_T_MAX - res_len)
                error ("result would be too large");
            res_len += LENGTH(e) - 1;
        }
    }

    /* Allocate result vector, then coerce and copy over elements of vectors
       being concatenated, signaling an error if the first element of a vector 
       does not match the last element of the previous vector. */

    res = allocVector (res_type, res_len);

    R_len_t res_pos = 0;  /* Place to store next element copied */

    for (i = 0; i < L_len; i++) {

        /* Coerce this vector to the result type. */

        e = coerceVector (VECTOR_ELT (L, i), res_type);
        R_len_t e_len = LENGTH(e);

        /* Check that last/first elements match. */

        if (i > 0) {
            if (TYPEOF(e) == STRSXP ? 
                   strcmp (CHAR (STRING_ELT (e_before, LENGTH(e_before)-1)), 
                           CHAR (STRING_ELT (e, 0))) != 0
                : TYPEOF(e) == REALSXP ?
                   REAL(e_before)[LENGTH(e_before)-1] != REAL(e)[0]
                :
                   INTEGER(e_before)[LENGTH(e_before)-1] != INTEGER(e)[0]) 
            {
                error ("end of argument %d doesn't match start of argument %d",
                         i, i+1);
            }
        }

        /* Copy elements from this vector into the result vector. */

        if (i == 0) {
            copy_elements (res, res_pos, 1, e, 0, 1, LENGTH(e));
            res_pos += LENGTH(e);
        }
        else {
            copy_elements (res, res_pos, 1, e, 1, 1, LENGTH(e) - 1);
            res_pos += LENGTH(e) - 1;
        }

        e_before = e;
    }

    RETURN_SEXP_INSIDE_PROTECT(res);
    END_PROTECT;
}
