/*
 *  pqR : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2013, 2014 Radford M. Neal
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

#include "helpers-app.h"

#ifndef HELPERS_DISABLED


/* NAMES OF TASKS.  The list below has to be manually updated when new 
   task procedures are defined. */

char *Rf_task_name (helpers_task_proc *task)
{
    /* Put one line here for every task procedure used.  Names of task 
       procedures should start with "task_".  The part of the name
       after "task_" should be given as the argument to TASK_NAME below. */

#   define TASK_NAME(n) \
        do { \
            extern helpers_task_proc task_##n; \
            if (task == &task_##n) return #n; \
        } while (0)

    TASK_NAME(row_or_col);
    /* a */
    TASK_NAME(rowSums_or_rowMeans);
    TASK_NAME(colSums_or_colMeans);
    /* b */
    TASK_NAME(transpose);
    /* c */
    TASK_NAME(unary_minus);
    /* d */
    TASK_NAME(integer_arithmetic);
    TASK_NAME(real_arithmetic);
    TASK_NAME(complex_arithmetic);
    TASK_NAME(abs);
    TASK_NAME(sum_abs);
    TASK_NAME(math1);
    TASK_NAME(sum_math1);
    TASK_NAME(relop);
    TASK_NAME(relop_and);
    TASK_NAME(relop_or);
    TASK_NAME(relop_sum);
    TASK_NAME(and_or);
    TASK_NAME(not);
    /* i */
    TASK_NAME(rep);
    /* k */
    TASK_NAME(fill_zeros);
    TASK_NAME(matprod_vec_vec);
    TASK_NAME(matprod_mat_vec);
    TASK_NAME(matprod_vec_mat);
    TASK_NAME(matprod_mat_mat);
    TASK_NAME(matprod_vec_vec_BLAS);
    TASK_NAME(matprod_mat_vec_BLAS);
    TASK_NAME(matprod_vec_mat_BLAS);
    TASK_NAME(matprod_mat_mat_BLAS);
    TASK_NAME(cfill_zeros);
    TASK_NAME(cmatprod);
    TASK_NAME(par_matprod_vec_vec);
    TASK_NAME(par_matprod_mat_vec);
    TASK_NAME(par_matprod_vec_mat);
    TASK_NAME(par_matprod_outer);
    TASK_NAME(par_matprod_mat_mat);
    /* s */
    TASK_NAME(matprod_trans1);
    TASK_NAME(matprod_trans2);
    TASK_NAME(matprod_trans1_BLAS);
    TASK_NAME(matprod_trans2_BLAS);
    TASK_NAME(cmatprod_trans1);
    TASK_NAME(cmatprod_trans2);
    TASK_NAME(par_matprod_trans1);
    TASK_NAME(par_matprod_trans2);
    /* t */
    TASK_NAME(copy_coerced);
    /* v */
    /* w */
    /* x */
    /* y */
    TASK_NAME(dotCode);
    /* z */

# ifdef R_TASK_MERGING
    TASK_NAME(merged_arith_abs);
# endif

    return "?";
}


/* NAMES OF VARIABLES.  Combines type (I=integer, R=real, C=complex), length,
   and address.  Storage for the name isn't reclaimed. */

char *Rf_var_name (helpers_var_ptr var)
{
    char h[50], s[100];
    char *p;
    sprintf (h, "%llx", (long long int) var);
    sprintf (s, "%c%d:%s", TYPEOF(var)==INTSXP ? 'I'
                            : TYPEOF(var)==REALSXP ? 'R'
                            : TYPEOF(var)==CPLXSXP ? 'C' : 'X',
                           length(var), 
                           h[0]=='0' && h[1]=='x' ? h+2 : h);
    p = malloc(strlen(s)+1);
    strcpy(p,s);
    return p;
}

#endif
