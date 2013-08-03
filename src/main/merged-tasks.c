/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Development Core Team.
 *  Copyright (C) 2003-4	    The R Foundation
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

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif
#define USE_FAST_PROTECT_MACROS

#include <complex.h>
#include "Defn.h"		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("Non-numeric argument to mathematical function")

#include <Rmath.h>
extern double Rf_gamma_cody(double);

#include "arithmetic.h"

#include <errno.h>

#include <helpers/helpers-app.h>

extern double (*math1_func_table[44])(double);
extern char math1_err_table[44];


/* MERGED TASK FOR MATH1 FUNCTIONS.  Initial unsophisticated version handling
   only two functions, to try things out. */

void task_math1_merged (helpers_op_t opcode, SEXP sy, SEXP sa, SEXP call)
{
    double *ra = REAL(sa);
    double *ry = REAL(sy);
    R_len_t n = LENGTH(sa);
    R_len_t i = 0;
    R_len_t a;

    int op1 = (opcode>>6) & 0x3f;
    int op2 = opcode & 0x3f;

    double (*f1)(double) = math1_func_table[op1];
    double (*f2)(double) = math1_func_table[op2];

    HELPERS_SETUP_OUT(5);

    if (math1_err_table[op2]==0) {
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else
                    ry[i] = f2(f1(ra[i]));
                HELPERS_NEXT_OUT(i);
            } while (i < a);
        }
    }
    else {
        int naflag = 0;
        while (i < n) {
            HELPERS_WAIT_IN1 (a, i, n);
            do {
                if (ISNAN(ra[i]))
                    ry[i] = ra[i];
                else {
                    ry[i] = f2(f1(ra[i]));
                    if (ISNAN(ry[i])) naflag = 1;
                }
                HELPERS_NEXT_OUT(i);
            } while (i < a);
        }
        /* Warning below is only possible if this is being done in master */
        if (naflag) warningcall(call, R_MSG_NA);
    }
}
