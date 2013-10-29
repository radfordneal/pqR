/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012  The R Development Core Team
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
# include <config.h>
#endif

#define USE_FAST_PROTECT_MACROS
#include "Defn.h"
#include <Rversion.h>
#include <helpers/helpers-app.h>

void attribute_hidden PrintGreeting(void)
{
    char buf[2000];

    Rprintf("\n");
    PrintVersion_part_1(buf);
    Rprintf("%s", buf);

    if (helpers_are_disabled)
        Rprintf (_("Deferred evaluation disabled (no helpers, no merging).\n"));
    else if (helpers_not_multithreading && helpers_not_merging)
        Rprintf (_("No helper threads, no task merging.\n"));
    else if (helpers_not_multithreading)
        Rprintf (_("Task merging enabled, no helper threads.\n"));
    else if (helpers_not_merging)
        Rprintf (_("%d helper threads, no task merging.\n"), helpers_num);
    else
        Rprintf (_("%d helper threads, task merging enabled.\n"), helpers_num);

    Rprintf("\n");

    Rprintf(_("R is free software and comes with ABSOLUTELY NO WARRANTY.\n\
You are welcome to redistribute it under certain conditions.\n\
Type 'license()' or 'licence()' for distribution details.\n\n"));
    Rprintf(_("R is a collaborative project with many contributors.\n\
Type 'contributors()' for more information and\n\
'citation()' on how to cite R or R packages in publications.\n\n"));
    Rprintf(_("Type 'demo()' for some demos, 'help()' for on-line help, or\n\
'help.start()' for an HTML browser interface to help.\n\
Type 'q()' to quit R.\n\n"));
}

SEXP attribute_hidden do_version(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP value, names;
    char buf[200];

    checkArity(op, args);

    PROTECT(value = allocVector(VECSXP,17));
    PROTECT(names = allocVector(STRSXP,17));

    SET_STRING_ELT(names, 0, mkChar("platform"));
    SET_VECTOR_ELT(value, 0, mkString(R_PLATFORM));
    SET_STRING_ELT(names, 1, mkChar("arch"));
    SET_VECTOR_ELT(value, 1, mkString(R_CPU));
    SET_STRING_ELT(names, 2, mkChar("os"));
    SET_VECTOR_ELT(value, 2, mkString(R_OS));

    sprintf(buf,"%s, %s", R_CPU, R_OS);
    SET_STRING_ELT(names, 3, mkChar("system"));
    SET_VECTOR_ELT(value, 3, mkString(buf));

    SET_STRING_ELT(names, 4, mkChar("status"));
    SET_VECTOR_ELT(value, 4, mkString(R_STATUS));
    SET_STRING_ELT(names, 5, mkChar("major"));
    SET_VECTOR_ELT(value, 5, mkString(R_MAJOR));
    SET_STRING_ELT(names, 6, mkChar("minor"));
    SET_VECTOR_ELT(value, 6, mkString(R_MINOR));
    SET_STRING_ELT(names, 7, mkChar("year"));
    SET_VECTOR_ELT(value, 7, mkString(R_YEAR));
    SET_STRING_ELT(names, 8, mkChar("month"));
    SET_VECTOR_ELT(value, 8, mkString(R_MONTH));
    SET_STRING_ELT(names, 9, mkChar("day"));
    SET_VECTOR_ELT(value, 9, mkString(R_DAY));
    SET_STRING_ELT(names, 10, mkChar("svn rev"));
    SET_VECTOR_ELT(value, 10, mkString(R_SVN_REVISION));
    SET_STRING_ELT(names, 11, mkChar("language"));
    SET_VECTOR_ELT(value, 11, mkString("R"));

    PrintVersionString(buf);
    SET_STRING_ELT(names, 12, mkChar("version.string"));
    SET_VECTOR_ELT(value, 12, mkString(buf));
    SET_STRING_ELT(names, 13, mkChar("nickname"));
    SET_VECTOR_ELT(value, 13, mkString(R_NICK));

    SET_STRING_ELT(names, 14, mkChar("pqR.base.version"));
    SET_VECTOR_ELT(value, 14, mkString(pqR_BASE_VERSION));
    SET_STRING_ELT(names, 15, mkChar("pqR.base.date"));
    SET_VECTOR_ELT(value, 15, mkString(pqR_BASE_DATE));
    SET_STRING_ELT(names, 16, mkChar("pqR.base.year"));
    SET_VECTOR_ELT(value, 16, mkString(pqR_BASE_YEAR));

    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}

void attribute_hidden PrintVersion(char *s)
{
    PrintVersion_part_1(s);

    strcat(s, "\n"
	   "R is free software and comes with ABSOLUTELY NO WARRANTY.\n"
	   "You are welcome to redistribute it under the terms of the\n"
	   "GNU General Public License versions 2 or 3.\n"
	   "For more information about these matters see\n"
	   "http://www.gnu.org/licenses/.\n");
}

void attribute_hidden PrintVersionString(char *s)
{
    if(strcmp(R_SVN_REVISION, "unknown") == 0) {
	sprintf(s, "R version %s.%s %s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY);
    } else if(strlen(R_STATUS) == 0) {
	sprintf(s, "R version %s.%s (%s-%s-%s)",
		R_MAJOR, R_MINOR, R_YEAR, R_MONTH, R_DAY);
    } else if(strcmp(R_STATUS, "Under development (unstable)") == 0) {
	sprintf(s, "R %s (%s-%s-%s r%s)",
		R_STATUS, R_YEAR, R_MONTH, R_DAY, R_SVN_REVISION);
    } else {
	sprintf(s, "R version %s.%s %s (%s-%s-%s r%s)",
		R_MAJOR, R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY,
		R_SVN_REVISION);
    }
}

void attribute_hidden PrintVersion_part_1(char *s)
{
    char tmp[200];

    strcpy(s,"pq"); 
    PrintVersionString(tmp); 
    strcat(s,tmp);

    sprintf(tmp, ", based on R %s (%s)\n\n", pqR_BASE_VERSION, pqR_BASE_DATE);
    strcat(s,tmp);

    sprintf (tmp, 
      "R is Copyright (C) %s The R Foundation for Statistical Computing\n", 
      pqR_BASE_YEAR);
    strcat(s,tmp);

    strcat (s, "ISBN 3-900051-07-0\n\n");

    sprintf (tmp, 
      "Modifications to R in pqR are Copyright (C) %s Radford M. Neal\n\n",
      R_YEAR);
    strcat(s,tmp);

    sprintf (tmp, "Platform: %s", R_PLATFORM);
    strcat(s,tmp);

    if (*R_ARCH) { sprintf(tmp,"/%s",R_ARCH); strcat(s,tmp); }

    sprintf (tmp, " (%d-bit)\n", 8*(int)sizeof(void *));
    strcat(s,tmp);
}
