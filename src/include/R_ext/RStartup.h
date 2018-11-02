/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2018 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2010  The R Core Team
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

#ifndef R_EXT_RSTARTUP_H_
#define R_EXT_RSTARTUP_H_

#include <R_ext/Boolean.h>	/* TRUE/FALSE */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef Win32
typedef int (*blah1) (const char *, char *, int, int);
typedef void (*blah2) (const char *, int);
typedef void (*blah3) (void);
typedef void (*blah4) (const char *);
/* Return value here is expected to be 1 for Yes, -1 for No and 0 for Cancel:
   symbolic constants in graphapp.h */
typedef int (*blah5) (const char *);
typedef void (*blah6) (int);
typedef void (*blah7) (const char *, int, int);
typedef enum {RGui, RTerm, LinkDLL} UImode;
#endif

/* Startup Actions */
typedef enum {
    SA_NORESTORE,/* = 0 */
    SA_RESTORE,
    SA_DEFAULT,/* was === SA_RESTORE */
    SA_NOSAVE,
    SA_SAVE,
    SA_SAVEASK,
    SA_SUICIDE
} SA_TYPE;

/* The following structure is used externally (eg, in RStudio) and hence
   must only be extended in a way that does not change field offsets and
   does not change the size, if such external uses are to remain valid
   without them being recompiled with the new version of this header file.
   This is platform dependent, of course, but the addition of R_PeakForElse
   has been done so that it will work at least for 32 and 64 bit Intel/AMD 
   processors with gcc/clang. */

typedef struct
{
    Rboolean R_Quiet;
    Rboolean R_Slave;
    Rboolean R_Interactive;
    Rboolean R_Verbose;
    Rboolean LoadSiteFile;
    Rboolean LoadInitFile;
    Rboolean DebugInitFile;
    SA_TYPE RestoreAction;
    SA_TYPE SaveAction;
    size_t vsize;
    size_t nsize;
    size_t max_vsize;
    size_t max_nsize;
    size_t ppsize;
    int NoRenviron;

#ifdef Win32
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */
    blah1 ReadConsole;
    blah2 WriteConsole;
    blah3 CallBack;
    blah4 ShowMessage;
    blah5 YesNoCancel;
    blah6 Busy;
    UImode CharacterMode;

#if SIZEOF_CHAR_P == 8
    Rboolean R_PeekForElse;    /* should fit in padding present anyway due to
                                  next item being a pointer */
#endif

    blah7 WriteConsoleEx;      /* used only if WriteConsole is NULL */

#if SIZEOF_CHAR_P == 4
    Rboolean R_PeekForElse;    /* 26th field, hope struct size is padded to
                                  multiple of 8 bytes so it will hold it even
                                  if it's absent in external declaration (as
                                  gcc & clang seem to do) */ 
#endif

#else  /* not Win32 */

    Rboolean R_PeekForElse;    /* 32-bit: 16th field, hope struct size padded
                                          to multiple of 8 bytes so it will
                                          hold it even if not declared (as 
                                          gcc & clang seem to do) */
                               /* 64-bit: second 32-bit item after size_t item,
                                          should fit in padding that will be 
                                          present even if it is not declared */
#endif

} structRstart;

typedef structRstart *Rstart;

void R_DefParams(Rstart);
void R_SetParams(Rstart);
void R_SetWin32(Rstart);
void R_SizeFromEnv(Rstart);
void R_common_command_line(int *, char **, Rstart);

void R_set_command_line_arguments(int argc, char **argv);

void setup_Rmainloop(void);

#ifdef __cplusplus
}
#endif

#endif
