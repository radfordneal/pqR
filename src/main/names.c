/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Development Core Team
 *  Copyright (C) 2003, 2004  The R Foundation
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

#define __R_Names__ /* used in Defn.h for extern on R_FunTab */
#define USE_FAST_PROTECT_MACROS
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Print.h>
#include "arithmetic.h" /* for do_math[1234], do_cmathfuns */

#include <Rinterface.h>


/* Table of .Internal(.) and .Primitive(.) R functions
 *
 * Each entry is a line with
 *
 *  printname	c-entry	 offset	 eval	arity	  pp-kind   precedence	    rightassoc
 *  ---------	-------	 ------	 ----	-----	  -------   ----------	    ----------
 *2 name	cfun	 code	 eval	arity	  gram.kind gram.precedence gram.rightassoc
 *3 PRIMNAME	PRIMFUN	 PRIMVAL [*]    PRIMARITY PPINFO    PPINFO	    PPINFO
 *
 * where "2" are the component names of the FUNTAB struct (Defn.h)
 * and	 "3" are the accessor macros. [*]: PRIMPRINT(.) uses the eval component
 *
 * printname:	The function name in R
 *
 * c-entry:	The name of the corresponding C function,
 *		actually declared in ../include/Internal.h .
 *		Convention:
 *		 - all start with "do_",
 *		 - all return SEXP.
 *		 - have argument list
 *		      (SEXP call, SEXP op, SEXP args, SEXP env)
 *                 or 
 *                    (SEXP call, SEXP op, SEXP args, SEXP env, int variant)
 *
 * offset:	the 'op' (offset pointer) above; used for C functions
 *		which deal with more than one R function...
 *
 * eval:	= VWXYZ (five digits) --- where e.g. '1' means '00001'
 *              V=1 says that the (builtin) primitive can handle operands
 *                  whose computation by a task may be pending
 *              V=0 says must not be passed operands still being computed
 *              W=1 says pass a "variant" argument to the c-entry procedure
 *                  (ignored if PPkind is PP_FOREIGN)
 *              W=0 says don't pass a "variant" argument
 *		X=1 says that we should force R_Visible off
 *		X=0 says that we should force R_Visible on
 *		X=2 says that we should switch R_Visible on but let the C
 *                  code update this.
 *		Y=1 says that this is an internal function which must
 *		    be accessed with a	.Internal(.) call, any other value is
 *		    accessible directly and printed in R as ".Primitive(..)".
 *		Z=1 says evaluate arguments before calling (BUILTINSXP) and
 *		Z=0 says don't evaluate (SPECIALSXP).
 *
 * arity:	How many arguments are required/allowed;  "-1"	meaning ``any''
 *
 * pp-kind:	Deparsing Info (-> PPkind in ../include/Defn.h )
 *
 * precedence: Operator precedence (-> PPprec in ../include/Defn.h )
 *
 * rightassoc: Right (1) or left (0) associative operator
 */


/* Entries for the FUNTAB table are defined in smaller tables in the source 
   files where the functions are defined, and copied to the table below on
   initialization.  Some entries are defined in this source file (names.c). */

#define MAX_FUNTAB_ENTRIES 1000  /* Increase as needed (may be spare slots) */

attribute_hidden FUNTAB R_FunTab[MAX_FUNTAB_ENTRIES+1]; /* terminated by NULL */

/* Pointers to the FUNTAB tables in the various source files, to be copied to
   R_FunTab.  Their names have the form R_FunTab_srcfilename. */

extern FUNTAB 
    R_FunTab_eval[], 
    R_FunTab_arithmetic[], 
    R_FunTab_complex[], 
    R_FunTab_relop[], 
    R_FunTab_logic[], 
    R_FunTab_array[], 
    R_FunTab_summary[], 
    R_FunTab_seq[], 
    R_FunTab_coerce[], 
    R_FunTab_attrib[], 
    R_FunTab_names[],

    R_FunTab_CommandLineArgs[],
    R_FunTab_RNG[],
    R_FunTab_Rdynload[],
    R_FunTab_Renviron[],
    R_FunTab_agrep[],
    R_FunTab_apply[],
    R_FunTab_bind[],
    R_FunTab_builtin[],
    R_FunTab_character[],
    R_FunTab_colors[],
    R_FunTab_connections[],
    R_FunTab_context[],
    R_FunTab_cov[],
    R_FunTab_cum[],
    R_FunTab_datetime[],
    R_FunTab_dcf[],
    R_FunTab_debug[],
    R_FunTab_deparse[],
    R_FunTab_deriv[],
    R_FunTab_devices[],
    R_FunTab_dotcode[],
    R_FunTab_dounzip[],
    R_FunTab_engine[],
    R_FunTab_envir[],
    R_FunTab_errors[],
    R_FunTab_fourier[],
    R_FunTab_gramLatex[],
    R_FunTab_gramRd[],
    R_FunTab_grep[],
    R_FunTab_identical[],
    R_FunTab_inspect[],
    R_FunTab_internet[],
    R_FunTab_list[],
    R_FunTab_main[],
    R_FunTab_memory[],
    R_FunTab_model[],
    R_FunTab_objects[],
    R_FunTab_optim[],
    R_FunTab_optimize[],
    R_FunTab_options[],
    R_FunTab_par[],
    R_FunTab_paste[],
    R_FunTab_platform[],
    R_FunTab_plot[],
    R_FunTab_plot3d[],
    R_FunTab_print[],
    R_FunTab_qsort[],
    R_FunTab_random[],
    R_FunTab_raw[],
    R_FunTab_saveload[],
    R_FunTab_scan[],
    R_FunTab_sort[],
    R_FunTab_source[],
    R_FunTab_split[],
    R_FunTab_sprintf[],
    R_FunTab_subassign[],
    R_FunTab_subset[],
    R_FunTab_sysutils[],
    R_FunTab_unique[],
    R_FunTab_util[],
    R_FunTab_version[];

static FUNTAB *FunTab_ptrs[] = { 
    R_FunTab_eval,
    R_FunTab_arithmetic, 
    R_FunTab_complex, 
    R_FunTab_relop,
    R_FunTab_logic,
    R_FunTab_array,
    R_FunTab_summary,
    R_FunTab_seq,
    R_FunTab_coerce,
    R_FunTab_attrib,
    R_FunTab_names,

    R_FunTab_CommandLineArgs,
    R_FunTab_RNG,
    R_FunTab_Rdynload,
    R_FunTab_Renviron,
    R_FunTab_agrep,
    R_FunTab_apply,
    R_FunTab_bind,
    R_FunTab_builtin,
    R_FunTab_character,
    R_FunTab_colors,
    R_FunTab_connections,
    R_FunTab_context,
    R_FunTab_cov,
    R_FunTab_cum,
    R_FunTab_datetime,
    R_FunTab_dcf,
    R_FunTab_debug,
    R_FunTab_deparse,
    R_FunTab_deriv,
    R_FunTab_devices,
    R_FunTab_dotcode,
    R_FunTab_dounzip,
    R_FunTab_engine,
    R_FunTab_envir,
    R_FunTab_errors,
    R_FunTab_fourier,
    R_FunTab_gramLatex,
    R_FunTab_gramRd,
    R_FunTab_grep,
    R_FunTab_identical,
    R_FunTab_inspect,
    R_FunTab_internet,
    R_FunTab_list,
    R_FunTab_main,
    R_FunTab_memory,
    R_FunTab_model,
    R_FunTab_objects,
    R_FunTab_optim,
    R_FunTab_optimize,
    R_FunTab_options,
    R_FunTab_par,
    R_FunTab_paste,
    R_FunTab_platform,
    R_FunTab_plot,
    R_FunTab_plot3d,
    R_FunTab_print,
    R_FunTab_qsort,
    R_FunTab_random,
    R_FunTab_raw,
    R_FunTab_saveload,
    R_FunTab_scan,
    R_FunTab_sort,
    R_FunTab_source,
    R_FunTab_split,
    R_FunTab_sprintf,
    R_FunTab_subassign,
    R_FunTab_subset,
    R_FunTab_sysutils,
    R_FunTab_unique,
    R_FunTab_util,
    R_FunTab_version,
    NULL
};


/* Tables of fast built-in functions.  These tables are found in various
   source files, with names having thee form R_FastFunTab_srcfilename. */

extern FASTFUNTAB 
    R_FastFunTab_arithmetic[],
    R_FastFunTab_complex[],
    R_FastFunTab_relop[],
    R_FastFunTab_logic[],
    R_FastFunTab_array[],
    R_FastFunTab_summary[],
    R_FastFunTab_seq[],
    R_FastFunTab_coerce[],
    R_FastFunTab_attrib[];

static FASTFUNTAB *FastFunTab_ptrs[] = { 
    R_FastFunTab_arithmetic, 
    R_FastFunTab_complex, 
    R_FastFunTab_relop,
    R_FastFunTab_logic,
    R_FastFunTab_array,
    R_FastFunTab_summary,
    R_FastFunTab_seq,
    R_FastFunTab_coerce,
    R_FastFunTab_attrib,
    NULL
};


/* FUNTAB entries defined in this source file, otherwise in non-standard way. */

attribute_hidden FUNTAB R_FunTab_names[] =
{
/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc */

{".Internal",	do_internal,	0,	1200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},

{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"class",	R_do_data_class,0,	10001,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",	R_do_data_class,	1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Note that the number of arguments in this group only applies
   to the default method */

{"machine",	do_machine,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

#ifdef Win32
{"system",	do_system,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"flush.console",do_flushconsole,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"win.version", do_winver,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"shell.exec",	do_shellexec,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"winDialog",	do_windialog,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"winDialogString", do_windialogstring, 0, 11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"winMenuNames", do_winmenunames, 0,    11,     0,      {PP_FUNCALL, PREC_FN,   0}},
{"winMenuItems", do_wingetmenuitems, 0, 11, 1, {PP_FUNCALL, PREC_FN, 0}},
{"winMenuAdd",	do_winmenuadd,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"winMenuDel",	do_winmenudel,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.size",	do_memsize,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"DLL.version",	do_dllversion,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bringToTop",	do_bringtotop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"msgWindow",	do_msgwindow,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"select.list",	do_selectlist,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"getClipboardFormats",do_getClipboardFormats,0,11,0,	{PP_FUNCALL, PREC_FN,	0}},
{"readClipboard",do_readClipboard,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"writeClipboard",do_writeClipboard,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"chooseFiles", do_chooseFiles, 0,	11,	5,	{PP_FUNCALL, PREC_FN,   0}},
{"chooseDir",	do_chooseDir,	0,	11,	2,	{PP_FUNCALL, PREC_FN,   0}},
{"getIdentification", do_getIdentification,0,11,0,	{PP_FUNCALL, PREC_FN,	0}},
{"getWindowHandle", do_getWindowHandle,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getWindowHandles",do_getWindowHandles,0,11,   2,	{PP_FUNCALL, PREC_FN,   0}},
{"getWindowTitle",do_getWindowTitle,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setWindowTitle",do_setTitle,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"arrangeWindows",do_arrangeWindows,0,  111,    4,      {PP_FUNCALL, PREC_FN,   0}},
{"setStatusBar",do_setStatusBar,0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"shortPathName",do_shortpath,	0,	11,	1,	{PP_FUNCALL, PREC_FN,   0}},
{"loadRconsole", do_loadRconsole,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"readRegistry",do_readRegistry,0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"winProgressBar",do_winprogressbar,0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"closeWinProgressBar",do_closewinprogressbar,0,111,1,	{PP_FUNCALL, PREC_FN,	0}},
{"setWinProgressBar",do_setwinprogressbar,0,11,	4,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#if defined(__APPLE_CC__) && defined(HAVE_AQUA)
{"wsbrowser",	do_wsbrowser,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"pkgbrowser",	do_browsepkgs,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"data.manager",	do_datamanger,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"package.manager",	do_packagemanger,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"flush.console",do_flushconsole,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"hsbrowser",	do_hsbrowser,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"select.list",	do_selectlist,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"aqua.custom.print", do_aqua_custom_print, 0, 11, 2,   {PP_FUNCALL, PREC_FN,   0}},
#endif

{"serializeToConn",	do_serializeToConn,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	do_unserializeFromConn,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"edit",	do_edit,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dataentry",	do_dataentry,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"dataviewer",	do_dataviewer,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"Sys.info",	do_sysinfo,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

#ifdef Unix
{"X11",		do_X11,		0,	111,	17,	{PP_FUNCALL, PREC_FN,	0}},
{"savePlot",	do_saveplot,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
#endif

{"getGraphicsEvent",do_getGraphicsEvent,0,  11, 1,      {PP_FUNCALL, PREC_FN,   0}},
{"getGraphicsEventEnv",do_getGraphicsEventEnv,0,11,1,{PP_FUNCALL, PREC_FN, 0}},
{"setGraphicsEventEnv",do_setGraphicsEventEnv,0,11,2,{PP_FUNCALL, PREC_FN, 0}},

{"loadhistory", do_loadhistory,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"savehistory", do_savehistory,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"addhistory",  do_addhistory,  0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lazyLoadDBfetch",do_lazyLoadDBfetch,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}}
};


/* Table of special names.  These are marked as special with SET_SPEC_SYM.  
   Environments that have never contained such a symbol are marked as such,
   with NO_SPEC_SYM, so they can be quickly skipped when searching for a 
   function named by such a special symbol. 

   Any symbols can be put here, but ones that contain special characters, or are
   reserved words, are the ones unlikely to be defined in any environment other
   than base, and hence the ones where this is most likely to help. */

static char *Spec_name[] = { 
  "if", "while", "repeat", "for", "break", "next", "return", "function",
  "(", "{",
  "+", "-", "*", "/", "^", "%%", "%/%", "%*%", ":",
  "==", "!=", "<", ">", "<=", ">=",
  "&", "|", "&&", "||", "!",
  "<-", "<<-", "=",
  "$", "[", "[[", 
  "$<-", "[<-", "[[<-", 
  0
};


/* also used in eval.c */
SEXP attribute_hidden R_Primitive(const char *primname)
{
    for (int i = 0; R_FunTab[i].name; i++) 
	if (strcmp(primname, R_FunTab[i].name) == 0) { /* all names are ASCII */
	    if ((R_FunTab[i].eval % 100 )/10)
		return R_NilValue; /* it is a .Internal */
	    else
		return mkPRIMSXP(i, R_FunTab[i].eval % 10);
	}
    return R_NilValue;
}

SEXP attribute_hidden do_primitive(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, prim;
    checkArity(op, args);
    name = CAR(args);
    if (!isString(name) || length(name) != 1 ||
	STRING_ELT(name, 0) == R_NilValue)
	errorcall(call, _("string argument required"));
    prim = R_Primitive(CHAR(STRING_ELT(name, 0)));
    if (prim == R_NilValue)
	errorcall(call, _("no such primitive function"));
    return prim;
}

int StrToInternal(const char *s)
{
    int i;
    for (i = 0; R_FunTab[i].name; i++)
	if (strcmp(s, R_FunTab[i].name) == 0) return i;
    return 0;
}

static void SymbolShortcuts(void)
{  /* ../include/Rinternals.h : */
    R_Bracket2Symbol = install("[[");
    R_BracketSymbol = install("[");
    R_BraceSymbol = install("{");
    R_ClassSymbol = install("class");
    R_DeviceSymbol = install(".Device");
    R_DimNamesSymbol = install("dimnames");
    R_DimSymbol = install("dim");
    R_DollarSymbol = install("$");
    R_DotsSymbol = install("...");
    R_DropSymbol = install("drop");
    R_LastvalueSymbol = install(".Last.value");
    R_LevelsSymbol = install("levels");
    R_ModeSymbol = install("mode");
    R_NameSymbol  = install("name");
    R_NamesSymbol = install("names");
    R_NaRmSymbol = install("na.rm");
    R_xSymbol = install("x");
    R_PackageSymbol = install("package");
    R_QuoteSymbol = install("quote");
    R_RowNamesSymbol = install("row.names");
    R_SeedsSymbol = install(".Random.seed");
    R_SourceSymbol = install("source");   /* Still present for back compatibility, but not used */
    R_TspSymbol = install("tsp");
    R_ValueSymbol = install("value");
    R_AssignSymbols[1] = install("<-");
    R_AssignSymbols[2] = install("<<-");
    R_AssignSymbols[3] = install("=");
    R_SubAssignSymbol = install("[<-");
    R_SubSubAssignSymbol = install("[[<-");
    R_DollarAssignSymbol = install("$<-");
    /* ../include/Defn.h , i.e. non-public : */
    R_CommentSymbol = install("comment");
    R_DotEnvSymbol = install(".Environment");
    R_ExactSymbol = install("exact");
    R_RecursiveSymbol = install("recursive");
    R_SrcfileSymbol = install("srcfile");
    R_SrcrefSymbol = install("srcref");
    R_WholeSrcrefSymbol = install("wholeSrcref");
    R_TmpvalSymbol = install("*tmp*");
    R_UseNamesSymbol = install("use.names");
    R_DoubleColonSymbol = install("::");
    R_TripleColonSymbol = install(":::");
    R_ConnIdSymbol = install("conn_id");
    R_DevicesSymbol = install(".Devices");

    R_dot_Generic = install(".Generic");
    R_dot_Method = install(".Method");
    R_dot_Methods = install(".Methods");
    R_dot_defined = install(".defined");
    R_dot_target = install(".target");
    R_dot_Group = install(".Group");
    R_dot_Class = install(".Class");
    R_dot_GenericCallEnv = install(".GenericCallEnv");
    R_dot_GenericDefEnv = install(".GenericDefEnv");

    R_NaokSymbol = install("NAOK");
    R_DupSymbol = install("DUP");
    R_PkgSymbol = install("PACKAGE");
    R_EncSymbol = install("ENCODING");
    R_CSingSymbol = install("Csingle");
    R_NativeSymbolSymbol = install("native symbol");
    R_RegisteredNativeSymbolSymbol = install("registered native symbol");
}

/* Set up built-in functions from R_FunTab and the FastFunTab_srcfile tables.
   The R_FunTab tables is created from smaller tables in the various source 
   files.  Also sets the SPEC_SYM flag for those functions that are unlikely 
   to be redefined outside "base". */

static void SetupBuiltins(void)
{
    int i, j, k;

    /* Combine tables in various source files into one table here. */

    i = 0;

    for (j = 0; FunTab_ptrs[j]!=NULL; j++) {
        for (k = 0; FunTab_ptrs[j][k].name!=NULL; k++) {
            if (i > MAX_FUNTAB_ENTRIES) {
                REprintf(
                  "Too many builtin functions - increase MAX_FUNTAB_ENTRIES\n");
                exit(1);
            }
            R_FunTab[i++] = FunTab_ptrs[j][k];
        }
    }

    R_FunTab[i].name = NULL;

    /* Install the primitive and internal functions.  Look for fast versions
       of each one. */

    for (i = 0; R_FunTab[i].name!=NULL; i++) {
        SEXP (*this_cfun)() = R_FunTab[i].cfun;
        int this_code = R_FunTab[i].code;
        SEXP prim;
        /* prim needs protect since install can (and does here) allocate */
        PROTECT(prim = mkPRIMSXP(i, R_FunTab[i].eval % 10));
        if ((R_FunTab[i].eval % 100 )/10)
            SET_INTERNAL(install(R_FunTab[i].name), prim);
        else
            SET_SYMVALUE(install(R_FunTab[i].name), prim);
        for (j = 0; FastFunTab_ptrs[j]!=NULL; j++) {
            for (k = 0; FastFunTab_ptrs[j][k].slow!=0; k++) {
                FASTFUNTAB *f = &FastFunTab_ptrs[j][k];
                if (f->slow==this_cfun && (f->code==-1 || f->code==this_code)) {
                    if (f->arity==1)
                        SET_PRIMFUN_FAST_UNARY(prim,f->fast,f->dsptch1,f->var1);
                    else
                        SET_PRIMFUN_FAST_BINARY(prim,f->fast,
                           f->dsptch1,f->dsptch2,f->var1,f->var2,f->arity==3);
                    goto found;
                }
            }
        }
      found:
        UNPROTECT(1);
    }

    /* Flag "special" symbols. */

    for (i = 0; Spec_name[i]; i++)
        SET_SPEC_SYM (install(Spec_name[i]), 1);
}

extern SEXP framenames; /* from model.c */

/* initialize the symbol table */
void InitNames()
{
    /* allocate the symbol table */
    if (!(R_SymbolTable = (SEXP *) calloc(HSIZE, sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for symbol table");

    /* R_UnboundValue */
    R_UnboundValue = allocSExp(SYMSXP);
    SET_SYMVALUE(R_UnboundValue, R_UnboundValue);
    SET_PRINTNAME(R_UnboundValue, R_NilValue);
    /* R_MissingArg */
    R_MissingArg = allocSExp(SYMSXP);
    SET_SYMVALUE(R_MissingArg, R_MissingArg);
    SET_PRINTNAME(R_MissingArg, mkChar(""));
    /* R_RestartToken */
    R_RestartToken = allocSExp(SYMSXP);
    SET_SYMVALUE(R_RestartToken, R_RestartToken);
    SET_PRINTNAME(R_RestartToken, mkChar(""));

    /* String constants (CHARSXP values) */
    /* Note: we don't want NA_STRING to be in the CHARSXP cache, so that
       mkChar("NA") is distinct from NA_STRING */
    /* NA_STRING */
    NA_STRING = allocCharsxp(strlen("NA"));
    strcpy(CHAR_RW(NA_STRING), "NA");
    SET_CACHED(NA_STRING);  /* Mark it */
    R_print.na_string = NA_STRING;
    /* R_BlankString */
    R_BlankString = mkChar("");

    /* Initialize the symbol Table */
    for (int i = 0; i < HSIZE; i++) R_SymbolTable[i] = R_NilValue;

    /* Set up a set of globals so that a symbol table search can be
       avoided when matching something like dim or dimnames. */
    SymbolShortcuts();

    /* Set up built-in functions. */
    SetupBuiltins();

    framenames = R_NilValue;

    R_initialize_bcode();
}


/*  install - probe the symbol table */
/*  If "name" is not found, it is installed in the symbol table.
    The symbol corresponding to the string "name" is returned. */

SEXP install(const char *name)
{
    SEXP sym;
    int i, hashcode;

#if 0  /* Enable for tuning info */
    if (strcmp(name,"HOWMANYSYMBOLS")==0) {
        double count = 0, count_sq = 0;
        for (i = 0; i<HSIZE; i++) {
            int c = 0;
            for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym)) c++;
            count += c;
            count_sq += c*c;
        }
        Rprintf("Number of symbols in table: %f\n", count);
        Rprintf("Expected number in accessed bucket: %f\n", count_sq/count);
    }
#endif

    if (*name == '\0')
	error(_("attempt to use zero-length variable name"));

    hashcode = R_Newhashpjw(name);
    i = hashcode % HSIZE;

    /* Check to see if the symbol is already present;  if it is, return it. */
    for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym)) {
        const char *s = CHAR(PRINTNAME(CAR(sym)));
	if (name[0] == s[0] /* quick pre-check */ && strcmp(name,s) == 0)
            return (CAR(sym));
    }

    /* Create a new symbol node and link it into the table. */
    if (strlen(name) > MAXIDSIZE)
	error(_("variable names are limited to %d bytes"), MAXIDSIZE);
    sym = mkSYMSXP(mkChar(name), R_UnboundValue);
    SET_HASHVALUE(PRINTNAME(sym), hashcode);
    SET_HASHASH(PRINTNAME(sym), 1);

    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return (sym);
}


/*  do_internal - This is the code for .Internal(). */

SEXP attribute_hidden do_internal (SEXP call, SEXP op, SEXP args, SEXP env,
                                   int variant)
{
    SEXP s, fun, ifun, ans;
    int save = R_PPStackTop;
    const void *vmax = VMAXGET();

    checkArity(op, args);
    s = CAR(args);
    if (!isPairList(s))
	errorcall(call, _("invalid .Internal() argument"));
    fun = CAR(s);
    if (!isSymbol(fun))
	errorcall(call, _("invalid internal function"));
    ifun = INTERNAL(fun);
    if (ifun == R_NilValue)
	errorcall(call, _("no internal function \"%s\""),
		  CHAR(PRINTNAME(fun)));

    args = CDR(s);
    if (TYPEOF(ifun) == BUILTINSXP) {
	args = PRIMFUN_PENDING_OK(ifun) ? evalListPendingOK (args, env, call)
                                        : evalList (args, env, call);
    }
    PROTECT(args);

    R_Visible = TRUE;

    ans = CALL_PRIMFUN(s, ifun, args, env, variant);

    int flag = PRIMPRINT(ifun);
    if (flag == 0) R_Visible = TRUE;
    else if (flag == 1) R_Visible = FALSE;

    UNPROTECT(1);
    check_stack_balance(ifun, save);
    VMAXSET(vmax);
    return (ans);
}
#undef __R_Names__
