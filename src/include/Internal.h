/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2013, 2014 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2009  The R Development Core Team
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

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H


/* Device drivers here (for ease of access) */

SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveplot(SEXP, SEXP, SEXP, SEXP);

#if defined(__APPLE_CC__) && defined(HAVE_AQUA)
SEXP do_wsbrowser(SEXP, SEXP, SEXP, SEXP);
SEXP do_browsepkgs(SEXP, SEXP, SEXP, SEXP);
SEXP do_datamanger(SEXP, SEXP, SEXP, SEXP);
SEXP do_packagemanger(SEXP, SEXP, SEXP, SEXP);
SEXP do_flushconsole(SEXP, SEXP, SEXP, SEXP);
SEXP do_hsbrowser(SEXP, SEXP, SEXP, SEXP);
SEXP do_selectlist(SEXP, SEXP, SEXP, SEXP);
SEXP do_aqua_custom_print(SEXP, SEXP, SEXP, SEXP);
#endif


/* Function Names */

#if Win32
SEXP do_arrangeWindows(SEXP, SEXP, SEXP, SEXP);
SEXP do_bringtotop(SEXP, SEXP, SEXP, SEXP);
SEXP do_chooseFiles(SEXP, SEXP, SEXP, SEXP);
SEXP do_chooseDir(SEXP, SEXP, SEXP, SEXP);
SEXP do_dllversion(SEXP, SEXP, SEXP, SEXP);
SEXP do_flushconsole(SEXP, SEXP, SEXP, SEXP);
SEXP do_getClipboardFormats(SEXP, SEXP, SEXP, SEXP);
SEXP do_getIdentification(SEXP, SEXP, SEXP, SEXP);
SEXP do_getWindowHandle(SEXP, SEXP, SEXP, SEXP);
SEXP do_getWindowHandles(SEXP, SEXP, SEXP, SEXP);
SEXP do_getWindowTitle(SEXP, SEXP, SEXP, SEXP);
SEXP do_loadRconsole(SEXP, SEXP, SEXP, SEXP);
SEXP do_memsize(SEXP, SEXP, SEXP, SEXP);
SEXP do_msgwindow(SEXP, SEXP, SEXP, SEXP);
SEXP do_readClipboard(SEXP, SEXP, SEXP, SEXP);
SEXP do_readRegistry(SEXP, SEXP, SEXP, SEXP);
SEXP do_selectlist(SEXP, SEXP, SEXP, SEXP);
SEXP do_setTitle(SEXP, SEXP, SEXP, SEXP);
SEXP do_setStatusBar(SEXP, SEXP, SEXP, SEXP);
SEXP do_shellexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_shortpath(SEXP, SEXP, SEXP, SEXP);
SEXP do_syswhich(SEXP, SEXP, SEXP, SEXP);
SEXP do_windialog(SEXP, SEXP, SEXP, SEXP);
SEXP do_windialogstring(SEXP, SEXP, SEXP, SEXP);
SEXP do_winmenuadd(SEXP, SEXP, SEXP, SEXP);
SEXP do_winmenudel(SEXP, SEXP, SEXP, SEXP);
SEXP do_winmenunames(SEXP, SEXP, SEXP, SEXP);
SEXP do_wingetmenuitems(SEXP, SEXP, SEXP, SEXP);
SEXP do_winver(SEXP, SEXP, SEXP, SEXP);
SEXP do_writeClipboard(SEXP, SEXP, SEXP, SEXP);
SEXP do_winprogressbar(SEXP, SEXP, SEXP, SEXP);
SEXP do_closewinprogressbar(SEXP, SEXP, SEXP, SEXP);
SEXP do_setwinprogressbar(SEXP, SEXP, SEXP, SEXP);
#endif

SEXP do_addhistory(SEXP, SEXP, SEXP, SEXP);
SEXP do_anydf(SEXP, SEXP, SEXP, SEXP);
SEXP do_asmatrixdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_AT_assign(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_dataentry(SEXP, SEXP, SEXP, SEXP);
SEXP do_dataframe(SEXP, SEXP, SEXP, SEXP);
SEXP do_dataviewer(SEXP, SEXP, SEXP, SEXP);
SEXP do_dumpb(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);
SEXP do_flatContour(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEvent(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_internal(SEXP, SEXP, SEXP, SEXP, int);
SEXP do_lazyLoadDBfetch(SEXP, SEXP, SEXP, SEXP);
SEXP do_libfixup(SEXP, SEXP, SEXP, SEXP);
SEXP do_loadhistory(SEXP, SEXP, SEXP, SEXP);
SEXP do_machine(SEXP, SEXP, SEXP, SEXP);
SEXP do_pack(SEXP, SEXP, SEXP, SEXP);
SEXP do_pause(SEXP, SEXP, SEXP, SEXP);
SEXP do_primitive(SEXP, SEXP, SEXP, SEXP);
SEXP do_printdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_fast_relop(SEXP, SEXP, SEXP, SEXP, SEXP, int);
SEXP do_restoreb(SEXP, SEXP, SEXP, SEXP);
SEXP do_rownames(SEXP, SEXP, SEXP, SEXP);
SEXP do_savehistory(SEXP, SEXP, SEXP, SEXP);
SEXP do_serializeToConn(SEXP, SEXP, SEXP, SEXP);
SEXP do_setGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_shade(SEXP, SEXP, SEXP, SEXP);
SEXP do_sysinfo(SEXP,SEXP,SEXP,SEXP);
SEXP do_syssleep(SEXP,SEXP,SEXP,SEXP);
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP, int);
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP, int);
SEXP do_subassigndf(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassigndf2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subsetdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_subsetdf2(SEXP, SEXP, SEXP, SEXP);
SEXP do_surface(SEXP, SEXP, SEXP, SEXP);
SEXP do_system(SEXP, SEXP, SEXP, SEXP);
SEXP do_unserializeFromConn(SEXP, SEXP, SEXP, SEXP);


SEXP R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* Connections */
SEXP do_readlines(SEXP, SEXP, SEXP, SEXP);



SEXP R_unary(SEXP, SEXP, SEXP, int);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP, int);

#endif /* not R_INTERNAL_H */
