/*
 *  pqR : A pretty quick version of R
 *  Copyright (C) 2017 by Radford M. Neal
 *
 *  Based on R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2011	    The R Core Team.
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


/* Template for a merge sort procedure, which sorts 'n' items in
   'src', storing the sorted list in 'dst'.  The data in 'src' may be
   destroyed during the sort.  The sort is stable.

   To avoid name conflicts, 'merge_sort' should be defined to
   something appropriate before including merge-sort.h.  A symbol
   'merge_value' must be defined, which is the type of an item value,
   or of an index to an item.  A macro 'merge_gtr' must also be
   defined, which will be called as merge_greater(v1,v2), where 'v1'
   and 'v2' are of type 'merge_value'.  

   These symbols can all be undefined, then redefined before including
   merge-sort.h again, to make another sort procedure. */

static void merge_sort (merge_value *dst, merge_value *src, int n)
{
    if (n == 0)
        return;

    if (n == 1) {
        *dst = *src;
        return;
    }

    void *vmax = VMAXGET();

    /* Set up array of tasks to do, mimicking recursion. 50 should be enough. */

    struct { int len, stp; merge_value *src, *dst; } todo[50]; 
    int top;

    todo[0].src = src;
    todo[0].dst = dst;
    todo[0].len = n;
    todo[0].stp = 0;
    top = 0;
#if 0
REprintf("merge: %d %p %p\n",n,ti,indx);
#endif
    /* Do tasks until original task is done. */

    for (;;) {
#if 0
REprintf("step: %d %d %d %p %p\n",
top,todo[top].len,todo[top].stp,todo[top].src,todo[top].dst);
for (int i = 0; i < n; i++) REprintf(" %d",ti[i]); REprintf("\n");
for (int i = 0; i < n; i++) REprintf(" %d",indx[i]); REprintf("\n");
#endif
        int n1 = (todo[top].len + 1) / 2;
        int n2 = todo[top].len - n1;

        switch (todo[top].stp) {
        case 0: {
            todo[top].stp = 1;
            if (n1 == 1) {
                *(todo[top].dst+n2) = *todo[top].src;
                break;
            }
            todo[top+1].len = n1;
            todo[top+1].stp = 0;
            todo[top+1].src = todo[top].src;
            todo[top+1].dst = todo[top].dst + n2;
            top += 1;
            break;
        }
        case 1: {
            todo[top].stp = 2;
            if (n2 == 1) {
                *todo[top].src = *(todo[top].src+n1);
                break;
            }
            todo[top+1].len = n2;
            todo[top+1].stp = 0;
            todo[top+1].src = todo[top].src + n1;
            todo[top+1].dst = todo[top].src;
            top += 1;
            break;
        }
        case 2: {
            merge_value *i1 = todo[top].dst + n2;
            merge_value *i2 = todo[top].src;
            merge_value *j = todo[top].dst;
            while (n1 + n2 > 0) {
                if (n1 == 0 || n2 != 0 && merge_greater (*i1, *i2)) {
                    *j++ = *i2++;
                    n2 -= 1;
                }
                else {
                    *j++ = *i1++;
                    n1 -= 1;
                }
            }
#if 0
REprintf("after merge:\n");
for (int i = 0; i < n; i++) REprintf(" %d",ti[i]); REprintf("\n");
for (int i = 0; i < n; i++) REprintf(" %d",indx[i]); REprintf("\n");
#endif
            if (top == 0) {
                VMAXSET(vmax);
                return;
            }
            top -= 1;
            break;
        }}
    }
}
