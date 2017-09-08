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
   merge-sort.h again, to make another sort procedure. 

   The algorithm used can be visualized recursively, though the
   recursion is not explicit in the code.  The operation can be viewed
   in terms of halves of src and dst, as follows:

           src = [ A | B ]     SORT->      dst = [ X | Y ]

   This is implemented as follows:

                A      SORT->    Y      (A destroyed)
                B      SORT->    A      (B destroyed)
               A,Y    MERGE->   dst
	       
   If src is of odd length, A is one bigger than B, and Y is one
   bigger than X.  The merge of A and Y will overwrite Y, but a write
   to an element of Y will happen only after that element is no longer
   needed. */

#define DEBUG 0  /* If non-zero, produce debug output to that many lines max */

static void merge_sort (merge_value *dst, merge_value *src, int n)
{
#   if DEBUG
        int count = 1;
        REprintf("src: %p, dst: %p, n: %d\n",src,dst,n);
#   endif

    if (n == 0)
        return;

    if (n == 1) {
        *dst = *src;
        return;
    }

    void *vmax = VMAXGET();

    /* Set up array of tasks to do, mimicking recursion. 50 should be enough. */

    struct { 
        merge_value *src;  /* Pointer to first element in source array */
        merge_value *dst;  /* Pointer to first element in destination array */
        int n1;            /* Number of elements in first half of source */
        int n2;            /* Number of elements in second half of source */
        int fhs;           /* Has first half been sorted? */
    } todo[50]; 

    int top;               /* Top value in todo */
    int n1, n2, fhs;       /* These mirror values in todo[top] */

    top = 0;
    todo[0].src = src;
    todo[0].dst = dst;
    n1 = todo[0].n1 = (n + 1) / 2;
    n2 = todo[0].n2 = n - todo[0].n1;
    fhs = todo[0].fhs = 0;

    /* Do and create tasks until original task is done. */

    for (;;) {

        if (!fhs) {

           /* Create tasks to sort first half, first half of first half, etc.,
              until this reaches the trivial point of sorting one element. */

            while (n1 > 1) {
                todo[top+1].n1 = (n1 + 1) / 2;
                todo[top+1].n2 = n1 - todo[top+1].n1;
                todo[top+1].src = todo[top].src;
                todo[top+1].dst = todo[top].dst + n2;
                todo[top+1].fhs = 0;
                n1 = todo[top+1].n1;
                n2 = todo[top+1].n2;
                top += 1;
            }

            /* Do the trivial operation of sorting a one-element first half. */

#           if DEBUG
                if (count++ < DEBUG)
                    REprintf("A: %p/%d -> %p/%d\n",
                             todo[top].src,*todo[top].src,
                             (todo[top].dst+n2),*(todo[top].dst+n2));
#           endif

            *(todo[top].dst+n2) = *todo[top].src;
            fhs = todo[top].fhs = 1;
        }

        /* Top task will have its first half sorted.  Create a task to sort the 
           second half, if this is not trivial. */

        if (n2 > 1) {
            todo[top+1].n1 = (n2 + 1) / 2;
            todo[top+1].n2 = n2 - todo[top+1].n1;
            todo[top+1].src = todo[top].src + n1;
            todo[top+1].dst = todo[top].src;
            n1 = todo[top+1].n1;
            n2 = todo[top+1].n2;
            fhs = todo[top+1].fhs = 0;
            top += 1;
            continue;
        }

        /* Do the trivial operation of sorting a one-element second half. */

#       if DEBUG
            if (count++ < DEBUG)
                REprintf("B: %p/%d -> %p/%d\n",
                         (todo[top].src+n1),*(todo[top].src+n1),
                         todo[top].src,*todo[top].src);
#       endif

        *todo[top].src = *(todo[top].src+n1);

        /* After a second half has been sorted, it will be possible to merge
           it with the first half.  If the merged result was a second half,
           it also can be merged, etc. */

        do {

            /* Merge first and second halves. */

            merge_value *i1 = todo[top].dst + n2;
            merge_value *i2 = todo[top].src;
            merge_value *j = todo[top].dst;

            do {
                if (n1 == 0 || n2 != 0 && merge_greater (*i1, *i2)) {
                    *j++ = *i2++;
                    n2 -= 1;
                }
                else {
                    *j++ = *i1++;
                    n1 -= 1;
                }
#               if DEBUG
                    if (count++ < DEBUG)
                        REprintf("C: %p = %d, n1 %d, n2 %d\n",j-1,*(j-1),n1,n2);
#               endif
            } while (n1 + n2 > 0);

            /* Check whether everything is now done! */

            if (top == 0) {
                VMAXSET(vmax);
                return;
            }

            /* Go on to the next task. */

            top -= 1;        
            n1 = todo[top].n1;
            n2 = todo[top].n2;
            fhs = todo[top].fhs;

        } while (fhs);

        fhs = todo[top].fhs = 1;
    }

    VMAXSET(vmax);
}
