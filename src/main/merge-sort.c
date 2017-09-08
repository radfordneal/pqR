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

static void merge_sort (merge_value *dst, merge_value *src, int n)
{
    if (n == 0)
        return;

    if (n == 1) {
        *dst = *src;
        return;
    }

    /* Set up array of tasks to do, mimicking recursion. 50 should be enough. */

    struct todo { 
        merge_value *src;  /* Pointer to first element in source array */
        merge_value *dst;  /* Pointer to first element in destination array */
        int n1;            /* Number of elements in first half of source */
        int n2;            /* Number of elements in second half of source */
        int fhs;           /* Has first half been sorted? */
    } todo[50]; 

    struct todo *top;      /* Top value in todo */

    top = todo;
    top[0].src = src;
    top[0].dst = dst;
    top[0].n1 = (n + 1) / 2;
    top[0].n2 = n - top[0].n1;
    top[0].fhs = 0;

    /* Do and create tasks until original task is done. */

    goto enter;

    for (;;) {

        /* At this point, the top task will have its first half sorted. */

        if (top[0].n2 > 2) {

            /* Create a task to sort the second half. */

            top[1].src = top[0].src + top[0].n1;
            top[1].dst = top[0].src;
            top[1].n1 = (top[0].n2 + 1) / 2;
            top[1].n2 = top[0].n2 / 2;
            top[1].fhs = 0;
            top += 1;
        }

        else { 

            /* Do simple operation of sorting a second half with <= 2 elements.*/

            merge_value *s2 = top[0].src + top[0].n1;
            if (top[0].n2 > 1) {
                if (merge_greater(*s2,*(s2+1))) {
                    *top[0].src = *(s2+1);
                    *(top[0].src+1) = *s2;
                }
                else {
                    *top[0].src = *s2;
                    *(top[0].src+1) = *(s2+1);
                }
            }
            else
                *top[0].src = *s2;

            /* After a second half has been sorted, it will be possible to merge
               it with the first half.  If the merged result was a second half,
               it also can be merged, etc. */

            do {

                /* Merge first and second halves. */

                int n1 = top[0].n1;
                int n2 = top[0].n2;

                merge_value *i1 = top[0].dst + n2;
                merge_value *i2 = top[0].src;
                merge_value *j = top[0].dst;

                for (;;) {
                    if (merge_greater (*i1, *i2)) {
                        *j++ = *i2++;
                        n2 -= 1;
                        if (n2 == 0) {
                            while (n1 > 0) { *j++ = *i1++; n1 -= 1; }
                            break;
                        }
                    }
                    else {
                        *j++ = *i1++;
                        n1 -= 1;
                        if (n1 == 0) {
                            while (n2 > 0) { *j++ = *i2++; n2 -= 1; }
                            break;
                        }
                    }
                }

                /* Check whether everything is now done! */

                if (top == todo)
                    return;

                /* Go on to the next task. */

                top -= 1;        

            } while (top[0].fhs);

            top[0].fhs = 1;
        }

      enter:

        if (!top[0].fhs) {

           /* Create tasks to sort first half, first half of first half, etc.,
              until this reaches the point of sorting <= 2 elements. */

            while (top[0].n1 > 2) {
                top[1].src = top[0].src;
                top[1].dst = top[0].dst + top[0].n2;
                top[1].n1 = (top[0].n1 + 1) / 2;
                top[1].n2 = top[0].n1 / 2;
                top[1].fhs = 0;
                top += 1;
            }

            /* Do simple operation of sorting a first half with <= 2 elements. */

            merge_value *d1 = top[0].dst + top[0].n2;
            if (top[0].n1 > 1) {
                if (merge_greater(*top[0].src,*(top[0].src+1))) {
                    *d1 = *(top[0].src+1);
                    *(d1+1) = *top[0].src;
                }
                else {
                    *d1 = *top[0].src;
                    *(d1+1) = *(top[0].src+1);
                }
            }
            else
                *d1 = *top[0].src;

            top[0].fhs = 1;
        }
    }
}
