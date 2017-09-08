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


/* Standalone test program.  Test merge_sort with

       cc -DTEST_MERGE_SORT merge-sort.c; a.out n1 n2 n3 ...
*/

#ifdef TEST_MERGE_SORT

#include <stdio.h>
#include <stdlib.h>

#define merge_value int
#define merge_greater(a,b) ((a) > (b))

static void merge_sort (merge_value *dst, merge_value *src, int n);

int main (int argc, char **argv)
{
    int n = argc - 1;
    merge_value d[n], s[n];
    int i;
    
    for (i = 0; i < n; i++) s[i] = atoi(argv[i+1]);
    merge_sort (d, s, n);
    for (i = 0; i < n; i++) printf(" %d",d[i]);
    printf("\n");

    return 0;
}

#endif


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

   The algorithm is recursive.  It can be viewed in terms of halves of
   src and dst, as follows:

           src = [ A | B ]     SORT->      dst = [ X | Y ]

   This is implemented as follows:

                B      SORT->    Y      (B destroyed)
                A      SORT->    B      (A destroyed)
               B,Y    MERGE->   dst
	       
   If src is of odd length, B is one bigger than A, and Y is one
   bigger than X.  The merge of B and Y will overwrite Y, but a write
   to an element of Y will happen only after that element is no longer
   needed.  Some writes may not be needed, because the last elements
   merged are already in place.  If n is small, the above procedure 
   is replaced by a simple insertion sort. */

#if 1   /* Simple recursive version */

static void merge_sort (merge_value *dst, merge_value *src, int n)
{
    if (n <= 10) {

        if (n == 0)
            return;

        dst[0] = src[0];

        int i, j;
        for (i = 1; i < n; i++) {
            j = i;
            while (j > 0 && merge_greater(dst[j-1],src[i])) {
                dst[j] = dst[j-1];
                j = j - 1;
            }
            dst[j] = src[i];
        }
    }
    else {

        int n1 = n / 2;
        int n2 = n - n1;

        merge_sort (dst + n1, src + n1, n2);
        merge_sort (src + n1, src, n1);

        merge_value *i1 = src + n1;
        merge_value *i2 = dst + n1;
        merge_value *j = dst;

        for (;;) {
            if (merge_greater (*i1, *i2)) {
                *j++ = *i2++;
                n2 -= 1;
                if (n2 == 0) {
                    do { *j++ = *i1++; n1 -= 1; } while (n1 > 0);
                    break;
                }
            }
            else {
                *j++ = *i1++;
                n1 -= 1;
                if (n1 == 0) {
                    /* no need for copy - already there */
                    break;
                }
            }
        }

        return;
    }
}

#else  /* Version that's not explicitly recursive */

static void merge_sort (merge_value *dst, merge_value *src, int n)
{
    if (n == 0)
        return;

    if (n == 1) {
        *dst = *src;
        return;
    }

    /* Array of tasks to do, mimicking recursion. 50 should be enough. */

    struct todo { 
        int n;             /* Number of elements in source/destination */
        merge_value *src;  /* Pointer to first element in source array */
        merge_value *dst;  /* Pointer to first element in destination array */
        int halves_done;   /* Number of halves that have been sorted */
    } todo[50]; 

    struct todo *top;      /* Top value in todo */

    top = todo;
    top[0].n = n;
    top[0].src = src;
    top[0].dst = dst;
    top[0].halves_done = 0;

    /* Do and create tasks until original task is done. */

    for (;;) {

        int n0 = top[0].n;

        if (n0 > 2) {

            /* Create a task to sort one half or the other. */

            int n1 = n0 / 2;

            if (top[0].halves_done) {
                top[1].n = n1;
                top[1].src = top[0].src;
                top[1].dst = top[0].src + n1;
            }
            else {
                top[1].n = n0 - n1;
                top[1].src = top[0].src + n1;
                top[1].dst = top[0].dst + n1;
            }

            top[1].halves_done = 0;
            top += 1;

            continue;
        }

        /* Sort one or two elements directly. */

        merge_value *s = top[0].src;
        merge_value *d = top[0].dst;

        if (n0 == 1)
            *d = *s;
        else {
            if (merge_greater(*s,*(s+1))) {
                *d = *(s+1);
                *(d+1) = *s;
            }
            else {
                *d = *s;
                *(d+1) = *(s+1);
            }
        }

        /* Do as many merges as possible. */

        for (;;) {

            /* Check whether we're all done. */

            if (top == todo)
                return;

            /* Pop to next remaining task, see if can do merge for it. */

            top -= 1;
            top[0].halves_done += 1;
            if (top[0].halves_done != 2)
                break;

            /* Both halves have been sorted, so we can merge. */

            int n0 = top[0].n;
            int n1 = n0 / 2;
            int n2 = n0 - n1;

            merge_value *i1 = top[0].src + n1;
            merge_value *i2 = top[0].dst + n1;
            merge_value *j = top[0].dst;

            for (;;) {
                if (merge_greater (*i1, *i2)) {
                    *j++ = *i2++;
                    n2 -= 1;
                    if (n2 == 0) {
                        do { *j++ = *i1++; n1 -= 1; } while (n1 > 0);
                        break;
                    }
                }
                else {
                    *j++ = *i1++;
                    n1 -= 1;
                    if (n1 == 0) {
                        /* no need for copy - data already there */
                        break;
                    }
                }
            }
        }
    }
}

#endif
