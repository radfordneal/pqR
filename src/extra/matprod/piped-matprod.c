/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Task Procedures for Matrix Multiplication With Pipelining

   Copyright (c) 2013 Radford M. Neal.

   The matprod library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/


#include "helpers-app.h"
#include "piped-matprod.h"


/* Dot product of two vectors, with pipelining of input y.

   The loop is unrolled to do two pairs of elements each iteration, with 
   the sum initialized either to zero or to the result from the first pair, 
   if the number of pairs is odd. */

void task_piped_matprod_vec_vec (helpers_op_t op, helpers_var_ptr sz, 
                                 helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *ox = x;
    helpers_size_t k, a;
    double s;

    k = LENGTH(sx);

    /* If k is odd, initialize sum to the first product, and adjust x,
       y, and i to account for this.  If k is even, just initialize
       sum to zero. */

    if (k & 1) {
        HELPERS_WAIT_IN2 (a, 0, k);
        s = *x++ * *y++;
    }
    else {
        s = 0.0;
        a = 0;
    }

    /* Add two products each time around the inner loop, adjusting x, y, 
       and i as we go. */

    for (;;) {
        helpers_size_t need_more_than = x - ox + 1;
        if (need_more_than >= k)
            break; 
        if (need_more_than >= a)
            HELPERS_WAIT_IN2 (a, need_more_than, k);
        double *ex = ox - 1 + a;
        do {
            s += *x++ * *y++;
            s += *x++ * *y++;
        } while (x < ex);
    }

    *REAL(sz) = s;
}


/* Product of row vector (x) of length k and k x m matrix (y), result stored 
   in z, with pipelining of the input y and of the output.

   The inner loop is a pair of vector dot products, each implemented 
   similarly to task_piped_matprod_vec_vec above, with loop unrolling 
   within each dot product. */

void task_piped_matprod_vec_mat (helpers_op_t op, helpers_var_ptr sz, 
                                 helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    helpers_size_t k_times_m = LENGTH(sy);
    helpers_size_t k = LENGTH(sx);
    helpers_size_t m = LENGTH(sz);
    helpers_size_t i, j, a, k_times_j;
    double s, s2, t;
    double *p, *y2;

    /* Set up the mask that determines how often helpers_amount_out is called.
       Done less often if computing an element of the result takes less time. */

    helpers_size_t mask = k<100 ? 0x1e : k<250 ? 0xe : k<500 ? 0x6 : 0x2;

    /* If m is odd, compute the first element of the result (the dot product
       of x and the first column of y).  Adjust y, z, and j to account for 
       having handled the first column. */

    if (m & 1) {

        HELPERS_WAIT_IN2 (a, k-1, k_times_m);

        p = x;
        i = k;

        /* Initialize sum to first product, if k odd; otherwise to 0. */

        if (i & 1) {
            s = *p++ * *y++;
            i -= 1;
        }
        else
            s = 0.0;

        /* Add two products each time around loop. Note: i is even. */

        while (i > 0) {
            s += *p++ * *y++;
            s += *p++ * *y++;
            i -= 2;
        }

        /* Store result of dot product as first element of result. */

        *z++ = s;
        j = 1;
        k_times_j = k;
    }
    else {
        a = 0;
        j = 0;
        k_times_j = 0;
    }

    /* In this loop, compute two consecutive elements of the result vector,
       by doing two dot products of x with columns of y.  Adjust y, z, and
       j as we go.  Note that m-j, the number of columns left to do, is even 
       when we start. */

    while (k_times_j < k_times_m) {

        j += 2;
        k_times_j += 2*k;

        /* Wait for data in the two columns to be available. */

        if (a < k_times_j) HELPERS_WAIT_IN2 (a, k_times_j-1, k_times_m);

        /* Set y and y2 to point to the starts of the two columns.  Note
           that y was modified in the previous dot product operation 
           so that it is now pointing at the next column to do. */

        y2 = y + k;

        /* Set p to point initially to x, and i to initially be k.  Both
           will be changed as the two dot products are computed. */

        p = x;
        i = k;

        /* If the two dot products sum an odd number of products, set
           the sums, s and s2, to the first products here, and adjust p, 
           y, y2, and i.  Otherwise, initialize s and s2 to zero. */

        if (i & 1) {
            t = *p++;
            s = t * *y++;
            s2 = t * *y2++;
            i -= 1;
        }
        else
            s2 = s = 0.0;

        /* Each time around this loop, add two products for each of the 
           two dot products, adjusting p, y, y2, and i as we go.  Note
           that i will be even when we start. */

        while (i > 0) {
            t = *p++;
            s += t * *y++;
            s2 += t * *y2++;
            t = *p++;
            s += t * *y++;
            s2 += t * *y2++;
            i -= 2;
        }

        y = y2;

        /* Store the two dot products in the result vector. */

        *z++ = s;
        *z++ = s2;

        /* Signal what elements of the result have been computed, but not
           too often, since this takes some time. */

        if ((j & mask) == 0) helpers_amount_out(j);
    }
}


/* Product of n x k matrix (x) and column vector of length k (y) with result 
   stored in z, with pipelining of input y.

   The product is computed using an outer loop that accumulates the sums for 
   all elements of the result vector, iterating over columns of x, in order
   to produce sequential accesses.  This loop is unrolled to accumulate from
   two columns of x at once, which probably reduces the number of memory
   accesses, and may give more potential to overlap accesses with computation.
   The result vector is initialized either to zeros or to the result from the 
   first column, if the number of columns is odd.  Order of summation is
   kept the same as the obvious method, for consistency of round-off errors.

   The case of n=2 is handled specially, accumulating sums in two local 
   variables rather than in the result vector, and then storing them in the 
   result at the end. */

void task_piped_matprod_mat_vec (helpers_op_t op, helpers_var_ptr sz, 
                                 helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    helpers_size_t k = LENGTH(sy);
    helpers_size_t n = LENGTH(sz);
    helpers_size_t a;
    helpers_size_t i, j;
    double *p, *q;
    double b, b2;

    HELPERS_WAIT_IN2 (a, 0, k);

    if (n == 2) { 

        double s1, s2;

        /* Initialize s1 and s2 to zero, if k is even, or to the products
           of the first element of y with the first column of x.  Adjust 
           x, y, and j accordingly. */

        if (k & 1) {
            b = *y++;
            s1 = *x++ * b;
            s2 = *x++ * b;
            j = 1;
        }
        else {
            s1 = s2 = 0.0;
            j = 0;
        }

        /* Each time around this loop, add the products of two columns of
           x with two elements of y to s1 and s2.  Adjust x, y, and j to
           account for this.  Note that k-j will be even when we start. */

        while (j < k) {
            while (j+2 <= a) {
                j += 2;
                b = *y++;
                b2 = *y++;
                s1 = (s1 + (x[0] * b)) + (x[2] * b2);
                s2 = (s2 + (x[1] * b)) + (x[3] * b2);
                x += 4;
            }
            if (j < k) HELPERS_WAIT_IN2 (a, j+1, k);
        }

        /* Store s1 and s2 in the result vector. */

        z[0] = s1;
        z[1] = s2;

        return;
    }

    /* Initialize sums in z to zero, if k is even, or to the product of
       the first element of y with the first column of x.  Adjust x, y,
       and j accordingly. */

    q = z;

    if (k & 1) {
        b = *y++;
        for (i = n; i > 0; i--)
            *q++ = *x++ * b;
        j = 1;
    }
    else {
        for (i = n; i > 0; i--)
            *q++ = 0.0;
        j = 0;
    }

    /* Each time around this loop, add the products of two columns of x 
       with two elements of y to the result vector, z.  Adjust x, y, and
       j to account for this.  Note that k-j will be even when we start. */

    while (j < k) {
        while (j+2 <= a) {
            j += 2;
            p = x + n;
            q = z;
            b = *y++;
            b2 = *y++;
            for (i = n; i > 0; i--) {
                double tmp = *q + (*x++ * b);  /* ensure order of addition */
                *q++ = tmp + (*p++ * b2);
            }
            x = p;
        }
        if (j < k) HELPERS_WAIT_IN2 (a, j+1, k);
    }
}


/* Product of an n x k matrix (x) and a k x m matrix (y) with result stored 
   in z, with pipelining of the input y and the output (by column).

   The inner loop is a matrix-vector product, implemented much as in 
   task_piped_matprod_mat_vec above, that computes one column of the result.  
   This gives a reasonably efficient implementation of an outer product 
   (where k is one). Order of summation is kept the same as the obvious 
   method, for consistency of round-off errors.

   The case of n=2 is handled specially, accumulating sums in two
   local variables rather than in a column of the result, and then storing
   them in the result column at the end. 

   The value of k (taken from op) must be greater than zero. */

void task_piped_matprod (helpers_op_t op, helpers_var_ptr sz, 
                         helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    double *oy = y;
    helpers_size_t k = op;
    helpers_size_t n_times_k = LENGTH(sx);
    helpers_size_t k_times_m = LENGTH(sy);
    helpers_size_t n_times_m = LENGTH(sz);
    helpers_size_t n = n_times_k / k;
    helpers_size_t a;
    double *p, *q, *r;
    double b, b2;
    int i, j;

    if (n_times_m == 0)  return;

    HELPERS_SETUP_OUT (k < 10 ? 6 : k < 100 ? 5 : 4);
    HELPERS_WAIT_IN2 (a, k-1, k_times_m);

    helpers_size_t done = 0;

    if (n == 2) { /* Treated specially */

        /* Compute one column of the result each time around this loop, 
           updating y and z accordingly. */

        for (;;) {

            double s1, s2;

            r = x;   /* r set to x, and then modified */
            j = k;   /* j set to k, and then modified */

            /* Initialize s1 and s2 to zero, if k is even, or to the 
               products of the first element of the next column of y with
               the first column of x.  Adjust x, y, and j accordingly. */

            if (j & 1) {
                b = *y++;
                s1 = *r++ * b;
                s2 = *r++ * b;
                j -= 1;
            }
            else
                s1 = s2 = 0.0;

            /* Each time around this loop, add the products of two columns
               of x and two elements of the next column of y to s1 and s2.
               Adjust x, y, and j to account for this.  Note that j will 
               be even when we start. */

            while (j > 0) {
                b = *y++;
                b2 = *y++;
                s1 = (s1 + (r[0] * b)) + (r[2] * b2);
                s2 = (s2 + (r[1] * b)) + (r[3] * b2);
                r += 4;
                j -= 2;
            }

            /* Store s1 and s2 in the result column. */

            z[0] = s1;
            z[1] = s2;

            /* Signal that a column of z has been computed. */

            HELPERS_BLOCK_OUT (done, n);

            /* Exit if we've done it all. */

            if (done == n_times_m)
                break;

            /* Move to the next column of z. */

            z += 2;

            /* Wait for the next column of y to become available. */

            if (a < y-oy+k) HELPERS_WAIT_IN2 (a, y-oy+k-1, k_times_m);
        }

        return;

    }

    /* Compute one column of the result each time around this loop, updating
       y and z accordingly. */

    for (;;) {

        q = z;
        r = x;   /* r set to x, and then modified */
        j = k;   /* j set to k, and then modified */

        /* Initialize sums in z to zero, if k is even, or to the product of
           the first element of the next column of y with the first column 
           of x.  Adjust r, y, and j accordingly. */

        if (j & 1) {
            b = *y++;
            for (i = n; i > 0; i--)
                *q++ = *r++ * b;
            j -= 1;
        }
        else 
            for (i = n; i > 0; i--)
                *q++ = 0;

        /* Each time around this loop, add the products of two columns of x 
           with two elements of the next column of y to the result vector, z.
           Adjust r, y, and j to account for this.  Note that j will be even 
           when we start. */

        while (j > 0) {
            p = r + n;
            q = z;
            b = *y++;
            b2 = *y++;
            for (i = n; i > 0; i--) {
                double tmp = *q + (*r++ * b);  /* ensure order of addition */
                *q++ = tmp + (*p++ * b2);
            }
            r = p;
            j -= 2;
        }

        /* Signal that a column of z has been computed. */

        HELPERS_BLOCK_OUT (done, n);

        /* Exit if we've done it all. */

        if (done == n_times_m)
            break;

        /* Move to the next column of z. */

        z += n;

        /* Wait for the next column of y to become available. */

        if (a < y-oy+k) HELPERS_WAIT_IN2 (a, y-oy+k-1, k_times_m);
    }
}
