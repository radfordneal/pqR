/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Task Procedures for Matrix Multiplication With Pipelining

   Copyright (c) 2013, 2014 Radford M. Neal.

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

    /* If k is odd, initialize sum to the first product, and adjust x and
       y to account for this.  If k is even, just initialize sum to zero. */

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
    helpers_size_t j, a, k_times_j;

    /* Set up the mask that determines how often helpers_amount_out is called.
       Done less often if computing an element of the result takes less time. */

    helpers_size_t mask = k<100 ? 0x1e : k<250 ? 0xe : k<500 ? 0x6 : 0x2;

    /* If m is odd, compute the first element of the result (the dot product
       of x and the first column of y).  Adjust y, z, and j to account for 
       having handled the first column. */

    if (m & 1) {

        HELPERS_WAIT_IN2 (a, k-1, k_times_m);

        double *p = x;    /* pointer goes along vector x */
        double *e = x+k;  /* point where p stops */
        double s;

        /* Initialize sum to first product, if k odd; otherwise to 0. */

        if (k & 1)
            s = *p++ * *y++;
        else
            s = 0.0;

        /* Add two products each time around loop. */

        while (p < e) {
            s += *p++ * *y++;
            s += *p++ * *y++;
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

        double s1, s2, t;
        double *y2 = y+k;

        j += 2;
        k_times_j += 2*k;

        /* Wait for data in the two columns to be available. */

        if (a < k_times_j) HELPERS_WAIT_IN2 (a, k_times_j-1, k_times_m);

        double *p = x;    /* pointer that goes along vector x */
        double *e = x+k;  /* point where p stops */

        /* If the two dot products sum an odd number of products, set
           the sums, s1 and s2, to the first products here, and adjust p,
           y, and y2.  Otherwise, initialize s1 and s2 to zero. */

        if (k & 1) {
            t = *p++;
            s1 = t * *y++;
            s2 = t * *y2++;
        }
        else
            s1 = s2 = 0.0;

        /* Each time around this loop, add two products for each of the
           two dot products, adjusting p, y, and y2 as we go.  Note that
           e-p will be even when we start. */

        while (p < e) {
            t = p[0];
            s1 += t * y[0];
            s2 += t * y2[0];
            t = p[1];
            s1 += t * y[1];
            s2 += t * y2[1];
            y += 2;
            y2 += 2;
            p += 2;
        }

        y = y2;

        /* Store the two dot products in the result vector. */

        *z++ = s1;
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
    helpers_size_t j;
    double *p, *q;
    double *f;

    if (n <= 0) return;

    HELPERS_WAIT_IN2 (a, 0, k);

    if (n == 2) { 

        double s1, s2;  /* sums for the two elements of a result column */

        /* Initialize s1 and s2 to zero, if k is even, or to the products
           of the first element of y with the first column of x.  Adjust 
           x, y, and j accordingly. */

        if (k & 1) {
            double b = *y++;
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
                double b1 = y[0];
                double b2 = y[1];
                s1 = (s1 + (x[0] * b1)) + (x[2] * b2);
                s2 = (s2 + (x[1] * b1)) + (x[3] * b2);
                x += 4;
                y += 2;
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

    f = z+n;
    if (k & 1) {
        double b = *y++;
        do { *q++ = *x++ * b; } while (q < f);
        j = 3;
    }
    else {
        do { *q++ = 0.0; } while (q < f);
        j = 1;
    }

    /* Each time around this loop, add the products of two columns of x 
       with two elements of y to the result vector, z.  Adjust x, y, and
       j to account for this.  Note that k-j will be even when we start. */

    for (;;) {
        if (j > a) {
            if (j >= k)
                break;
            HELPERS_WAIT_IN2 (a, j-1, k);
        }
        double b1 = y[0];
        double b2 = y[1];
        q = z;
        f = z+n;
        p = x+n;
        do { *q = (*q + (*x++ * b1)) + (*p++ * b2); } while (++q < f);
        x = p;
        y += 2;
        j += 2;
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
   them in the result column at the end. */

void task_piped_matprod_mat_mat (helpers_op_t op, helpers_var_ptr sz, 
                                 helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    double *oy = y;
    helpers_size_t k = op;
    helpers_size_t n_times_k = LENGTH(sx);
    helpers_size_t k_times_m = LENGTH(sy);
    helpers_size_t n_times_m = LENGTH(sz);
    helpers_size_t n = n_times_k / k;
    helpers_size_t m = k_times_m / k;
    helpers_size_t done = 0;
    helpers_size_t a = 0;
    int j;

    if (n_times_m == 0)  return;

    HELPERS_SETUP_OUT (k < 10 ? 6 : k < 100 ? 5 : 4);

    /* Set result to zeros if k is zero. */

    if (k <= 0) {
        int i, j;
        for (j = 0; j < m; j++) {
            for (i = 0; i < n; i++) *z++ = 0.0;
            HELPERS_BLOCK_OUT(done,n);
        }
        return;
    }

    if (n == 2) { /* Treated specially */

        /* If m is odd, compute the first column of the result, and
           update y, z, and m accordingly. */

        if (m & 1) {

            double s1, s2;    /* sums for a column of the result */
            double *r = x;    /* r set to x, and then incremented */
            double *e = y+k;  /* stop when y reaches here */

            /* Wait for the first column of y to become available. */

            HELPERS_WAIT_IN2 (a, k-1, k_times_m);

            /* Initialize s1 and s2 to zero, if k is even, or to the 
               products of the first element of the next column of y with
               the first column of x.  Adjust r and y accordingly. */

            if (k & 1) {
                double b = *y++;
                s1 = *r++ * b;
                s2 = *r++ * b;
                j -= 1;
            }
            else
                s1 = s2 = 0.0;

            /* Each time around this loop, add the products of two columns
               of x and two elements of the first column of y to s1 and s2.
               Adjust r and y to account for this.  Note that e-y will 
               be even when we start. */

            while (y < e) {
                double b1 = y[0];
                double b2 = y[1];
                s1 = (s1 + (r[0] * b1)) + (r[2] * b2);
                s2 = (s2 + (r[1] * b1)) + (r[3] * b2);
                r += 4;
                y += 2;
            }

            /* Store s1 and s2 in the result column. */

            z[0] = s1;
            z[1] = s2;

            /* Signal that a column of z has been computed. */

            HELPERS_BLOCK_OUT (done, 2);

            /* Move to next column of the result. */

            z += 2;
            m -= 1;

        }

        /* Compute two columns of the result each time around this loop, 
           updating y, z, and m accordingly.  Note that m is now even. */

        while (m > 0) {

            double s11, s12, s21, s22;
            double *y2 = y + k;
            double *e = y2;     /* where y should stop */
            double *r = x;      /* r set to x, and then incrementd */

            /* Wait for the next two columns of y to become available. */

            if (a < y2-oy+k) {
                HELPERS_WAIT_IN2 (a, y2-oy+k-1, k_times_m);
            }

            /* Initialize sums for columns to zero, if k is even, or to the 
               products of the first elements of the next two columns of y
               with the first column of x.  Adjust r and y accordingly. */

            if (k & 1) {
                double b1 = *y++;
                double b2 = *y2++;
                s11 = r[0] * b1;
                s12 = r[1] * b1;
                s21 = r[0] * b2;
                s22 = r[1] * b2;
                r += 2;
            }
            else
                s11 = s12 = s21 = s22 = 0.0;

            /* Each time around this loop, add the products of two columns
               of x with elements of the next two columns of y to the sums.
               Adjust r and y to account for this.  Note that e-y will 
               be even. */

            while (y < e) {
                double b11 = y[0];
                double b12 = y[1];
                double b21 = y2[0];
                double b22 = y2[1];
                s11 = (s11 + (r[0] * b11)) + (r[2] * b12);
                s12 = (s12 + (r[1] * b11)) + (r[3] * b12);
                s21 = (s21 + (r[0] * b21)) + (r[2] * b22);
                s22 = (s22 + (r[1] * b21)) + (r[3] * b22);
                r += 4;
                y2 += 2;
                y += 2;
            }

            /* Store sums in the next two result columns. */

            z[0] = s11;
            z[1] = s12;
            z[2] = s21;
            z[3] = s22;

            /* Signal that two columns of z have been computed. */

            HELPERS_BLOCK_OUT (done, 4);

            /* Move to next column of the result. */

            y = y2;
            z += 4;
            m -= 2;
        }

        return;

    }

    /* If m is odd, compute the first column of the result, updating y and z
       m to account for this column having been computed.  Also update 'done'
       using HELPERS_BLOCK_OUT. */

    if (m & 1) {

        double *r = x;    /* r set to x, then modified as columns of x summed */
        double *e = y+k;  /* where y should stop */

        j = k;   /* j set to k, and then modified as values are read from y */

        /* Wait for the first column of y to become available. */

        HELPERS_WAIT_IN2 (a, k-1, k_times_m);

        /* Initialize sums in z to zero, if k is even, or to the product of
           the first element of the next column of y with the first column 
           of x (in which case adjust r and y accordingly). */

        if (k & 1) {
            double b = *y++;
            double *q = z;
            double *f = z+n;
            do { *q++ = *r++ * b; } while (q < f);
        }
        else {
            double *q = z;
            double *f = z+n;
            do { *q++ = 0.0; } while (q < f);
        }

        /* Each time around this loop, add the products of two columns of x 
           with two elements of the next column of y to the result vector, z.
           Adjust r and y to account for this.  Note that e-y will be even 
           when we start. */

        while (y < e) {
            double *q = z;
            double *f = z+n;
            double b1 = y[0];
            double b2 = y[1];
            do {
                *q = (*q + (*r * b1)) + (*(r+n) * b2);
                r += 1;
                q += 1;
            } while (q < f);
            r += n;
            y += 2;
        }

        /* Signal that a column of z has been computed. */

        HELPERS_BLOCK_OUT (done, n);

        /* Move to next column of the result. */

        z += n;
    }

    /* Compute two columns of the result each time around this loop, updating
       y and z accordingly.  */

    while (done < n_times_m) {

        double *y2 = y + k;
        double *e = y2; /* where y should stop */
        double *r = x;  /* r set to x, then modified as columns of x summed */

        /* Wait for the next two columns of y to become available. */

        if (a < y2-oy+k) {
            HELPERS_WAIT_IN2 (a, y2-oy+k-1, k_times_m);
        }

        /* Initialize sums in next two columns of z to zero, if k is even, 
           or to the products of the first elements of the next two columns
           of y with the first column of x, if k is odd (in which case adjust 
           r and y accordingly). */

        if (k & 1) {
            double b1 = *y++;
            double b2 = *y2++;
            double *q = z;
            double *f = x+n;
            do { *q++ = *r++ * b1; } while (r < f);
            r = x;
            do { *q++ = *r++ * b2; } while (r < f);
        }
        else {
            double *q = z;
            double *f = z+2*n;
            do { *q++ = 0.0; } while (q < f);
        }

        /* Each time around this loop, add the products of two columns of x 
           with two elements of the next two columns of y to the next two
           columns of the result vector, z.  Adjust r and y to account 
           for this.  Note that e-y will be even. */

        while (y < e) {
            double *q = z;
            double *f = z+n;
            double b11 = y[0];
            double b12 = y[1];
            double b21 = y2[0];
            double b22 = y2[1];
            do {
                double s1 = *r;
                double s2 = *(r+n);
                *q = (*q + (s1 * b11)) + (s2 * b12);
                *(q+n) = (*(q+n) + (s1 * b21)) + (s2 * b22);
                r += 1;
                q += 1;
            } while (q < f);
            r += n;
            y2 += 2;
            y += 2;
        }

        /* Signal that two columns of z have been computed. */

        HELPERS_BLOCK_OUT (done, 2*n);

        /* Move to the next two columns. */

        y = y2;
        z += 2*n;
    }
}


/* Product of the transpose of a k x n matrix (x) and a k x m matrix (y) 
   with result stored in z, with pipelining of the input y and the output
   (by column). 
                                                                         
   Each element of the result is the dot product of a column of x and a
   column of y.  The result is computed two columns at a time, which
   allows the memory accesses to columns of x to be used for two such
   dot products (with two columns of y).  Two columns of x are also done
   at once, again so memory accesses can be re-used.  The result is that,
   except perhaps for the first column or first element in a column, four
   elements of the result are computed at a time, using four accesses to
   columns of x and y (half the number of accesses that would be needed
   for doing four dot products in the obvious way).  
                                                                         
   When the two operands are the same, the result will be a symmetric
   matrix.  After computation of each column or pair of columns, they are
   copied to the corresponding rows; hence each column need be computed
   only from the diagonal element down.
*/

void task_piped_matprod_trans1 (helpers_op_t op, helpers_var_ptr sz, 
                                helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    double *oy = y;
    helpers_size_t k = op;
    helpers_size_t n_times_k = LENGTH(sx);
    helpers_size_t k_times_m = LENGTH(sy);
    helpers_size_t n_times_m = LENGTH(sz);
    helpers_size_t n = n_times_k / k;
    helpers_size_t m = k_times_m / k;

    int sym = x==y && n==m;  /* same operands, so symmetric result? */
    double *oz = z;          /* original value of z */
    int j = 0;               /* number of columns of result produced so far */
    helpers_size_t done = 0; /* amount of the result piped out so far */
    helpers_size_t a = 0;    /* amount of y now available */

    if (n_times_m <= 0) return;

    HELPERS_SETUP_OUT (k < 10 ? 6 : k < 100 ? 5 : 4);

    /* Set result to zeros if k is zero. */

    if (k <= 0) {
        int i, j;
        for (j = 0; j < m; j++) {
            for (i = 0; i < n; i++) *z++ = 0.0;
            HELPERS_BLOCK_OUT(done,n);
        }
        return;
    }

    /* If m is odd, compute the first column of the result, updating y, z, and 
       m to account for this column having been computed (so that the situation
       is the same as if m had been even to start with). */

    if (m & 1) {

        double *r = x;
        double *e = z+n;
        double *rz = z;

        /* Wait for first column of y to become available. */

        HELPERS_WAIT_IN2 (a, k-1, k_times_m);

        /* If n is odd, compute the first element of the first column of the
           result here.  Also, move r to point to the second column of x, and
           increment z.  For use if result is symmetric, advance rz to second
           element of the first row (no need to copy 1st element to itself). */

        if (n & 1) {
            double s = 0;
            double *q = y;
            double *e = y+k;
            do { s += *r++ * *q++; } while (q < e);
            *z++ = s;
            rz += n;
        }

        /* Compute the remainder of the first column of the result two
           elements at a time (looking at two columns of x).  Note that 
           e-z will be even.  If result is symmetric, copy elements to
           the first row as well. */

        while (z < e) {
            double s0 = 0;
            double s1 = 0;
            double *q = y;
            double *f = y+k;
            do {
                double t = *q;
                s0 += *r * t;
                s1 += *(r+k) * t;
                r += 1;
                q += 1;
            } while (q < f);
            r += k;
            *z++ = s0;
            *z++ = s1;
            if (sym) {
                *rz = s0;
                rz += n;
                *rz = s1;
                rz += n;
            }
        }

        /* Signal that a column of z has been computed. */

        HELPERS_BLOCK_OUT (done, n);

        y += k;
        j += 1;
    }

    /* Compute two columns of the result each time around this loop, updating
       y, z, and j accordingly.  Note that m-j will be even. */

    while (j < m) {

        double *z2 = z+n;
        double *e = z2;
        double *r = x;
        int nn = n;
        double *rz;

        /* If the result is symmetric, skip down to the diagonal element
           of the first column.  Also, let nn be the number of elements to
           compute for these column, and set r to the start of the column
           of x to use. */

        if (sym) {
            z += j;
            z2 += j;
            nn -= j;
            r += j*k;
            rz = z;
        }

        /* Wait for the next two columns of y to become available. */

        int needed = y - oy + 2*k;
        if (a < needed) HELPERS_WAIT_IN2 (a, needed-1, k_times_m);

        /* If an odd number of elements are to be computed in the two columns,
           compute the first elements here.  Also, if result is symmetric,
           advance rz (but no need to store, since it would be redundant). */

        if (nn & 1) {
            double s0 = 0;
            double s1 = 0;
            double *q = y;
            double *f = y+k;
            do {
                double t = *r++;
                s0 += t * *q;
                s1 += t * *(q+k);
                q += 1;
            } while (q < f);
            *z++ = s0;
            *z2++ = s1;
            if (sym) rz += n;
        }

        /* Compute the remainder of the two columns of the result, two elements
           at a time.  Copy them to the corresponding rows too, if the result
           is symmetric. */

        while (z < e) {
            double s00 = 0.0;
            double s01 = 0.0;
            double s10 = 0.0;
            double s11 = 0.0;
            double *q = y;
            int i = k;
            do {
                double t = *r;
                double t2 = *(r+k);
                double u = *q;
                double u2 = *(q+k);
                s00 += t * u;
                s01 += t * u2;
                s10 += t2 * u;
                s11 += t2 * u2;
                r += 1;
                q += 1;
            } while (--i > 0);
            *z++ = s00;
            *z2++ = s01;
            *z++ = s10;
            *z2++ = s11;
            if (sym) {
                rz[0] = s00;
                rz[1] = s01;
                rz += n;
                rz[0] = s10;
                rz[1] = s11;
                rz += n;
            }
            r += k;
        }

        /* Signal that two columns of z have been computed. */

        HELPERS_BLOCK_OUT (done, 2*n);

        /* Go on to next two columns of y. */

        z = z2;
        y += 2*k;
        j += 2;
    }
}


/* Product of an n x k matrix (x) and the transpose of an m x k matrix (y) 
   with result stored in z, with pipelining of the output (by column).

   When the two operands are the same, the result will be a symmetric
   matrix.  Only the lower-triangular part of the result is computed,
   with the elements in columns that are computed then being copied to
   the corresponding elements in rows above the diagonal.

   The case of n=2 is handled specially, accumulating sums in two local
   variables rather than in a column of the result, and then storing
   them in the result column at the end. */

void task_piped_matprod_trans2 (helpers_op_t op, helpers_var_ptr sz, 
                                helpers_var_ptr sx, helpers_var_ptr sy)
{
    double *x = REAL(sx), *y = REAL(sy), *z = REAL(sz);
    helpers_size_t k = op;
    helpers_size_t n_times_k = LENGTH(sx);
    helpers_size_t k_times_m = LENGTH(sy);
    helpers_size_t n = n_times_k / k;
    helpers_size_t m = k_times_m / k;

    int sym = x==y && n==m;     /* same operands, so symmetric result? */
    double *oz = z;             /* original value of z */
    double *ex = x + n_times_k; /* point past end of x */
    int j = 0;                  /* number of columns of result produced so far*/

    helpers_size_t done = 0;    /* amount of the result piped out so far */
    helpers_size_t a;

    if (n <= 0) return;

    HELPERS_SETUP_OUT (k < 10 ? 6 : k < 100 ? 5 : 4);

    /* Set result to zeros if k is zero. */

    if (k <= 0) {
        int i, j;
        for (j = 0; j < m; j++) {
            for (i = 0; i < n; i++) *z++ = 0.0;
            HELPERS_BLOCK_OUT(done,n);
        }
        return;
    }

    /* Wait for second operand, just in case we were scheduled with pipelining
       for it (since the other piped_matprod task procedures do this). */

    HELPERS_WAIT_IN2 (a, k_times_m-1, k_times_m);

    if (n == 2) {

        /* If m is odd, compute the first column of the result, and
           update y, z, and mt accordingly. */

        if (m & 1) {

            double s1, s2;
            double *q = y;
            double *r = x;
            double *e = x+2*k;

            /* Initialize s1 and s2 to zero, if k is even, or to the 
               products of the first element of the first row of y with
               the first column of x.  Adjust r and q accordingly. */

            if (k & 1) {
                double b = *q;
                s1 = *r++ * b;
                s2 = *r++ * b;
                q += m;
            }
            else
                s1 = s2 = 0.0;

            /* Each time around this loop, add the products of two columns
               of x and two elements of the first row of y to s1 and s2.
               Adjust r and q to account for this. */

            while (r < e) {
                double b1, b2;
                b1 = *q;
                q += m;
                b2 = *q;
                q += m;
                s1 = (s1 + (r[0] * b1)) + (r[2] * b2);
                s2 = (s2 + (r[1] * b1)) + (r[3] * b2);
                r += 4;
            }

            /* Store s1 and s2 in the result column. */

            z[0] = s1;
            z[1] = s2;

            /* Signal that a column of z has been computed. */

            HELPERS_BLOCK_OUT (done, 2);

            /* Move to next column of the result, and next row of y. */

            z += 2;
            y += 1;
            j += 1;
        }

        /* Compute two columns of the result each time around this loop, 
           updating y, z, and j accordingly.  Note that m-j is now even. */

        while (j < m) {

            double s11, s12, s21, s22;
            double *q = y;
            double *r = x;

            /* Initialize sums for columns to zero, if k is even, or to the 
               products of the first elements of the next two rows of y with
               the first column of x.  Adjust r and q accordingly. */

            if (k & 1) {
                double b = *q;
                double b2 = *(q+1);
                q += m;
                s11 = r[0] * b;
                s12 = r[1] * b;
                s21 = r[0] * b2;
                s22 = r[1] * b2;
                r += 2;
            }
            else
                s11 = s12 = s21 = s22 = 0.0;

            /* Each time around this loop, add the products of two columns
               of x with elements of the next two rows of y to the sums.
               Adjust r and q to account for this. */

            while (r < ex) {
                double b11, b12, b21, b22;
                b11 = *q;
                b21 = *(q+1);
                q += m;
                b12 = *q;
                b22 = *(q+1);
                q += m;
                s11 = (s11 + (r[0] * b11)) + (r[2] * b12);
                s12 = (s12 + (r[1] * b11)) + (r[3] * b12);
                s21 = (s21 + (r[0] * b21)) + (r[2] * b22);
                s22 = (s22 + (r[1] * b21)) + (r[3] * b22);
                r += 4;
            }

            /* Store sums in the next two result columns. */

            z[0] = s11;
            z[1] = s12;
            z[2] = s21;
            z[3] = s22;

            /* Signal that two columns of z have been computed. */

            HELPERS_BLOCK_OUT (done, 4);

            /* Move forward two to the next column of the result and 
               the next row of y. */

            z += 4;
            y += 2;
            j += 2;
        }

        return;
    }

    /* If m is odd, compute the first column of the result, updating y, z, and 
       j to account for this column having been computed.  Also, if result
       is symmetric, copy this column to the first row. */

    if (m & 1) {

        double *q = y;
        double *r = x;
        double *ez = z+n;

        /* Initialize sums in z to zero, if k is even, or to the product of
           the first element of the first row of y with the first column 
           of x (in which case adjust r and q accordingly). */

        if (k & 1) {
            double *t = z;
            double *f = z+n;
            double b = *q;
            do { *t++ = *r++ * b; } while (t < f);
            q += m;
        }
        else {
            double *t = z;
            double *f = z+n;
            do { *t++ = 0.0; } while (t < f);
        }

        /* Each time around this loop, add the products of two columns of x 
           with two elements of the first row of y to the result vector, z.
           Adjust r and y to account for this. */

        while (r < ex) {
            double *t = z;
            double b1, b2;
            b1 = *q;
            q += m;
            b2 = *q;
            q += m;
            do {
                *t = (*t + (*r * b1)) + (*(r+n) * b2);
                r += 1;
                t += 1;
            } while (t < ez);
            r += n;
        }

        /* Signal that a column of z has been computed. */

        HELPERS_BLOCK_OUT (done, n);

        /* Copy first column to first row, if result is symmetric. */

        if (sym) {
            double *t = z+1;
            double *q = z+n;
            while (t < ez) {
                *q = *t;
                t += 1;
                q += n;
            }
        }

        /* Move to next column of the result and the next row of y. */

        z += n;
        y += 1;
        j += 1;
    }

    /* Compute two columns of the result each time around this loop, updating
       y, z, and j accordingly.  Note that m-j will be even.  If the result
       is symmetric, only the parts of the columns at and below the diagonal
       are computed (except one element above the diagonal is computed for
       the second column), and these parts are then copied to the corresponding
       rows. */

    while (j < m) {

        double *zs = sym ? z+j : z;   /* Where to start storing sums */
        double *ez = z+n;             /* Where we stop storing sums */
        double *xs = x;
        double *t1 = zs;
        double *t2 = t1 + n;
        double *q = y;

        /* Initialize sums in the next two columns of z to zero, if k is 
           even, or to the products of the first elements of the next two
           rows of y with the first column of x (in which case adjust r and 
           q accordingly). */

        if (k & 1) {
            double b1 = *q;
            double b2 = *(q+1);
            double *r = sym ? xs+j : xs;
            do {
                double s = *r++;
                *t1++ = s * b1;
                *t2++ = s * b2;
            } while (t1 < ez);
            xs += n;
            q += m;
        }
        else {
            do {
                *t1++ = 0;
                *t2++ = 0;
            } while (t1 < ez);
        }

        /* Each time around this loop, add the products of two columns of x 
           with elements of the next two rows of y to the next two columns
           the result vector, z.  Adjust r and y account for this. */

        while (xs < ex) {
            double b11, b12, b21, b22;
            double *t1 = zs;
            double *t2 = t1 + n;
            double *r = sym ? xs+j : xs;
            b11 = *q;
            b21 = *(q+1);
            q += m;
            b12 = *q;
            b22 = *(q+1);
            q += m;
            do {
                double s1 = *r;
                double s2 = *(r+n);
                *t1 = (*t1 + (s1 * b11)) + (s2 * b12);
                *t2 = (*t2 + (s1 * b21)) + (s2 * b22);
                t1 += 1;
                t2 += 1;
                r += 1;
            } while (t1 < ez);
            xs += 2*n;
        }

        /* If the result is symmetric, copy the columns just computed
           to the corresponding rows. */

        if (sym) {
            double *t1 = zs + 2;
            double *t2 = t1 + n;
            double *q = zs + 2*n;
            while (t1 < ez) {
                q[0] = *t1;
                q[1] = *t2;
                t1 += 1;
                t2 += 1;
                q += n;
            }
        }

        /* Signal that two columns of z have been computed.  Note! This
           can't be done earlier, because the receiver may change data once
           it has been made available. */

        HELPERS_BLOCK_OUT (done, 2*n);

        /* Move forward two to the next column of the result and the
           next row of y. */

        z += 2*n;
        y += 2;
        j += 2;
    }
}
