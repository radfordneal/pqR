/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Task Procedures for Parallel/Pipelined Matrix Multiplication

   Copyright (c) 2013, 2014, 2017, 2018 Radford M. Neal.

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


/* IMPORTANT NOTE:  The interface for the application-visible functions
   defined here is documented in api-doc, and the implementation strategy
   is documented in imp-doc.  These documents should be consulted before
   reading this code, and updated if this code is changed. */


#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef MATPROD_APP_INCLUDED
# include "matprod-app.h"
#endif

#include "helpers-app.h"
#include "par-matprod.h"

/* -------------------------------------------------------------------------- */

#define PAR_MATPROD  /* Tell matprod.c it's being included from here */

#define SCOPE static

#define EXTRA(start_z,last_z,input_first) \
               , start_z, last_z, input_first, LENGTH(sz)
#define EXTRAD , double *start_z, double *last_z, int input_first, size_t s_sz
#define EXTRAZ , 0, 0, 0, 0
#define EXTRAN , start_z, last_z, input_first, s_sz

#define THRESH 64

#define AMTOUT(_z_) \
  do \
  { if (start_z == 0) break; \
    assert ((_z_) - start_z <= s_sz); \
    if (input_first) \
    { helpers_size_t a = helpers_avail0 (last_z - start_z); \
      if (a == 0) break; \
      helpers_amount_out(a); \
      if (a < last_z - start_z) break; \
      input_first = 0; \
    } \
    if ((_z_) - last_z >= THRESH) \
    { helpers_amount_out ((_z_) - start_z); \
      last_z = (_z_); \
    } \
  } while (0)

#define THRESH 64  /* keep a multiple of 8 to avoid possible alignment issues */

#include "matprod.c"

/* -------------------------------------------------------------------------- */

#define OP_K(op) (op & 0x7fffffff)          /* get value of k from task op */
#define OP_S(op) (1 + ((op >> 32) & 0xff))  /* get value of s from task op */
#define OP_W(op) (op >> 40)                 /* get value of w from task op */

#define MAKE_OP(w,s,k) /* create task op value from w, s, and k */ \
  (((helpers_op_t)(w)<<40) | ((helpers_op_t)((s)-1)<<32) | k)

#define CACHE_ALIGN(z) \
  ((double *) (((uintptr_t)(z)+0x18) & ~0x3f)) /* round to 64-byte boundary */

#define WAIT_FOR_EARLIER_TASKS(sz) \
  do \
  { ; /* Wait for tasks doing earlier portions to complete. */ \
  } while (helpers_avail0(LENGTH(sz)) < LENGTH(sz))

#define SETUP_SPLIT(cond) /* set s to split, reducing till 'cond' 0, or s 1 */ \
  int s = OP_S(op); \
  int w = OP_W(op); \
  while (s > 1 && (cond)) \
  { if (w == 0) return; \
    s -= 1; w -= 1; \
  }


/* Scalar-vector product, with pipelining of input y and output z. */

void task_par_matprod_scalar_vec (helpers_op_t op, helpers_var_ptr sz,
                                  helpers_var_ptr sx, helpers_var_ptr sy)
{
  double x = *REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t m = LENGTH(sy);

  if (m <= 0)
  { return;
  }

  SETUP_SPLIT (4*s > m)

  if (s > 1)
  { 
    int t0 = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
    int t1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;

    helpers_size_t a = t0;

    while (a < t1)
    { 
      helpers_size_t oa = a;
      helpers_size_t na = t1-a <= 4 ? t1 : a+4;
      HELPERS_WAIT_IN2 (a, na-1, m);
      if (a > t1) a = t1;
      else if (a < t1) a &= ~3;

      matprod_scalar_vec (x, y+oa, z+oa, a-oa EXTRA (z, z+oa, w));
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t a = 0;

    while (a < m)
    { 
      helpers_size_t oa = a;
      helpers_size_t na = m-a <= 4 ? m : a+4;
      HELPERS_WAIT_IN2 (a, na-1, m);
      if (a < m) a &= ~3;

      matprod_scalar_vec (x, y+oa, z+oa, a-oa EXTRA (z, z+oa, 0));
    }
  }
}


/* Dot product of two vectors, with pipelining of input y. */

void task_par_matprod_vec_vec (helpers_op_t op, helpers_var_ptr sz,
                               helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t k = LENGTH(sx);

  SETUP_SPLIT (1);  /* always use only one thread */

  helpers_size_t a = 0;
  double sum = 0;

  while (a < k)
  { 
    helpers_size_t oa = a;
    helpers_size_t na = k-a <= 4 ? k : a+4;
    HELPERS_WAIT_IN2 (a, na-1, k);
    if (a < k) a &= ~3;

    sum = matprod_vec_vec_sub (x+oa, y+oa, a-oa, sum);
  }

  z[0] = sum;
}


/* Product of row vector (x) of length k and k x m matrix (y), result stored
   in z, with pipelining of the input y and of the output. */

void task_par_matprod_vec_mat (helpers_op_t op, helpers_var_ptr sz,
                               helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t m = LENGTH(sz);

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_vec_vec (op, sz, sx, sy);
    }
    return;
  }

  helpers_size_t k = LENGTH(sx);

  if (k == 1)
  { task_par_matprod_scalar_vec (op, sz, sx, sy);
    return;
  }

  helpers_size_t k_times_m = LENGTH(sy);

  SETUP_SPLIT (4*s > m)

  if (k == 0)
  { if (w == s-1)  /* do in only the last thread */
    { matprod_vec_mat (x, y, z, k, m EXTRA (z, z, 0));
    }
    return;
  }

  if (s > 1)
  { 
    if (w < s)
    { 
      helpers_size_t d, d1, a, a1;

      d = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
      d1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;
      a = d * k;
      a1 = d1 * k;

      while (d < d1)
      { 
        helpers_size_t od = d;
        helpers_size_t na = d1-d <= 4 ? a1 : k*(d+4);
        HELPERS_WAIT_IN2 (a, na-1, a1);
        d = a/k;
        if (d < m) d &= ~3;

        matprod_vec_mat (x, y+od*k, z+od, k, d-od EXTRA (z, z+od, w));
      }
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t d = 0;
    helpers_size_t a = 0;

    while (d < m)
    { 
      helpers_size_t od = d;
      helpers_size_t na = m-d <= 4 ? k_times_m : k*(d+4);
      HELPERS_WAIT_IN2 (a, na-1, k_times_m);
      d = a/k;
      if (d < m) d &= ~3;

      matprod_vec_mat (x, y+od*k, z+od, k, d-od EXTRA (z, z+od, 0));
    }
  }
}


/* Product of n x k matrix (x) and column vector of length k (y) with result
   stored in z, with pipelining of input y. */

void task_par_matprod_mat_vec (helpers_op_t op, helpers_var_ptr sz,
                               helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t n = LENGTH(sz);

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_vec_vec (op, sz, sx, sy);
    }
    return;
  }

  helpers_size_t k = LENGTH(sy);

  if (k == 1)
  { task_par_matprod_scalar_vec (op, sz, sy, sx);
    return;
  }

  SETUP_SPLIT (8*s > n)

  if (k <= 1)
  { if (w == s-1)  /* do in only the last thread */
    { helpers_size_t a;
      if (k > 0) HELPERS_WAIT_IN2 (a, k-1, k);
      matprod_mat_vec (x, y, z, n, k);
    }
    return;
  }

  if (s > 1)
  { 
    int t0 = (int) ((double)n * w / s);
    int t1 = (int) ((double)n * (w+1) / s);

    double *z0 = z + t0;
    double *z1 = z + t1;

    if (ALIGN >= 16 && ALIGN_OFFSET == 0)
    { /* Align split in z to cache line boundary to avoid false sharing */
      if (w != 0)   z0 = CACHE_ALIGN (z + t0);
      if (w != s-1) z1 = CACHE_ALIGN (z + t1);
    }
    else if (ALIGN >= 16)
    { /* Preserve up to 32-byte alignment */
      if (w != 0)   z0 = z + ((t0 + 2) & ~3);
      if (w != s-1) z1 = z + ((t1 + 2) & ~3);
    }

    int xrows = z1 - z0;
    x += z0 - z;
    z += z0 - z;

    helpers_size_t a = 0;
    int add = 0;

    while (a < k)
    { 
      helpers_size_t oa = a;
      helpers_size_t na = k-a <= 4 ? k : a+4;
      HELPERS_WAIT_IN2 (a, na-1, k);
      if (a < k) a &= ~3;

      matprod_mat_vec_sub_xrows0 (x+oa*n, y+oa, z0, n, a-oa, xrows, add);
      add = 1;
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t a = 0;
    int add = 0;

    while (a < k)
    { 
      helpers_size_t oa = a;
      helpers_size_t na = k-a <= 4 ? k : a+4;
      HELPERS_WAIT_IN2 (a, na-1, k);
      if (a < k) a &= ~3;

      matprod_mat_vec_sub (x+oa*n, y+oa, z, n, a-oa, add);
      add = 1;
    }
  }
}

/* Product of an n x 1 matrix (x) and a 1 x m matrix (y) with result stored
   in z, with pipelining of the input y and the output (by column). */

void task_par_matprod_outer (helpers_op_t op, helpers_var_ptr sz,
                             helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t n = LENGTH(sx);

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_scalar_vec (op, sz, sx, sy);
    }
    return;
  }

  helpers_size_t m = LENGTH(sy);

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_scalar_vec (op, sz, sy, sx);
    }
    return;
  }

  SETUP_SPLIT (4*s > m)

  if (s > 1)
  { 
    helpers_size_t d, d1, a, a1;

    d = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
    d1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;
    a = d;
    a1 = d1;

    while (d < d1)
    { 
      helpers_size_t od = d;
      helpers_size_t na = d1-d <= 4 ? a1 : d+4;
      HELPERS_WAIT_IN2 (a, na-1, a1);

      d = a;
      if (d < m) d &= ~3;
      if (d > d1) d = d1;

      matprod_outer (x, y+od, z+od*n, n, d-od EXTRA (z, z+od*n, w));
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t d = 0;
    helpers_size_t a = 0;

    while (d < m)
    { 
      helpers_size_t od = d;
      helpers_size_t na = m-d <= 4 ? m : d+4;
      HELPERS_WAIT_IN2 (a, na-1, m);
      d = a;
      if (d < m) d &= ~3;

      matprod_outer (x, y+od, z+od*n, n, d-od EXTRA (z, z+od*n, 0));
    }
  }
}


/* Product of an n x k matrix (x) and a k x m matrix (y) with result stored
   in z, with pipelining of the input y and the output (by column). */

void task_par_matprod_mat_mat (helpers_op_t op, helpers_var_ptr sz,
                               helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t k = OP_K(op);

  if (k == 1)
  { task_par_matprod_outer (op, sz, sx, sy);
    return;
  }

  helpers_size_t n_times_k = LENGTH(sx);
  helpers_size_t n = n_times_k / k;

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_vec_mat (op, sz, sx, sy);
    }
    return;
  }

  helpers_size_t k_times_m = LENGTH(sy);
  helpers_size_t m = k_times_m / k;

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_mat_vec (op, sz, sx, sy);
    }
    return;
  }

  SETUP_SPLIT (4*s > m)

  if (k == 0)
  { if (w == s-1)  /* do in only the last thread */
    { matprod_mat_mat (x, y, z, n, k, m EXTRA (z, z, 0));
    }
    return;
  }

  if (s > 1)
  { 
    helpers_size_t d, d1, a, a1;

    d = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
    d1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;
    a = d * k;
    a1 = d1 * k;

    while (d < d1)
    { 
      helpers_size_t od = d;
      helpers_size_t na = d1-d <= 4 ? a1 : k*(d+4);
      HELPERS_WAIT_IN2 (a, na-1, a1);

      d = a/k;
      if (d < m) d &= ~3;
      if (d > d1) d = d1;

      matprod_mat_mat (x, y+od*k, z+od*n, n, k, d-od EXTRA (z, z+od*n, w));
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t d = 0;
    helpers_size_t a = 0;

    while (d < m)
    { 
      helpers_size_t od = d;
      helpers_size_t na = m-d <= 4 ? k_times_m : k*(d+4);
      HELPERS_WAIT_IN2 (a, na-1, k_times_m);
      d = a/k;
      if (d < m) d &= ~3;

      matprod_mat_mat (x, y+od*k, z+od*n, n, k, d-od EXTRA (z, z+od*n, 0));
    }
  }
}


/* Product of the transpose of a k x n matrix (x) and a k x m matrix (y)
   with result stored in z, with pipelining of the input y and the output
   (by column). */

void task_par_matprod_trans1 (helpers_op_t op, helpers_var_ptr sz,
                              helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t k = OP_K(op);

  if (k == 1)
  { task_par_matprod_outer (op, sz, sx, sy);
    return;
  }

  helpers_size_t n_times_k = LENGTH(sx);
  helpers_size_t n = n_times_k / k;

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_vec_mat (op, sz, sx, sy);
    }
    return;
  }

  helpers_size_t k_times_m = LENGTH(sy);
  helpers_size_t m = k_times_m / k;

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_vec_mat (op, sz, sy, sx);
    }
    return;
  }

  SETUP_SPLIT (4*s > m)

  if (k == 0)
  { if (w == s-1)  /* do in only the last thread */
    { matprod_trans1 (x, y, z, n, k, m EXTRA (z, z, 0));
    }
    return;
  }

  int dosym = x==y && n==m && k>4 && (n>8 || k>8);

  if (s > 1)  /* do in more than one thread */
  { 
    helpers_size_t d, d1, a, a1;

    if (dosym)
    { d = w == 0 ? 0 : (helpers_size_t) (m*(1-sqrt(1-(double)w/s))) & ~3;
      d1 = w == s-1 ? m : (helpers_size_t) (m*(1-sqrt(1-(double)(w+1)/s))) & ~3;
    }
    else
    { d = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
      d1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;
    }

    a = d * k;
    a1 = d1 * k;

    while (d < d1)
    { 
      helpers_size_t od = d;
      helpers_size_t na = d1-d <= 4 ? a1 : k*(d+4);
      HELPERS_WAIT_IN2 (a, na-1, a1);

      d = a/k;
      if (d < m) d &= ~3;
      if (d > d1) d = d1;

      double *sym = dosym ? z+od*n+od : 0;

      matprod_trans1_sub (x, y+od*k, z+od*n, n, k, d-od, sym
                          EXTRA (z, z+od*n, w));
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    helpers_size_t d = 0;
    helpers_size_t a = 0;

    while (d < m)
    { 
      helpers_size_t od = d;
      helpers_size_t na = m-d <= 4 ? k_times_m : k*(d+4);
      HELPERS_WAIT_IN2 (a, na-1, k_times_m);
      d = a/k;
      if (d < m) d &= ~3;

      double *sym = dosym ? z+od*n+od : 0;

      matprod_trans1_sub (x, y+od*k, z+od*n, n, k, d-od, sym
                          EXTRA (z, z+od*n, 0));
    }
  }
}


/* Product of an n x k matrix (x) and the transpose of an m x k matrix (y)
   with result stored in z, with pipelining of the output (by column). */

void task_par_matprod_trans2 (helpers_op_t op, helpers_var_ptr sz,
                              helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t k = OP_K(op);

  if (k == 1)
  { task_par_matprod_outer (op, sz, sx, sy);
    return;
  }

  helpers_size_t n_times_k = LENGTH(sx);
  helpers_size_t n = n_times_k / k;

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_mat_vec (op, sz, sy, sx);
    }
    return;
  }

  helpers_size_t k_times_m = LENGTH(sy);
  helpers_size_t m = k_times_m / k;

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_mat_vec (op, sz, sx, sy);
    }
    return;
  }

  SETUP_SPLIT (4*s > m)

  if (k == 0)
  { if (w == s-1)  /* do in only the last thread */
    { matprod_trans2 (x, y, z, n, k, m EXTRA (z, z, 0));
    }
    return;
  }

  helpers_size_t a = 0;

  int dosym = x==y && n==m && k>4 && (n>8 || k>8);

  HELPERS_WAIT_IN2 (a, k_times_m-1, k_times_m);

  if (s > 1)
  { 
    helpers_size_t od, d;

    if (dosym)
    { od = w == 0 ? 0 : (helpers_size_t) (m*(1-sqrt(1-(double)w/s))) & ~3;
      d = w == s-1 ? m : (helpers_size_t) (m*(1-sqrt(1-(double)(w+1)/s))) & ~3;
    }
    else
    { od = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
      d = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;
    }

    if (d > od)
    { double *sym = dosym ? z+od*n+od : 0;
      matprod_trans2_sub (x, y+od, z+od*n, n, k, m, d-od, sym
                          EXTRA (z, z+od*n, w));
    }

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    double *sym = dosym ? z : 0;

    matprod_trans2_sub (x, y, z, n, k, m, m, sym
                        EXTRA (z, z, 0));
  }
}


/* Product of the transpose of an n x k matrix (x) and the transpose
   of an m x k matrix (y) with result stored in z, with no pipelining
   of inputs or output. */

void task_par_matprod_trans12 (helpers_op_t op, helpers_var_ptr sz,
                               helpers_var_ptr sx, helpers_var_ptr sy)
{
  double * MATPROD_RESTRICT x = REAL(sx);
  double * MATPROD_RESTRICT y = REAL(sy);
  double * MATPROD_RESTRICT z = REAL(sz);

  helpers_size_t k = OP_K(op);

  if (k == 1)
  { task_par_matprod_outer (op, sz, sx, sy);
    return;
  }

  helpers_size_t n_times_k = LENGTH(sx);
  helpers_size_t n = n_times_k / k;

  if (n <= 1)
  { if (n == 1)
    { task_par_matprod_mat_vec (op, sz, sy, sx);
    }
    return;
  }

  helpers_size_t k_times_m = LENGTH(sy);
  helpers_size_t m = k_times_m / k;

  if (m <= 1)
  { if (m == 1)
    { task_par_matprod_vec_mat (op, sz, sy, sx);
    }
    return;
  }

  SETUP_SPLIT (4*s > m)

  if (k == 0)
  { if (w == s-1)  /* do in only the last thread */
    { matprod_trans12 (x, y, z, n, k, m);
    }
    return;
  }

  helpers_size_t a = 0;

  HELPERS_WAIT_IN2 (a, k_times_m-1, k_times_m);

  if (s > 1)
  { 
    helpers_size_t d, d1;
    d = w == 0 ? 0 : (helpers_size_t) ((double)m * w / s) & ~3;
    d1 = w == s-1 ? m : (helpers_size_t) ((double)m * (w+1) / s) & ~3;

    matprod_trans12_sub (x, y+d, z+(size_t)d*n, n, k, m, d1-d);

    if (w != 0) WAIT_FOR_EARLIER_TASKS(sz);
  }

  else  /* only one thread */
  { 
    matprod_trans12 (x, y, z, n, k, m);
  }
}

/* -------------------------------------------------------------------------- */

#define DECIDE_SPLIT(size,cond) \
  int s = split < 0 ? -split : split; \
  if (s > (size)) s = (size); \
  if (split > 0) \
    while (s > 1 && (cond)) s -= 1;


#define MINMUL 8192   /* Desired minimum number of multiplies per thread */


void par_matprod_scalar_vec (helpers_var_ptr z, helpers_var_ptr x,
                             helpers_var_ptr y, int split, int pipe)
{
  helpers_size_t m = LENGTH(z);

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_scalar_vec (*REAL(x), REAL(y), REAL(z), m EXTRAZ);
    return;
  }

  DECIDE_SPLIT (m, s > 2 || MINMUL*s > m)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_scalar_vec,
                       MAKE_OP(w,s,0), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2_OUT : 0,
                     task_par_matprod_scalar_vec,
                     0, z, x, y);
  }
}


#define DISABLE_VEC_VEC_SPLIT 1  /* Splitting dot products may be undesirable */

void par_matprod_vec_vec (helpers_var_ptr z, helpers_var_ptr x,
                          helpers_var_ptr y, int split, int pipe)
{
  helpers_size_t k = LENGTH(x);

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    REAL(z)[0] = matprod_vec_vec (REAL(x), REAL(y), k);
    return;
  }

  /* Always use only one thread. */

  helpers_do_task (pipe ? HELPERS_PIPE_IN2 : 0,
                   task_par_matprod_vec_vec,
                   0, z, x, y);
}


void par_matprod_vec_mat (helpers_var_ptr z, helpers_var_ptr x,
                          helpers_var_ptr y, int split, int pipe)
{
  helpers_size_t k = LENGTH(x);
  helpers_size_t m = LENGTH(z);

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_vec_mat (REAL(x), REAL(y), REAL(z), k, m EXTRAZ);
    return;
  }

  double multiplies = (double)k*m;

  DECIDE_SPLIT (m, s > 3 && k < 4 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_vec_mat,
                       MAKE_OP(w,s,0), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2_OUT : 0,
                     task_par_matprod_vec_mat,
                     0, z, x, y);
  }
}


void par_matprod_mat_vec (helpers_var_ptr z, helpers_var_ptr x,
                          helpers_var_ptr y, int split, int pipe)
{
  helpers_size_t n = LENGTH(z);
  helpers_size_t k = LENGTH(y);

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_mat_vec (REAL(x), REAL(y), REAL(z), n, k);
    return;
  }

  double multiplies = (double)n*k;

  DECIDE_SPLIT (n, s > 3 && k < 4 
                     || (ALIGN >= 16 && ALIGN_OFFSET==0 ? 8*s : 32*s) > n
                     || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02:HELPERS_PIPE_IN0),
                       task_par_matprod_mat_vec,
                       MAKE_OP(w,s,0), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2 : 0,
                     task_par_matprod_mat_vec,
                     0, z, x, y);
  }
}


void par_matprod_outer (helpers_var_ptr z, helpers_var_ptr x,
                        helpers_var_ptr y, int split, int pipe)
{
  helpers_size_t n = LENGTH(x);
  helpers_size_t m = LENGTH(y);

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_outer (REAL(x), REAL(y), REAL(z), n, m EXTRAZ);
    return;
  }

  double multiplies = (double)n*m;

  DECIDE_SPLIT (m, s > 2 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_outer,
                       MAKE_OP(w,s,0), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2_OUT : 0,
                     task_par_matprod_outer,
                     0, z, x, y);
  }
}


void par_matprod_mat_mat (helpers_var_ptr z, helpers_var_ptr x,
                          helpers_var_ptr y, int k, int split, int pipe)
{
  helpers_size_t n = LENGTH(x) / k;
  helpers_size_t m = LENGTH(y) / k;

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_mat_mat (REAL(x), REAL(y), REAL(z), n, k, m EXTRAZ);
    return;
  }

  double multiplies = (double)n*k*m;

  DECIDE_SPLIT (m, s > 3 && k < 4 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_mat_mat,
                       MAKE_OP(w,s,k), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2_OUT : 0,
                     task_par_matprod_mat_mat,
                     k, z, x, y);
  }
}


void par_matprod_trans1 (helpers_var_ptr z, helpers_var_ptr x,
                         helpers_var_ptr y, int k, int split, int pipe)
{
  helpers_size_t n = LENGTH(x) / k;
  helpers_size_t m = LENGTH(y) / k;

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_trans1 (REAL(x), REAL(y), REAL(z), n, k, m EXTRAZ);
    return;
  }

  double multiplies = (double)n*k*m;

  DECIDE_SPLIT (m, s > 3 && k < 4 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? (pipe?HELPERS_PIPE_IN2_OUT:HELPERS_PIPE_OUT) :
                       w < s-1 ? HELPERS_PIPE_IN02_OUT
                               : (pipe?HELPERS_PIPE_IN02_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_trans1,
                       MAKE_OP(w,s,k), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_IN2_OUT : 0,
                     task_par_matprod_trans1,
                     k, z, x, y);
  }
}


void par_matprod_trans2 (helpers_var_ptr z, helpers_var_ptr x,
                         helpers_var_ptr y, int k, int split, int pipe)
{
  helpers_size_t n = LENGTH(x) / k;
  helpers_size_t m = LENGTH(y) / k;

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_trans2 (REAL(x), REAL(y), REAL(z), n, k, m EXTRAZ);
    return;
  }

  double multiplies = (double)n*k*m;

  DECIDE_SPLIT (m, s > 3 && k < 4 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? HELPERS_PIPE_OUT :
                       w < s-1 ? HELPERS_PIPE_IN0_OUT
                               : (pipe?HELPERS_PIPE_IN0_OUT:HELPERS_PIPE_IN0),
                       task_par_matprod_trans2,
                       MAKE_OP(w,s,k), z, x, y);
    }
  }
  else
  { helpers_do_task (pipe ? HELPERS_PIPE_OUT : 0,
                     task_par_matprod_trans2,
                     k, z, x, y);
  }
}


void par_matprod_trans12 (helpers_var_ptr z, helpers_var_ptr x,
                          helpers_var_ptr y, int k, int split, int pipe)
{
  helpers_size_t n = LENGTH(x) / k;
  helpers_size_t m = LENGTH(y) / k;

  if (split == 0)
  { helpers_wait_until_not_being_computed(y);
    matprod_trans12 (REAL(x), REAL(y), REAL(z), n, k, m);
    return;
  }

  double multiplies = (double)n*k*m;

  DECIDE_SPLIT (m, s > 3 && k < 4 || 4*s > m || MINMUL*s > multiplies)

  if (s > 1)
  { int w;
    for (w = 0; w < s; w++)
    { helpers_do_task (w == 0  ? HELPERS_PIPE_OUT :
                       w < s-1 ? HELPERS_PIPE_IN0_OUT
                               : HELPERS_PIPE_IN0,
                       task_par_matprod_trans12,
                       MAKE_OP(w,s,k), z, x, y);
    }
  }
  else
  { helpers_do_task (0,
                     task_par_matprod_trans12,
                     k, z, x, y);
  }
}
