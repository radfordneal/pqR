/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION
             C Procedures for Matrix Multiplication Without Pipelining

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


#include <stdlib.h>
#include <stdint.h>


/* TUNABLE CACHE OPTIMIZATION CONSTANTS.  These values are used to
   divide computations into parts, so as to improve cache behaviour.
   They are chosen based on the assumed sizes of the L1 and L2 caches.
   See imp-doc for more information.

   The values below should all be multiples of 8 in order to avoid any
   changes to alignment. */

#define VEC_MAT_YROWS (512+256)  /* # of rows to look at at once */

#define MAT_VEC_XROWS (1024+512) /* # of elements to compute in a part */

#define OUTER_ROWS (2048-128)    /* # of rows to compute in a part */

#define MAT_MAT_XROWS (1024-64)  /* # of rows to compute in a part */
#define MAT_MAT_XCOLS 32         /* # of columns in part, if # of rows is max */

#define TRANS1_XROWS (512+128)   /* # of rows to look at at once */
#define TRANS1_XCOLS 48          /* # of columns in part, if # of rows is max */

#define TRANS2_XROWS (1024-64)   /* # of rows to compute in a part */
#define TRANS2_XCOLS 32          /* # of columns in part, if # of rows is max */

#define TRANS12_ZELEM 2048       /* # of elements can store locally on stack */

#define DOUBLES_IN_LLC 95000     /* # of doubles assumed to fit in the
                                    last-level cache with some space to spare */


/* SETUP TO FACILITATE INCLUSION AND USE OF MATPROD.C IN PIPED-MATPROD.C.
   If PAR_MATPROD is defined, SCOPE, AMTOUT, EXTRAD, and EXTRAN will
   be defined before matprod.c is included.  Otherwise, they are
   defined here as nothing.

   Also, matprod.h and perhaps matprod-app.h are included only if
   PAR_MATPROD is not defined. */

#ifndef PAR_MATPROD

# ifdef MATPROD_APP_INCLUDED
#   include "matprod-app.h"
# endif

# include "matprod.h"

# define SCOPE
# define AMTOUT(z) do {} while (0)
# define EXTRAD
# define EXTRAN
# define EXTRAZ

#endif


/* DEBUGGING FACILITIES. */

#define DEBUG_PRINTF 0     /* Set to 1 to enable printf of procedure args */

#if DEBUG_PRINTF
# ifdef PAR_MATPROD
#   define debug_printf helpers_debug
# else
#   include <stdio.h>
#   define debug_printf printf
# endif
#endif

#define NDEBUG           /* Define to disable assertion check */
#include <assert.h>

#define CHK_ALIGN(p) assert (((uintptr_t)(p)&(ALIGN-1)) == ALIGN_OFFSET)


/* SET UP ALIGNMENT DEFINITIONS. */

#ifndef ALIGN
# define ALIGN 1
#endif

#ifndef ALIGN_OFFSET
# define ALIGN_OFFSET 0
#endif

#if ALIGN < 0 || (ALIGN & (ALIGN-1)) != 0
# error "alignment must be a power of two"
#endif

#if ALIGN_OFFSET >= ALIGN
# error "alignment offset must be less than alignment"
#endif

#if ALIGN_OFFSET % 8 != 0
# error "alignment offset must be a multiple of eight"
#endif

#define ALIGN_FORWARD ((ALIGN - ALIGN_OFFSET) % ALIGN)

#if ALIGN >= 8 && __GNUC__ && !defined(NO_ASSUME_ALIGNED)
# define CAN_ASSUME_ALIGNED 1
# define ASSUME_ALIGNED(x,a,o) __builtin_assume_aligned(x,a,o)
#else
# define CAN_ASSUME_ALIGNED 0
# define ASSUME_ALIGNED(x,a,o) (x)
#endif


/* SET UP SIMD DEFINITIONS. */

#if __SSE2__ && !defined(DISABLE_SIMD_CODE)
# define CAN_USE_SSE2 1
#else
# define CAN_USE_SSE2 0
#endif

#if __SSE3__ && !defined(DISABLE_SIMD_CODE)
# define CAN_USE_SSE3 1
#else
# define CAN_USE_SSE3 0
#endif

#if __AVX__ && !defined(DISABLE_SIMD_CODE) && !defined(DISABLE_AVX_CODE)
# define CAN_USE_AVX 1
#else
# define CAN_USE_AVX 0
#endif

#if __AVX2__ && !defined(DISABLE_SIMD_CODE) && !defined(DISABLE_AVX_CODE)
# define CAN_USE_AVX2 1
#else
# define CAN_USE_AVX2 0
#endif

#if CAN_USE_SSE2 || CAN_USE_SSE3 || CAN_USE_AVX || CAN_USE_AVX2
# include <immintrin.h>
#endif


/* VERSIONS OF LOAD AND STORE TAKING ADVANTAGE OF KNOWN ALIGNMENT.
   The loadA and storeA macros do an aligned load/store if ALIGN is
   suitably large, assuming that any offset has been compensated for.
   The loadAA and storeAA macro do an unalign load/store only if ALIGN
   is suitably large and ALIGN_OFFSET mod the required alignment is
   zero, as is appropriate for an address that is one of the arguments
   plus a multiple of ALIGN. */

#define _mm_loadA_pd(w) \
  (ALIGN>=16 ? _mm_load_pd(w) : _mm_loadu_pd(w))
#define _mm_storeA_pd(w,v) \
  (ALIGN>=16 ? _mm_store_pd(w,v) : _mm_storeu_pd(w,v))
#define _mm256_loadA_pd(w) \
  (ALIGN>=32 ? _mm256_load_pd(w) : _mm256_loadu_pd(w))
#define _mm256_storeA_pd(w,v) \
  (ALIGN>=32 ? _mm256_store_pd(w,v) : _mm256_storeu_pd(w,v))

#define _mm_loadAA_pd(w) \
  (ALIGN>=16 && ALIGN_OFFSET%16==0 ? _mm_load_pd(w) : _mm_loadu_pd(w))
#define _mm_storeAA_pd(w,v) \
  (ALIGN>=16 && ALIGN_OFFSET%16==0 ? _mm_store_pd(w,v) : _mm_storeu_pd(w,v))
#define _mm256_loadAA_pd(w) \
  (ALIGN>=32 && ALIGN_OFFSET==0 ? _mm256_load_pd(w) : _mm256_loadu_pd(w))
#define _mm256_storeAA_pd(w,v) \
  (ALIGN>=32 && ALIGN_OFFSET==0 ? _mm256_store_pd(w,v) : _mm256_storeu_pd(w,v))


/* CONDITIONAL CAST FOR AVX VS. SSE.  Macro to cast a variable to
   __m128d if it is __m256d, which does nothing when AVX is not
   available, and hence the variable will already be __m128d.  This
   facilitates sharing of code in AVX and SSE sections.

   The cast128a version is for use when the AVX code is enabled only if
   ENABLE_ALL_AVX_CODE is defined. */

#if CAN_USE_AVX || CAN_USE_AVX2
# define cast128(x) _mm256_castpd256_pd128(x)
#else
# define cast128(x) (x)
#endif

#if (CAN_USE_AVX || CAN_USE_AVX2) && ENABLE_ALL_AVX_CODE
# define cast128a(x) _mm256_castpd256_pd128(x)
#else
# define cast128a(x) (x)
#endif


/* MACRO TO SPLIT COUNT, ALIGNED.  Given that M < c < 2*M, with M a
   multiple of 4, finds a value v s.t. v <= M and c-v <= M, and v is 
   a multiple of 4. */

#define SPLITC(c,M) M  /* trivial method for now, not necessarily best */


/* Set vector/matrix z with s elements to all zeros.  This is a
   degenerate special case of other operations. */

static void set_to_zeros (double * MATPROD_RESTRICT z, size_t s)
{
  size_t i;
  for (i = 0; i < s; i++)
  { z[i] = 0.0;
  }
}

/* -------------------------------------------------------------------------- */
/*                              FILL_LOWER                                    */

/* Fill the lower triangle of an n-by-n matrix from the upper
   triangle.  Fills two rows at once to improve cache performance. */

SCOPE void matprod_fill_lower (double * MATPROD_RESTRICT z, int n)
{
  CHK_ALIGN(z);

  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Inner block because this is maybe required by "restrict" usage. */
  { 
    /* Note that the first row requires no action, and hence can be
       done or not as helps alignment. */

#   if ALIGN_FORWARD & 8
      double * MATPROD_RESTRICT zr = z + 1;
      double * MATPROD_RESTRICT zc = z + n;
      int i = 1;
#   else
      double * MATPROD_RESTRICT zr = z;
      double * MATPROD_RESTRICT zc = z;
      int i = 0;
#   endif

    /* Fill in two rows each time around this loop. */

    while (i <= n-2)
    { 
      double *zp = zr;
      int j = 0;

      while (j <= i-2)
      {
#       if CAN_USE_SSE2
        { _mm_storeA_pd (zp,
                      _mm_loadh_pd (_mm_load_sd(zc+j), zc+j+n));
          _mm_storeu_pd (zp+n,
                      _mm_loadh_pd (_mm_load_sd(zc+j+1), zc+j+n+1));
        }
#       else  /* non-SIMD code */
        { zp[0] = zc[j];
          zp[1] = (zc+n)[j];
          (zp+n)[0] = zc[j+1];
          (zp+n)[1] = (zc+n)[j+1];
        }
#       endif
        zp += n; zp += n;
        j += 2;
      }

      if (j < i)
      {
#       if CAN_USE_SSE2
        { _mm_storeA_pd (zp, _mm_loadh_pd (_mm_load_sd(zc+j), zc+j+n));
        }
#       else  /* non-SIMD code */
        { zp[0] = zc[j];
          zp[1] = (zc+n)[j];
        }
#       endif
        zp += n;
      }

      zp[1] = (zc+n)[i];

      zc += n; zc += n;
      zr += 2;
      i += 2;
    }

    /* Fill in the last row, if not done above. */

    if (i < n)
    { double *zp = zr;
      int j = 0;
      while (j < i)
      { zp[0] = zc[j];
        zp += n;
        j += 1;
      }
    }
  }
}

/* -------------------------------------------------------------------------- */
/*                              SCALAR_VEC                                    */

/* Multiply vector y of length m by scalar x, storing result in vector z. */

#define SCALAR_VEC_THRESH 1024  /* amount threshold for output pipelining */

SCOPE void matprod_scalar_vec (double x, double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("scalar_vec %f %p %p - %d\n",
                              x, y, z,   m);
# endif

  CHK_ALIGN(y); CHK_ALIGN(z);

  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Handle m of 0, 1, or 2 specially. */

  if (m <= 2)
  { if (m == 2)
    { z[0] = x * y[0];
      z[1] = x * y[1];
    }
    else if (m == 1)
    { z[0] = x * y[0];
    }
    return;
  }

  int i = 0;  /* indexes y and z */

# if CAN_USE_SSE2 || CAN_USE_AVX
  {
#   if CAN_USE_AVX
      __m256d X = _mm256_set1_pd (x);
#   else
      __m128d X = _mm_set1_pd (x);
#   endif

#   if ALIGN_FORWARD & 8
    { _mm_store_sd (z, _mm_mul_sd (cast128(X), _mm_load_sd(y)));
      i += 1;
    }
#   endif

#   if ALIGN_FORWARD & 16
    { _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
      i += 2;
    }
#   endif

#   if CAN_USE_AVX
    { 
#     ifdef PAR_MATPROD
      { while (i < m-SCALAR_VEC_THRESH)
        { int e = i+SCALAR_VEC_THRESH;
          while (i < e)
          { _mm256_storeA_pd (z+i, _mm256_mul_pd (X, _mm256_loadA_pd(y+i)));
            i += 4;
          }
          AMTOUT(z+i);
        }
      }
#     endif
      while (i <= m-4)
      { _mm256_storeA_pd (z+i, _mm256_mul_pd (X, _mm256_loadA_pd(y+i)));
        i += 4;
      }
    }
#   else  /* CAN_USE_SSE2 */
    { 
#     ifdef PAR_MATPROD
      { while (i < m-SCALAR_VEC_THRESH)
        { int e = i+SCALAR_VEC_THRESH;
          while (i < e)
          { _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
            i += 2;
            _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
            i += 2;
          }
          AMTOUT(z+i);
        }
      }
#     endif
      while (i <= m-4)
      { _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
        i += 2;
        _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
        i += 2;
      }
    }
#   endif

    if (i <= m-2)
    { _mm_storeA_pd (z+i, _mm_mul_pd (cast128(X), _mm_loadA_pd(y+i)));
      i += 2;
    }

    if (i < m)
    { _mm_store_sd (z+i, _mm_mul_sd (cast128(X), _mm_load_sd(y+i)));
    }
  }

# else  /* non-SIMD code */
  {
#   ifdef PAR_MATPROD
    { while (i < m-SCALAR_VEC_THRESH)
      { int e = i+SCALAR_VEC_THRESH;
        while (i < e)
        { z[i] = x * y[i];
          i += 1;
        }
        AMTOUT(z+i);
      }
    }
#   endif
    while (i < m)
    { z[i] = x * y[i];
      i += 1;
    }
  }
# endif
}

/* -------------------------------------------------------------------------- */
/*                                VEC_VEC                                     */

/* Dot product of vectors x and y of length k, with result returned as the
   function value. */

static double matprod_vec_vec_sub(double * MATPROD_RESTRICT x,
                                  double * MATPROD_RESTRICT y, int k, double s);

SCOPE double matprod_vec_vec (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y, int k)
{
# if DEBUG_PRINTF
    debug_printf("vec_vec %p %p - %d\n",
                           x, y,   k);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);

  return matprod_vec_vec_sub (x, y, k, 0.0);
}

static double matprod_vec_vec_sub (double * MATPROD_RESTRICT x,
                                   double * MATPROD_RESTRICT y, int k, double s)
{
# if DEBUG_PRINTF
    debug_printf("vec_vec_sub %p %p %f - %d\n",
                               x, y, s,   k);
# endif

  /* Handle k = 0, 1, or 2 specially. */

  if (k <= 2)
  { if (k == 2)
      return s + x[0] * y[0] + x[1] * y[1];
    else if (k == 1)
      return s + x[0] * y[0];
    else  /* k <= 0 */
      return s;
  }

  int i = 0;

  /* Use an unrolled loop to add most products, perhaps using SSE2
     or AVX instructions. */

# if CAN_USE_SSE2 || CAN_USE_AVX
  {
    __m128d S, A;

    S = _mm_set_sd(s);

#   if (ALIGN_FORWARD & 8)
      S = _mm_add_sd (S, _mm_mul_sd (_mm_load_sd(x), _mm_load_sd(y)));
      i += 1;
#   endif

#   if (ALIGN_FORWARD & 16)
      A = _mm_mul_pd (_mm_loadA_pd(x+i), _mm_loadA_pd(y+i));
      S = _mm_add_sd (A, S);
      A = _mm_unpackhi_pd (A, A);
      S = _mm_add_sd (A, S);
      i += 2;
#   endif

#   if CAN_USE_AVX && ENABLE_ALL_AVX_CODE
    {
      __m256d AA;

      while (i <= k-8)
      { AA = _mm256_mul_pd (_mm256_loadA_pd(x+i),
                        _mm256_loadA_pd(y+i));
        A = cast128a(AA);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        A = _mm256_extractf128_pd(AA,1);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        AA = _mm256_mul_pd (_mm256_loadA_pd(x+i+4),
                        _mm256_loadA_pd(y+i+4));
        A = cast128a(AA);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        A = _mm256_extractf128_pd(AA,1);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        i += 8;
      }

      if (i <= k-4)
      { AA = _mm256_mul_pd (_mm256_loadA_pd(x+i),
                        _mm256_loadA_pd(y+i));
        A = cast128a(AA);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        A = _mm256_extractf128_pd(AA,1);
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        i += 4;
      }
    }

#   else  /* CAN_USE_SSE2 */
    {
      while (i <= k-4)
      { A = _mm_mul_pd (_mm_loadA_pd(x+i), _mm_loadA_pd(y+i));
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        A  = _mm_mul_pd (_mm_loadA_pd(x+i+2), _mm_loadA_pd(y+i+2));
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        i += 4;
      }
    }
#   endif

    if (i <= k-2)
    { A = _mm_mul_pd (_mm_loadA_pd(x+i), _mm_loadA_pd(y+i));
      S = _mm_add_sd (A, S);
      A = _mm_unpackhi_pd (A, A);
      S = _mm_add_sd (A, S);
      i += 2;
    }

    if (i < k)
    { A = _mm_mul_sd (_mm_load_sd(x+i), _mm_load_sd(y+i));
      S = _mm_add_sd (A, S);
    }

    _mm_store_sd (&s, S);
  }

# else  /* non-SIMD code */
  {
#   if (ALIGN_FORWARD & 8)
      s += x[0] * y[0];
      i += 1;
#   endif

#   if (ALIGN_FORWARD & 16)
      s += x[i] * y[i];
      s += x[i+1] * y[i+1];
      i += 2;
#   endif

    while (i <= k-4)
    { s += x[i+0] * y[i+0];
      s += x[i+1] * y[i+1];
      s += x[i+2] * y[i+2];
      s += x[i+3] * y[i+3];
      i += 4;
    }

    if (i <= k-2)
    { s += x[i+0] * y[i+0];
      s += x[i+1] * y[i+1];
      i += 2;
    }

    if (i < k)
    { s += x[i] * y[i];
    }
  }
# endif

  return s;
}

/* -------------------------------------------------------------------------- */
/*                                VEC_MAT                                     */

/* Product of row vector (x) of length k and k x m matrix (y), result
   stored in z.

   Cases where k is 0, 1, or 2 and where m is 0 or 1 are handled
   specially. */

static void matprod_vec_mat_sub_yrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int k, int m, int yrows, int add EXTRAD);

static void matprod_vec_mat_k2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z, int m);

SCOPE void matprod_vec_mat (double * MATPROD_RESTRICT x,
                            double * MATPROD_RESTRICT y,
                            double * MATPROD_RESTRICT z, int k, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("vec_mat %p %p %p - %d %d\n",
                           x, y, z,   k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (m <= 1)
  { if (m == 1)
    { z[0] = matprod_vec_vec (x, y, k);
    }
    return;
  }

  /* Specially handle cases where y has two or fewer rows. */

  if (k <= 2)
  { 
    if (k == 2)
    { matprod_vec_mat_k2 (x, y, z, m);
    }
    else if (k == 1)
    { matprod_scalar_vec (x[0], y, z, m EXTRAN);
    }
    else  /* k == 0 */
    { set_to_zeros (z, m);
    }

    return;
  }

  /* The general case with k > 2.  Calls matprod_vec_mat_sub_yrows to do parts
     (only one part if y is a matrix with fewer than VEC_MAT_YROWS). */

  int yrows = k;
  int add = 0;

  if (yrows > VEC_MAT_YROWS && m > 4)
  { while (yrows >= 2*VEC_MAT_YROWS)
    { matprod_vec_mat_sub_yrows(x, y, z, k, m, VEC_MAT_YROWS, add EXTRAZ);
      x += VEC_MAT_YROWS;
      y += VEC_MAT_YROWS;
      yrows -= VEC_MAT_YROWS;
      add = 1;
    }
    if (yrows > VEC_MAT_YROWS)
    { int nr = SPLITC (yrows, VEC_MAT_YROWS); 
      matprod_vec_mat_sub_yrows(x, y, z, k, m, nr, add EXTRAZ);
      x += nr;
      y += nr;
      yrows -= nr;
      add = 1;
    }
  }
  matprod_vec_mat_sub_yrows (x, y, z, k, m, yrows, add EXTRAN);
}


/* Multiply the first 'yrows' elements of vector x with the first
   'yrows' rows of matrix y, storing the result in z if 'add' is zero,
   or adding the result to z if 'add' is non-zero.  Note that x and y
   may not be the start of the original vector/matrix.  The k argument
   is the number of rows in the original y, which is the amount to
   step to go right to an element in the same row and the next column.
   The m argument is the number of columns in y.

   The same alignment assumptions hold for x, y, and z as with the
   visible procedures.

   Note that k and 'yrows' will be greater than 2. */

static void matprod_vec_mat_sub_yrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int k, int m, int yrows, int add EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("vec_mat_sub_yrows %p %p %p - %d %d %d\n",
                                     x, y, z,   k, m, yrows);
# endif

  assert (k > 2);
  assert (yrows > 2);

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Each iteration of this loop computes four consecutive elements
     of the result vector, by doing four dot products of x with
     columns of y.  Adjusts y, z, and m as it goes.

     For SIMD code, the sums for the dot products are initialized to
     from zero to three dot products, as helps alignment, plus the
     current value in z, if 'add' is set. */

  while (m >= 4)
  { 
    int i = 0;

#   if CAN_USE_AVX
    {
      __m256d S, B;

      if (add)
        S = _mm256_loadu_pd(z);
      else
        S = _mm256_setzero_pd();

#     if ALIGN_FORWARD & 8
        B = _mm256_set_pd ((y+k+k+k)[i], (y+k+k)[i], (y+k)[i], y[i]);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i]), B);
        S = _mm256_add_pd (B, S);
        i += 1;
#     endif

#     if ALIGN_FORWARD & 16
      {
        __m128d Y0, Y1, Y2, Y3;
        __m256d T0, T1;
        Y0 = _mm_loadA_pd(y+i);
        Y1 = _mm_loadu_pd(y+i+k);
        Y2 = _mm_loadA_pd(y+i+k+k);
        Y3 = _mm_loadu_pd(y+i+k+k+k);
        T0 = _mm256_castpd128_pd256 (Y0);
        T0 = _mm256_insertf128_pd (T0, Y2, 1);
        T1 = _mm256_castpd128_pd256 (Y1);
        T1 = _mm256_insertf128_pd (T1, Y3, 1);
        B = _mm256_unpacklo_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i]), B);
        S = _mm256_add_pd (B, S);
        B = _mm256_unpackhi_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i+1]), B);
        S = _mm256_add_pd (B, S);
        i += 2;
      }
#     endif

      while (i <= yrows-4)
      { __m256d Y0, Y1, Y2, Y3;
        __m256d T0, T1;
        Y0 = _mm256_loadA_pd(y+i);
        Y1 = _mm256_loadu_pd(y+i+k);
        Y2 = _mm256_loadu_pd(y+i+k+k);
        Y3 = _mm256_loadu_pd(y+i+k+k+k);
        T0 = _mm256_permute2f128_pd (Y0, Y2, 0x20);
        T1 = _mm256_permute2f128_pd (Y1, Y3, 0x20);
        B = _mm256_unpacklo_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i]), B);
        S = _mm256_add_pd (B, S);
        B = _mm256_unpackhi_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i+1]), B);
        S = _mm256_add_pd (B, S);
        T0 = _mm256_permute2f128_pd (Y0, Y2, 0x31);
        T1 = _mm256_permute2f128_pd (Y1, Y3, 0x31);
        B = _mm256_unpacklo_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i+2]), B);
        S = _mm256_add_pd (B, S);
        B = _mm256_unpackhi_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i+3]), B);
        S = _mm256_add_pd (B, S);
        i += 4;
      }

      if (i <= yrows-2)
      { __m128d Y0, Y1, Y2, Y3;
        __m256d T0, T1;
        Y0 = _mm_loadA_pd(y+i);
        Y1 = _mm_loadu_pd(y+i+k);
        Y2 = _mm_loadA_pd(y+i+k+k);
        Y3 = _mm_loadu_pd(y+i+k+k+k);
        T0 = _mm256_castpd128_pd256 (Y0);
        T0 = _mm256_insertf128_pd (T0, Y2, 1);
        T1 = _mm256_castpd128_pd256 (Y1);
        T1 = _mm256_insertf128_pd (T1, Y3, 1);
        B = _mm256_unpacklo_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i]), B);
        S = _mm256_add_pd (B, S);
        B = _mm256_unpackhi_pd (T0, T1);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i+1]), B);
        S = _mm256_add_pd (B, S);
        i += 2;
      }

      if (i < yrows)
      { B = _mm256_set_pd ((y+k+k+k)[i], (y+k+k)[i], (y+k)[i], y[i]);
        B = _mm256_mul_pd (_mm256_set1_pd(x[i]), B);
        S = _mm256_add_pd (B, S);
      }

      _mm256_storeu_pd (z, S);
    }

#   elif CAN_USE_SSE2 && ALIGN >= 16 /* works, but slower, when unaligned */
    {
      __m128d S0, S1;

      if (add)
      { S0 = _mm_loadu_pd(z);
        S1 = _mm_loadu_pd(z+2);
      }
      else
      { S0 = _mm_setzero_pd();
        S1 = _mm_setzero_pd();
      }

#     if ALIGN_FORWARD & 8
      {
        __m128d P = _mm_set1_pd(x[i]);
        S0 = _mm_add_pd(S0, _mm_mul_pd (P,
                            _mm_set_pd ((y+k)[i], y[i])));
        S1 = _mm_add_pd(S1, _mm_mul_pd (P,
                            _mm_set_pd ((y+k+k+k)[i], (y+k+k)[i])));
        i += 1;
      }
#     endif

      if (ALIGN < 16 || (k & 1))  /* no alignment for second column */
      { while (i <= yrows-2)
        { __m128d T0, T1;
          __m128d P = _mm_loadA_pd(x+i);
          T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
          T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
          S0 = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S0);
          S0 = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S0);
          T0 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
          T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k+k+k), P);
          S1 = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S1);
          S1 = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S1);
          i += 2;
        }
      }
      else  /* second column has same 16-byte alignment as first */
      { while (i <= yrows-2)
        { __m128d T0, T1;
          __m128d P = _mm_loadA_pd(x+i);
          T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
          T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k), P);
          S0 = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S0);
          S0 = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S0);
          T0 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
          T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k+k), P);
          S1 = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S1);
          S1 = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S1);
          i += 2;
        }
      }

      if (i < yrows)
      { __m128d P = _mm_set1_pd(x[i]);
        S0 = _mm_add_pd (S0,
             _mm_mul_pd (P, _mm_set_pd((y+k)[i],y[i])));
        S1 = _mm_add_pd (S1,
             _mm_mul_pd (P, _mm_set_pd((y+k+k+k)[i],(y+k+k)[i])));
      }

      _mm_storeu_pd (z, S0);
      _mm_storeu_pd (z+2, S1);
    }

#   else  /* non-SIMD code */
    {
      double s[4];

      if (add)
      { s[0] = z[0];
        s[1] = z[1];
        s[2] = z[2];
        s[3] = z[3];
      }
      else
        s[0] = s[1] = s[2] = s[3] = 0.0;

      while (i <= yrows-2)
      { s[0] += x[i+0] * y[i];
        s[1] += x[i+0] * (y+k)[i];
        s[2] += x[i+0] * (y+k+k)[i];
        s[3] += x[i+0] * (y+k+k+k)[i];
        s[0] += x[i+1] * y[i+1];
        s[1] += x[i+1] * (y+k)[i+1];
        s[2] += x[i+1] * (y+k+k)[i+1];
        s[3] += x[i+1] * (y+k+k+k)[i+1];
        i += 2;
      }

      if (i < yrows)
      { s[0] += x[i] * y[i];
        s[1] += x[i] * (y+k)[i];
        s[2] += x[i] * (y+k+k)[i];
        s[3] += x[i] * (y+k+k+k)[i];
      }

      z[0] = s[0];
      z[1] = s[1];
      z[2] = s[2];
      z[3] = s[3];
    }

#   endif

    y += k; y += k; y += k; y += k;
    z += 4;
    m -= 4;

    AMTOUT(z);
  }

  /* Compute the final few dot products left over from the loop above. */

  if (m == 3)  /* Do three more dot products */
  { 
    int i = 0;

#   if CAN_USE_AVX || CAN_USE_SSE2 && ALIGN >= 16  /* slower unaligned */
    {
      __m128d S, S2;

      if (add)
      { S = _mm_loadu_pd(z);
        S2 = _mm_load_sd(z+2);
      }
      else
      { S = _mm_setzero_pd();
        S2 = _mm_setzero_pd();
      }

#     if ALIGN_FORWARD & 8
      { __m128d P = _mm_set1_pd(x[i]);
        S = _mm_add_pd (S, _mm_mul_pd (P, _mm_set_pd ((y+k)[i], y[i])));
        S2 = _mm_add_sd (S2, _mm_mul_sd (P, _mm_set_sd ((y+k+k)[i])));
        i += 1;
      }
#     endif

#     if ALIGN_FORWARD & 16
      { __m128d P = _mm_loadA_pd(x+i);
        __m128d T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
        __m128d T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
        __m128d T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
        S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
        S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
        S2 = _mm_add_sd (T2, S2);
        S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
        i += 2;
      }
#     endif

#     if CAN_USE_AVX
        if (ALIGN >= 16 && (k & 1))
        { while (i <= yrows-4)
          { __m256d P = _mm256_loadA_pd(x+i);
            __m256d T0 = _mm256_mul_pd (_mm256_loadA_pd(y+i), P);
            __m256d T1 = _mm256_mul_pd (_mm256_loadu_pd(y+i+k), P);
            __m256d T2 = _mm256_mul_pd (_mm256_loadu_pd
                              (ASSUME_ALIGNED (y+i+k+k, 16, 0)), P);
            __m128d L2 = cast128 (T2);
            __m128d H2 = _mm256_extractf128_pd (T2, 1);
            __m256d L = _mm256_unpacklo_pd(T0,T1);
            __m256d H = _mm256_unpackhi_pd(T0,T1);
            S = _mm_add_pd (cast128(L), S);
            S = _mm_add_pd (cast128(H), S);
            S = _mm_add_pd (_mm256_extractf128_pd(L,1), S);
            S = _mm_add_pd (_mm256_extractf128_pd(H,1), S);
            S2 = _mm_add_sd (L2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(L2,L2), S2);
            S2 = _mm_add_sd (H2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(H2,H2), S2);
            i += 4;
          }
        }
        else  /* third column aligned if first is */
        { while (i <= yrows-4)
          { __m256d P = _mm256_loadA_pd(x+i);
            __m256d T0 = _mm256_mul_pd (_mm256_loadA_pd(y+i),P);
            __m256d T1 = _mm256_mul_pd (_mm256_loadu_pd
              (ALIGN < 16 ? y+i+k : ASSUME_ALIGNED(y+i+k,16,0)), P);
            __m256d T2 = _mm256_mul_pd (_mm256_loadA_pd(y+i+k+k),P);
            __m128d L2 = cast128 (T2);
            __m128d H2 = _mm256_extractf128_pd (T2, 1);
            __m256d L = _mm256_unpacklo_pd(T0,T1);
            __m256d H = _mm256_unpackhi_pd(T0,T1);
            S = _mm_add_pd (cast128(L), S);
            S = _mm_add_pd (cast128(H), S);
            S = _mm_add_pd (_mm256_extractf128_pd(L,1), S);
            S = _mm_add_pd (_mm256_extractf128_pd(H,1), S);
            S2 = _mm_add_sd (L2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(L2,L2), S2);
            S2 = _mm_add_sd (H2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(H2,H2), S2);
            i += 4;
          }
        }
#     else  /* CAN_USE_SSE2 */
        if (ALIGN >= 16 && (k & 1))
        { while (i <= yrows-4)
          { __m128d P, T0, T1, T2;
            P = _mm_loadA_pd(x+i);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
            T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
            T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            S2 = _mm_add_sd (T2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
            P = _mm_loadA_pd(x+i+2);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i+2), P);
            T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k+2), P);
            T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k+2), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            S2 = _mm_add_sd (T2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
            i += 4;
          }
         }
         else  /* second column has same 16-byte alignment as first */
      { while (i <= yrows-4)
          { __m128d P, T0, T1, T2;
            P = _mm_loadA_pd(x+i);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
            T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k), P);
            T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            S2 = _mm_add_sd (T2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
            P = _mm_loadA_pd(x+i+2);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i+2), P);
            T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k+2), P);
            T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k+2), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            S2 = _mm_add_sd (T2, S2);
            S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
            i += 4;
          }
        }
#     endif

      if (i <= yrows-2)
      { __m128d P = _mm_loadA_pd(x+i);
        __m128d T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
        __m128d T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
        __m128d T2 = _mm_mul_pd (_mm_loadA_pd(y+i+k+k), P);
        S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
        S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
        S2 = _mm_add_sd (T2, S2);
        S2 = _mm_add_sd (_mm_unpackhi_pd(T2,T2), S2);
        i += 2;
      }

      if (i < yrows)
      { __m128d P = _mm_set1_pd(x[i]);
        __m128d B;
        B = _mm_mul_pd (P, _mm_set_pd((y+k)[i],y[i]));
        S = _mm_add_pd (B, S);
        B = _mm_mul_sd (P, _mm_set_sd ((y+k+k)[i]));
        S2 = _mm_add_sd (B, S2);
      }

      _mm_storeu_pd (z, S);
      _mm_store_sd (z+2, S2);
    }

#   else  /* non-SIMD code */
    {
      double s[3];
      if (add)
      { s[0] = z[0];
        s[1] = z[1];
        s[2] = z[2];
      }
      else
        s[0] = s[1] = s[2] = 0.0;

      while (i <= yrows-2)
      { s[0] += x[i+0] * y[i];
        s[1] += x[i+0] * (y+k)[i];
        s[2] += x[i+0] * (y+k+k)[i];
        s[0] += x[i+1] * y[i+1];
        s[1] += x[i+1] * (y+k)[i+1];
        s[2] += x[i+1] * (y+k+k)[i+1];
        i += 2;
      }

      if (i < yrows)
      { s[0] += x[i] * y[i];
        s[1] += x[i] * (y+k)[i];
        s[2] += x[i] * (y+k+k)[i];
      }

      z[0] = s[0];
      z[1] = s[1];
      z[2] = s[2];
    }
#   endif
  }

  else if (m == 2)  /* Do two more dot products */
  { 
    int i = 0;

#   if CAN_USE_AVX || CAN_USE_SSE2 && ALIGN >= 16 /* slower when unaligned*/
    {
      __m128d S;

      if (add)
        S = _mm_loadu_pd(z);
      else
        S = _mm_setzero_pd();

#     if ALIGN_FORWARD & 8
        S = _mm_add_pd (S, _mm_mul_pd (_mm_set1_pd(x[i]),
                                   _mm_set_pd ((y+k)[i], y[i])));
        i += 1;
#     endif

#     if ALIGN_FORWARD & 16
      {
        __m128d P = _mm_loadA_pd(x+i);
        __m128d T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
        __m128d T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
        S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
        S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
        i += 2;
      }
#     endif

#     if CAN_USE_AVX
      {
        if (ALIGN >= 16 && (k & 1))
        { while (i <= yrows-4)
          { __m256d P = _mm256_loadA_pd(x+i);
            __m256d T0 = _mm256_mul_pd (_mm256_loadA_pd(y+i), P);
            __m256d T1 = _mm256_mul_pd (_mm256_loadu_pd(y+i+k), P);
            __m256d L = _mm256_unpacklo_pd(T0,T1);
            __m256d H = _mm256_unpackhi_pd(T0,T1);
            S = _mm_add_pd (cast128(L), S);
            S = _mm_add_pd (cast128(H), S);
            S = _mm_add_pd (_mm256_extractf128_pd(L,1), S);
            S = _mm_add_pd (_mm256_extractf128_pd(H,1), S);
            i += 4;
          }
        }
        else  /* second column has same 16-byte alignment as first */
        { while (i <= yrows-4)
          { __m256d P = _mm256_loadA_pd(x+i);
            __m256d T0 = _mm256_mul_pd (_mm256_loadA_pd(y+i), P);
            __m256d T1 = _mm256_mul_pd (_mm256_loadu_pd
              (ALIGN < 16 ? y+i+k : ASSUME_ALIGNED(y+i+k,16,0)), P);
            __m256d L = _mm256_unpacklo_pd(T0,T1);
            __m256d H = _mm256_unpackhi_pd(T0,T1);
            S = _mm_add_pd (cast128(L), S);
            S = _mm_add_pd (cast128(H), S);
            S = _mm_add_pd (_mm256_extractf128_pd(L,1), S);
            S = _mm_add_pd (_mm256_extractf128_pd(H,1), S);
            i += 4;
          }
        }
      }
#     else  /* CAN_USE_SSE2 */
      {
        if (ALIGN >= 16 && (k & 1))
        { while (i <= yrows-4)
          { __m128d P, T0, T1;
            P = _mm_loadA_pd(x+i);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
            T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            P = _mm_loadA_pd(x+i+2);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i+2), P);
            T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k+2), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            i += 4;
          }
        }
        else  /* second column has same 16-byte alignment as first */
        { while (i <= yrows-4)
          { __m128d P, T0, T1;
            P = _mm_loadA_pd(x+i);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
            T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            P = _mm_loadA_pd(x+i+2);
            T0 = _mm_mul_pd (_mm_loadA_pd(y+i+2), P);
            T1 = _mm_mul_pd (_mm_loadA_pd(y+i+k+2), P);
            S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
            S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
            i += 4;
          }
        }
      }
#     endif

      if (i <= yrows-2)
      { __m128d P = _mm_loadA_pd(x+i);
        __m128d T0 = _mm_mul_pd (_mm_loadA_pd(y+i), P);
        __m128d T1 = _mm_mul_pd (_mm_loadu_pd(y+i+k), P);
        S = _mm_add_pd (_mm_unpacklo_pd(T0,T1), S);
        S = _mm_add_pd (_mm_unpackhi_pd(T0,T1), S);
        i += 2;
      }

      if (i < yrows)
      { __m128d B;
        B = _mm_mul_pd (_mm_set1_pd(x[i]), _mm_set_pd((y+k)[i],y[i]));
        S = _mm_add_pd (B, S);
      }

      _mm_storeu_pd (z, S);
    }

#   else  /* non-SIMD code */
    {
      double s[2];

      if (add)
      { s[0] = z[0];
        s[1] = z[1];
      }
      else
        s[0] = s[1] = 0.0;

      while (i <= yrows-2)
      { s[0] += x[i+0] * y[i];
        s[1] += x[i+0] * (y+k)[i];
        s[0] += x[i+1] * y[i+1];
        s[1] += x[i+1] * (y+k)[i+1];
        i += 2;
      }

      if (i < yrows)
      { s[0] += x[i] * y[i];
        s[1] += x[i] * (y+k)[i];
      }

      z[0] = s[0];
      z[1] = s[1];
    }
#   endif
  }

  else if (m == 1)  /* Do one final dot product */
  { 
    int i = 0;

#   if CAN_USE_SSE2
    {
      __m128d S, A;

      if (add)
        S = _mm_load_sd(z);
      else
        S = _mm_setzero_pd();

#     if ALIGN_FORWARD & 8
        S = _mm_add_sd (S, _mm_mul_sd (_mm_set_sd (x[i]),
                                   _mm_set_sd (y[i])));
        i += 1;
#     endif

      while (i <= yrows-4)
      { A = _mm_mul_pd (_mm_loadA_pd(x+i), _mm_loadA_pd(y+i));
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        A  = _mm_mul_pd (_mm_loadA_pd(x+i+2), _mm_loadA_pd(y+i+2));
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        i += 4;
      }

      if (i <= yrows-2)
      { A = _mm_mul_pd (_mm_loadA_pd(x+i), _mm_loadA_pd(y+i));
        S = _mm_add_sd (A, S);
        A = _mm_unpackhi_pd (A, A);
        S = _mm_add_sd (A, S);
        i += 2;
      }

      if (i < yrows)
      { A = _mm_mul_sd (_mm_load_sd(x+i), _mm_load_sd(y+i));
        S = _mm_add_sd (A, S);
      }

      _mm_store_sd (z, S);
    }

#   else  /* non-SIMD code */
    {
      double s;

      if (add)
        s = z[0];
      else
        s = 0.0;

      while (i <= yrows-4)
      { s += x[i+0] * y[i+0];
        s += x[i+1] * y[i+1];
        s += x[i+2] * y[i+2];
        s += x[i+3] * y[i+3];
        i += 4;
      }

      if (i <= yrows-2)
      { s += x[i+0] * y[i+0];
        s += x[i+1] * y[i+1];
        i += 2;
      }

      if (i < yrows)
      { s += x[i] * y[i];
      }

      z[0] = s;
    }
#   endif
  }
}

/* Multiply vector x of dimension 2 by matrix y of dimension 2 x m, putting
   result in vector z of dimension m. */

static void matprod_vec_mat_k2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z, int m)

{
# if DEBUG_PRINTF
    debug_printf("vec_mat_k2 %p %p %p - %d\n",
                              x, y, z,   m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int i = 0;

# if CAN_USE_SSE2 && (ALIGN_OFFSET & 8) || CAN_USE_SSE3 || CAN_USE_AVX
  {
#   if CAN_USE_AVX && (ALIGN_OFFSET & 8) == 0  /* AVX slower if unaligned */
      __m256d TT = _mm256_set_pd (x[1], x[0], x[1], x[0]);
#   endif

    __m128d T = _mm_set_pd (x[1], x[0]);

#   if ALIGN_OFFSET & 8
      __m128d R = _mm_set_pd (x[0], x[1]);
#   endif

#   if ALIGN_FORWARD & 8
      _mm_store_sd (z+i, _mm_add_sd (_mm_mul_sd (T, _mm_load_sd(y)),
                               _mm_mul_sd (R, _mm_load_sd(y+1))));
      y += 2;
      i += 1;
#   endif

    while (i <= m-4)
    {
#     if CAN_USE_AVX && (ALIGN_OFFSET & 8) == 0  /* slower if unaligned */
      {
         __m256d A = _mm256_mul_pd (TT, _mm256_loadAA_pd(y));
         __m256d B = _mm256_mul_pd (TT, _mm256_loadAA_pd(y+4));
         _mm256_storeAA_pd (z+i,_mm256_hadd_pd
                              (_mm256_permute2f128_pd(A,B,0x20),
                               _mm256_permute2f128_pd(A,B,0x31)));
        y += 8;
        i += 4;
      }
#     elif ALIGN_OFFSET & 8  /* && CAN_USE_SSE2 */
      {
         __m128d A, B;
         A = _mm_mul_pd (R, _mm_loadA_pd(y+1));
         B = _mm_mul_pd (T,_mm_loadh_pd (_mm_load_sd(y), y+3));
         _mm_storeA_pd (z+i, _mm_add_pd(A,B));
         A = _mm_mul_pd (R, _mm_loadA_pd(y+5));
         B = _mm_mul_pd (T,_mm_loadh_pd (_mm_load_sd(y+4), y+7));
         _mm_storeA_pd (z+i+2, _mm_add_pd(A,B));
        y += 8;
        i += 4;
      }
#     else /* (ALIGN_OFFSET & 8) == 0 && CAN_USE_SSE3 */
      {
         __m128d A, B;
         A = _mm_mul_pd (T, _mm_loadA_pd(y));
         B = _mm_mul_pd (T, _mm_loadA_pd(y+2));
         _mm_storeA_pd (z+i, _mm_hadd_pd(A,B));
         A = _mm_mul_pd (T, _mm_loadA_pd(y+4));
         B = _mm_mul_pd (T, _mm_loadA_pd(y+6));
         _mm_storeA_pd (z+i+2, _mm_hadd_pd(A,B));
        y += 8;
        i += 4;
      }
#     endif
    }

    if (i <= m-2)
    {
#     if ALIGN_OFFSET & 8
         __m128d A, B;
         A = _mm_mul_pd (R, _mm_loadA_pd(y+1));
         B = _mm_mul_pd (T, _mm_loadh_pd (_mm_load_sd(y), y+3));
         _mm_storeA_pd (z+i, _mm_add_pd(A,B));
#     else
        __m128d A, B;
        A = _mm_mul_pd (T, _mm_loadAA_pd(y));
        B = _mm_mul_pd (T, _mm_loadAA_pd(y+2));
        _mm_storeA_pd (z+i, _mm_hadd_pd(A,B));
#     endif
      y += 4;
      i += 2;
    }

    if (i < m)
    {
#     if ALIGN_OFFSET & 8
        _mm_store_sd (z+i,_mm_add_sd(_mm_mul_sd (T, _mm_load_sd(y)),
                                 _mm_mul_sd (R, _mm_load_sd(y+1))));
#     else
        __m128d A = _mm_mul_pd (T, _mm_loadAA_pd(y));
        _mm_store_sd (z+i, _mm_hadd_pd(A,A));
#     endif
    }
  }

# else  /* non-SIMD code */
  {
    double t[2] = { x[0], x[1] };

    while (i <= m-4)
    { z[i+0] = t[0] * y[0] + t[1] * y[1];
      z[i+1] = t[0] * y[2] + t[1] * y[3];
      z[i+2] = t[0] * y[4] + t[1] * y[5];
      z[i+3] = t[0] * y[6] + t[1] * y[7];
      y += 8;
      i += 4;
    }

    if (i <= m-2)
    { z[i+0] = t[0] * y[0] + t[1] * y[1];
      z[i+1] = t[0] * y[2] + t[1] * y[3];
      y += 4;
      i += 2;
    }

    if (i < m)
    { z[i] = t[0] * y[0] + t[1] * y[1];
    }
  }
# endif
}

/* -------------------------------------------------------------------------- */
/*                                MAT_VEC                                     */

/* Product of n x k matrix (x) and column vector of length k (y) with result
   stored in z.

   Cases where k is 0 or 1 and cases where n is 0, 1, 2, 3, or 4 are
   handled specially. */

static void matprod_mat_vec_sub (double * MATPROD_RESTRICT x,
                                 double * MATPROD_RESTRICT y,
                                 double * MATPROD_RESTRICT z,
                                 int n, int k, int add);

static void matprod_mat_vec_sub_xrows0 (double * MATPROD_RESTRICT x,
                                        double * MATPROD_RESTRICT y,
                                        double * MATPROD_RESTRICT z,
                                        int n, int k, int xrows, int add);

static void matprod_mat_vec_sub_xrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int n, int k, int xrows, int add);

static void matprod_mat_vec_n2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add);

static void matprod_mat_vec_n3 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add);

static void matprod_mat_vec_n4 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add);

SCOPE void matprod_mat_vec (double * MATPROD_RESTRICT x,
                            double * MATPROD_RESTRICT y,
                            double * MATPROD_RESTRICT z, int n, int k)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec %p %p %p - %d %d\n",
                           x, y, z,   n, k);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  matprod_mat_vec_sub (x, y, z, n, k, 0);
}

/* Matrix-vector product, with result stored in z if 'add' is zero, or
   added to z if 'add' is non-zero.

   Called above and from par-matprod.c. */

static void matprod_mat_vec_sub (double * MATPROD_RESTRICT x,
                                 double * MATPROD_RESTRICT y,
                                 double * MATPROD_RESTRICT z,
                                 int n, int k, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_sub %p %p %p - %d %d %d\n",
                               x, y, z,   n, k, add);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Specially handle cases of scalar times row vector and of matrix
     with zero rows. */

  if (k <= 1)
  { if (k == 1)
    { if (add)
      { double t = y[0];
        int i;
        for (i = 0; i < n; i++) z[i] += t * x[i];
      }
      else
      { matprod_scalar_vec (y[0], x, z, n EXTRAZ);
      }
    }
    else  /* k == 0 */
    { if (!add)
      { set_to_zeros (z, n);
      }
    }
    return;
  }

  /* Specially handle matrices with 4 or fewer rows. */

  if (n <= 4)
  { if (n == 4)
    { matprod_mat_vec_n4 (x, y, z, k, add);
    }
    else if (n == 3)
    { matprod_mat_vec_n3 (x, y, z, k, add);
    }
    else if (n == 2)
    { matprod_mat_vec_n2 (x, y, z, k, add);
    }
    else if (n == 1)
    { z[0] = matprod_vec_vec_sub (x, y, k, add ? z[0] : 0.0);
    }
    return;
  }

  /* The general case with n > 4. */

  matprod_mat_vec_sub_xrows0 (x, y, z, n, k, n, add);
}

/* Call matprod_mat_vec_sub_xrows to do parts of a matrix-vector
   multiply (only one part for a matrix with fewer than MAT_VEC_XROWS
   rows).

   Note that n must be at least 4 and k must be at least 2.

   Called above and from par-matprod.c. */

static void matprod_mat_vec_sub_xrows0 (double * MATPROD_RESTRICT x,
                                        double * MATPROD_RESTRICT y,
                                        double * MATPROD_RESTRICT z,
                                        int n, int k, int xrows, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_sub_xrows0 %p %p %p - %d %d %d %d\n",
                                      x, y, z,   n, k, xrows, add);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (n >= 4);
  assert (k >= 2);

  /* Call matprod_mat_vec_sub_xrows to do parts (only one part if x
     has fewer than MAT_VEC_XROWS rows). */

  if (xrows > MAT_VEC_XROWS && k > 2)
  { while (xrows >= 2*MAT_VEC_XROWS)
    { matprod_mat_vec_sub_xrows (x, y, z, n, k, MAT_VEC_XROWS, add);
      x += MAT_VEC_XROWS;
      z += MAT_VEC_XROWS;
      xrows -= MAT_VEC_XROWS;
    }
    if (xrows > MAT_VEC_XROWS)
    { int nr = SPLITC (xrows, MAT_VEC_XROWS);
      matprod_mat_vec_sub_xrows (x, y, z, n, k, nr, add);
      x += nr;
      z += nr;
      xrows -= nr;
    }
  }

  matprod_mat_vec_sub_xrows (x, y, z, n, k, xrows, add);
}


/* Multiply the first 'xrows' of x with the elements of y, storing the
   result in z.  Note that x and z may not be the start of the
   original matrix/vector.  The k argument is the number of columns in
   x and elements in y.  The n argument is the number of rows in the
   original matrix x, which is the amount to step to go right to an
   element in the same row and the next column.

   The same alignment assumptions hold for x, y, and z as with the
   visible procedures.

   Note that k will be at least 2, and 'xrows' will be at least 4. */

static void matprod_mat_vec_sub_xrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int n, int k, int xrows, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_sub_xrows %p %p %p - %d %d %d %d\n",
                                     x, y, z,   n, k, xrows, add);
# endif

  assert (xrows >= 4);
  assert (k >= 2);

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int i;   /* index for x and z */

  /* To start, if we're not adding to z, set z to the sum from the
     first two columns of x times elements of y.  A few initial
     products may be done to help alignment, before the main loop.

     Here, and later, x and y are advanced, and k decremented, as
     columns are computed. */

  if (!add)
  { 
    i = 0;

#   if CAN_USE_AVX || CAN_USE_SSE2
    {
#     if CAN_USE_AVX
        __m256d Y0 = _mm256_set1_pd(y[0]);
        __m256d Y1 = _mm256_set1_pd(y[1]);
#     else  /* CAN_USE_SSE2 */
        __m128d Y0 = _mm_set1_pd(y[0]);
        __m128d Y1 = _mm_set1_pd(y[1]);
#     endif

#     if ALIGN_FORWARD & 8
      {
        __m128d X0 = _mm_mul_sd (_mm_load_sd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_sd (_mm_load_sd(x+i+n), cast128(Y1));
        _mm_store_sd (z+i, _mm_add_sd (X0, X1));
        i += 1;
      }
#     endif

#     if CAN_USE_AVX && (ALIGN_FORWARD & 16)
      {
        __m128d X0 = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), cast128(Y1));
        _mm_storeA_pd (z+i, _mm_add_pd (X0, X1));
        i += 2;
      }
#     endif

      while (i <= xrows-4)
      {
#       if CAN_USE_AVX
        {
          __m256d X0 = _mm256_mul_pd (_mm256_loadA_pd(x+i), Y0);
          __m256d X1 = _mm256_mul_pd (_mm256_loadu_pd(x+i+n), Y1);
          _mm256_storeA_pd (z+i, _mm256_add_pd (X0, X1));
        }
#       else  /* CAN_USE_SSE2 */
        {
          __m128d X0, X1;
          X0 = _mm_mul_pd (_mm_loadA_pd(x+i), Y0);
          X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), Y1);
          _mm_storeA_pd (z+i, _mm_add_pd (X0, X1));
          X0 = _mm_mul_pd (_mm_loadA_pd(x+i+2), Y0);
          X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n+2), Y1);
          _mm_storeA_pd (z+i+2, _mm_add_pd (X0, X1));
        }
#       endif
        i += 4;
      }

      if (i <= xrows-2)
      { __m128d X0 = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), cast128(Y1));
        _mm_storeA_pd (z+i, _mm_add_pd (X0, X1));
        i += 2;
      }

      if (i < xrows)
      { __m128d X0 = _mm_mul_sd (_mm_load_sd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_sd (_mm_load_sd(x+i+n), cast128(Y1));
        _mm_store_sd (z+i, _mm_add_sd (X0, X1));
      }
    }

#   else  /* non-SIMD code */
    {
#     if ALIGN_FORWARD & 8
        z[i] = x[i] * y[0] + (x+n)[i] * y[1];
        i += 1;
#     endif

      while (i <= xrows-4)
      { z[i+0] = x[i+0] * y[0] + (x+n)[i+0] * y[1];
        z[i+1] = x[i+1] * y[0] + (x+n)[i+1] * y[1];
        z[i+2] = x[i+2] * y[0] + (x+n)[i+2] * y[1];
        z[i+3] = x[i+3] * y[0] + (x+n)[i+3] * y[1];
        i += 4;
      }
      if (i <= xrows-2)
      { z[i+0] = x[i+0] * y[0] + (x+n)[i+0] * y[1];
        z[i+1] = x[i+1] * y[0] + (x+n)[i+1] * y[1];
        i += 2;
      }

      if (i < xrows)
      { z[i] = x[i] * y[0] + (x+n)[i] * y[1];
      }
    }
#   endif

    x += n; x += n;
    y += 2;
    k -= 2;
  }

  /* Each time around this loop, add the products of two columns of
     x with two elements of y to the result vector, z.  Adjust x, y,
     and k to account for this. */

  while (k > 1)
  { 
    i = 0;

#   if CAN_USE_AVX || CAN_USE_SSE2
    {
#     if CAN_USE_AVX
        __m256d Y0 = _mm256_set1_pd(y[0]);
        __m256d Y1 = _mm256_set1_pd(y[1]);
#     else  /* CAN_USE_SSE2 */
        __m128d Y0 = _mm_set1_pd(y[0]);
        __m128d Y1 = _mm_set1_pd(y[1]);
#     endif

#     if ALIGN_FORWARD & 8
      {
        __m128d Z = _mm_load_sd(z+i);
        __m128d X0 = _mm_mul_sd (_mm_load_sd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_sd (_mm_load_sd(x+i+n), cast128(Y1));
        Z = _mm_add_sd (_mm_add_sd (Z, X0), X1);
        _mm_store_sd (z+i, Z);
        i += 1;
      }
#     endif

#     if CAN_USE_AVX && (ALIGN_FORWARD & 16)
      {
        __m128d Z = _mm_loadA_pd(z+i);
        __m128d X0 = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), cast128(Y1));
        Z = _mm_add_pd (_mm_add_pd (Z, X0), X1);
        _mm_storeA_pd (z+i, Z);
        i += 2;
      }
#     endif

      while (i <= xrows-4)
      {
#       if CAN_USE_AVX
        {
          __m256d Z = _mm256_loadA_pd(z+i);
          __m256d X0 = _mm256_mul_pd (_mm256_loadu_pd(x+i), Y0);
          __m256d X1 = _mm256_mul_pd (_mm256_loadu_pd(x+i+n), Y1);
          Z = _mm256_add_pd (_mm256_add_pd (Z, X0), X1);
          _mm256_storeA_pd (z+i, Z);
        }
#       else  /* CAN_USE_SSE2 */
        {
          __m128d Z, X0, X1;
          Z = _mm_loadA_pd(z+i);
          X0 = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y0));
          X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), cast128(Y1));
          Z = _mm_add_pd (_mm_add_pd (Z, X0), X1);
          _mm_storeA_pd (z+i, Z);
          Z = _mm_loadA_pd(z+i+2);
          X0 = _mm_mul_pd (_mm_loadA_pd(x+i+2), cast128(Y0));
          X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n+2), cast128(Y1));
          Z = _mm_add_pd (_mm_add_pd (Z, X0), X1);
          _mm_storeA_pd (z+i+2, Z);
        }
#       endif
        i += 4;
      }

      if (i <= xrows-2)
      { __m128d Z = _mm_loadA_pd(z+i);
        __m128d X0 = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_pd (_mm_loadu_pd(x+i+n), cast128(Y1));
        Z = _mm_add_pd (_mm_add_pd (Z, X0), X1);
        _mm_storeA_pd (z+i, Z);
        i += 2;
      }

      if (i < xrows)
      { __m128d Z = _mm_load_sd(z+i);
        __m128d X0 = _mm_mul_sd (_mm_load_sd(x+i), cast128(Y0));
        __m128d X1 = _mm_mul_sd (_mm_load_sd(x+i+n), cast128(Y1));
        Z = _mm_add_sd (_mm_add_sd (Z, X0), X1);
        _mm_store_sd (z+i, Z);
      }
    }
#   else  /* non-SIMD code */
    {
#     if ALIGN_FORWARD & 8
        z[i+0] = (z[i+0] + x[0] * y[0]) + (x+n)[0] * y[1];
        i += 1;
#     endif

      while (i <= xrows-4)
      { z[i+0] = (z[i+0] + x[i+0] * y[0]) + (x+n)[i+0] * y[1];
        z[i+1] = (z[i+1] + x[i+1] * y[0]) + (x+n)[i+1] * y[1];
        z[i+2] = (z[i+2] + x[i+2] * y[0]) + (x+n)[i+2] * y[1];
        z[i+3] = (z[i+3] + x[i+3] * y[0]) + (x+n)[i+3] * y[1];
        i += 4;
      }
      if (i <= xrows-2)
      { z[i+0] = (z[i+0] + x[i+0] * y[0]) + (x+n)[i+0] * y[1];
        z[i+1] = (z[i+1] + x[i+1] * y[0]) + (x+n)[i+1] * y[1];
        i += 2;
      }

      if (i < xrows)
      { z[i+0] = (z[i+0] + x[i+0] * y[0]) + (x+n)[i] * y[1];
      }

    }
#   endif

    x += n; x += n;
    y += 2;
    k -= 2;
  }

  /* Add the last column if there are an odd number of columns. */

  if (k >= 1)
  { 
    i = 0;

#   if CAN_USE_AVX || CAN_USE_SSE2
    {
#     if CAN_USE_AVX
        __m256d Y = _mm256_set1_pd(y[0]);
#     else
        __m128d Y = _mm_set1_pd(y[0]);
#     endif

#     if ALIGN_FORWARD & 8
      {
        __m128d Z = _mm_load_sd(z+i);
        Z = _mm_add_sd (Z, _mm_mul_sd (_mm_load_sd(x+i), cast128(Y)));
        _mm_store_sd (z+i, Z);
        i += 1;
      }
#     endif

#     if CAN_USE_AVX && (ALIGN_FORWARD & 16)
      {
        __m128d Z = _mm_loadA_pd(z+i);
        __m128d X = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y));
        Z = _mm_add_pd (Z, X);
        _mm_storeA_pd (z+i, Z);
        i += 2;
      }
#     endif

      while (i <= xrows-4)
      {
#       if CAN_USE_AVX
        {
          __m256d Z = _mm256_loadA_pd(z+i);
          __m256d X = _mm256_mul_pd (_mm256_loadu_pd(x+i), Y);
          Z = _mm256_add_pd (Z, X);
          _mm256_storeA_pd (z+i, Z);
        }
#       else  /* CAN_USE_SSE2 */
        {
          __m128d Z, X;
          Z = _mm_loadA_pd(z+i);
          X = _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y));
          Z = _mm_add_pd (Z, X);
          _mm_storeA_pd (z+i, Z);
          Z = _mm_loadA_pd(z+i+2);
          X = _mm_mul_pd (_mm_loadA_pd(x+i+2), cast128(Y));
          Z = _mm_add_pd (Z, X);
          _mm_storeA_pd (z+i+2, Z);
        }
#       endif
        i += 4;
      }

      if (i <= xrows-2)
      { __m128d Z = _mm_loadA_pd(z+i);
        Z = _mm_add_pd (Z, _mm_mul_pd (_mm_loadA_pd(x+i), cast128(Y)));
        _mm_storeA_pd (z+i, Z);
        i += 2;
      }

      if (i < xrows)
      { __m128d Z = _mm_load_sd(z+i);
        Z = _mm_add_sd (Z, _mm_mul_sd (_mm_load_sd(x+i), cast128(Y)));
        _mm_store_sd (z+i, Z);
      }
    }

#   else  /* non-SIMD code */
    {
#     if ALIGN_FORWARD & 8
        z[i] += x[i] * y[0];
        i += 1;
#     endif

      while (i <= xrows-4)
      { z[i+0] += x[i+0] * y[0];
        z[i+1] += x[i+1] * y[0];
        z[i+2] += x[i+2] * y[0];
        z[i+3] += x[i+3] * y[0];
        i += 4;
      }

      if (i <= xrows-2)
      { z[i+0] += x[i+0] * y[0];
        z[i+1] += x[i+1] * y[0];
        i += 2;
      }

      if (i < xrows)
      { z[i] += x[i] * y[0];
      }
    }

#   endif
  }
}

/* Multiply 2 x k vector x by vector y of dimension k, storing the result
   in z if 'add' is zero, or adding the result to z if 'add' is non-zero. */

static void matprod_mat_vec_n2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_n2 %p %p %p - %d %d\n",
                              x, y, z,   k, add);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

# if CAN_USE_SSE2 && ALIGN >= 16 && ALIGN_OFFSET%16 == 0

    __m128d S;  /* sums for the two values in the result */
    __m128d A;

    /* Each time around this loop, add the products of two columns
       of x with two elements of y to S.  Adjust x and y to
       account for this. */

    S = add ? _mm_load_pd(z) : _mm_setzero_pd();

    while (k > 1)
    { A = _mm_mul_pd (_mm_load_pd(x), _mm_set1_pd(y[0]));
      S = _mm_add_pd (A, S);
      A = _mm_mul_pd (_mm_load_pd(x+2), _mm_set1_pd(y[1]));
      S = _mm_add_pd (A, S);
      x += 4;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { A = _mm_mul_pd (_mm_load_pd(x), _mm_set1_pd(y[0]));
      S = _mm_add_pd (A, S);
    }

    /* Store the two sums in S in the result vector. */

    _mm_store_pd (z, S);

# elif CAN_USE_SSE2 && ALIGN >= 16 && ALIGN_OFFSET%16 == 8

    /* Do kitty-corner summation. */

    __m128d S;  /* sums for the two values in the result, swapped */
    __m128d A;  /* pairs of values from y */

    A = _mm_loadu_pd (y);
    S = _mm_mul_sd (_mm_load_sd(x), A);
    S = _mm_shuffle_pd (_mm_setzero_pd(), S, 0);
    y += 1;

    if (add)
      S = _mm_add_pd (_mm_set_pd (z[0], z[1]), S);

    /* Each time around this loop, do two additions to S of
       products with the last element in a column of x and the
       first element of the next column with two elements of y.
       Adjust x and y to account for this. */

    while (k >= 3)
    { S = _mm_add_pd (S, _mm_mul_pd (_mm_load_pd(x+1), A));
      A = _mm_load_pd (y);
      S = _mm_add_pd (S, _mm_mul_pd (_mm_load_pd(x+3), A));
      A = _mm_unpackhi_pd (A, A);
      A = _mm_loadh_pd (A, y+2);
      x += 4;
      y += 2;
      k -= 2;
    }

    if (k >= 2)
    { S = _mm_add_pd (S, _mm_mul_pd (_mm_load_pd(x+1), A));
      A = _mm_unpackhi_pd (A, A);
      x += 2;
    }

    S = _mm_add_pd (S, _mm_mul_sd (_mm_load_sd(x+1), A));

    /* Store the two sums in S in the result vector. */

    _mm_storeh_pd (z, S);
    _mm_store_sd (z+1, S);

# else  /* non-SIMD code */

    double s[2];  /* sums for the two values in the result vector */

    if (add)
    { s[0] = z[0];
      s[1] = z[1];
    }
    else
      s[0] = s[1] = 0;

    /* Each time around this loop, add the products of two columns
       of x with two elements of y to s[0] and s[1].  Adjust x and
       y to account for this. */

    while (k > 1)
    { s[0] = (s[0] + (x[0] * y[0])) + (x[2] * y[1]);
      s[1] = (s[1] + (x[1] * y[0])) + (x[3] * y[1]);
      x += 4;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { s[0] += x[0] * y[0];
      s[1] += x[1] * y[0];
    }

    /* Store the two sums in s[0] and s[1] in the result vector. */

    z[0] = s[0];
    z[1] = s[1];

# endif
}

/* Multiply 3 x k vector x by vector y of dimension k, storing the result
   in z if 'add' is zero, or adding the result to z if 'add' is non-zero. */

static void matprod_mat_vec_n3 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_n3 %p %p %p - %d %d\n",
                              x, y, z,   k, add);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* The loops below accumulate the products of two columns of x
     with two elements of y. */

# if CAN_USE_SSE2 && ALIGN >= 16 || CAN_USE_AVX
  {
    __m128d S;   /* first two sums */
    __m128d S2;  /* last sum (in low 64 bits) */

#   if (ALIGN_FORWARD & 8)
    {
      __m128d B0 = _mm_set1_pd (y[0]);
      S = _mm_mul_pd (_mm_loadu_pd (x), B0);
      S2 = _mm_mul_sd (_mm_load_sd (x+2), B0);
      x += 3;
      y += 1;
      k -= 1;
      if (add)
      { S = _mm_add_pd (_mm_loadu_pd (z), S);
        S2 = _mm_add_sd (_mm_load_sd (z+2), S2);
      }
    }
#   else
    {
      if (add)
      { S = _mm_loadAA_pd (z);
        S2 = _mm_load_sd (z+2);
      }
      else
      { S = _mm_setzero_pd();
        S2 = _mm_setzero_pd();
      }
    }
#   endif

    while (k > 1)
    { __m128d A1 = _mm_loadA_pd (x+2);
      __m128d A2 = _mm_loadA_pd (x+4);
      __m128d B0 = _mm_set1_pd (y[0]);
      __m128d B1 = _mm_set1_pd (y[1]);
      S = _mm_add_pd (S, _mm_mul_pd (_mm_loadA_pd (x), B0));
      S2 = _mm_add_sd (S2, _mm_mul_sd (A1, B0));
      S = _mm_add_pd (S, _mm_mul_pd (_mm_shuffle_pd (A1, A2, 1), B1));
      S2 = _mm_add_sd (S2, _mm_mul_sd (_mm_unpackhi_pd (A2, A2), B1));
      x += 6;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { __m128d B0 = _mm_set1_pd(y[0]);
      S = _mm_add_pd (S, _mm_mul_pd (_mm_loadA_pd(x), B0));
      S2 = _mm_add_sd (S2, _mm_mul_sd (_mm_load_sd(x+2), B0));
    }

    _mm_storeAA_pd (z, S);
    _mm_store_sd (z+2, S2);
  }

# else  /* non-SIMD code */
  {
    double s[3];  /* sums for the three values in the result vector */

    if (add)
    { s[0] = z[0];
      s[1] = z[1];
      s[2] = z[2];
    }
    else
      s[0] = s[1] = s[2] = 0;

    while (k > 1)
    { s[0] = (s[0] + (x[0] * y[0])) + (x[3] * y[1]);
      s[1] = (s[1] + (x[1] * y[0])) + (x[4] * y[1]);
      s[2] = (s[2] + (x[2] * y[0])) + (x[5] * y[1]);
      x += 6;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { s[0] += x[0] * y[0];
      s[1] += x[1] * y[0];
      s[2] += x[2] * y[0];
    }

    /* Store the three sums in s[0], s[1], and s[2] in the result vector. */

    z[0] = s[0];
    z[1] = s[1];
    z[2] = s[2];
  }
# endif
}

/* Multiply 4 x k vector x by vector y of dimension k, storing the result
   in z if 'add' is zero, or adding the result to z if 'add' is non-zero. */

static void matprod_mat_vec_n4 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int add)
{
# if DEBUG_PRINTF
    debug_printf("mat_vec_n4 %p %p %p - %d %d\n",
                              x, y, z,   k, add);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* The loops below accumulate the products of two columns of x
     with two elements of y. */

# if CAN_USE_AVX && ENABLE_ALL_AVX_CODE
  {
    __m256d S = add ? _mm256_loadAA_pd(z) : _mm256_setzero_pd();

    while (k > 1)
    { __m256d B0 = _mm256_set1_pd (y[0]);
      __m256d A0 = _mm256_loadAA_pd (x);
      __m256d B1 = _mm256_set1_pd (y[1]);
      __m256d A1 = _mm256_loadAA_pd (x+4);
      S = _mm256_add_pd (S, _mm256_mul_pd (A0, B0));
      S = _mm256_add_pd (S, _mm256_mul_pd (A1, B1));
      x += 8;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { __m256d B0 = _mm256_set1_pd (y[0]);
      S = _mm256_add_pd (S, _mm256_mul_pd (_mm256_loadAA_pd (x), B0));
    }

    _mm256_storeAA_pd (z, S);
  }
# elif CAN_USE_SSE2 && ALIGN >= 16 || CAN_USE_AVX
  {
    __m128d S0 = add ? _mm_loadAA_pd(z) : _mm_setzero_pd(); /* 1st 2 sums */
    __m128d S1 = add ? _mm_loadAA_pd(z+2) : _mm_setzero_pd();   /* last 2 */

    while (k > 1)
    { __m128d B0 = _mm_set1_pd (y[0]);
      __m128d B1 = _mm_set1_pd (y[1]);
      S0 = _mm_add_pd (S0, _mm_mul_pd (_mm_loadAA_pd (x), B0));
      S1 = _mm_add_pd (S1, _mm_mul_pd (_mm_loadAA_pd (x+2), B0));
      S0 = _mm_add_pd (S0, _mm_mul_pd (_mm_loadAA_pd (x+4), B1));
      S1 = _mm_add_pd (S1, _mm_mul_pd (_mm_loadAA_pd (x+6), B1));
      x += 8;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { __m128d B0 = _mm_set1_pd (y[0]);
      S0 = _mm_add_pd (S0, _mm_mul_pd (_mm_loadAA_pd (x), B0));
      S1 = _mm_add_pd (S1, _mm_mul_pd (_mm_loadAA_pd (x+2), B0));
    }

    _mm_storeAA_pd (z, S0);
    _mm_storeAA_pd (z+2, S1);
  }
# else
  {
    double s[4];  /* sums for the four values in the result vector */

    if (add)
    { s[0] = z[0];
      s[1] = z[1];
      s[2] = z[2];
      s[3] = z[3];
    }
    else
      s[0] = s[1] = s[2] = s[3] = 0;

    while (k > 1)
    { s[0] = (s[0] + (x[0] * y[0])) + (x[4] * y[1]);
      s[1] = (s[1] + (x[1] * y[0])) + (x[5] * y[1]);
      s[2] = (s[2] + (x[2] * y[0])) + (x[6] * y[1]);
      s[3] = (s[3] + (x[3] * y[0])) + (x[7] * y[1]);
      x += 8;
      y += 2;
      k -= 2;
    }

    if (k >= 1)
    { s[0] += x[0] * y[0];
      s[1] += x[1] * y[0];
      s[2] += x[2] * y[0];
      s[3] += x[3] * y[0];
    }

    /* Store the four sums in the result vector. */

    z[0] = s[0];
    z[1] = s[1];
    z[2] = s[2];
    z[3] = s[3];
  }
# endif
}

/* -------------------------------------------------------------------------- */
/*                                 OUTER                                      */

/* Outer product - n x 1 matrix x times 1 x m matrix y, with result stored
   in the n x m matrix z.

   Cases where n is four or less are handled specially, storing all of
   x in local variables rather thna fetching it to compute each column
   of z. */

static void matprod_outer_sub (double * MATPROD_RESTRICT x,
                               double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z,
                               int n, int m, int rows EXTRAD);

static void matprod_outer_n2 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m);

static void matprod_outer_n3 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m);

static void matprod_outer_n4 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m);

SCOPE void matprod_outer (double * MATPROD_RESTRICT x,
                          double * MATPROD_RESTRICT y,
                          double * MATPROD_RESTRICT z, int n, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("outer %p %p %p - %d %d\n",
                         x, y, z,   n, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (m <= 1)
  { if (m == 1)
    { matprod_scalar_vec (y[0], x, z, n EXTRAN);
    }
    return;
  }

  if (n <= 4)
  { if (n == 4)
    { matprod_outer_n4 (x, y, z, m);
    }
    else if (n == 3)
    { matprod_outer_n3 (x, y, z, m);
    }
    else if (n == 2)
    { matprod_outer_n2 (x, y, z, m);
    }
    else if (n == 1)
    { matprod_scalar_vec (x[0], y, z, m EXTRAN);
    }
    return;
  }

  /* The general case with n > 4.  Calls matprod_outer_sub to do
     parts (only one part if z has fewer than OUTER_ROWS rows). */

  int rows = n;

  while (rows > 2*OUTER_ROWS)
  { matprod_outer_sub (x, y, z, n, m, OUTER_ROWS EXTRAN);
    x += OUTER_ROWS;
    z += OUTER_ROWS;
    rows -= OUTER_ROWS;
  }

  if (rows > OUTER_ROWS)
  { int nr = SPLITC (rows, OUTER_ROWS);
    matprod_outer_sub (x, y, z, n, m, nr EXTRAN);
    x += nr;
    z += nr;
    rows -= nr;
  }

  matprod_outer_sub (x, y, z, n, m, rows EXTRAN);
}

/* Compute 'rows' rows of an outer product, with x pointing to the
   left operand vector, y pointing to the right operand vector, and z
   pointing to where to store the first result element, with n being
   the amount to move to go to the next column of z.

   The same alignment assumptions apply as for the visible procedures. */

static void matprod_outer_sub (double * MATPROD_RESTRICT x,
                               double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z,
                               int n, int m, int rows EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("outer_sub %p %p %p - %d %d %d\n",
                             x, y, z,   n, m, rows);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int j = 0;

  while (j < m)
  { 
    int i = 0;

#   if CAN_USE_SSE2 || CAN_USE_AVX
    {
#     if CAN_USE_AVX
        __m256d T = _mm256_set1_pd (y[j]);
#     else
        __m128d T = _mm_set1_pd (y[j]);
#     endif

#     if ALIGN_FORWARD & 8
      { _mm_store_sd (z+i, _mm_mul_sd (_mm_load_sd (x+i), cast128(T)));
        i += 1;
      }
#     endif

#     if CAN_USE_AVX
      {
#       if ALIGN_FORWARD & 16
        { _mm_storeu_pd (z+i, _mm_mul_pd (_mm_loadA_pd (x+i), cast128(T)));
          i += 2;
        }
#       endif

        if (ALIGN < 32 || (n & 3))  /* z+i may not be aligned */
        { while (i <= rows-4)
          { _mm256_storeu_pd (z+i, _mm256_mul_pd (_mm256_loadA_pd (x+i), T));
            i += 4;
          }
        }
        else  /* z+i is 32-byte aligned, if original z is */
        { while (i <= rows-4)
          { _mm256_storeA_pd (z+i, _mm256_mul_pd (_mm256_loadA_pd (x+i), T));
            i += 4;
          }
        }
      }
#     else  /* CAN_USE_SSE2 */
      {
        if (ALIGN < 16 || (n & 1))  /* z+i may not be aligned */
        { while (i <= rows-4)
          { _mm_storeu_pd (z+i, _mm_mul_pd (_mm_loadA_pd (x+i), cast128(T)));
            _mm_storeu_pd (z+i+2, _mm_mul_pd (_mm_loadA_pd(x+i+2), cast128(T)));
            i += 4;
          }
        }
        else  /* z+i is 16-byte aligned, if z original is */
        { while (i <= rows-4)
          { _mm_storeA_pd (z+i, _mm_mul_pd (_mm_loadA_pd (x+i), cast128(T)));
            _mm_storeA_pd (z+i+2, _mm_mul_pd (_mm_loadA_pd(x+i+2), cast128(T)));
            i += 4;
          }
        }
      }
#     endif

      if (i <= rows-2)
      { _mm_storeu_pd (z+i, _mm_mul_pd (_mm_loadA_pd (x+i), cast128(T)));
        i += 2;
      }

      if (i < rows)
      { _mm_store_sd (z+i, _mm_mul_sd (_mm_load_sd (x+i), cast128(T)));
      }
    }

#   else  /* non-SIMD code */
    {
      double t = y[j];

      while (i <= rows-4)
      { z[i+0] = x[i+0] * t;
        z[i+1] = x[i+1] * t;
        z[i+2] = x[i+2] * t;
        z[i+3] = x[i+3] * t;
        i += 4;
      }

      if (i <= rows-2)
      { z[i+0] = x[i+0] * t;
        z[i+1] = x[i+1] * t;
        i += 2;
      }

      if (i < rows)
      { z[i] = x[i] * t;
      }
    }
#   endif

    AMTOUT(z+rows);

    z += n;
    j += 1;
  }
}

/* Multiply 4 x 1 vector by 1 x m vector. */

static void matprod_outer_n4 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m)
{
# if DEBUG_PRINTF
    debug_printf("outer_n4 %p %p %p - %d\n",
                            x, y, z,   m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int j = 0;

# if CAN_USE_AVX
  {
    __m256d X = _mm256_loadu_pd (x);
    while (j <= m-2)
    { _mm256_storeAA_pd (z,  _mm256_mul_pd(X,_mm256_set1_pd(y[j])));
      _mm256_storeAA_pd (z+4,_mm256_mul_pd(X,_mm256_set1_pd(y[j+1])));
      z += 8;
      j += 2;
    }
    if (j < m)
    { _mm256_storeAA_pd (z, _mm256_mul_pd (X,_mm256_set1_pd(y[j])));
    }
  }

# elif CAN_USE_SSE2 && (ALIGN_OFFSET & 8)
  {
    __m128d X0 = _mm_load_sd (x);
    __m128d X21 = _mm_load_pd (x+1);
    __m128d X3 = _mm_load_sd (x+3);
    __m128d X03 = _mm_shuffle_pd (X3, X0, 0);
    __m128d Y;
    Y = _mm_set1_pd (y[j]);
    _mm_store_sd (z, _mm_mul_sd (X0, Y));
    while (j <= m-3)
    { _mm_store_pd (z+1, _mm_mul_pd (X21, Y));
      Y = _mm_loadh_pd (Y, y+j+1);
      _mm_store_pd (z+3, _mm_mul_pd (X03, Y));
      Y = _mm_loadl_pd (Y, y+j+1);
      _mm_store_pd (z+5, _mm_mul_pd (X21, Y));
      Y = _mm_loadh_pd (Y, y+j+2);
      _mm_store_pd (z+7, _mm_mul_pd (X03, Y));
      Y = _mm_loadl_pd (Y, y+j+2);
      z += 8;
      j += 2;
    }
    _mm_store_pd (z+1, _mm_mul_pd (X21, Y));
    _mm_store_sd (z+3, _mm_mul_sd (X3, Y));
    j += 1;
    if (j < m)
    { Y = _mm_set1_pd (y[j]);
      _mm_store_sd (z+4, _mm_mul_sd (X0, Y));
      _mm_store_pd (z+5, _mm_mul_pd (X21, Y));
      _mm_store_sd (z+7, _mm_mul_sd (X3, Y));
    }
  }

# elif CAN_USE_SSE2
  {
    __m128d Xa = _mm_loadu_pd (x);
    __m128d Xb = _mm_loadu_pd (x+2);
    while (j <= m-2)
    { __m128d Y;
      Y = _mm_set1_pd (y[j]);
      _mm_storeAA_pd (z, _mm_mul_pd (Xa, Y));
      _mm_storeAA_pd (z+2, _mm_mul_pd (Xb, Y));
      Y = _mm_set1_pd (y[j+1]);
      _mm_storeAA_pd (z+4, _mm_mul_pd (Xa, Y));
      _mm_storeAA_pd (z+6, _mm_mul_pd (Xb, Y));
      z += 8;
      j += 2;
    }
    if (j < m)
    { __m128d Y;
      Y = _mm_set1_pd (y[j]);
      _mm_storeAA_pd (z, _mm_mul_pd (Xa, Y));
      _mm_storeAA_pd (z+2, _mm_mul_pd (Xb, Y));
    }
  }

# else  /* non-SIMD code */
  {
    double X[4] = { x[0], x[1], x[2], x[3] };
    while (j < m)
    { double yj = y[j];
      z[0] = yj * X[0];
      z[1] = yj * X[1];
      z[2] = yj * X[2];
      z[3] = yj * X[3];
      z += 4;
      j += 1;
    }
  }
# endif
}

/* Multiply 3 x 1 vector by 1 x m vector. */

static void matprod_outer_n3 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m)
{
# if DEBUG_PRINTF
    debug_printf("outer_n3 %p %p %p - %d\n",
                            x, y, z,   m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int j = 0;

# if CAN_USE_AVX
  {
    __m256d Xa = _mm256_set_pd (x[0], x[2], x[1], x[0]);
    __m128d Xc = _mm_loadu_pd (x+1);

#   if ALIGN_FORWARD & 8
    { __m128d Y = _mm_set1_pd (y[j]);
      _mm_store_sd (z, _mm_mul_sd(cast128(Xa),Y));
      _mm_storeA_pd (z+1, _mm_mul_pd(Xc,Y));
      z += 3;
      j += 1;
    }
#   endif

    while (j <= m-2)
    { __m256d Y0 = _mm256_set1_pd (y[j]);
      __m256d Y1 = _mm256_set1_pd (y[j+1]);
      __m256d Ya = _mm256_blend_pd (Y0, Y1, 0x8);
      _mm256_storeu_pd (z, _mm256_mul_pd (Xa, Ya));
      _mm_storeA_pd (z+4, _mm_mul_pd (Xc, cast128(Y1)));
      z += 6;
      j += 2;
    }

    if (j < m)
    { __m128d Y = _mm_set1_pd (y[j]);
      _mm_storeA_pd (z, _mm_mul_pd (cast128(Xa), Y));
      _mm_store_sd (z+2, _mm_mul_sd (_mm256_extractf128_pd(Xa,1), Y));
    }
  }

# elif CAN_USE_SSE2
  {
    __m128d Xa = _mm_set_pd (x[1], x[0]);
    __m128d Xb = _mm_set_pd (x[0], x[2]);
    __m128d Xc = _mm_set_pd (x[2], x[1]);
    __m128d Y;

#   if ALIGN_FORWARD & 8
    { Y = _mm_set1_pd (y[j]);
      _mm_store_sd (z, _mm_mul_sd(Xa,Y));
      _mm_storeA_pd (z+1, _mm_mul_pd(Xc,Y));
      z += 3;
      j += 1;
    }
#   endif

    while (j <= m-2)
    { Y = _mm_set1_pd (y[j]);
      _mm_storeA_pd (z, _mm_mul_pd(Xa,Y));
      Y = _mm_set_pd (y[j+1],y[j]);
      _mm_storeA_pd (z+2, _mm_mul_pd(Xb,Y));
      Y = _mm_set1_pd (y[j+1]);
      _mm_storeA_pd (z+4, _mm_mul_pd(Xc,Y));
      z += 6;
      j += 2;
    }

    if (j < m)
    { Y = _mm_set1_pd (y[j]);
      _mm_storeA_pd (z, _mm_mul_pd(Xa,Y));
      _mm_store_sd (z+2, _mm_mul_sd(Xb,Y));
    }
  }

# else  /* non-SIMD code */
  {
    double X[3] = { x[0], x[1], x[2] };

    while (j <= m-2)
    { double y0 = y[j+0];
      z[0] = y0 * X[0];
      z[1] = y0 * X[1];
      z[2] = y0 * X[2];
      double y1 = y[j+1];
      z[3] = y1 * X[0];
      z[4] = y1 * X[1];
      z[5] = y1 * X[2];
      z += 6;
      j += 2;
    }

    if (j < m)
    { double yj = y[j];
      z[0] = yj * X[0];
      z[1] = yj * X[1];
      z[2] = yj * X[2];
    }
  }
# endif
}

/* Multiply 2 x 1 vector by 1 x m vector. */

static void matprod_outer_n2 (double * MATPROD_RESTRICT x,
                              double * MATPROD_RESTRICT y,
                              double * MATPROD_RESTRICT z, int m)
{
# if DEBUG_PRINTF
    debug_printf("outer_n2 %p %p %p - %d\n",
                            x, y, z,   m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int j = 0;

# if CAN_USE_AVX || CAN_USE_SSE2
  {
#   if CAN_USE_AVX
      __m256d X = _mm256_set_pd (x[1], x[0], x[1], x[0]);
#   else  /* CAN_USE_SSE2 */
      __m128d X = _mm_loadA_pd (x);
#   endif

#   if ALIGN_FORWARD == 16
    { _mm_storeu_pd (z, _mm_mul_pd (cast128(X), _mm_set1_pd (y[j])));
      z += 2;
      j += 1;
    }
#   endif

    while (j <= m-2)
    {
#     if CAN_USE_AVX
      { _mm256_storeu_pd (z, _mm256_mul_pd (X,
                             _mm256_set_pd (y[j+1], y[j+1], y[j], y[j])));
      }
#     else  /* CAN_USE_SSE2 */
      { _mm_storeAA_pd (z, _mm_mul_pd(X, _mm_set1_pd (y[j])));
        _mm_storeAA_pd (z+2, _mm_mul_pd(X, _mm_set1_pd (y[j+1])));
      }
#     endif
      z += 4;
      j += 2;
    }

    if (j < m)
    { _mm_storeAA_pd (z, _mm_mul_pd (cast128(X), _mm_set1_pd (y[j])));
    }
  }

# else  /* non-SIMD code */
  {
    double X[3] = { x[0], x[1] };

    while (j < m)
    { double yj = y[j];
      z[0] = yj * X[0];
      z[1] = yj * X[1];
      z += 2;
      j += 1;
    }
  }
# endif
}

/* -------------------------------------------------------------------------- */
/*                                MAT_MAT                                     */

/* Product of an n x k matrix (x) and a k x m matrix (y) with result stored
   in z.

   Outer products (where k is one) are handled specially with the
   matprod_outer procedure.

   Cases where n is 2 are also handled specially, in matprod_mat_mat_n2. */

static void matprod_mat_mat_sub_xrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int n, int k, int m,
                                       int xrows, int zn EXTRAD);

static void matprod_mat_mat_sub_xrowscols (double * MATPROD_RESTRICT x,
                                           double * MATPROD_RESTRICT y,
                                           double * MATPROD_RESTRICT z,
                                           int n, int k, int m,
                                           int xrows, int zn, int xcols,
                                           int add EXTRAD);

static void matprod_mat_mat_n2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int m);

SCOPE void matprod_mat_mat (double * MATPROD_RESTRICT x,
                            double * MATPROD_RESTRICT y,
                            double * MATPROD_RESTRICT z,
                            int n, int k, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("mat_mat %p %p %p - %d %d %d\n",
                           x, y, z,   n, k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (k <= 1)
  { if (k == 1)
    { matprod_outer (x, y, z, n, m EXTRAN);
    }
    else
    { set_to_zeros (z, (size_t)n*m);
    }
    return;
  }

  if (n <= 2)
  { if (n == 2)
    { matprod_mat_mat_n2 (x, y, z, k, m);
    }
    else if (n == 1)
    { matprod_vec_mat (x, y, z, k, m EXTRAN);
    }
    return;
  }
  if (m <= 1)
  { if (m == 1)
    { matprod_mat_vec (x, y, z, n, k);
    }
    return;
  }

  /* The general case with n > 2.  Calls matprod_mat_mat_sub_xrow to
     do parts - only one part for a matrix with fewer than
     MAT_MAT_XROWS rows and fewer than MAT_MAT_XCOLS columns, in
     which case matprod_mat_mat_sub_xrowscols is called directly. */

  if (n <= MAT_MAT_XROWS && k <= MAT_MAT_XCOLS)  /* do small cases quickly */
  { matprod_mat_mat_sub_xrowscols (x, y, z, n, k, m,
                                   n, n, k, 0 EXTRAN);
    return;
  }

  int cachable = n <= MAT_MAT_XROWS
           ? m : 1 + (int) (DOUBLES_IN_LLC / (MAT_MAT_XROWS+(double)k));

  int mm = m;

  for (;;)
  { 
    double *xx = x;
    double *zz = z;
    int xrows = n;
    int m1 = mm;

    if (m1 > cachable)
    { m1 = mm < 2*cachable ? mm/2 : cachable;
      m1 = (m1 + 7) & ~7;
      if (m1 > mm) m1 = mm;
    }

    if (xrows > MAT_MAT_XROWS)
    { while (xrows >= 2*MAT_MAT_XROWS)
      { matprod_mat_mat_sub_xrows (xx, y, zz, n, k, m1,
                                   MAT_MAT_XROWS, n EXTRAZ);
        xx += MAT_MAT_XROWS;
        zz += MAT_MAT_XROWS;
        xrows -= MAT_MAT_XROWS;
      }
      if (xrows > MAT_MAT_XROWS)
      { int nr = SPLITC (xrows, MAT_MAT_XROWS);
        matprod_mat_mat_sub_xrows (xx, y, zz, n, k, m1,
                                   nr, n EXTRAZ);
        xx += nr;
        zz += nr;
        xrows -= nr;
      }
    }

    matprod_mat_mat_sub_xrows (xx, y, zz, n, k, m1,
                               xrows, n EXTRAN);

    mm -= m1;
    if (mm == 0)
      break;

    y += (size_t)m1*k;
    z += (size_t)m1*n;
  }
}

/* Multiply 'xrows' rows of x by y, storing the result in 'xrows' of z.
   Note that x and z do not necessarily point to the start of the
   full matrix, but rather to the first element referenced.  The step
   from one column to the next is n for x and zn for z.

   The same alignment assumptions hold for x, y, and z as with the
   visible procedures.

   Note that k and m will be at least 2, and n and 'xrows' will be at
   least 3. */

static void matprod_mat_mat_sub_xrows (double * MATPROD_RESTRICT x,
                                       double * MATPROD_RESTRICT y,
                                       double * MATPROD_RESTRICT z,
                                       int n, int k, int m,
                                       int xrows, int zn EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("mat_mat_sub_xrows %p %p %p - %d %d %d - %d %d\n",
                                     x, y, z,   n, k, m,  xrows, zn);
# endif

  assert (k >= 2);
  assert (m >= 2);
  assert (n >= 3);
  assert (xrows >= 3);

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int chunk;

  if (k <= MAT_MAT_XCOLS
   || k <= (chunk = (MAT_MAT_XCOLS*MAT_MAT_XROWS/xrows) & ~7))
  { matprod_mat_mat_sub_xrowscols (x, y, z, n, k, m,
                                   xrows, zn, k, 0 EXTRAN);
    return;
  }

  int xcols = k;
  int add = 0;

  while (xcols > 2*chunk)
  { matprod_mat_mat_sub_xrowscols (x, y, z, n, k, m,
                                   xrows, zn, chunk, add EXTRAZ);
    x += chunk*n;
    y += chunk;
    xcols -= chunk;
    add = 1;
  }

  if (xcols > chunk)
  { int nc = ((xcols+1)/2) & ~3;  /* keep any alignment of x */
    matprod_mat_mat_sub_xrowscols (x, y, z, n, k, m,
                                   xrows, zn, nc, add EXTRAZ);
    x += nc*n;
    y += nc;
    xcols -= nc;
    add = 1;
  }

  matprod_mat_mat_sub_xrowscols (x, y, z, n, k, m,
                                 xrows, zn, xcols, add EXTRAN);
}

/* Multiply the first 'xrows' rows and 'xcols' columns of x with m
   columns of y, storing the result in z.  Note that x, y, and z may
   not be the start of the original matrices.  The k argument is the
   number of rows in the original matrix y, which is the amount to
   step to go right to an element of y in the same row and the next
   column.  The n argument is the number of rows in the original
   matrix x, which is the amount to step to go right to an element of
   x in the same row and the next column.  The 'zn' argument is the
   number of rows in the full result matrix, z, which is the amount to
   step to go to the right to an element of z in the same row but next
   column.

   If 'add' is non-zero, the results are added to the existing values
   in z, rather than replacing existing values.

   The same alignment assumptions hold for x, y, and z as with the
   visible procedures.

   Note that k, m, and 'xcols' will be at least 2, and n and 'xrows'
   will be at least 3. */

static void matprod_mat_mat_sub_xrowscols (double * MATPROD_RESTRICT x,
                                           double * MATPROD_RESTRICT y,
                                           double * MATPROD_RESTRICT z,
                                           int n, int k, int m,
                                           int xrows, int zn, int xcols,
                                           int add EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("mat_mat_sub_xrowscols %p %p %p - %d %d %d - %d %d %d %d\n",
                                         x, y, z,   n, k, m,  xrows, zn, 
                                                              xcols, add);
# endif

  assert (k >= 2);
  assert (m >= 2);
  assert (n >= 3);
  assert (xrows >= 3);
  assert (xcols >= 2);

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  while (m > 1)
  { 
    double *yy = y;
    double *xx = x;
    int i = 0;

    /* Unless we're adding, initialize sums in next two columns of
       z to the sum of the first two products, which will exist,
       since 'xcols' is at least two.  Note also that 'xrows' is
       at least three. */

    if (!add)
    { 
      int j = 0;

#     if CAN_USE_AVX || CAN_USE_SSE2
      {
#       if CAN_USE_AVX
          __m256d B11 = _mm256_set1_pd (yy[0]);
          __m256d B12 = _mm256_set1_pd (yy[1]);
          __m256d B21 = _mm256_set1_pd ((yy+k)[0]);
          __m256d B22 = _mm256_set1_pd ((yy+k)[1]);
#       else  /* CAN_USE_SSE2 */
          __m128d B11 = _mm_set1_pd (yy[0]);
          __m128d B12 = _mm_set1_pd (yy[1]);
          __m128d B21 = _mm_set1_pd ((yy+k)[0]);
          __m128d B22 = _mm_set1_pd ((yy+k)[1]);
#       endif

#       if ALIGN_FORWARD & 8
        {
          __m128d S1 = _mm_set_sd(xx[0]);
          __m128d S2 = _mm_set_sd((xx+n)[0]);
          _mm_store_sd (z+j, _mm_add_sd(_mm_mul_sd (S1, cast128(B11)),
                                        _mm_mul_sd (S2, cast128(B12))));
          _mm_store_sd(z+j+zn, _mm_add_sd(_mm_mul_sd (S1, cast128(B21)),
                                          _mm_mul_sd (S2, cast128(B22))));
          j += 1;
        }
#       endif

#       if CAN_USE_AVX && (ALIGN_FORWARD & 16)
        {
          __m128d S1 = _mm_loadA_pd(xx+j);
          __m128d S2 = _mm_loadu_pd(xx+j+n);
          _mm_storeA_pd (z+j, _mm_add_pd (_mm_mul_pd (S1, cast128(B11)),
                                          _mm_mul_pd (S2, cast128(B12))));
          _mm_storeu_pd (z+j+zn, _mm_add_pd (_mm_mul_pd (S1, cast128(B21)),
                                             _mm_mul_pd (S2, cast128(B22))));
          j += 2;
        }
#       endif

        if (j <= xrows-4)
#       if CAN_USE_AVX
        {
          if (ALIGN >= 32 && (n & 3) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m256d S1 = _mm256_loadA_pd(xx+j);
              __m256d S2 = _mm256_loadA_pd(xx+j+n);
              _mm256_storeA_pd (z+j, _mm256_add_pd (_mm256_mul_pd(S1,B11),
                                                    _mm256_mul_pd(S2,B12)));
                _mm256_storeA_pd (z+j+zn, _mm256_add_pd(_mm256_mul_pd(S1,B21),
                                                        _mm256_mul_pd(S2,B22)));
                j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            { __m256d S1 = _mm256_loadA_pd(xx+j);
              __m256d S2 = _mm256_loadu_pd(xx+j+n);
              _mm256_storeu_pd (z+j, _mm256_add_pd (_mm256_mul_pd(S1,B11),
                                                    _mm256_mul_pd(S2,B12)));
                _mm256_storeu_pd (z+j+zn, _mm256_add_pd(_mm256_mul_pd(S1,B21),
                                                        _mm256_mul_pd(S2,B22)));
                j += 4;
            } while (j <= xrows-4);
          }
        }
#       else  /* CAN_USE_SSE2 */
        {
          if (ALIGN >= 16 && (n & 1) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m128d S1, S2;
              S1 = _mm_loadA_pd(xx+j);
              S2 = _mm_loadA_pd(xx+j+n);
              _mm_storeA_pd (z+j, _mm_add_pd (_mm_mul_pd(S1,B11),
                                              _mm_mul_pd(S2,B12)));
              _mm_storeA_pd (z+j+zn, _mm_add_pd (_mm_mul_pd(S1,B21),
                                                 _mm_mul_pd(S2,B22)));
              S1 = _mm_loadA_pd(xx+j+2);
              S2 = _mm_loadA_pd(xx+j+2+n);
              _mm_storeA_pd (z+j+2, _mm_add_pd(_mm_mul_pd(S1,B11),
                                               _mm_mul_pd(S2,B12)));
              _mm_storeA_pd(z+j+2+zn, _mm_add_pd(_mm_mul_pd(S1,B21),
                                                 _mm_mul_pd(S2,B22)));
              j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            {  __m128d S1, S2;
              S1 = _mm_loadA_pd(xx+j);
              S2 = _mm_loadu_pd(xx+j+n);
              _mm_storeA_pd (z+j, _mm_add_pd (_mm_mul_pd(S1,B11),
                                              _mm_mul_pd(S2,B12)));
              _mm_storeu_pd (z+j+zn, _mm_add_pd (_mm_mul_pd(S1,B21),
                                                 _mm_mul_pd(S2,B22)));
              S1 = _mm_loadA_pd(xx+j+2);
              S2 = _mm_loadu_pd(xx+j+2+n);
              _mm_storeA_pd (z+j+2, _mm_add_pd (_mm_mul_pd(S1,B11),
                                                _mm_mul_pd(S2,B12)));
              _mm_storeu_pd(z+j+2+zn, _mm_add_pd (_mm_mul_pd(S1,B21),
                                                  _mm_mul_pd(S2,B22)));
              j += 4;
            } while (j <= xrows-4);
          }
        }
#       endif

        if (j <= xrows-2)
        { __m128d S1 = _mm_loadA_pd(xx+j);
          __m128d S2 = _mm_loadu_pd(xx+j+n);
          _mm_storeA_pd (z+j,_mm_add_pd (_mm_mul_pd(S1,cast128(B11)),
                                         _mm_mul_pd(S2,cast128(B12))));
          _mm_storeu_pd (z+j+zn, _mm_add_pd (_mm_mul_pd(S1,cast128(B21)),
                                             _mm_mul_pd(S2,cast128(B22))));
          j += 2;
        }

        if (j < xrows)
        { __m128d S1 = _mm_set_sd((xx+j)[0]);
          __m128d S2 = _mm_set_sd((xx+j+n)[0]);
          _mm_store_sd (z+j,_mm_add_sd (_mm_mul_sd (S1,cast128(B11)),
                                        _mm_mul_sd (S2,cast128(B12))));
          _mm_store_sd (z+j+zn, _mm_add_sd (_mm_mul_sd (S1,cast128(B21)),
                                            _mm_mul_sd (S2,cast128(B22))));
        }
      }

#     else  /* non-SIMD code */
      {
        double b11 = yy[0];
        double b12 = yy[1];
        double b21 = (yy+k)[0];
        double b22 = (yy+k)[1];

        while (j < xrows)
        { double s1 = xx[j];
          double s2 = (xx+n)[j];
          z[j] = (s1 * b11) + (s2 * b12);
          (z+zn)[j] = (s1 * b21) + (s2 * b22);
          j += 1;
        }
      }
#     endif

      xx += n; xx += n;
      yy += 2;
      i += 2;
    }

    /* Each time around this loop, add the products of two columns
       of x with two elements of the next two columns of y to the
       next two columns of the result vector, z.  Adjust y, z, and
       m to account for this. */

    while (i <= xcols-2)
    { 
      int j = 0;

#     if CAN_USE_AVX || CAN_USE_SSE2
      {
#       if CAN_USE_AVX
          __m256d B11 = _mm256_set1_pd(yy[0]);
          __m256d B12 = _mm256_set1_pd(yy[1]);
          __m256d B21 = _mm256_set1_pd((yy+k)[0]);
          __m256d B22 = _mm256_set1_pd((yy+k)[1]);
#       else
          __m128d B11 = _mm_set1_pd(yy[0]);
          __m128d B12 = _mm_set1_pd(yy[1]);
          __m128d B21 = _mm_set1_pd((yy+k)[0]);
          __m128d B22 = _mm_set1_pd((yy+k)[1]);
#       endif

#       if ALIGN_FORWARD & 8
        {
          __m128d S1 = _mm_set_sd((xx+j)[0]);
          __m128d S2 = _mm_set_sd((xx+j+n)[0]);
          _mm_store_sd (z+j,
                        _mm_add_sd (_mm_add_sd (_mm_load_sd(z+j),
                                                _mm_mul_sd(S1,cast128(B11))),
                                    _mm_mul_sd(S2,cast128(B12))));
          _mm_store_sd (z+j+zn,
                        _mm_add_sd (_mm_add_sd (_mm_load_sd(z+j+zn),
                                                _mm_mul_sd(S1,cast128(B21))),
                                    _mm_mul_sd(S2,cast128(B22))));
          j += 1;
        }
#       endif

#       if CAN_USE_AVX && ALIGN >= 32
        if (((uintptr_t)(z+j) & 0x1f) != 0)
        {
          __m128d S1 = _mm_loadA_pd(xx+j);
          __m128d S2 = _mm_loadu_pd(xx+j+n);
          _mm_storeA_pd (z+j,
                         _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j),
                                                 _mm_mul_pd(S1,cast128(B11))),
                                     _mm_mul_pd(S2,cast128(B12))));
          _mm_storeu_pd (z+j+zn,
                         _mm_add_pd (_mm_add_pd (_mm_loadu_pd(z+j+zn),
                                                 _mm_mul_pd(S1,cast128(B21))),
                                     _mm_mul_pd(S2,cast128(B22))));
          j += 2;
        }
#       endif

        if (j <= xrows-4)
#       if CAN_USE_AVX
        {
          if (ALIGN >= 32 && (n & 3) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m256d S1 = _mm256_loadA_pd(xx+j);
              __m256d S2 = _mm256_loadA_pd(xx+j+n);
              _mm256_storeA_pd (z+j, 
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadA_pd(z+j),
                                                        _mm256_mul_pd(S1,B11)),
                                         _mm256_mul_pd(S2,B12)));
              _mm256_storeA_pd (z+j+zn,
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadA_pd(z+j+zn),
                                                        _mm256_mul_pd(S1,B21)),
                                         _mm256_mul_pd(S2,B22)));
                j += 4;
            } while (j <= xrows-4);
          }
          else if (ALIGN >= 32 && ((uintptr_t)(xx+j) & 0x1f) == 0)
          { do
            { __m256d S1 = _mm256_loadA_pd(xx+j);
              __m256d S2 = _mm256_loadu_pd(xx+j+n);
              _mm256_storeA_pd (z+j, 
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadA_pd(z+j),
                                                        _mm256_mul_pd(S1,B11)),
                                         _mm256_mul_pd(S2,B12)));
              _mm256_storeu_pd (z+j+zn,
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadu_pd(z+j+zn),
                                                       _mm256_mul_pd(S1,B21)),
                                         _mm256_mul_pd(S2,B22)));
              j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            { __m256d S1 = _mm256_loadu_pd(xx+j);
              __m256d S2 = _mm256_loadu_pd(xx+j+n);
              _mm256_storeA_pd (z+j, 
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadA_pd(z+j),
                                                        _mm256_mul_pd(S1,B11)),
                                         _mm256_mul_pd(S2,B12)));
              _mm256_storeu_pd (z+j+zn,
                          _mm256_add_pd (_mm256_add_pd (_mm256_loadu_pd(z+j+zn),
                                                        _mm256_mul_pd(S1,B21)),
                                         _mm256_mul_pd(S2,B22)));
              j += 4;
            } while (j <= xrows-4);
          }
        }
#       else  /* CAN_USE_SSE2 */
        {
          if (ALIGN >= 16 && (n & 1) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m128d S1, S2;
              S1 = _mm_loadA_pd(xx+j);
              S2 = _mm_loadA_pd(xx+j+n);
              _mm_storeA_pd (z+j, 
                       _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j),
                                               _mm_mul_pd(S1,B11)),
                                   _mm_mul_pd(S2,B12)));
              _mm_storeA_pd (z+j+zn,
                       _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j+zn),
                                               _mm_mul_pd(S1,B21)),
                                   _mm_mul_pd(S2,B22)));
              S1 = _mm_loadA_pd(xx+j+2);
              S2 = _mm_loadA_pd(xx+j+n+2);
              _mm_storeA_pd (z+j+2, 
                       _mm_add_pd (_mm_add_pd(_mm_loadA_pd(z+j+2),
                                              _mm_mul_pd(S1,B11)),
                                   _mm_mul_pd(S2,B12)));
              _mm_storeA_pd (z+j+zn+2, 
                       _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j+zn+2),
                                               _mm_mul_pd(S1,B21)),
                                   _mm_mul_pd(S2,B22)));
                j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            { __m128d S1, S2;
              S1 = _mm_loadA_pd(xx+j);
              S2 = _mm_loadu_pd(xx+j+n);
              _mm_storeA_pd (z+j,
                       _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j),
                                               _mm_mul_pd(S1,B11)),
                                   _mm_mul_pd(S2,B12)));
              _mm_storeu_pd (z+j+zn,
                       _mm_add_pd (_mm_add_pd (_mm_loadu_pd(z+j+zn),
                                               _mm_mul_pd(S1,B21)),
                                   _mm_mul_pd(S2,B22)));
              S1 = _mm_loadA_pd(xx+j+2);
              S2 = _mm_loadu_pd(xx+j+n+2);
              _mm_storeA_pd (z+j+2,
                       _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j+2),
                                               _mm_mul_pd(S1,B11)),
                                   _mm_mul_pd(S2,B12)));
              _mm_storeu_pd (z+j+zn+2,
                       _mm_add_pd (_mm_add_pd (_mm_loadu_pd(z+j+zn+2),
                                               _mm_mul_pd(S1,B21)),
                                   _mm_mul_pd(S2,B22)));
              j += 4;
            } while (j <= xrows-4);
          }
        }
#       endif

        if (j <= xrows-2)
        { __m128d S1 = _mm_loadA_pd(xx+j);
          __m128d S2 = _mm_loadu_pd(xx+j+n);
          _mm_storeA_pd (z+j, 
                   _mm_add_pd (_mm_add_pd (_mm_loadA_pd(z+j),
                                           _mm_mul_pd(S1,cast128(B11))),
                               _mm_mul_pd(S2,cast128(B12))));
          _mm_storeu_pd (z+j+zn,
                   _mm_add_pd (_mm_add_pd (_mm_loadu_pd(z+j+zn),
                                           _mm_mul_pd(S1,cast128(B21))),
                               _mm_mul_pd(S2,cast128(B22))));
          j += 2;
        }

        if (j < xrows)
        { __m128d S1 = _mm_set_sd((xx+j)[0]);
          __m128d S2 = _mm_set_sd((xx+j+n)[0]);
          _mm_store_sd (z+j,
                        _mm_add_sd (_mm_add_sd (_mm_load_sd(z+j),
                                                _mm_mul_sd(S1,cast128(B11))),
                                    _mm_mul_sd(S2,cast128(B12))));
          _mm_store_sd (z+j+zn,
                        _mm_add_sd (_mm_add_sd (_mm_load_sd(z+j+zn),
                                                _mm_mul_sd(S1,cast128(B21))),
                                    _mm_mul_sd(S2,cast128(B22))));
        }
      }
#     else  /* non-SIMD code */
      {
        double b11 = yy[0];
        double b12 = yy[1];
        double b21 = (yy+k)[0];
        double b22 = (yy+k)[1];
        double s1, s2;

        while (j <= xrows-2)
        { s1 = xx[j];
          s2 = (xx+n)[j];
          z[j] = (z[j] + (s1 * b11)) + (s2 * b12);
          (z+zn)[j] = ((z+zn)[j] + (s1*b21)) + (s2*b22);
          s1 = xx[j+1];
          s2 = (xx+n)[j+1];
          z[j+1] = (z[j+1] + (s1 * b11)) + (s2 * b12);
          (z+zn)[j+1] = ((z+zn)[j+1] + (s1*b21)) + (s2*b22);
          j += 2;
        }

        if (j < xrows)
        { s1 = xx[j];
          s2 = (xx+n)[j];
          z[j] = (z[j] + (s1 * b11)) + (s2 * b12);
          (z+zn)[j] = ((z+zn)[j] + (s1*b21)) + (s2*b22);
        }
      }
#     endif

      xx += n; xx += n;
      yy += 2;
      i += 2;
    }

    /* Add products with the last column of x, if not already done above. */

    if (i < xcols)
    { 
      int j = 0;

#     if CAN_USE_AVX || CAN_USE_SSE2
      {
#       if CAN_USE_AVX
          __m256d B1 = _mm256_set1_pd (yy[0]);
          __m256d B2 = _mm256_set1_pd ((yy+k)[0]);
#       else
          __m128d B1 = _mm_set1_pd (yy[0]);
          __m128d B2 = _mm_set1_pd ((yy+k)[0]);
#       endif

#       if ALIGN_FORWARD & 8
        {
          __m128d S = _mm_load_sd(xx+j);
          _mm_store_sd (z+j, _mm_add_sd (_mm_load_sd(z+j),
                                         _mm_mul_sd(S, cast128(B1))));
          _mm_store_sd (z+j+zn, _mm_add_sd (_mm_load_sd(z+j+zn),
                                            _mm_mul_sd(S, cast128(B2))));
          j += 1;
        }
#       endif

#       if CAN_USE_AVX && ALIGN >= 32
        if (((uintptr_t)(z+j) & 0x1f) != 0)
        { __m128d S = _mm_loadA_pd(xx+j);
          _mm_storeA_pd (z+j, _mm_add_pd (_mm_loadA_pd(z+j),
                                          _mm_mul_pd(S,cast128(B1))));
          _mm_storeu_pd (z+j+zn, _mm_add_pd(_mm_loadu_pd(z+j+zn),
                                            _mm_mul_pd(S,cast128(B2))));
          j += 2;
        }
#       endif

        if (j <= xrows-4)
#       if CAN_USE_AVX
        {
          if (ALIGN >= 32 && (n & 3) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m256d S = _mm256_loadA_pd(xx+j);
              _mm256_storeA_pd (z+j, _mm256_add_pd (_mm256_loadA_pd(z+j),
                                                    _mm256_mul_pd(S,B1)));
              _mm256_storeA_pd (z+j+zn, _mm256_add_pd (_mm256_loadA_pd(z+j+zn),
                                                       _mm256_mul_pd(S,B2)));
              j += 4;
            } while (j <= xrows-4);
          }
          else if (ALIGN >= 32 && ((uintptr_t)(xx+j) & 0x1f) == 0)
          { do
            { __m256d S = _mm256_loadA_pd(xx+j);
              _mm256_storeA_pd (z+j, _mm256_add_pd (_mm256_loadA_pd(z+j),
                                                    _mm256_mul_pd(S,B1)));
              _mm256_storeu_pd (z+j+zn, _mm256_add_pd(_mm256_loadu_pd(z+j+zn),
                                                      _mm256_mul_pd(S,B2)));
              j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            { __m256d S = _mm256_loadu_pd(xx+j);
              _mm256_storeA_pd (z+j, _mm256_add_pd (_mm256_loadA_pd(z+j),
                                                   _mm256_mul_pd(S,B1)));
              _mm256_storeu_pd (z+j+zn, _mm256_add_pd(_mm256_loadu_pd(z+j+zn),
                                                      _mm256_mul_pd(S,B2)));
              j += 4;
            } while (j <= xrows-4);
          }
        }
#       else  /* CAN_USE_SSE2 */
        {
          if (ALIGN >= 16 && (n & 1) == 0)  /* +n keeps ptr aligned*/
          { do
            { __m128d S;
              S = _mm_loadA_pd(xx+j);
              _mm_storeA_pd (z+j, _mm_add_pd (_mm_loadA_pd(z+j),
                                              _mm_mul_pd(S,cast128(B1))));
              _mm_storeA_pd (z+j+zn, _mm_add_pd (_mm_loadA_pd(z+j+zn),
                                                 _mm_mul_pd(S,cast128(B2))));
              S = _mm_loadA_pd(xx+j+2);
              _mm_storeA_pd (z+j+2,_mm_add_pd (_mm_loadA_pd(z+j+2),
                                               _mm_mul_pd(S,cast128(B1))));
              _mm_storeA_pd (z+j+zn+2, _mm_add_pd (_mm_loadA_pd(z+j+zn+2),
                                                   _mm_mul_pd(S,cast128(B2))));
              j += 4;
            } while (j <= xrows-4);
          }
          else
          { do
            { __m128d S;
              S = _mm_loadA_pd(xx+j);
              _mm_storeA_pd (z+j, _mm_add_pd (_mm_loadA_pd(z+j),
                                              _mm_mul_pd(S,cast128(B1))));
              _mm_storeu_pd (z+j+zn, _mm_add_pd (_mm_loadu_pd(z+j+zn),
                                                 _mm_mul_pd(S,cast128(B2))));
              S = _mm_loadA_pd(xx+j+2);
              _mm_storeA_pd (z+j+2, _mm_add_pd (_mm_loadA_pd(z+j+2),
                                                _mm_mul_pd(S,cast128(B1))));
              _mm_storeu_pd (z+j+zn+2, _mm_add_pd (_mm_loadu_pd(z+j+zn+2),
                                                   _mm_mul_pd(S,cast128(B2))));
              j += 4;
            } while (j <= xrows-4);
          }
        }
#       endif

        if (j <= xrows-2)
        { __m128d S = _mm_loadA_pd(xx+j);
          _mm_storeA_pd (z+j, _mm_add_pd (_mm_loadA_pd(z+j),
                                          _mm_mul_pd(S,cast128(B1))));
          _mm_storeu_pd (z+j+zn, _mm_add_pd (_mm_loadu_pd(z+j+zn),
                                             _mm_mul_pd(S,cast128(B2))));
          j += 2;
        }

        if (j < xrows)
        { __m128d S = _mm_load_sd(xx+j);
          _mm_store_sd (z+j, _mm_add_sd (_mm_load_sd(z+j),
                                         _mm_mul_sd(S, cast128(B1))));
          _mm_store_sd (z+j+zn, _mm_add_sd (_mm_load_sd(z+j+zn),
                                            _mm_mul_sd(S, cast128(B2))));
        }
      }

#     else  /* non-SIMD code */
      {
        double b1 = yy[0];
        double b2 = (yy+k)[0];
        while (j <= xrows-2)
        { double s1 = xx[j];
          double s2 = xx[j+1];
          z[j] += s1 * b1;
          z[j+1] += s2 * b1;
          (z+zn)[j] += s1 * b2;
          (z+zn)[j+1] += s2 * b2;
          j += 2;
        }
        if (j < xrows)
        { double s = xx[j];
          z[j] += s * b1;
          (z+zn)[j] += s * b2;
        }
      }
#     endif
    }

    AMTOUT(z+zn+xrows);

    /* Move to the next pairs of y and z columns. */

    z += zn; z += zn;
    y += k; y += k;
    m -= 2;
  }

  /* If m is odd, compute the last column of the result. */

  if (m >= 1)
  { 
    double *xx = x;
    double *yy = y;
    int i = 0;
    int j;

    /* If we're not adding, initialize sums in the last column of
       z to the products of the first two elements of the last
       column of y with the first two columns of x. */

    if (!add)
    { double b0 = yy[i+0];
      double b1 = yy[i+1];
      j = 0;
#     if ALIGN_FORWARD & 8
      { z[j] = xx[j] * b0 + (xx+n)[j] * b1;
        j += 1;
      }
#     endif
      while (j <= xrows-2)
      { z[j+0] = xx[j+0] * b0 + (xx+n)[j+0] * b1;
        z[j+1] = xx[j+1] * b0 + (xx+n)[j+1] * b1;
        j += 2;
      }
      if (j < xrows)
      { z[j] = xx[j] * b0 + (xx+n)[j] * b1;
      }
      xx += n; xx += n;
      i += 2;
    }

    /* Each time around this loop, add the products of two columns
       of x with two elements of the last column of y to the last
       column of the result. */

    while (i <= xcols-2)
    { 
#     if CAN_USE_SSE2 || CAN_USE_AVX
      {
#       if CAN_USE_AVX
          __m256d B0 = _mm256_set1_pd (yy[i+0]);
          __m256d B1 = _mm256_set1_pd (yy[i+1]);
#       else
          __m128d B0 = _mm_set1_pd (yy[i+0]);
          __m128d B1 = _mm_set1_pd (yy[i+1]);
#       endif

        j = 0;

#       if ALIGN_FORWARD & 8
        { __m128d T = _mm_load_sd(z+j);
          T = _mm_add_sd (
                 _mm_add_sd (T, _mm_mul_sd (_mm_load_sd (xx+j), cast128(B0))),
                 _mm_mul_sd (_mm_load_sd (xx+n+j), cast128(B1)));
          _mm_store_sd (z+j, T);
          j += 1;
        }
#       endif

#       if CAN_USE_AVX && (ALIGN_FORWARD & 16)
        { __m128d T = _mm_loadA_pd(z+j);
          T = _mm_add_pd (
                 _mm_add_pd (T, _mm_mul_pd (_mm_loadA_pd (xx+j), cast128(B0))),
                 _mm_mul_pd (_mm_loadu_pd (xx+n+j), cast128(B1)));
          _mm_storeA_pd (z+j, T);
          j += 2;
        }
#       endif

        while (j <= xrows-4)
        {
#         if CAN_USE_AVX
          {
            __m256d T = _mm256_loadu_pd(z+j);
            T = _mm256_add_pd (
                   _mm256_add_pd (T, _mm256_mul_pd (_mm256_loadu_pd(xx+j),B0)),
                   _mm256_mul_pd (_mm256_loadu_pd (xx+n+j), B1));
            _mm256_storeu_pd (z+j, T);
          }
#         else  /* CAN_USE_SSE2 */
          {
            __m128d T;
            T = _mm_loadA_pd(z+j);
            T = _mm_add_pd (
                   _mm_add_pd (T, _mm_mul_pd (_mm_loadA_pd (xx+j), B0)),
                   _mm_mul_pd (_mm_loadu_pd (xx+n+j), B1));
            _mm_storeA_pd (z+j, T);
            T = _mm_loadA_pd(z+j+2);
            T = _mm_add_pd (
                    _mm_add_pd (T, _mm_mul_pd (_mm_loadA_pd (xx+j+2), B0)),
                    _mm_mul_pd (_mm_loadu_pd (xx+n+j+2), B1));
            _mm_storeA_pd (z+j+2, T);
          }
#         endif
          j += 4;
        }

        if (j <= xrows-2)
        { __m128d T = _mm_loadA_pd(z+j);
          T = _mm_add_pd (
                 _mm_add_pd (T, _mm_mul_pd (_mm_loadA_pd (xx+j), cast128(B0))),
                 _mm_mul_pd (_mm_loadu_pd (xx+n+j), cast128(B1)));
          _mm_storeA_pd (z+j, T);
          j += 2;
        }

        if (j < xrows)
        { __m128d T = _mm_load_sd(z+j);
          T = _mm_add_sd (
                 _mm_add_sd (T, _mm_mul_sd (_mm_load_sd (xx+j), cast128(B0))),
                 _mm_mul_sd (_mm_load_sd (xx+n+j), cast128(B1)));
          _mm_store_sd (z+j, T);
        }
      }

#     elif CAN_USE_SSE2
      {
        j = 0;
#       if ALIGN_FORWARD & 8
          __m128d T = _mm_load_sd(z+j);
          T = _mm_add_sd (
                 _mm_add_sd (T, _mm_mul_sd (_mm_load_sd (xx+j), B0)),
                 _mm_mul_sd (_mm_load_sd (xx+n+j), B1));
          _mm_store_sd (z+j, T);
          j += 1;
#       endif
        while (j <= xrows-2)
        { __m128d T = _mm_loadA_pd(z+j);
          T = _mm_add_pd (
                 _mm_add_pd (T, _mm_mul_pd (_mm_loadA_pd (xx+j), B0)),
                 _mm_mul_pd (_mm_loadu_pd (xx+n+j), B1));
          _mm_storeA_pd (z+j, T);
          j += 2;
        }
        if (j < xrows)
        { __m128d T = _mm_load_sd(z+j);
          T = _mm_add_sd (
                 _mm_add_sd (T, _mm_mul_sd (_mm_load_sd (xx+j), B0)),
                 _mm_mul_sd (_mm_load_sd (xx+n+j), B1));
          _mm_store_sd (z+j, T);
        }
      }

#     else  /* non-SIMD code */
      {
        double b0 = yy[i+0];
        double b1 = yy[i+1];
        j = 0;
        while (j <= xrows-2)
        { z[j+0] = (z[j+0] + xx[j+0] * b0) + (xx+n)[j+0] * b1;
          z[j+1] = (z[j+1] + xx[j+1] * b0) + (xx+n)[j+1] * b1;
          j += 2;
        }
        if (j < xrows)
        { z[j] = (z[j] + xx[j] * b0) + (xx+n)[j] * b1;
        }
      }
#     endif

      xx += n; xx += n;
      i += 2;
    }

    /* Add products with final column of x, if not done above. */

    if (i < xcols)
    { double b = yy[i];
      j = 0;
#     if ALIGN_FORWARD & 8
      { z[j] += xx[j] * b;
        j += 1;
      }
#     endif
      while (j <= xrows-2)
      { z[j+0] += xx[j+0] * b;
        z[j+1] += xx[j+1] * b;
        j += 2;
      }
      if (j < xrows)
      { z[j] += xx[j] * b;
      }
    }
  }
}

/* Multiply 2 x k matrix x by k x m matrix y, storing result in z.

   The same alignment assumptions hold for x, y, and z as with the
   visible procedures. */

static void matprod_mat_mat_n2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int k, int m)
{
# if DEBUG_PRINTF
    debug_printf("mat_mat_n2 %p %p %p - %d %d\n",
                              x, y, z,   k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Compute two columns of the result each time around this loop,
     updating y, z, and m accordingly. */

  while (m > 1)
  { 
    double *xx = x;
    int i = 0;

#   if CAN_USE_SSE2
    {
      __m128d S0 = _mm_setzero_pd();
      __m128d S1 = _mm_setzero_pd();

      /* Each time around this loop, add the products of two
         columns of x with elements of the next two columns of y
         to the sums. */

      while (i <= k-2)
      { __m128d X;
        X = _mm_loadAA_pd(xx);
        S0 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd((y+k)[i])), S1);
        X = _mm_loadAA_pd(xx+2);
        S0 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i+1])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd((y+k)[i+1])), S1);
        xx += 4;
        i += 2;
      }

      if (i < k)
      { __m128d X;
        X = _mm_loadAA_pd(xx);
        S0 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd((y+k)[i])), S1);
      }

      /* Store sums in the next two result columns. */

      _mm_storeAA_pd (z, S0);
      _mm_storeAA_pd (z+2, S1);
    }

#   else  /* non-SIMD code */
    {
      double s[4] = { 0, 0, 0, 0 };

      /* Each time around this loop, add the products of two
         columns of x with elements of the next two columns of y
         to the sums. */

      while (i <= k-2)
      { double b11 = y[i];
        double b12 = y[i+1];
        double b21 = (y+k)[i];
        double b22 = (y+k)[i+1];
        s[0] = (s[0] + (xx[0] * b11)) + (xx[2] * b12);
        s[1] = (s[1] + (xx[1] * b11)) + (xx[3] * b12);
        s[2] = (s[2] + (xx[0] * b21)) + (xx[2] * b22);
        s[3] = (s[3] + (xx[1] * b21)) + (xx[3] * b22);
        xx += 4;
        i += 2;
      }

      if (i < k)
      { double b1 = y[i];
        double b2 = (y+k)[i];
        s[0] += xx[0] * b1;
        s[1] += xx[1] * b1;
        s[2] += xx[0] * b2;
        s[3] += xx[1] * b2;
      }

      /* Store sums in the next two result columns. */

      z[0] = s[0];
      z[1] = s[1];
      z[2] = s[2];
      z[3] = s[3];
    }
#   endif

    /* Move forward by two to next column of the result and the
       next column of y. */

    y += k; y +=k;
    z += 4;
    m -= 2;
  }

  /* If m is odd, compute the last column of the result. */

  if (m >= 1)
  { 
    double *xx = x;
    int i = 0;

#   if CAN_USE_SSE2
    {
      __m128d S = _mm_setzero_pd();

      /* Each time around this loop, add the products of the
         next two columns of x with elements of the last column
         of y to the sums. */

      while (i <= k-2)
      { __m128d X;
        X = _mm_loadAA_pd(xx);
        S = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i])), S);
        X = _mm_loadAA_pd(xx+2);
        S = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i+1])), S);
        xx += 4;
        i += 2;
      }

      if (i < k)
      { __m128d X;
        X = _mm_loadAA_pd(xx);
        S = _mm_add_pd (_mm_mul_pd (X, _mm_set1_pd(y[i])), S);
      }

      /* Store sums in the last result column. */

      _mm_storeAA_pd (z, S);
    }

#   else  /* non-SIMD code */
    {
      double s[2] = { 0, 0 };  /* sums for the two values in the result */

      /* Each time around this loop, add the products of two
         columns of x with two elements of the last column of y
         to s[0] and s[1]. */

      while (i <= k-2)
      { double b1 = y[i];
        double b2 = y[i+1];
        s[0] = (s[0] + (xx[0] * b1)) + (xx[2] * b2);
        s[1] = (s[1] + (xx[1] * b1)) + (xx[3] * b2);
        xx += 4;
        i += 2;
      }

      if (i < k)
      { double b = y[i];
        s[0] += xx[0] * b;
        s[1] += xx[1] * b;
      }

      /* Store the two sums in s[0] and s[1] in the result vector. */

      z[0] = s[0];
      z[1] = s[1];
    }
#   endif
  }
}

/* -------------------------------------------------------------------------- */
/*                                 TRANS1                                     */

/* Product of the transpose of a k x n matrix (x) and a k x m matrix (y)
   with result stored in z.

   The case of k=2 is handled specially.

   When the two operands are the same, the result will be a symmetric
   matrix.  During computation of each column or pair of columns, elements
   are copied to the corresponding rows; hence each column need be
   computed only from the diagonal element down. */

static void matprod_trans1_sub (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k, int m, double *sym EXTRAD);

static void matprod_trans1_sub_xrows (double * MATPROD_RESTRICT x,
                                      double * MATPROD_RESTRICT y,
                                      double * MATPROD_RESTRICT z,
                                      int n, int k, int m,
                                      int add, int final, int xrows, 
                                      double *sym EXTRAD);

static void matprod_trans1_sub_xrowscols (double * MATPROD_RESTRICT x,
                                          double * MATPROD_RESTRICT y,
                                          double * MATPROD_RESTRICT z,
                                          int n, int k, int m,
                                          int add, int final, 
                                          int xrows, int xcols,
                                          double *sym EXTRAD);

static void matprod_trans1_k2 (double * MATPROD_RESTRICT x,
                               double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z,
                               int n, int m);

SCOPE void matprod_trans1 (double * MATPROD_RESTRICT x,
                           double * MATPROD_RESTRICT y,
                           double * MATPROD_RESTRICT z,
                           int n, int k, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans1 %p %p %p - %d %d %d\n",
                          x, y, z,   n, k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (n <= 1)
  { if (n == 1)
    { matprod_vec_mat (x, y, z, k, m EXTRAN);
    }
    return;
  }
  if (m <= 1)
  { if (m == 1)
    { matprod_vec_mat (y, x, z, k, n EXTRAN);
    }
    return;
  }

  if (k <= 1)
  { if (k == 1)
    { matprod_outer (x, y, z, n, m EXTRAN);
    }
    else
    { set_to_zeros (z, (size_t)n*m);
    }
    return;
  }

  double *sym = x==y && n==m && (n>8 || k>8) ? z : 0;

  matprod_trans1_sub (x, y, z, n, k, m, sym EXTRAN);
}

/* Product of the transpose of a k x n matrix (x) and a k x m matrix
   (y) with result stored in z.  Note that x, y, and z may not be the
   full original matrix.

   If 'sym' is non-zero, the result stored in z is symmetric, and
   'sym' points to the element on the diagonal of the full matrix that
   is in the first column of z.

   Note that n, m, and k must be greater than 1.

   The case of k=2 is handled specially, ignoring any symmetry (which may
   not be advantegeious to exploit, given each element is quick to compute).

   Called above and from par-matprod.c. */

static void matprod_trans1_sub (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k, int m, double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans1_sub %p %p %p - %d %d %d %p\n",
                              x, y, z,   n, k, m, sym);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (n >= 2);
  assert (k >= 2);
  assert (m >= 2);

  if (k == 2)
  { matprod_trans1_k2 (x, y, z, n, m);
    return;
  }
    
  if (k <= TRANS1_XROWS && n <= TRANS1_XCOLS)  /* do small cases quickly */
  { matprod_trans1_sub_xrowscols (x, y, z, n, k, m,
                                  0, 1, k, n, sym EXTRAN);
    return;
  }

  int cachable = n <= TRANS1_XCOLS || n <= TRANS1_XCOLS*TRANS1_XROWS/k
                  ? m : 1 + (DOUBLES_IN_LLC / (TRANS1_XROWS + TRANS1_XCOLS));

  int mm = m;

  for (;;)
  { 
    double *xx = x;
    double *yy = y;
    int xrows = k;
    int add = 0;
    int m1 = mm;

    if (m1 > cachable)
    { m1 = mm < 2*cachable ? mm/2 : cachable;
      m1 = (m1 + 7) & ~7;
      if (m1 > mm) m1 = mm;
    }

    if (xrows > TRANS1_XROWS)
    { while (xrows >= 2*TRANS1_XROWS)
      { matprod_trans1_sub_xrows (xx, yy, z, n, k, m1,
                                  add, 0, TRANS1_XROWS, sym EXTRAZ);
        xx += TRANS1_XROWS;
        yy += TRANS1_XROWS;
        xrows -= TRANS1_XROWS;
        add = 1;
      }
      if (xrows > TRANS1_XROWS)
      { int nr = SPLITC (xrows, TRANS1_XROWS);
        matprod_trans1_sub_xrows (xx, yy, z, n, k, m1,
                                  add, 0, nr, sym EXTRAZ);
        xx += nr;
        yy += nr;
        xrows -= nr;
        add = 1;
      }
    }

    matprod_trans1_sub_xrows (xx, yy, z, n, k, m1,
                              add, 1, xrows, sym EXTRAN);

    mm -= m1;
    if (mm == 0)
      break;

    y += (size_t)m1*k;
    z += (size_t)m1*n;
    if (sym)
    { sym += (size_t)m1*n;
      sym += m1;
    }
  }
}

static void matprod_trans1_sub_xrows (double * MATPROD_RESTRICT x,
                                      double * MATPROD_RESTRICT y,
                                      double * MATPROD_RESTRICT z,
                                      int n, int k, int m,
                                      int add, int final, int xrows,
                                      double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans1_sub_xrows %p %p %p - %d %d %d - %d %d %d %p\n",
                                    x, y, z,   n, k, m,   add, final, xrows,
                                                          sym);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (xrows <= TRANS1_XROWS);

  int xcols = n;
  int chunk;

  if (xcols <= TRANS1_XCOLS
   || xcols <= (chunk = (TRANS1_XCOLS*TRANS1_XROWS/xrows) & ~7))
  { matprod_trans1_sub_xrowscols (x, y, z, n, k, m,
                                  add, final, xrows, xcols, sym EXTRAN);
    return;
  }

  while (xcols > 2*chunk)
  { matprod_trans1_sub_xrowscols (x, y, z, n, k, m, 
                                  add, final, xrows, chunk, sym EXTRAZ);
    x += (size_t)chunk*k;
    z += chunk;
    xcols -= chunk;
  }

  if (xcols > chunk)
  { int nc = ((xcols+1)/2) & ~3;  /* keep any alignment of x, z */
    matprod_trans1_sub_xrowscols (x, y, z, n, k, m, 
                                  add, final, xrows, nc, sym EXTRAZ);
    x += (size_t)nc*k;
    z += nc;
    xcols -= nc;
  }

  matprod_trans1_sub_xrowscols (x, y, z, n, k, m, 
                                add, final, xrows, xcols, sym EXTRAN);
}

static void matprod_trans1_sub_xrowscols (double * MATPROD_RESTRICT x,
                                          double * MATPROD_RESTRICT y,
                                          double * MATPROD_RESTRICT z,
                                          int n, int k, int m,
                                          int add, int final, 
                                          int xrows, int xcols,
                                          double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans1_sub_xrowscols %p %p %p - %d %d %d - %d %d %d %d %p\n",
                                        x, y, z,   n, k, m,  add, final,
                                                             xrows, xcols,
                                                             sym);
# endif

  /* For symmetric computations, shrink 'xcols' and m (which are the
     number of rows and columns of z that are computed) to eliminate
     parts that are above the diagonal.  If nothing is left, return.
     Also updates z and x accordingly.  Maintains alignment. */

  if (sym)
  { if (z+xcols <= sym)
    { return;
    }
    if (z < sym)
    { int d = (sym-z) & ~3;
      z += d;
      x += (size_t)d*k;
      xcols -= d;
    }
    double *t = z + (xcols-1);
    double *s = sym + (size_t)n * (t-sym) + (t-sym);
    if ((size_t)n * (m-1) > s - t)
    { m = 1 + (s - t) / n;
    }
  }

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int j = 0;               /* number of columns of result produced so far */
  int me = m-1;            /* limit for xcols that can be computed as pairs */

  /* Compute two columns of the result each time around this loop, updating
     y, z, and j accordingly. */

  while (j < me)
  { 
    double *xs = sym == 0 || z >= sym ? x : x + ((z-sym) & ~3);
    int nn = x + xcols - xs;
    double *zs = z;

    /* Compute sets of four elements in the two columns being computed. */

    while (nn >= 4)
    { 
#     if CAN_USE_AVX
      {
        double *r = xs;
        double *q, *qe;
        __m256d S0, /* sums for first two cols of x times 2 cols of y */
                S1; /* sums for last two cols of x times 2 cols of y */
        if (add)
        { S0 = _mm256_set_pd ((z+n)[1], (z+n)[0], z[1], z[0]);
          S1 = _mm256_set_pd ((z+n)[3], (z+n)[2], z[3], z[2]);
        }
        else
        { S0 = _mm256_setzero_pd();
          S1 = _mm256_setzero_pd();
        }
#       if ALIGN_FORWARD & 8
        { __m256d Y = _mm256_set_pd ((y+k)[0], (y+k)[0], y[0], y[0]);
          __m256d X;
          X = _mm256_set_pd ((r+k)[0], r[0], (r+k)[0], r[0]);
          S0 = _mm256_add_pd (S0, _mm256_mul_pd(X,Y));
          X = _mm256_set_pd ((r+k+k+k)[0], (r+k+k)[0],
                           (r+k+k+k)[0], (r+k+k)[0]);
          S1 = _mm256_add_pd (S1, _mm256_mul_pd(X,Y));
          r += 1;
          q = y+1;
          qe = q+((xrows-1)-3);
        }
#       else
        { q = y;
          qe = q+(xrows-3);
        }
#       endif
        while (q < qe)
        { __m256d R0, Rk, M00, M0k, Mk0, Mkk, L0, H0, Lk, Hk;
          __m256d Q0 = _mm256_loadu_pd(q);
          __m256d Qk = _mm256_loadu_pd(q+k);
          R0 = _mm256_loadu_pd(r);
          Rk = _mm256_loadu_pd(r+k);
          M00 = _mm256_mul_pd (Q0, R0);
          M0k = _mm256_mul_pd (Q0, Rk);
          Mk0 = _mm256_mul_pd (Qk, R0);
          Mkk = _mm256_mul_pd (Qk, Rk);
          L0 = _mm256_unpacklo_pd (M00, M0k);
          H0 = _mm256_unpackhi_pd (M00, M0k);
          Lk = _mm256_unpacklo_pd (Mk0, Mkk);
          Hk = _mm256_unpackhi_pd (Mk0, Mkk);
          S0 = _mm256_add_pd(S0,_mm256_permute2f128_pd(L0, Lk, 0x20));
          S0 = _mm256_add_pd(S0,_mm256_permute2f128_pd(H0, Hk, 0x20));
          S0 = _mm256_add_pd(S0,_mm256_permute2f128_pd(L0, Lk, 0x31));
          S0 = _mm256_add_pd(S0,_mm256_permute2f128_pd(H0, Hk, 0x31));
          R0 = _mm256_loadu_pd(r+k+k);
          Rk = _mm256_loadu_pd(r+k+k+k);
          M00 = _mm256_mul_pd (Q0, R0);
          M0k = _mm256_mul_pd (Q0, Rk);
          Mk0 = _mm256_mul_pd (Qk, R0);
          Mkk = _mm256_mul_pd (Qk, Rk);
          L0 = _mm256_unpacklo_pd (M00, M0k);
          H0 = _mm256_unpackhi_pd (M00, M0k);
          Lk = _mm256_unpacklo_pd (Mk0, Mkk);
          Hk = _mm256_unpackhi_pd (Mk0, Mkk);
          S1 = _mm256_add_pd(S1,_mm256_permute2f128_pd(L0, Lk, 0x20));
          S1 = _mm256_add_pd(S1,_mm256_permute2f128_pd(H0, Hk, 0x20));
          S1 = _mm256_add_pd(S1,_mm256_permute2f128_pd(L0, Lk, 0x31));
          S1 = _mm256_add_pd(S1,_mm256_permute2f128_pd(H0, Hk, 0x31));
          r += 4;
          q += 4;
        }
        qe = y+xrows;
        while (q < qe)
        { __m256d Q = _mm256_set_pd ((q+k)[0], (q+k)[0], q[0], q[0]);
          __m256d X;
          X = _mm256_set_pd ((r+k)[0], r[0], (r+k)[0], r[0]);
          S0 = _mm256_add_pd (_mm256_mul_pd(X,Q), S0);
          X = _mm256_set_pd ((r+k+k+k)[0], (r+k+k)[0],
                           (r+k+k+k)[0], (r+k+k)[0]);
          S1 = _mm256_add_pd (_mm256_mul_pd(X,Q), S1);
          r += 1;
          q += 1;
        }
        __m128d H0 = _mm256_extractf128_pd(S0,1);
        _mm_storeu_pd (z, cast128(S0));
        _mm_storeu_pd (z+n, H0);
        __m128d H1 = _mm256_extractf128_pd(S1,1);
        _mm_storeu_pd (z+2, cast128(S1));
        _mm_storeu_pd (z+n+2, H1);
      }

#     elif CAN_USE_SSE2
      {
        double *r = xs;
        double *q, *qe;
        __m128d S0,   /* sums for first two x cols times first y col */
                S1,   /* sums for first two x cols times second y col */
                S2,   /* sums for last two x cols times first y col */
                S3;   /* sums for last two x cols times second y col */
        if (add)
        { S0 = _mm_loadAA_pd (z);
          S1 = _mm_loadu_pd (z+n);
          S2 = _mm_loadAA_pd (z+2);
          S3 = _mm_loadu_pd (z+n+2);
        }
        else
        { S0 = _mm_setzero_pd();
          S1 = _mm_setzero_pd();
          S2 = _mm_setzero_pd();
          S3 = _mm_setzero_pd();
        }
#       if ALIGN_FORWARD & 8
        { __m128d X, Y0, Yk;
          X = _mm_set_pd ((r+k)[0], r[0]);
          Y0 = _mm_set_pd (y[0], y[0]);
          Yk = _mm_set_pd ((y+k)[0], (y+k)[0]);
          S0 = _mm_add_pd (S0, _mm_mul_pd(X,Y0));
          S1 = _mm_add_pd (S1, _mm_mul_pd(X,Yk));
          X = _mm_set_pd ((r+k+k+k)[0], (r+k+k)[0]);
          S2 = _mm_add_pd (S2, _mm_mul_pd(X,Y0));
          S3 = _mm_add_pd (S3, _mm_mul_pd(X,Yk));
          r += 1;
          q = y+1;
          qe = q+((xrows-1)-1);
        }
#       else
        { q = y;
          qe = q+(xrows-1);
        }
#       endif
        while (q < qe)
        { __m128d R0, Rk, M00, M0k, Mk0, Mkk, L0, H0, Lk, Hk;
          __m128d Q0 = _mm_loadA_pd(q);
          __m128d Qk = _mm_loadu_pd(q+k);
          R0 = _mm_loadA_pd(r);
          Rk = _mm_loadu_pd(r+k);
          M00 = _mm_mul_pd (Q0, R0);
          M0k = _mm_mul_pd (Q0, Rk);
          Mk0 = _mm_mul_pd (Qk, R0);
          Mkk = _mm_mul_pd (Qk, Rk);
          L0 = _mm_unpacklo_pd (M00, M0k);
          H0 = _mm_unpackhi_pd (M00, M0k);
          Lk = _mm_unpacklo_pd (Mk0, Mkk);
          Hk = _mm_unpackhi_pd (Mk0, Mkk);
          S0 = _mm_add_pd (S0, L0);
          S0 = _mm_add_pd (S0, H0);
          S1 = _mm_add_pd (S1, Lk);
          S1 = _mm_add_pd (S1, Hk);
          R0 = _mm_loadA_pd(r+k+k);
          Rk = _mm_loadu_pd(r+k+k+k);
          M00 = _mm_mul_pd (Q0, R0);
          M0k = _mm_mul_pd (Q0, Rk);
          Mk0 = _mm_mul_pd (Qk, R0);
          Mkk = _mm_mul_pd (Qk, Rk);
          L0 = _mm_unpacklo_pd (M00, M0k);
          H0 = _mm_unpackhi_pd (M00, M0k);
          Lk = _mm_unpacklo_pd (Mk0, Mkk);
          Hk = _mm_unpackhi_pd (Mk0, Mkk);
          S2 = _mm_add_pd (S2, L0);
          S2 = _mm_add_pd (S2, H0);
          S3 = _mm_add_pd (S3, Lk);
          S3 = _mm_add_pd (S3, Hk);
          r += 2;
          q += 2;
        }
        if (q < y+xrows)
        { __m128d X;
          __m128d Q0 = _mm_set_pd (q[0], q[0]);
          __m128d Qk = _mm_set_pd ((q+k)[0], (q+k)[0]);
          X = _mm_set_pd ((r+k)[0], r[0]);
          S0 = _mm_add_pd (_mm_mul_pd(X,Q0), S0);
          S1 = _mm_add_pd (_mm_mul_pd(X,Qk), S1);
          X = _mm_set_pd ((r+k+k+k)[0], (r+k+k)[0]);
          S2 = _mm_add_pd (_mm_mul_pd(X,Q0), S2);
          S3 = _mm_add_pd (_mm_mul_pd(X,Qk), S3);
          r += 1;
          q += 1;
        }
        _mm_storeAA_pd (z, S0);
        _mm_storeu_pd (z+n, S1);
        _mm_storeAA_pd (z+2, S2);
        _mm_storeu_pd (z+n+2, S3);
      }

#     else  /* non-SIMD code */
      {
        double *r = xs;
        double s[4], s2[4];
        if (add)
        { s[0] = z[0];
          s[1] = (z+n)[0];
          s[2] = z[1];
          s[3] = (z+n)[1];
          s2[0] = z[2];
          s2[1] = (z+n)[2];
          s2[2] = z[3];
          s2[3] = (z+n)[3];
        }
        else
        { s[0] = s[1] = s[2] = s[3] = 0;
          s2[0] = s2[1] = s2[2] = s2[3] = 0;
        }
        double *q = y;
        int i = xrows;
        do
        { double t, t2;
          double u = q[0];
          double u2 = (q+k)[0];
          t = r[0];
          t2 = (r+k)[0];
          s[0] += t * u;
          s[1] += t * u2;
          s[2] += t2 * u;
          s[3] += t2 * u2;
          t = (r+k+k)[0];
          t2 = (r+k+k+k)[0];
          s2[0] += t * u;
          s2[1] += t * u2;
          s2[2] += t2 * u;
          s2[3] += t2 * u2;
          r += 1;
          q += 1;
        } while (--i > 0);
        z[0] = s[0];
        (z+n)[0] = s[1];
        z[1] = s[2];
        (z+n)[1] = s[3];
        z[2] = s2[0];
        (z+n)[2] = s2[1];
        z[3] = s2[2];
        (z+n)[3] = s2[3];
      }
#     endif

      z += 4;
      xs += k; xs += k; xs += k; xs += k;
      nn -= 4;
    }

    /* Compute the remaining elements of the columns here. */

    if (nn > 1)  /* at least two more elements to compute in each column */
    { 
#     if CAN_USE_AVX
      {
        double *r = xs;
        double *q, *qe;
        __m256d S;
        if (add)
        { S = _mm256_set_pd ((z+n)[1], (z+n)[0], z[1], z[0]);
        }
        else
        { S = _mm256_setzero_pd();
        }
#       if ALIGN_FORWARD & 8
        { __m256d X = _mm256_set_pd ((r+k)[0], r[0], (r+k)[0], r[0]);
          __m256d Y = _mm256_set_pd ((y+k)[0], (y+k)[0], y[0], y[0]);
          S = _mm256_add_pd (S, _mm256_mul_pd(X,Y));
          r += 1;
          q = y+1;
          qe = q+((xrows-1)-3);
        }
#       else
        { q = y;
          qe = q+(xrows-3);
        }
#       endif
        while (q < qe)
        { __m256d Q0 = _mm256_loadu_pd(q);
          __m256d R0 = _mm256_loadu_pd(r);
          __m256d Rk = _mm256_loadu_pd(r+k);
          __m256d Qk = _mm256_loadu_pd(q+k);
          __m256d M00 = _mm256_mul_pd (Q0, R0);
          __m256d M0k = _mm256_mul_pd (Q0, Rk);
          __m256d Mk0 = _mm256_mul_pd (Qk, R0);
          __m256d Mkk = _mm256_mul_pd (Qk, Rk);
          __m256d L0 = _mm256_unpacklo_pd (M00, M0k);
          __m256d H0 = _mm256_unpackhi_pd (M00, M0k);
          __m256d Lk = _mm256_unpacklo_pd (Mk0, Mkk);
          __m256d Hk = _mm256_unpackhi_pd (Mk0, Mkk);
          S = _mm256_add_pd (S, _mm256_permute2f128_pd(L0, Lk, 0x20));
          S = _mm256_add_pd (S, _mm256_permute2f128_pd(H0, Hk, 0x20));
          S = _mm256_add_pd (S, _mm256_permute2f128_pd(L0, Lk, 0x31));
          S = _mm256_add_pd (S, _mm256_permute2f128_pd(H0, Hk, 0x31));
          r += 4;
          q += 4;
        }
        qe = y+xrows;
        while (q < qe)
        { __m256d X = _mm256_set_pd ((r+k)[0], r[0], (r+k)[0], r[0]);
          __m256d Y = _mm256_set_pd ((q+k)[0], (q+k)[0], q[0], q[0]);
          S = _mm256_add_pd (_mm256_mul_pd(X,Y), S);
          r += 1;
          q += 1;
        }
        __m128d H = _mm256_extractf128_pd(S,1);
        _mm_storeu_pd (z, cast128(S));
        _mm_storeu_pd (z+n, H);
      }

#     elif CAN_USE_SSE2
      {
        double *r = xs;
        double *q, *qe;
        __m128d S0, S1;
        if (add)
        { S0 = _mm_loadAA_pd (z);
          S1 = _mm_loadu_pd (z+n);
        }
        else
        { S0 = _mm_setzero_pd();
          S1 = _mm_setzero_pd();
        }
#       if ALIGN_FORWARD & 8
        { __m128d X, Y;
          X = _mm_set_pd ((r+k)[0], r[0]);
          Y = _mm_set_pd (y[0], y[0]);
          S0 = _mm_add_pd (S0, _mm_mul_pd(X,Y));
          Y = _mm_set_pd ((y+k)[0], (y+k)[0]);
          S1 = _mm_add_pd (S1, _mm_mul_pd(X,Y));
          r += 1;
          q = y+1;
          qe = q+((xrows-1)-1);
        }
#       else
        { q = y;
          qe = q+(xrows-1);
        }
#       endif
        while (q < qe)
        { __m128d Q0 = _mm_loadA_pd(q);
          __m128d R0 = _mm_loadA_pd(r);
          __m128d Rk = _mm_loadu_pd(r+k);
          __m128d Qk = _mm_loadu_pd(q+k);
          __m128d M00 = _mm_mul_pd (Q0, R0);
          __m128d M0k = _mm_mul_pd (Q0, Rk);
          __m128d Mk0 = _mm_mul_pd (Qk, R0);
          __m128d Mkk = _mm_mul_pd (Qk, Rk);
          __m128d L0 = _mm_unpacklo_pd (M00, M0k);
          __m128d H0 = _mm_unpackhi_pd (M00, M0k);
          __m128d Lk = _mm_unpacklo_pd (Mk0, Mkk);
          __m128d Hk = _mm_unpackhi_pd (Mk0, Mkk);
          S0 = _mm_add_pd (S0, L0);
          S0 = _mm_add_pd (S0, H0);
          S1 = _mm_add_pd (S1, Lk);
          S1 = _mm_add_pd (S1, Hk);
          r += 2;
          q += 2;
        }
        if (q < y+xrows)
        { __m128d X, Y;
          X = _mm_set_pd ((r+k)[0], r[0]);
          Y = _mm_set_pd (q[0], q[0]);
          S0 = _mm_add_pd (_mm_mul_pd(X,Y), S0);
          Y = _mm_set_pd ((q+k)[0], (q+k)[0]);
          S1 = _mm_add_pd (_mm_mul_pd(X,Y), S1);
          r += 1;
          q += 1;
        }
        _mm_storeAA_pd (z, S0);
        _mm_storeu_pd (z+n, S1);
      }

#     else  /* non-SIMD code */
      {
        double *r = xs;
        double s[4];
        if (add)
        { s[0] = z[0];
          s[1] = (z+n)[0];
          s[2] = z[1];
          s[3] = (z+n)[1];
        }
        else
        { s[0] = s[1] = s[2] = s[3] = 0;
        }
        double *q = y;
        int i = xrows;
        do
        { double t = r[0];
          double t2 = (r+k)[0];
          double u = q[0];
          double u2 = (q+k)[0];
          s[0] += t * u;
          s[1] += t * u2;
          s[2] += t2 * u;
          s[3] += t2 * u2;
          r += 1;
          q += 1;
        } while (--i > 0);
        z[0] = s[0];
        (z+n)[0] = s[1];
        z[1] = s[2];
        (z+n)[1] = s[3];
      }

#     endif

      z += 2;
      xs += k; xs += k;
      nn -= 2;
    }

    if (nn >= 1)  /* one more element to compute in each column */
    { double s0, s1;
      double *r = xs;
      double *q = y;
      if (add)
      { s0 = z[0];
        s1 = (z+n)[0];
      }
      else
      { s0 = s1 = 0;
      }
      int i = xrows;
      do
      { double t = r[0];
        s0 += t * q[0];
        s1 += t * (q+k)[0];
        q += 1;
        r += 1;
      } while (--i > 0);
      z[0] = s0;
      (z+n)[0] = s1;
      z += 1;
      nn -= 1;
    }

    /* Copy to symmetric elements. */

    if (sym && final)
    { double *s = sym + (size_t)n * (zs-sym);
      double *t = zs;
      while (t < z)
      { s[0] = t[0];
        s[1] = (t+n)[0];
        s += n;
        t += 1;
      }
    }

    if (final)
    { AMTOUT(z+n);
    }

    /* Go on to next two columns of y and z. */

    z = zs + n + n;
    y += k; y += k;
    j += 2;

    if (sym)
    { sym += n; sym += n;
      if (z <= sym)
      { xcols -= 2;
        x += k; x += k;
        z += 2;
      }
      sym += 2;
    }
  }

  /* If m is odd, compute the final column of the result. */

  if (m & 1)
  { 
    double *xs = sym == 0 || z >= sym ? x : x + ((z-sym) & ~3);
    double *e = z+xcols;
    double *zs = z;

    /* If xcols is odd, compute the first element of the column here. */

    if (xcols & 1)
    { double *r = xs;
      double s;
      if (add)
      { s = z[0];
      }
      else
      { s = 0;
      }
      double *q = y;
      double *f = y+xrows;
      do
      { s += r[0] * q[0];
        r += 1;
        q += 1;
      } while (q < f);
      z[0] = s;
      xs += k;
      z += 1;
    }

    /* Compute the remainder of the column two elements at a time. */

    while (z < e)
    { double s0 = 0;
      double s1 = 0;
      if (add)
      { s0 = z[0];
        s1 = z[1];
      }
      else
      { s0 = s1 = 0;
      }
      double *r = xs;
      double *q = y;
      double *f = y+xrows;
      do
      { double t = q[0];
        s0 += r[0] * t;
        s1 += (r+k)[0] * t;
        r += 1;
        q += 1;
      } while (q < f);
      z[0] = s0;
      z[1] = s1;
      xs += k; xs += k;
      z += 2;
    }

    /* Copy to symmetric elements. */

    if (sym && final)
    { double *s = sym + (size_t)n * (zs-sym);
      double *t = zs;
      while (t < z)
      { s[0] = t[0];
        s += n;
        t += 1;
      }
    }
  }
}

/* Multiply the transpose of the 2 x n matrix x by the m x 2 matrix y, storing
   the result in the n x m matrix z. */

void matprod_trans1_k2 (double * MATPROD_RESTRICT x,
                        double * MATPROD_RESTRICT y,
                        double * MATPROD_RESTRICT z,
                        int n, int m)
{
# if DEBUG_PRINTF
    debug_printf("trans1_k2 %p %p %p - %d %d\n",
                             x, y, z,   n, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

# if CAN_USE_AVX || CAN_USE_SSE3
  {
    double *e = y + m + m;
    while (y < e)
    {
#     if CAN_USE_AVX
        __m256d Y = _mm256_set_pd (y[1], y[0], y[1], y[0]);
#     else  /* CAN_USE_SSE3 */
        __m128d Y = _mm_set_pd (y[1], y[0]);
#     endif
      double *p = x;
      int i = 0;
      while (i <= n-4)
      {
#       if CAN_USE_AVX
        { __m256d M0 = _mm256_mul_pd (Y, _mm256_loadAA_pd(p));
          __m256d M1 = _mm256_mul_pd (Y, _mm256_loadAA_pd(p+4));
          __m256d Z = _mm256_hadd_pd (_mm256_permute2f128_pd (M0, M1, 0x20),
                                     _mm256_permute2f128_pd (M0, M1, 0x31));
          _mm256_storeu_pd (z, Z);
        }
#       else  /* CAN_USE_SSE3 */
        { __m128d M0, M1;
          M0 = _mm_mul_pd (Y, _mm_loadAA_pd(p));
          M1 = _mm_mul_pd (Y, _mm_loadAA_pd(p+2));
          _mm_storeu_pd (z, _mm_hadd_pd (M0, M1));
          M0 = _mm_mul_pd (Y, _mm_loadAA_pd(p+4));
          M1 = _mm_mul_pd (Y, _mm_loadAA_pd(p+6));
          _mm_storeu_pd (z+2, _mm_hadd_pd (M0, M1));
        }
#       endif
        z += 4;
        p += 8;
        i += 4;
      }
      if (i <= n-2)
      { __m128d M0 = _mm_mul_pd (cast128(Y), _mm_loadu_pd(p));
        __m128d M1 = _mm_mul_pd (cast128(Y), _mm_loadu_pd(p+2));
        _mm_storeu_pd (z, _mm_hadd_pd (M0, M1));
        z += 2;
        p += 4;
        i += 2;
      }
      if (i < n)
      { __m128d M0 = _mm_mul_pd (cast128(Y), _mm_loadu_pd(p));
        _mm_store_sd (z, _mm_hadd_pd (M0, M0));
        z += 1;
      }
      y += 2;
    }
  }

# else  /* non-SIMD code */
  {
    double *e = y + m + m;
    while (y < e)
    { double t0 = y[0];
      double t1 = y[1];
      double *p = x;
      int i = 0;
      while (i <= n-2)
      { z[0] = p[0] * t0 + p[1] * t1;
        z[1] = p[2] * t0 + p[3] * t1;
        z += 2;
        p += 4;
        i += 2;
      }
      if (i < n)
      { z[0] = p[0] * t0 + p[1] * t1;
        z += 1;
      }
      y += 2;
    }
  }
# endif
}

/* -------------------------------------------------------------------------- */
/*                                 TRANS2                                     */

/* Product of an n x k matrix (x) and the transpose of an m x k matrix (y)
   with the result stored in z.

   When the two operands are the same, the result will be a symmetric
   matrix.  Only the lower-triangular part of the result is computed,
   with the elements in columns that are computed then being copied to
   the corresponding elements in rows above the diagonal.

   Cases where n is two are handled specially, accumulating sums in
   two local variables rather than in a column of the result, and then
   storing them in the result column at the end. */

static void matprod_trans2_sub (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k, int m,
                                int yrows, double *sym EXTRAD);

static void matprod_trans2_sub_xrows (double * MATPROD_RESTRICT x,
                                      double * MATPROD_RESTRICT y,
                                      double * MATPROD_RESTRICT z,
                                      int n, int k, int m,
                                      int xrows, int yrows, double *sym EXTRAD);

static void matprod_trans2_sub_xrowscols (double * MATPROD_RESTRICT x,
                                          double * MATPROD_RESTRICT y,
                                          double * MATPROD_RESTRICT z,
                                          int n, int k, int m,
                                          int xrows, int yrows, int xcols,
                                          int add, int final, 
                                          double *sym EXTRAD);

static void matprod_trans2_n2 (double * MATPROD_RESTRICT x,
                               double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z,
                               int k, int m, int yrows);

SCOPE void matprod_trans2 (double * MATPROD_RESTRICT x,
                           double * MATPROD_RESTRICT y,
                           double * MATPROD_RESTRICT z,
                           int n, int k, int m EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans2 %p %p %p - %d %d %d\n",
                          x, y, z,   n, k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (k <= 1)
  { if (k == 1)
    { matprod_outer (x, y, z, n, m EXTRAN);
    }
    else
    { set_to_zeros (z, (size_t)n*m);
    }
    return;
  }

  if (n <= 1)
  { if (n == 1)
    { matprod_mat_vec (y, x, z, m, k);
    }
    return;
  }

  if (m <= 1)
  { if (m == 1)
    { matprod_mat_vec (x, y, z, n, k);
    }
    return;
  }

  double *sym = x==y && n==m && (n>8 || k>8) ? z : 0;

  matprod_trans2_sub (x, y, z, n, k, m, m, sym EXTRAN);
}

/* Compute 'yrows' columns of the product of x and the transpose of y,
   storing the result in z.  Note that y and z do not necessarily
   point to the start of the entire matrix, but m will be the amount
   to step to the next column of y, and n will be the amount to step
   to the next column of z.

   If 'sym' is non-zero, the result stored in z is symmetric, and
   'sym' points to the element on the diagonal of the full matrix
   that is in the first column of z.

   Note that n, m, and k must be greater than 1.

   Called above and from par-matprod.c. */

static void matprod_trans2_sub (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k, int m,
                                int yrows, double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans2_sub %p %p %p - %d %d %d - %d %p\n",
                              x, y, z,   n, k, m,  yrows, sym);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (n >= 2);
  assert (k >= 2);
  assert (m >= 2);

  if (n == 2)
  { matprod_trans2_n2 (x, y, z, k, m, yrows);
    return;
  }

  if (n <= TRANS2_XROWS && k <= TRANS2_XCOLS)  /* do small cases quickly */
  { matprod_trans2_sub_xrowscols (x, y, z, n, k, m,
                                  n, yrows, k, 0, 1, sym EXTRAN);
    return;
  }

  int cachable = n <= TRANS2_XROWS ? yrows
                        : 1 + (int) (DOUBLES_IN_LLC / (TRANS2_XROWS+(double)k));

  int mm = yrows;

  for (;;)
  { 
    int m1 = mm;

    if (m1 > cachable)
    { m1 = mm < 2*cachable ? mm/2 : cachable;
      m1 = (m1 + 7) & ~7;
      if (m1 > mm) m1 = mm;
    }

    double *xx = x;
    double *zz = z;
    int xrows = n;

    if (xrows > TRANS2_XROWS)
    { 
      while (xrows >= 2*TRANS2_XROWS)
      { matprod_trans2_sub_xrows (xx, y, zz, n, k, m,
                                  TRANS2_XROWS, m1, sym EXTRAZ);
        xx += TRANS2_XROWS;
        zz += TRANS2_XROWS;
        xrows -= TRANS2_XROWS;
      }

      if (xrows > TRANS2_XROWS)
      { int nr = SPLITC (xrows, TRANS2_XROWS);
        matprod_trans2_sub_xrows (xx, y, zz, n, k, m,
                                  nr, m1, sym EXTRAZ);
        xx += nr;
        zz += nr;
        xrows -= nr;
      }
    }

    matprod_trans2_sub_xrows (xx, y, zz, n, k, m, 
                              xrows, m1, sym EXTRAN);

    mm -= m1;
    if (mm == 0)
    { break;
    }

    y += m1;
    z += (size_t)m1*n;
    if (sym)
    { sym += (size_t)m1*n;
      sym += m1;
    }
  }
}

/* Compute 'xrows' rows and 'yrows' columns of the product of x and
   the transpose of y, storing the result in z.  Note that y and z do
   not necessarily point to the start of the entire matrix, but m will
   be the amount to step to the next column of y, and n will be the
   amount to step to the next column of z.

   'xrows' must be no larger than TRANS2_XROWS.

   If 'sym' is non-zero, the result stored in z is symmetric, and
   'sym' points to the element on the diagonal of the full matrix
   that is in the first column of z. */

static void matprod_trans2_sub_xrows (double * MATPROD_RESTRICT x,
                                      double * MATPROD_RESTRICT y,
                                      double * MATPROD_RESTRICT z,
                                      int n, int k, int m,
                                      int xrows, int yrows, double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans2_sub_xrows %p %p %p - %d %d %d - %d %d %p\n",
                                    x, y, z,   n, k, m,  xrows, yrows, sym);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (xrows <= TRANS2_XROWS);

  int chunk;

  if (k <= TRANS2_XCOLS
   || k <= (chunk = (TRANS2_XCOLS*TRANS2_XROWS/xrows) & ~7))
  { matprod_trans2_sub_xrowscols (x, y, z, n, k, m,
                                  xrows, yrows, k, 0, 1, sym EXTRAN);
    return;
  }

  int xcols = k;
  int add = 0;

  while (xcols > 2*chunk)
  { matprod_trans2_sub_xrowscols (x, y, z, n, k, m,
                                  xrows, yrows, chunk, add, 0, sym EXTRAZ);
    x += (size_t)chunk*n;
    y += (size_t)chunk*m;
    xcols -= chunk;
    add = 1;
  }

  if (xcols > chunk)
  { int nc = ((xcols+1)/2) & ~3;  /* keep any alignment of x */
    matprod_trans2_sub_xrowscols (x, y, z, n, k, m,
                                  xrows, yrows, nc, add, 0, sym EXTRAZ);
    x += (size_t)nc*n;
    y += (size_t)nc*m;
    xcols -= nc;
    add = 1;
  }

  matprod_trans2_sub_xrowscols (x, y, z, n, k, m,
                                xrows, yrows, xcols, add, 1, sym EXTRAN);
}

/* Compute 'xcols' products of the sums that give the values of the
   elements in 'xrows' rows and 'yrows' columns of the product of x
   and the transpose of y, storing the result in z if 'add' is 0, or
   adding to it if 'add' is non-zero.  Note that y and z do not
   necessarily point to the start of the entire matrix, but m will be
   the amount to step to the next column of y, and n will be the
   amount to step to the next column of z.

   If 'sym' is non-zero, the result stored in z is symmetric, and
   'sym' points to the element on the diagonal of the full matrix that
   is in the first column of z.  This information is used to shrink
   the number of rows and columns of z that are computed (sometimes to
   none) and to find the symmetrical counterpart to which computed
   elements should be copied.  The copying need be done only if 'final'
   is 1, indicating that this is the final change to the result elements. */

static void matprod_trans2_sub_xrowscols (double * MATPROD_RESTRICT x,
                                          double * MATPROD_RESTRICT y,
                                          double * MATPROD_RESTRICT z,
                                          int n, int k, int m,
                                          int xrows, int yrows, int xcols,
                                          int add, int final, 
                                          double *sym EXTRAD)
{
# if DEBUG_PRINTF
    debug_printf("trans2_sub_xrowscols %p %p %p - %d %d %d - %d %d %d %d %p\n",
                                        x, y, z,   n, k, m,  xrows,yrows,xcols,
                                                             add, sym);
# endif

  assert (m >= 2);
  assert (xcols >= 2);

  /* For symmetric computations, shrink 'xrows' and 'yrows' (which are
     also the number of rows and columns of z that are computed) to
     eliminate parts that are above the diagonal.  If nothing is left,
     return.  Also updates z and x accordingly.  Maintains alignment. */

  if (sym)
  { if (z+xrows <= sym)
    { return;
    }
    if (z < sym)
    { int d = (sym-z) & ~3;
      z += d;
      x += d;
      xrows -= d;
    }
    double *t = z + (xrows-1);
    double *s = sym + (size_t)n * (t-sym) + (t-sym);
    if ((size_t)n * (yrows-1) > s - t)
    { yrows = 1 + (s - t) / n;
    }
  }

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  int m2 = yrows;

  /* Compute two columns of the result each time around this loop. */

  double *ex = x + (size_t)n*(xcols-1) + xrows;

  while (m2 > 1)
  { 
    double *ez = z + xrows - 1; /* Last place to store a sum */
    double *xs = sym == 0 || z >= sym ? x : x + ((z-sym) & ~3);
    double *q = y;

    /* Unless we're adding, initialize sums in next two columns of
       z to the sum of the first two products.  Note that xcols and m
       are at least two here. */

    if (!add)
    {
#     if 1  /* non-SIMD code */
      {
        double *t = z;
        double *r = xs;
        double b11 = y[0];
        double b12 = (y+m)[0];
        double b21 = y[1];
        double b22 = (y+m)[1];
        do
        { double s1 = r[0];
          double s2 = (r+n)[0];
          t[0] = (s1 * b11) + (s2 * b12);
          (t+n)[0] = (s1 * b21) + (s2 * b22);
          r += 1;
          t += 1;
        } while (t <= ez);
        xs += n; xs += n;
        q += m; q += m;
      }
#     endif
    }

    /* Each time around this loop, add the products of two columns
       of x with elements of the next two rows of y to the next
       two columns the result vector, z (except parts that aren't
       needed when symetric). */

    while (xs < ex-n)
    { 
      double *t = z;
      double *r = xs;

#     if CAN_USE_AVX || CAN_USE_SSE2
      {
#       if CAN_USE_AVX
          __m256d B11 = _mm256_set1_pd(q[0]);
          __m256d B12 = _mm256_set1_pd((q+m)[0]);
          __m256d B21 = _mm256_set1_pd(q[1]);
          __m256d B22 = _mm256_set1_pd((q+m)[1]);
#       else
          __m128d B11 = _mm_set1_pd(q[0]);
          __m128d B12 = _mm_set1_pd((q+m)[0]);
          __m128d B21 = _mm_set1_pd(q[1]);
          __m128d B22 = _mm_set1_pd((q+m)[1]);
#       endif

#       if ALIGN_OFFSET & 8
        {
          __m128d S1 = _mm_load_sd(r);
          __m128d S2 = _mm_load_sd(r+n);
          __m128d T1 = _mm_load_sd(t);
          __m128d T2 = _mm_load_sd(t+n);
          T1 = _mm_add_sd (_mm_mul_sd (S2, cast128(B12)),
                           _mm_add_sd (_mm_mul_sd (S1, cast128(B11)), T1));
          T2 = _mm_add_sd (_mm_mul_sd (S2, cast128(B22)),

                           _mm_add_sd (_mm_mul_sd (S1, cast128(B21)), T2));
          _mm_store_sd (t, T1);
          _mm_store_sd (t+n, T2);
          t += 1;
          r += 1;
        }
#       endif

        while (t < ez-2)
        {
#         if CAN_USE_AVX
          { __m256d S1, S2, T1, T2;
            S1 = _mm256_loadu_pd(r);
            S2 = _mm256_loadu_pd(r+n);
            T1 = _mm256_loadu_pd(t);
            T2 = _mm256_loadu_pd(t+n);
            T1 = _mm256_add_pd (_mm256_mul_pd (S2, B12),
                                _mm256_add_pd (_mm256_mul_pd (S1, B11), T1));
            T2 = _mm256_add_pd (_mm256_mul_pd (S2, B22),
                                _mm256_add_pd (_mm256_mul_pd (S1, B21), T2));
            _mm256_storeu_pd (t, T1);
            _mm256_storeu_pd (t+n, T2);
          }
#         else  /* CAN_USE_SSE2 */
          { __m128d S1, S2, T1, T2;
            S1 = _mm_loadu_pd(r);
            S2 = _mm_loadu_pd(r+n);
            T1 = _mm_loadu_pd(t);
            T2 = _mm_loadu_pd(t+n);
            T1 = _mm_add_pd (_mm_mul_pd (S2, cast128(B12)),
                             _mm_add_pd (_mm_mul_pd (S1, cast128(B11)), T1));
            T2 = _mm_add_pd (_mm_mul_pd (S2, cast128(B22)),
                             _mm_add_pd (_mm_mul_pd (S1, cast128(B21)), T2));
            _mm_storeu_pd (t, T1);
            _mm_storeu_pd (t+n, T2);
            S1 = _mm_loadu_pd(r+2);
            S2 = _mm_loadu_pd(r+n+2);
            T1 = _mm_loadu_pd(t+2);
            T2 = _mm_loadu_pd(t+n+2);
            T1 = _mm_add_pd (_mm_mul_pd (S2, cast128(B12)),
                             _mm_add_pd (_mm_mul_pd (S1, cast128(B11)), T1));
            T2 = _mm_add_pd (_mm_mul_pd (S2, cast128(B22)),
                             _mm_add_pd (_mm_mul_pd (S1, cast128(B21)), T2));
            _mm_storeu_pd (t+2, T1);
            _mm_storeu_pd (t+n+2, T2);
          }
#         endif
          t += 4;
          r += 4;
        }

        if (t < ez)
        { __m128d S1 = _mm_loadu_pd(r);
          __m128d S2 = _mm_loadu_pd(r+n);
          __m128d T1 = _mm_loadu_pd(t);
          __m128d T2 = _mm_loadu_pd(t+n);
          T1 = _mm_add_pd (_mm_mul_pd (S2, cast128(B12)),
                           _mm_add_pd (_mm_mul_pd (S1, cast128(B11)), T1));
          T2 = _mm_add_pd (_mm_mul_pd (S2, cast128(B22)),
                           _mm_add_pd (_mm_mul_pd (S1, cast128(B21)), T2));
          _mm_storeu_pd (t, T1);
          _mm_storeu_pd (t+n, T2);
          t += 2;
          r += 2;
        }

        if (t <= ez)
        { __m128d S1 = _mm_load_sd(r);
          __m128d S2 = _mm_load_sd(r+n);
          __m128d T1 = _mm_load_sd(t);
          __m128d T2 = _mm_load_sd(t+n);
          T1 = _mm_add_sd (_mm_mul_sd (S2, cast128(B12)),
                           _mm_add_sd (_mm_mul_sd (S1, cast128(B11)), T1));
          T2 = _mm_add_sd (_mm_mul_sd (S2, cast128(B22)),
                           _mm_add_sd (_mm_mul_sd (S1, cast128(B21)), T2));
          _mm_store_sd (t, T1);
          _mm_store_sd (t+n, T2);
        }
      }
#     else  /* non-SIMD code */
      {
        double b11 = q[0];
        double b12 = (q+m)[0];
        double b21 = q[1];
        double b22 = (q+m)[1];
        while (t < ez)
        { double s11 = r[0];
          double s12 = (r+n)[0];
          double s21 = r[1];
          double s22 = (r+n)[1];
          t[0] = (t[0] + (s11 * b11)) + (s12 * b12);
          t[1] = (t[1] + (s21 * b11)) + (s22 * b12);
          t[n] = (t[n] + (s11 * b21)) + (s12 * b22);
          (t+n)[1] = ((t+n)[1] + (s21 * b21)) + (s22 * b22);
          t += 2;
          r += 2;
        }
        if (t <= ez)
        { double s1 = r[0];
          double s2 = (r+n)[0];
          t[0] = (t[0] + (s1 * b11)) + (s2 * b12);
          (t+n)[0] = ((t+n)[0] + (s1 * b21)) + (s2 * b22);
        }
      }
#     endif
      xs += n; xs += n;
      q += m; q += m;
    }

    if (xs < ex)
    { double *t = z;
      double *r = xs;
      double b1 = q[0];
      double b2 = q[1];
      do
      { double s = r[0];
        t[0] += s * b1;
        (t+n)[0] += s * b2;
        r += 1;
        t += 1;
      } while (t <= ez);
    }

    /* Copy to symmetric elements. */

    if (sym && final)
    { double *s = sym + (size_t)n * (z-sym);
      double *t = z;
      while (t <= ez) 
      { s[0] = t[0];
        s[1] = (t+n)[0];
        s += n;
        t += 1;
      }
    }

    if (final)
    { AMTOUT(ez+n+1);
    }

    /* Move forward by two, to the next column of the result and the
       next row of y. */

    z += n; z += n;
    y += 2;
    m2 -= 2;

    if (sym) 
    { sym += n; sym += n;
      if (z <= sym)
      { xrows -= 2;
        x += 2;
        z += 2;
      }
      sym += 2;
    }
  }

  /* If 'yrows' is odd, compute the last column of the result. */

  if (m2 >= 1)
  { 
    double *ez = z + xrows - 1; /* Last place to store a sum */
    double *xs = sym == 0 || z >= sym ? x : x + ((z-sym) & ~3);
    double *q = y;

    /* Unless we're adding, initialize sums in z to the product of
       the first element of the last row of y with the first
       column of x. */

    if (!add)
    { double *r = xs;
      double *t = z;
      double b = q[0];
      int n2 = xrows;
      while (n2 > 1)
      { t[0] = r[0] * b;
        t[1] = r[1] * b;
        r += 2;
        t += 2;
        n2 -= 2;
      }
      if (n2 >= 1)
      { t[0] = r[0] * b;
      }
      q += m;
      xs += n;
    }

    /* Each time around this loop, add the products of two
       columns of x with two elements of the first row of y to
       the result vector, z.  Adjust r and y to account for this. */

    while (xs < ex-n)
    { double *r = xs;
      double *t = z;
      double b1, b2;
      b1 = q[0];
      b2 = (q+m)[0];
      while (t < ez)
      { t[0] = (t[0] + (r[0] * b1)) + ((r+n)[0] * b2);
        t[1] = (t[1] + (r[1] * b1)) + ((r+n)[1] * b2);
        r += 2;
        t += 2;
      }
      if (t <= ez)
      { t[0] = (t[0] + (r[0] * b1)) + ((r+n)[0] * b2);
        r += 1;
      }
      q += m; q += m;
      xs += n; xs += n;
    }

    if (xs < ex)
    { double *r = xs;
      double *t = z;
      double b = q[0];
      while (t < ez)
      { t[0] += r[0] * b;
        t[1] += r[1] * b;
        r += 2;
        t += 2;
      }
      if (t <= ez)
      { t[0] += r[0] * b;
      }
    }

    /* Copy to symmetric elements. */

    if (sym && final)
    { double *s = sym + (size_t)n * (z-sym);
      double *t = z;
      while (t <= ez) 
      { s[0] = t[0];
        s += n;
        t += 1;
      }
    }
  }
}

static void matprod_trans2_n2 (double * MATPROD_RESTRICT x,
                               double * MATPROD_RESTRICT y,
                               double * MATPROD_RESTRICT z,
                               int k, int m, int yrows)
{
# if DEBUG_PRINTF
    debug_printf("trans2_n2 %p %p %p - %d %d - %d\n",
                             x, y, z,   k, m,  yrows);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Compute two columns of the result each time around this loop,
     updating y, z, and m accordingly. */

  while (yrows > 1)
  { 
    double *r = x;
    double *q = y;
    int k2 = k;

    /* The loops below each time around add the products of two
       columns of x with elements of the next two rows of y to the
       sums, adjusting r and q to account for this. */

#   if CAN_USE_AVX && ENABLE_ALL_AVX_CODE

      __m256d S = _mm256_setzero_pd();

      while (k2 > 1)
      { __m256d Q, R;
        __m128d X;
        Q = _mm256_set_pd (q[1], q[1], q[0], q[0]);
        X = _mm_loadAA_pd(r);
        R = _mm256_insertf128_pd (_mm256_castpd128_pd256(X), X, 1);
        S = _mm256_add_pd (_mm256_mul_pd(R,Q), S);
        Q = _mm256_set_pd ((q+m)[1], (q+m)[1], (q+m)[0], (q+m)[0]);
        X = _mm_loadAA_pd(r+2);
        R = _mm256_insertf128_pd (_mm256_castpd128_pd256(X), X, 1);
        S = _mm256_add_pd (_mm256_mul_pd(R,Q), S);
        q += m; q += m;
        r += 4;
        k2 -= 2;
      }

      if (k2 >= 1)
      { __m256d Q, R;
        Q = _mm256_set_pd (q[1], q[1], q[0], q[0]);
        R = _mm256_set_pd (r[1], r[0], r[1], r[0]);
        S = _mm256_add_pd (_mm256_mul_pd(R,Q), S);
      }

      /* Store sums in the next two result columns. */

      _mm256_storeAA_pd (z, S);

#   elif CAN_USE_SSE2

      __m128d S0 = _mm_setzero_pd();
      __m128d S1 = _mm_setzero_pd();

      while (k2 > 1)
      { __m128d R;
        R = _mm_loadAA_pd(r);
        S0 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd(q[0])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd(q[1])), S1);
        R = _mm_loadAA_pd(r+2);
        S0 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd((q+m)[0])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd((q+m)[1])), S1);
        q += m; q += m;
        r += 4;
        k2 -= 2;
      }

      if (k2 >= 1)
      { __m128d R;
        R = _mm_loadAA_pd(r);
        S0 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd(q[0])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (R, _mm_set1_pd(q[1])), S1);
      }

      /* Store sums in the next two result columns. */

      _mm_storeAA_pd (z, S0);
      _mm_storeAA_pd (z+2, S1);

#   else  /* non-SIMD code */

      double s[4] = { 0, 0, 0, 0 };  /* sums for two columns of result */

      while (k2 > 1)
      { double b00 = q[0];
        double b10 = q[1];
        double b01 = (q+m)[0];
        double b11 = (q+m)[1];
        s[0] = (s[0] + (r[0] * b00)) + (r[2] * b01);
        s[1] = (s[1] + (r[1] * b00)) + (r[3] * b01);
        s[2] = (s[2] + (r[0] * b10)) + (r[2] * b11);
        s[3] = (s[3] + (r[1] * b10)) + (r[3] * b11);
        q += m; q += m;
        r += 4;
        k2 -= 2;
      }

      /* Add one more product if necessary. */

      if (k2 >= 1)
      { double b0 = q[0];
        double b1 = q[1];
        s[0] += r[0] * b0;
        s[1] += r[1] * b0;
        s[2] += r[0] * b1;
        s[3] += r[1] * b1;
      }

      /* Store sums in the next two result columns. */

      z[0] = s[0];
      z[1] = s[1];
      z[2] = s[2];
      z[3] = s[3];

#   endif

    /* Move forward two columns to the next column to compute of
       the result, and also move forward by two rows of y, in
       corresponding fashion. */

    y += 2;     /* forward by two rows */
    z += 4;     /* forward by two columns */
    yrows -= 2;
  }

  /* If m is odd, compute the last column of the result. */

  if (yrows >= 1)
  { 
    double *r = x;
    double *q = y;
    int k2 = k;

    /* The loops below each time around add the products of two
       columns of x with two elements of the last row of y to the
       sums. */

#   if CAN_USE_SSE2
    {
      __m128d S = _mm_setzero_pd();

      while (k2 > 1)
      { S = _mm_add_pd (S, _mm_mul_pd (_mm_set1_pd (q[0]),
                                       _mm_loadAA_pd(r)));
        S = _mm_add_pd (S, _mm_mul_pd (_mm_set1_pd ((q+m)[0]),
                                       _mm_loadAA_pd(r+2)));
        q += m; q += m;
        r += 4;
        k2 -= 2;
      }

      if (k2 >= 1)
      { S = _mm_add_pd (S, _mm_mul_pd (_mm_set1_pd (q[0]),
                                       _mm_loadAA_pd(r)));
      }

      /* Store the two sums in S in the last column of the result. */

      _mm_storeAA_pd (z, S);
    }
#   else  /* non-SIMD code */
    {
      double s[2] = { 0, 0 }; /* sums for 2 values in last col of result*/

      while (k2 > 1)
      { double b1 = q[0];
        double b2 = (q+m)[0];
        s[0] = (s[0] + (r[0] * b1)) + (r[2] * b2);
        s[1] = (s[1] + (r[1] * b1)) + (r[3] * b2);
        q += m; q += m;
        r += 4;
        k2 -= 2;
      }

      if (k2 >= 1)
      { double b = q[0];
        s[0] += r[0] * b;
        s[1] += r[1] * b;
      }

      /* Store the two sums in s[0] and s[1] in the last column of
         the result. */

      z[0] = s[0];
      z[1] = s[1];
    }
#   endif
  }
}

/* -------------------------------------------------------------------------- */
/*                                TRANS12                                     */

/* Product of the transpose of an k x n matrix (x) and the transpose
   of an m x k matrix (y) with the result stored in z. */

static void matprod_trans12_sub (double * MATPROD_RESTRICT x,
                                 double * MATPROD_RESTRICT y,
                                 double * MATPROD_RESTRICT z,
                                 int n, int k, int m, int zcols);
static void matprod_trans12_m2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k);

SCOPE void matprod_trans12 (double * MATPROD_RESTRICT x,
                            double * MATPROD_RESTRICT y,
                            double * MATPROD_RESTRICT z,
                            int n, int k, int m)
{
# if DEBUG_PRINTF
    debug_printf("trans12 %p %p %p - %d %d %d\n",
                           x, y, z,   n, k, m);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  if (k <= 1)
  { if (k == 1)
    { matprod_outer (x, y, z, n, m EXTRAZ);
    }
    else
    { set_to_zeros (z, (size_t)n*m);
    }
    return;
  }

  if (n <= 1)
  { if (n == 1)
    { matprod_mat_vec (y, x, z, m, k);
    }
    return;
  }

  if (m <= 1)
  { if (m == 1)
    { matprod_vec_mat (y, x, z, k, n EXTRAZ);
    }
    return;
  }

  matprod_trans12_sub (x, y, z, n, k, m, m);
}

/* Store the first 'zcols' columns of the product of the transpose of
   x and the transpose of y.  Note that y and z do not necessarily
   point to the start of the original operands, but m gives the amount
   to step to get to the next column of y, and n gives the amount to
   step to get to the next column of z.

   Note that n, m, k, and zcols must be at least 2.

   The usual alignment assumptions must hold for x, y, and z.

   Called above and from par-matprod.c. */

static void matprod_trans12_sub (double * MATPROD_RESTRICT x,
                                 double * MATPROD_RESTRICT y,
                                 double * MATPROD_RESTRICT z,
                                 int n, int k, int m, int zcols)
{
# if DEBUG_PRINTF
    debug_printf("trans12_sub %p %p %p - %d %d %d - %d\n",
                               x, y, z,   n, k, m,  zcols);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  assert (m >= 2);
  assert (k >= 2);
  assert (n >= 2);
  assert (zcols >= 2);

  if (m == 2)
  { assert (zcols == 2);
    matprod_trans12_m2 (x, y, z, n, k);
    return;
  }

  int zr, zc;

  zc = 128;
  if (zc > zcols)
  { zc = zcols;
  }
  zr = (TRANS12_ZELEM / zc) & ~3;
  if (zr > n)
  { zr = n;
  }

  double zelem [zr*zc + 3];  /* +3 to allow alignment */
  double *ztmp = zelem;
  while ((((uintptr_t)ztmp) & (ALIGN-1)) != ALIGN_OFFSET)
  { ztmp += 1;
  }

  int j = 0;

  while (j < zcols)
  { 
    int remaining_cols = zcols - j;
    int zzc = remaining_cols <= zc ? remaining_cols :
              remaining_cols - zc < 3 ? (remaining_cols/2) & ~3 : zc;

    double *xx = x;
    int i = 0;

    while (i < n)
    { 
      int remaining_rows = n - i;
      int zzr = remaining_rows <= zr ? remaining_rows :
            remaining_rows - zr < 3 ? remaining_rows/2 : zr;

      matprod_mat_mat_sub_xrows (y, xx, ztmp, m, k, zzr,
                                 zzc, zzc EXTRAZ);

      int ii, jj;
      for (ii = 0; ii < zzr; ii++)
      { for (jj = 0; jj < zzc; jj++)
        { z [i + ii + (size_t)jj*n] = ztmp[jj + ii*zzc];
        }
      }

      xx += (size_t)zzr*k;
      i += zzr;
    }

    y += zzc;
    z += (size_t)zzc*n;
    j += zzc;
  }
}

/* Multiply the transpose of the k x n matrix x by the transpose of
   the 2 x k matrix y, storing the result in the n x 2 matrix z.

   The usual alignment assumptions must hold for x, y, and z. */

static void matprod_trans12_m2 (double * MATPROD_RESTRICT x,
                                double * MATPROD_RESTRICT y,
                                double * MATPROD_RESTRICT z,
                                int n, int k)
{
# if DEBUG_PRINTF
    debug_printf("trans12_m2 %p %p %p - %d %d\n",
                              x, y, z,   n, k);
# endif

  CHK_ALIGN(x); CHK_ALIGN(y); CHK_ALIGN(z);

  x = ASSUME_ALIGNED (x, ALIGN, ALIGN_OFFSET);
  y = ASSUME_ALIGNED (y, ALIGN, ALIGN_OFFSET);
  z = ASSUME_ALIGNED (z, ALIGN, ALIGN_OFFSET);

  /* Compute two rows of the result each time around this loop,
     updating x and z accordingly. */

  int j = 0;

  while (j <= n-2)
  { 
    double *yy = y;
    int i = 0;

#   if CAN_USE_SSE2
    {
      __m128d S0 = _mm_setzero_pd();  /* sums for first row of z */
      __m128d S1 = _mm_setzero_pd();  /* sums for second row of z */

      /* Each time around this loop, add the products of two
         columns of y with elements of the next two columns of x
         to the sums. */

      while (i <= k-2)
      { __m128d Y;
        Y = _mm_loadAA_pd(yy);
        S0 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd((x+k)[i])), S1);
        Y = _mm_loadAA_pd(yy+2);
        S0 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i+1])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd((x+k)[i+1])), S1);
        yy += 4;
        i += 2;
      }

      if (i < k)
      { __m128d Y;
        Y = _mm_loadAA_pd(yy);
        S0 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i])), S0);
        S1 = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd((x+k)[i])), S1);
      }

      /* Store sums in the next two result columns. */

      _mm_storeAA_pd (z, _mm_unpacklo_pd(S0,S1));
      _mm_storeu_pd (z+n, _mm_unpackhi_pd(S0,S1));
    }

#   else  /* non-SIMD code */
    {
      double s[4] = { 0, 0, 0, 0 };

      /* Each time around this loop, add the products of two
         columns of y with elements of the next two columns of x
         to the sums. */

      while (i <= k-2)
      { double b11 = x[i];
        double b12 = x[i+1];
        double b21 = (x+k)[i];
        double b22 = (x+k)[i+1];
        s[0] = (s[0] + (yy[0] * b11)) + (yy[2] * b12);
        s[1] = (s[1] + (yy[1] * b11)) + (yy[3] * b12);
        s[2] = (s[2] + (yy[0] * b21)) + (yy[2] * b22);
        s[3] = (s[3] + (yy[1] * b21)) + (yy[3] * b22);
        yy += 4;
        i += 2;
      }

      if (i < k)
      { double b1 = x[i];
        double b2 = (x+k)[i];
        s[0] += yy[0] * b1;
        s[1] += yy[1] * b1;
        s[2] += yy[0] * b2;
        s[3] += yy[1] * b2;
      }

      /* Store sums in the next two result rows. */

      z[0] = s[0];
      z[1] = s[2];
      (z+n)[0] = s[1];
      (z+n)[1] = s[3];
    }
#   endif

    /* Move forward by two to next row of the result and the next
       column of x. */

    x += k; x +=k;
    z += 2;
    j += 2;
  }

  /* If n is odd, compute the last row of the result. */

  if (j < n)
  { 
    double *yy = y;
    int i = 0;

#   if CAN_USE_SSE2
    {
      __m128d S = _mm_setzero_pd();

      /* Each time around this loop, add the products of the
         next two columns of y with elements of the last column
         of x to the sums. */

      while (i <= k-2)
      { __m128d Y;
        Y = _mm_loadAA_pd(yy);
        S = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i])), S);
        Y = _mm_loadAA_pd(yy+2);
        S = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i+1])), S);
        yy += 4;
        i += 2;
      }

      if (i < k)
      { __m128d Y;
        Y = _mm_loadAA_pd(yy);
        S = _mm_add_pd (_mm_mul_pd (Y, _mm_set1_pd(x[i])), S);
      }

      /* Store sums in the last result row. */

      _mm_store_sd (z, S);
      _mm_storeh_pd (z+n, S);
    }

#   else  /* non-SIMD code */
    {
      double s[2] = { 0, 0 };  /* sums for the two values in the result */

      /* Each time around this loop, add the products of two
         columns of y with two elements of the last column of x
         to s[0] and s[1]. */

      while (i <= k-2)
      { double b1 = x[i];
        double b2 = x[i+1];
        s[0] = (s[0] + (yy[0] * b1)) + (yy[2] * b2);
        s[1] = (s[1] + (yy[1] * b1)) + (yy[3] * b2);
        yy += 4;
        i += 2;
      }

      if (i < k)
      { double b = x[i];
        s[0] += yy[0] * b;
        s[1] += yy[1] * b;
      }

      /* Store the two sums in s[0] and s[1] in the last row of result. */

      z[0] = s[0];
      (z+n)[0] = s[1];
    }
#   endif
  }
}
