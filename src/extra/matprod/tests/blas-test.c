/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Test Program for BLAS Matrix Multiplication Routines

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

#include <stdlib.h>
#include <stdio.h>

#include "test.h"
#include "matprod.h"

char *prog_name = "blas-test";

extern double ddot_ (int *, double *, int *, double *, int *);
extern void dgemv_ (char *, int *, int *, double *, double *, int *,
                    double *, int *, double *, double *, int *);
extern void dgemm_ (char *, char *, int *, int *, int *, double *,
                    double *, int *, double *, int *, double *, 
                    double *, int *);
extern void dsyrk_ (char *, char *, int *, int *, double *, double *,
                    int *, double *, double *, int *);
                    

void xerbla_ (char *e, int i)
{
  fprintf(stderr,"BLAS error\n");
  exit(1);
}

/* Fill the lower triangle of an n-by-n matrix from the upper triangle.  Fills
   two rows at once to improve cache performance. */

static void fill_lower (double *z, int n)
{
   int i, ii, jj, e;

    /* This loop fills two rows of the lower triangle each iteration.
       Since there's nothing to fill for the first row, we can either
       start with it or with the next row, so that the number of rows
       we fill will be a multiple of two. */

    for (i = (n&1); i < n; i += 2) {

        ii = i;    /* first position to fill in the first row of the pair */
        jj = i*n;  /* first position to fetch from */

        /* This loop fills in the pair of rows, also filling the diagonal
           element of the first (which is unnecessary but innocuous). */

        e = jj+i;

        for (;;) {
            z[ii] = z[jj];
            z[ii+1] = z[jj+n];
            if (jj == e) break;
            ii += n;
            jj += 1;
        }
    }
}

void do_test (int rep)
{
  int ione = 1;
  double one = 1.0;
  double zero = 0.0;

  int r, i, v;

  for (r = 0; r<rep; r++)
  { v = vec[nmat];
    for (i = nmat-2; i>=0; i--)
    { v |= vec[i+1];
      if (vec[i] && v && matrows[i]==1 && matcols[nmat-1]==1) 
      { *product[i] = ddot_(&matcols[i],matrix[i],&ione,product[i+1],&ione);
      }
      else if (v && matcols[nmat-1]==1)
      { dgemv_("N", &matrows[i], &matcols[i], &one, matrix[i], &matrows[i],
               product[i+1], &ione, &zero, product[i], &ione);
      }
      else if (vec[i] && matrows[i]==1)
      { dgemv_("T", &matcols[i], &matcols[nmat-1], &one, 
               product[i+1], &matcols[i], matrix[i], &ione, &zero, 
               product[i], &ione);
      }
      else
      { int t1 = i==0 && trans1;
        int t2 = i==nmat-2 && trans2;
        if (t1 && matrix[i]==product[i+1])
        { dsyrk_("U", "T", &matrows[i], &matcols[i], &one, matrix[i],
                 &matcols[i], &zero, product[i], &matrows[i]);
          fill_lower(product[i],matrows[i]);
        }
        else if (t2 && matrix[i]==product[i+1])
        { dsyrk_("U", "N", &matrows[i], &matcols[i], &one, matrix[i],
                 &matrows[i], &zero, product[i], &matrows[i]);
          fill_lower(product[i],matrows[i]);
        }
        else
        { dgemm_(t1 ? "T" : "N", t2 ? "T" : "N", 
                 &matrows[i], &matcols[nmat-1], &matcols[i], &one, 
                 matrix[i], t1 ? &matcols[i] : &matrows[i], 
                 product[i+1], t2 ? &matcols[nmat-1] : &matcols[i],
                 &zero, product[i], &matrows[i]);
        }
      }
    }
  }  

  print_result();
}
