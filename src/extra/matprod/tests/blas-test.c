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

void xerbla_ (char *e, int i)
{
  fprintf(stderr,"BLAS error\n");
  exit(1);
}

void do_test (int rep)
{
  char *N1 = "N", *N2 = "N";
  char *T1 = "T";
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
      { dgemv_(N1, &matrows[i], &matcols[i], &one, matrix[i], &matrows[i],
               product[i+1], &ione, &zero, product[i], &ione);
      }
      else if (vec[i] && matrows[i]==1)
      { dgemv_(T1, &matcols[i], &matcols[nmat-1], &one, 
               product[i+1], &matcols[i], matrix[i], &ione, &zero, 
               product[i], &ione);
      }
      else
      { dgemm_(N1, N2, &matrows[i], &matcols[nmat-1], &matcols[i], 
               &one, matrix[i], &matrows[i], product[i+1], &matcols[i], &zero,
               product[i], &matrows[i]);
      }
    }
  }  

  if (prodlen[0]!=1) printf ("%.16g ", product[0][0]);
  printf ("%.16g\n", product[0][prodlen[0]-1]);
}
