/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION
             Test Program for BLAS Matrix Multiplication Routines

   Copyright (c) 2013, 2018 Radford M. Neal.

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
      { int t1 = trans[i];
        int t2 = i==nmat-2 ? trans[i+1] : 0;
        if (t1>1 && !t2 && matrix[i]==matrix[i+1])
        { dsyrk_("U", "T", &matrows[i], &matcols[i], &one, matrix[i],
                 &matcols[i], &zero, product[i], &matrows[i]);
          matprod_fill_lower(product[i],matrows[i]);
        }
        else if (t2>1 && !t1 && matrix[i]==matrix[i+1])
        { dsyrk_("U", "N", &matrows[i], &matcols[i], &one, matrix[i],
                 &matrows[i], &zero, product[i], &matrows[i]);
          matprod_fill_lower(product[i],matrows[i]);
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
