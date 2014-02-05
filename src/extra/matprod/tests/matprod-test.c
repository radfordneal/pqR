/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Test Program for Matrix Multiplicaton Without Pipelining

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

#include <stdlib.h>
#include <stdio.h>

#include "test.h"
#include "matprod.h"

char *prog_name = "matprod-test";

void do_test (int rep)
{
  int r, i, v;

  for (r = 0; r<rep; r++)
  { v = vec[nmat];
    for (i = nmat-2; i>=0; i--)
    { v |= vec[i+1];
      if (vec[i] && v && matrows[i]==1 && matcols[nmat-1]==1) 
      { *product[i] = matprod_vec_vec (matrix[i], product[i+1], matcols[i]);
      }
      else if (v && matcols[nmat-1]==1)
      { matprod_mat_vec (matrix[i], product[i+1], product[i],
                         matrows[i], matcols[i]);
      }
      else if (vec[i] && matrows[i]==1)
      { matprod_vec_mat (matrix[i], product[i+1], product[i],
                         matcols[i], matcols[nmat-1]);
      }
      else if (i==0 && trans1)
      { matprod_trans1 (matrix[i], product[i+1], product[i],
                        matrows[i], matcols[i], matcols[nmat-1]);
      }
      else if (i==nmat-2 && trans2)
      { matprod_trans2 (matrix[i], product[i+1], product[i],
                        matrows[i], matcols[i], matcols[nmat-1]);
      }
      else
      { matprod_mat_mat (matrix[i], product[i+1], product[i],
                         matrows[i], matcols[i], matcols[nmat-1]);
      }
    }
  }  

  print_result();
}
