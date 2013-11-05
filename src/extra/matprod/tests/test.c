/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Common Portion of Test Programs

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
#include <string.h>

#define EXTERN
#include "test.h"

static void usage(void)
{ 
  fprintf (stderr, "Usage: %s rep [ \"t\" ] dim dim dim { dim } [ \"t\" ]\n", 
                    prog_name);
  exit(1);
}

void print_result (void)
{ 
  double *m = product[0];
  int s = prodlen[0];

  printf ("%.16g", m[0]);
  if (s>1) printf (" %.16g", m[1]);
  if (s>3) printf (" %.16g", m[s-2]);
  if (s>2) printf (" %.16g", m[s-1]);
  printf("\n");

#if 0
  { int i, j;
    printf("\n");
    for (i = 0; i<matrows[0]; i++)
    { for (j = 0; j<matcols[nmat-1]; j++)
      { printf(" %f",product[0][i+j*matrows[0]]);
      }
      printf("\n");
    }
  }
#endif
}

int main (int argc, char **argv)
{
  int rep;     /* Number of times to repeat test */
  char junk;   /* Junk variable for use in sscanf */
  int i, j, k;

  /* Process arguments. */

  if (argc<5) usage();

  if (sscanf(argv[1],"%d%c",&rep,&junk)!=1 || rep<=0) usage();

  trans1 = trans2 = 0;
  if (strcmp(argv[2],"t")==0 || strcmp(argv[2],"T")==0)
  { trans1 = 1 + (strcmp(argv[2],"T")==0);
    argv += 1;
    argc -= 1;
  }
  if (strcmp(argv[argc-1],"t")==0 || strcmp(argv[argc-1],"T")==0)
  { trans2 = 1 + (strcmp(argv[argc-1],"T")==0);
    argc -= 1;
  }

  nmat = argc-3;

  if (nmat<2 || nmat==2 && trans1 && trans2) usage();

  if (nmat>MAX_MATRICES)
  { fprintf(stderr,"Too many matrices specified\n");
    exit(1);
  }

  for (i = 0; i<argc-2; i++)
  { int d;
    if (strcmp(argv[i+2],"v")==0 || strcmp(argv[i+2],"V")==0)
    { d = 1;
      vec[i] = 1;
    }
    else
    { if (sscanf(argv[i+2],"%d%c",&d,&junk)!=1) usage();
      vec[i] = 0;
    }
    if (i<nmat) matrows[i] = d;
    if (i>0) matcols[i-1] = d;
  }

  last_V = strcmp(argv[argc-1],"V")==0;

  /* For each matrix, compute matlen and allocate space, or re-use space
     (if possible) when a "T" option applies. */

  for (i = 0; i<nmat; i++)
  { matlen[i] = matrows[i] * matcols[i];
    if (matlen[i] != (double) matrows[i] * matcols[i])
    { fprintf(stderr,"Matrix is too large\n");
      exit(2);
    }
    if (i==1 && trans1>1 || i==nmat-1 && trans2>1)
    { if (matrows[i-1]!=matcols[i])
      { fprintf(stderr,"\"T\" option used when dimensions don't match\n");
        exit(1);
      }
      matrix[i] = matrix[i-1];
    }
    else
    { matrix[i] = calloc (sizeof (double), matlen[i]);
      if (matrix[i]==0)
      { fprintf(stderr,"Couldn't allocate space for matrix\n");
        exit(2);
      }
    }
  }

  /* For each product, compute prodlen and allocate space. */

  for (i = 0; i<nmat-1; i++)
  { prodlen[i] = matrows[i] * matcols[nmat-1];
    if (prodlen[i] != (double) matrows[i] * matcols[nmat-1])
    { fprintf(stderr,"Product matrix is too large\n");
      exit(2);
    }
    product[i] = calloc (sizeof (double), prodlen[i]);
    if (product[i]==0)
    { fprintf(stderr,"Couldn't allocate space for product matrix\n");
      exit(2);
    }
  }

  /* Last "product" is actually the last input matrix. */

  product[nmat-1] = matrix[nmat-1];
  prodlen[nmat-1] = matlen[nmat-1];

  /* Initialize the matrices.  With a "T" option, the same matrix will
     be initialized twice, but to the same thing both times (due to care
     in placement of parentheses below). */

  for (i = 0; i<nmat; i++)
  { for (j = 0; j<matcols[i]; j++) 
    { for (k = 0; k<matrows[i]; k++) 
      { int ix = i==0 && trans1 || i==nmat-1 && trans2 
                   ? j + matcols[i]*k : k + matrows[i]*j;
        matrix[i][ix] = 0.1*(matrows[i]+matcols[i]) 
                      + 0.01 * (matrows[i]*matcols[i])
                      + 0.01 * ((j+1)*(k+1));
      }
    }
  }

  /* Run test on these matrices (do_test may or may not return). */

  do_test(rep);

  return 0;
}
