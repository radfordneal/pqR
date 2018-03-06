/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION
             Common Portion of Test Programs

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
#include <string.h>
#include <stdint.h>
#include <limits.h>

#define EXTERN
#include "test.h"

#ifndef ALIGN

#define ALLOC(n) calloc (sizeof (double), (n));

#else

#ifndef ALIGN_OFFSET
#define ALIGN_OFFSET 0
#endif

static inline double *ALLOC (size_t n)
{
  double *a = malloc (n * sizeof (double) + ALIGN-1);
  while ((((uintptr_t)a) & (ALIGN-1)) != ALIGN_OFFSET)
  { a += 1;
  }

# if 0
    printf ("Allocated matrix of %lld doubles, aligned as %d,%d: %p\n", 
             (long long)n, ALIGN, ALIGN_OFFSET, a);
# endif

  return a;
}

#endif

static void usage(void)
{ 
  fprintf (stderr, "Usage: %s rep dim dim dim { dim }\n", prog_name);
  exit(1);
}

void print_result (void)
{ 
  double *m = product[0];
  size_t s = prodlen[0];

  if (s>0) printf ("%.16g", m[0]);
  if (s>1) printf (" %.16g", m[1]);
  if (s>3) printf (" %.16g", m[s-2]);
  if (s>2) printf (" %.16g", m[s-1]);
  printf("\n");

  if (getenv("PRINTWHOLE") != 0)
  { int i, j;
    printf("\n");
    for (i = 0; i<matrows[0]; i++)
    { for (j = 0; j<matcols[nmat-1]; j++)
      { printf(" %f",product[0][i+matrows[0]*(size_t)j]);
      }
      printf("\n");
    }
  }

  if (getenv("PRINTNEG") != 0)
  { int i, j;
    for (i = 0; i<matrows[0]; i++)
    { for (j = 0; j<matcols[nmat-1]; j++)
      { double v = product[0][i+matrows[0]*(size_t)j];
        if (v < 0)
        { printf (" negative at %4d,%4d: %f\n", i, j, v);
        }
      }
    }
  }
}

/* Check that results are correct. */

static double *product_check[MAX_MATRICES]; /* Pointers to storage for checks */

void check_results (void)
{
  int i, v;
  
  v = vec[nmat];

  for (i = nmat-2; i>=0; i--)
  { double *x = matrix[i];
    double *y = product_check[i+1];
    double *z = product_check[i];
    size_t N = matrows[i];
    size_t K = matcols[i];
    size_t M = matcols[nmat-1];
    size_t j, k, l;
    double s;
    v |= vec[i+1];
    if (vec[i] && v && N==1 && M==1)  /* vec X vec */
    { s = 0;
      for (k = 0; k < K; k++) s += x[k] * y[k];
      z[0] = s;
    }
    else if (v && M==1)               /* mat X vec */
    { for (j = 0; j < N; j++)
      { s = 0;
        for (k = 0; k < K; k++) s += x[j+N*k] * y[k];
        z[j] = s;
      }
    }
    else if (vec[i] && matrows[i]==1) /* vec X mat */
    { for (l = 0; l < M; l++)
      { s = 0;
        for (k = 0; k < K; k++) s += x[k] * y[k+K*l];
        z[l] = s;
      }
    }
    else if (i==nmat-2 && trans[i] && trans[i+1])   /* t(mat) X t(mat) */
    { for (j = 0; j < N; j++)
      { for (l = 0; l < M; l++)
        { s = 0;
          for (k = 0; k < K; k++) s += x[k+K*j] * y[l+M*k];
          z[j+N*l] = s;
        }
      }
    }
    else if (i==nmat-2 && trans[i+1]) /* mat X t(mat) */
    { for (j = 0; j < N; j++)
      { for (l = 0; l < M; l++)
        { s = 0;
          for (k = 0; k < K; k++) s += x[j+N*k] * y[l+M*k];
          z[j+N*l] = s;
        }
      }
    }
    else if (trans[i])                /* t(mat) X mat */
    { for (j = 0; j < N; j++)
      { for (l = 0; l < M; l++)
        { s = 0;
          for (k = 0; k < K; k++) s += x[k+K*j] * y[k+K*l];
          z[j+N*l] = s;
        }
      }
    }
    else                              /* mat X mat */
    { for (j = 0; j < N; j++)
      { for (l = 0; l < M; l++)
        { s = 0;
          for (k = 0; k < K; k++) s += x[j+N*k] * y[k+K*l];
          z[j+N*l] = s;
        }
      }
    }

    for (j = 0; j < N; j++)
    { for (l = 0; l < M; l++)
      { if (z[j+N*l] != product[i][j+N*l])
        { fprintf(stderr,
                "Check failed on computation of result %d : (%d,%d) %f %f %g\n",
                 i, (int)j, (int)l, product[i][j+N*l], z[j+N*l], 
                 product[i][j+N*l]-z[j+N*l]);
          abort();
        }
      }
    }
  }  

  printf("Check OK\n");
}

int main (int argc, char **argv)
{
  int rep;                  /* Number of times to repeat test */
  char junk;                /* Junk variable for use in sscanf */
  int ndim;                 /* Number of dimensions found so far */
  int i, j, k;

  /* Process arguments. */

  if (argc<5) usage();

  if (sscanf(argv[1],"%d%c",&rep,&junk)!=1 || rep<=0) usage();

  ndim = 0;

  for (i = 2; i<argc; i++)
  { int d, v;
    if (strcmp(argv[i],"t")==0 || strcmp(argv[i],"T")==0)
    { if (i >= argc-2 || trans[ndim]!=0) usage();
      trans[ndim] = 1 + (strcmp(argv[i],"T")==0);
      continue;
    }
    if (strcmp(argv[i],"v")==0 || strcmp(argv[i],"V")==0)
    { d = 1;
      v = 1 + (strcmp(argv[i],"V")==0);
    }
    else
    { if (sscanf(argv[i],"%d%c",&d,&junk)!=1 || d<0) usage();
      v = 0;
    }
    if (ndim>MAX_MATRICES)
    { fprintf(stderr,"Too many matrices specified\n");
      exit(1);
    }
    dim[ndim] = d;
    vec[ndim] = v;
    ndim += 1;
  }

  if (ndim<3) usage();
  if (trans[nmat-2] && trans[nmat-1]) usage();

  nmat = ndim-1;

  do_check = getenv("CHECK") != NULL;

  if (trans[nmat-1]>1 && trans[nmat-2]
   || trans[nmat-2]>1 && trans[nmat-1])
  { fprintf(stderr,"\"T\" option used when other operand is also transposed\n");
    exit(1);
  }

  /* For each matrix, compute matlen and allocate space, or re-use space
     when a "T" option applies. */

  for (i = 0; i<nmat; i++)
  { matrows[i] = dim[i];
    matcols[i] = dim[i+1];
    matlen[i] = (size_t) matrows[i] * (size_t) matcols[i];
    if (trans[i]>1 && (i!=nmat-2 && i!=nmat-1)) usage();
    if (i==nmat-1 && (trans[i]>1 || trans[i-1]>1))  /* share space */
    { if (matrows[i-1]!=matcols[i])
      { fprintf(stderr,"\"T\" option used when dimensions don't match\n");
        exit(1);
      }
      matrix[i] = matrix[i-1];
    }
    else
    { matrix[i] = ALLOC(matlen[i]+5);
      if (matrix[i]==0)
      { fprintf(stderr,"Couldn't allocate space for matrix\n");
        exit(2);
      }
      matrix[i][matlen[i]] = 1.23e10;  /* in hopes of causing a wrong result */
      matrix[i][matlen[i]+1] = -4.56;  /*   if any of these are mistakenly   */
      matrix[i][matlen[i]+2] = 65432;  /*   looked at                        */
      matrix[i][matlen[i]+3] = 0.123;
      matrix[i][matlen[i]+4] = 987e7;
    }
  }

  /* For each product, compute prodlen and allocate space. */

  for (i = 0; i<nmat-1; i++)
  { prodlen[i] = (size_t) matrows[i] * (size_t) matcols[nmat-1];
    product[i] = ALLOC(prodlen[i]+1);
    if (do_check) product_check[i] = ALLOC(prodlen[i]);
    if (product[i]==0 || do_check && product_check[i]==0)
    { fprintf(stderr,"Couldn't allocate space for product matrix\n");
      exit(2);
    }
    size_t h;
    for (h = 0; h < prodlen[i]; h++)
    { product[i][h] = -7.234-h;    /* junk shouldn't affect result, but might */
    }
    product[i][prodlen[i]] = 1.1;  /* for check that it doesn't get wiped out */
  }

  /* Last "product" is actually the last input matrix. */

  product[nmat-1] = product_check[nmat-1] = matrix[nmat-1];
  prodlen[nmat-1] = matlen[nmat-1];

  /* Initialize the matrices.  With a "T" option, the same matrix will
     be initialized twice, but to the same thing both times (due to care
     in placement of parentheses below). */

# if 0  /* Enable for special debugging initialization */
#   define INITVAL(i,j,k) (i == 0 ? (j==0 && k==0 ? 1.1 : 1) \
                                  : (j==0 && k==0 ? 2.2 : 1))
# else
#   define INITVAL(i,j,k) (0.1*((double)matrows[i]+(double)matcols[i])  \
                            + 0.01 * ((double)matrows[i]*(double)matcols[i]) \
                            + 0.01 * ((j+1.0)*(k+1.0)))
# endif

  for (i = 0; i<nmat; i++)
  { for (j = 0; j<matcols[i]; j++) 
    { for (k = 0; k<matrows[i]; k++) 
      { size_t ix = trans[i] ? j + matcols[i]*(size_t)k 
                             : k + matrows[i]*(size_t)j;
        matrix[i][ix] = INITVAL(i,j,k);
      }
    }
#   if 0  /* enable to print initialized matrices */
    { printf("\nInput matrix %d\n\n",i);
      for (k = 0; k<matrows[i]; k++) 
      { for (j = 0; j<matcols[i]; j++) 
        { size_t ix = trans[i] ? j + matcols[i]*(size_t)k 
                               : k + matrows[i]*(size_t)j;
          printf(" %f",matrix[i][ix]);
        }
        printf("\n");
      }
    }
#   endif
  }

  /* Run test on these matrices (do_test may or may not return). */

  do_test(rep);

  /* Check that input matrices are the same as they were initialized to above.*/

  for (i = 0; i<nmat; i++)
  { for (j = 0; j<matcols[i]; j++) 
    { for (k = 0; k<matrows[i]; k++) 
      { size_t ix = trans[i] ? j + matcols[i]*(size_t)k 
                             : k + matrows[i]*(size_t)j;
        if (matrix[i][ix] != INITVAL(i,j,k))
        { fprintf (stderr, 
                  "Input matrix %d changed after operation (%lld, %g, %g)\n",
                   i, (long long)ix, matrix[i][ix], INITVAL(i,j,k));
          abort();
        }
      }
    }
    if (matrix[i][matlen[i]] != 1.23e10
     || matrix[i][matlen[i]+1] != -4.56
     || matrix[i][matlen[i]+2] != 65432
     || matrix[i][matlen[i]+3] != 0.123
     || matrix[i][matlen[i]+4] != 987e7)
    { fprintf (stderr,"Data after input matrix %d changed after operation\n",i);
      abort();
    }
  }

  /* Check that memory hasn't been corrupted after result matrices. */

  for (i = 0; i<nmat-1; i++)
  { if (product[i][prodlen[i]] != 1.1) 
    { fprintf (stderr, "Memory after product matrix %d corrupted (%f)\n",
                        i, product[i][prodlen[i]]);
      abort();
    }
  }

  /* Check against simple implementations. */

  if (do_check)
  { check_results();
  }

  return 0;
}
