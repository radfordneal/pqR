/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Simple Example Program

   Copyright (c) 2013 Radford M. Neal.

   The helpers library is free software; you can redistribute it and/or modify
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
#include <math.h>

#include "helpers-app.h"


/* EXTERNAL DECLARATIONS. */

double *A, *B, *C, D;            /* Variables used (A,B,C vectors, D scalar) */

int B_in_use, B_being_computed;  /* Markers for variable B (not used here) */


/* DECLARATIONS GLOBAL WITHIN THIS MODULE. */

static helpers_size_t size;  /* Size of vectors */
static int rep;             /* Number of repetitions */
static int trace;           /* Trace last repetition? */


/* SET OUTPUT VECTOR TO EXPONENTIAL OF A SEQUENCE GOING FROM 0 TO 1. */

void sequence 
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  for (i = 0; i<size; i++) 
  { o[i] = exp ((double)i / size);
  }
}


/* COMPUTE SIN OR COS OF VECTOR. */

void sin_cos
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  for (i = 0; i<size; i++)
  { o[i] = op ? sin(i1[i]) : cos(i1[i]);
  }
}


/* AVERAGE SUM OF SQUARES OF VECTOR ELEMENTS. */

void ave_sqr 
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  double sum = 0;
  for (i = 0; i<size; i++) 
  { sum += i1[i]*i1[i] + i2[i]*i2[i];
  }
  *o = sum/size;
}


/* RETURN THE NAME OF A TASK. */

char *my_task_name (helpers_task_proc *p)
{ 
  return (p)==sequence ? "sequence" 
       : (p)==sin_cos  ? "sin_cos"
       : (p)==ave_sqr  ? "ave_sqr"
       : "?";
}


/* MASTER PROCEDURE.  Schedules tasks to compute things "rep" times, then 
   checks the results of the last repetition, and looks at how much the 
   final results differ from one. */

void helpers_master (void)
{
  int r;

  printf ("\nUsing %d helpers, vector size %u, repeating %d times\n", 
             helpers_num, size, rep);

  /* Compute results "rep" times, by scheduling tasks. */

  for (r = 1; r<=rep; r++)
  {
    if (trace==1 && r==rep)
    { helpers_trace(1);
    }

    helpers_do_task (0, sequence, 0, A, NULL, NULL);
    helpers_do_task (0, sin_cos, 1, B, A, NULL);
    helpers_do_task (0, sin_cos, 0, C, A, NULL);
    helpers_do_task (0, ave_sqr, 0, &D, B, C);

    helpers_wait_for_all();
  }

  /* Print final value computed. */

  printf ("\nFinal value of D: %.8f\n", D);
}


/* MAIN PROGRAM.  Processes command arguments, allocates variables, and 
   calls helpers_startup. */

int main (int argc, char **argv)
{
  char junk;
  int s, n;

  /* Process arguments. */

  if (argc>1 && strcmp(argv[1],"-t")==0)
  { trace = 1;
    argc -= 1;
    argv += 1;
  }

  if (argc!=4
   || sscanf(argv[1],"%d%c",&n,&junk)!=1 || n<0
   || sscanf(argv[2],"%d%c",&s,&junk)!=1 || s<0
   || sscanf(argv[3],"%d%c",&rep,&junk)!=1 || rep<1)
  { fprintf (stderr, "Usage:  simple [ -t ] n-helpers vec-size repetitions\n");
    exit(1);
  }

  size = s;

  /* Allocate space for variables. */

  A = calloc (size, sizeof(double));
  B = calloc (size, sizeof(double));
  C = calloc (size, sizeof(double));

  if (A==0 || B==0 || C==0)
  { fprintf (stderr, "Not enough memory for vectors of size %u\n", size);
    exit(1);
  }

  /* Do all the computations; never returns. */

  helpers_startup(n);
}
