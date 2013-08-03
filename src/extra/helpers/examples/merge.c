/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Example Program Demonstrating Merging of Operations

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

double *A, *B, *C, D;            /* Variables (B, C, and D not used here) */

int B_in_use, B_being_computed;  /* Markers for variable B (not used here) */


/* DECLARATIONS GLOBAL WITHIN THIS MODULE. */

static helpers_size_t size; /* Size of vectors */
static int rep;             /* Number of repetitions */
static int trace;           /* Trace last repetition? */
static int no_merge;        /* Don't merge? */
static int extra_flags;     /* Extra flags for scheduling final task */


/* SET OUTPUT VECTOR TO A SEQUENCE GOING FROM 0 TO 1. */

void seq_task
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  double f = 1.0/size;
  for (i = 0; i<size; i++) 
  { o[i] = (double)i * f;
  }
}


/* COMPUTE VECTOR TIME 1.3. */

void mul_task
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  for (i = 0; i<size; i++)
  { o[i] = i1[i] * 1.3;
  }
}


/* COMPUTE VECTOR PLUS 2.1. */

void add_task
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  for (i = 0; i<size; i++)
  { o[i] = i1[i] + 2.1;
  }
}


/* COMPUTE VECTOR TIMES 1.3 PLUS 2.1. */

void mul_add_task
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i;
  for (i = 0; i<size; i++)
  { o[i] = i1[i] * 1.3 + 2.1;
  }
}


/* RETURN THE NAME OF A TASK. */

char *my_task_name (helpers_task_proc *p)
{ 
  return (p)==seq_task     ? "seq" 
       : (p)==mul_task     ? "mul"
       : (p)==add_task     ? "add"
       : (p)==mul_add_task ? "mul_add"
       : "?";
}


/* MASTER PROCEDURE.  Schedules tasks to compute things "rep" times, then 
   checks the results of the last repetition. */

void helpers_master (void)
{
  int r, i;

  printf ("\nUsing %d helpers, vector size %u, repeating %d times\n", 
             helpers_num, size, rep);

  /* Compute results "rep" times, by scheduling tasks. */

  for (r = 1; r<=rep; r++)
  {
    if (trace==1 && r==rep)
    { helpers_trace(1);
    }

    helpers_do_task (no_merge ? 0 : HELPERS_MERGE_OUT,
                     seq_task, 0, A, NULL, NULL);
    helpers_do_task (no_merge ? 0 : HELPERS_MERGE_IN_OUT,
                     mul_task, 0, A, A, NULL);
    helpers_do_task (no_merge ? 0 : extra_flags | HELPERS_MERGE_IN,
                     add_task, 0, A, A, NULL);

    helpers_wait_for_all();
  }

  /* Check first, middle, and last values of final vector computed. */

  for (i = 0; i<=2; i++)
  { int j = i==0 ? 0 : i==1 ? size/2 : size-1;
    if (A[j] != ((double)j/size) * 1.3 + 2.1)
    { fprintf (stderr, "Wrong value for A[%d] (%.15g rather than %.15f)\n", 
                       j, A[j], ((double)j/size) * 1.3 + 2.1);
    }
  }
}


/* MAIN PROGRAM.  Processes command arguments, allocates variables, and 
   calls helpers_startup. */

int main (int argc, char **argv)
{
  char junk;
  int s, n;

  trace = no_merge = extra_flags = 0;

  /* Process arguments. */

  while (argc>1)
  { if (strcmp(argv[1],"-t")==0)
    { trace = 1;
    }
    else if (strcmp(argv[1],"-d")==0)
    { no_merge = 1;
    }
    else if (strcmp(argv[1],"-m")==0)
    { extra_flags |= HELPERS_MASTER_ONLY;
    }
    else if (strcmp(argv[1],"-n")==0)
    { extra_flags |= HELPERS_MASTER_NOW;
    }
    else
    { break;
    }
    argc -= 1;
    argv += 1;
  }

  if (argc!=4
   || sscanf(argv[1],"%d%c",&n,&junk)!=1 || n<0
   || sscanf(argv[2],"%d%c",&s,&junk)!=1 || s<0
   || sscanf(argv[3],"%d%c",&rep,&junk)!=1 || rep<1)
  { fprintf (stderr, 
    "Usage:  merge [ -t ] [ -d ] [ -m ] [ -n ] n-helpers vec-size repetitions\n"
    );
    exit(1);
  }

  size = s;

  /* Allocate space for variables. */

  A = calloc (size, sizeof(double));

  if (A==0)
  { fprintf (stderr, "Not enough memory for vector of size %u\n", size);
    exit(1);
  }

  /* Do all the computations; never returns. */

  helpers_startup(n);
}
