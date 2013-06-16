/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Example Program with Parallelization of Single Computations

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

int B_in_use, B_being_computed;  /* Markers for variable B */


/* DECLARATIONS GLOBAL WITHIN THIS MODULE. */

static int pipeline[4];    /* Whether pipelining should be done for each task */
static int slow[4];        /* How much to slow down computations */

static helpers_size_t size; /* Size of vectors */
static helpers_size_t chunk;/* Size of interleaved chunks */
static int par;            /* Number of parallel tasks in sin/cos computation */
static int rep;            /* Number of repetitions */
static int trace;          /* Trace option:  0=none, 1=last repetition, 2=all */
static int stats;          /* Output statistics at end? */
static int verify;         /* Verify results for last repetition? */

static volatile double a, b, c, d; /* For checking results; making them volatile
                                      globals avoids excess optimization */


/* SET OUTPUT VECTOR TO EXPONENTIAL OF A SEQUENCE GOING FROM 0 TO 1. */

void sequence
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i; int j;
  for (i = 0; i<size; i++) 
  { o[i] = exp ((double)i / size);
    for (j = 0; j<slow[0]; j++) o[i] = log(exp(o[i]));
  }
}

/* SET OUTPUT VECTOR TO SEQUENCE - PIPELINED OUTPUT. */

void sequence_piped
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i = 0;
  HELPERS_SETUP_OUT (6);
  while (i<size)
  { int j;
    o[i] = exp ((double)i / size);
    for (j = 0; j<slow[0]; j++) o[i] = log(exp(o[i]));
    HELPERS_NEXT_OUT (i);
  }
}


/* COMPUTE SIN OR COS OF VECTOR.  Computes sin when o&3 is 1, cos when it is 2.
   Computation is split into stages, numbered 0, 1, ..., par-1, with stage par-1
   doing chunk 0, stage par-2 doing chunk 1, ..., stage 0 doing chunk par-1,
   stage par-1 doing chunk par, stage par-2 doing chunk par+1, ... */

void sin_cos
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ 
  int oper = op&3;      /* 1 = sin, 2 = cos */
  int stage = op>>2;    /* stage of computation handled here (0 to par-1) */
  int skip = chunk*par; /* total elements done or skipped in one iteration */

  helpers_size_t a0 = 0;
  helpers_size_t i, k, e;
  int j;

  /* Loop doing one chunk each time around (skipping chunks not done here). */

  for (i = chunk*(par-1-stage); i<size; i += skip)
  { 
    /* Set e to index past the end of next chunk to do here. */

    e = i + chunk;
    if (e>size) e = size;

    /* Skip input-output data to end of chunk done here. */

    if (stage>0 && a0<e)
    { HELPERS_WAIT_IN0 (a0, e-1, size);
    }

    /* Inform next stage that data is available to start of chunk done here. */

    if (stage<par-1) 
    { helpers_amount_out(i);
    }

    /* Do the next chunk that is to be done by this stage. */

    for (k = i; k<e; k++)
    { o[k] = oper==1 ? sin(i1[k]) : cos(i1[k]);
      for (j = 0; j<slow[oper]; j++) o[k] = log(exp(o[k]));
    }
  }

  /* Wait for earlier stage to finish. */

  if (stage>0)
  { HELPERS_WAIT_IN0 (a0, size-1, size);
  }
}

/* COMPUTE SIN OR COS OF VECTOR - WITH PIPELINED INPUT AND FINAL OUTPUT. */

void sin_cos_piped
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{
  int oper = op&3;      /* 1 = sin, 2 = cos */
  int stage = op>>2;    /* stage of computation handled here (0 to par-1) */
  int skip = chunk*par; /* total elements done or skipped in one iteration */

  helpers_size_t a0 = 0;
  helpers_size_t a1 = 0;
  helpers_size_t i, k, e;
  int j;

  /* Set up for pipelined output. */

  HELPERS_SETUP_OUT (6);

  /* Loop doing one chunk each time around (skipping chunks not done here). */

  for (i = chunk*(par-1-stage); i<size; i += skip)
  { 
    /* Set e to index past the end of next chunk to do here. */

    e = i + chunk;
    if (e>size) e = size;

    /* Skip input-output data to end of chunk done here.  As we go, pass on 
       how much is available (up to i) to the next stage, since this may allow
       the final output to be pipelined sooner. */

    if (stage>0)
    { while (a0<i)
      { HELPERS_WAIT_IN0 (a0, a0, size);
        helpers_amount_out (a0>i ? i : a0);
      }
      if (a0<e) 
      { HELPERS_WAIT_IN0 (a0, e-1, size);
      }
    }

    /* Do the next chunk that is to be done by this stage. */

    k = i;
    while (k < e)
    { if (a1<=k) HELPERS_WAIT_IN1 (a1, k, size);
      do
      { o[k] = oper==1 ? sin(i1[k]) : cos(i1[k]);
        for (j = 0; j<slow[oper]; j++) o[k] = log(exp(o[k]));
        HELPERS_NEXT_OUT(k);
      } while (k < a1 && k < e);
    }
  }

  /* Wait for earlier stage to finish. */

  if (stage>0)
  { HELPERS_WAIT_IN0 (a0, size-1, size);
  }
}


/* AVERAGE SUM OF SQUARES OF VECTOR ELEMENTS. */

void ave_sqr
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i; int j;
  double sum = 0;
  for (i = 0; i<size; i++) 
  { double t = i1[i]*i1[i] + i2[i]*i2[i];
    for (j = 0; j<slow[3]; j++) t = log(exp(t));
    sum += t;
  }
  *o = sum/size;
}

/* AVERAGE SUM OF SQUARES OF VECTOR ELEMENTS - WITH PIPELINED INPUT.*/

void ave_sqr_piped
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i = 0;
  double sum = 0;
  while (i<size)
  { helpers_size_t a1, a2;
    int j;
    HELPERS_WAIT_IN1 (a1, i, size);
    HELPERS_WAIT_IN2 (a2, i, size);
    do
    { double t = i1[i]*i1[i] + i2[i]*i2[i];
      for (j = 0; j<slow[3]; j++) t = log(exp(t));
      sum += t;
      i += 1;
    } while (i<a1 && i<a2);
  }
  *o = sum/size;
}


/* RETURN THE NAME OF A TASK. */

char *my_task_name (helpers_task_proc *p)
{ 
  return (p)==sequence || (p)==sequence_piped ? "sequence" 
       : (p)==sin_cos  || (p)==sin_cos_piped  ? "sin_cos"
       : (p)==ave_sqr  || (p)==ave_sqr_piped  ? "ave_sqr"
       : "?";
}


/* MASTER PROCEDURE.  Schedules tasks to compute things "rep" times, then 
   checks the results of the last repetition, and looks at how much the 
   final results differ from one. */

void helpers_master (void)
{
  int p, r;

  printf (
   "\nUsing %d helpers, vector size %u, chunk size %u, par %d, repeating %d times\n", 
   helpers_num, size, chunk, par, rep);

  if (trace==2) 
  { helpers_trace(1);
  }

  /* Compute results "rep" times, by scheduling tasks. */

  for (r = 1; r<=rep; r++)
  {
    if (trace==1 && r==rep)
    { helpers_trace(1);
    }

    if (pipeline[0]) 
    { helpers_do_task (HELPERS_PIPE_OUT, sequence_piped, 0, A, NULL, NULL);
    }
    else
    { helpers_do_task (0, sequence, 0, A, NULL, NULL);
    }

    if (pipeline[1]) 
    { helpers_do_task (HELPERS_PIPE_IN1_OUT,
                       sin_cos_piped, 1+0, B, A, NULL);
      for (p = 1; p<par; p++)
      { helpers_do_task (HELPERS_PIPE_IN01_OUT,
                         sin_cos_piped, 1+(p<<2), B, A, NULL);
      }
    }
    else
    { helpers_do_task (par==1 ? 0 : HELPERS_PIPE_OUT,
                       sin_cos, 1+0, B, A, NULL);
      for (p = 1; p<par; p++)
      { helpers_do_task (p==par-1 ? HELPERS_PIPE_IN0 : HELPERS_PIPE_IN0_OUT,
                         sin_cos, 1+(p<<2), B, A, NULL);
      }
    }

    if (pipeline[2]) 
    { helpers_do_task (HELPERS_PIPE_IN1_OUT,
                       sin_cos_piped, 2+0, C, A, NULL);
      for (p = 1; p<par; p++)
      { helpers_do_task (HELPERS_PIPE_IN01_OUT,
                         sin_cos_piped, 2+(p<<2), C, A, NULL);
      }
    }
    else
    { helpers_do_task (par==1 ? 0 : HELPERS_PIPE_OUT,
                       sin_cos, 2+0, C, A, NULL);
      for (p = 1; p<par; p++)
      { helpers_do_task (p==par-1 ? HELPERS_PIPE_IN0 : HELPERS_PIPE_IN0_OUT,
                         sin_cos, 2+(p<<2), C, A, NULL);
      }
    }

    if (pipeline[3]) 
    { helpers_do_task (HELPERS_PIPE_IN12, ave_sqr_piped, 0, &D, B, C);
    }
    else
    { helpers_do_task (0, ave_sqr, 0, &D, B, C);
    }

    helpers_wait_for_all();
  }

  /* If -v, check results of last repetition by recomputing here.  Recompute 
     in backwards order, since that makes it more likely we'll find a bug that 
     gets us here before the last task has finished. */

  if (verify)
  { helpers_size_t i;
    double sum = 0;
    i = size;
    while (i > 0)
    { double t;
      int j;
      i -= 1;
      a = exp ((double)i / size);
      for (j = 0; j<slow[0]; j++) a = log(exp(a));
      b = sin(a);
      for (j = 0; j<slow[1]; j++) b = log(exp(b));
      c = cos(a);
      for (j = 0; j<slow[2]; j++) c = log(exp(c));
      if (A[i] != a || B[i] != b || C[i] != c)
      { printf ("Error in value at index %u: %f %f, %f %f, %f %f\n", i,
                 A[i], a, B[i], b, C[i], c);
        break;
      }
      t = b*b + c*c;
      for (j = 0; j<slow[3]; j++) t = log(exp(t));
      sum += t;
    }
    d = sum/size;
    if (D != d)
    { printf ("Error in final value: %f %f\n", D, d);
    }
  }

  /* Print final value computed. */

  printf ("\nFinal value of D: %.8f\n", D);

  /* Print statistics, if -s used. */

  if (stats) helpers_stats();
}


/* MAIN PROGRAM. Processes command arguments, allocates variables, and 
   calls helpers_startup. */

int main (int argc, char **argv)
{
  char junk;
  int s, c, p, n;

  /* Process options. */

  while (argc>1)
  { if (strcmp(argv[1],"-t")==0)        trace = 1;
    else if (strcmp(argv[1],"-T")==0)   trace = 2;
    else if (strcmp(argv[1],"-s")==0)   stats = 1;
    else if (strcmp(argv[1],"-v")==0)   verify = 1;
    else if (strcmp(argv[1],"-l1")==0)  slow[0] += 1;
    else if (strcmp(argv[1],"-l2")==0)  slow[1] += 1;
    else if (strcmp(argv[1],"-l3")==0)  slow[2] += 1;
    else if (strcmp(argv[1],"-l4")==0)  slow[3] += 1;
    else if (strcmp(argv[1],"-p1")==0)  pipeline[0] = 1;
    else if (strcmp(argv[1],"-p2")==0)  pipeline[1] = 1;
    else if (strcmp(argv[1],"-p3")==0)  pipeline[2] = 1;
    else if (strcmp(argv[1],"-p4")==0)  pipeline[3] = 1;
    else if (strcmp(argv[1],"-p")==0)
    { pipeline[0] = pipeline[1] = pipeline[2] = pipeline[3] = 1;
    }
    else 
    { break;
    }
    argc -= 1;
    argv += 1;
  }

  /* Process main arguments. */

  if (argc!=6
   || sscanf(argv[1],"%d%c",&n,&junk)!=1 || n<0
   || sscanf(argv[2],"%d%c",&s,&junk)!=1 || s<0
   || sscanf(argv[3],"%d%c",&c,&junk)!=1 || c<1
   || sscanf(argv[4],"%d%c",&p,&junk)!=1 || p<1
   || sscanf(argv[5],"%d%c",&rep,&junk)!=1 || rep<1)
  { fprintf (stderr, 
    "Usage:  parex { option } n-helpers vec-size chunk-size par repetitions\n");
    fprintf (stderr, "Options: -t, -T, -s, -lN, -pN, -p\n");
    exit(1);
  }

  size = s;
  chunk = c;
  par = p;

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
