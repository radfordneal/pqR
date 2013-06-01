/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Example Program with Many Options

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

#include <helpers-app.h>


/* EXTERNAL DECLARATIONS. */

double *A, *B, *C, D;            /* Variables used (A,B,C vectors, D scalar) */

int B_in_use, B_being_computed;  /* Markers for variable B */

volatile int _x_x_;              /* For time-wasting loop */


/* DECLARATIONS GLOBAL WITHIN THIS MODULE. */

static int pipeline[4];    /* Whether pipelining should be done for each task */
static int master_only[4]; /* Whether each task should be master-only */
static int master_now[4];  /* Whether each task should be master-now */
static int slow[4];        /* How much to slow down computations */

static helpers_size_t size; /* Size of vectors */
static int rep;            /* Number of repetitions */
static int trace;          /* Trace option:  0=none, 1=last repetition, 2=all */
static int stats;          /* Output statistics at end? */
static int do_direct;      /* Do direct computation after last repetition? */
static int verify;         /* Verify results for last repetition? */
static int get_var_list;   /* Get variable list? */
static int wait_master_only; /* Wait for master-only tasks */
static int wait_unused;    /* Wait for A, B, C to be unused? */
static int wait_computed;  /* Wait for D to be computed? */
static int show_markers;   /* Print markers for variable B last repetition? */
static int C_sum_computation; /* Compute sum of two elements in C at end? */
static int idle_numbers;   /* Print numbers of idle helpers at end */

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


/* COMPUTE SIN OR COS OF VECTOR.  Computes sin when o is 1, cos when o is 2. */

void sin_cos
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i; int j;
  for (i = 0; i<size; i++)
  { o[i] = op==1 ? sin(i1[i]) : cos(i1[i]);
    for (j = 0; j<slow[op]; j++) o[i] = log(exp(o[i]));
  }
}

/* COMPUTE SIN OR COS OF VECTOR - PIPELINED. */

#define ALT 1  /* Set to 1 to switch to alternative pipelined output method */

void sin_cos_piped
  (helpers_op_t op, helpers_var_ptr o, helpers_var_ptr i1, helpers_var_ptr i2)
{ helpers_size_t i = 0;
  HELPERS_SETUP_OUT (6);
  while (i<size)
  { int a1, j;
    HELPERS_WAIT_IN1 (a1, i, size);
#if ALT
    do 
    { helpers_size_t u = HELPERS_UP_TO(i,a1);
      do
      { o[i] = op==1 ? sin(i1[i]) : cos(i1[i]);
        for (j = 0; j<slow[op]; j++) o[i] = log(exp(o[i]));
        i += 1;
      } while (i<=u);
      helpers_amount_out(i);
    } while (i<a1);
#else
    do
    { o[i] = op==1 ? sin(i1[i]) : cos(i1[i]);
      for (j = 0; j<slow[op]; j++) o[i] = log(exp(o[i]));
      HELPERS_NEXT_OUT (i);
    } while (i<a1);
#endif
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
  int r, f;

  printf ("\nUsing %d helpers, vector size %u, repeating %d times\n", 
             helpers_num, size, rep);

  if (trace==2) 
  { helpers_trace(1);
  }

  /* Compute results "rep" times, by scheduling tasks. */

  for (r = 1; r<=rep; r++)
  {
    if (trace==1 && r==rep)
    { helpers_trace(1);
    }

    f = 0;
    if (pipeline[0])    f |= HELPERS_PIPE_OUT;
    if (master_only[0]) f |= HELPERS_MASTER_ONLY;
    if (master_now[0])  f |= HELPERS_MASTER_NOW;
    if (wait_unused) helpers_wait_until_not_in_use(A);
    if (show_markers && r==rep) 
    { printf("Markers for B before step (1):  in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }
    helpers_do_task (f, pipeline[0]?sequence_piped:sequence, 0, A, NULL, NULL);
    if (show_markers && r==rep) 
    { printf("Markers for B after step (1):   in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }

    f = 0;
    if (pipeline[1])    f |= HELPERS_PIPE_IN1_OUT;
    if (master_only[1]) f |= HELPERS_MASTER_ONLY;
    if (master_now[1])  f |= HELPERS_MASTER_NOW;
    if (wait_unused) helpers_wait_until_not_in_use(B);
    if (show_markers && r==rep) 
    { printf("Markers for B before step (2):  in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }
    helpers_do_task (f, pipeline[1] ? sin_cos_piped : sin_cos, 1, B, A, NULL);
    if (show_markers && r==rep) 
    { printf("Markers for B after step (2):   in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }

    f = 0;
    if (pipeline[2])    f |= HELPERS_PIPE_IN1_OUT;
    if (master_only[2]) f |= HELPERS_MASTER_ONLY;
    if (master_now[2])  f |= HELPERS_MASTER_NOW;
    if (wait_unused) helpers_wait_until_not_in_use(C);
    if (show_markers && r==rep) 
    { printf("Markers for B before step (3):  in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }
    helpers_do_task (f, pipeline[2] ? sin_cos_piped : sin_cos, 2, C, A, NULL);
    if (show_markers && r==rep) 
    { printf("Markers for B after step (3):   in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }

    f = 0;
    if (pipeline[3])    f |= HELPERS_PIPE_IN12;
    if (master_only[3]) f |= HELPERS_MASTER_ONLY;
    if (master_now[3])  f |= HELPERS_MASTER_NOW;
    if (wait_unused) helpers_wait_until_not_in_use(&D);
    if (show_markers && r==rep) 
    { printf("Markers for B before step (4):  in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }
    helpers_do_task (f, pipeline[3] ? ave_sqr_piped : ave_sqr, 0, &D, B, C);
    if (show_markers && r==rep) 
    { printf("Markers for B after step (4):   in use %d, being computed %d\n", 
             B_in_use, B_being_computed);
    }

    if (get_var_list && r==rep)
    { helpers_var_ptr *list;
      printf("Variable list:");
      for (list = helpers_var_list(); *list != NULL; list++)
      { printf(" %s",helpers_var_name(*list));
      }
      printf("\n");
    }

    if (C_sum_computation)
    { helpers_size_t a;
      double e1, e2;
      helpers_start_computing_var(C);
      HELPERS_WAIT_IN_VAR (C, a, size/3, size);
      e1 = C[size/3];
      if (a<=size/2) HELPERS_WAIT_IN_VAR (C, a, size/2, size);
      e2 = C[size/2];
      if (r==rep)
      { printf ("Sum of C[%u] and C[%u] is %f\n", size/3, size/2, e1+e2);
      }
    }

    if (idle_numbers && r==rep)
    { int c; 
      for (c = 0; c<20; c++)
      { printf ("Numbers of idle helpers: %d\n",helpers_idle());
        for (_x_x_ = 100000; _x_x_>0; _x_x_--) ;
      }
    }

    if (wait_master_only)
    { helpers_wait_for_all_master_only();
    }

    if (wait_computed)
    { helpers_wait_until_not_being_computed (&D);
    }

    if (!wait_computed && !wait_unused)
    { helpers_wait_for_all();
    }
  }

  helpers_wait_for_all();

  if (show_markers)
  { printf("Markers for B at end:  in use %d, being computed %d\n", 
           B_in_use, B_being_computed);
  }
  
  if (get_var_list && r==rep && *helpers_var_list()!=NULL)
  { printf("Variable list isn't empty at end!\n");
  }

  /* Do direct computation if -D specified. */

  if (do_direct)
  { printf("Computing final values directly\n");
    if (pipeline[0]) sequence_piped(0,A,NULL,NULL);else sequence(0,A,NULL,NULL);
    if (pipeline[1]) sin_cos_piped(1,B,A,NULL);    else sin_cos(1,B,A,NULL);
    if (pipeline[2]) sin_cos_piped(0,C,A,NULL);    else sin_cos(2,C,A,NULL);
    if (pipeline[3]) ave_sqr_piped(0,&D,B,C);      else ave_sqr(0,&D,B,C);
  }

  /* If -v, check results of last repetition (or direct computation, if -D)
     by recomputing here.  Recompute in backwards order, since that makes 
     it more likely we'll find a bug that gets us here before the last task 
     has finished. */

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
  int s, n;

  /* Process options. */

  while (argc>1)
  { if (strcmp(argv[1],"-t")==0)        trace = 1;
    else if (strcmp(argv[1],"-T")==0)   trace = 2;
    else if (strcmp(argv[1],"-s")==0)   stats = 1;
    else if (strcmp(argv[1],"-D")==0)   do_direct = 1;
    else if (strcmp(argv[1],"-v")==0)   verify = 1;
    else if (strcmp(argv[1],"-g")==0)   get_var_list = 1;
    else if (strcmp(argv[1],"-u")==0)   wait_unused = 1;
    else if (strcmp(argv[1],"-c")==0)   wait_computed = 1;
    else if (strcmp(argv[1],"-o")==0)   wait_master_only = 1;
    else if (strcmp(argv[1],"-k")==0)   show_markers = 1;
    else if (strcmp(argv[1],"-C")==0)   C_sum_computation = 1;
    else if (strcmp(argv[1],"-i")==0)   idle_numbers = 1;
    else if (strcmp(argv[1],"-m1")==0)  master_only[0] = 1;
    else if (strcmp(argv[1],"-m2")==0)  master_only[1] = 1;
    else if (strcmp(argv[1],"-m3")==0)  master_only[2] = 1;
    else if (strcmp(argv[1],"-m4")==0)  master_only[3] = 1;
    else if (strcmp(argv[1],"-n1")==0)  master_now[0] = 1;
    else if (strcmp(argv[1],"-n2")==0)  master_now[1] = 1;
    else if (strcmp(argv[1],"-n3")==0)  master_now[2] = 1;
    else if (strcmp(argv[1],"-n4")==0)  master_now[3] = 1;
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
    else if (strcmp(argv[1],"-dh")==0)  helpers_disable(1);
    else if (strcmp(argv[1],"-dp")==0)  helpers_no_pipelining(1);
    else 
    { break;
    }
    argc -= 1;
    argv += 1;
  }

  /* Process main arguments. */

  if (argc!=4
   || sscanf(argv[1],"%d%c",&n,&junk)!=1 || n<0
   || sscanf(argv[2],"%d%c",&s,&junk)!=1 || s<0
   || sscanf(argv[3],"%d%c",&rep,&junk)!=1 || rep<1)
  { fprintf (stderr, 
      "Usage:  example { option } n-helpers vec-size repetitions\n");
    fprintf (stderr, "Options: "
  "-t, -T, -s, -D, -v, -g, -u, -c, -o, -k, -C, -mN, -nM, -lN, -pN, -p, -dh, -dp"
  "\n");
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
