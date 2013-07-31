/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             C Procedures Implementing the Facility

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
#include <string.h>
#include <inttypes.h>

#include "helpers-app.h"

#ifndef HELPERS_DISABLED

#ifndef HELPERS_NO_MULTITHREADING
#include <omp.h>
#endif


/* -----------------------------  OPTIONS  ---------------------------------- */

/* MAXIMUM NUMBER OF TASKS THAT CAN BE OUTSTANDING.  Must be a power of two
   minus one, and no more than 255 (to fit in an unsigned char). */

#ifndef MAX_TASKS    /* Allow value from compile option to override below's */
#define MAX_TASKS 255
#endif

#if MAX_TASKS < 1 || MAX_TASK > 255 || (MAX_TASKS & (MAX_TASKS+1)) != 0
#error Invalid value for MAX_TASKS
#endif


/* OPTION FOR AMOUNT OF TRACE OUTPUT.  Setting this option to 0 will disable
   all trace output, even when it is enabled by helpers_trace.  Normal trace
   output is obtained when the option is set to 1, with only a small overhead
   when trace output has not been enabled by helpers_trace.  Setting this 
   option to 2 or 3 produces more informative trace output, but at the cost of
   increasing overhead (more for 3 than 2) even when tracing is not enabled. */

#ifndef ENABLE_TRACE   /* Allow value from compile option to override below's */
#define ENABLE_TRACE 1 /* 0, 1, 2, or 3 for no, normal, extra... trace output */
#endif


/* OPTION FOR DISABLING STATISTICS.  If ENABLE_STATS is defined as 0, the
   helpers_stats procedure does nothing, which gives a small savings in time
   for some common operations. */

#ifndef ENABLE_STATS   /* Allow value from compile option to override below's */
#define ENABLE_STATS 1
#endif


/* COUNT OF TIMES TO CHECK FOR NON-EMPTY UNTAKEN QUEUE BEFORE SUSPENDING. */

#ifndef SPIN_EMPTY    /* Allow value from compile option to override below's */
#define SPIN_EMPTY 1000

#endif


/* ---------------------------  DECLARATIONS -------------------------------- */

/* DEFAULT DEFINITIONS FOR MACROS.  Defined here if not in helpers-app.h. */

#ifndef helpers_var_name
#define helpers_var_name(v) "?"
#endif

#ifndef helpers_task_name
#define helpers_task_name(t) "?"
#endif

#ifndef helpers_printf
#include <stdio.h>
#define helpers_printf printf
#endif


/* NULL VARIABLE POINTER, AND VARIABLE NAME MACRO HANDLING NULL. */

#define null ((helpers_var_ptr) 0)

#define var_name(v) ((v)==null ? "" : helpers_var_name(v))


/* MACROS FOR OMP ATOMIC DIRECTIVES.  Atomic read and write directives exist
   only in OpenMP 3.1, so with OpenMP 3.0 access to the data types used must be 
   atomic due to the architecture.  See "imp-doc" for discussion.  Use of the
   OpenMP 3.1 atomic read and write directives can be disabled by defining
   ASSUME_ATOMIC_READ_CHAR, ASSUME_ATOMIC_WRITE_CHAR, ASSUME_ATOMIC_READ_SIZE, 
   or ASSUME_ATOMIC_WRITE_SIZE, which may be advisable for performance reasons,
   if the architecture guarantees atomicity in any case. */

#define ATOMIC_READ_CHAR(_stmt_) _stmt_   /* First define macros for use with */
#define ATOMIC_WRITE_CHAR(_stmt_) _stmt_  /*   OpenMP 3.0, or when use of the */
#define ATOMIC_READ_SIZE(_stmt_) _stmt_   /*   atomic directives is disabled, */
#define ATOMIC_WRITE_SIZE(_stmt_) _stmt_  /*   or when no multithreading.     */

#ifndef HELPERS_NO_MULTITHREADING

#if _OPENMP>=201107  /* Redefine macros for OpenMP version 3.1 or later, if use
                        of an atomic directive isn't disabled in the context. */

#ifndef ASSUME_ATOMIC_READ_CHAR
#undef ATOMIC_READ_CHAR
#define ATOMIC_READ_CHAR(_stmt_) do { \
  _Pragma("omp atomic read") \
  _stmt_; \
} while (0)
#endif

#ifndef ASSUME_ATOMIC_WRITE_CHAR
#undef ATOMIC_WRITE_CHAR
#define ATOMIC_WRITE_CHAR(_stmt_) do { \
  _Pragma("omp atomic write") \
  _stmt_; \
} while (0)
#endif

#ifndef ASSUME_ATOMIC_READ_SIZE
#undef ATOMIC_READ_SIZE
#define ATOMIC_READ_SIZE(_stmt_) do { \
  _Pragma("omp atomic read") \
  _stmt_; \
} while (0)
#endif

#ifndef ASSUME_ATOMIC_WRITE_SIZE
#undef ATOMIC_WRITE_SIZE
#define ATOMIC_WRITE_SIZE(_stmt_) do { \
  _Pragma("omp atomic write") \
  _stmt_; \
} while (0)
#endif

#endif
#endif


/* MACRO FOR OMP FLUSH DIRECTIVE.  Can be used followed by a semicolon anywhere
   a statement is syntactically allowed. */

#ifndef HELPERS_NO_MULTITHREADING

#define FLUSH do { \
  _Pragma("omp flush"); \
} while (0)

#else

#define FLUSH do { } while (0)

#endif


/* MACRO FOR OMP WTIME FUNCTION.  Just returns zero if multithreading is
   disabled, so we don't depend on OpenMP being implemented. */

#ifndef HELPERS_NO_MULTITHREADING
#define WTIME() omp_get_wtime()
#else
#define WTIME() 0
#endif


/* NUMBER OF HELPERS.  Declared as extern in helpers.h, defined here.  Helpers 
   are identified by integers from 1 to helpers_num, with 0 sometimes used to 
   identify the master.  The value of helpers_num will not exceed HELPERS_MAX, 
   which will be no more than 127, to allow a helper index to be stored in an 
   signed char.  A helper index of -1 is sometimes used to indicate no helper 
   (nor the master) is assigned. 

   Defined as the constant 0 if HELPERS_NO_MULTITHREADING defined */

#ifndef HELPERS_NO_MULTITHREADING

int helpers_num;          /* Number of helpers */

#endif

typedef signed char hix;  /* Type of a helper index (0 = master, -1 = none) */


/* DECLARATION OF TASK INDEXES.  Task indexes range from 1 to MAX_TASKS.
   An index of 0 either indicates no task, or sometimes a master-now task
   that hasn't really been scheduled.  Task indexes are stored in an unsigned
   char, so MAX_TASKS is limited to be at most 255. */

typedef unsigned char tix;  /* Type of a task index, or of a count of tasks */


/* TABLE OF TASKS.  The "info" fields in an entry in the "task" array are 
   initialized by the master when a task is scheduled.  The "done", "amt_out",
   and "helper" fields may be updated by both the master and the helpers, with 
   careful flushing but without a lock being used, since accesses to them
   are assumed to be atomic.

   Entry zero in the table is used for tasks run in the master that haven't 
   really been scheduled.  The "pipe" field in this entry should be kept zero 
   except when such a task is being done, so that pipelined task procedures 
   called directly by the master thread will work properly.  The "amt_out" 
   field for entry zero may be set, but will never be accessed (except for 
   trace output, so it is set to 0 before a task procedure is started).

   Entries are forced to be 256 bytes in size by the presence of the "space" 
   field (assuming that the "info" field is no more than 256 bytes in size). 
   This makes index arithmetic faster, and may prevent possible performance 
   degradation from cache invalidation when one entry is updated and a 
   different entry is then accessed.  Fields updated by helpers are all put 
   first to increase the chance that they are not split between cache lines
   (since the start of the structure is likely aligned to some extent).
*/

static union task_entry 
{ 
  struct task_info               /* Information on a task */
  { 
    /* Fields initialized by the master, updated by helpers and the master. */

    helpers_size_t amt_out;        /* Number of parts produced for output */
    hix helper;                    /* Helper that took this task, or 0, or -1 */
    char done;                     /* Has this task finished? */

    /* Fields below are written only by the master, but read by helpers too. 
       The "needed" field may be written by the master and read by helpers
       without synchronization. */

    signed char needed;            /* Needed by master? (+1 finish, -1 start) */
    tix pipe[3];                   /* Tasks producing inputs, 0 when done */
    short flags;                   /* Flags task was scheduled with */
    helpers_task_proc *task_to_do; /* Task procedure to execute */
    helpers_op_t op;               /* The unsigned integer operand */
    helpers_var_ptr var[3];        /* The output variable, [0], and the input 
                                      variables, [1] and [2]; any may be null */

    /* The fields below are used only when ENABLE_TRACE is 2 or 3. */

    tix pipe_at_start[3];          /* Value of "pipe" when task is started */
    helpers_size_t first_amt[3];   /* First non-zero value from helpers_availN*/
    helpers_size_t last_amt[3];    /* Last value from helpers_amount */

    /* The fields below are used only when ENABLE_TRACE is 3. */

    double start_wtime;            /* Wall clock time when started */
    double done_wtime;             /* Wall clock time when done */

  } info;

  char space[256];               /* Sets the size of each entry */

} task[MAX_TASKS+1];


/* ARRAY OF TASK ENTRIES CURRENTLY USED AND UNUSED.  Read and written only 
   by the master thread.  The "used" array always contains all the task
   indexes from 1 to MAX_TASKS, in some order.  The first "n_used" of these
   are currently being used, and are in the order they were scheduled.  The
   remaining entries are unused, and in arbitrary order. */

static tix used[MAX_TASKS];  /* All task indexes; first helpers_tasks in use */
int helpers_tasks = 0;       /* Number of tasks outstanding = indexes in use */


/* QUEUES OF TASK INDEXES.  These are circular queues, with in/out pointers
   incremented modulo MAX_TASKS+1, which must be a power of two.  They can 
   never overflow because the number of tasks scheduled but not yet gone is 
   limited to MAX_TASKS.  The index 0, used for tasks done directly in the 
   master, never appears in these queues. */

#define QSize (MAX_TASKS+1)  /* Number of entries in a wrap-around queue  */
#define QMask MAX_TASKS      /* Mask for implementing modulo QSize arithmetic */

/* Queue of master-only tasks.  This queue is accessed only by the master. 
   Tasks in this queue are in the order they were scheduled, which is the
   order in which they must be done. */

static tix master_only[QSize], master_only_in, master_only_out;

/* Queue of tasks not yet taken by a helper (or master), in arbitrary order.  
   The untaken_in pointer is modified only by the master; the untaken_out 
   pointer is modified by the master or a helper.  Accesses by helpers
   and the master that check for the queue being empty, or make it non-empty,
   are done with "untaken_lock" set.  */

static tix untaken[QSize], untaken_in, untaken_out;

#ifndef HELPERS_NO_MULTITHREADING

static omp_lock_t untaken_lock;   /* Lock to set for accessing untaken queue */

#endif


/* LOCK SET FOR STARTING A TASK.  Set by a helper or the master when looking
   for a task to start.  When there's nothing to do, other helpers will block
   trying to set this lock. */

#ifndef HELPERS_NO_MULTITHREADING

static omp_lock_t start_lock;  /* Lock set by thread looking for task to start*/

#endif


/* PAIR OF LOCKS USED FOR SUSPENDING A HELPER. */

#ifndef HELPERS_NO_MULTITHREADING

static hix suspended;      /* Helper that has suspended, or 0 if none */

static omp_lock_t suspend_lock[2];/* Locks used alternately to suspend helper */

static int which_suspends; /* Which lock a helper sets to suspend itself */
static int which_wakes;    /* Which lock the master unsets to wake helper */

static int suspend_initialized;  /* Set to 1 when master has done initial set */

#endif


/* VARIABLE LIST.  Storage for an array of variables used by current tasks. 
   Each task uses at most three variables (out, in1, and in2).  The array
   is null-terminated. */

static helpers_var_ptr var_list[3*MAX_TASKS+1] = { (helpers_var_ptr) 0 };


/* VARIABLES PRIVATE TO EACH HELPER AND MASTER. */

static int this_thread;   /* What thread we are: 0 = master, other = helper # */

static tix this_task;     /* The task this thread is doing, undefined if none,
                             except 0 in the master when it's not doing a task
                             so directly-called pipelined task procedures work*/

static struct task_info *this_task_info;  /* Pointer to info for this_task */

#ifndef HELPERS_NO_MULTITHREADING

#pragma omp threadprivate (this_thread, this_task, this_task_info)

#endif


/* VARIABLES HOLDING DISABLING OPTIONS. */

int helpers_are_disabled = 0;    /* 1 if helpers currently disabled */

#ifndef HELPERS_NO_MULTITHREADING
int helpers_not_pipelining;      /* 1 if pipelining currently disabled */
int helpers_not_multithreading;  /* 1 if currently all tasks done by master */
#endif


/* TRACE VARIABLES. */

#if ENABLE_TRACE>0
static char trace = 0;    /* Are we printing trace information now? */
#else
#define trace 0           /* Tracing never enabled */
#endif

static double init_wtime; /* Wall clock time when helpers_startup called, set
                             and used only if ENABLE_TRACE is 3 */


/* STATISTICS ON HELPERS AND MASTER.  The tasks_done field in the zeroth entry 
   pertains to the master.  These statistics are updated and read only in the 
   master thread.  Declared but not used if ENABLE_STATS is zero. */

static struct stats
{ int tasks_done;         /* Number of tasks done by this helper/master */
  int times_woken;        /* Number of times this helper was woken */
} stats[HELPERS_MAX+1];


/* FORWARD DECLARATIONS OF STATIC PROCEDURES. */

static void do_task_in_master (int);
static int runnable (tix);


/* -------------------------  TRACE PROCEDURES  ----------------------------- */

/* PRINT LIST OF CURRENT TASKS.  Prints the task indexes, followed by "*" if 
   the task is flagged as needing to complete, "+" if the task is flagged
   as needing to start, and "." if the task is not needed, and then by "F"
   if the task has finished, "X" if the task is executing, "R" if the task is 
   not executing but is runnable, and nothing otherwise. */

static void trace_task_list (void)
{ int i;
  for (i = 0; i<helpers_tasks; i++) 
  { struct task_info *info = &task[used[i]].info;
    helpers_printf(" %d%s%s", used[i], 
      info->needed>0 ? "*" : info->needed<0 ? "+" : ".",
      info->done ? "F" : info->helper>=0 ? "X" : runnable(used[i]) ? "R" : "");
  }
}


/* TRACE OUTPUT FOR STARTING A TASK.  The task index is the first argument,
   with 0 indicating that the task will be done in the master without giving
   it a task index.  The remaining arguments are from helpers_do_task. */

static void trace_started 
  (tix t, int flags, helpers_task_proc *task_to_do, helpers_op_t op, 
   helpers_var_ptr out, helpers_var_ptr in1, helpers_var_ptr in2)
{ 
  if (t==0)
  { helpers_printf (
      "HELPERS: Task done directly in master: %s(%"PRIuMAX",%s,%s,%s)", 
      helpers_task_name(task_to_do), (uintmax_t) op, 
      var_name(out), var_name(in1), var_name(in2));
  }
  else
  { helpers_printf (
      "HELPERS: Task %d scheduled: %s(%"PRIuMAX",%s,%s,%s)", t,
      helpers_task_name(task_to_do), 
      (uintmax_t) op, 
      var_name(out), var_name(in1), var_name(in2));
  }

  if (flags & HELPERS_MASTER_ONLY) helpers_printf(" MASTER_ONLY");
  if (flags & HELPERS_MASTER_NOW)  helpers_printf(" MASTER_NOW");
  if (flags & HELPERS_PIPE_OUT)    helpers_printf(" PIPE_OUT");
  if (flags & HELPERS_PIPE_IN0)    helpers_printf(" PIPE_IN0");
  if (flags & HELPERS_PIPE_IN1)    helpers_printf(" PIPE_IN1");
  if (flags & HELPERS_PIPE_IN2)    helpers_printf(" PIPE_IN2");

  if (ENABLE_TRACE>1)
  { struct task_info *info = &task[t].info;
    helpers_printf (" : %d %d %d", info->pipe[0], info->pipe[1], info->pipe[2]);
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }

  helpers_printf("\n");

  if (ENABLE_TRACE>1)
  { helpers_printf("HELPERS: Current tasks:");
    trace_task_list();
    helpers_printf("\n");
  }
}


/* TRACE OUTPUT FOR COMPLETION OF A TASK. */

static void trace_completed (tix t)
{
  struct task_info *info = &task[t].info;

  if (t==0)
  { helpers_printf ("HELPERS: Task done directly completed");
  }
  else if (info->helper==0)
  { helpers_printf ("HELPERS: Task %d completed in master  ", t);
  }
  else
  { helpers_printf ("HELPERS: Task %d completed in helper %d", t, info->helper);
  }

  if (ENABLE_TRACE>1)
  { helpers_printf (" : %d %d %d : %"PRIuMAX" %"PRIuMAX" %"PRIuMAX
                    " : %"PRIuMAX" %"PRIuMAX" %"PRIuMAX" : %"PRIuMAX, 
      info->pipe_at_start[0], info->pipe_at_start[1], info->pipe_at_start[2],
      (uintmax_t) info->first_amt[0], 
      (uintmax_t) info->first_amt[1], 
      (uintmax_t) info->first_amt[2],
      (uintmax_t) info->last_amt[0], 
      (uintmax_t) info->last_amt[1], 
      (uintmax_t) info->last_amt[2],
      (uintmax_t) info->amt_out);
    if (ENABLE_TRACE>2)
    { if (t==0)
      { helpers_printf (" @ %.3f>%.3f", info->start_wtime - init_wtime,
                                        info->done_wtime - init_wtime);
      }
      else
      { helpers_printf (" @ %.3f>%.3f>%.3f", info->start_wtime - init_wtime,
                                             info->done_wtime - init_wtime,
                                             WTIME() - init_wtime);
      }
    }
  }

  helpers_printf("\n");
}


/* TRACE OUTPUT FOR STARTING COMPUTATION OF A VARIABLE. */

static void trace_start_computing_var (int f, helpers_var_ptr v)
{
  switch (f)
  { case 0: 
      helpers_printf ("HELPERS: Computation of %s has already finished\n",
                       var_name(v));
      break;
    case 1: 
      helpers_printf ("HELPERS: Computation of %s has already started\n",
                       var_name(v));
      break;
    case 2: 
      helpers_printf ("HELPERS: Computation of %s is starting in a helper\n",
                       var_name(v));
      break;
    case 3: 
      helpers_printf ("HELPERS: Computation of %s is being started\n",
                       var_name(v));
      break;
  }
}


/* TRACE OUTPUT FOR WAITING FOR COMPUTATIONS TO FINISH. */

static void trace_wait_until_not_being_computed
  (int any, helpers_var_ptr v1, helpers_var_ptr v2)
{ 
  helpers_printf (any ? "HELPERS: Wait until " : "HELPERS: No wait until ");

  if (v1==null && v2==null)
  { helpers_printf ("no vars not being computed");
  }
  else if (v1!=null && v2!=null)
  { helpers_printf ("%s and %s not being computed", var_name(v1), var_name(v2));
  }
  else
  { helpers_printf ("%s not being computed", var_name (v1==null ? v2 : v1));
  }

  if (ENABLE_TRACE>1)
  { helpers_printf(" :");
    trace_task_list();
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }

  helpers_printf("\n");
}


/* TRACE OUTPUT FOR WAITING FOR VARIABLE TO NOT BE IN USE. */

static void trace_wait_until_not_in_use (int any, helpers_var_ptr v)
{ 
  if (!any)
  { helpers_printf ("HELPERS: No wait until %s not in use\n", var_name(v));
    return;
  }

  helpers_printf ("HELPERS: Waiting until %s not in use", var_name(v));

  if (ENABLE_TRACE>1)
  { helpers_printf(" :");
    trace_task_list();
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }

  helpers_printf("\n");
}


/* TRACE OUTPUT FOR WAITING FOR ALL MASTER-ONLY TASKS TO COMPLETE. */

static void trace_wait_for_all_master_only (void)
{ 
  if (master_only_in==master_only_out)
  { helpers_printf ("HELPERS: No wait for all master-only tasks to complete\n");
    return;
  }

  helpers_printf ("HELPERS: Waiting for all master-only tasks to complete");

  if (ENABLE_TRACE>1)
  { helpers_printf(" :");
    trace_task_list();
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }

  helpers_printf("\n");
}


/* TRACE OUTPUT FOR WAITING FOR ALL TASKS TO COMPLETE. */

static void trace_wait_for_all (void)
{ 
  if (helpers_tasks==0)
  { helpers_printf ("HELPERS: No wait for all tasks to complete\n");
    return;
  }

  helpers_printf ("HELPERS: Waiting for all tasks to complete");

  if (ENABLE_TRACE>1)
  { helpers_printf(" :");
    trace_task_list();
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }

  helpers_printf("\n");
}


/* TRACE OUTPUT FOR NO LONGER WAITING. */

static void trace_done_waiting (void)
{
  helpers_printf ("HELPERS: Done waiting");
  if (ENABLE_TRACE>1)
  { helpers_printf(" :");
    trace_task_list();
    if (ENABLE_TRACE>2)
    { helpers_printf (" @ %.3f", WTIME()-init_wtime);
    }
  }
  helpers_printf("\n");
}


/* --------------------------  UTILITY PROCEDURES  -------------------------- */

/* RUN THE TASK JUST TAKEN.  When called, the new task should be in this_task,
   with its info in this_task_info.  A flush operation (explicit or implicit)
   should be done before calling this procedure. */

static void run_this_task (void)
{
  if (ENABLE_TRACE>1) 
  { this_task_info->pipe_at_start[0] = this_task_info->pipe[0];
    this_task_info->pipe_at_start[1] = this_task_info->pipe[1];
    this_task_info->pipe_at_start[2] = this_task_info->pipe[2];
    this_task_info->first_amt[0] = 0;
    this_task_info->first_amt[1] = 0;
    this_task_info->first_amt[2] = 0;
    if (ENABLE_TRACE>2)
    { this_task_info->start_wtime = WTIME();
    }
  }

  this_task_info->task_to_do (this_task_info->op, this_task_info->var[0], 
                              this_task_info->var[1], this_task_info->var[2]);

  if (ENABLE_TRACE>2) 
  { this_task_info->done_wtime = WTIME();
  }

  FLUSH;  /* ensure data and info is up-to-date when 'done' flag is set */

  ATOMIC_WRITE_CHAR (this_task_info->done = 1);
  FLUSH;
}


/* CHECK WHETHER A TASK IS RUNNABLE.  A task cannot be run if any of its
   input/output variables are being computed by another task that hasn't
   finished, and the other task has either not started, or it doesn't 
   produce pipelined output, or this task doesn't handle pipelined input. 
   This function returns 0 if the task is not runnable, -1 if it is runnable
   with pipelining, and +1 if it is runnable without pipelining. 

   Careful flushing is needed here to handle the possibility that a task
   producing one of the inputs finishes around this time and a new task 
   with the same task id quickly starts up. */

static int runnable (tix t)
{
  struct task_info *info = &task[t].info;
  int f, r;
  char d;
  tix p;
  hix h;

  r = +1;

  FLUSH;  /* make sure we see recent values of 'pipe', 'done', and 'helper' */

  /* Look at availability of input from the 'out' varable. */

  ATOMIC_READ_CHAR (p = info->pipe[0]);
  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (p!=0 && !d)
  { ATOMIC_READ_CHAR (h = task[p].info.helper);
    f = task[p].info.flags;
    FLUSH;
    ATOMIC_READ_CHAR (p = info->pipe[0]);
    if (p!=0) /* could be 0 if task finished since check above */
    { if (h<0 || ! (f & HELPERS_PIPE_OUT) || ! (info->flags & HELPERS_PIPE_IN0))
      { return 0;
      }
      r = -1;
    }
  }

  /* Look at availability of input from the 'in1' varable. */

  ATOMIC_READ_CHAR (p = info->pipe[1]);
  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (p!=0 && !d)
  { ATOMIC_READ_CHAR (h = task[p].info.helper);
    f = task[p].info.flags;
    FLUSH;
    ATOMIC_READ_CHAR (p = info->pipe[1]);
    if (p!=0) /* could be 0 if task finished since check above */
    { if (h<0 || ! (f & HELPERS_PIPE_OUT) || ! (info->flags & HELPERS_PIPE_IN1))
      { return 0;
      }
      r = -1;
    }
  }

  /* Look at availability of input from the 'in2' varable. */

  ATOMIC_READ_CHAR (p = info->pipe[2]);
  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (p!=0 && !d)
  { ATOMIC_READ_CHAR (h = task[p].info.helper);
    f = task[p].info.flags;
    FLUSH;
    ATOMIC_READ_CHAR (p = info->pipe[2]);
    if (p!=0) /* could be 0 if task finished since check above */
    { if (h<0 || ! (f & HELPERS_PIPE_OUT) || ! (info->flags & HELPERS_PIPE_IN2))
      { return 0;
      }
      r = -1;
    }
  }

  return r;
}


/* FIND AN UNTAKEN TASK THAT IS RUNNABLE, AND REMOVE IT FROM QUEUE.  If its
   argument is zero, it returns 0 if no untaken task is runnable (master-only 
   tasks not considered), and otherwise the index of a runnable task from the 
   untaken queue, which it removes from this queue (moving untaken_out).  If 
   more than one untaken task is runnable, a task whose start or completion is 
   needed now by the master is preferred, and secondarily, a task whose inputs 
   are fully computed is preferred to one that would use one or more pipelined 
   inputs that are not fully computed, with any other tie broken arbitrarily.

   If the argument is 1, only a "needed" task will be found, with 0 being
   returned if no needed task is runnable.

   This procedure should be called only when start_lock has been set, so that
   another thread will not be manipulating the queue simultaneously.  When
   this procedure is called from a helper, however, it is possible that the
   master will simultaneously be adding an entry at the untaken_in end. 
   The value of untaken_in is therefore fetched once, followed by a flush
   to ensure that data accessed in the queue before that is up-to-date. */

static tix find_untaken_runnable (int only_needed)
{
  int i, f, p, r, n;
  tix u_in, t;

  /* We assume a flush has been done recently (explicitly, or implicity from
     setting start_lock), so the value seen for untaken_in is recent.  We flush
     after to ensure that the data in the queue up to u_in is up-to-date. */

  ATOMIC_READ_CHAR (u_in = untaken_in);
  FLUSH;

  /* Look for a runnable task in the untaken queue, setting f to its index
     in this queue. */

  f = -1;  /* nothing runnable found yet */

  p = only_needed ? 1 : -1;  /* if p is 1, ignore non-needed tasks
                                if p is 0, ignore non-needed pipelined tasks
                                if p is -1, any runnable task might be used */

  for (i = untaken_out; i!=u_in; i = (i+1)&QMask) 
  { t = untaken[i];
    ATOMIC_READ_CHAR (n = task[t].info.needed);
    if (n)
    { r = runnable(t);
      if (r!=0)
      { f = i;
        if (r>0)    /* needed now and runnable without pipelining, so we can */
        { break;    /*   stop the search now, since this is highest priority */
	}
        else        /* needed now and runnable with pipelining */
        { p = 1;
        }
      }
    }
    else if (p<=0)
    { r = runnable(t);
      if (r>0)
      { f = i;      /* not needed now and runnable without pipelining */
        p = 0;
      }
      else if (r<0 && p<0)
      { f = i;      /* not needed now and runnable with pipelining */
      }
    }
  }

  /* Return 0 if no runnable task found, or none needed, if only_needed is 1. */

  if (f < 0) return 0;

  /* Otherwise, take the highest priority runnable task. */

  t = untaken[f];

  /* Remove the task taken from the untaken queue.  The index at untaken_out
     is copied to where the task being removed is located, at which point
     untaken_out can be moved forward. */

  untaken[f] = untaken[untaken_out];
  ATOMIC_WRITE_CHAR (untaken_out = (untaken_out + 1) & QMask);
  FLUSH;

  return t;
}


/* NOTICE COMPLETED TASKS.  Called only from the master thread.  Note that it
   starts and ends with FLUSH operations (except when there are no tasks). */

static void notice_completed (void)
{
  helpers_var_ptr v;
  int i, j, k, w;
  char d;

  /* Quick exit when no tasks. */

  if (helpers_tasks==0)
  { return;
  }

  /* Flush so that 'done' flags will be visible. */

  FLUSH;

  /* Find first task that has completed, if there is any. */

  i = 0;
  for (;;)
  { ATOMIC_READ_CHAR (d = task[used[i]].info.done);
    if (d) 
    { break;
    }
    i += 1;
    if (i==helpers_tasks)
    { return;
    }
  }

  /* Loop through tasks in 'used', processing completion of any now done. */

  k = i;
  for ( ; i<helpers_tasks; i++)
  { 
    tix t = used[i];
    struct task_info *info = &task[t].info;

    ATOMIC_READ_CHAR (d = info->done);

    if (!d) /* Swap so tasks not finished end up all at the bottom */
    { 
      used[i] = used[k];
      used[k] = t;
      k += 1;
    }
    else /* Process completion of a task that has now finished. */
    {
      /* Update 'pipe' fields for tasks that were taking input from this one. */

      for (j = i+1; j<helpers_tasks; j++)
      { struct task_info *ninfo = &task[used[j]].info;
        for (w = 0; w<=2; w++)
        { if (ninfo->pipe[w]==t) 
          { if (ENABLE_TRACE>1)
            { ATOMIC_READ_SIZE (ninfo->last_amt[w] = info->amt_out);
            }
            ATOMIC_WRITE_CHAR (ninfo->pipe[w] = 0);
          }
        }
      }
  
      /* Increment count of tasks done by helper/master that did this task. */
  
      if (ENABLE_STATS) stats[info->helper].tasks_done += 1;
  
      /* Write trace output showing task completion, if trace enabled. */
  
      if (trace) trace_completed(t);
  
      /* Unset the in-use and being-computed flags as appropriate, if the 
         application defined the required macros.  This requires scanning
         all other current tasks to see if some other task is still using
         or computing the variable. */
  
#ifdef helpers_mark_not_being_computed
      v = info->var[0];
      if (v!=null)
      { for (j = 0; j<helpers_tasks; j++)
        { struct task_info *einfo = &task[used[j]].info;
          if (einfo->var[0]==v) 
          { ATOMIC_READ_CHAR (d = einfo->done);
            if (!d) 
            { goto done_c;
            }
          }
        }
        helpers_mark_not_being_computed(v);
      }
    done_c: ;
#endif
  
#ifdef helpers_mark_not_in_use
      for (w = 1; w<=2; w++)
      { v = info->var[w];
        if (v!=null && v!=info->var[0])
        { for (j = 0; j<helpers_tasks; j++)
          { struct task_info *einfo = &task[used[j]].info;
            if (einfo->var[0]!=v && (einfo->var[1]==v || einfo->var[2]==v))
            { ATOMIC_READ_CHAR (d = einfo->done);
              if (!d) 
              { goto done_u;
              }
            }
          }
          helpers_mark_not_in_use(v);
        }
      done_u: ;
      }
#endif
    }
  }

  /* Update number of tasks. */

  helpers_tasks = k;

  /* Flush so updated 'pipe' fields will be visible to helpers, and so that
     data written by tasks that have completed will be visible to master. */

  FLUSH;

  /* Output new task list, if that's enabled. */

  if (ENABLE_TRACE>1 && trace)
  { helpers_printf("HELPERS: Current tasks:");
    trace_task_list();
    helpers_printf("\n");
  }
}


/* MARK AS NEEDED A TASK AND THE TASKS IT TAKES INPUT FROM.  The 'needed' 
   argument should be -1 (needs to start) or +1 (needs to finish).  Tasks 
   should be marked from most recent to oldest, so that a task marked with 
   -1 because it provides input will not have previously been marked with +1.  
   It's enough to mark a task that provides input as needing to start, since 
   if pipelining can't be done, it will anyway have to finish before the task 
   taking that input can start. */

static void mark_as_needed (struct task_info *info, int needed)
{
  int w;

  if (info->needed <= 0) 
  { ATOMIC_WRITE_CHAR (info->needed = needed);
  }

  for (w = 0; w<=2; w++)  
  { int p = info->pipe[w];
    if (p != 0) ATOMIC_WRITE_CHAR (task[p].info.needed = -1);
  }
}


/* WAIT WHILE ANY TASKS NEED TO START OR FINISH.  Looks at the "needed" flags
   for tasks, where -1 means needs to start, +1 means needs to finish, and 0
   means not needed.  While it waits, it tries to do needed tasks in the master,
   and notices completed tasks.  

   Note that if a task needs to finish, a FLUSH will have been done (in 
   notice_completed) after that task finishes, but if a task needs only to
   start no FLUSH may have been done after it starts when wait_while_any_needed
   returns.  However, the lack of a FLUSH in this circumstance should be 
   innocuous, since data will be accessed only after amt_out is looked at 
   after a later FLUSH. */

static void wait_while_any_needed (void)
{
  loop:
  { int i; hix h;

    do_task_in_master(1);
    notice_completed();  /* does a FLUSH */

    for (i = 0; i<helpers_tasks; i++)
    { struct task_info *info = &task[used[i]].info;
      if (info->needed != 0)
      { if (info->needed > 0) goto loop;  /* needs to finish, but hasn't */
        ATOMIC_READ_CHAR (h = info->helper);
        if (h < 0) goto loop;  /* needs to start, but hasn't */
      }
    }
  }
}


/* -----------------  TASK EXECUTION IN THE MASTER OR A HELPER -------------- */

/* MAYBE DO A NEEDED TASK IN THE MASTER.  Does the next master-only task if it
   is runnable (and is needed, if only_needed is 1), and otherwise tries to do 
   a task from the untaken queue (provided it is needed, if only_needed is 1).
   Returns without waiting if no task is sutiable, or if it can't set 
   start_lock (so a helper must currently be looking for a task to run). */

static void do_task_in_master (int only_needed)
{
  tix u_out;

  this_task = 0;

  /* Do the next master-only task if there is one (and perhaps needed), and it
     is runnable.  If so, remove the task to be done from the master_only queue.
     Note that this doesn't require any queue locking, since the master_only
     queue is accessed only in the master thread. */

  if (master_only_in!=master_only_out)
  { 
    tix m = master_only[master_only_out];

    if ((!only_needed || task[m].info.needed) && runnable(m))
    { master_only_out = (master_only_out + 1) & QMask;
      this_task = m;
    }
  }

  /* Otherwise, look for a (perhaps needed) task from the untaken queue, but 
     only if no helper is trying to (and hence no other thread has set 
     start_lock).  Return if no needed and runnable task can be found this 
     way either. */

  if (this_task==0)
  { 
    hix h;
 
#   ifndef HELPERS_NO_MULTITHREADING

    if (!helpers_not_multithreading)
    {
      ATOMIC_READ_CHAR (h = suspended);

      FLUSH;  /* ensures that h>0 below after seeing untaken_in!=untaken_out
                 does really mean the queue is non-empty (but shouldn't be) */
    }

#   endif

    ATOMIC_READ_CHAR (u_out = untaken_out);
    if (untaken_in==u_out)
    { return; 
    }

#   ifndef HELPERS_NO_MULTITHREADING

    if (!helpers_not_multithreading)
    { 
      if (!omp_test_lock (&start_lock))
      { 
        /* See if a helper is supended - it shouldn't be! - and wake it up
           if it is. */

        if (h>0)
        { omp_set_lock (&suspend_lock[!which_wakes]);
          suspended = 0;
          omp_unset_lock (&suspend_lock[which_wakes]);
          which_wakes = !which_wakes;
          if (ENABLE_STATS) stats[h].times_woken += 1;
          helpers_printf("HELPER WAS SUSPENDED WHEN IT SHOULDN'T HAVE BEEN!\n");
        }

        return;
      }
    }

#   endif

    this_task = find_untaken_runnable(only_needed);

#   ifndef HELPERS_NO_MULTITHREADING

    if (!helpers_not_multithreading)
    { omp_unset_lock (&start_lock);
    }

#   endif

    if (this_task==0)
    { return;
    }
  }

  /* Do the task in the master. */

  this_task_info = &task[this_task].info;
  ATOMIC_WRITE_CHAR (this_task_info->helper = 0);

  FLUSH;
  run_this_task();

  /* Set this_task to indicate that nothing is being done in the master,
     so that directly-called task procedures will work correctly. */

  this_task = 0;
  this_task_info = &task[this_task].info;
}


/* PROCEDURE EXECUTED IN HELPER THREADS.  Loops looking for something for 
   this helper to do. */

#ifndef HELPERS_NO_MULTITHREADING

static void helper_proc (void)
{
  /* Set lock for becoming the helper that looks for a task to start. */

  omp_set_lock (&start_lock);

  /* Loop, each time waiting for a non-empty untaken queue, looking for a 
     runnable task to start, doing it, and flagging its completion. */

  for (;;)
  {
    int count;
    tix u_in;

    /* Loop up to SPIN_EMPTY times, or until there is an untaken task
       (and multithreading hasn't been disabled). */

    for (count = 0; count<SPIN_EMPTY; count++)
    { ATOMIC_READ_CHAR (u_in = untaken_in);
      if (u_in!=untaken_out && !helpers_not_multithreading)
      { break;
      }
      FLUSH;
    }

    if (count==SPIN_EMPTY)
    {
      int will_suspend = 0;

      /* Decide whether to suspend by looking whether the queue is empty
         with the lock set, so the master won't be putting a task in at 
         the same time. */

      omp_set_lock (&untaken_lock);

      ATOMIC_READ_CHAR (u_in = untaken_in);
      if (u_in==untaken_out || helpers_not_multithreading) 
      { ATOMIC_WRITE_CHAR (suspended = this_thread);
        will_suspend = 1;
      }

      omp_unset_lock (&untaken_lock);

      /* If we decided to suspend, do that now. */

      if (will_suspend) /* look at will_suspend, not suspended (could change) */
      {
        /* Suspend by setting the lock that will have been set by the master 
           (though if it has already noticed the suspended flag, and decided 
           to unsuspend, it may have already unset it). 

           The master will unsuspend this suspended helper when it next puts
           a task in the untaken queue (when multithreading hasn't been 
           disabled). */

        omp_set_lock (&suspend_lock[which_suspends]);
        omp_unset_lock (&suspend_lock[which_suspends]);
        which_suspends = !which_suspends;

        /* Go back to the start of the main loop, looking again at whether
           the queue is empty. */

        continue;
      }
    }

    /* Wait until there is a task to do.  Note that start_lock will have 
       been set, and the untaken queue must contain at least one task, which
       must eventually become runnable. */

    do { this_task = find_untaken_runnable(0); } while (this_task==0);

    /* Do the task in this helper, unsetting start_lock first, then setting
       it again after. */

    this_task_info = &task[this_task].info;
    ATOMIC_WRITE_CHAR (this_task_info->helper = this_thread);

    omp_unset_lock (&start_lock);  /* implies a flush */
    run_this_task();
    omp_set_lock (&start_lock);  /* implies a flush */
  }
}

#endif


/* -------------------------  TASK SCHEDULING  ------------------------------ */

/* DO A TASK OR SCHEDULE IT TO BE DONE. */

void helpers_do_task 
  (int flags, helpers_task_proc *task_to_do, helpers_op_t op, 
   helpers_var_ptr out, helpers_var_ptr in1, helpers_var_ptr in2)
{
  struct task_info *info;
  int i;
  tix t;
  hix h;

  /* If helpers are disabled, do the task directly.  There's no possible need 
     to wait.  Note that task[0].info will be set to all zeros (either from 
     initialization or clearing when helpers were disabled). */

  if (helpers_are_disabled)
  { info = &task[0].info;
    goto direct;
  }

  /* Notice tasks that have now completed.  Note that this does a flush
     (unless there are no tasks). */

  notice_completed();

  /* Set up for task - either master-now or another kind. */

  if (flags & HELPERS_MASTER_NOW)
  { 
    /* Use the pseudo-entry 0 for this master-now task, which will be done
       directly in the master.  Don't bother setting task_to_do, op, out, 
       in1, and in2 in info, since they are unused.  Note that pipe0, pipe1, 
       pipe2, last_amt0, last_amt1, last_amt2, and helper should already be 
       zero. */

    t = 0;
    info = &task[0].info;

    /* Wait for other tasks as required, if there are any other tasks. */

    if (helpers_tasks > 0) 
    {
      /* Mark tasks that are master-only or that compute variables needed by 
         the master-now task as needing to be started or completed.  Also mark 
         as needed those tasks needed to do those tasks, etc.*/
  
      for (i = helpers_tasks-1; i>=0; i--)
      {
        struct task_info *uinfo = &task[used[i]].info;
        int needed = uinfo->needed;

        if (uinfo->flags & HELPERS_MASTER_ONLY)
        { needed = +1;
        }
        else if (needed <= 0)
        { helpers_var_ptr v0 = uinfo->var[0];
          if (v0 != null)
          { if (uinfo->flags & HELPERS_PIPE_OUT)
            { if (v0==out) 
              { needed = flags & HELPERS_PIPE_IN0 ? -1 : +1;
              }
              if (v0==in1 && needed<=0)
              { needed = flags & HELPERS_PIPE_IN1 ? -1 : +1;
              }
              if (v0==in2 && needed<=0)
              { needed = flags & HELPERS_PIPE_IN2 ? -1 : +1;
              }
            }
            else
            { if (v0==out || v0==in1 || v0==in2) 
              { needed = +1;
              }
            }
          }
        }

        if (needed != 0) mark_as_needed (uinfo, needed);       
      }

      /* Wait for the tasks marked as needed above to start or complete. */
  
      wait_while_any_needed();
    }
  }

  else /* not master-now */
  { 
    /* Wait for a free task entry.  If there are no free entries, loop until 
       an entry is free, while doing master-only tasks that are runnable now,
       or any other tasks in the master if no runnable master-only tasks. */

    while (helpers_tasks==MAX_TASKS)
    { do_task_in_master(0);
      notice_completed();
    }

    /* Store info about the new task in a new task entry (t and info).  But 
       don't move helpers_tasks yet, since we don't want to see the new task. */

    t = used[helpers_tasks];
    info = &task[t].info;
    info->task_to_do = task_to_do;
    info->op = op;
    info->var[0] = out;
    info->var[1] = in1;
    info->var[2] = in2;

    info->pipe[0] = info->pipe[1] = info->pipe[2] = 0;
    if (ENABLE_TRACE>1)
    { info->last_amt[0] = info->last_amt[1] = info->last_amt[2] = 0;
    }

    info->flags = helpers_not_pipelining ? flags&~HELPERS_PIPE_OUT : flags;

    info->helper = -1; /* nobody is doing the task yet */
    info->needed = 0;  /* master isn't currently waiting for task to finish */

    /* Set the in-use and being-computed flags as appropriate, if the 
       application defined the required macros.  (Note that we don't do
       this if the task is done directly in the master, since the flags
       could never be consulted until unset anyway.) */

#ifdef helpers_mark_in_use
    if (in1!=null && in1!=out) helpers_mark_in_use(in1);
    if (in2!=null && in2!=out) helpers_mark_in_use(in2);
#endif

#ifdef helpers_mark_being_computed
    if (out!=null) helpers_mark_being_computed(out);
#endif

    /* Clear 'done' and 'amt_out' in the task info for the new task.  Not
       necessary in a task done directly in the master (since never seen). */

    info->done = 0;
    info->amt_out = 0;
  }

  /* For each input or output variable in the new task, find the previous task
     (if any) outputting that variable.  When more than one task has the same 
     output variable, the one scheduled most recently takes precedence. */

  info->pipe[0] = info->pipe[1] = info->pipe[2] = 0;

  for (i = 0; i<helpers_tasks; i++)
  { tix u = used[i];
    helpers_var_ptr v = task[used[i]].info.var[0];
    if (v!=null)
    { if (v==out) info->pipe[0] = u;
      if (v==in1) info->pipe[1] = u;
      if (v==in2) info->pipe[2] = u;
    }
  }

  /* Do a master-now task directly. */

  if (t==0) 
  {
    goto direct;
  }

  /* Mark the new task entry as being in use. */

  helpers_tasks += 1;

  /* Write trace output showing task scheduled, if trace enabled. */

  if (trace) trace_started (t, flags, task_to_do, op, out, in1, in2);

  /* If this is a master-only task, just put it in the master_only queue. */

  if (flags & HELPERS_MASTER_ONLY)
  { master_only[master_only_in] = t;
    master_only_in = (master_only_in + 1) & QMask;
    return;
  }

  /* For a non-master-only task, put it in the untaken queue, where it may 
     then be noticed by a helper looking for a task to start (or eventually
     be done by the master thread). Set the lock when incrementing untaken_in,
     and find out if a helper has suspended while the lock is set.  (But
     don't unsuspend a helper if multithreading is currently disabled.) */

  untaken[untaken_in] = t;

# ifndef HELPERS_NO_MULTITHREADING

  if (!helpers_not_multithreading)
  { omp_set_lock (&untaken_lock);    /* does an implicit FLUSH */
    h = suspended;
  }

# endif

  ATOMIC_WRITE_CHAR (untaken_in = (untaken_in+1) & QMask);

# ifndef HELPERS_NO_MULTITHREADING

  if (!helpers_not_multithreading)
  { 
    omp_unset_lock (&untaken_lock);  /* does an implicit FLUSH */

    /* Wake the suspended helper, if there is one. */

    if (h!=0)
    { omp_set_lock (&suspend_lock[!which_wakes]);
      suspended = 0;
      omp_unset_lock (&suspend_lock[which_wakes]);
      which_wakes = !which_wakes;
      if (ENABLE_STATS) stats[h].times_woken += 1;
    }
  }

# endif

  return;

direct:

  /* Do this task in the master without scheduling it. */

  if (trace) trace_started (0, flags, task_to_do, op, out, in1, in2);

  /* Code below is like in run_this_task, except this procedure's arguments
     are used without their being stored in the task info structure, and
     there's no need to set the 'done' flag. */

  if (ENABLE_TRACE>1) 
  { this_task_info->pipe_at_start[0] = this_task_info->pipe[0];
    this_task_info->pipe_at_start[1] = this_task_info->pipe[1];
    this_task_info->pipe_at_start[2] = this_task_info->pipe[2];
    this_task_info->first_amt[0] = 0;
    this_task_info->first_amt[1] = 0;
    this_task_info->first_amt[2] = 0;
    if (ENABLE_TRACE>2)
    { this_task_info->start_wtime = WTIME();
    }
  }

  task_to_do (op, out, in1, in2);

  if (ENABLE_TRACE>2) 
  { this_task_info->done_wtime = WTIME();
  }

  if (trace) trace_completed (0);

  /* Update stats on tasks done in master. */

  if (ENABLE_STATS) stats[0].tasks_done += 1;

  /* Set "pipe" and "last_amt" fields to zero, so that later direct
     calls of task procedures and master-now tasks will work properly. */

  info->amt_out = 0;
  info->pipe[0] = info->pipe[1] = info->pipe[2] = 0;
  if (ENABLE_TRACE>1)
  { info->last_amt[0] = info->last_amt[1] = info->last_amt[2] = 0;
  }
}


/* ----------  MAJOR PROCEDURES CALLED FROM APPLICATION PROGRAMS ------------ */

/* START COMPUTATION OF A VARIABLE.  Checks whether the task computing the
   variable passed has started in a helper.  If so, it returns immediately.
   Otherwise, it sees which tasks are needed for computation to start.  If
   a master-only task isn't needed, it waits as long as there seems to be an 
   idle helper (so helpers can do the work).  It does the task itself in the 
   master thread if there seem to be no idle helpers, or a master-only task
   needs to be done. */

void helpers_start_computing_var (helpers_var_ptr v)
{
  struct task_info *vinfo;
  int master_only_needed;
  int vindex;
  char d;
  hix h;
  int i;

  /* Quick exit if no processes scheduled (includes when no helper threads). */

  if (helpers_tasks == 0)
  { if (trace) trace_start_computing_var(0,v);
    return;
  }

  /* Flush so that 'done' and 'helper' fields will be up-to-date. */

  FLUSH;

  /* Search for the task outputting this variable.  Return if none, or task
     doing computation has finished. */

  vindex = helpers_tasks;
  for (;;)
  { vindex -= 1;
    vinfo = &task[used[vindex]].info;
    if (vinfo->var[0] == v) 
    { ATOMIC_READ_CHAR (d = vinfo->done);
      if (d)
      { return;
      }
      else
      { break;
      }
    }
    if (vindex == 0) 
    { if (trace) trace_start_computing_var(0,v);
      return;
    }
  }

  /* See if we need to wait at all. */

  ATOMIC_READ_CHAR (h = vinfo->helper);
  if (h > 0) 
  { if (trace) trace_start_computing_var(1,v);
    return;
  }

  /* Mark the task that computes v as needing to start, along with any task 
     that computes a variable used by the task that computes v, or that is 
     needed to finish a needed task scheduled later, or that is a master-only 
     task that needs to run before a later master-only task that is needed. */

  mark_as_needed (vinfo, -1);
  master_only_needed = vinfo->flags & HELPERS_MASTER_ONLY;

  for (i = vindex-1; i>=0; i--)
  { 
    struct task_info *uinfo = &task[used[i]].info;
    int needed = uinfo->needed;

    if (master_only_needed && (uinfo->flags & HELPERS_MASTER_ONLY))
    { needed = -1;
    }

    if (needed != 0) 
    { mark_as_needed (uinfo, needed);
      if (uinfo->flags & HELPERS_MASTER_ONLY) master_only_needed = 1;
    }
  }

  /* If computing v does not require running a master-only task, loop until
     the computation of v has started (in which case return), or it looks 
     like no helpers are idle.  It is possible that it will look like no 
     helpers are idle when actually one is (since a helper might quickly
     finish a task and start on a new one), but it should not be possible for 
     the loop to continue for very long when there are no idle helpers. */

  if (!master_only_needed)
  {
    while (helpers_idle() > 0)  /* Note: helpers_idle calls notice_completed */
    {
      ATOMIC_READ_CHAR (h = vinfo->helper);
      if (h > 0) 
      { if (trace) trace_start_computing_var(2,v);
        return;
      }
    }
  }

  /* Wait until all the needed tasks have at least started (but needn't have
     finished), perhaps doing some of them in the master thread. */

  if (trace) trace_start_computing_var(3,v);

  wait_while_any_needed();
}


/* WAIT FOR TASKS USING A VARIABLE.  Waits until all tasks have completed that
   have v as an input (not including as output too).  These tasks, and the
   tasks that have to start before they can complete, are marked as needed
   so that they will preferentially be done by helpers or the master. */

void helpers_wait_until_not_in_use (helpers_var_ptr v)
{
  int any_needed, master_only_needed, i;

  /* Quick check for there being no uncompleted tasks. */

  notice_completed();

  if (helpers_tasks==0) 
  { if (trace) trace_wait_until_not_in_use(0,v);
    return;
  }

  /* Mark a task as needing to complete if it uses the variable passed as
     an input (but not as its output), or is a master-only task that needs 
     to run before some later master-only task that is needed.  Mark a task 
     as needing to start if it is needed by some needed task scheduled later. */

  any_needed = 0;
  master_only_needed = 0;

  for (i = helpers_tasks-1; i>=0; i--)
  { 
    struct task_info *uinfo = &task[used[i]].info;
    int needed = uinfo->needed;

    if (master_only_needed && (uinfo->flags & HELPERS_MASTER_ONLY)
         || (uinfo->var[1]==v || uinfo->var[2]==v) && uinfo->var[0]!=v)
    { needed = +1;
    }

    if (needed != 0) 
    { mark_as_needed (uinfo, needed);
      if (uinfo->flags & HELPERS_MASTER_ONLY) master_only_needed = 1;
      any_needed = 1;
    }
  }

  /* Quick check for no "needed" tasks. */

  if (!any_needed)
  { if (trace) trace_wait_until_not_in_use(0,v);
    return;
  }

  /* Wait while doing tasks in the master and noticing completed tasks. */

  if (trace) trace_wait_until_not_in_use(1,v);

  wait_while_any_needed();

  if (trace) trace_done_waiting();
}


/* WAIT FOR TASKS THAT ARE COMPUTING VARIABLES.  Waits until all tasks have
   completed that have v1 or v2 as an output.  These tasks, and the tasks that 
   have to start before they can complete, are marked as needed so that they 
   will preferentially be done (by helpers or the master). */

void helpers_wait_until_not_being_computed2
  (helpers_var_ptr v1, helpers_var_ptr v2)
{
  int any_needed, master_only_needed, i;

  /* Quick check for there being no uncompleted tasks. */

  notice_completed();

  if (helpers_tasks==0)
  { if (trace) trace_wait_until_not_being_computed (0,v1,v2);
    return;
  }

  /* Mark a task as needing to complete if it computes one of the variables 
     passed as arguments, or is a master-only task that needs to run before 
     some later master-only task that is needed.  Mark a task as needing to 
     start if it is needed by some needed task scheduled later. */

  any_needed = 0;
  master_only_needed = 0;

  for (i = helpers_tasks-1; i>=0; i--)
  { 
    struct task_info *uinfo = &task[used[i]].info;
    int needed = uinfo->needed;

    if (master_only_needed && (uinfo->flags & HELPERS_MASTER_ONLY)
         || uinfo->var[0]!=null && (v1==uinfo->var[0] || v2==uinfo->var[0]))
    { needed = +1;
    }

    if (needed != 0) 
    { mark_as_needed (uinfo, needed);
      if (uinfo->flags & HELPERS_MASTER_ONLY) master_only_needed = 1;
      any_needed = 1;
    }
  }

  /* Quick check for no "needed" tasks. */

  if (!any_needed)
  { if (trace) trace_wait_until_not_being_computed (0,v1,v2);
    return;
  }

  /* Wait while doing tasks in the master and noticing completed tasks. */

  if (trace) trace_wait_until_not_being_computed (1,v1,v2);

  wait_while_any_needed();

  if (trace) trace_done_waiting();
}


/* WAIT FOR ALL SCHEDULED MASTER-ONLY TASKS TO COMPLETE.  Also has to wait
   for tasks that need to start before some master-only task can be done. */

void helpers_wait_for_all_master_only (void)
{
  int i;

  if (trace) trace_wait_for_all_master_only();

  /* Return if there are no uncompleted master-only tasks. */

  if (master_only_in==master_only_out)
  { return;
  }

  /* Mark a task as needing to start if it is master-only, or is needed to 
     do a needed task scheduled later. */

  for (i = helpers_tasks-1; i>=0; i--)
  { 
    struct task_info *uinfo = &task[used[i]].info;
    int needed = uinfo->needed;

    if (uinfo->flags & HELPERS_MASTER_ONLY)
    { needed = +1;
    }

    if (needed != 0) 
    { mark_as_needed (uinfo, needed);
    }
  }

  /* Wait while doing tasks in the master and noticing completed tasks. */

  wait_while_any_needed();

  if (trace) trace_done_waiting();
}


/* WAIT FOR ALL SCHEDULED TASKS TO COMPLETE. */

void helpers_wait_for_all (void)
{
  int i;

  if (trace) trace_wait_for_all();

  /* Quick check for no uncompleted tasks. */

  if (helpers_tasks==0) 
  { return;
  }

  /* Mark all tasks as needed. */

  for (i = 0; i<helpers_tasks; i++)
  { ATOMIC_WRITE_CHAR (task[used[i]].info.needed = +1);
  }

  /* Wait while doing tasks in the master and noticing completed tasks. */

  wait_while_any_needed();

  if (trace) trace_done_waiting();
}


/* -----------------------  PIPELINING PROCEDURES  -------------------------- */

/* SAY HOW MUCH OF THE OUTPUT HAS BEEN PRODUCED SO FAR.  Changes the 
   amt_out field for this task - without synchronization, on the assumption 
   that reading and writing are atomic operations.  A flush is done before
   to ensure that the new data is there before the updated value for amt_out
   indicates that it is there.  No flush is done after the update - at worst,
   the new value for amt_out will be flushed on the next call, or when the
   task finished. */

#ifndef HELPERS_NO_MULTITHREADING

void helpers_amount_out (helpers_size_t amt)
{ 
  if (this_task_info->flags & HELPERS_PIPE_OUT)
  { FLUSH;
    ATOMIC_WRITE_SIZE (this_task_info->amt_out = amt);
  }
}

#endif


/* GET THE AMOUNT OF AN INPUT THAT HAS BEEN PRODUCED SO FAR.  Care is needed to
   handle the possibility that the task producing the input will terminate
   around this time, and another task with the same task index will immediately
   start.  To handle this, the amt_out field for the task producing the
   input is read before a flush, and termination of the input task is checked
   after the flush.  If that check says the input task hadn't terminated at
   the later time, the earlier read before the flush must have obtained amt_out
   for that input task, not some later task.  If the input task has terminated,
   the maximum passed is returned.  No flush is done at the beginning - at 
   worst, fresh values will be obtained on the next call. */

#ifndef HELPERS_NO_MULTITHREADING

helpers_size_t helpers_avail0 (helpers_size_t mx)
{
  helpers_size_t n;
  char d;
  tix p;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[0]);
  if (p == 0) return mx;

  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (d) return mx;

  ATOMIC_READ_SIZE (n = task[p].info.amt_out);

  FLUSH;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[0]);
  if (p==0) return mx;

  if (ENABLE_TRACE>1)
  { if (this_task_info->first_amt[0]==0) this_task_info->first_amt[0] = n;
  }

  return n;
}

helpers_size_t helpers_avail1 (helpers_size_t mx)
{
  helpers_size_t n;
  char d;
  tix p;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[1]);
  if (p == 0) return mx;

  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (d) return mx;

  ATOMIC_READ_SIZE (n = task[p].info.amt_out);

  FLUSH;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[1]);
  if (p==0) return mx;

  if (ENABLE_TRACE>1)
  { if (this_task_info->first_amt[1]==0) this_task_info->first_amt[1] = n;
  }

  return n;
}

helpers_size_t helpers_avail2 (helpers_size_t mx)
{
  helpers_size_t n;
  char d;
  tix p;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[2]);
  if (p == 0) return mx;

  ATOMIC_READ_CHAR (d = task[p].info.done);
  if (d) return mx;

  ATOMIC_READ_SIZE (n = task[p].info.amt_out);

  FLUSH;

  ATOMIC_READ_CHAR (p = this_task_info->pipe[2]);
  if (p==0) return mx;

  if (ENABLE_TRACE>1)
  { if (this_task_info->first_amt[2]==0) this_task_info->first_amt[2] = n;
  }

  return n;
}

#endif


/* GET THE AMOUNT OF A VARIABLE THAT HAS BEEN PRODUCED SO FAR.  Searches for the
   most recently scheduled task that has that variable as its output, and 
   returns its amt_out value.  Returns the maximum passed if no task is 
   outputting the variable.  Note that there is no need to worry about
   the task with some id changing, since this is called only by the master. */

#ifndef HELPERS_NO_MULTITHREADING

helpers_size_t helpers_avail_var (helpers_var_ptr v, helpers_size_t mx)
{
  helpers_size_t n;
  char d;
  int i;

  for (i = helpers_tasks-1; i>=0; i--)
  { struct task_info *info = &task[used[i]].info;
    if (info->var[0] == v)
    { FLUSH;
      ATOMIC_READ_CHAR (d = info->done);
      if (d) return mx;
      ATOMIC_READ_SIZE (n = info->amt_out);
      return n;
    }
  }

  return mx;
}

#endif


/* -------------  MISCELLANEOUS PROCEDURES USED BY APPLICATIONS  ------------ */


/* RETURN AN ESTIMATE OF THE NUMBER OF IDLE HELPERS.  Note that it calls
   notice_completed to start (unless helpers are disabled). */

#ifndef HELPERS_NO_MULTITHREADING

int helpers_idle (void)
{
  int i, c;
  hix h;

  if (helpers_are_disabled) return 0;

  notice_completed();

  c = helpers_num;

  for (i = 0; i<helpers_tasks; i++)
  { ATOMIC_READ_CHAR (h = task[used[i]].info.helper);
    if (h>0) c -= 1;
    if (c<=0) return 0;
  }

  return c;
}

#endif


/* RETURN A LIST OF ALL VARIABLES USED BY UNCOMPLETED TASKS. */

helpers_var_ptr *helpers_var_list (void)
{
  int i, j;

  notice_completed();

  for (i = 0, j = 0; i<helpers_tasks; i++)
  { struct task_info *info = &task[used[i]].info;
    if (info->var[0]!=null) var_list[j++] = info->var[0];
    if (info->var[1]!=null) var_list[j++] = info->var[1];
    if (info->var[2]!=null) var_list[j++] = info->var[2];
  }

  var_list[j] = (helpers_var_ptr) 0;

  return var_list;
}


/* DISABLE / RE-ENABLE PIPELINING. */

#ifndef HELPERS_NO_MULTITHREADING

void helpers_no_pipelining (int a)
{
  helpers_not_pipelining = a!=0 || helpers_num==0;
}

#endif


/* DISABLE / RE-ENABLE HELPERS.  Before disabling, wait for all tasks to 
   complete, and clear task[0].info. */

void helpers_disable (int a)
{
  if (a && !helpers_are_disabled)
  { helpers_wait_for_all();
    memset (&task[0].info, 0, sizeof task[0].info);
  }
  helpers_are_disabled = a!=0;
  if (trace) 
  { helpers_printf ("HELPERS: Helpers facility %s\n",
                    helpers_are_disabled ? "disabled" : "enabled");
  }
}


/* DISABLE / RE-ENABLE USE OF HELPER THREADS.  Before setting, wait for
   all tasks to complete. */

#ifndef HELPERS_NO_MULTITHREADING

void helpers_no_multithreading (int a)
{
  helpers_wait_for_all();
  helpers_not_multithreading = a!=0;
  FLUSH;
  if (trace) 
  { helpers_printf ("HELPERS: Multithreading %s\n",
                    helpers_not_multithreading ? "disabled" : "enabled");
  }
}

#endif


/* CHANGE WHETHER OR NOT TRACE OUTPUT IS PRODUCED. */

void helpers_trace (int f)
{
# if ENABLE_TRACE!=0
    trace = f;
# endif
}


/* PRINT STATISTICS. */

void helpers_stats (void)
{
  int tot_done, tot_woken;
  struct stats *h;
  int j;

  if (!ENABLE_STATS) return;

  if (this_thread!=0) return;

  helpers_printf("\nHELPERS STATISTICS\n\n");

  if (helpers_num==0)
  { helpers_printf ("          Tasks done\n\n");
  }
  else
  { helpers_printf ("          Tasks done  Times woken\n\n");
  }

  tot_done = 0;
  tot_woken = 0;

  for (j = 1; j<=helpers_num; j++)
  { h = &stats[j];
    helpers_printf ("helper %-2d %8d    %9d\n",
                    j, h->tasks_done, h->times_woken);
    tot_done += h->tasks_done;
    tot_woken += h->times_woken;
  }

  if (helpers_num>0)
  { helpers_printf ("master    %8d\n\n", stats[0].tasks_done);
  }
  tot_done += stats[0].tasks_done;

  helpers_printf ("totals    %8d    %9d\n", tot_done, tot_woken);
}


/* ------------------------  STARTUP PROCEDURE  ----------------------------- */

/* START HELPER THREADS AND RUN MASTER PROCEDURE. */

void helpers_startup (int n)
{
  tix i;

  /* Record initial wall clock time, if ENABLE_TRACE is 3. */

  if (ENABLE_TRACE>2)
  { init_wtime = WTIME();
  }

  /* Initialize for there being no tasks yet. */

  for (i = 0; i<MAX_TASKS; i++) used[i] = i+1;
  helpers_tasks = 0;

  master_only_in = master_only_out = 0;
  untaken_in = untaken_out = 0;

  this_task = 0;
  this_task_info = &task[this_task].info;

  /* Set the number of helpers desired, always zero if no multithreading. */

# ifndef HELPERS_NO_MULTITHREADING
    helpers_num = n<0 ? 0 : n>HELPERS_MAX ? HELPERS_MAX : n;
# endif

  /* If no helpers are wanted, just call the master procedure, then exit. */

  if (helpers_num==0) 
  { 
#   ifndef HELPERS_NO_MULTITHREADING
      helpers_not_multithreading = 1;
      helpers_not_pipelining = 1;
#   endif

    helpers_master();
    exit(0);
  }

  /* Otherwise, set up for using helper threads. */

# ifndef HELPERS_NO_MULTITHREADING

  helpers_not_multithreading = 0;
  helpers_not_pipelining = 0;

  /* Initialize all locks. */

  omp_init_lock (&untaken_lock);
  omp_init_lock (&start_lock);
  omp_init_lock (&suspend_lock[0]);
  omp_init_lock (&suspend_lock[1]);
  
  which_suspends = which_wakes = 0;
  suspend_initialized = 0;

  #pragma omp parallel num_threads(n+1)
  {
    this_thread = omp_get_thread_num();

    if (this_thread==0)
    {
      /* CODE EXECUTED BY THE MASTER THREAD. */

      /* Find out how many helpers we really have. */

      helpers_num = omp_get_num_threads() - 1;

      /* Set suspend_lock[0] so helpers will be able to suspend themselves. */

      omp_set_lock (&suspend_lock[0]);

      suspend_initialized = 1;
      FLUSH;

      /* Run the user-supplied master procedure. */

      helpers_master();

      exit(0);
    }
    else
    {
      /* CODE EXECUTED BY THE HELPER THREADS. */
  
      /* Wait for master to set suspend lock. */
  
      do {
        FLUSH;
      } while (!suspend_initialized);
  
      /* Run the procedure done in each helper. */
  
      helper_proc();
    }
  }

# endif

}

#endif
