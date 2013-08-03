/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Interface to C Procedures Called by Application Programs

   Copyright (c) 2013 Radford M. Neal.

   The helpers library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2, as 
   published by the Free Software Foundation.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/


/* This header file must be included from a helpers-app.h file, which must
   define types helpers_var_ptr, helpers_op_t, and helpers_len_t. */


/* TYPE OF A PROCEDURE FOR PERFORMING A TASK.  Arguments are an operand of
   type helpers_op_t, a pointer to the result variable, and two pointers
   to operand variables.  Any of the pointers may be null. */

typedef void helpers_task_proc \
  (helpers_op_t, helpers_var_ptr, helpers_var_ptr, helpers_var_ptr);


/* MAXIMUM NUMBER OF HELPER THREADS.  Must be no more than 127 (to fit in
   a signed char). */

#define HELPERS_MAX 127

#if HELPERS_MAX < 0 || HELPERS_MAX > 127
#error Invalid value for HELPERS_MAX
#endif


/* FLAGS FOR TASK SCHEDULING. */

#define HELPERS_MASTER_ONLY 1
#define HELPERS_MASTER_NOW  2

#define HELPERS_PIPE_OUT    4
#define HELPERS_PIPE_IN0    8
#define HELPERS_PIPE_IN1   16
#define HELPERS_PIPE_IN2   32

#define HELPERS_PIPE_IN01 (HELPERS_PIPE_IN0 + HELPERS_PIPE_IN1)
#define HELPERS_PIPE_IN02 (HELPERS_PIPE_IN0 + HELPERS_PIPE_IN2)
#define HELPERS_PIPE_IN12 (HELPERS_PIPE_IN1 + HELPERS_PIPE_IN2)

#define HELPERS_PIPE_IN012 (HELPERS_PIPE_IN01 + HELPERS_PIPE_IN2)

#define HELPERS_PIPE_IN0_OUT (HELPERS_PIPE_IN0 + HELPERS_PIPE_OUT)
#define HELPERS_PIPE_IN1_OUT (HELPERS_PIPE_IN1 + HELPERS_PIPE_OUT)
#define HELPERS_PIPE_IN2_OUT (HELPERS_PIPE_IN2 + HELPERS_PIPE_OUT)

#define HELPERS_PIPE_IN01_OUT (HELPERS_PIPE_IN01 + HELPERS_PIPE_OUT)
#define HELPERS_PIPE_IN02_OUT (HELPERS_PIPE_IN02 + HELPERS_PIPE_OUT)
#define HELPERS_PIPE_IN12_OUT (HELPERS_PIPE_IN12 + HELPERS_PIPE_OUT)

#define HELPERS_PIPE_IN012_OUT (HELPERS_PIPE_IN012 + HELPERS_PIPE_OUT)

#define HELPERS_MERGE_IN   64
#define HELPERS_MERGE_OUT 128

#define HELPERS_MERGE_IN_OUT (HELPERS_MERGE_IN + HELPERS_MERGE_OUT)


/* MASTER PROCEDURE DEFINED BY THE USER. */

void helpers_master (void);          /* The master procedure, does it all */


/* PROCEDURES/MACROS/VARIABLE DEFINED BY THE HELPERS MODULE.  Some of these 
   are replaced by stubs when HELPERS_DISABLED or HELPERS_NO_MULTITHREADING
   are defined. */


#if defined(HELPERS_DISABLED) || defined(HELPERS_NO_MULTITHREADING)

/* STUBS FOR THINGS THAT DO NOTHING MUCH WHEN THERE ARE NO HELPER THREADS. */

#define helpers_num 0
#define helpers_not_multithreading 1
#define helpers_not_pipelining 1

#define helpers_amount_out(p)        0

#define helpers_avail0(mx)           (mx)
#define helpers_avail1(mx)           (mx)
#define helpers_avail1(mx)           (mx)

#define helpers_avail_var(v,mx)      (mx)

#define helpers_idle()               0

#define helpers_no_multithreading(a) 0
#define helpers_no_pipelining(a)     0

#define HELPERS_SETUP_OUT(_pow2_)    do {} while (0)
#define HELPERS_NEXT_OUT(_i_)        do { _i_ += 1; } while (0)
#define HELPERS_BLOCK_OUT(_i_,_k_)   do { _i_ += _k_; } while (0)

#define HELPERS_UP_TO(_i_,_a_)       ((_a_)-1)
#define HELPERS_UP_TO2(_i,_a1_,_a2_) ((_a1_) > (_a2_) ? (_a2_)-1 : (_a1_)-1)

#define HELPERS_WAIT_IN0(_avail_,_prev_,_len_) do {_avail_ = (_len_);} while (0)
#define HELPERS_WAIT_IN1(_avail_,_prev_,_len_) do {_avail_ = (_len_);} while (0)
#define HELPERS_WAIT_IN2(_avail_,_prev_,_len_) do {_avail_ = (_len_);} while (0)

#define HELPERS_WAIT_IN_VAR(_v_,_avail_,_prev_,_len_) \
                                               do {_avail_ = (_len_);} while (0)

#else

/* NON_STUB PROCEDURES/MACROS FOR WHEN THERE MAY BE HELPER THREADS. */

extern int helpers_num;              /* Number of helper threads */
extern int helpers_not_multithreading; /* 1 if tasks done only by master */
extern int helpers_not_pipelining;   /* 1 if pipelining is disabled */

void helpers_amount_out (helpers_size_t); /* Set how much of output produced */

helpers_size_t helpers_avail0 (helpers_size_t); /* Get how much available */
helpers_size_t helpers_avail1 (helpers_size_t);
helpers_size_t helpers_avail2 (helpers_size_t);

helpers_size_t helpers_avail_var (helpers_var_ptr, helpers_size_t); 
                                     /* How much of a variable is available */

int helpers_idle (void);             /* Estimate of number of idle helpers */

void helpers_no_multithreading (int);/* Disable/re-enable tasks in helpers */
void helpers_no_pipelining (int);    /* Disable/re-enable pipelining */

/* Set up for pipelined output in a task procedure. */

#define HELPERS_SETUP_OUT(_pow2_) \
  const helpers_size_t helpers_mask = ((helpers_size_t) 1 << (_pow2_)) - 1;

/* Handle one element of pipelined output. */

#define HELPERS_NEXT_OUT(_i_) \
  do { \
    if ((++_i_&helpers_mask)==0) helpers_amount_out (_i_); \
  } while (0)

/* Handle a block of elements of pipelined output. */

#define HELPERS_BLOCK_OUT(_i_,_k_) \
  do { \
    helpers_size_t _oldi_ = _i_; \
    _i_ += (_k_); \
    if ((_oldi_^_i_)&~helpers_mask) helpers_amount_out (_i_); \
  } while (0)

#define HELPERS_UP_TO(_i_,_a_) \
  ((_a_) <= ((_i_)|helpers_mask) ? (_a_)-1 : ((_i_)|helpers_mask))

#define HELPERS_UP_TO2(_i_,_a1_,_a2_) \
  ((_a1_) > (_a2_) ? \
     ((_a2_) <= ((_i_)|helpers_mask) ? (_a2_)-1 : ((_i_)|helpers_mask)) \
   : ((_a1_) <= ((_i_)|helpers_mask) ? (_a1_)-1 : ((_i_)|helpers_mask)))

/* Get more pipelined input. */

#define HELPERS_WAIT_IN0(_avail_,_prev_,_len_) \
  do { \
    helpers_size_t _svp_ = (_prev_); \
    do { _avail_ = helpers_avail0(_len_); } while (_avail_ <= _svp_); \
  } while (0)

#define HELPERS_WAIT_IN1(_avail_,_prev_,_len_) \
  do { \
    helpers_size_t _svp_ = (_prev_); \
    do { _avail_ = helpers_avail1(_len_); } while (_avail_ <= _svp_); \
  } while (0)

#define HELPERS_WAIT_IN2(_avail_,_prev_,_len_) \
  do { \
    helpers_size_t _svp_ = (_prev_); \
    do { _avail_ = helpers_avail2(_len_); } while (_avail_ <= _svp_); \
  } while (0)

#define HELPERS_WAIT_IN_VAR(_v_,_avail_,_prev_,_len_) \
  do { \
    helpers_size_t _svp_ = (_prev_); \
    do { _avail_ = helpers_avail_var(_v_,_len_); } while (_avail_ <= _svp_); \
  } while (0)

#endif


#if defined(HELPERS_DISABLED)

/* STUBS FOR WHEN THE HELPERS FACILITY (NOT JUST HELPER THREADS) IS DISABLED. */

#define helpers_tasks 0
#define helpers_are_disabled 1

#define helpers_startup(n)           (helpers_master())

#define helpers_do_task(_flags_,_proc_,_op_,_out_,_in1_,_in2_) \
  ((_proc_)((_op_),(_out_),(_in1_),(_in2_)))

#define helpers_start_computing_var(v)              0
#define helpers_wait_until_not_in_use(v)            0
#define helpers_wait_until_not_being_computed2(u,v) 0
#define helpers_wait_until_not_being_computed(v)    0
#define helpers_wait_for_all_master_only()          0
#define helpers_wait_for_all()                      0

static helpers_var_ptr helpers_var_list_null[1] = { (helpers_var_ptr) 0 };

#define helpers_var_list()           helpers_var_list_null

#define helpers_trace(f)             0
#define helpers_stats()              0
#define helpers_disable(a)           0

#define HELPERS_NOW_OR_LATER(_c1_,_c2_,_flags_,_proc_,_op_,_out_,_in1_,_in2_) \
  ((_proc_)((_op_),(_out_),(_in1_),(_in2_)))


#else

/* NON-STUBS NEEDED WHEN HELPERS NOT DISABLED, EVEN IF NO MULTITHREADING. */

extern int helpers_tasks;            /* Number of outstanding tasks */
extern int helpers_are_disabled;     /* 1 if helpers are not enabled */

void helpers_startup (int);          /* Set up and then call master procedure */

void helpers_do_task                 /* Schedule a task to be performed */
  (int, helpers_task_proc *, 
   helpers_op_t, helpers_var_ptr, helpers_var_ptr, helpers_var_ptr);

void helpers_start_computing_var     /* Start computation of a variable */
  (helpers_var_ptr);

void helpers_wait_until_not_in_use   /* Wait till variable not used as input */
  (helpers_var_ptr);

void helpers_wait_until_not_being_computed2 /* Wait till two vars not outputs */
  (helpers_var_ptr, helpers_var_ptr);

#define helpers_wait_until_not_being_computed(v) \
  helpers_wait_until_not_being_computed2((v),(helpers_var_ptr)0)

void helpers_wait_for_all_master_only (void);/* Wait for all master-only tasks*/
void helpers_wait_for_all (void);     /* Wait till all tasks have finished */

helpers_var_ptr *helpers_var_list(void);  /* Return list of variables in use */

void helpers_trace (int);            /* Set whether trace info is written */
void helpers_stats (void);           /* Print statistics */
void helpers_disable (int);          /* Disable/re-enable helpers */

/* Conditionally schedule task with helpers_do_task or call it directly. */

#define HELPERS_NOW_OR_LATER(_c1_,_c2_,_flags_,_proc_,_op_,_out_,_in1_,_in2_) \
 ((_c1_)? helpers_do_task((_flags_),(_proc_),(_op_),(_out_),(_in1_),(_in2_)) : \
  (_c2_)? helpers_do_task((_flags_) | HELPERS_MASTER_NOW, \
                                    (_proc_),(_op_),(_out_),(_in1_),(_in2_)) : \
          ((_proc_)((_op_),(_out_),(_in1_),(_in2_))))

#endif
