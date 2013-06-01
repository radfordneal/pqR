/* HELPERS - A LIBRARY SUPPORTING COMPUTATIONS USING HELPER THREADS
             Application Header File for Example Programs

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


/* This is the include file helpers-app.h for use with the example 
   applications of the helpers facility. */


/* The task operand type and vector length type are both unsigned int. */

typedef unsigned int helpers_op_t;
typedef unsigned int helpers_size_t;


/* The four variables used are arrays of doubles (except D is scalar). */

typedef double *helpers_var_ptr;  

extern double *A, *B, *C, D;


/* Macros for marking a variable as "in use" or "being computed".  These
   markers are maintained for variable B only, in the two global variables
   declared below, and defined in example.c. */

extern int B_in_use, B_being_computed;

#define helpers_mark_in_use(v) \
  do { if ((v)==B) B_in_use = 1; } while (0)

#define helpers_mark_not_in_use(v) \
  do { if ((v)==B) B_in_use = 0; } while (0)

#define helpers_mark_being_computed(v) \
  do { if ((v)==B) B_being_computed = 1; } while (0)

#define helpers_mark_not_being_computed(v) \
  do { if ((v)==B) B_being_computed = 0; } while (0)


/* Include the helpers.h file here, so declarations below can use types
   defined in it.  */

#include "helpers.h"


/* Macro giving the name of a variable. */

#define helpers_var_name(v) \
  ( (v)==A ? "A" : (v)==B ? "B" : (v)==C ? "C" : (v)==&D ? "D" : "?")


/* Macro giving the name of a task. */

extern char *my_task_name (helpers_task_proc *);

#define helpers_task_name(p) my_task_name(p)
