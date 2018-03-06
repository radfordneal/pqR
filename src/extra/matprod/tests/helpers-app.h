/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION
             Application Helpers Header File for Test Program with Pipelining

   Copyright (c) 2013, 2014, 2018 Radford M. Neal.

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


#include <stdint.h>


/* The task operator type is uint64_t, enough room for both k and split
   indicators. */

typedef uint64_t helpers_op_t;


/* The vector length type is an unsigned int. */

typedef unsigned int helpers_size_t;


/* The variables are identified by positive integers for the matrix operands
   and negative integers for the products. */

typedef int helpers_var_ptr;  


/* Define ENABLE_DEBUG as 1 to make helpers_debug do something. */

#define ENABLE_DEBUG 0


/* Now include the helpers.h file, after the above declarations. */

#include "helpers.h"


/* Macro giving the name of a task. */

#include "par-matprod.h"

extern helpers_task_proc task_output_vector;

#define helpers_task_name(p) \
( p==task_par_matprod_scalar_vec ? "matprod_scalar_vec" : \
  p==task_par_matprod_vec_vec    ? "matprod_vec_vec" : \
  p==task_par_matprod_mat_vec    ? "matprod_mat_vec" : \
  p==task_par_matprod_vec_mat    ? "matprod_vec_mat" : \
  p==task_par_matprod_outer      ? "matprod_outer" : \
  p==task_par_matprod_mat_mat    ? "matprod_mat_mat" : \
  p==task_par_matprod_trans1     ? "matprod_trans1" : \
  p==task_par_matprod_trans2     ? "matprod_trans2" : \
  p==task_par_matprod_trans12    ? "matprod_trans12" : \
  p==task_output_vector          ? "output_vector" : "?" \
)

/* Macro giving the name of a variable. */

extern char *my_var_name (helpers_var_ptr v);

#define helpers_var_name(v) my_var_name(v)


/* Include the header for the test skeleton, since needed for macros below. */

#include "test.h"


/* Macros used in par-matprod for getting the length and data pointers
   for variables. */

#define REAL(v)   (v>0 ? matrix[v-1] : product[-v-1])
#define LENGTH(v) (v>0 ? matlen[v-1] : prodlen[-v-1])
