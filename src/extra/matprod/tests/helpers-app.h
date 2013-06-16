/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Application Helpers Header File for Test Program with Pipelining

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


/* The task operand type and vector length type are both unsigned int. */

typedef unsigned int helpers_op_t;
typedef unsigned int helpers_size_t;


/* The variables are identified by positive integers for the matrix operands
   and negative integers for the products. */

typedef int helpers_var_ptr;  


/* Now include the helpers.h file, after the above declarations. */

#include "helpers.h"


/* Macro giving the name of a task. */

extern helpers_task_proc task_piped_matprod_vec_vec, task_piped_matprod_mat_vec,
                         task_piped_matprod_vec_mat, task_piped_matprod,
                         task_output_vector;

#define helpers_task_name(p) \
( p==task_piped_matprod_vec_vec ? "matprod_vec_vec" : \
  p==task_piped_matprod_mat_vec ? "matprod_mat_vec" : \
  p==task_piped_matprod_vec_mat ? "matprod_vec_mat" : \
  p==task_piped_matprod ? "matprod" : \
  p==task_output_vector ? "output_vector" : "?" \
)

/* Macro giving the name of a variable. */

extern char *my_var_name (helpers_var_ptr v);

#define helpers_var_name(v) my_var_name(v)


/* Include the header for the test skeleton, since needed for macros below. */

#include "test.h"


/* Macros used in piped-matprod for getting the length and data pointers
   for variables. */

#define REAL(v)   (v>0 ? matrix[v-1] : product[-v-1])
#define LENGTH(v) (v>0 ? matlen[v-1] : prodlen[-v-1])
