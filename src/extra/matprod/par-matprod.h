/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Interface to Task Procedures for Parallel Operation

   Copyright (c) 2013, 2014, 2017, 2018 Radford M. Neal.

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


/* Define MATPROD_RESTRICT as the restrict keyword, unless
   MATPROD_NO_RESTRICT has been defined, in which case it is nothing. */

#undef MATPROD_RESTRICT

#ifdef MATPROD_NO_RESTRICT
#define MATPROD_RESTRICT 
#else
#define MATPROD_RESTRICT restrict
#endif


helpers_task_proc task_par_matprod_scalar_vec;
helpers_task_proc task_par_matprod_vec_vec;
helpers_task_proc task_par_matprod_vec_mat;
helpers_task_proc task_par_matprod_mat_vec;
helpers_task_proc task_par_matprod_mat_mat;
helpers_task_proc task_par_matprod_outer;
helpers_task_proc task_par_matprod_trans1;
helpers_task_proc task_par_matprod_trans2;
helpers_task_proc task_par_matprod_trans12;

void par_matprod_scalar_vec (helpers_var_ptr z, helpers_var_ptr x, 
                             helpers_var_ptr y, int split, int pipe);

void par_matprod_vec_vec (helpers_var_ptr z, helpers_var_ptr x, 
                          helpers_var_ptr y, int split, int pipe);

void par_matprod_vec_mat (helpers_var_ptr z, helpers_var_ptr x, 
                          helpers_var_ptr y, int split, int pipe);

void par_matprod_mat_vec (helpers_var_ptr z, helpers_var_ptr x, 
                          helpers_var_ptr y, int split, int pipe);

void par_matprod_outer (helpers_var_ptr z, helpers_var_ptr x, 
                        helpers_var_ptr y, int split, int pipe);

void par_matprod_mat_mat (helpers_var_ptr z, helpers_var_ptr x, 
                          helpers_var_ptr y, int k, int split, int pipe);

void par_matprod_trans1 (helpers_var_ptr z, helpers_var_ptr x, 
                         helpers_var_ptr y, int k, int split, int pipe);

void par_matprod_trans2 (helpers_var_ptr z, helpers_var_ptr x, 
                         helpers_var_ptr y, int k, int split, int pipe);

void par_matprod_trans12 (helpers_var_ptr z, helpers_var_ptr x, 
                          helpers_var_ptr y, int k, int split, int pipe);
