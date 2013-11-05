/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
             Interface to Procedures Without Pipelining

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


double matprod_vec_vec (double *x, double *y, int k);
void matprod_vec_mat (double *x, double *y, double *z, int k, int m);
void matprod_mat_vec (double *x, double *y, double *z, int n, int k);
void matprod (double *x, double *y, double *z, int n, int k, int m);
void matprod_trans1 (double *x, double *y, double *z, int n, int k, int m);
void matprod_trans2 (double *x, double *y, double *z, int n, int k, int m);
