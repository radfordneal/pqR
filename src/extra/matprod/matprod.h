/* MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION
             Interface to Single-Thread Procedures 

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

void matprod_scalar_vec (double x, double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z, int m);

double matprod_vec_vec (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          int k);

void matprod_vec_mat (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int k,
          int m);

void matprod_mat_vec (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int k);

void matprod_mat_mat (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int k,
          int m);

void matprod_outer (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int m);

void matprod_trans1 (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int k,
          int m);

void matprod_trans2 (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int k,
          int m);

void matprod_trans12 (double * MATPROD_RESTRICT x,
          double * MATPROD_RESTRICT y,
          double * MATPROD_RESTRICT z,
          int n,
          int k,
          int m);

void matprod_fill_lower (double * MATPROD_RESTRICT z, int n);
