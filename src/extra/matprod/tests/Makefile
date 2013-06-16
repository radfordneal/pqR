# MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
#           Makefile for Compiling Test Programs
#
# Copyright (c) 2013 Radford M. Neal.
# 
#   The matprod library is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

progs=matprod-test matprod-alt-test piped-matprod-test piped-matprod-test-d \
      blas-test blas2-test

all:	$(progs)

matprod-test:		test.h test.c matprod-test.c ../matprod.h ../matprod.c
	gcc -I.. -I. -O2 test.c matprod-test.c ../matprod.c \
		-o matprod-test

matprod-alt-test:	test.h test.c matprod-test.c ../matprod.h ../matprod.c
	gcc -I.. -I. -O2 \
		-DALT_MULT_VEC_VEC -DALT_MULT_MAT_VEC \
		-DALT_MULT_VEC_MAT -DALT_MULT_MAT_MAT \
		test.c matprod-test.c ../matprod.c \
		-o matprod-alt-test

piped-matprod-test:	test.h test.c piped-matprod-test.c ../piped-matprod.h \
			../piped-matprod.c helpers-app.h helpers.h helpers.c
	gcc -I.. -I. -O2 -fopenmp \
		test.c piped-matprod-test.c ../piped-matprod.c helpers.c \
		-o piped-matprod-test

piped-matprod-test-d:	test.h test.c piped-matprod-test.c ../piped-matprod.h \
			../piped-matprod.c helpers-app.h helpers.h helpers.c
	gcc -I.. -I. -O2 -fopenmp -DHELPERS_DISABLED \
		test.c piped-matprod-test.c ../piped-matprod.c helpers.c \
		-o piped-matprod-test-d

blas-test:		test.h test.c blas-test.c ddot.f dgemm.f dgemv.f lsame.f
	gcc -I.. -I. -O2 test.c blas-test.c ddot.f dgemm.f dgemv.f lsame.f \
		-o blas-test

blas2-test:		test.h test.c blas-test.c blas.f
	gcc -I.. -I. -O2 test.c blas-test.c blas.f \
		-lm -o blas2-test

clean:
	rm -f $(progs)
