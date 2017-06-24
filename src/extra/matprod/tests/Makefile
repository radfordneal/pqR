# MATPROD - A LIBRARY FOR MATRIX MULTIPLICATION WITH OPTIONAL PIPELINING
#           Makefile for Compiling Test Programs
#
# Copyright (c) 2013, 2017 Radford M. Neal.
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

progs=matprod-test matprod-norestrict-test matprod-alt-test \
      piped-matprod-test piped-matprod-norestrict-test piped-matprod-d-test \

blas-progs=blas1-test blas2-test blas3-test  # blas0-test omitted

CC=gcc-7
CFLAGS=-g -std=gnu99 -O3 -march=native -mtune=native -mfpmath=sse

all:	$(progs)
	true

blas:	$(blas-progs)
	true

matprod-test:		test.h test.c matprod-test.c ../matprod.h ../matprod.c
	$(CC) -I.. -I. $(CFLAGS) test.c matprod-test.c ../matprod.c \
		-o matprod-test

matprod-norestrict-test:	test.h test.c matprod-test.c ../matprod.h ../matprod.c
	$(CC) -I.. -I. $(CFLAGS) -DMATPROD_NO_RESTRICT \
		test.c matprod-test.c ../matprod.c \
		-o matprod-norestrict-test

matprod-alt-test:	test.h test.c matprod-test.c ../matprod.h ../matprod.c
	$(CC) -I.. -I. $(CFLAGS) \
		-DALT_MATPROD_VEC_VEC -DALT_MATPROD_MAT_VEC \
		-DALT_MATPROD_VEC_MAT -DALT_MATPROD -DALT_MATPROD_TRANS2 \
		test.c matprod-test.c ../matprod.c \
		-o matprod-alt-test

piped-matprod-test:	test.h test.c piped-matprod-test.c ../piped-matprod.h \
			../piped-matprod.c helpers-app.h helpers.h helpers.c
	$(CC) -I.. -I. $(CFLAGS) -fopenmp \
		test.c piped-matprod-test.c ../piped-matprod.c helpers.c \
		-o piped-matprod-test

piped-matprod-norestrict-test:	test.h test.c piped-matprod-test.c ../piped-matprod.h \
			../piped-matprod.c helpers-app.h helpers.h helpers.c
	$(CC) -I.. -I. $(CFLAGS) -fopenmp -DMATPROD_NO_RESTRICT \
		test.c piped-matprod-test.c ../piped-matprod.c helpers.c \
		-o piped-matprod-norestrict-test

piped-matprod-d-test:	test.h test.c piped-matprod-test.c ../piped-matprod.h \
			../piped-matprod.c helpers-app.h helpers.h helpers.c
	$(CC) -I.. -I. $(CFLAGS) -fopenmp -DHELPERS_DISABLED \
		test.c piped-matprod-test.c ../piped-matprod.c helpers.c \
		-o piped-matprod-d-test

blas0-test:		test.h test.c blas-test.c \
			ddot.f dgemm.f dgemv.f dsyrk.f lsame.f
	$(CC) -I.. -I. $(CFLAGS) \
		test.c blas-test.c ddot.f dgemm.f dgemv.f dsyrk.f lsame.f \
		-o blas0-test

blas1-test:		test.h test.c blas-test.c blas.f
	$(CC) -I.. -I. $(CFLAGS) test.c blas-test.c blas.f \
		-lm -o blas1-test

blas2-test:		test.h test.c blas-test.c blas.f
	$(CC) -I.. -I. $(CFLAGS) test.c blas-test.c blas2.f \
		-lm -o blas2-test

blas3-test:		test.h test.c blas-test.c
	$(CC) -I.. -I. $(CFLAGS) test.c blas-test.c /usr/lib/libblas.so.3 \
		-lm -o blas3-test

clean:
	rm -f $(progs) $(blas-progs)
