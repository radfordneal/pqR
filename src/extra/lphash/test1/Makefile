all:	test1 test1-resize test1-linear

test1:		test1.c lphash-app.h lphash.h lphash.c
	gcc -DLPHASH_STATS test1.c lphash.c \
	    -o test1

test1-resize:	test1.c lphash-app.h lphash.h lphash.c
	gcc -DLPHASH_MAX_LOAD=0.5 -DLPHASH_STATS test1.c lphash.c \
	    -o test1-resize

test1-linear:	test1.c lphash-app.h lphash.h lphash.c
	gcc -DLPHASH_LINEAR -DLPHASH_STATS test1.c lphash.c \
	    -o test1-linear
