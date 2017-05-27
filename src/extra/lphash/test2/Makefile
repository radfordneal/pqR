all:	test2 test2-linear

test2:		test2.c lphash-app.h lphash.h lphash.c
	gcc test2.c lphash.c -o test2

test2-linear:	test2.c lphash-app.h lphash.h lphash.c
	gcc -DLPHASH_LINEAR test2.c lphash.c -o test2-linear
