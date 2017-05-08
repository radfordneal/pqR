all:	test-set test-set-static

test-set:	test-set.c set.c set-app.h set.h
	gcc test-set.c set.c -o test-set

test-set-static:	test-set.c set.c set-app.h set.h
	gcc -DSET_STATIC=1 test-set.c -o test-set-static
