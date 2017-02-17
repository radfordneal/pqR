test-sggc7:	test-sggc7.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h \
		test-common2.h test-calloc.c test-calloc.h
	gcc -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 -Wno-logical-op-parentheses \
		test-sggc7.c sggc.c set.c test-calloc.c -o test-sggc7
