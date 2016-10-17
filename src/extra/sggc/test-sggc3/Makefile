test-sggc3:	test-sggc3.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h \
		test-common.h test-malloc.c test-malloc.h
	gcc -g -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc3.c sggc.c set.c test-malloc.c -o test-sggc3
