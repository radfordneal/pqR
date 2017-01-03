test-sggc1:	test-sggc1.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -g -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc1.c sggc.c set.c test-calloc.c -o test-sggc1
