test-sggc4:	test-sggc4.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc4.c sggc.c sbset.c test-calloc.c -o test-sggc4
