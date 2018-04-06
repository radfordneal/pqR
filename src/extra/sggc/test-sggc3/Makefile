all:	test-sggc3 test-sggc3d test-sggc3t

test-sggc3:	test-sggc3.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc3.c sggc.c sbset.c test-calloc.c -o test-sggc3

test-sggc3d:	test-sggc3.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
                -DSGGC_CLEAR_FREE -DSGGC_KEEP_CPTR=0 \
		test-sggc3.c sggc.c sbset.c test-calloc.c -o test-sggc3d

test-sggc3t:	test-sggc3.c sggc.c sbset.c sggc-app.h sbset.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 -DSGGC_TRACE_CPTR \
		test-sggc3.c sggc.c sbset.c test-calloc.c -o test-sggc3t
