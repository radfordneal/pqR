all:	test-sggc2 test-sggc2a

test-sggc2:	test-sggc2.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h \
		sbset.h test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc2.c sggc.c sbset.c test-calloc.c -o test-sggc2

test-sggc2a:	test-sggc2.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h \
		sbset.h test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 -DSGGC_DATA_ALIGNMENT=16 \
		test-sggc2.c sggc.c sbset.c test-calloc.c -o test-sggc2a
