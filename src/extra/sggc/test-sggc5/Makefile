test-sggc5:	test-sggc5.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
		test-sggc5.c sggc.c sbset.c test-calloc.c -o test-sggc5


# Below not made by default, need settings of environment vars CPTR and TRAP.

test-sggc5t:	test-sggc5.c sggc.c sbset.c sggc-app.h sbset.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
                            -DSGGC_TRACE_CPTR=$(CPTR) \
		test-sggc5.c sggc.c sbset.c test-calloc.c -o test-sggc5t

test-sggc5ta:	test-sggc5.c sggc.c sbset.c sggc-app.h sbset.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
                            -DSGGC_TRACE_CPTR=$(CPTR) \
                            -DSGGC_TRACE_ALLOC_TRAP=$(TRAP) \
		test-sggc5.c sggc.c sbset.c test-calloc.c -o test-sggc5ta

test-sggc5tf:	test-sggc5.c sggc.c sbset.c sggc-app.h sbset.h sggc.h sbset-app.h sbset.h \
		test-common.h test-calloc.c test-calloc.h
	gcc -std=c99 -g -O0 -DSGGC_DEBUG=1 -DSET_DEBUG=1 \
                            -DSGGC_TRACE_CPTR=$(CPTR) \
                            -DSGGC_TRACE_FREE_TRAP=$(TRAP) \
		test-sggc5.c sggc.c sbset.c test-calloc.c -o test-sggc5tf
