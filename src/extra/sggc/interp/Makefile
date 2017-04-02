all:	interp interp-db interp-pg interp-no-offset interp-no-set-static \
	interp-no-max-segments interp-no-segment-at-a-time interp-no-builtins \
	interp-alloc-kind interp-alloc-small-kind \
	interp-alloc-small-kind-quickly \
	interp-memset-quickly interp-seg-direct interp-seg-direct-no-max \
	interp-uncollected-nil interp-uncollected-nil-syms \
	interp-uncollected-nil-syms-globals

CC=gcc

interp:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c -o interp

interp-pg:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -c interp.c sggc.c
	$(CC) -g -pg interp.o sggc.o -o interp-pg

interp-db:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O0 -DSET_DEBUG=1 \
	 interp.c sggc.c set.c -o interp-db

interp-no-offset:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 \
	 interp.c sggc.c -o interp-no-offset

interp-no-set-static:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c set.c -o interp-no-set-static

interp-no-max-segments:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c -o interp-no-max-segments

interp-no-segment-at-a-time:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSGGC_SEGMENT_AT_A_TIME=0 \
	 interp.c sggc.c -o interp-no-segment-at-a-time

interp-no-builtins:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSET_USE_BUILTINS=0 \
	 interp.c sggc.c -o interp-no-builtins

interp-alloc-kind:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUSE_ALLOC_KIND=1 \
	 interp.c sggc.c -o interp-alloc-kind

interp-alloc-small-kind:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND=1 \
	 interp.c sggc.c -o interp-alloc-small-kind

interp-alloc-small-kind-quickly:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND_QUICKLY=1 -DUSE_ALLOC_SMALL_KIND=1 \
	 interp.c sggc.c -o interp-alloc-small-kind-quickly

interp-memset-quickly:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSGGC_USE_MEMSET \
	 interp.c sggc.c -o interp-memset-quickly

interp-seg-direct:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSGGC_SEG_DIRECT \
	 interp.c sggc.c -o interp-seg-direct

interp-seg-direct-no-max:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSGGC_SEG_DIRECT \
	 interp.c sggc.c -o interp-seg-direct-no-max

interp-uncollected-nil:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL \
	 interp.c sggc.c -o interp-uncollected-nil

interp-uncollected-nil-syms:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL_SYMS \
	 interp.c sggc.c -o interp-uncollected-nil-syms

interp-uncollected-nil-syms-globals:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL_SYMS_GLOBALS \
	 interp.c sggc.c -o interp-uncollected-nil-syms-globals
