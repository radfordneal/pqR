all:	interp interp-db interp-no-offset interp-no-set-static \
	interp-no-max-segments interp-no-segment-at-a-time interp-no-builtins \
	interp-alloc-small-kind interp-alloc-small-kind-quickly

interp:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c -o interp

interp-db:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O0 -DSET_DEBUG=1 \
	 interp.c sggc.c set.c -o interp-db

interp-no-offset:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 \
	 interp.c sggc.c -o interp-no-offset

interp-no-set-static:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c set.c -o interp-no-set-static

interp-no-max-segments:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 interp.c sggc.c -o interp-no-max-segments

interp-no-segment-at-a-time:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSGGC_SEGMENT_AT_A_TIME=0 \
	 interp.c sggc.c -o interp-no-segment-at-a-time

interp-no-builtins:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DSET_USE_BUILTINS=0 \
	 interp.c sggc.c -o interp-no-builtins

interp-alloc-small-kind:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND=1 \
	 interp.c sggc.c -o interp-alloc-small-kind

interp-alloc-small-kind-quickly:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSET_STATIC=1 -DSGGC_USE_OFFSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND_QUICKLY=1 \
	 interp.c sggc.c -o interp-alloc-small-kind-quickly
