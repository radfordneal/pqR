all:	interp interp-db interp-pg interp-no-offset interp-no-sbset-static \
	interp-no-max-segments interp-no-segment-at-a-time interp-no-builtins \
	interp-alloc-kind interp-alloc-small-kind \
	interp-alloc-small-kind-quickly \
	interp-memset-quickly interp-seg-direct interp-seg-direct-no-max \
	interp-uncollected-nil interp-uncollected-nil-syms \
	interp-uncollected-nil-syms-globals interp-call-freed \
	interp-clear-free interp-clear-free-no-reuse interp-check-valid \
	interp-no-object-zero interp-seg-blocking interp-data-blocking \
	interp-find-obj-ret \
	interpmod.o

CC=gcc

interpmod.o:	interpmod.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h
	$(CC) -c -Wall interpmod.c

interp:	interp.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 interp.c sggc.c -o interp

interp-pg:	interp.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -c interp.c sggc.c
	$(CC) -g -pg interp.o sggc.o -o interp-pg

interp-db:	interp.c sggc.c sbset.c sggc-app.h sggc.h sbset-app.h sbset.h
	$(CC) -g -O0 -DSBSET_DEBUG=1 \
	 interp.c sggc.c sbset.c -o interp-db

interp-no-offset:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 interp.c sggc.c -o interp-no-offset

interp-no-sbset-static:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 interp.c sggc.c sbset.c -o interp-no-sbset-static

interp-no-max-segments:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSBSET_STATIC=1 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 interp.c sggc.c -o interp-no-max-segments

interp-no-segment-at-a-time:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
				sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_SEGMENT_AT_A_TIME=0 \
	 interp.c sggc.c -o interp-no-segment-at-a-time

interp-no-builtins:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSBSET_USE_BUILTINS=0 \
	 interp.c sggc.c -o interp-no-builtins

interp-alloc-kind:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUSE_ALLOC_KIND=1 \
	 interp.c sggc.c -o interp-alloc-kind

interp-alloc-small-kind:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
				sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND=1 \
	 interp.c sggc.c -o interp-alloc-small-kind

interp-alloc-small-kind-quickly:	interp.c sggc.c sbset.c sggc-app.h \
					sggc.h sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUSE_ALLOC_SMALL_KIND_QUICKLY=1 -DUSE_ALLOC_SMALL_KIND=1 \
	 interp.c sggc.c -o interp-alloc-small-kind-quickly

interp-memset-quickly:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_USE_MEMSET \
	 interp.c sggc.c -o interp-memset-quickly

interp-seg-direct:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_SEG_DIRECT \
	 interp.c sggc.c -o interp-seg-direct

interp-seg-direct-no-max:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
				sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSBSET_STATIC=1 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_SEG_DIRECT \
	 interp.c sggc.c -o interp-seg-direct-no-max

interp-uncollected-nil:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL \
	 interp.c sggc.c -o interp-uncollected-nil

interp-uncollected-nil-syms:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
				sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL_SYMS \
	 interp.c sggc.c -o interp-uncollected-nil-syms

interp-uncollected-nil-syms-globals:	interp.c sggc.c sbset.c sggc-app.h \
					sggc.h sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DUNCOLLECTED_NIL_SYMS_GLOBALS \
	 interp.c sggc.c -o interp-uncollected-nil-syms-globals

interp-call-freed:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DCALL_NEWLY_FREED=1 \
	 interp.c sggc.c -o interp-call-freed

interp-clear-free:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_CLEAR_FREE \
	 interp.c sggc.c -o interp-clear-free

interp-clear-free-no-reuse:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
				sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_CLEAR_FREE -DNO_REUSE=1 \
	 interp.c sggc.c -o interp-clear-free-no-reuse

interp-check-valid:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DCHECK_VALID=1 \
	 interp.c sggc.c -o interp-check-valid

interp-no-object-zero:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_NO_OBJECT_ZERO \
	 interp.c sggc.c -o interp-no-object-zero

interp-seg-blocking:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_SEG_BLOCKING=2048 \
	 interp.c sggc.c -o interp-seg-blocking

interp-data-blocking:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_SMALL_DATA_BLOCKING=8 -DSGGC_SMALL_DATA_ALIGN=32 \
	 interp.c sggc.c -o interp-data-blocking

interp-find-obj-ret:	interp.c sggc.c sbset.c sggc-app.h sggc.h \
			sbset-app.h sbset.h
	$(CC) -g -O3 -march=native -mtune=native \
	 -DSGGC_MAX_SEGMENTS=10000 -DSBSET_STATIC=1 \
	 -DSGGC_USE_OFFSBSET_POINTERS=1 \
	 -DSGGC_FIND_OBJECT_RETURN \
	 interp.c sggc.c -o interp-find-obj-ret
