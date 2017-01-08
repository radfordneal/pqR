all:	interp interp-db interp-set-static

interp:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 interp.c sggc.c set.c -o interp

interp-db:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -DSGGC_DEBUG=1 -DSET_DEBUG=1 interp.c sggc.c set.c -o interp-db

interp-set-static:	interp.c sggc.c set.c sggc-app.h sggc.h set-app.h set.h
	gcc -g -O3 -DSET_STATIC=1 interp.c sggc.c -o interp-set-static
