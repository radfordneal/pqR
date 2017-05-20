all:	test-sbset test-sbset-static

test-sbset:	test-sbset.c sbset.c sbset-app.h sbset.h
	gcc test-sbset.c sbset.c -o test-sbset

test-sbset-static:	test-sbset.c sbset.c sbset-app.h sbset.h
	gcc -DSBSET_STATIC=1 test-sbset.c -o test-sbset-static
