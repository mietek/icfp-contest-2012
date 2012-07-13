all: libvm.a

clean:
	rm -f libvm.o libvm.a

libvm.o: libvm.h libvm.c
	gcc -c --std=c99 -Wall -O2 -o libvm.o libvm.c

libvm.a: libvm.o
	ar rcs libvm.a libvm.o
