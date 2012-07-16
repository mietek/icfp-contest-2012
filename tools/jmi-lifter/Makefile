all: main

main: main.c ../../bin/libdebugvm.o
	gcc --std=c99 -Wall -O2 -I../../src -o main main.c ../../bin/libdebugvm.o

clean:
	rm -f main

.PHONY: all clean
