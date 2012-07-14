all: lifter validator

libvm.o: libvm.h libvm.c
	gcc -c -g -DDEBUG --std=c99 -Wall -O2 -o libvm.o libvm.c

lifter: libvm.o VM.hs Lifter.hs
	ghc --make -Wall -O2 -o lifter Lifter.hs libvm.o

validator: libvm.o VM.hs Validator.hs
	ghc --make -Wall -O2 -o validator Validator.hs libvm.o

clean:
	rm -f *.hi *.o lifter validator

.PHONY: all clean
