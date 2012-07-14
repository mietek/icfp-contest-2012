all: libvm.a validator

clean:
	rm -f libvm.o libvm.a VM.hi VM.o Validator.hi Validator.o validator

libvm.o: libvm.h libvm.c
	gcc -c --std=c99 -Wall -O2 -o libvm.o libvm.c

libvm.a: libvm.o
	ar rcs libvm.a libvm.o

validator: VM.hs libvm.o
	ghc --make -Wall -O2 -L. -lvm -fforce-recomp -o validator Validator.hs
