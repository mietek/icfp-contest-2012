all: lifter validator

vm.o: vm.h vm.c
	gcc -c -g -DDEBUG --std=c99 -Wall -O2 -o vm.o vm.c

libvm.a: vm.o
	ar rcs libvm.a vm.o

lifter: libvm.a VM.hs Lifter.hs
	ghc --make -Wall -O2 -L. -lvm -fforce-recomp -o lifter Lifter.hs

validator: libvm.a VM.hs Validator.hs
	ghc --make -Wall -O2 -L. -lvm -fforce-recomp -o validator Validator.hs

clean:
	rm -f *.a *.hi *.o lifter validator

.PHONY: all clean
