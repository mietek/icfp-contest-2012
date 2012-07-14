all: lifter validator debuglifter debugvalidator


libvm.o: libvm.h libvm.c
	gcc -c --std=c99 -Wall -O2 -o libvm.o libvm.c

lifter: libvm.o VM.hs Lifter.hs
	ghc --make -Wall -O2 -o lifter Lifter.hs libvm.o

validator: libvm.o VM.hs Validator.hs
	ghc --make -Wall -O2 -o validator Validator.hs libvm.o


libdebugvm.o: libvm.h libvm.c
	gcc -c -g -DDEBUG --std=c99 -Wall -O2 -o libdebugvm.o libvm.c

debuglifter: libdebugvm.o VM.hs Lifter.hs
	ghc --make -Wall -O2 -o debuglifter Lifter.hs libdebugvm.o

debugvalidator: libdebugvm.o VM.hs Validator.hs
	ghc --make -Wall -O2 -o debugvalidator Validator.hs libdebugvm.o


clean:
	rm -f *.hi *.o lifter validator debuglifter debugvalidator

.PHONY: all clean
