CFLAGS = --std=c99 -Wall -O2
DEBUGCFLAGS = -g -DDEBUG $(CFLAGS)

HSFLAGS = --make -Wall -O2
DEBUGHSFLAGS = $(HSFLAGS)

TARBALL = icfp-95780824.tgz


all: bin/lifter bin/validator bin/debuglifter bin/debugvalidator


bin:
	mkdir bin


bin/libvm.o: bin src/libvm.h src/libvm.c
	gcc -c $(CFLAGS) -o bin/libvm.o src/libvm.c

bin/lifter: bin src/Lifter.hs src/VM.hs bin/libvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/lifter Lifter.hs ../bin/libvm.o

bin/validator: bin src/Validator.hs src/VM.hs bin/libvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/validator Validator.hs ../bin/libvm.o


bin/libdebugvm.o: bin src/libvm.h src/libvm.c
	gcc -c $(DEBUGCFLAGS) -o bin/libdebugvm.o src/libvm.c

bin/debuglifter: bin src/Lifter.hs src/VM.hs bin/libdebugvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/debuglifter Lifter.hs ../bin/libdebugvm.o

bin/debugvalidator: bin src/Validator.hs src/VM.hs bin/libdebugvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/debugvalidator Validator.hs ../bin/libdebugvm.o


tarball: $(TARBALL)

$(TARBALL): lifter
	rm -f src/*.hi src/*.o
	tar zcvf $(TARBALL) install lifter Makefile PACKAGES-TESTING README src

lifter: bin/lifter
	cp bin/lifter lifter


clean:
	rm -f bin/* src/*.hi src/*.o lifter $(TARBALL)

.PHONY: all clean
