CFLAGS = --std=c99 -Wall -O2
DEBUGCFLAGS = -g -DDEBUG $(CFLAGS)

HSFLAGS = --make -Wall -O2 -hidir ../bin -odir ../bin
DEBUGHSFLAGS = $(HSFLAGS)

TARBALL = icfp-95780824.tgz


all: bin/lifter bin/validator bin/debuglifter bin/debugvalidator


bin/libvm.o: src/libvm.h src/libvm.c
	gcc -c $(CFLAGS) -o bin/libvm.o src/libvm.c

bin/lifter: src/Lifter.hs src/VM.hs bin/libvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/lifter Lifter.hs ../bin/libvm.o

bin/validator: src/Validator.hs src/VM.hs bin/libvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/validator Validator.hs ../bin/libvm.o


bin/libdebugvm.o: src/libvm.h src/libvm.c
	gcc -c $(DEBUGCFLAGS) -o bin/libdebugvm.o src/libvm.c

bin/debuglifter: src/Lifter.hs src/VM.hs bin/libdebugvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/debuglifter Lifter.hs ../bin/libdebugvm.o

bin/debugvalidator: src/Validator.hs src/VM.hs bin/libdebugvm.o
	cd src; ghc $(HSFLAGS) -o ../bin/debugvalidator Validator.hs ../bin/libdebugvm.o


tarball: $(TARBALL)

$(TARBALL): lifter
	tar zcvf $(TARBALL) install lifter PACKAGES-TESTING src README

lifter: bin/lifter
	cp bin/lifter lifter


clean:
	rm -f bin/*

.PHONY: all clean
