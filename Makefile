TARBALL = icfp-95780824.tgz

all: tarball

tarball: $(TARBALL)

$(TARBALL): lifter
	tar zcvf $(TARBALL) install lifter PACKAGES-TESTING src README

lifter:
	touch lifter

clean:
	rm -f $(TARBALL) lifter

.PHONY: all tarball clean
