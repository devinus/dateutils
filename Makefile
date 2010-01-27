include support/include.mk
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.0.1

all:
	mkdir -p ./ebin
	(cd src;$(MAKE))

clean:
	rm -rf ebin/*.beam *.tgz

test: all
	mkdir -p t/.logs
	prove t/*.t

package: clean
	@mkdir dateutils-$(VERSION)/ && cp -rf src t support Makefile dateutils-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf dateutils-$(VERSION).tgz dateutils-$(VERSION)
	@rm -rf dateutils-$(VERSION)/
    	
install:
	mkdir -p $(prefix)/$(LIBDIR)/dateutils-$(VERSION)/ebin
	for i in ebin/*.beam; do install $$i $(prefix)/$(LIBDIR)/dateutils-$(VERSION)/$$i ; done
