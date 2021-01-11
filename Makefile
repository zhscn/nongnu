include config.mk

.PHONY: info package clean

.SUFFIXES: .texi .info

all: package

help:
	$(info make info     - generate the info manual)
	$(info make package  - generate a tar file containing the package)
	$(info make clean    - remove generated files)
	@exit

.texi.info:
	$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	$(INSTALLINFO) $? $@

info: $(PKG).info dir

$(PKG)-$(VERSION).tar: $(PKG).info dir *.el LICENSE
	mkdir $(PKG)-$(VERSION)
	cp -a $? $(PKG)-$(VERSION)/
	$(TAR) -cf $@ $(PKG)-$(VERSION)
	rm -rf $(PKG)-$(VERSION)

package: $(PKG)-$(VERSION).tar

clean:
	rm -rf $(PKG).info dir $(PKG)-$(VERSION).tar
