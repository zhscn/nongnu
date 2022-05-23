include config.mk

.PHONY: all help info package clean

.SUFFIXES: .texi .info

all: clean package

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

$(PKG)-pkg.el: $(PKG).el
	sed -n -e 's/.* --- /(define-package "$(PKG)" "$(VERSION)" "/' \
	    -e 's/ -\*- lexical-binding: t -\*-/"/p' \
	    -e "s/;; Package-Requires: /  '/p" \
	    -e 's/, /" "/' -e 's/;; Keywords: /  :keywords ("/p' $? \
	    | sed '$$s/$$/")/' > $@
	sed -n -e 's/ </" . "/' -e 's/>/"))/' \
	    -e 's/;; Author: /  :authors (("/p' $? \
	    >> $@
	sed -n -e 's/ </" . "/' -e 's/>/")/' \
	    -e 's/;; Maintainer: /  :maintainer ("/p' \
	    -e 's/;; URL: /  :url "/p' $? \
	    | sed '$$s/$$/")\n;; Local Variables:\n;; no-byte-compile: t\n;; end:/' \
	    >> $@

$(PKG)-$(VERSION).tar: $(PKG).info dir $(PKG)-pkg.el *.el LICENSE
	mkdir $(PKG)-$(VERSION)
	cp -a $? $(PKG)-$(VERSION)/
	$(TAR) -cf $@ $(PKG)-$(VERSION)
	rm -rf $(PKG)-$(VERSION)

package: $(PKG)-$(VERSION).tar

clean:
	rm -f $(PKG).info dir $(PKG)-pkg.el $(PKG)-$(VERSION).tar
