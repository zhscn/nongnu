include config.mk

.PHONY: info clean

all: info package

help:
	$(info make all     - generate info manual)
	$(info make info    - generate info manual)
	$(info make package - generate tar file containing the package)
	$(info make clean   - remove generated files)
	@exit

info: $(PKG).info dir

%.info: %.texi
	@echo "Generating $@"
	@$(MAKEINFO) --no-split $< -o $@

dir: $(PKG).info
	@echo "Generating $@"
	@$(INSTALLINFO) $< $@

package: $(PKG)-$(VERSION).tar

%.tar: $(PKG).info dir *.el
	@echo "Creating temporary package directory"
	@mkdir $(PKG)-$(VERSION)
	@echo "Copying package files to temporary directory"
	@cp -a $^ $(PKG)-$(VERSION)/
	@echo "Creating package archive $@"
	@tar -cf $@ $(PKG)-$(VERSION)
	@echo "Removing temporary package directory"
	@rm -rf $(PKG)-$(VERSION)

clean:
	@echo "Cleaning..."
	@rm -rf $(PKG).info dir $(PKG)-$(VERSION).tar
