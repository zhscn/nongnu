#!/usr/bin/make

# Copyright © 2024 Vivek Das Moapatra <vivek@etla.org>
# SPDX-License-Identifier: GPL-3.0-or-later

ELCS  := $(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: check all elcs check-%

all: check

clean:
	@rm -rvf $(ELCS)

%.elc: %.el
	@emacs -Q -q --batch \
	  --eval '(progn (require '\''bytecomp) (byte-recompile-file "$<" t 0))'

elcs: $(ELCS)

check-%:
	emacs -Q -q --batch -l tests/test-$*.el

check-base32: base32.elc
check-totp: $(ELCS) check-base32

check: check-totp

