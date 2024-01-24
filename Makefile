#!/usr/bin/make

# Copyright © 2024 Vivek Das Moapatra <vivek@etla.org>
# SPDX-License-Identifier: GPL-3.0-or-later

ELCS  := $(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: check all elcs

all: check

clean:
	@rm -rvf $(ELCS)

%.elc: %.el
	@emacs -Q -q --batch \
	  --eval '(progn (require '\''bytecomp) (byte-recompile-file "$<" t 0))'

elcs: $(ELCS)

check: $(ELCS)
	emacs -Q -q --batch -l tests/test-totp.el

