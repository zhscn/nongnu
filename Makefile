all:
	@echo "There is no need to call make or install Geiser-STklos."
	@echo "You can call 'make test' to test it, though."

.PHONY: test-stklos
test-stklos:
	stklos --no-init-file --utf8-encoding=yes -f geiser-stklos-test.stk

.PHONY: test-emacs
test-emacs:
	@emacs -batch -l ert -l geiser-stklos-test.el -f ert-run-tests-batch-and-exit 1> test-emacs-stdout.log

test: test-stklos test-emacs


