all:
	@echo "There is no need to call make ot install Geiser-STklos."
	@echo "You can call 'make test' to test it, though."

test:
	stklos --no-init-file --utf8-encoding=yes -f geiser-stklos-test.stk
