GUILE := rtl-guile

.PHONY: test

test:
	@for t in ./test-suite/tests/*.test; do $(GUILE) -L ./test-suite/ $$t; done


