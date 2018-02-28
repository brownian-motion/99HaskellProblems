tests: .test
.test: src/problem*.hs
	@for sourcefile in $? ; do \
	    echo "Testing $$sourcefile:"; \
	    runhaskell $$sourcefile; \
	    echo ; \
	done ; \
	touch .test

.PHONY: tests clean

clean:
	rm .test
