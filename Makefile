tests: .test
.test: $(shell find src/ \( -type f -name 'problem*.hs' \) )
	@for sourcefile in $? ; do \
	    echo "Testing $$sourcefile:"; \
	    runhaskell $$sourcefile; \
	    echo ; \
	done ; \
	touch .test

.PHONY: tests clean

clean:
	rm .test
