.POSIX:
EMACS = emacs

compile: cplx.elc

check: test
test:  cplx-tests.elc
	$(EMACS) -batch -Q -L . -l cplx-tests.elc -f ert-run-tests-batch

clean:
	rm -f cplx.elc cplx-tests.elc

cplx-tests.elc: cplx-tests.el cplx.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
