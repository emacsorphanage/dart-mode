all: package-lint dart-mode.elc test

.cask:
	cask install

package-lint: .cask
	cask emacs -batch -l package-lint.el -f package-lint-batch-and-exit

dart-mode.elc:
	emacs -batch -f batch-byte-compile dart-mode.el

test-setup: .cask dart-mode.elc

test-font-lock: test-setup
	cask emacs -batch -l dart-mode.elc -l ert -l test/test-font-lock.el -f ert-run-tests-batch-and-exit

test-indentation: test-setup
	cask emacs -batch -l dart-mode.elc -l ert -l test/test-indentation.el -f ert-run-tests-batch-and-exit

test: test-font-lock test-indentation

checkdoc:
	emacs -batch -eval "(when (>= emacs-major-version 25) (checkdoc-file \"dart-mode.el\"))"

clean: clean-cask clean-elc

clean-cask:
	rm -rf .cask

clean-elc:
	rm dart-mode.elc

.PHONY: test
