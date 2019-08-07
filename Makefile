all: package-lint dart-mode.elc checkdoc

.cask:
	cask install

package-lint: .cask
	cask emacs -batch -l package-lint.el -f package-lint-batch-and-exit

dart-mode.elc: .cask
	cask emacs -batch -f batch-byte-compile dart-mode.el

checkdoc:
	emacs -batch -eval "(checkdoc-file \"dart-mode.el\")"

clean: clean-cask clean-elc

clean-cask:
	rm -rf .cask

clean-elc:
	rm dart-mode.elc
