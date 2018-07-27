.PHONY: test

test:
	cask emacs -Q -batch -L . --eval "(require 'dart-mode)"
