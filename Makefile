.PHONY: test

test:
	cask exec ert-runner

emacs:
	cask emacs -Q -nw -l dart-mode.el -l init.el test/dart
