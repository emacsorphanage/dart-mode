all: package-lint compile test

ci: clean build compile

build:
	eask package
	eask install

compile:
	eask compile

package-lint: build
	eask lint package

test-setup: build compile
	eask install-deps --dev

test-font-lock: test-setup
	eask test ert test/test/test-font-lock.el

test-indentation: test-setup
	eask test ert test/test-indentation.el

test: test-font-lock test-indentation

checkdoc:
	eask lint checkdoc

clean:
	eask clean all

clean-elc:
	eask clean elc

.PHONY: test
