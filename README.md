`dart-mode` is a major mode for editing Dart files in Emacs.

* [Installation](#installation)
* [Dart Analyzer](#dart-analyzer)
  * [Error Checking](#error-checking)

## Installation

1. Add [Marmalade](https://marmalade-repo.org/#download) to your
   `package-archives` if you don't already have it.

2.  Install dart-mode via:
    ```
    M-x package-refresh-contents [RET]
    M-x package-install [RET] dart-mode
    ```

## Dart Analyzer

`dart-mode` supports the Dart analysis server, which runs in the background and
analyzes your Dart code to figure out what every identifier and method call
refers to. It provides all sorts of useful features that aren't possible when
your code is treated as plain text.

To enable analyzer support, add `(setq dart-enable-analysis-server t)` to your
`.emacs` file.

### Error Checking

The Dart analyzer can use [Flycheck][] to notify you of errors and warnings in
your Dart code. To enable this, just add `(add-hook 'dart-mode-hook
'flycheck-mode)` to your `.emacs` file. Don't worry about installing
Flycheck—if you have `dart-mode`, you automatically have it as well!

[Flycheck]: http://www.flycheck.org/en/latest/
