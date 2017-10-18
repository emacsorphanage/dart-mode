`dart-mode` is a major mode for editing Dart files in Emacs.

* [Installation](#installation)
* [Dart Analyzer](#dart-analyzer)
  * [Error Checking](#error-checking)
  * [Seeing Information](#seeing-information)
  * [Navigation](#navigation)
  * [Search](#search)

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

### Seeing Information

To see all the information the analyzer knows about a particular identifier,
move your cursor onto it and press `C-c ?`. This will show the identifier's type
and documentation in the echo area at the bottom of the editor, as well as some
extra information if it's available.

Sometimes there's just too much documentation to fit down there, or you want to
keep the documentation open as you're working. In that case, you can run `C-u
C-c ?` instead to open the information in a new window to read at your leisure.

### Navigation

When your cursor is on an identifier, you can press `C-c C-g` to go to the exact
location that identifier was originally defined. This can even take you to the
Dart SDK's sources, or to packages that your library imports. Be careful when
you're there, though: any edits may corrupt your package cache!

### Search

You can search for all references to the identifier under your cursor by
pressing `C-c C-f`. This will show you everywhere a method, getter, or setter is
called; everywhere a class is used as a type, constructed, or has static methods
called on it; everywhere a named argument is passed; and so on.
