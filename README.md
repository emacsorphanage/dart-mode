`dart-mode` is a major mode for editing Dart files in Emacs.

* [Installation](#installation)
  * [General Configuration](#general-configuration)
* [Dart Analyzer](#dart-analyzer)
  * [Error Checking](#error-checking)
  * [Seeing Information](#seeing-information)
  * [Navigation](#navigation)
  * [Search](#search)
  * [Expansion](#expansion)
* [Dart Formatter](#dart-formatter)
  * [Formatter Configuration](#formatter-configuration)

## Installation

1. Add [Melpa](https://melpa.org/#/getting-started) to your
   `package-archives` if you don't already have it.

2.  Install dart-mode via:
    ```
    M-x package-refresh-contents [RET]
    M-x package-install [RET] dart-mode
    ```

### General Configuration

The `dart-sdk-path` variable can be set to tell Emacs where to find the Dart
SDK. This is used to run the [Dart analysis server](#dart-analyzer) and the Dart
formatter. By default, it's set by finding the `dart` executable on the system
path.

If you've installed `flutter` but not `dart`, you might try `(setq
dart-sdk-path "/path/to/flutter/bin/cache/dart-sdk/")`.

If you're on Windows, you will need to make sure that the `diff`
executable is available to Emacs. One way to do this is install from
http://gnuwin32.sourceforge.net/packages/diffutils.htm and add
something like the following to your init file. `(setq exec-path
(append exec-path '("C:/Program Files (x86)/GnuWin32/bin")))`


Note that user code that wants to run Dart scripts can use the
`dart-executable-path` function to locate the `dart` executable itself in the
SDK's `bin/` directory.

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

There are several ways to use the analyzer to search through your Dart code. All
of these pop up a search results window listing every use with handy links to
take you right to the code. Note that for large codebases, additional results
may be added to these results pages as the analyzer finds them. But once you see
the last "Found X results" line, you know for sure you're seeing everything!

* You can search for all references to the identifier under your cursor by
  pressing `C-c C-f`. This will show you everywhere a method, getter, or setter
  is called; everywhere a class is used as a type, constructed, or has static
  methods called on it; everywhere a named argument is passed; and so on.

* You can search for all member declarations with a given name by pressing `C-c
  C-e`. This will list all declarations within classes that have the name, but
  not any declarations at the top level.

* If you want to search for top-level declarations instead, you can press `C-c
  C-t`.

* If you want to find all *references to* members with a given name, you can
  press `C-c C-r`. This will show you everywhere a member with that name is
  called, even if it's in a dynamic context and the analyzer can't figure out
  what it's referring to.

### Expansion

If you press `M-/`, the analyzer will try to expand whatever text you've already
typed into a valid identifier. This uses the same logic that IDEs use for
autocomplete, but the UI works like Emacs' [`dabbrev-expand`][dabbrev] command.
You can press `M-/` multiple times in a row to cycle through possible
completions.

When you've selected an expansion that's a method call, you can press `M-?`
(`M-/` plus shift) to insert the parameter list. The first parameter will be
selected, and anything you type will replace it. Once it's replaced, you can
press `M-?` again to select the second parameter, and so on.

[dabbrev]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html

If the analysis server isn't enabled for the current buffer, this will fall back
to whatever command is assigned to `M-/` outside of Dart mode (`dabbrev-expand`
in vanilla Emacs). This will usually pick up any custom key bindings, but if it
doesn't you can manually choose a fallback by setting the `

## Dart Formatter

Dart comes with a [formatter][] that modifies Dart code's whitespace to make the
formatting consistent and readable. You can press `C-c C-o` to format the
current buffer.

[formatter]: https://github.com/dart-lang/dart_style#readme

### Formatter Configuration

By default, `dart-mode` will use the version of the formatter that's bundled
with the Dart SDK. However, you can customize this by setting
`dart-formatter-command-override`. Note that if you want to access the formatter
command from Elisp, you should call the `dart-formatter-command` function
instead.

When formatting fails, usually because the buffer's Dart code couldn't be
parsed, a buffer listing the errors will pop up by default. This behavior can be
customized by setting `dart-formatter-show-errors`. It has three valid values:

* `'buffer` is the default, and pops up a buffer listing the errors.
* `'echo` shows the errors temporarily in the echo area at the bottom of the frame.
* `nil` doesn't show the errors at all.

If you set the `dart-format-on-save` variable to `t`, the formatter will be run
automatically before you save any Dart buffer. This can be helpful when working
on codebases where formatting is required.
