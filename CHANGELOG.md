## 1.0.0

### Additions

* Added a `dart-show-hover` command bound to `C-c ?` which displays information
  about the Dart API under the cursor.

* Added a `dart-goto` command bound to `C-c C-g` which takes the user to the
  definition of the Dart API under the cursor.

* Added a `dart-find-refs` command bound to `C-c C-f` which shows a list of
  references to the Dart API under the cursor.

* Added a `dart-find-member-decls` command bound to `C-c C-e` which shows a list
  of declarations of members with a given name.

* Added a `dart-find-top-level-decls` command bound to `C-c C-t` which shows a
  list of declarations of top-level elements with a given name.

* Added a `dart-find-member-refs` command bound to `C-c C-r` which shows a list
  of references to members with a given name.

* Added a `dart-expand` command bound to `M-/` which cycles through possible
  completions for the text typed by the user, as well as a
  `dart-expand-parameters` command bound to `M-?` which inserts the parameter
  list for the chosen completion.

* Added a `dart-sdk-path` variable.

### Replacements

* `dart-executable-path` is now a function rather than a variable, so that it
  updates when `dart-sdk-path` is updated.

### Removals

* The `dart-analysis-server-snapshot-path` variable has been removed.
