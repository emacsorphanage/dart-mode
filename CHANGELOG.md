## 1.0.0

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

* `dart-executable-path`'s default value is now set correctly even if the
  executable named `dart` on the user's path is a symlink or a wrapper script.
