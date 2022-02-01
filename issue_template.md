<!--
Please familiarize yourself with
<https://en.wikipedia.org/wiki/Minimal_reproducible_example>.

The template below may give an idea of what to include.

Modify as appropriate.
-->

First, I created a temporary working directory.

```
> mkdir /tmp/mbe
> cd /tmp/mbe
> touch .emacs
> touch example.dart
```

Contents of init file:

```elisp
;; /tmp/mbe/.emacs

(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(let ((my-pkgs '(dart-mode)))
  (when (seq-remove 'package-installed-p my-pkgs)
    (package-refresh-contents)
    (mapc (lambda (pkg) (package-install pkg t)) my-pkgs)))
```

Contents of Dart file:

```dart
// /tmp/mbe/example.dart
void main() {}
```

Then I ran the following:

```
> HOME=$(pwd)
> emacs example.dart
```
