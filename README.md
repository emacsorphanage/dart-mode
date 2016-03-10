Dart Mode
=========
Dart Mode is a major mode for editing Dart files in Emacs.

## Installation

1. Add [Marmalade](https://marmalade-repo.org/#download) to your
   `package-archives` if you don't already have it.

1.  Install dart-mode via:
    ```
    M-x package-refresh-contents [RET]
    M-x package-install [RET] dart-mode
    ```

1.  OPTIONAL: To enable on-the-fly syntax checking, add the
    following to your `.emacs` file:
    ```
    (setq dart-enable-analysis-server t)
    (add-hook 'dart-mode-hook 'flycheck-mode)
    ```

1.  OPTIONAL: To enable imenu support, add the
    following to your `.emacs` file:
    ```
	(add-hook 'dart-mode-hook
          (lambda ()
            (setq imenu-create-index-function #'dart-imenu-index)))
    ```
