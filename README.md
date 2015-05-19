Dart Mode
=========
Dart Mode is a major mode for editing Dart files in Emacs.

## Installation

1.  Download this repository, e.g.
    ```
    $ git clone https://github.com/nex3/dart-mode.git
    ```
1.  Ensure you have `cl-lib`, `dash` and `flycheck` installed.
1.  Load `dart-mode.el` on your path and require both `flycheck` and
    `dart-mode`. An typical `.emacs` configuration file might look like:
    ```
    (load "/path/to/dart-mode.el")
    (require 'flycheck)
    (require 'dart-mode)
    (setq dart-enable-analysis-server t)
    (add-hook 'dart-mode-hook 'flycheck-mode)
    (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
    ```
