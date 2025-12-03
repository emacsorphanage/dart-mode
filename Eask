;; -*- mode: eask; lexical-binding: t -*-

(package "dart-mode"
         "1.0.7"
         "Major mode for editing Dart files")

(website-url "https://github.com/emacsorphanage/dart-mode")
(keywords "languages")

(package-file "dart-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")

(development
 (depends-on "ert-runner")
 (depends-on "faceup")
 (depends-on "package-lint"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
