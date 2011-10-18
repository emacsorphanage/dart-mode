;;; dart-mode.el --- Major mode for editing Dart files

;; Author: Nathan Weizenbaum
;; URL: http://code.google.com/p/dart-mode
;; Version: 0.3
;; Keywords: language

;; Copyright (C) 2011 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'dart-mode)
;;
;; Known bugs:
;;
;; * Multiline strings using """ and ''' are not recognized. They fontify
;;   correctly, but only because they look like three strings in a row.
;; * In a map with identifier keys, the first key is fontified like a label.
;; * Optional arguments aren't always fontified correctly.
;; * =>-style methods that span multiple lines can screw up indentation and
;;   brace matching.
;; * Named constructors aren't fontified correctly.

;;; Code:

(require 'cc-mode)
(require 'cc-langs)
(eval-when-compile (require 'cl))

(eval-and-compile (c-add-language 'dart-mode 'java-mode))


;;; CC configuration

(c-lang-defconst c-symbol-start
  dart (concat "[" c-alpha "_]"))

(c-lang-defconst c-after-id-concat-ops
  dart nil)

(c-lang-defconst c-multiline-string-start-char
  dart ?@)

(c-lang-defconst c-opt-cpp-prefix
  dart "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives
  dart nil)

(c-lang-defconst c-cpp-include-directives
  dart nil)

(c-lang-defconst c-opt-cpp-macro-define
  dart nil)

(c-lang-defconst c-cpp-expr-directives
  dart '("import" "source" "library" "resource"))

(c-lang-defconst c-cpp-expr-functions
  dart nil)

(c-lang-defconst c-operators
  dart `((prefix "#")
         ,@(c-lang-const c-identifier-ops)
         (postfix-if-paren "<" ">")
         (prefix "super")
         (left-assoc ".")
         (postfix "++" "--" "[" "]" "(" ")")
         (unary "++" "--" "+" "-" "!" "~" "negate" "new" "const")
         (left-assoc "*" "/" "%")
         (left-assoc "+" "-")
         (left-assoc "<<" ">>" ">>>")
         (left-assoc "<" ">" "<=" ">=")
         (left-assoc "==" "!=" "===" "!==" "is" "is!")
         (left-assoc "&")
         (left-assoc "^")
         (left-assoc "|")
         (left-assoc "&&")
         (left-assoc "||")
         (right-assoc-sequence "?" ":")
         (left-assoc "=>")
         (right-assoc ,@(c-lang-const c-assignment-operators))
         (left-assoc ",")))

(c-lang-defconst c-overloadable-operators
  dart '("==" "<" ">" "<=" ">=" "-" "+" "*" "/" "%" "|" "^" "&"
         "<<" ">>" ">>>" "[]=" "[]" "~" "negate"))

(c-lang-defconst c-opt-op-identifier-prefix
  dart (c-make-keywords-re t '("operator")))

(c-lang-defconst c-doc-comment-start-regexp
  dart nil)

(c-lang-defconst c-paragraph-start
  dart "$")

(c-lang-defconst c-primitive-type-kwds
  dart '("Dynamic" "void" "num" "int" "double" "bool"))

(c-lang-defconst c-class-decl-kwds
  dart '("class" "interface"))

;; Don't put these in c-modifier-kwds because they can be used without a type
;; following them.
(c-lang-defconst c-typeless-decl-kwds
  dart '("abstract" "const" "factory" "final" "operator" "static" "typedef" "var"))

(c-lang-defconst c-modifier-kwds
  dart nil)

(c-lang-defconst c-other-decl-kwds
  dart nil)

(c-lang-defconst c-decl-hangon-kwds
  dart '("get" "set" "native"))

(c-lang-defconst c-postfix-decl-spec-kwds
  dart '("extends" "implements" "factory"))

(c-lang-defconst c-type-list-kwds
  dart '("new" "const" "is" "is!" "extends" "implements" "factory"))

(c-lang-defconst c-ref-list-kwds
  dart nil)

(c-lang-defconst c-block-stmt-2-kwds
  dart '("for" "if" "switch" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  dart '("break" "continue" "return" "throw"))

(c-lang-defconst c-before-label-kwds
  dart '("break" "continue"))

(c-lang-defconst c-inexpr-class-kwds
  dart nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  dart nil)

(c-lang-defconst c-other-kwds
  dart '("in"))

(c-lang-defconst c-cast-parens
  dart nil)

(c-lang-defconst c-block-prefix-disallowed-chars
  dart (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                       '(?\" ?')))

(c-lang-defconst c-opt-type-suffix-key
  dart nil)

(c-lang-defconst c-recognize-typeless-decls
  dart t)

(c-lang-defconst c-recognize-<>-arglists
  dart t)

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))

(c-lang-defconst c-opt-postfix-decl-spec-kwds
  dart '("native"))


;;; CC indentation support

(defun dart-in-block-p (syntax-guess)
  "Return whether or not the immediately enclosing {} block is a code block.
The other option, of course, is a map literal.

SYNTAX-GUESS is the output of `c-guess-basic-syntax'."
  (save-excursion
    (c-safe
      (backward-up-list)
      (when (= (char-after) ?\{)
        (c-backward-comments)
        (or
         ;; Both anonymous and named functions have a ")" immediately before the
         ;; code block.
         (= (char-before) ?\))
         ;; "else" is the only keyword that comes immediately before a block.
         (looking-back "\\<else\\>")
         ;; CC is good at figuring out if we're in a class.
         (assq 'inclass syntax-guess))))))

(defadvice c-guess-basic-syntax (after dart-guess-basic-syntax activate)
  (when (c-major-mode-is 'dart-mode)
    (let* ((syntax (car (last ad-return-value)))
           (type (car syntax)))
      (save-excursion
        (beginning-of-line)

        (or
         ;; Handle array literal indentation
         (when (memq type
                     '(arglist-intro
                       arglist-cont
                       arglist-cont-nonempty
                       arglist-close))
           (save-excursion
             (c-safe
               (backward-up-list)
               (when (= (char-after) ?\[)
                 (setq ad-return-value
                       `((,(case type
                             (arglist-intro 'brace-list-intro)
                             ((arglist-cont arglist-cont-nonempty) 'brace-list-entry)
                             (arglist-close 'brace-list-close))
                          ,(cadr syntax))))
                 t))))

         ;; Handle map literal indentation
         (when (and (memq type '(label statement-block-intro statement-cont statement block-close))
                    (not (dart-in-block-p ad-return-value)))
           (save-excursion
             (c-safe
               (c-backward-comments)
               ;; Completely reset ad-return-value here because otherwise it
               ;; gets super-screwy.
               (if (= (char-before) ?\{)
                   (progn
                     (back-to-indentation)
                     (setq ad-return-value `((brace-list-intro ,(point))))
                     t)
                 (backward-up-list)
                 (when (= (char-after) ?\{)
                   (c-forward-comments)
                   (back-to-indentation)
                   (setq ad-return-value `((brace-list-entry ,(point))))
                   t))))))))))


;;; Boilerplate font-lock piping

(defcustom dart-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in DART mode.
Each list item should be a regexp matching a single identifier.")

(defconst dart-font-lock-keywords-1 (c-lang-const c-matchers-1 dart)
  "Minimal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-2 (c-lang-const c-matchers-2 dart)
  "Fast normal highlighting for Dart mode.")

(defconst dart-font-lock-keywords-3 (c-lang-const c-matchers-3 dart)
  "Accurate normal highlighting for Dart mode.")

(defvar dart-font-lock-keywords dart-font-lock-keywords-3
  "Default expressions to highlight in Dart mode.")

(defvar dart-mode-syntax-table nil
  "Syntax table used in dart-mode buffers.")
(unless dart-mode-syntax-table
  (setq dart-mode-syntax-table
        (funcall (c-lang-const c-make-mode-syntax-table dart))))


;;; Initialization

;;;###autoload (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

;;;###autoload
(defun dart-mode ()
  "Major mode for editing Dart files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `dart-mode-hook'.

Key bindings:
\\{dart-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table dart-mode-syntax-table)
  (setq major-mode 'dart-mode
        mode-name "Dart")
  (c-init-language-vars dart-mode)
  (c-common-init 'dart-mode)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'dart-mode-hook)
  (c-update-modeline))

(provide 'dart-mode)

;;; dart-mode.el ends here
