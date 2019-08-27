(require 'faceup)

(defvar dart-font-lock-test-dir (faceup-this-file-directory))

(defun dart-font-lock-test-apps (file)
  "Test that the dart FILE is fontifies as the .faceup file describes."
  (let ((dart-mode-hook nil))
    (faceup-test-font-lock-file 'dart-mode
                                (concat dart-font-lock-test-dir file))))
(faceup-defexplainer dart-font-lock-test-apps)

(ert-deftest dart-font-lock-language-samples-test ()
  (should (dart-font-lock-test-apps "faceup/core/async.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/async.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/classes.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/comments.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/control-flow-statements.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/exceptions.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/functions.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/hello-world.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/imports.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/inheritance.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/interfaces-and-abstract-classes.dart"))
  (should (dart-font-lock-test-apps "faceup/language-samples/variables.dart")))

(ert-deftest dart-font-lock-abstract-method-test ()
  (should (dart-font-lock-test-apps "faceup/issues/abstract-methods.dart")))

(ert-deftest dart-font-lock-covariant-test ()
  (should (dart-font-lock-test-apps "faceup/issues/covariant.dart")))

(ert-deftest dart-font-lock-declared-operators-test ()
  (should (dart-font-lock-test-apps "faceup/issues/declared-operators.dart")))

(ert-deftest dart-font-lock-factory-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/factory.dart")))

(ert-deftest dart-font-lock-false-positive-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/stagehand/server.dart")))

(ert-deftest dart-font-lock-generic-method-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/generic-method.dart")))

(ert-deftest dart-font-lock-named-constructors-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/named-constructors.dart")))

(defun dart-font-lock-test (faceup)
  (faceup-test-font-lock-string 'dart-mode faceup))
(faceup-defexplainer dart-font-lock-test)

(ert-deftest dart-font-lock-declared-identifier-anchors ()
  "Simple Dart font-lock tests."
  (should (dart-font-lock-test "«k:var» «v:a», «v:b»;"))
  (should (dart-font-lock-test "group(«s:\"WordCount: Ignore special characters - \"», ignoreSpecialCharacters);")))
