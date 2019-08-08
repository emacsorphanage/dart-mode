(require 'faceup)

(defvar dart-font-lock-test-dir (faceup-this-file-directory))

(defun dart-font-lock-test-apps (file)
  "Test that the dart FILE is fontifies as the .faceup file describes."
  (let ((dart-mode-hook nil))
    (faceup-test-font-lock-file 'dart-mode
                                (concat dart-font-lock-test-dir file))))
(faceup-defexplainer dart-font-lock-test-apps)

(ert-deftest dart-font-lock-language-samples-test ()
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
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/abstract-methods.dart")))

(ert-deftest dart-font-lock-covariant-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/covariant.dart")))

(ert-deftest dart-font-lock-declared-operators-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/declared-operators.dart")))

(ert-deftest dart-font-lock-named-constructors-test ()
  :expected-result :failed
  (should (dart-font-lock-test-apps "faceup/issues/named-constructors.dart")))