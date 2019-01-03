;;; dart-mode-test.el --- Tests for dart-mode

(require 'dart-mode)
(require 'faceup)

(defvar dart-font-lock-test-dir
  (expand-file-name "faceup-tests"
                    (faceup-this-file-directory)))

(defun dart-font-lock-test-apps (file)
  "Test that the dart FILE is fontifies as the .faceup file describes."
  (faceup-test-font-lock-file 'dart-mode
                              (expand-file-name file dart-font-lock-test-dir)))
(faceup-defexplainer dart-font-lock-test-apps)

(defun dart-font-lock-test (faceup)
  (faceup-test-font-lock-string 'dart-mode faceup))
(faceup-defexplainer dart-font-lock-test)

(ert-deftest dart-font-lock-file-test ()
  (should (dart-font-lock-test-apps "anonymous-functions.dart"))
  (should (dart-font-lock-test-apps "catch.dart"))
  (should (dart-font-lock-test-apps "control-flow-statements.dart"))
  (should (dart-font-lock-test-apps "default-parameter-values.dart"))
  (should (dart-font-lock-test-apps "optional-position-parameters.dart"))
  ;; (should (dart-font-lock-test-apps "string-interpolation-breaks-on-quotes.dart"))
  ;; (should (dart-font-lock-test-apps "wikipedia-class.dart"))
  (should (dart-font-lock-test-apps "wikipedia-fibonacci.dart"))
  (should (dart-font-lock-test-apps "wikipedia-hello-world.dart")))

(ert-deftest dart-font-lock-string-interpolation-nested-string-test ()
  :expected-result :failed
  (should (dart-font-lock-test ("  «t:String» «b:get» «v:_errorName» => «s:\"Invalid argument${!_hasValue ? \"(s)\" : \"\"}\"»;"))))

(ert-deftest dart-font-lock-method-declaration ()
  :expected-result :failed
  (should (dart-font-lock-tests "  «t:Point».«f:origin»()")))

(ert-deftest dart-font-lock-operator-declaration ()
  :expected-result :failed
  (should (dart-font-lock-tests "  «t:Point» «b:operator» «f:+»(«t:Point» «v:other») => «t:Point»(x + other.x, y + other.y);")))

;;; dart-mode-test.el ends here
