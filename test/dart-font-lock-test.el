
(defun dart-font-lock-test (faceup)
  (let (dart-enable-analysis-server)
    (faceup-test-font-lock-string 'dart-mode faceup)))
(faceup-defexplainer dart-font-lock-test)

(ert-deftest dart-void-main-test ()
  (should (dart-font-lock-test "«t:void» «f:main»() =>")))

(ert-deftest dart-main-test ()
  (should (dart-font-lock-test "«f:main»() =>")))

(ert-deftest dart-double-quoted-string-test ()
  (should (dart-font-lock-test "«t:String» «v:str» = «s:\"Hello, World!\"»;")))

(ert-deftest dart-single-quoted-string-test ()
  (should (dart-font-lock-test "«t:String» «v:str» = «s:'Hello, World!'»;")))

(ert-deftest dart-single-quoted-multi-line-string-test ()
  (should (dart-font-lock-test "«t:String» «v:str» = «s:'''Don't do that.'''»;")))

(ert-deftest dart-double-quoted-multi-line-string-test ()
  (should (dart-font-lock-test "«t:String» «v:str» = «s:\"\"\"Don't do that.\"\"\"»;")))
