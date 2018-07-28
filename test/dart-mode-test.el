;;; dart-mode-test.el --- Tests for dart-mode

(require 'dart-mode)

(ert-deftest dart-test ()
  (let ((file (make-temp-file "analysis")))
    (dart-add-analysis-root-for-file file)
    (let ((proc (dart--analysis-server-process
		 dart--analysis-server)))
      (accept-process-output proc)
      (accept-process-output proc)
      (accept-process-output proc 8))))

;;; dart-mode-test.el ends here
