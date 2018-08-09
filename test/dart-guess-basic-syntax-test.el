
(defun remove-char (char)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward char nil t)
      (setq beg (match-beginning 0))
      (delete-region beg (match-end 0))
      beg)))

(defun set-buffer-string-with-point (string)
  (insert string)
  (let ((anchor (remove-char "↓"))
        (point (remove-char "·")))
    (goto-char point)
    anchor))

(defun ert-dart-syntax-test (type string)
  (let (dart-enable-analysis-server)
    (with-temp-buffer
      (dart-mode)
      (let* ((anchor (set-buffer-string-with-point string))
             (syntax (dart-guess-basic-syntax)))
        (if anchor
            (should (equal syntax (list (list type anchor))))
          (should (equal syntax (list (list type)))))))))

(ert-deftest dart-syntax-comment-test ()
  (ert-dart-syntax-test 'comment "
/*
 *·
 */
"))

(ert-deftest dart-syntax-block-intro-test ()
  (ert-dart-syntax-test 'block-intro "
↓void main() {
  ·bool stmt = True;
  bool stmt = True;
}
"))

(ert-deftest dart-syntax-statement-test ()
  (ert-dart-syntax-test 'statement "
void main() {
  ↓bool stmt = True;
  ·bool stmt = True;
}
"))

(ert-deftest dart-syntax-close-test ()
  (ert-dart-syntax-test 'close "
↓void main() {
  bool stmt = True;
  bool stmt = True;
·}
"))
