
(ert-deftest dart-syntax-block-intro-test ()
  (with-temp-buffer
    (insert "
↓void main() {
  ·bool stmt = True;
  bool stmt = True;
}
")
    (goto-char (point-min))
    (let (anchor point syntax)
      (setq anchor (1- (search-forward "↓")))
      (delete-char -1)
      (setq point (1- (search-forward "·")))
      (delete-char -1)
      (should (equal (dart-guess-basic-syntax)
                     `((block-intro ,anchor)))))))

(ert-deftest dart-syntax-statement-test ()
  (with-temp-buffer
    (insert "
void main() {
  ↓bool stmt = True;
  ·bool stmt = True;
}
")
    (goto-char (point-min))
    (let (anchor point syntax)
      (setq anchor (1- (search-forward "↓")))
      (delete-char -1)
      (setq point (1- (search-forward "·")))
      (delete-char -1)
      (should (equal (dart-guess-basic-syntax)
                     `((statement ,anchor)))))))

(ert-deftest dart-syntax-close-test ()
  (with-temp-buffer
    (insert "
↓void main() {
  bool stmt = True;
  bool stmt = True;
·}
")
    (goto-char (point-min))
    (let (anchor point syntax)
      (setq anchor (1- (search-forward "↓")))
      (delete-char -1)
      (setq point (1- (search-forward "·")))
      (delete-char -1)
      (should (equal (dart-guess-basic-syntax)
                     `((close ,anchor)))))))
