;;; lisp-mode-tests.el --- Test Lisp editing commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Brady Trainor

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

;; The wonderful code below is stolen from
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-mode/test/unit-test.el,
;; as of commit a5afa3a.

;;; Code:

(require 'ert)
(require 'shut-up)
(require 'dart-mode)

;; (ert-deftest dart-auto-mode-alist ()
;;   "Assert that files with extension .dart open in dart-mode."

;;   )

(defmacro should-indent-to (source result)
  "Assert that SOURCE is indented to produce RESULT."
  `(with-temp-buffer
     (insert ,source)
     (dart-mode)
     (shut-up
       (indent-region (point-min) (point-max)))
     (should (equal (buffer-substring-no-properties (point-min) (point-max))
                    ,result))))

(defmacro should-preserve-indent (source)
  "Assert that SOURCE does not change when indented."
  (let ((src-sym (make-symbol "src")))
    `(let ((,src-sym ,source))
       (should-indent-to ,src-sym ,src-sym))))

(ert-deftest dart-indent-main ()
  (should-preserve-indent
   "
main() {
  print(2);
  print(3);
}
"))
(ert-deftest dart-indent-conditional ()
  (should-preserve-indent
   "
if (isRaining()) {
  you.bringRainCoat();
} else if (isSnowing()) {
  you.wearJacket();
} else {
  car.putTopDown();
}
"))

(ert-deftest dart-indent-for ()
  (should-preserve-indent
   "
for (var i = 0; i < 5; i++) {
  message.write('!');
}
"))

(ert-deftest dart-indent-while ()
  (should-preserve-indent
   "
while (!isDone()) {
  doSomething();
}
"))

(ert-deftest dart-indent-arglist ()
  :expected-result :failed
  (should-preserve-indent
   "
main() {
  print(\"Learn Dart in 15 minutes!\");
  [
    example1,
  ].forEach((ef) => ef());
}
"))

(ert-deftest dart-indent-follow-comment ()
  (should-preserve-indent
   "
/*
 *
 */
main() {}
"))

(ert-deftest dart-indent-do-while ()
  (should-preserve-indent
   "
do {
  printLine();
} while (!atEndOfPage());
"))

(ert-deftest dart-indent-switch ()
  :expected-result :failed
  (should-preserve-indent
   "
var command = 'OPEN';
switch (command) {
  case 'CLOSED':
    executeClosed();
    break;
  case 'PENDING':
    executePending();
    break;
  case 'APPROVED':
    executeApproved();
    break;
  case 'DENIED':
    executeDenied();
    break;
  case 'OPEN':
    executeOpen();
    break;
  default:
    executeUnknown();
}
"))

(provide 'dart-mode-tests)
;;; dart-mode-tests.el ends here
