;;; company-dart.el

;; Author: Sidart Kurias
;; Version: 0.01
;; Package-Requires: ((dart-mode "0.14") (company-mode))
;; Keywords: language

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

;; Add something similar to your init file
;; (add-hook 'dart-mode-hook (lambda ()
;;    (set (make-local-variable 'company-backends)
;;      '(company-dart (company-dabbrev company-yasnippet)))))
;;
;; Dart completion will be invoked only after the "." character has been typed.
;; For this to work company-minimum-prefix-length should be set to 0. If that
;; is inconvenient you can manually invoke completion by binding (company-dart)
;; to any key you like.  Includes an almighty hack (sleep-for). Removing this
;; will need modification in company-mode (??).
;;
;; A good source for snippets
;; https://github.com/JEG2/dotfiles/tree/master/emacs.d/jeg2/snippets/dart-mode/
;;
;; https://github.com/expez/company-quickhelp. Shows complete documentation as
;; a popup.
;;

;;; Code:
(require 'dart-mode)
(require 'company)


(defun dart--company-prepare-candidates (response)
  "Grab candidates and properties from Json response."
  (let ((completions (cdr (assq 'results (assq 'params response)))))
    (mapcar
     (lambda (completion)
       (let ((docSummary (assoc 'docSummary completion))
	     (parameters  (assoc 'parameters (assoc 'element completion)))
	     (docComplete  (assoc 'docComplete completion))
	     (candidate (cdr (assq 'completion completion))))
	 (propertize
	  (concat candidate (format "%s" (if  parameters (cdr parameters) " ")))
	  (car docSummary) (cdr docSummary)
	  (car docComplete) (cdr docComplete))
	 ))
     completions)))


(defun dart--register-for-completion-event (response callback)
  "Get the result-id passed by the analysis server and register a callback to
be invoked when the event is received."
  (-when-let* ((result-assoc (assoc 'result response))
  	       (id-assoc (assoc 'id result-assoc))
  	       (raw-id (cdr id-assoc))
  	       (id (string-to-number raw-id)))
    (push (cons id
  		(lambda (resp)
  		  (-when-let* ((candidates (dart--company-prepare-candidates
  					    resp)))
  		    (funcall callback candidates))))
  	  dart--analysis-completion-callbacks)))

(defun dart--get-completions ( callback)
  "Ask the analysis server for suggestions."
  (dart--analysis-server-send
   "completion.getSuggestions"
   `((file . ,(buffer-file-name))
     (offset . ,(point)))
   (lambda (response)
     (dart--register-for-completion-event response callback)))

  ;; Company mode expects the candidates to be ready as soon as this routine
  ;; exits. The analysis server sends the candidates information via a
  ;; subsequent notification. Wait 75ms to receive and process that
  ;; notification.
  (sleep-for 0 75))


(defun dart--completion-meta (s)
  "Show summary documentation."
  (get-text-property 0 'docSummary s))

(defun dart--completion-doc (s)
  "Show complete documentation in the help buffer."
  (--when-let (get-text-property 0 'docComplete s)
    (company-doc-buffer it)))

;;;###autoload
(defun company-dart (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-dart))
    (prefix (if (and (derived-mode-p 'dart-mode)
		     (save-excursion (eq (char-before) 46)))
		"" nil))
    (candidates
     (cons :async
           (lambda (callback)
	     (dart--get-completions callback))))
    (duplicates t)
    (doc-buffer (dart--completion-doc arg))
    (meta (dart--completion-meta arg))))

(provide 'company-dart)
