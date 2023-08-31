;;; flymake-guile.el --- Guile flymake backend -*- lexical-binding: t -*-

;; Copyright (c) 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>

;; Author: Distopico <distopico@riseup.net>
;; Package-Requires: ((emacs "26.1"))
;; Keywords: language, tools
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Flymake backend for GNU Guile using `guild' compile.
;;
;; Usage:
;;   (add-hook 'scheme-mode-hook 'flymake-guile)

;;; Code:

(require 'flymake-quickdef)

(defgroup flycheck-guile nil
  "GNU Guile Flymake backend."
  :prefix "flymake-guile-"
  :group 'flymake)

(defcustom flymake-guile-guild-binary "guild"
  "Name of the Guile `guild' executable."
  :type 'string
  :group 'flymake-guile)

(defcustom flymake-guile-guild-args nil
  "Additional arguments for `guild' compile command."
  :type 'string
  :group 'flymake-guile)

(defcustom flymake-guile-warnings '("3")
  "A list of warnings to enable for `guild compile'.

The value of this variable could be an list string of warning types
or an warning level.

The list of supported warning types/levels can be found by running
`guild compile -W help'."
  :type  '(string)
  :group 'flymake-guile)

(defvar geiser-guile-load-path)

(defvar geiser-repl-current-project-function)

(defvar geiser-repl-add-project-paths)

(defvar flymake-guile--diag-lnum-rx ":\\([[:digit:]]+\\):\\([[:digit:]]+\\):\s")

(defvar flymake-guile--fix-col-rule-rx "unbound variable")

(defun flymake-guile--project-path ()
  "Determine project paths from geiser configuration."
  (when-let ((geiser-repl-add-project-paths)
	   (root (funcall geiser-repl-current-project-function)))
      (mapcar (lambda (path)
		(expand-file-name path root))
	      (cond ((eq t geiser-repl-add-project-paths)
		     '("."))
		    ((listp geiser-repl-add-project-paths)
		     geiser-repl-add-project-paths)))))

(defun flymake-guile--load-path-args ()
  "Build the `load-path' arguments for `guild compile'."
  (mapcan (lambda (path)
	    (list "-L" path))
	  (append (flymake-guile--project-path)
		  geiser-guile-load-path)))

(defun flymake-guile--warning-level-args ()
  "Build the `warning' arguments for `guild compile'."
  (if (listp flymake-guile-warnings)
      (mapcan (lambda (rule)
		(list "-W" rule))
	      flymake-guile-warnings)
    (error "`flymake-guile-warnings' must be a list of: e.g '(\"unused-module\")")))

(defun flymake-guile--get-diagnostic (stack-msg stack-lnum stack-cnum stack-file source)
  "Get the diagnostic line and message for the `SOURCE'.
If the diagnostic has additional information for the source file
extract if otherwise use the `STACK-MSG' and `STACK-LNUM'/`STACK-CNUM'.
Also verify if the `STACK-FILE' and the source file are te same."
  (let* ((text stack-msg)
	(lnum stack-lnum)
	(cnum stack-cnum)
	(origin-file (file-name-nondirectory stack-file))
	(source-file (file-name-nondirectory (buffer-file-name source)))
	(maybe-stack (string-match
		      (concat
		       source-file
		       flymake-guile--diag-lnum-rx
		       "\\(.*\\)")
		      text))
	;; origin-file/stack-file could be an
	;; internal guile file.
	(file (cond (maybe-stack source-file)
		    (t origin-file))))
    (when maybe-stack
      (setq lnum (match-string 1 text))
      (setq cnum (match-string 2 text))
      (setq text (match-string 3 text)))
    (when (not (string= source-file file))
      ;; Set the line number to zero when the report comes from
      ;; another file different from the source, this usually
      ;; means that there is an error parsing or and syntax's error
      ;; and guile Backtrace is reporting lines from internal code
      ;; e.g "ice-9/boot-9".
      (setq lnum "0")
      (setq cnum "0"))
    (cons (cons (string-to-number lnum)
		(let ((col (string-to-number cnum)))
		  (if (and (> col 0)
			   ;; The column in this type of errors are not
			   ;; consistent And will mark all the lines in a
			   ;; multi-line definition.
			   (string-match-p flymake-guile--fix-col-rule-rx text))
		      (+ col 1)
		    (- col 1))))
	  text)))

(flymake-quickdef-backend flymake-guile-backend
  :pre-let ((guild-exec (executable-find flymake-guile-guild-binary)))
  :pre-check (unless guild-exec (error "Cannot find guild executable"))
  :write-type 'file
  :proc-form (append
	      (list guild-exec
		"compile"
		"-O0")
	      (flymake-guile--warning-level-args)
	      (flymake-guile--load-path-args)
	      flymake-guile-guild-args
	      (list fmqd-temp-file))
  :search-regexp (concat
		  "\\(.*\\)"
		  flymake-guile--diag-lnum-rx
		  "\\(.*\\):[[:space:]]?"
		  "\\(Syntax error:[[:space:]].*\\|.*\\)$")
  :prep-diagnostic
  (let* ((stack_file (match-string 1))
	 (stack_lnum (match-string 2))
	 (stack_cnum (match-string 3))
	 (severity (match-string 4))
	 (stack_msg (match-string 5))
	 (report (flymake-guile--get-diagnostic
		  stack_msg
		  stack_lnum
		  stack_cnum
		  stack_file
		  fmqd-source))
	 (lnum (car (car report)))
	 (cnum (cdr (car report)))
	 (text (cdr report))
	 (pos (flymake-diag-region fmqd-source lnum cnum))
	 (beg (car pos))
	 (end (cdr pos))
	 (type (cond
		((string= severity "warning") :warning)
		((string= severity "In procedure raise-exception") :error)
		(t :note)))
	 (msg (string-trim text)))
    (list fmqd-source beg end type msg)))

;;;###autoload
(defun flymake-guile ()
  "Add the `guile' backend into Flymake's diagnostic list."
  (add-hook 'flymake-diagnostic-functions 'flymake-guile-backend nil t))

(provide 'flymake-guile)
;;; flymake-guile.el ends here
