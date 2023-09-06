;;; flymake-guile.el --- Guile flymake backend -*- lexical-binding: t -*-

;; Copyright (c) 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>

;; Author: Distopico <distopico@riseup.net>
;; Package-Requires: ((emacs "26.1") (flymake "1.2.1"))
;; Keywords: language, tools
;; Version: 0.5

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
;;
;;    Emacs 29 and later:
;;
;;   (use-package flymake-guile
;;     :ensure t
;;     :hook (scheme-mode-hook . flymake-guile))

;;; Code:

(defgroup flycheck-guile nil
  "GNU Guile Flymake backend."
  :prefix "flymake-guile-"
  :group 'flymake)

(defcustom flymake-guile-guild-executable "guild"
  "Name of the Guile `guild' executable."
  :type 'string
  :group 'flymake-guile)

(defcustom flymake-guile-guild-args ""
  "Additional arguments for `guild' compile command."
  :type 'string
  :group 'flymake-guile)

(defcustom flymake-guile-warnings '("3")
  "A list of warnings to enable for `guild compile'.

The value of this variable could be an list string of warning types
or an warning level.

The list of supported warning types/levels can be found by running
`guild compile -W help'."
  :type  '(repeat string)
  :group 'flymake-guile)

(defvar geiser-guile-load-path)

(defvar geiser-repl-current-project-function)

(defvar geiser-repl-add-project-paths)

(defvar-local flymake-guile--proc nil)

(defvar-local flymake-guile--diag-lnum-rx
  ":\\([[:digit:]]+\\):\\([[:digit:]]+\\):\s")

(defvar-local flymake-guile--fix-col-rule
  '(("unbound variable" . +1)
    ("unused variable" . +2)
    ("unused local top" . +2)))

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
    (error
     "`flymake-guile-warnings' must be a list of: e.g '(\"unused-module\")")))

(defun flymake-guile--type-diagnostic (severity)
  "Get diagnostic type based on `SEVERITY' string from `guild'."
  (cond ((string= severity "warning") :warning)
	((string= severity "In procedure raise-exception") :error)
	(t :note)))

(defun flymake-guile--get-col-fix (text)
  "Get diagnostic column position fix base on `TEXT'.
The column of some types of errors/warning are not consistent and
will mark all the lines in a multi-line definition so this try to
fix and getter a better position."
  (catch 'rule-match
    (dolist (col-rule flymake-guile--fix-col-rule)
      (when (string-match-p (car col-rule) text)
	(throw 'rule-match (cdr col-rule))))))

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
    (cons (let ((line (string-to-number lnum))
		(col (string-to-number cnum))
		(col-fix (flymake-guile--get-col-fix text)))
	    (cons line
		  (if (and col-fix
			   (or (> col 0)
			       (> line 0)))
		      ;; Try to report in a better position
		      (+ col col-fix)
		    col)))
	  text)))

(defun flymake-guile--prep-diagnostic (source proc)
  "Prepare and make `flymake' diagnostic based in the `SOURCE'.
`PROC' process sentinel is used to add log context."
  (let ((diags nil))
    (while (search-forward-regexp
	    (concat
	     "\\(.*\\)"
	     flymake-guile--diag-lnum-rx
	     "\\(.*\\):[[:space:]]?"
	     "\\(Syntax error:[[:space:]].*\\|.*\\)$")
	    nil t)
      (save-match-data
	(save-excursion
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
			  source))
		 (lnum (caar report))
		 (cnum (cdar report))
		 (text (cdr report))
		 (pos (flymake-diag-region source lnum cnum))
		 (diag-beg (car pos))
		 (diag-end (cdr pos))
		 (diag-type (flymake-guile--type-diagnostic severity))
		 (msg (string-trim text)))
	    ;; Prevent diagnostics with invalid values
	    (if (and (integer-or-marker-p diag-beg)
		     (integer-or-marker-p diag-end))
		(push (flymake-make-diagnostic source diag-beg
					       diag-end diag-type msg)
		      diags)
	      (with-current-buffer source
		(flymake-log :error "Invalid buffer position %s or %s in %s"
			     diag-beg diag-end proc)))))))
    diags))

(defun flymake-guile--make-sentinel (report-fn source temp-dir)
  "Generate a process sentinel reporting to `REPORT-FN'.
The argument `SOURCE' and `TEMP-DIR' are respectively used to pass
the buffer containing the source code being checked and the
temporary director generated for the checking."
  (lambda (proc _event)
    (unless (process-live-p proc)
      (unwind-protect
	  (if (eq proc (buffer-local-value 'flymake-guile--proc source))
	      (with-current-buffer source
		(save-restriction
		  (widen)
		  (with-current-buffer (process-buffer proc)
		    (goto-char (point-min))
		    (save-match-data
		      (let ((diags (flymake-guile--prep-diagnostic source proc)))
			(funcall report-fn (nreverse diags)))))))
	    (flymake-log :warning "Canceling obsolete check %s" proc))
	(delete-directory temp-dir t)
	(kill-buffer (process-buffer proc))))))

(defun flymake-guile-backend (report-fn &rest _args)
  "GNU Guile backend for Flymake using `guild'.
For the interpretation of `REPORT-FN', consult the Info
node `(flymake) Backend functions'."
  (let* ((guild-exec (or (executable-find flymake-guile-guild-executable)
			 (error "Cannot find guild executable")))
	 (source (current-buffer))
	 (temp-dir (make-temp-file "flymake-guile-" t))
	 (temp-file (expand-file-name (file-name-nondirectory
				       (or (buffer-file-name)
					   (buffer-name)))
				      temp-dir)))
    (when (process-live-p flymake-guile--proc)
      (kill-process flymake-guile--proc))
    (save-restriction
      (widen)
      (write-region nil nil temp-file nil 'silent)
      (setq flymake-guile--proc
	    (make-process
	     :name "flymake-guile-backend-flymake"
	     :noquery t
	     :connection-type 'pipe
	     :buffer (generate-new-buffer " *flymake-guile-backend-flymake*")
	     :sentinel (flymake-guile--make-sentinel report-fn source temp-dir)
	     :command
	     (append (list guild-exec "compile" "-O0")
		     (flymake-guile--warning-level-args)
		     (flymake-guile--load-path-args)
		     flymake-guile-guild-args
		     (list temp-file)))))))

;;;###autoload
(defun flymake-guile ()
  "Add the `guile' backend into Flymake's diagnostic list."
  (add-hook 'flymake-diagnostic-functions 'flymake-guile-backend nil t))

(provide 'flymake-guile)
;;; flymake-guile.el ends here
