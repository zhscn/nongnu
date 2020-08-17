;;; geiser-stklos.el -- STklos Scheme implementation of the geiser protocols

;; Author: Jeronimo Pellegrini <j_p@aleph0.info>
;; Maintainer:
;; Keywords: languages, stklos, scheme, geiser
;; Homepage: 
;; Package-Requires: ((emacs "24.4") (geiser-core "1.0"))
;; SPDX-License-Identifier: BSD-3-Clause
;; Version: 1.0


;;; Code:

(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'compile)
(require 'info-look)

(eval-when-compile (require 'cl-lib))



;;; Customization:

(defgroup geiser-stklos
  nil
  "Customization for Geiser's STklos Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-stklos-binary
    "stklos"
  "Name to use to call the STklos executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-stklos)


(geiser-custom--defcustom geiser-stklos-extra-command-line-parameters
    '()
  "Additional parameters to supply to the STklos binary."
  :type '(repeat string)
  :group 'geiser-stklos)

(geiser-custom--defcustom geiser-stklos-extra-keywords
    nil
  "Extra keywords highlighted in STklos scheme buffers."
  :type '(repeat string)
  :group 'geiser-stklos)

;; FIXME: should ask STklos,
;; (read-case-sensitive) returns the proper value, but
;; this should be done during REPL startup.
;; And the value can be changed later, because read-case-sensitive
;; is a parameter object!
(geiser-custom--defcustom geiser-stklos-case-sensitive
    t
  "Non-nil means keyword highlighting is case-sensitive. You need
to restart Geiser in order for it to see you've changed this
option."
  :type 'boolean
  :group 'geiser-stklos)



;;; REPL support:

(defvar geiser-stklos-scheme-dir
  (expand-file-name "" (file-name-directory load-file-name))
  "Directory where the STklos scheme geiser modules are installed.")

;; returns the name of the executable.
(defun geiser-stklos--binary ()
  (if (listp geiser-stklos-binary)
      (car geiser-stklos-binary)
    geiser-stklos-binary))

;; a list of strings to be passed to STklos
(defun geiser-stklos--parameters ()
  "Return a list with all parameters needed to start STklos Scheme.
This function uses `geiser-stklos-init-file' if it exists."
  `(,@geiser-stklos-extra-command-line-parameters
    "-i" ;; do not use ANSI color codes
    "-n" ;; do not use the line editor
    "-l" ,(expand-file-name "geiser.stk" geiser-stklos-scheme-dir)))

;; STklos' prompt is  "MODULE> ". The regexp is "[^>]*> ".
;; Not perfect, because if a module has a ">" sign
;; in its name, things break...
(defconst geiser-stklos--prompt-regexp "[^>]*> ")


;;; Evaluation support:

;; Translates symbols into Scheme procedure calls from
;; geiser.stk :
(defun geiser-stklos--geiser-procedure (proc &rest args)
  (cl-case proc
    ((eval compile)
     (let ((form (mapconcat 'identity (cdr args) " "))
           (module (cond ((string-equal "'()" (car args))
                          "'()")
                         ((and (car args))
                          (concat "'" (car args)))
                         (t
                          "#f"))))
       (format "((in-module GEISER geiser:eval) %s '%s)" module form)))
    ((load-file compile-file)
     (format "((in-module GEISER geiser:load-file) %s)" (car args)))
    ((no-values)
     "((in-module GEISER geiser:no-values))")
    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

;;; Modules

;; Regular expression used to try to guess which module
;; the current file is associated to.
(defconst geiser-stklos--module-re
  "(define-module +\\([^) ]+\\)")


;; from the start point, which must be an opening
;; ( or [, find its closing match and return its
;; position, or the end of buffer position if a
;; closing match is not found.
(defun find-close-par (&optional start-point)
  (interactive)
  (let ((start (if (null start-point)
                   (point)
                 start-point))
        (opening '( ?\[ ?\( ))
        (closing '( ?\] ?\) )))
    (when (not (member (char-after start)
                       opening))
      (error "find-close-par: not ( or ["))
    (let ((stack (list (char-after start)))
          (p (+ 1 start)))
      (while (not (or (= p (point-max))
                      (null stack)))
        (let ((c (char-after p)))
          (cond ((member c closing)
                 (pop stack))
                ((member c opening)
                 (push c stack))))                
        (incf p))
      p)))

;; find which module should be used for the position where the
;; cursor is.
;; if the user is editing text inside a module definition -- which is
;; between "(define-module " and its closing parenthesis, then
;; the current module should be taken as that one, so defines and sets
;; will be done inside that module.
(defun geiser-stklos--get-module (&optional module)
  (cond ((null module)
         (let ((here (point)))
           (save-excursion
             ;; goto end of line, so if we are already exacly on the module
             ;; definition, we'll be able to find it searching backwards:
             (end-of-line)
             ;; module defined BEFORE point:
             (let ((module-begin (re-search-backward geiser-stklos--module-re nil t)))
               (if module-begin
                   ;; and we're not AFTER it was closed:
                   (let ((module-end (find-close-par module-begin)))
                     (if (< here module-end)
                         (geiser-stklos--get-module (match-string-no-properties 1))
                       :f))
                 :f)))))
        ((symbolp module) (geiser-stklos--get-module (symbol-name module))) ; try again, as string
        ((listp module) module)
        ((stringp module)
         (condition-case e
             (car (geiser-syntax--read-from-string module))
           (progn (message (format "error -> %s" e))
                  (error   :f))))
        (t :f)))


;; string sent to STklos to tell it to enter a module.
(defun geiser-stklos--enter-command (module)
  (format "(select-module %s)" module))


;; Finds the beginning of the symbol around point.
(defun geiser-stklos--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))


;; string sent to STklos to tell it to load a module.
(defun geiser-stklos--import-command (module)
  (format "(require \"%s\")" module))

;; string sent to STklos to tell it to exit.
;; (this could also be ",q"...)
(defun geiser-stklos--exit-command () "(exit 0)")



;;; Error display

(defun geiser-stklos--display-error (module key msg)
  (newline)
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key) msg (not (zerop (length msg)))))


;;; Guessing wether a buffer is a STklos REPL buffer

;; The function (geiser-stklos--guess) tries to
;; ascertain whether a buffer is STklos Scheme.
;; This will possibly fail:
;;
;; - with false negative, if the buffer is running STklos
;; but th euser is in not in the stklos module, AND
;; the user was not in the stklos module recently, so
;; there are no "stklos" strings in the buffer.
;;
;; - with false positive, if the buffer is not a STklos buffer,
;; but there is a string "stklos>" there. I see no way
;; to prevent this.
(defconst geiser-stklos--guess-re
  (regexp-opt '("stklos>")))

(defun geiser-stklos--guess ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-stklos--guess-re nil t)))

;;; REPL startup

;; Minimum version of STklos supported. If a less recent version
;; is used, Geiser will refuse to start.
(defconst geiser-stklos-minimum-version "1.50")

;; this function obtains the version of the STklos binary
;; available.
(defun geiser-stklos--version (binary)
  ;; use SRFI-176!!!
  (cadr (assoc 'version
               (read (shell-command-to-string
                      (concat binary
                              " -e \"(write (version-alist))\"" ))))))


;; Function ran at startup
(defun geiser-stklos--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)))


;; These are symbols that we want to be highlighted in STklos code.
(defconst geiser-stklos-builtin-keywords
  '("assume"
    "fluid-let"
    "dotimes"
    "macro-expand"
    "define-struct"
    "call/ec"
    "with-handler" ))

;; The symbols that are to be highlighted as keywords, besides
;; the standard Scheme ones
(defun geiser-stklos--keywords ()
  (append (geiser-syntax--simple-keywords geiser-stklos-extra-keywords)
          (geiser-syntax--simple-keywords geiser-stklos-builtin-keywords)))


;;; Implementation definition:

(define-geiser-implementation stklos
  (binary                 geiser-stklos--binary)         ; ok
  (arglist                geiser-stklos--parameters)     ; ok
  (version-command        geiser-stklos--version)        ; ok
  (minimum-version        geiser-stklos-minimum-version) ; ok
  (repl-startup           geiser-stklos--startup)        ; ok
  (prompt-regexp          geiser-stklos--prompt-regexp)  ; ok
  (debugger-prompt-regexp nil) ;; no debugger
  (enter-debugger         nil) ;; no debugger
  (marshall-procedure     geiser-stklos--geiser-procedure)
  (find-module            geiser-stklos--get-module)
  (enter-command          geiser-stklos--enter-command)  ; ok
  (exit-command           geiser-stklos--exit-command)   ; ok
  (import-command         geiser-stklos--import-command) ; ok
  (find-symbol-begin      geiser-stklos--symbol-begin)   ; ok
  (display-error          geiser-stklos--display-error)
  ;; (external-help geiser-stklos--manual-look-up) ;; cannot easily search by keyword
  (check-buffer           geiser-stklos--guess)
  (keywords               geiser-stklos--keywords)       ; ok
  (case-sensitive         geiser-stklos-case-sensitive)  ; ok
  (unsupported            '(callers callees))            ; doesn't seem to make any difference?
  )

;; STklos files are .stk, and we may wat to open .scm files with STklos also:
;;
(geiser-impl--add-to-alist 'regexp "\\.scm$" 'stklos t)
(geiser-impl--add-to-alist 'regexp "\\.stk$" 'stklos t)


(provide 'geiser-stklos)
