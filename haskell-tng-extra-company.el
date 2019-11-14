;;; haskell-tng-extra-company.el --- company mode integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Integration with `company-mode' completions for
;;
;;  1. keywords
;;  2. `hsinspect' contextual completions
;;
;;; Code:

(require 'company)
(require 'company-dabbrev-code)
(require 'company-files)
(require 'company-keywords)
(require 'smie)

(require 'haskell-tng-hsinspect)
(require 'haskell-tng-rx)

;; TODO completions for modules in imports
;; TODO completions for explicits in imports

(defcustom haskell-tng-company-backends
  '(company-files
    (haskell-tng--company-hsinspect company-dabbrev-code company-keywords))
  "The company mode backends to use for haskell files"
  :type 'listp
  :group 'haskell-tng)

(defun haskell-tng--company-hsinspect (command &optional arg &rest ignored)
  "`company-mode' backend for `hsinspect'."
  (interactive (list 'interactive))
  ;;(message "TNG asked %S" command)
  (pcase command
    ;; NOTE: init can run before local variables are set, so we don't want that
    ('prefix (and (not (company-in-string-or-comment))
                  (or (looking-at (rx symbol-end))
                      (eq (char-before) ?.))
                  (buffer-substring-no-properties
                   (save-excursion
                     (funcall smie-backward-token-function)
                     (let ((lbp (line-beginning-position)))
                       ;; include FQNs, workaround ungreedy backwards regexp
                       (while (looking-back haskell-tng--rx-c-qual lbp 't)
                         ;; TODO try regexp without while
                         (goto-char (match-beginning 0))))
                     (point))
                   (point))))
    ('candidates
     ;;(message "TNG asked with %S" arg)
     (seq-mapcat
      (lambda (names) (all-completions arg (seq-map #'cdr names)))
      (haskell-tng--hsinspect-imports 'no-work nil)))
    ('sorted t)
    ('duplicates t)
    ;; TODO 'meta return the FQN
    ;; TODO 'location (file, line)

    ;; TODO 'annotation when there are multiple choices, with support to either
    ;; qualify the symbol or to add the relevant hiding clauses to the import
    ;; section.
    ))

(add-to-list
 'company-keywords-alist
 `(haskell-tng-mode ,@haskell-tng--keywords))

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (setq-local company-backends haskell-tng-company-backends)
   (company-mode 1)))

(provide 'haskell-tng-extra-company)
;;; haskell-tng-extra-company.el ends here
