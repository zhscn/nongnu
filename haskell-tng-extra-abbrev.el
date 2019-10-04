;;; haskell-tng-extra-abbrev.el --- abbrev-mode integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Some useful abbrev-mode expansions.
;;
;;; Code:

(require 'abbrev)
(require 'skeleton)
(require 'haskell-tng-mode)

;; TODO a macro to easily define an abbrev and skeleton?

(abbrev-table-put
 haskell-tng-mode-abbrev-table
 :regexp (rx (or bol space) ;; don't fire for \case
             (submatch (+ (not space))) point))

(defun haskell-tng--abbrev-expand-p ()
  "abbrevs should not expand in strings and comments."
  (not (nth 8 (syntax-ppss))))

(define-skeleton haskell-tng--skeleton-case-of
  "case...of boilerplate"
  nil "case " _ " of")
(define-abbrev
  haskell-tng-mode-abbrev-table
  "case" "" #'haskell-tng--skeleton-case-of
  :system t
  :case-fixed t
  :enable-function #'haskell-tng--abbrev-expand-p)

(define-skeleton haskell-tng--skeleton-if-then-else
  "if...then...else boilerplate"
  nil "if " _ " then else")
(define-abbrev
  haskell-tng-mode-abbrev-table
  "if" "" #'haskell-tng--skeleton-if-then-else
  :system t
  :case-fixed t
  :enable-function #'haskell-tng--abbrev-expand-p)

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (abbrev-mode 1)))

(provide 'haskell-tng-extra-abbrev)
;;; haskell-tng-extra-abbrev.el ends here
