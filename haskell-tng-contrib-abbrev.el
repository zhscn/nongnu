;;; haskell-tng-contrib-abbrev.el --- abbrev-mode integration -*- lexical-binding: t -*-

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

(define-skeleton haskell-tng--skeleton-case-of
  "case...of boilerplate"
  nil "case " _ " of")

(define-abbrev
  haskell-tng-mode-abbrev-table
  "case" "" #'haskell-tng--skeleton-case-of)

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   ;; TODO disable the request to save the abbrev table
   (abbrev-mode 1)))

(provide 'haskell-tng-contrib-abbrev)
;;; haskell-tng-contrib-abbrev.el ends here
