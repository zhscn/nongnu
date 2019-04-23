;;; haskell-tng-compile.el --- Batch compilation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  An idiomatic `compilation-mode' batch compilation command that detects
;;  warnings and errors, extracting line numbers, columns and ranges.
;;
;;; Code:

(require 'compile)
(require 'ansi-color)

;; FIXME implement batch compilation
;; TODO prettify-symbol rules for home dirs and project dirs, etc
;; TODO set compilation-directory when opening the file
;; TODO set compilation-environment to include TASTY envvars

(defvar haskell-tng-compile-error-regexp-alist
  "The `compilation-error-regexp-alist' for `haskell-tng'."
  nil
  )

(defun haskell-tng-compile:ansi-color ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode haskell-tng-compilation-mode "haskell-tng-compilation"
  (add-hook 'compilation-filter-hook
            'haskell-tng-compile:ansi-color nil t))

(provide 'haskell-tng-compile)
;;; haskell-tng-compile.el ends here
