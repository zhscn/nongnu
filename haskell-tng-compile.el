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

;; TODO prettify-symbol rules for home dirs and project dirs, etc
;; TODO set compilation-directory when opening the file
;; TODO set compilation-environment to include TASTY envvars

(defvar haskell-tng-compile-error-regexp-alist
  "The `compilation-error-regexp-alist' for `haskell-tng'."
  ;; FIXME error-regexp-alist
  nil
  )

(defvar haskell-tng-compile:history '("cabal v2-build -O0"))
(defvar-local haskell-tng-compile:command nil)
(defvar-local haskell-tng-compile:alt "cabal v2-clean")

(defun haskell-tng-compile (&optional edit-command)
  "`compile' specialised to Haskell:

1. Runs the command from the cabal project directory.
2. ghc info, warning and error detection.

First use in a buffer or calling with a prefix will prompt for a
command, otherwise the last command is used.

The command history is global across all Haskell files.

A universal argument will invoke `haskell-tng-compile:alt'."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((last haskell-tng-compile:command)
         (command (pcase edit-command
                    ((and 'nil (guard last)) last)
                    ('-  haskell-tng-compile:alt)
                    (_ (read-shell-command
                        "Compile command: "
                        (or last (car haskell-tng-compile:history))
                        '(haskell-tng-compile:history . 1))))))
    (setq-local haskell-tng-compile:command command)

    (when-let (default-directory
                (haskell-tng:locate-dominating-file
                 (rx (| "cabal.project" "cabal.project.local" "cabal.project.freeze"
                        (: (+ any) ".cabal")))))
      (compilation-start
       command
       'haskell-tng-compilation-mode
       ;; TODO name the compilation buffer
       ))))

(defun haskell-tng-compile:ansi-color ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode haskell-tng-compilation-mode "haskell-tng-compilation"
  (add-hook 'compilation-filter-hook
            'haskell-tng-compile:ansi-color nil t))

(provide 'haskell-tng-compile)
;;; haskell-tng-compile.el ends here
