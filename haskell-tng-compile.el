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
(require 'subr-x)

(require 'haskell-tng-util)

;; TODO set compilation-directory when opening the file
;; TODO set compilation-environment to include TASTY envvars
;; TODO support long running (ghcid) compile buffers
;; TODO generic flycheck integration https://emacs.stackexchange.com/questions/51894

(defvar haskell-tng-compilation-error-regexp-alist
  (let ((file '(: (group (+ any) ".hs")))
        (num '(: (group (+ digit))))
        (err '(: ": " (group "error") ":"))
        (war '(: ": " (group "warning") ":")))
    `(;; ghc errors / warnings (including -ferror-spans)
      (,(rx-to-string `(: bol ,file ":" ,num ":" ,num (? "-" ,num) ,err))
       1 2 (3 . 4) 2 1 (5 'compilation-error))
      (,(rx-to-string `(: bol ,file ":" ,num ":" ,num (? "-" ,num) ,war))
       1 2 (3 . 4) 1 1 (5 'compilation-warning))
      (,(rx-to-string `(: bol ,file ":(" ,num "," ,num ")-(" ,num "," ,num ")" ,err))
       1 (2 . 4) (3 . 5) 2 1 (6 'compilation-error))
      (,(rx-to-string `(: bol ,file ":(" ,num "," ,num ")-(" ,num "," ,num ")" ,war))
       1 (2 . 4) (3 . 5) 1 1 (6 'compilation-warning))

      ;; tasty / hspec and miscellaneous ghc extra info
      (,(rx-to-string
         `(: bol (? (+ space) "error, called at") (+ space) (? "(") ,file ":" ,num ":" ,num (? "-" ,num ":")))
       1 2 (3 . 4) 2 1)
      (,(rx-to-string
         `(: bol (? (+ space) "error, called at") (+ space) (? "(") ,file ":(" ,num "," ,num ")-(" ,num "," ,num ")"))
       1 (2 . 4) (3 . 5) 2 1)

      ;; ghc information.
      (,(rx-to-string
         ;; Finds .hs references if `source-paths' are emitted
         `(: bol "[" (* space) ,num (+ space) "of" (+ space) ,num "] Compiling "
             (group (+ (not (syntax whitespace))))
             (* space) "(" (* space) ,file))
       4 nil nil 0 4)
      (,(rx-to-string
         ;; Highlights compilation progress
         `(: bol "[" (* space) ,num (+ space) "of" (+ space) ,num "] Compiling "
             (group (+ (not (syntax whitespace)))) (group word-end)))
       nil nil nil 0 4 (1 'compilation-info) (2 'compilation-info) (3 'compilation-info))
      ))
  "The `compilation-error-regexp-alist' for `haskell-tng'.")

(defvar haskell-tng--compile-history
  ;; Prefer --enable-tests due to
  ;; https://github.com/haskell/cabal/issues/6114
  '("cabal v2-build -O0 --enable-tests "
    "cabal v2-run -O0 --enable-tests tasty -- "))
(defvar-local haskell-tng--compile-command nil)
(defvar-local haskell-tng--compile-alt "cabal v2-clean")

(defvar haskell-tng--compile-dominating-file
  (rx (| "cabal.project" "cabal.project.local" "cabal.project.freeze"
         (: (+ any) ".cabal")
         "package.yaml" "stack.yaml")))

(defun haskell-tng-compile (&optional edit-command)
  "`compile' specialised to Haskell:

1. Runs the command from the cabal project directory.
2. ghc info, warning and error detection.

First use in a buffer or calling with a prefix will prompt for a
command, otherwise the last command is used.

The command history is global across all Haskell files.

A universal argument will invoke `haskell-tng--compile-alt', which
will cause the subsequent call to prompt."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let* ((last haskell-tng--compile-command)
         (command (pcase edit-command
                    ((and 'nil (guard last)) last)
                    ('-  haskell-tng--compile-alt)
                    (_ (read-shell-command
                        "Compile command: "
                        (or last (car haskell-tng--compile-history))
                        ;; TODO haskell-tng--compile-command should always be
                        ;;      first in the prompted history, even if another
                        ;;      command was used elsewhere. Might require
                        ;;      mutating / reordering the global history here.
                        '(haskell-tng--compile-history . 1))))))
    (setq haskell-tng--compile-command
          (unless (equal command haskell-tng--compile-alt) command))

    (let ((default-directory
            (or
             (haskell-tng--util-locate-dominating-file
              haskell-tng--compile-dominating-file)
             default-directory)))
      (compilation-start
       command
       'haskell-tng-compilation-mode
       ;; TODO name the compilation buffer
       ))))

(defun haskell-tng--compile-ansi-color ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode haskell-tng-compilation-mode "haskell-tng-compilation"
  ;; TODO add a hook to detect ghcid recompiles and clear the buffer
  (add-hook 'compilation-filter-hook
            'haskell-tng--compile-ansi-color nil t))

(provide 'haskell-tng-compile)
;;; haskell-tng-compile.el ends here
