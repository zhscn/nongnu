;;; haskell-tng-hsinspect.el --- hsinspect integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  `hsinspect' is a companion tool to `haskell-tng' that uses the `ghc' api to
;;  extract semantic information. This file provides an integration layer and
;;  some features.
;;
;;; Code:

(require 'subr-x)

(require 'haskell-tng-compile)

;;;###autoload
(defun haskell-tng-fqn-at-point ()
  "Consult the imports in scope and display the fully qualified
name of the symbol at point in the minibuffer."
  (interactive) ;; TODO prefix should copy to kill ring
  (if-let* ((sym (haskell-tng--hsinspect-symbol-at-point))
            (found (seq-find
                    (lambda (names) (member sym (seq-map #'cdr names)))
                    (haskell-tng--hsinspect-imports))))
      ;; TODO multiple hits
      ;; TODO feedback when hsinspect is broken
      (message "%s" (cdar (last found)))
    (if (eq t haskell-tng--hsinspect-imports)
        (error "hsinspect is not available")
      (message "<not imported>"))))

(defun haskell-tng--hsinspect-symbol-at-point ()
  "A `symbol-at-point' that includes FQN parts."
  (buffer-substring-no-properties
   (save-excursion
     (while ;; WORKAROUND non-greedy matches
         (re-search-backward
          (rx symbol-start (+ (| word (syntax symbol) ".")) point)
          (line-beginning-position)
          t))
     (match-beginning 0))
   (save-excursion
     (re-search-forward
      (rx point (+ (| word (syntax symbol) ".")) symbol-end)
      (line-end-position) t)
     (match-end 0))))

(defun haskell-tng--hsinspect-ghcflags ()
  ;; https://github.com/haskell/cabal/issues/6203
  "Obtain the ghc flags for the current buffer"
  (if-let (default-directory (locate-dominating-file default-directory ".ghc.flags"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".ghc.flags"))
        (split-string
         (buffer-substring-no-properties (point-min) (point-max))))
    (user-error "could not find `.ghc.flags'.")))

;; TODO maybe we should just rely on the build tool being able to launch the
;; correct version of hsinspect, then we can drop the need for .ghc.version
;; files.
(defun haskell-tng--hsinspect-ghc ()
  "Obtain the version of hsinspect that matches the project's compiler."
  (if-let (default-directory (locate-dominating-file default-directory ".ghc.version"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".ghc.version"))
        (concat
         "hsinspect-ghc-"
         (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (user-error "could not find `.ghc.version'.")))

;; TODO invalidate cache when imports section has changed
;; TODO is there a way to tell Emacs not to render this in `C-h v'?
;;      (suggestion is to advise around describe-key)
(defvar-local haskell-tng--hsinspect-imports nil
  "Cache for the last `imports' call for this buffer.
t means the process failed.")
(defun haskell-tng--hsinspect-imports (&optional lookup-only)
  (if (or lookup-only haskell-tng--hsinspect-imports)
      (unless (eq t haskell-tng--hsinspect-imports)
        haskell-tng--hsinspect-imports)
    (setq haskell-tng--hsinspect-imports t) ;; avoid races
    (ignore-errors (kill-buffer "*hsinspect*"))
    (when-let ((ghcflags (haskell-tng--hsinspect-ghcflags))
               ;; default-directory is so that relative paths in ghcflags work
               (default-directory (haskell-tng--util-locate-dominating-file
                                   haskell-tng--compile-dominating-package)))
      (if (/= 0
              (let ((process-environment (cons "GHC_ENVIRONMENT=-" process-environment)))
                (apply
                 #'call-process
                 ;; TODO async
                 (haskell-tng--hsinspect-ghc)
                 nil "*hsinspect*" nil
                 ;; need to disable all warnings
                 (append `("imports" ,buffer-file-name "--") ghcflags))))
          (user-error "`hsinspect' failed. See the *hsinspect* buffer for more information")
        (setq haskell-tng--hsinspect-imports
              (with-current-buffer "*hsinspect*"
                (goto-char (point-max))
                (backward-sexp)
                (or (ignore-errors (read (current-buffer))) t)))))))

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
