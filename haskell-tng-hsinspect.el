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
  (if-let* ((sym (symbol-name (symbol-at-point)))
            (found (seq-find
                    (lambda (names) (member sym (seq-map #'cdr names)))
                    (haskell-tng--hsinspect-imports))))
      ;; TODO multiple hits
      ;; TODO feedback when hsinspect is broken
      (message "%s" (car (last found)))
    (if (eq t haskell-tng--hsinspect-imports)
        (error "hsinspect is not available")
      (message "<not imported>"))))

(defvar haskell-tng-hsinspect
  ;; NOTE in order for this hack to work, the user needs to have setup a
  ;; cabal.project.local that contains their default options (optimisations,
  ;; enabling tests, etc) otherwise it will (at best) invalidate the cache and
  ;; (at worst) not find local projects.
  (expand-file-name
   "cabal-ghcflags.sh"
   (when load-file-name
     (file-name-directory load-file-name))))
;;;###autoload
(defun haskell-tng-hsinspect ()
  "Required (for now) to initialise a project for use with `hsinspect'.
Only needs to be performed once every time the dependencies
change."
  (interactive)
  (when-let ((default-directory
               (or
                (haskell-tng--util-locate-dominating-file
                 haskell-tng--compile-dominating-project)
                (haskell-tng--util-locate-dominating-file
                 haskell-tng--compile-dominating-package))))
    (async-shell-command haskell-tng-hsinspect)))

(defun haskell-tng--hsinspect-ghcflags ()
  ;; https://github.com/haskell/cabal/issues/6203
  "Obtain the ghc flags for the current buffer"
  (if-let (default-directory (locate-dominating-file default-directory ".ghc.flags"))
      (seq-map
       ;; hsinspect works best if we trick the compiler into thinking that the
       ;; file we are inspecting is independent of the current unit.
       (lambda (e) (if (equal e "-this-unit-id") "-package-id" e))
       (with-temp-buffer
         (insert-file-contents (expand-file-name ".ghc.flags"))
         (split-string
          (buffer-substring-no-properties (point-min) (point-max)))))
    (user-error "could not find `.ghc.flags'. Run `M-x haskell-tng-hsinspect'")))

(defun haskell-tng--hsinspect-ghc ()
  "Obtain the version of hsinspect that matches the project's compiler."
  (if-let (default-directory (locate-dominating-file default-directory ".ghc.version"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".ghc.version"))
        (concat
         "hsinspect-ghc-"
         (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (user-error "could not find `.ghc.version'. Run `M-x haskell-tng-hsinspect'")))

;; TODO invalidate cache when imports section has changed
;; TODO is there a way to tell Emacs not to render this in `C-h v'?
;;      (suggestion is to advise around describe-key)
(defvar-local haskell-tng--hsinspect-imports nil
  "Cache for the last `imports' call for this buffer.
t means the process failed.")
(defun haskell-tng--hsinspect-imports ()
  (if haskell-tng--hsinspect-imports
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
                 (append `("imports" ,buffer-file-name "--") ghcflags '("-w")))))
          (user-error "`hsinspect' failed. See the *hsinspect* buffer for more information")
        (setq haskell-tng--hsinspect-imports
              (with-current-buffer "*hsinspect*"
                (goto-char (point-max))
                (backward-sexp)
                (or (ignore-errors (read (current-buffer))) t)))))))

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
