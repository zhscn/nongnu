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

(require 'haskell-tng-compile)

;; TODO automatically installing hsinspect under hsinspect-ghc-X

(defcustom haskell-tng-hsinspect
  ;; TODO https://github.com/haskell/cabal/issues/6182
  ;;
  ;; all flags (optimisations, compiler, tests, etc) must match what the user
  ;; typed or else local packages aren't visible.
  ;;'("cabal" "v2-exec" "-v0" "-O0" "--enable-tests" "--")
  '() ;; best to use environment files, less fragile
  "Launch command for the `hsinspect' external tool."
  :type 'listp
  :group 'haskell-tng)

(defvar-local haskell-tng-hsinspect-langexts nil)
;; TODO improve the validity checker
;;;###autoload
(put 'haskell-tng-hsinspect-langexts 'safe-local-variable #'listp)

;;;###autoload
(defun haskell-tng-fqn-at-point ()
  "Consult the imports in scope and display the fully qualified
name of the symbol at point in the minibuffer."
  (interactive) ;; TODO prefix should copy to kill ring
  (if-let* ((sym (symbol-name (symbol-at-point)))
            (found (seq-find
                    (lambda (names) (member sym names))
                    (haskell-tng--hsinspect-imports))))
      ;; TODO multiple hits
      (message "%s" (car (last found)))
    (message "<not imported>")))

;; FIXME implement the `.hsinspect.env' hack and document the workflow

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
    (let ((default-directory
            (or
             (haskell-tng--util-locate-dominating-file
              haskell-tng--compile-dominating-file)
             default-directory)))
      (if (/= 0 (apply
                 #'call-process
                 ;; TODO launching the correct hsinspect-ghc-X version
                 ;; TODO is there a way to pipe into a string not a buffer?
                 ;; TODO async
                 "hsinspect"
                 nil "*hsinspect*" nil
                 (append `("imports" ,buffer-file-name)
                         haskell-tng-hsinspect-langexts)))
          (user-error "`hsinspect' failed. See the *hsinspect* buffer for more information.")
        (setq haskell-tng--hsinspect-imports
              (with-current-buffer "*hsinspect*"
                (goto-char (point-min))
                (re-search-forward (rx bol "(") nil t) ;; sometimes there is junk from the launcher
                (goto-char (match-beginning 0))
                (or (ignore-errors (read (current-buffer))) t)))))))

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
