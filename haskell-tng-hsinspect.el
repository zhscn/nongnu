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

;; TODO this file needs tests, if not testing calling hsinspect then at least
;; with pre-canned data.

(require 'subr-x)

(require 'popup)
;; FIXME remove the dependency on third party "popup". Unfortunately this is
;; blocked on Emacs shipping with a usable menu and tooltip library.
;; `tooltip-show' and `popup-menu' are mouse centric whereas we need `point'
;; centric. https://emacs.stackexchange.com/questions/53373

(require 'haskell-tng-compile)
(require 'haskell-tng-util)

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
      (popup-tip (format "%s" (cdar (last found))))
    (if (eq t haskell-tng--hsinspect-imports)
        (error "hsinspect is not available")
      (message "<not imported>"))))

;;;###autoload
(defun haskell-tng-import-symbol-at-point ()
  "Import the symbol at point"
  ;; TODO prefix + FQN should mean use unqualified `as' import
  ;; TODO prefix + unqualified should mean to import entire module
  ;; TODO shortlist for FQN imports (no need to calc the index)
  (interactive)
  ;; TODO update the hsinspect-imports cache
  (when-let* ((index (haskell-tng--hsinspect-index))
              (sym (haskell-tng--hsinspect-symbol-at-point)))
    (message "Seaching for '%s' in %s modules" sym (length index))
    (if (string-match (rx bos (group (+ anything)) "." (group (+ (not (any ".")))) eos) sym)
        (let* ((fqn (match-string 1 sym))
               (sym (match-string 2 sym)))
          (when-let (hit (haskell-tng--hsinspect-import-popup index sym))
            (haskell-tng--import-symbol (car hit) fqn)))
      (when-let (hit (haskell-tng--hsinspect-import-popup index sym))
        ;; TODO add parens around operators
        ;; TODO add the type around data constructors (requires hsinspect changes)
        (haskell-tng--import-symbol (car hit) nil (cdr hit))))))

(defun haskell-tng--hsinspect-import-popup (index sym)
  (when-let ((hits (haskell-tng--hsinspect-import-candidates index sym)))
    ;; TODO special case one hit
    (when-let* ((entries (mapcar 'car hits)) ;; TODO include function name
                (selected (popup-menu* entries)))
      (seq-find (lambda (el) (equal (car el) selected)) hits))))

(defun haskell-tng--hsinspect-import-candidates (index sym)
  "Return a list of (module . symbol)"
  ;; TODO threading/do syntax
  ;; TODO alist variable binding like RecordWildcards
  (seq-mapcat
   (lambda (pkg-entry)
     (let ((modules (alist-get 'modules pkg-entry)))
       (seq-mapcat
        (lambda (module-entry)
          (let ((module (alist-get 'module module-entry))
                (ids (alist-get 'ids module-entry)))
            ;;(message "MODULE= %s" module)
            (seq-mapcat
             (lambda (entry)
               (let ((name (alist-get 'name entry)))
                 (when (equal name sym) `(,(cons module name)))))
             ids)))
        modules)))
   index))

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

;; TODO invalidate cache when imports section has changed
;; FIXME cache per file (timestamp based, for optimal browsing)
(defvar-local haskell-tng--hsinspect-imports nil
  "Cache for the last `imports' call for this buffer.
t means the process failed.")
(defun haskell-tng--hsinspect-imports (&optional lookup-only)
  (if (or lookup-only haskell-tng--hsinspect-imports)
      (unless (eq t haskell-tng--hsinspect-imports)
        haskell-tng--hsinspect-imports)
    (setq haskell-tng--hsinspect-imports t) ;; avoid races
    (setq haskell-tng--hsinspect-imports
          (haskell-tng--hsinspect "imports" buffer-file-name))))

;; FIXME this can be more efficiently cached alongside the .ghc.flags file, not per source file
;; (it's also fast to load so maybe persist it in a cache dir and check timestamps)
(defvar-local haskell-tng--hsinspect-index nil)
(defun haskell-tng--hsinspect-index (&optional lookup-only)
  (if (or lookup-only haskell-tng--hsinspect-index)
      (unless (eq t haskell-tng--hsinspect-index)
        haskell-tng--hsinspect-index)
    (setq haskell-tng--hsinspect-index t) ;; avoid races
    (setq haskell-tng--hsinspect-index
          (haskell-tng--hsinspect "index"))))

;; FIXME cache per project (or package at least)
(defvar-local haskell-tng--hsinspect-exe nil)
(defvar haskell-tng--hsinspect-which-hsinspect
  "cabal exec -v0 which -- hsinspect")
(defun haskell-tng--hsinspect-exe ()
  "The binary to use for `hsinspect'"
  (or
   haskell-tng--hsinspect-exe
   (setq
    haskell-tng--hsinspect-exe
    (let ((which (string-trim (shell-command-to-string haskell-tng--hsinspect-which-hsinspect))))
      (if (file-exists-p which)
          which
        ;; fall back to system installed binary
        "hsinspect")))))

(defun haskell-tng--hsinspect (&rest params)
  (ignore-errors (kill-buffer "*hsinspect*"))
  (when-let ((ghcflags (haskell-tng--hsinspect-ghcflags))
             ;; TODO search for the .cabal file and then delete .ghc.version support
             (default-directory (locate-dominating-file default-directory ".ghc.version")))
    (if (/= 0
            (let ((process-environment (cons "GHC_ENVIRONMENT=-" process-environment)))
              (apply
               #'call-process
               ;; TODO async
               (haskell-tng--hsinspect-exe)
               nil "*hsinspect*" nil
               (append params '("--") ghcflags))))
        (user-error "`hsinspect' failed. See the *hsinspect* buffer for more information")
      (with-current-buffer "*hsinspect*"
        ;; TODO remove this resilience against stdout / stderr noise
        (goto-char (point-max))
        (backward-sexp)
        (or (ignore-errors (read (current-buffer))) t)))))

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
