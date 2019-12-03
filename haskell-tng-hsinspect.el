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

;; Popups are not supported in stock Emacs so an extension is necessary:
;; https://emacs.stackexchange.com/questions/53373
;;
;; `x-show-tip' looks horrible and has no way to control when the popup closes.
;; `x-popup-menu' looks horrible and is incredibly complicated.
(require 'popup)

(require 'haskell-tng-compile)
(require 'haskell-tng-util)

;;;###autoload
(defun haskell-tng-fqn-at-point (&optional alt)
  "Consult the imports in scope and display the fully qualified
name of the symbol at point in the minibuffer.

A prefix argument ensures that caches are flushes."
  (interactive "P")
  (if-let* ((sym (haskell-tng--hsinspect-symbol-at-point))
            (found (haskell-tng--hsinspect-qualify
                    (haskell-tng--hsinspect-imports nil alt)
                    sym)))
      ;; TODO multiple hits
      ;; TODO add type information from the index when available
      (popup-tip (format "%s" found)))
  (user-error "Not found"))

;; TODO jump-to-definition using import + index + heuristics

;;;###autoload
(defun haskell-tng-import-symbol-at-point (&optional alt)
  "Import the symbol at point"
  ;; TODO double prefix + FQN should mean use unqualified `as' import
  ;; TODO double prefix + unqualified should mean to import entire module
  ;; TODO shortlist for FQN imports (no need to calc the index)
  ;; TODO fqn version doesn't work one after the last character and non-fqn version doesn't work on first
  (interactive "P")
  ;; TODO update the hsinspect-imports cache
  (when-let* ((index (haskell-tng--hsinspect-index alt))
              (sym (haskell-tng--hsinspect-symbol-at-point)))
    (message "Seaching for '%s' in %s modules" sym (length index))
    (if (string-match (rx bos (group (+ anything)) "." (group (+ (not (any ".")))) eos) sym)
        (let* ((fqn (match-string 1 sym))
               (sym (match-string 2 sym)))
          ;; FIXME types and data constructors
          (when-let (hit (haskell-tng--hsinspect-import-popup index sym))
            (haskell-tng--import-symbol (alist-get 'module hit) fqn)))
      (when-let (hit (haskell-tng--hsinspect-import-popup index sym))
        ;; TODO add parens around operators
        ;; TODO add the type around data constructors (requires hsinspect changes)
        (haskell-tng--import-symbol (alist-get 'module hit) nil (alist-get 'name hit))))))

;; TODO expand out pattern matches (function defns and cases) based on the cons
;; for a type obtained from the Index.

;; TODO expand out wildcards in pattern matches. We can calculate the type by
;; looking at the names of the other data constructors that have been used.

(defun haskell-tng--hsinspect-qualify (imports sym)
  (cdar
   (last
    (seq-find
     (lambda (names) (member sym (seq-map #'cdr names)))
     imports))))

(defun haskell-tng--hsinspect-import-popup (index sym)
  (when-let ((hits (haskell-tng--hsinspect-import-candidates index sym)))
    ;; TODO special case one hit
    ;; TODO show more context, like the type
    (when-let* ((entries (mapcar (lambda (el) (alist-get 'module el)) hits))
                (selected (popup-menu* entries)))
      (seq-find (lambda (el) (equal (alist-get 'module el) selected)) hits))))

(defun haskell-tng--hsinspect-import-candidates (index sym)
  "Return an list of alists with keys: unitid, module, name, type.
When using hsinspect-0.0.8, also: class, export, flavour."
  ;; TODO threading/do syntax
  ;; TODO alist variable binding like RecordWildcards
  (seq-mapcat
   (lambda (pkg-entry)
     (let ((unitid (alist-get 'unitid pkg-entry))
           (modules (alist-get 'modules pkg-entry)))
       (seq-mapcat
        (lambda (module-entry)
          (let ((module (alist-get 'module module-entry))
                (ids (alist-get 'ids module-entry)))
            ;;(message "MODULE= %s" module)
            (seq-mapcat
             (lambda (entry)
               (let ((name (alist-get 'name entry))
                     (type (alist-get 'type entry))
                     (class (alist-get 'class entry))
                     (export (alist-get 'export entry))
                     (flavour (alist-get 'flavour entry)))
                 (when (equal name sym)
                   ;; TODO add the hsinspect-0.0.8 bits
                   `(((unitid . ,unitid)
                      (module . ,module)
                      (name . ,name)
                      (type . ,type)
                      (class . ,class)
                      (export . ,export)
                      (flavour . ,flavour))))))
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
          'no-error))
     (match-beginning 0))
   (save-excursion
     (re-search-forward
      (rx point (+ (| word (syntax symbol) ".")) symbol-end)
      (line-end-position) 'no-error)
     (match-end 0))))

(defun haskell-tng--hsinspect-ghcflags ()
  ;; https://github.com/haskell/cabal/issues/6203
  "Obtain the ghc flags for the current buffer"
  (if-let (default-directory (locate-dominating-file default-directory ".ghc.flags"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".ghc.flags"))
        (split-string
         (buffer-substring-no-properties (point-min) (point-max))))
    (user-error "Could not find `.ghc.flags': add GhcFlags.Plugin and compile.")))

(defvar-local haskell-tng--hsinspect-imports nil)
(defun haskell-tng--hsinspect-imports (&optional no-work flush-cache)
  (haskell-tng--hsinspect-cached
   (lambda () (haskell-tng--hsinspect flush-cache "imports" buffer-file-name))
   'haskell-tng--hsinspect-imports
   (concat "hsinspect-0.0.7" buffer-file-name "." "imports")
   no-work
   flush-cache))

;; TODO add a package-wide variable cache
(defun haskell-tng--hsinspect-index (&optional flush-cache)
  (when-let (ghcflags-dir
             (locate-dominating-file default-directory ".ghc.flags"))
    (haskell-tng--util-cached-disk
     (lambda () (haskell-tng--hsinspect flush-cache "index"))
     (concat "hsinspect-0.0.7" (expand-file-name ghcflags-dir) "index")
     nil
     flush-cache)))

;; TODO add a project-wide variable cache
(defun haskell-tng--hsinspect-exe (&optional flush-cache)
  "The cached binary to use for `hsinspect'"
  (when-let (package-dir (or
                          (haskell-tng--util-locate-dominating-file
                           haskell-tng--compile-dominating-project)
                          (haskell-tng--util-locate-dominating-file
                           haskell-tng--compile-dominating-package)))
    (haskell-tng--util-cached-disk
     #'haskell-tng--hsinspect-which-hsinspect
     (concat "which" (expand-file-name package-dir) "hsinspect")
     nil
     flush-cache)))

(defvar haskell-tng--hsinspect-which-hsinspect
  "cabal build -v0 :pkg:hsinspect:exe:hsinspect && cabal exec -v0 which -- hsinspect")
(defun haskell-tng--hsinspect-which-hsinspect ()
  "Finds and checks the hsinspect binary for the current buffer.

This is uncached, prefer `haskell-tng--hsinspect-exe'."
  (let ((supported '("0.0.7" "0.0.8"))
        (bin
         (car
          (last
           (split-string
            (string-trim
             (shell-command-to-string
              haskell-tng--hsinspect-which-hsinspect))
            "\n")))))
    (if (file-executable-p bin)
        (let ((version
               (string-trim
                (shell-command-to-string (concat bin " --version")))))
          (if (member version supported)
              ;; TODO from 0.0.8+ do a --ghc-version check (a common failure mode)
              bin
            (user-error "The hsinspect binary is the wrong version: got `%s' require `%s'" version supported)))
      (user-error "The hsinspect binary is not executable: %S" bin))))

(defun haskell-tng--hsinspect (flush-cache &rest params)
  (ignore-errors (kill-buffer "*hsinspect*"))
  (when-let ((ghcflags (haskell-tng--hsinspect-ghcflags))
             (default-directory (haskell-tng--util-locate-dominating-file
                                 haskell-tng--compile-dominating-package)))
    (if (/= 0
            (let ((process-environment (cons "GHC_ENVIRONMENT=-" process-environment)))
              (apply
               #'call-process
               (or (haskell-tng--hsinspect-exe flush-cache)
                   (user-error "Could not find hsinspect: add to build-tool-depends"))
               nil "*hsinspect*" nil
               (append params '("--") ghcflags))))
        (user-error "Failed, see *hsinspect* buffer for more information")
      (with-current-buffer "*hsinspect*"
        ;; TODO remove this resilience against stdout / stderr noise
        (goto-char (point-max))
        (backward-sexp)
        (ignore-errors (read (current-buffer)))))))

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
