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

(eval-when-compile
  (require 'cl-macs))

(require 'array)
(require 'dired)
(require 'subr-x)
(require 'tar-mode)
(require 'timer)
(require 'url)

;; Popups are not supported in stock Emacs so an extension is necessary:
;; https://emacs.stackexchange.com/questions/53373
;;
;; `x-show-tip' looks horrible and has no way to control when the popup closes.
;; `x-popup-menu' looks horrible and is incredibly complicated.
(require 'popup)

(require 'haskell-tng-compile)
(require 'haskell-tng-rx)
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
      ;; FIXME add type information from the index when available
      (haskell-tng--hsinspect-popup-tip (format "%s" found))
    (user-error "Not found")))

;;;###autoload
(defun haskell-tng-jump-to-definition (&optional alt)
  "Consult the `imports' in scope to calculate the symbol at point,
then find the package using the `index', then visit the
definition of the symbol in the build tool's source archive."
  (interactive "P")
  ;; TODO better error reporting when any of these things fail
  (when-let* ((imports (haskell-tng--hsinspect-imports nil alt))
              (index (haskell-tng--hsinspect-index alt))
              ;; TODO imports and index can be calculated in parallel
              (sym (haskell-tng--hsinspect-symbol-at-point))
              (qualified (haskell-tng--hsinspect-qualify imports sym)))
    (pcase-let* ((`(,imported . ,name)
                  (haskell-tng--string-split-last qualified "."))
                 (`(,pkg-entry ,module-entry ,internal-srcid ,internal-module)
                  (haskell-tng--hsinspect-follow index nil imported name)))
      (if (or (null pkg-entry) (alist-get 'inplace pkg-entry))
          (progn
            ;; simpler alternative to parsing build tool specific data
            (if-let (package-dir (or
                                  (haskell-tng--util-locate-dominating-file
                                   haskell-tng--compile-dominating-project)
                                  (haskell-tng--util-locate-dominating-file
                                   haskell-tng--compile-dominating-package)))
                (pcase-let ((`(,dir . ,file) (haskell-tng--string-split-last imported ".")))
                  (let ((find (string-trim
                               (shell-command-to-string
                                (format "find %s -path \"*/%s/*\" -name \"%s.hs\""
                                        package-dir
                                        (haskell-tng--string-replace dir "." "/")
                                        file)))))
                    (if (file-readable-p find)
                        (progn
                          (find-file find)
                          (haskell-tng--hsinspect-goto-symbol name))
                      (error "did not find %s.hs in %s" file dir))))
              (error "%s is defined in a local package and the project root is unknown" qualified)))
        (when-let* ((srcid (or internal-srcid (alist-get 'srcid pkg-entry)))
                    (module (or internal-module (alist-get 'module module-entry)))
                    (file (concat (haskell-tng--string-replace module "." "/") ".hs"))
                    (tarball (haskell-tng--hsinspect-srcid-source srcid)))
          (when (not (file-exists-p tarball))
            ;; We can't expect stack to reveal source locations because it
            ;; obfuscates all downloads. Cabal has "cabal get" but it is broken.
            ;; WORKAROUND https://github.com/haskell/cabal/issues/6443
            (let ((remote (haskell-tng--hsinspect-hackage-source srcid))
                  (dir (file-name-directory tarball)))
              (unless (file-directory-p dir)
                (make-directory dir t))
              (message "%s was not found, attempting to download %s" tarball remote)
              (url-copy-file remote tarball)))
          (message "Loading %s from %s" sym tarball)
          (find-file tarball)
          ;; TODO it would be a faster UX if we used ZIP instead of TAR.GZ because
          ;;      this requires us to decompress the entire file to find the index,
          ;;      and then again until we reach the entry we want to load. But that
          ;;      would come with the cost of recompressing, plus the storage cost
          ;;      of caching it all.
          (let ((archive (current-buffer)))
            (goto-char (point-min))
            (re-search-forward (rx-to-string `(: (* any) ,file)))
            ;; TODO could set the index cache variable to the one we used for the
            ;;      search, if it provided any useful features.
            (tar-extract)
            (kill-buffer archive)
            (read-only-mode 1)
            (haskell-tng--hsinspect-goto-symbol name)))))))

;; TODO re-use the imenu top-level parser, this is a massive hack
(defun haskell-tng--hsinspect-goto-symbol (name)
  (goto-char (point-min))
  (re-search-forward (rx line-start "import" word-end) nil t)
  (or
   (re-search-forward (rx-to-string `(: (| bol "= " "| " "data " "type " "class ") ,name symbol-end)) nil t)
   (re-search-forward (rx-to-string `(: symbol-start ,name symbol-end)))))

(defun haskell-tng--string-replace (str from to)
  (mapconcat 'identity (split-string str (regexp-quote from)) to))

(defun haskell-tng--string-split-last (str sep)
  "Return `(front . back)' of a STR split on the last SEP."
  ;; TODO optimise
  (let* ((parts (split-string str (regexp-quote sep)))
         (front (mapconcat 'identity (butlast parts) sep))
         (back (car (last parts))))
    (cons front back)))

(defun haskell-tng--hsinspect-srcid-source (srcid)
  (message "[haskell-tng] [DEBUG] tarball %s" srcid)
  (pcase-let ((`(,package . ,version) (haskell-tng--string-split-last srcid "-")))
    (expand-file-name
     (concat
      "~/.cabal/packages/hackage.haskell.org/"
      package "/" version "/" srcid
      ".tar.gz"))))

(defun haskell-tng--hsinspect-hackage-source (srcid)
  (concat "http://hackage.haskell.org/package/" srcid "/" srcid ".tar.gz"))

;; TODO haskell-tng-show-documentation

(defvar-local haskell-tng-hsinspect-as
  ;; TODO populate with even more than this
  '(("Data.Aeson" . "Json")
    ("Data.List" . "L")
    ("Data.List" . "List")
    ("Data.List.NonEmpty" . "NE")
    ("Data.List.NonEmpty" . "NEL")
    ("Data.Set" . "S")
    ("Data.Set" . "Set")
    ("Data.Map.Strict" . "M")
    ("Data.Map.Strict" . "Map")
    ("Data.ByteString" . "BS")
    ("Data.ByteString.Lazy" . "LBS")
    ("Data.Text" . "T"))
  "An alist of (MODULE . NAME) to use for qualified imports.

The first matching MODULE is expanded into NAME when expanding an
unqualified symbol into a qualified one. If one is not provided
here, the short module name is used.

The first matching NAME is imported as MODULE when importing a
qualified symbol. In such cases, the short module name is not
automatically used and must be provided.")
(put 'haskell-tng-hsinspect-as 'safe-local-variable #'listp)
(defun haskell-tng--hsinspect-as (module)
  (or
   (alist-get module haskell-tng-hsinspect-as nil nil 'equal)
   (read-string
    (concat "import qualified " module " as ")
    (car (last (split-string module (regexp-quote ".")))))))

(defcustom haskell-tng-hsinspect-qualify nil
  "`haskell-tng-import-symbol-at-point' will prefer qualified imports."
  :type 'booleanp
  :group 'haskell-tng)

;;;###autoload
(defun haskell-tng-import-symbol-at-point (&optional alt)
  "Import the symbol at point by querying the user to select from a menu.

If the symbol is qualified, the module will be imported
qualified.

If called with a `-' prefix, the module will be imported
qualified and the user will be asked for the name (behaviour is
reversed if `haskell-tng-hsinspect-qualify' is set).

Respects the `C-u' cache invalidation convention."
  (interactive "P")
  ;; TODO add parens around operators (or should that be in the utility?)
  (let (qual
        (flush-cache (and alt (not (eq '- alt)))))
    (when-let ((index (haskell-tng--hsinspect-index flush-cache))
               (sym (haskell-tng--hsinspect-symbol-at-point)))
      (message "Searching for '%s' in %s packages" sym (length index))

      (when (string-match (rx bos (group (+ anything)) "." (group (+ (not (any ".")))) eos) sym)
        (setq qual (match-string 1 sym))
        (setq sym (match-string 2 sym)))

      (let ((qual_ (car (rassoc qual haskell-tng-hsinspect-as))))
        (if (when qual_
              (haskell-tng--hsinspect-check-fqn-import index qual_ sym))
            (haskell-tng--hsinspect-import-symbol index qual_ qual)
          (when-let (hit (haskell-tng--hsinspect-import-popup index sym))
            (let* ((module (alist-get 'module hit))
                   (class (alist-get 'class hit))
                   (type (alist-get 'type hit))
                   (name (alist-get 'name hit)))
              (cond
               (qual (haskell-tng--hsinspect-import-symbol index module qual))

               ((xor haskell-tng-hsinspect-qualify (eq '- alt))
                (when-let (as (haskell-tng--hsinspect-as module))
                  (haskell-tng--hsinspect-import-symbol index module as)
                  (save-excursion
                    (haskell-tng--hsinspect-beginning-of-symbol)
                    (insert as "."))))

               ((eq class 'tycon)
                (haskell-tng--hsinspect-import-symbol
                 index
                 module nil
                 (haskell-tng--hsinspect-return-type type)))

               ((eq class 'con)
                (haskell-tng--hsinspect-import-symbol
                 index
                 module nil
                 (concat (haskell-tng--hsinspect-return-type type) "(..)")))

               (t (haskell-tng--hsinspect-import-symbol index module nil name)))))))
      )))

(defun haskell-tng--hsinspect-extract-imports (index module &optional as sym)
  "Calculates the imports from INDEX that are implied by MODULE AS and SYM."
  (if sym
      `(((local . ,sym) (full . ,(concat module "." sym))))
    (cl-loop
     for pkg-entry in index
     append
     (cl-loop
      for module-entry in (alist-get 'modules pkg-entry)
      when (equal module (alist-get 'module module-entry))
      append
      (cl-loop
       for entry in (alist-get 'ids module-entry)
       collect
       (let* ((name (alist-get 'name entry))
              (type (alist-get 'type entry))
              (id (pcase (alist-get 'class entry)
                    ((or 'id 'con 'pat) name)
                    ('tycon type)))
              (full (concat module "." id)))
         (if as
             `((qual . ,(concat as "." id))
               (full . ,full))
           `((local . ,id)
             (full . ,full)))))))))

(defun haskell-tng--hsinspect-check-fqn-import (index module sym)
  "Checks if an FQN exists"
  (cl-block nested
    (cl-loop
     for pkg-entry in index
     do
     (cl-loop
      for module-entry in (alist-get 'modules pkg-entry)
      when (equal module (alist-get 'module module-entry))
      do
      (cl-loop
       for entry in (alist-get 'ids module-entry)
       do
       (let* ((name (alist-get 'name entry))
              (type (alist-get 'type entry))
              (id (pcase (alist-get 'class entry)
                    ((or 'id 'con 'pat) name)
                    ('tycon type))))
         (when (equal sym id)
           (cl-return-from nested
             `(,(alist-get 'srcid pkg-entry))))))))))

(defun haskell-tng--hsinspect-return-type (type)
  (car
   (split-string
    (car
     (last
      (split-string
       type (rx "->" (* space))))))))

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

(defun haskell-tng--hsinspect-index-get-module (index srcid module)
  "Return the (pkg-entry . module-entry) for SRCID and MODULE.
nil if nothing was found.

If SRCID is nil then the first matching MODULE is used."
  (cl-block nested
    (cl-loop
     for pkg-entry in index
     when (or (null srcid) (equal srcid (alist-get 'srcid pkg-entry)))
     do
     (cl-loop
      for module-entry in (alist-get 'modules pkg-entry)
      when (equal module (alist-get 'module module-entry))
      do
      (cl-return-from nested (cons pkg-entry module-entry))))))

(defun haskell-tng--hsinspect-follow (index srcid module name)
  "Follow re-exports of MODULE to find where it was originally defined.

Takes the form `(pkg-entry module-entry srcid internal)' where
`srcid' and `internal' may point to a target that isn't in the
index (e.g. an unexported module), at which point we lose the
ability to follow any further."
  ;; TODO probably doesn't work for 'tycon
  ;; TODO `hsinspect index' could evaluate all re-exports to their final destination
  (when srcid
    (message "[haskell-tng] [DEBUG] follow %s %s %s" srcid module name))
  (when-let*
      ((found (haskell-tng--hsinspect-index-get-module index srcid module))
       (pkg-entry (car found))
       (srcid_ (alist-get 'srcid pkg-entry))
       (module-entry (cdr found))
       (entry (seq-find
               (lambda (e) (equal name (or (alist-get 'name e) (alist-get 'type e))))
               (alist-get 'ids module-entry))))
    (or (when-let* ((export (alist-get 'export entry))
                    (e-srcid (or (alist-get 'srcid export) srcid_))
                    (e-module (alist-get 'module export)))
          (or (haskell-tng--hsinspect-follow index e-srcid e-module name)
              (list pkg-entry module-entry e-srcid e-module)))
        (list pkg-entry module-entry))))

(defun haskell-tng--hsinspect-import-popup (index sym)
  (when-let ((hits (haskell-tng--hsinspect-import-candidates index sym)))
    ;; TODO special case one hit
    ;; TODO show more context, like the type
    (when-let* ((entries (mapcar (lambda (el) (alist-get 'module el)) hits))
                (selected (haskell-tng--hsinspect-popup-menu entries)))
      (seq-find (lambda (el) (equal (alist-get 'module el) selected)) hits))))

;; FIXME allow this to be configurable so pop-up is an -extra with zero dep dependencies
(defvar-local haskell-tng--hsinspect-popup-menu nil)
(defun haskell-tng--hsinspect-popup-menu (entries)
  (or
   haskell-tng--hsinspect-popup-menu ;; testing feature
   (popup-menu* entries)))

(defvar-local haskell-tng--hsinspect-popup-tip nil)
(defun haskell-tng--hsinspect-popup-tip (tip)
  (if haskell-tng--hsinspect-popup-tip ;; testing feature
      tip
    (popup-tip tip)))

;; TODO when the same name is reused as a type and data constructor we show dupe
;; entries to the user. We should dedupe that to just the cons unless we have a
;; way to make the choice clearer.
(defun haskell-tng--hsinspect-import-candidates (index sym)
  "Return an list of alists with keys: module, name, type.
When using hsinspect-0.0.8, also: class, export, flavour.
When using hsinspect-0.0.9, also: srcid."
  (cl-loop
   for pkg-entry in index
   append
   (cl-loop
    for module-entry in (alist-get 'modules pkg-entry)
    append
    (cl-loop
     for entry in (alist-get 'ids module-entry)
     append
     (let ((srcid (alist-get 'srcid pkg-entry))
           (module (alist-get 'module module-entry))
           (name (alist-get 'name entry))
           (type (alist-get 'type entry))
           (class (alist-get 'class entry))
           (export (alist-get 'export entry))
           (flavour (alist-get 'flavour entry)))
       ;; TODO alist variable binding like RecordWildcards
       (when (or
              (and
               (member class '(id con pat))
               (equal name sym))
              (and
               (equal class 'tycon)
               (equal type sym)))
         `(((srcid . ,srcid)
            (module . ,module)
            (name . ,name)
            (type . ,type)
            (class . ,class)
            (export . ,export)
            (flavour . ,flavour)))))))))

(defun haskell-tng--hsinspect-symbol-at-point ()
  "A `symbol-at-point' that includes FQN parts."
  (buffer-substring-no-properties
   (save-excursion
     (haskell-tng--hsinspect-beginning-of-symbol)
     (point))
   (save-excursion
     (re-search-forward
      (rx point (+ (| word (syntax symbol) ".")) symbol-end)
      (line-end-position) 't)
     (point))))

(defun haskell-tng--hsinspect-beginning-of-symbol ()
  (let ((lbp (line-beginning-position)))
    ;; can't use `smie-backward-token-function' because we could be at the start,
    ;; middle, or end.
    (re-search-backward
     (rx symbol-start (+ (| word (syntax symbol) ".")) point)
     lbp 't)
    ;; WORKAROUND non-greedy matches
    (while (looking-back haskell-tng--rx-c-qual lbp 't)
      (goto-char (match-beginning 0)))))

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
  (haskell-tng--util-cached
   (lambda () (haskell-tng--hsinspect flush-cache "imports" buffer-file-name))
   'haskell-tng--hsinspect-imports
   (concat "hsinspect-0.0.7" buffer-file-name "." "imports")
   no-work
   flush-cache))

(defun haskell-tng--hsinspect-import-symbol (index module as &optional sym)
  "Add the import to the current buffer and update `haskell-tng--hsinspect-imports'.

Does not persist the cache changes to disk."
  (haskell-tng--util-import-symbol module as sym)
  (let ((updates (haskell-tng--hsinspect-extract-imports index module as sym)))
    (setq haskell-tng--hsinspect-imports
          (if (eq haskell-tng--hsinspect-imports 'cached-nil)
              updates
            (append haskell-tng--hsinspect-imports updates)))))

;; TODO add a package-wide variable cache
(defvar-local haskell-tng--hsinspect-index nil)
(defun haskell-tng--hsinspect-index (&optional flush-cache)
  (or ;; this variable cache is only used in tests
   haskell-tng--hsinspect-index
   (when-let (ghcflags-dir
              (locate-dominating-file default-directory ".ghc.flags"))
     (haskell-tng--util-cached-disk
      (lambda () (haskell-tng--hsinspect flush-cache "index"))
      (concat "hsinspect-0.0.7" (expand-file-name ghcflags-dir) "index")
      nil
      flush-cache))))

;; TODO add a project-wide variable cache
(defvar-local haskell-tng--hsinspect-exe nil)
(defun haskell-tng--hsinspect-exe (&optional flush-cache)
  "The cached binary to use for `hsinspect'"
  (when-let (package-dir (or
                          (haskell-tng--util-locate-dominating-file
                           haskell-tng--compile-dominating-project)
                          (haskell-tng--util-locate-dominating-file
                           haskell-tng--compile-dominating-package)))
    (haskell-tng--util-cached
     #'haskell-tng--hsinspect-which-hsinspect
     'haskell-tng--hsinspect-exe
     (concat "which" (expand-file-name package-dir) "hsinspect")
     nil
     flush-cache)))

(defun haskell-tng--hsinspect-which-hsinspect ()
  "Finds and checks the hsinspect binary for the current buffer.

This is uncached, prefer `haskell-tng--hsinspect-exe'."
  (let ((supported '("0.0.7" "0.0.8" "0.0.9" "0.0.10" "0.0.11" "0.0.12" "0.0.13" "0.0.14" "0.0.15" "0.0.16" "0.0.17" "0.0.18" "0.0.19" "0.1.0" "0.1.1"))
        (bin (haskell-tng--util-ghcpath-which "hsinspect")))
    (if bin
        (let ((version
               (string-trim
                (shell-command-to-string (concat bin " --version")))))
          (if (member version supported)
              ;; TODO from 0.0.8+ do a --ghc-version check (a common failure mode)
              bin
            (user-error "The hsinspect binary is the wrong version: got `%s' require `%s'" version supported)))
      (user-error "The hsinspect binary is not available. See https://gitlab.com/tseenshe/hsinspect#installation"))))

(defun haskell-tng--hsinspect (flush-cache &rest params)
  (ignore-errors (kill-buffer "*hsinspect*"))
  (when-let ((ghcpath (haskell-tng--util-ghcpath))
             (ghcflags (haskell-tng--hsinspect-ghcflags))
             (hsinspect (haskell-tng--hsinspect-exe flush-cache))
             (default-directory (haskell-tng--util-locate-dominating-file
                                 haskell-tng--compile-dominating-package)))
    (if (/= 0
            (let ((process-environment (cons "GHC_ENVIRONMENT=-" process-environment)))
              (setq process-environment
                    (cons (concat "PATH=" ghcpath) process-environment))
              (apply
               #'call-process
               hsinspect
               nil "*hsinspect*" nil
               (append params '("--") ghcflags))))
        (user-error "Failed, see *hsinspect* buffer for more information")
      (with-current-buffer "*hsinspect*"
        ;; TODO remove this resilience against stdout / stderr noise
        (goto-char (point-max))
        (backward-sexp)
        (ignore-errors (read (current-buffer)))))))

(defun haskell-tng-hsinspect (&optional alt)
  "Fill the `hsinspect' caches"
  (interactive "P")
  (haskell-tng--hsinspect-imports nil alt)
  (haskell-tng--hsinspect-index alt))

(defcustom haskell-tng-hsinspect-auto -1
  "Automatically run `hsinspect'.

Runs after the given number of idle seconds, ignoring errors.
Negative to disable."
  :type 'integerp
  :group 'haskell-tng)

(defun haskell-tng-hsinspect-hook ()
  (when (<= 0 haskell-tng-hsinspect-auto)
    (run-with-idle-timer
     haskell-tng-hsinspect-auto
     nil
     (lambda ()
       (message "[haskell-tng] running hsinspect on idle")
       (make-thread #'haskell-tng-hsinspect)))))

(add-hook
 'haskell-tng-mode-hook
 #'haskell-tng-hsinspect-hook)

(provide 'haskell-tng-hsinspect)
;;; haskell-tng-hsinspect.el ends here
