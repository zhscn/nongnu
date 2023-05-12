;;; haskell-tng-extra.el --- Untested features -*- lexical-binding: t -*-

;; TODO Rename all -tng-extra things (again) to -ds9

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Untested / untestable commands that may require an external process to exist
;;  on PATH.
;;
;;; Code:

;; TODO a generic wrapper around commands that can be downloaded and built using
;;      cabal install.

;; TODO cabal-fmt on new file creation (for the `insert' support). Maybe with a
;; hook in both `before-save-hook' (detecting that the file is new) and
;; `after-save-hook' (running the command and resetting the newfile var).

(require 'hideshow)
(require 'subr-x)

(require 'haskell-tng-util)

;;###autoload
(defun haskell-tng-newline (&optional alt)
  "A `newline-and-indent' with a better user experience for `haskell-tng-mode'.

When in a comment and called with a prefix, the comment will be completed."
  (interactive "P")
  ;; TODO a dynamically bound variable might improve the quality of
  ;;      'empty-line-token predictions. Parens are special-cased.
  (when (<= (- (point-max) 1) (point))
    ;; WORKAROUND https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36432
    ;; TODO fix the bug properly in SMIE
    (save-excursion (insert "\n\n")))
  (let ((rem (save-excursion
               (skip-syntax-forward " ")
               (unless (looking-at (rx (syntax close-parenthesis)))
                 (when (/= (point) (line-end-position))
                   (buffer-substring-no-properties (point) (line-end-position)))))))
    (when rem
      (delete-region (point) (line-end-position)))
    ;; TODO don't continue line comments if there is code before them
    ;;
    ;; TODO in-comment indent should observe but not repeat | haddock markers
    ;;
    ;; TODO newline-and-indent is adding a lot of newlines
    (cond
     (alt
      (call-interactively #'newline-and-indent))
     ((looking-back (rx (>= 3 "-")) (line-beginning-position))
      ;; don't continue or indent visual line breaks
      (call-interactively #'newline))
     (t
      (call-interactively #'comment-indent-new-line)))
    (when rem
      (save-excursion
        (insert rem)))))

;;;###autoload
(defun haskell-tng-format ()
  "Uses stylish-haskell if there is a config file, falling back to ormolu."
  (interactive)
  (if (locate-dominating-file default-directory ".stylish-haskell.yaml")
      (haskell-tng-stylish-haskell)
    (haskell-tng-ormolu)))

;;;###autoload
(defvar-local haskell-tng-stylish-haskell nil
  "A cache of the `stylish-haskell' binary as seen from this buffer.")
(defun haskell-tng-stylish-haskell ()
  "Apply `stylish-haskell' rules."
  ;; TODO use https://github.com/purcell/reformatter.el
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (if (/= 0 (call-process
             (haskell-tng--util-cached-variable
              (lambda () (haskell-tng--util-which "stylish-haskell"))
              'haskell-tng-stylish-haskell)
             nil "*stylish-haskell*" nil "-i" buffer-file-name))
      (user-error "stylish-haskell formatting failed")
    (revert-buffer t t t)))

;;;###autoload
(defvar-local haskell-tng-ormolu nil
  "A cache of the `ormolu' binary as seen from this buffer.")
(defun haskell-tng-ormolu ()
  "Apply `ormolu' rules."
  ;; TODO use https://github.com/purcell/reformatter.el
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (if (/= 0 (call-process
             (haskell-tng--util-cached-variable
              (lambda () (haskell-tng--util-which "ormolu"))
              'haskell-tng-ormolu)
             nil "*ormolu*" nil
             ;; "-p"
             "-o" "-XTypeApplications"
             "-o" "-XBangPatterns"
             "-o" "-XPatternSynonyms"
             "-m" "inplace"
             buffer-file-name))
      (user-error "ormolu formatting failed")
    (revert-buffer t t t)))

;;;###autoload
(defvar-local haskell-tng-stack2cabal nil
  "A cache of the `stack2cabal' binary as seen from this buffer.")
(defun haskell-tng-stack2cabal ()
  "Prepare a stack project for use with cabal."
  (interactive)
  (when-let (default-directory
              (locate-dominating-file default-directory "stack.yaml"))
    (when (/= 0 (call-process
                 (haskell-tng--util-cached-variable
                  (lambda () (haskell-tng--util-which "stack2cabal"))
                  'haskell-tng-stack2cabal)))
      (user-error "stack2cabal failed"))))

;;;###autoload
(defvar-local haskell-tng-boilerplate nil
  "A cache of the `boilerplate' binary as seen from this buffer.")
(defun haskell-tng-boilerplate ()
  "Apply `boilerplate' expansion rules."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (if (/= 0 (call-process
             (haskell-tng--util-cached-variable
              (lambda () (haskell-tng--util-which "boilerplate"))
              'haskell-tng-boilerplate)
             nil "*boilerplate*" nil "-i" buffer-file-name))
      (user-error "boilerplate generation failed")
    (revert-buffer t t t)
    (when hs-minor-mode
      (hs-hide-all))))

;;;###autoload
(defun haskell-tng-goto-imports ()
  "Hack to jump to imports"
  ;; TODO is this useful?
  (interactive)
  ;; TODO comment / text resilience
  (goto-char (point-min))
  (re-search-forward (rx line-start "import" word-end))
  (forward-line 0))

;;;###autoload
(defun haskell-tng-import-module (module)
  "Adds an unqualified wildcard import."
  ;; TODO autocomplete on available imports
  (interactive "s")
  (haskell-tng--util-import-symbol module nil nil))

;;;###autoload
(defun haskell-tng-current-module ()
  "Puts the current module name into the kill ring."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "module" word-end))
    (forward-comment (point-max))
    (re-search-forward (rx point (group (+ (not space))) space))
    (kill-new (match-string 1))))

;;;###autoload
(defun haskell-tng-filename-to-modulename ()
  "Infers the ModuleName for the current file based on filesystem layout."
  (mapconcat
   #'identity
   (reverse
    (seq-take-while
     (lambda (e) (let (case-fold-search)
              (string-match-p (rx bos upper) e)))
     (reverse
      (split-string
       (file-name-sans-extension buffer-file-name)
       "\\/"))))
   "."))

;; TODO cleanup imports based on -ddump-minimal-imports (requires user opt-in,
;;      .imports discovery and cleanup). Could also do dead code analysis with
;;      this data.

(provide 'haskell-tng-extra)
;;; haskell-tng-extra.el ends here
