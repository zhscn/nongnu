;;; haskell-tng-contrib.el --- Untested features -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Untested / untestable commands that are either contributed by the community
;;  or require an external process to exist on PATH.
;;
;;; Code:

;; TODO a generic wrapper around commands that can be downloaded and built using
;;      cabal v2-install.

(require 'subr-x)

;;;###autoload
(defun haskell-tng-stylish-haskell ()
  "Apply `stylish-haskell' rules."
  ;; TODO use https://github.com/purcell/reformatter.el
  ;; TODO error buffer should be easy to dismiss
  (interactive)
  (save-buffer)
  (unless (= 0 (call-process "stylish-haskell" nil "*stylish-haskell*" nil "-i" buffer-file-name))
    (pop-to-buffer "*stylish-haskell*" nil t))
  (revert-buffer t t t))

;;;###autoload
(defun haskell-tng-stack2cabal ()
  "Prepare a stack project for use with cabal."
  (interactive)
  (when-let (default-directory
              (locate-dominating-file default-directory "stack.yaml"))
    (call-process "stack2cabal")))

;;;###autoload
(defun haskell-tng-goto-imports ()
  "Hack to jump to imports"
  ;; TODO imenu navigation will replace this
  (interactive)
  (re-search-backward (rx line-start "import")))

;;;###autoload
(defun haskell-tng-current-module ()
  "Puts the current module name into the kill ring."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "module" word-end) nil nil)
    (forward-comment (point-max))
    (re-search-forward (rx point (group (+ (not space))) space))
    (kill-new (match-string 1))))

(provide 'haskell-tng-contrib)
;;; haskell-tng-contrib.el ends here
