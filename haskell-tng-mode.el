;;; haskell-tng-mode.el --- Major mode for editing Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

;; Homepage: https://gitlab.com/tseenshe/haskell-tng-mode
;; Keywords: languages
;; Package-Version: 0.0.1
;; Package-Requires: ((dash "2.14.1"))

;;; Commentary:
;;
;;  A modern rewrite of `haskell-mode'.
;;
;;; Code:

(require 'dabbrev)
(require 'haskell-tng-syntax)
(require 'haskell-tng-font-lock)

;;;###autoload
(define-derived-mode haskell-tng-mode prog-mode "Hask"
  "Major mode for editing Haskell programs."
  :group 'haskell-tng
  :syntax-table haskell-tng:syntax-table

  ;; TODO paragraph-start, paragraph-separate, fill-paragraph-function
  ;;
  ;; TODO comment-start, comment-padding, comment-start-skip, comment-end,
  ;;      comment-end-skip, comment-auto-fill-only-comments,
  ;;      parse-sexp-ignore-comments (it is annoying that we must specify
  ;;      comments here AND in the syntax table)
  ;;
  ;; TODO mark-defun
  ;; TODO font-lock-mark-block-function

  (setq
   ;; TAB is evil
   indent-tabs-mode nil

   ;; case-sensitive language
   dabbrev-case-fold-search nil
   dabbrev-case-distinction nil
   dabbrev-case-replace nil

   words-include-escapes t
   syntax-propertize-function #'haskell-tng:syntax-propertize
   parse-sexp-lookup-properties t

   font-lock-defaults '(haskell-tng:keywords)
   font-lock-multiline t
   font-lock-extend-region-functions '(font-lock-extend-region-wholelines
                                       haskell-tng:extend-parens-open
                                       haskell-tng:extend-parens-close
                                       haskell-tng:extend-type-open
                                       haskell-tng:extend-type-close
                                       haskell-tng:extend-defns
                                       haskell-tng:extend-module-open
                                       haskell-tng:extend-module-close
                                       haskell-tng:extend-import-open
                                       haskell-tng:extend-import-close)

   ;; whitespace is meaningful, no electric indentation
   electric-indent-inhibit t)
  )

(defcustom haskell-tng-mode-hook nil
  "List of functions to run after `haskell-tng-mode' is enabled."
  :group 'haskell-tng
  :type 'hook)

;; (progn
;;   (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-tng-mode))
;;   (modify-coding-system-alist 'file "\\.hs\\'" 'utf-8))

(provide 'haskell-tng-mode)
;;; haskell-tng-mode.el ends here
