;;; haskell-tng-mode.el --- Major mode for editing Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;; Homepage: https://gitlab.com/tseenshe/haskell-tng-mode
;; Keywords: languages
;; Package-Version: 0.0.1
;; Package-Requires: ((dash "2.16.0") (emacs "24.3"))

;;; Commentary:
;;
;;  An experimental rewrite of `haskell-mode'.
;;
;;; Code:

(require 'dabbrev)

(require 'haskell-tng-syntax)
(require 'haskell-tng-font-lock)
(require 'haskell-tng-smie)
(require 'haskell-tng-compile)

(defgroup haskell-tng ()
  "Haskell support: The Next Generation."
  :group 'languages)

(defcustom haskell-tng-prettify-symbols
  '(("forall" . ?∀))
  "Integration with `prettify-symbols' giving the impression of UnicodeSyntax.

Load `prettify-symbols-mode' in `haskell-tng-mode-hook'."
  :type 'listp
  :group 'haskell-tng)

;;;###autoload
(define-derived-mode haskell-tng-mode prog-mode "Hask"
  "Major mode for editing Haskell programs."
  :group 'haskell-tng
  :syntax-table haskell-tng--syntax-table

  ;; TODO paragraph-start, paragraph-separate, fill-paragraph-function
  ;;
  ;; TODO mark-defun / font-lock-mark-block-function

  ;; TODO use setq-local (write a macro to allow multiple parameters)
  (setq
   ;; TAB is evil
   indent-tabs-mode nil
   tab-width 8

   ;; case-sensitive language
   dabbrev-case-fold-search nil
   dabbrev-case-distinction nil
   dabbrev-case-replace nil

   words-include-escapes t
   syntax-propertize-function #'haskell-tng--syntax-propertize
   parse-sexp-lookup-properties t
   parse-sexp-ignore-comments t

   ;; annoying that comments need to be defined here and syntax table
   comment-start "--"
   comment-padding 1
   comment-start-skip (rx "-" (+ "-") (* " "))
   comment-end ""
   comment-end-skip (rx (* "\s") (group (| (any ?\n) (syntax comment-end))))
   comment-auto-fill-only-comments t

   font-lock-defaults '(haskell-tng--font-lock-keywords)
   font-lock-multiline t
   font-lock-extend-region-functions haskell-tng--font-lock-extend-region-functions

   prettify-symbols-alist haskell-tng-prettify-symbols)

  ;; whitespace is meaningful, disable electric indentation. Note that
  ;; `electric-indent-inhibit' causes a performance regression in SMIE
  ;; indentation, so it's best to just make sure it is disabled.
  (electric-indent-mode 0)

  (setq-local smie-blink-matching-inners nil) ;; c.f. `smie-closer-alist'

  (haskell-tng--smie-setup)

  (cl-flet ((bind (key def) (define-key haskell-tng-mode-map (kbd key) def)))
    (bind "<return>" 'haskell-tng-newline)

    ;; core compilation loop, supports C-u and C-- prefixes
    (bind "C-c c" 'haskell-tng-compile)
    (bind "C-c e" 'next-error)

    ;; external tools
    (bind "C-c C" 'haskell-tng-stack2cabal)
    (bind "C-c C-r f" 'haskell-tng-stylish-haskell)
    )
  )

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-tng-mode))
  (modify-coding-system-alist 'file "\\.hs\\'" 'utf-8))

(provide 'haskell-tng-mode)
;;; haskell-tng-mode.el ends here
