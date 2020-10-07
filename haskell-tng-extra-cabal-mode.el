;;; haskell-tng-extra-cabal-mode.el --- a basic cabal-mode -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  A basic cabal-mode
;;
;;; Code:

(define-derived-mode cabal-tng-mode fundamental-mode "Cabal"
  "Major mode for editing Cabal configuration."
  (setq-local compile-command (format "cabal-fmt -i %s" buffer-file-name)))

(progn
  (add-to-list 'auto-mode-alist `(,(rx ".cabal" eos) . cabal-tng-mode))
  (modify-coding-system-alist 'file (rx ".cabal" eos) 'utf-8))

(provide 'haskell-tng-extra-cabal-mode)
;;; haskell-tng-extra-cabal-mode.el ends here
