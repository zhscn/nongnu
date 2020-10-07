;;; haskell-tng-extra-hideshow.el --- hideshow-mode integration -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Hide and show BOILERPLATE blocks.
;;
;;; Code:

(require 'hideshow)
(require 'haskell-tng-mode)

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (hs-minor-mode 1)
   (setq-local hs-hide-comments-when-hiding-all nil)
   (let ((inhibit-message t))
     (hs-hide-all))))

(add-to-list
 'hs-special-modes-alist
 '(haskell-tng-mode
   "{- BOILERPLATE START -}"
   "{- BOILERPLATE END -}"
   "-- "
   haskell-tng--extra-hideshow-forward))

(defun haskell-tng--extra-hideshow-forward (&optional _arg _interactive)
  "hide-show forward function that does what you'd expect"
  (goto-char
   (save-excursion
     (re-search-forward hs-block-end-regexp nil t)
     (point))))

(provide 'haskell-tng-extra-hideshow)
;;; haskell-tng-extra-hideshow.el ends here
