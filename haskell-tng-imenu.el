;;; haskell-tng-imenu.el --- imenu support -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Creates a summary of the current file for navigation purposes.
;;
;;; Code:

(defun haskell-tng--imenu-create ()
  "Creates a `imenu--index-alist' for the current buffer."
  ;; Simple elements in the alist look like (INDEX-NAME . POSITION).
  ;; A nested sub-alist element looks like (INDEX-NAME . SUB-ALIST).

  ;; FIXME implement imenu
  '(("*Rescan*" . -99))
  )

(provide 'haskell-tng-imenu)
;;; haskell-tng-imenu.el ends here
