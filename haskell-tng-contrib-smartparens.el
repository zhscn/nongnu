;;; haskell-tng-contrib-smartparens.el --- smartparens integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;; Code:

(require 'smartparens)

(dolist (pair '(("(" . ")")
                ("[" . "]")
                ("{" . "}")
                ("{-" . "-}")
                ("{-#" . "#-}")))
  (sp-local-pair 'haskell-tng-mode (car pair) (cdr pair)
                 :post-handlers '(("| " "SPC"))))

(advice-add #'sp--indent-region :around #'haskell-tng--contrib-sp-indent)
(defun haskell-tng--contrib-sp-indent (f &rest args)
  "Disables `sp--indent-region' locally."
  (unless (eq major-mode 'haskell-tng-mode)
    (apply f args)))

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (smartparens-mode 1)))

(provide 'haskell-tng-contrib-smartparens)
;;; haskell-tng-contrib-smartparens.el ends here
