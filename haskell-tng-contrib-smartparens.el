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

;; TODO use advise instead of redefining the function
;; WORKAROUND smartparens indenting all the time
(defun sp--indent-region (start end &optional column)
  (unless (or
           (bound-and-true-p haskell-tng-mode)
           (bound-and-true-p aggressive-indent-mode))
    (cl-letf (((symbol-function 'message) #'ignore))
      (indent-region start end column))))

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (smartparens-mode 1)))

(provide 'haskell-tng-contrib-smartparens)
;;; haskell-tng-contrib-smartparens.el ends here
