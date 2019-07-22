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

;; WORKAROUND smartparens is indenting all the time, which is not good
(defun sp--indent-region (_1 _2 &optional _3)
  ;; TODO disable this function just in this mode
  )

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (smartparens-mode 1)))

(provide 'haskell-tng-contrib-smartparens)
;;; haskell-tng-contrib-smartparens.el ends here
