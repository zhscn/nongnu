;;; haskell-tng-extra-lsp-hsinspect.el --- support for hsinspect-lsp -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  This is only for developing and testing the `hsinspect-lsp' binary:
;;  `haskell-tng-hsinspect' is a far superior way for users to interact with
;;  `hsinspect'.
;;
;;; Code:

(require 'lsp-mode)
(require 'subr-x)

(defcustom haskell-tng-lsp-hsinspect '("hsinspect-lsp")
  "The command and args to launch the hsinspect language server."
  :group 'haskell-tng
  :type 'listp)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda () (string-join haskell-tng-lsp-hsinspect " ")))
                  :major-modes '(haskell-tng-mode)
                  :server-id 'hsinspect-lsp))

(add-to-list
 'lsp-language-id-configuration
 '(haskell-tng-mode . "haskell"))

(provide 'haskell-tng-extra-lsp-hsinspect)
;;; haskell-tng-extra-lsp-hsinspect.el ends here
