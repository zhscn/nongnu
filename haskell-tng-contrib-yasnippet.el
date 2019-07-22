;;; haskell-tng-contrib-yasnippet.el --- yasnippet integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;; Code:

(require 'yasnippet)

(add-to-list
 'yas-snippet-dirs
 (expand-file-name
  "snippets"
  (when load-file-name
    (file-name-directory load-file-name))))
(yas-reload-all nil t)

(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   (yas-minor-mode 1)))

(provide 'haskell-tng-contrib-yasnippet)
;;; haskell-tng-contrib-yasnippet.el ends here
