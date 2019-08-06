;;; haskell-tng-contrib-company.el --- company mode integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Integration with `company-mode' completions for
;;
;;  1. keywords
;;  2. `hsinspect' contextual completions
;;
;;; Code:

(require 'company)

(require 'haskell-tng-hsinspect)

;; TODO keywords
;; TODO completions for imports (needs to know project wide module list)
;; TODO completions for symbols (can this be made more contextual)
;; TODO add-hook and a TNG specific list of default backends

(provide 'haskell-tng-contrib-company)
;;; haskell-tng-contrib-company.el ends here
