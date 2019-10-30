;;; haskell-tng-extra-stack.el --- sets stack as the default -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Overrides the defaults with stack variants.
;;
;;; Code:

(require 'haskell-tng-compile)
(require 'haskell-tng-hsinspect)

(setq
 haskell-tng--hsinspect-which-hsinspect "stack exec --silent which -- hsinspect"
 haskell-tng--compile-history '("stack build --fast --no-run-tests --ghc-options=\"-j\""
                                "stack build --fast --ghc-options=\"-j\""))

(setq-default
 haskell-tng--compile-alt "stack clean")

(provide 'haskell-tng-extra-stack)
;;; haskell-tng-extra-stack.el ends here
