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
 haskell-tng--compile-dominating-project (rx "stack.yaml")
 haskell-tng--compile-history '("stack build --fast --no-interleaved-output --ghc-options=\"-j\" --no-run-tests"
                                "stack build --fast --no-interleaved-output --ghc-options=\"-j\""))

(setq-default
 haskell-tng--compile-alt "stack clean"
 projectile-tags-command "fast-tags -Re --exclude=.stack-work .")

(provide 'haskell-tng-extra-stack)
;;; haskell-tng-extra-stack.el ends here
