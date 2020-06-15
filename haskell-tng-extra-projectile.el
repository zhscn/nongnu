;;; haskell-tng-extra-projectile.el --- Projectile integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;; Code:

(require 'projectile)

(require 'haskell-tng-util)

;; TODO fix the haskell-stack detection to also include cabal
;; TODO populate the projectile compile/run/test commands

(make-variable-buffer-local 'projectile-tags-command)
(add-hook
 'haskell-tng-mode-hook
 (lambda ()
   ;; Excluding dist-newstyle means excluding git source deps and generated
   ;; files, but also gives a bit of a speed boost since it will ignore
   ;; directories containing object files.
   (setq-local projectile-tags-command
               (concat
                (haskell-tng--util-which "fast-tags")
                " -Re --exclude=dist-newstyle ."))))

(provide 'haskell-tng-extra-projectile)
;;; haskell-tng-extra-projectile.el ends here
