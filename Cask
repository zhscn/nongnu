;;-*- Mode: Lisp -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  See http://cask.readthedocs.org for information about Cask.
;;
;;    cask pkg-file
;;
;;    cask update
;;    cask install
;;
;;  are particularly useful commands.
;;
;; To run the tests:
;;    cask exec ert-runner
;;
;;; Code:

;; TODO set up CI on gitlab

(source melpa-stable)
(source melpa) ;; for faceup

(package-file "haskell-tng-mode.el")

(development
 (depends-on "company")
 (depends-on "popup")
 ;;(depends-on "elsa") ;; cask exec elsa FILE
 (depends-on "faceup")
 (depends-on "ert-runner")
 ;;(depends-on "undercover")
 (depends-on "projectile")
 (depends-on "smartparens")
 (depends-on "yasnippet"))

;;; Cask ends here
