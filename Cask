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

(source melpa-stable)
(source melpa) ;; for faceup

(package-file "haskell-tng-mode.el")

(development
 (depends-on "company" "0.9.12")
 ;;(depends-on "elsa") ;; cask exec elsa FILE
 (depends-on "faceup") ;; no stable release
 (depends-on "ert-runner" "0.7.0")
 (depends-on "shut-up" "0.3.2")
 (depends-on "projectile" "2.1.0")
 (depends-on "smartparens" "1.11.0")
 (depends-on "yasnippet" "0.14.0"))

;;; Cask ends here
