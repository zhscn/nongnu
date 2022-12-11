;;; recomplete-test.el --- Highlight indent scope test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-recomplete
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This is a test for `recomplete'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `emacs -batch -l tests/recomplete-test.el -f ert-run-tests-batch-and-exit'
;;

;;; Code:

(require 'ert)

;; ---------------------------------------------------------------------------
;; Setup Environment

(setq recomplete-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path recomplete-basedir)
(require 'recomplete)

;; ---------------------------------------------------------------------------
;; Test Internal Replacement Function

(defmacro ert-replace-in-region (id str-src args output replaced-range)
  `(ert-deftest ,id ()
     "Generic test."
     (with-temp-buffer
       (insert ,str-src)
       (let ((result (apply 'recomplete-replace-in-region ,args)))
         (should (equal ,output (buffer-substring-no-properties (point-min) (point-max))))
         (should (equal result ,replaced-range))))))

(ert-replace-in-region replace-in-region-nop "" '("" 1 1) "" (cons 1 1))
(ert-replace-in-region replace-in-region-single-same "x" '("x" 1 2) "x" (cons 1 1))
(ert-replace-in-region replace-in-region-single-different "x" '("y" 1 2) "y" (cons 1 2))

(ert-replace-in-region replace-in-region-word-same "hello" '("hello" 1 6) "hello" (cons 1 1))
(ert-replace-in-region replace-in-region-word-same-start "HELLO" '("Hello" 1 6) "Hello" (cons 2 6))
(ert-replace-in-region replace-in-region-word-same-end "HELLO" '("hELLO" 1 6) "hELLO" (cons 1 2))
(ert-replace-in-region
 replace-in-region-word-different-1
 "hello"
 '("world" 1 6)
 "world"
 (cons 1 6))
(ert-replace-in-region
 replace-in-region-word-different-2
 "hello"
 '("HELLO" 1 6)
 "HELLO"
 (cons 1 6))
(ert-replace-in-region
 replace-in-region-word-contract
 "commpletion"
 '("completion" 1 12)
 "completion"
 (cons 3 3))
(ert-replace-in-region
 replace-in-region-word-expand
 "copletion"
 '("completion" 1 10)
 "completion"
 (cons 3 4))

(ert-replace-in-region
 replace-in-region-word-mix-same
 "pre hello post"
 '("hello" 5 10)
 "pre hello post"
 (cons 5 5))
(ert-replace-in-region
 replace-in-region-word-mix-same-start
 "pre HELLO post"
 '("Hello" 5 10)
 "pre Hello post"
 (cons 6 10))
(ert-replace-in-region
 replace-in-region-word-mix-same-end
 "pre HELLO post"
 '("hELLO" 5 10)
 "pre hELLO post"
 (cons 5 6))
(ert-replace-in-region
 replace-in-region-word-mix-different-1
 "pre hello post"
 '("world" 5 10)
 "pre world post"
 (cons 5 10))
(ert-replace-in-region
 replace-in-region-word-mix-different-2
 "pre hello post"
 '("HELLO" 5 10)
 "pre HELLO post"
 (cons 5 10))
(ert-replace-in-region
 replace-in-region-word-mix-contract
 "pre commpletion post"
 '("completion" 5 16)
 "pre completion post"
 (cons 7 7))
(ert-replace-in-region
 replace-in-region-word-mix-expand
 "pre copletion post"
 '("completion" 5 14)
 "pre completion post"
 (cons 7 8))


(provide 'recomplete-test)
;;; recomplete-test.el ends here
