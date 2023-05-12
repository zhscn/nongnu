;;; haskell-tng-syntax-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

(defun have-expected-syntax (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   #'buffer-to-syntax-string
   "syntax"))

(defun buffer-to-syntax-string ()
  (goto-char (point-max))
  (syntax-propertize (point))
  (let (codes)
    (while (not (bobp))
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table
      (when-let (class (syntax-class (syntax-after (point))))
        (when (looking-at (rx eol))
          (push "\n" codes))
        (push (pcase class
                (0 " ")
                (1 ".")
                (2 "w")
                (3 "_")
                (4 "(")
                (5 ")")
                (6 "'")
                (7 "\"")
                (8 "$")
                (9 "\\")
                (10 "//")
                (11 "<")
                (12 ">")
                (13 "@")
                (14 "!")
                (15 "|"))
              codes))
      (forward-char -1))
    (s-join "" codes)))

;; to generate .faceup files, use faceup-view-buffer
(ert-deftest haskell-tng-syntax-file-tests-medley ()
  (should (have-expected-syntax (testdata "src/medley.hs"))))

(ert-deftest haskell-tng-syntax-file-tests-layout ()
  (should (have-expected-syntax (testdata "src/layout.hs"))))

(ert-deftest haskell-tng-syntax-file-tests-indentation ()
  (should (have-expected-syntax (testdata "src/indentation.hs"))))

;;; haskell-tng-syntax-test.el ends here
