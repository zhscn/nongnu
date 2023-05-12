;;; haskell-tng-imenu-test.el --- Tests for imenu generation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

(ert-deftest haskell-tng-imenu-file-tests-layout ()
  (should (have-expected-imenu (testdata "src/layout.hs"))))

(ert-deftest haskell-tng-imenu-file-tests-indentation ()
  (should (have-expected-imenu (testdata "src/indentation.hs"))))

(ert-deftest haskell-tng-imenu-file-tests-medley ()
  (should (have-expected-imenu (testdata "src/medley.hs"))))

(defun have-expected-imenu (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (imenu "module")
     (with-output-to-string
       (pp imenu--index-alist)))
   "imenu"))

(provide 'haskell-tng-imenu-test)
;;; haskell-tng-imenu-test.el ends here
