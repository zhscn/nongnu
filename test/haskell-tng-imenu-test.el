;;; haskell-tng-imenu-test.el --- Tests for imenu generation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-lexer-file-tests:layout ()
  (should (have-expected-imenu (testdata "src/layout.hs"))))

(defun have-expected-imenu (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (imenu "*Rescan*")
     (let ((entries imenu--index-alist))
       (with-temp-buffer
         (pp entries (current-buffer))
         (buffer-string))))
   "imenu"))

(provide 'haskell-tng-imenu-test)
;;; haskell-tng-imenu-test.el ends here
