;;; haskell-tng-font-lock-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'faceup)

(require 'haskell-tng-mode)
(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(defun have-expected-faces (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   #'buffer-to-faceup-string
   "faceup"))

;; to generate .faceup files, use faceup-view-buffer
(ert-deftest haskell-tng-font-lock-file-tests:medley ()
  (should (have-expected-faces (testdata "src/medley.hs"))))

(ert-deftest haskell-tng-font-lock-file-tests:layout ()
  (should (have-expected-faces (testdata "src/layout.hs"))))

;;; haskell-tng-font-lock-test.el ends here
