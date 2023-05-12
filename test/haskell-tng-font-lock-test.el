;;; haskell-tng-font-lock-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

(defun have-expected-faces (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   #'buffer-to-faceup-string
   "faceup"))

;; to generate .faceup files, use faceup-view-buffer
(ert-deftest haskell-tng-font-lock-file-tests-medley ()
  (should (have-expected-faces (testdata "src/medley.hs"))))

(ert-deftest haskell-tng-font-lock-file-tests-layout ()
  (should (have-expected-faces (testdata "src/layout.hs"))))

(ert-deftest haskell-tng-font-lock-file-tests-indentation ()
  (should (have-expected-faces (testdata "src/indentation.hs"))))

(ert-deftest haskell-tng-font-lock-file-tests-indentation1 ()
  (should (have-expected-faces (testdata "src/indentation-options1.hs"))))

(ert-deftest haskell-tng-font-lock-file-tests-indentation2 ()
  (should (have-expected-faces (testdata "src/indentation-options2.hs"))))

;;; haskell-tng-font-lock-test.el ends here
