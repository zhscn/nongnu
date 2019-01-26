;;; haskell-tng-font-lock-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'faceup)

(require 'haskell-tng-mode)
(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

;; Not using `faceup-defexplainer' because it doesn't write over files.
(defun haskell-tng-font-lock-test:parse-to-string ()
  (font-lock-fontify-region (point-min) (point-max))
  (faceup-markup-buffer))

(defun have-expected-faces (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   #'haskell-tng-font-lock-test:parse-to-string
   "faceup"))

;; to generate .faceup files, use faceup-view-buffer
(ert-deftest haskell-tng-font-lock-file-tests ()
  (should (have-expected-faces "src/medley.hs"))

  (should (have-expected-faces "src/layout.hs"))
  )

;;; haskell-tng-font-lock-test.el ends here
