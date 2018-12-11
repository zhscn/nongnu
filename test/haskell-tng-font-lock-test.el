;;; haskell-tng-font-lock-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-mode)

(require 'ert)
(require 'faceup)

(defun have-expected-faces (file)
  (faceup-test-font-lock-file
   'haskell-tng-mode
   (expand-file-name
    file
    (eval-when-compile (faceup-this-file-directory)))))
(faceup-defexplainer have-expected-faces)

(ert-deftest haskell-tng-font-lock-file-tests ()
  (should (have-expected-faces "faces/medley.hs")))

;;; haskell-tng-font-lock-test.el ends here
