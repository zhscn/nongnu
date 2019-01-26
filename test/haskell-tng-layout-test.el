;;; haskell-tng-layout-test.el --- Tests for significant whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(defun haskell-tng-layout-test:parse-to-string ()
  (goto-char 0)
  (let (tokens exit)
    (while (not exit)
      (when-let (virtuals (haskell-tng-layout:virtuals-at-point))
        (push (s-join "" virtuals) tokens))
      (if (eobp)
          (setq exit t)
        (push (string (char-after)) tokens)
        (forward-char)))
   (s-join "" (reverse tokens))))

(defun have-expected-layout (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   #'haskell-tng-layout-test:parse-to-string
   "layout"))

(ert-deftest haskell-tng-layout-file-tests ()
  ;; the Haskell2010 test case
  (should (have-expected-layout "src/layout.hs"))

  (should (have-expected-layout "src/medley.hs"))
  )

;;; haskell-tng-layout-test.el ends here
