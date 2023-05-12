;;; haskell-tng-layout-test.el --- Tests for significant whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

(ert-deftest haskell-tng-layout-file-tests-layout ()
  ;; the Haskell2010 test case
  (should (have-expected-layout (testdata "src/layout.hs")))
  (should (have-expected-layout-reverse (testdata "src/layout.hs"))))

(ert-deftest haskell-tng-layout-file-tests-indentation ()
  (should (have-expected-layout (testdata "src/indentation.hs")))
  (should (have-expected-layout-reverse (testdata "src/indentation.hs"))))

(ert-deftest haskell-tng-layout-file-tests-medley ()
  (should (have-expected-layout (testdata "src/medley.hs")))
  (should (have-expected-layout-reverse (testdata "src/medley.hs"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing utilities

(defun haskell-tng--layout-test-parse-to-string (&optional reverse)
  (if reverse
      (goto-char (point-max))
    (goto-char 0))
  (let (tokens exit)
    (while (not exit)
      (when-let (virtuals (haskell-tng--layout-virtuals-at-point))
        (push (s-join "" virtuals) tokens))
      (if (or (and (not reverse) (eobp))
              (and reverse (bobp)))
          (setq exit t)
        (push (string (if reverse (char-before) (char-after))) tokens)
        (forward-char (if reverse -1 1))))
    (s-join "" (if reverse tokens (reverse tokens)))))

(defun have-expected-layout (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   #'haskell-tng--layout-test-parse-to-string
   "layout"))
(defun have-expected-layout-reverse (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda () (haskell-tng--layout-test-parse-to-string t))
   "layout"))

(ert-deftest haskell-tng-layout-cache-ordering-tests ()
  (with-temp-buffer
    (insert-file-contents (testdata "src/layout.hs"))
    (haskell-tng-mode)

    ;; Differs from the regular layout test because the cache is empty. We could
    ;; do a more intensive version of this by randomly sampling the points.
    (goto-char 94)
    (should
     (equal
      (haskell-tng--layout-virtuals-at-point)
      '("{")))))

(ert-deftest haskell-tng-layout-cache-invalidation-tests ()
  (with-temp-buffer
    (insert-file-contents (testdata "src/layout.hs"))
    (haskell-tng-mode)

    (goto-char 317)
    (should
     (equal
      (haskell-tng--layout-virtuals-at-point)
      '(";")))

    (insert " ")
    (goto-char 317)
    (should
     (not
      (haskell-tng--layout-virtuals-at-point)))))

;;; haskell-tng-layout-test.el ends here
