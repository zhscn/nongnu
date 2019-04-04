;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-indent-file-tests ()
  ;; Four indentation regression tests are possible:
  ;;
  ;;   1. newline-and-indent when writing code
  ;;   2. ... with subsequent indent-line-function cycles
  ;;   3. indent-line-function at the beginning of an existing line
  ;;   4. ... with subsequent indent-line-function cycles
  ;;
  ;; Expectations could use lines of symbols such as | and . or digits to
  ;; indicate where the indentation(s) go. 1 and 2 are the most interesting so
  ;; could be combined into one test. 3 and 4 could also be combined.

  ;; (should (have-expected-newline-indent (testdata "src/layout.hs")))
  ;; (should (have-expected-indent (testdata "src/layout.hs")))

  ;; (should (have-expected-newline-indent (testdata "src/medley.hs")))
  ;; (should (have-expected-indent (testdata "src/medley.hs")))

  )

(defun haskell-tng-indent-test:newline-indents ()
  ;; FIXME
  )

(defun haskell-tng-indent-test:indents ()
  ;; FIXME
  )

(defun haskell-tng-indent-test:indents-to-string (indents)
  "INDENTS is a list of INDENT which are a non-empty list of
column numbers indicating the suggested indentation levels. The
head entry is the newline-and-indent and the rest are the
indent-line-function cycles."
  ;; FIXME
  )

(defun haskell-tng-indent-test:indent-to-string (indent)
  ;; FIXME
  )

(defun have-expected-newline-indent (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:newline-indents)))
   "newline-indent"))

(defun have-expected-indent (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:indents)))
   "indent"))

;;; haskell-tng-indent-test.el ends here
