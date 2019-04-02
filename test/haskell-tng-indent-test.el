;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-indent-file-tests ()
  ;; FIXME four kinds of indentation test:
  ;;
  ;;   1. newline-and-indent when writing code
  ;;   2. ... with subsequent indent-line-function cycles
  ;;   3. indent-line-function at the beginning of an existing line
  ;;   4. ... with subsequent indent-line-function cycles
  ;;
  ;; Expectations could use lines of | and . to indicate where the
  ;; indentation(s) go. 1 and 2 are the most interesting so could be combined
  ;; into one test. 3 and 4 could also be combined.
  ;;
  ;; (should (have-expected-newline-indent (testdata "src/layout.hs")))
  ;; (should (have-expected-indent (testdata "src/layout.hs")))

  )

;;; haskell-tng-indent-test.el ends here
