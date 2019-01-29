;;; haskell-tng-lexer-test.el --- Tests for navigation and indentation -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-smie-file-tests ()
  ;; FIXME tests for s-expressions
  ;; (should (have-expected-forward-lex (testdata "src/medley.hs")))
  ;; (should (have-expected-forward-lex (testdata "src/layout.hs")))
  )

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-lexer-test.el ends here
