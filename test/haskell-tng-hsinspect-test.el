;;; haskell-tng-hsinspect-test.el --- Tests for hsinspect features -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)

(require 'haskell-tng-mode)
(require 'haskell-tng-hsinspect)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-hsinspect-test-qualify-latest ()
  (let ((imports
         (haskell-tng--util-read
          (testdata "data/hsinspect-0.0.8-imports.sexp.gz"))))

    ;; function search
    (should
     (equal
      (haskell-tng--hsinspect-qualify imports "contramap")
      "Data.Functor.Contravariant.contramap"))

    ;; operator search
    (should
     (equal
      (haskell-tng--hsinspect-qualify imports ">$<")
      "Data.Functor.Contravariant.>$<"))

    ;; TODO type search
    ;; TODO constructor search
    ))

(ert-deftest haskell-tng-hsinspect-test-import-candidates-latest ()
  (let ((index
         (haskell-tng--util-read
          (testdata "data/hsinspect-0.0.8-index.sexp.gz"))))

    ;; function search
    (should
     (equal
      (haskell-tng--hsinspect-import-candidates index "throw")
      '(("Control.Exception.Base" . "throw")
        ("Control.Exception" . "throw")
        ("GHC.Exception" . "throw"))))

    ;; operator search
    (should
     (equal
      (haskell-tng--hsinspect-import-candidates index ">$<")
      '(("Data.Functor.Contravariant" . ">$<"))))

    ;; TODO type search
    ;; TODO constructor search
    ))

;; TODO tests for 0.0.7 data

;;; haskell-tng-hsinspect-test.el ends here
