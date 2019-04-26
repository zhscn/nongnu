;;; haskell-tng-compile-test.el --- Tests for compilation mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'dash)
(require 'faceup)

(require 'haskell-tng-mode)
(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(defun have-expected-errors (file)
  (with-temp-buffer
    (let ((output (current-buffer))
          (faceup-default-property 'font-lock-face)
          (faceup-properties '(font-lock-face)))
      (compilation-start
       (format "cat %s" file)
       'haskell-tng-compilation-mode
       (lambda (_) output))

      (while compilation-in-progress
        (sit-for 0.01))
      (haskell-tng-compile:clean-output)

      (haskell-tng-testutils:assert-file-contents
       file
       output
       #'buffer-to-faceup-string
       "faceup"))))

;; TODO locally scope this override to this test. Would also be good to override
;;      abbreviate-file-name and current-time-string
(defun compilation-handle-exit (_1 _2 _3)
  "Overrides the default behaviour to remove noise")

(defun haskell-tng-compile:clean-output ()
  "Removes timestamps and local file paths"
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (kill-line 4)
    ;; not needed with the custom compilation-handle-exit
    ;;(goto-char (point-max))
    ;;(kill-line -2)
    ))

;; to generate .faceup files, use faceup-view-buffer
(ert-deftest haskell-tng-compile-errors-file-tests ()
  (should (have-expected-errors (testdata "src/ghc-8.4.4-error.compile")))
  (should (have-expected-errors (testdata "src/ghc-8.4.4-errorspan.compile"))))

(ert-deftest haskell-tng-compile-warnings-file-tests ()
  (should (have-expected-errors (testdata "src/ghc-8.4.4-warning.compile")))
  (should (have-expected-errors (testdata "src/ghc-8.4.4-warningspan.compile"))))

(ert-deftest haskell-tng-compile-hspec-file-tests ()
  (should (have-expected-errors (testdata "src/hspec-failure.compile"))))

(ert-deftest haskell-tng-compile-tasty-file-tests ()
  ;; TODO assert on ansi colours, implemented with overlays
  ;;
  ;; There is an overlay here:
  ;;  From 11527 to 11714
  ;;   face                 (foreground-color . "red3")
  ;;   insert-behind-hooks  (ansi-color-freeze-overlay)
  ;;   modification-hooks   (ansi-color-freeze-overlay)
  ;; There are text properties here:
  ;;   fontified            t
  (should (have-expected-errors (testdata "src/tasty-failure.compile"))))

;;; haskell-tng-compile-test.el ends here
