;;; haskell-tng-compile-test.el --- Tests for compilation mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'dash)
(require 'faceup)

(require 'haskell-compile)

(require 'haskell-tng-mode)
(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(defun have-expected-errors (file)
  (with-temp-buffer
    (let ((output (current-buffer))
          ;; compilation-mode uses font-lock-face, not face
          (faceup-default-property 'font-lock-face)
          ;; maybe add compilation-message ?
          ;; maybe add overlays ?
          (faceup-properties '(font-lock-face)))
      (compilation-start
       (format "cat %s" file)
       'haskell-compilation-mode
       (lambda (_) output))

      (while compilation-in-progress
        (sit-for 0.01))
      (haskell-tng-compile:clean-output)

      ;; (font-lock-fontify-region (point-min) (point-max))
      ;; (--dotimes (point-max)
      ;;   (let ((p (+ 1 it)))
      ;;     (message "POINT=%s PROPS=%S" p (text-properties-at p))))

      (haskell-tng-testutils:assert-file-contents
       file
       output
       #'buffer-to-faceup-string
       "faceup"))))

(defun haskell-tng-compile:clean-output ()
  "Removes timestamps and local file paths"
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (kill-line 4)
    (goto-char (point-max))
    (kill-line -2)))

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
