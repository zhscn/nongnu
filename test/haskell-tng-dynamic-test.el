;;; haskell-tng-dynamic-test.el --- tests that run dynamic commands -*- lexical-binding: t -*-

;; Copyright (C) 2020 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  These tests load a user-file and search for comments containing dynamic
;;  interactive commands, simulating user input, which are then compared to a
;;  file on disk containing the expected output.
;;
;;; Code:

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

(ert-deftest haskell-tng-dynamic-file-tests ()
  (should (have-expected-dynamic-output (testdata "src/hsinspect.hs"))))

(defun have-expected-dynamic-output (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   #'haskell-tng-dynamic-test-to-string
   "dynamic"))

(defun haskell-tng-dynamic-test-to-string ()
  (goto-char (point-min))

  (while (re-search-forward (rx word-start "RUN" word-end) nil t)
    (when (is-comment-at-point)
      (push-mark (point) t)
      (shut-up
       (eval (read (current-buffer))))
      (pop-mark)))
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'haskell-tng-dynamic-test)
;;; haskell-tng-dynamic-test.el ends here
