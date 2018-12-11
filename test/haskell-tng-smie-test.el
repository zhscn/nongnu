;;; haskell-tng-smie-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-mode)

(require 'ert)
(require 'faceup)

(defun haskell-tng-smie:lex-forward-buffer ()
  (let* ((buf (current-buffer))
         (work (switch-to-buffer (concat (buffer-file-name) ".lexer.forward"))))
    (switch-to-buffer buf)
    (goto-char (point-min))

    ;; FIXME progress through the buf writing the returned values to work
    ;; maybe with a character to indicate invocations, maybe newlines.

    ))

(defun have-expected-forward-lexer (file)
  (let* ((filename (expand-file-name
                    file
                    (eval-when-compile (faceup-this-file-directory))))
         (golden (concat filename ".lexer.forward")))

    ;; FIXME run the lex-forward-buffer and compare the result with the version
    ;; on disk, perhaps a trimmed diff.

    ))

(ert-deftest haskell-tng-smie-file-tests ()
  (should (have-expected-forward-lexer "faces/medley.hs")))

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-smie-test.el ends here
