;;; haskell-tng-smie-test.el --- Tests for fontification -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-mode)

(require 'ert)
(require 's)

(defmacro haskell-tng-smie:this-lisp-directory ()
  (expand-file-name
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory)))

(defvar smie-forward-token-function)
;; TODO make this behave consistently interactive / non-interactive
;; (maybe wrap it)
(defun haskell-tng-smie:forward-token-to-buffer ()
  "Forward lex the current buffer using SMIE lexer and dump to a buffer."
  (interactive)
  (let* ((buf (current-buffer))
         (work (generate-new-buffer (buffer-name))))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((start (point))
             (token (apply smie-forward-token-function ())))
        (when (= (point) start)
          (unless (or (s-present? token) (eobp))
            (setq token (char-to-string (char-after (point)))))
          (forward-char))
        (with-current-buffer work
          (insert token "\n"))))
    (if (called-interactively-p 'interactive)
      (switch-to-buffer work)
      work)))

(defun have-expected-forward-lex (file)
  (let* ((backup-inhibited t)
         (filename (expand-file-name
                    file
                    (haskell-tng-smie:this-lisp-directory)))
         (golden (concat filename ".forward"))
         (expected (with-temp-buffer
                     (insert-file-contents golden)
                     (buffer-string)))
         (lexed (with-temp-buffer
                  ;; TODO load this buffer correctly, to id the mode
                  (haskell-tng-mode)
                  (insert-file-contents filename)
                  (haskell-tng-smie:forward-token-to-buffer)))
         (got (with-current-buffer lexed (buffer-string))))
    (unwind-protect
        (or (s-equals? got expected)
            ;; TODO make this a parameter
            ;; writes out the new version on failure
            (progn
              (with-current-buffer lexed
                (write-file golden))
              nil))
      (kill-buffer lexed))))

(ert-deftest haskell-tng-smie-file-tests ()
  (should (have-expected-forward-lex "faces/medley.hs")))

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-smie-test.el ends here
