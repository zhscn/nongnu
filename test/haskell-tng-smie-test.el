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

;; FIXME return a list of lines, each a list of tokens. It produces a much
;; cleaner output for regression testing.
(defun haskell-tng-smie:forward-tokens (&optional display)
  "Forward lex the current buffer using SMIE lexer and return the list of tokens.

When called interactively, shows the tokens in a buffer."
  (interactive '(t))
  (defvar smie-forward-token-function)
  (let* ((tokens '()))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((start (point))
             (token (funcall smie-forward-token-function)))
        (when (and (not token) (= (point) start))
          (setq token (car (smie-indent-forward-token)))
          (when (= start (point)) (forward-char 1))
          (unless token
            (setq token (buffer-substring-no-properties start (point))))
          ;; differentiate that these tokens come from the syntax table
          (setq token (concat "SYNTAX_" token)))
        (unless (member token '(nil ""))
          (push token tokens))))
    (if display
        (haskell-tng-smie:display-tokens tokens)
      (nreverse tokens))))

(defun haskell-tng-smie:tokens-to-string (tokens)
  (concat (mapconcat #'identity tokens "\n") "\n"))

(defun haskell-tng-smie:display-tokens (tokens)
  (with-current-buffer (get-buffer-create "*Haskell-TNG-SMIE-test*")
    (insert (haskell-tng-smie:tokens-to-string tokens))
    (pop-to-buffer (current-buffer))))

(defun have-expected-forward-lex (file)
  (let* ((backup-inhibited t)
         (filename (expand-file-name
                    file
                    (haskell-tng-smie:this-lisp-directory)))
         (golden (concat filename ".lexer"))
         (expected (with-temp-buffer
                     (insert-file-contents golden)
                     (buffer-string)))
         (lexed (with-temp-buffer
                  (insert-file-contents filename)
                  ;; TODO load this buffer correctly, to id the mode
                  (haskell-tng-mode)
                  (haskell-tng-smie:forward-tokens)))
         (got (haskell-tng-smie:tokens-to-string lexed)))
    (or (equal got expected)
        ;; TODO make this a parameter
        ;; writes out the new version on failure
        (progn
          (write-region got nil golden)
          nil))))

;; TODO the backwards test should simply assert consistency

(ert-deftest haskell-tng-smie-file-tests ()
  (should (have-expected-forward-lex "faces/medley.hs"))
  ;; FIXME this is the real test
  ;;(should (have-expected-forward-lex "lexer/layout.hs"))
  )

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-smie-test.el ends here
