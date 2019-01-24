;;; haskell-tng-smie-test.el --- Tests for navigation and indentation -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-mode)

(require 'dash)
(require 'ert)
(require 's)

(defmacro haskell-tng-smie:this-lisp-directory ()
  (expand-file-name
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory)))

;; copy/pasta of `smie-indent-forward-token' but rendering lexed tokens in a way
;; more ammenable to regression testing (e.g. syntax table usage)
(defun haskell-tng-smie:indent-forward-token ()
  (let ((tok (funcall smie-forward-token-function)))
    (cond
     ((< 0 (length tok)) tok)
     ((looking-at (rx (| (syntax open-parenthesis)
                         (syntax close-parenthesis))))
      (concat "_" (haskell-tng-smie:last-match)))
     ((looking-at (rx (| (syntax string-quote)
                         (syntax string-delimiter))))
      (let ((start (point)))
        (forward-sexp 1)
        (concat "_" (buffer-substring-no-properties start (point)))))
     ((eobp) nil)
     (t (error "Bumped into unknown token")))))

(defun haskell-tng-smie:forward-tokens (&optional display)
  "Forward lex the current buffer using SMIE lexer and return the list of lines,
where each line is a list of tokens.

When called interactively, shows the tokens in a buffer."
  (interactive '(t))
  (defvar smie-forward-token-function)
  (let* ((lines '(())))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((start (point))
             (token (haskell-tng-smie:indent-forward-token)))
        (let ((line-diff (- (line-number-at-pos (point))
                            (line-number-at-pos start))))
          (unless (<= line-diff 0)
            (setq lines (append (-repeat line-diff nil) lines))))
        (unless (s-blank? token)
          (push token (car lines)))))
    (let ((ordered (reverse (--map (reverse it) lines))))
      (if display
          (haskell-tng-smie:display-tokens ordered)
        ordered))))

(defun haskell-tng-smie:tokens-to-string (lines)
  (concat (s-join "\n" (--map (s-join " " it) lines)) "\n"))

(defun haskell-tng-smie:display-tokens (lines)
  (with-current-buffer (get-buffer-create "*Haskell-TNG-SMIE-test*")
    (insert (haskell-tng-smie:tokens-to-string lines))
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
        ;; TODO make this a setting
        ;; writes out the new version on failure
        (progn
          (write-region got nil golden)
          nil))))

;; TODO the backwards test should simply assert consistency

(ert-deftest haskell-tng-smie-file-tests ()
  (should (have-expected-forward-lex "src/medley.hs"))
  (should (have-expected-forward-lex "src/layout.hs"))
  )

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-smie-test.el ends here
