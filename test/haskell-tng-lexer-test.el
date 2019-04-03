;;; haskell-tng-lexer-test.el --- Tests for the SMIE lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-lexer-file-tests ()
  (should (have-expected-forward-lex (testdata "src/layout.hs")))
  (should (have-expected-forward-lex (testdata "src/medley.hs")))

  (should (have-expected-backward-lex (testdata "src/layout.hs")))
  (should (have-expected-backward-lex (testdata "src/medley.hs")))
  )

(ert-deftest haskell-tng-lexer-state-invalidation-tests ()
  (with-temp-buffer
    (insert-file-contents (testdata "src/layout.hs"))
    (haskell-tng-mode)

    ;; three parses at this position will produce a virtual token and a real
    ;; token, then move the point for another token.
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "VARID"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "«"))

    ;; repeating the above, but with a user edit, should reset the state
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (save-excursion
      (goto-char (point-max))
      (insert " "))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "VARID"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "«"))

    ;; repeating again, but jumping the lexer, should reset the state
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (goto-char 327)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "CONID"))
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "VARID"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) "«"))

    ;; repeating those tests, but for the backward lexer
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) "[]"))

    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (save-excursion
      (goto-char (point-max))
      (insert " "))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) "[]"))

    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (goto-char 327)
    (should (equal (haskell-tng-lexer-test:indent-backward-token) "«"))
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) "[]"))

    ;; jumping between forward and backward at point should reset state
    (goto-char 317)
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-lexer-test:indent-backward-token) ";"))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMIE testing utilities

;; copy/pasta of `smie-indent-forward-token' but rendering lexed tokens in a way
;; more ammenable to regression testing (e.g. syntax table usage)
(defun haskell-tng-lexer-test:indent-forward-token ()
  (let ((tok (funcall smie-forward-token-function)))
    (cond
     ((< 0 (length tok)) tok)
     ((eobp) nil)
     ((looking-at (rx (syntax open-parenthesis)))
      (haskell-tng-lexer:last-match)
      "«")
     ((looking-at (rx (syntax close-parenthesis)))
      (haskell-tng-lexer:last-match)
      "»")
     ((looking-at (rx (| (syntax string-quote)
                         (syntax string-delimiter))))
      (forward-sexp 1)
      "§")
     (t (error "Unknown token: '%s' with '%S'"
               (string (char-after))
               (syntax-after (point)))))))

;; same as above, but for `smie-indent-backward-token'
(defun haskell-tng-lexer-test:indent-backward-token ()
  (let ((tok (funcall smie-backward-token-function)))
    (cond
     ((< 0 (length tok)) tok)
     ((bobp) nil)
     ((looking-back (rx (syntax open-parenthesis)) (- (point) 1))
      (haskell-tng-lexer:last-match 'reverse)
      "«")
     ((looking-back (rx (syntax close-parenthesis)) (- (point) 1))
      (haskell-tng-lexer:last-match 'reverse)
      "»")
     ((looking-back (rx (| (syntax string-quote)
                           (syntax string-delimiter)))
                    (- (point) 1))
      (backward-sexp 1)
      "§")
     (t (error "Unknown token: '%s' with '%S'"
               (string (char-before))
               (progn
                 (backward-char)
                 (syntax-after (point))))))))

(defun haskell-tng-lexer-test:tokens (&optional reverse)
  "Lex the current buffer using SMIE and return the list of lines,
where each line is a list of tokens.

When called interactively, shows the tokens in a buffer."
  (let (lines quit)
    (push nil lines)
    (goto-char (if reverse (point-max) (point-min)))
    (while (not quit)
      (let* ((start (point))
             (token (if reverse
                        (haskell-tng-lexer-test:indent-backward-token)
                      (haskell-tng-lexer-test:indent-forward-token))))
        (let ((line-diff (- (line-number-at-pos (point))
                            (line-number-at-pos start))))
          (unless (= line-diff 0)
            (setq lines (append (-repeat (abs line-diff) nil) lines))))
        (if (and (not token) (if reverse (bobp) (eobp)))
            (setq quit 't)
          (unless (s-blank? token)
            (push token (car lines))))))
    (if reverse
        lines
      (reverse (--map (reverse it) lines)))))

(defun haskell-tng-lexer-test:tokens-to-string (lines)
  (concat (s-join "\n" (--map (s-join " " it) lines)) "\n"))

(defun have-expected-forward-lex (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda () (haskell-tng-lexer-test:tokens-to-string
          (haskell-tng-lexer-test:tokens)))
   "lexer"))

(defun have-expected-backward-lex (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda () (haskell-tng-lexer-test:tokens-to-string
          (haskell-tng-lexer-test:tokens 'reverse)))
   "lexer"))

;;; haskell-tng-lexer-test.el ends here
