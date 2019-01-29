;;; haskell-tng-smie-test.el --- Tests for navigation and indentation -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

;; copy/pasta of `smie-indent-forward-token' but rendering lexed tokens in a way
;; more ammenable to regression testing (e.g. syntax table usage)
(defun haskell-tng-smie-test:indent-forward-token ()
  (let ((tok (funcall smie-forward-token-function)))
    (cond
     ((< 0 (length tok)) tok)
     ((eobp) nil)
     ((looking-at (rx (| (syntax open-parenthesis)
                         (syntax close-parenthesis))))
      (concat "_" (haskell-tng-smie:last-match)))
     ((looking-at (rx (| (syntax string-quote)
                         (syntax string-delimiter))))
      (let ((start (point)))
        (forward-sexp 1)
        (concat "_" (buffer-substring-no-properties start (point)))))
     (t (error "Bumped into unknown token")))))

;; same as above, but for `smie-indent-backward-token'
(defun haskell-tng-smie-test:indent-backward-token ()
  (let ((tok (funcall smie-backward-token-function)))
    (cond
     ((< 0 (length tok)) tok)
     ((bobp) nil)
     ((looking-back (rx (| (syntax open-parenthesis)
                           (syntax close-parenthesis)))
                    (- (point) 1))
      (concat "_" (haskell-tng-smie:last-match 'reverse)))
     ((looking-back (rx (| (syntax string-quote)
                           (syntax string-delimiter)))
                    (- (point) 1))
      (let ((start (point)))
        (backward-sexp 1)
        (concat "_" (buffer-substring-no-properties (point) start))))
     (t (error "Bumped into unknown token")))))

(defun haskell-tng-smie-test:tokens (&optional reverse)
  "Lex the current buffer using SMIE and return the list of lines,
where each line is a list of tokens.

When called interactively, shows the tokens in a buffer."
  (let ((lines (list nil))
        quit)
    (goto-char (if reverse (point-max) (point-min)))
    (while (not quit)
      (let* ((start (point))
             (token (if reverse
                        (haskell-tng-smie-test:indent-backward-token)
                      (haskell-tng-smie-test:indent-forward-token))))
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

(defun haskell-tng-smie-test:tokens-to-string (lines)
  (concat (s-join "\n" (--map (s-join " " it) lines)) "\n"))

(defun have-expected-forward-lex (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda () (haskell-tng-smie-test:tokens-to-string
          (haskell-tng-smie-test:tokens)))
   "lexer"))

(defun have-expected-backward-lex (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda () (haskell-tng-smie-test:tokens-to-string
          (haskell-tng-smie-test:tokens 'reverse)))
   "lexer"))

(ert-deftest haskell-tng-smie-file-tests ()
  ;;(should (have-expected-forward-lex (testdata "src/medley.hs")))
  ;;(should (have-expected-forward-lex (testdata "src/layout.hs")))

  (should (have-expected-backward-lex (testdata "src/medley.hs")))
  (should (have-expected-backward-lex (testdata "src/layout.hs")))
  )

(ert-deftest haskell-tng-smie-state-invalidation-tests ()
  (with-temp-buffer
    (insert-file-contents (testdata "src/layout.hs"))
    (haskell-tng-mode)

    ;; three parses at this position will produce a virtual token and a real
    ;; token, then move the point for another token.
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "stkToLst"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "_("))

    ;; repeating the above, but with a user edit, should reset the state
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (save-excursion
      (goto-char (point-max))
      (insert " "))
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "stkToLst"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "_("))

    ;; repeating again, but jumping the lexer, should reset the state
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (goto-char 327)
    (should (equal (haskell-tng-smie-test:indent-forward-token) "MkStack"))
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "stkToLst"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) "_("))

    ;; repeating those tests, but for the backward lexer
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_]"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_["))

    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (save-excursion
      (goto-char (point-max))
      (insert " "))
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_]"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_["))

    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (goto-char 327)
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_("))
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_]"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) "_["))

    ;; jumping between forward and backward at point should reset state
    (goto-char 317)
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-forward-token) ";"))
    (should (equal (haskell-tng-smie-test:indent-backward-token) ";"))
    ))

;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63

;;; haskell-tng-smie-test.el ends here
