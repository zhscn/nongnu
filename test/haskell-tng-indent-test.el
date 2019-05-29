;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'ert-x)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

  ;; Three indentation regression tests are possible:
  ;;
  ;;   1. newline-and-indent with the rest of the file deleted (append)
  ;;   2. newline-and-indent with the rest of the file intact (insert)
  ;;   3. indent-line-function at the beginning of each line (re-indent)
  ;;
  ;; each maybe with alternative indentation suggestions.
  ;;
  ;; Expectations could use lines of symbols such as | and . or digits to
  ;; indicate where the indentation(s) go.
  ;;
  ;; Test 1 involves a lot of buffer refreshing and will be very slow.

(ert-deftest haskell-tng-newline-indent-file-tests ()
  (should (have-expected-newline-indent-insert (testdata "src/indentation.hs")))

  ;; (should (have-expected-newline-indent-insert (testdata "src/layout.hs")))
  ;; (should (have-expected-newline-indent-insert (testdata "src/medley.hs")))
  )

(ert-deftest haskell-tng-reindent-file-tests ()
  (should (have-expected-reindent-insert (testdata "src/indentation.hs")))

  ;; (should (have-expected-reindent-insert (testdata "src/layout.hs")))
  ;; (should (have-expected-reindent-insert (testdata "src/medley.hs")))
  )

(defun current-line-string ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (- (line-beginning-position 2) 1)))

(defun haskell-tng-indent-test:indent-insert (return-mode)
  ;; FIXME the slow append test
  (let (indents)
    (while (not (eobp))
      ;; the command loop is necessary for this/last-command
      (cl-flet ((RET ()
                     (end-of-line)
                     (ert-simulate-command '(newline-and-indent))
                     (current-column))
                (TAB ()
                     (ert-simulate-command '(indent-for-tab-command))
                     (current-column)))

        (let ((orig (current-indentation))
              (line (current-line-string))
              (prime (if return-mode (RET) (TAB)))
              alts)
          (while (and (TAB)
                      (not (eq (current-column) prime))
                      (not (member (current-column) alts)))
            (push (current-column) alts))
          (push `(, return-mode ,line . (,prime . ,(reverse alts))) indents)
          ;; unfortunately killing resets this-command so we can't test double
          ;; newline insertions, which could accidentally trigger alts only.
          (if return-mode
              (kill-whole-line)
            (indent-line-to orig)
            (ert-simulate-command '(forward-line))))))
    (reverse indents)))

(defun haskell-tng-indent-test:indents-to-string (indents)
  "INDENTS is a list of INDENT.

INDENT is a non-empty list of (RETURN-MODE . (LINE . (INDENT .
ALTS))) where RETURN-MODE is t for newline insertions (i.e. LINE
is a string of the previous line) and nil for reindent (i.e. LINE
is a string of the current line).

INDENT is the integer suggested next line indentation column and
ALTS is a list of integer alternative indentations."
  (s-join "\n" (-flatten
                (-map #'haskell-tng-indent-test:indent-to-string indents))))

(defun haskell-tng-indent-test:indent-to-string (indent)
  (let* ((return-mode (car indent))
         (line (cadr indent))
         (prime (caddr indent))
         (alts (cdddr indent))
         (widest (-max (cddr indent)))
         repr)
    (--dotimes (+ 1 widest)
      (push
       (cond
        ((eq it prime) "v")
        ((member it alts)
         (let ((i (-elem-index it alts)))
           (if (< i 9)
               (number-to-string (+ 1 i))
             ".")))
        (t " "))
       repr))
    (let ((indents (s-join "" (reverse repr))))
      (if return-mode
          (list line indents)
        (list indents line)))))

(defun have-expected-newline-indent-insert (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:indent-insert t)))
   "insert.indent"))

(defun have-expected-reindent-insert (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:indent-insert nil)))
   "reindent"))

;;; haskell-tng-indent-test.el ends here
