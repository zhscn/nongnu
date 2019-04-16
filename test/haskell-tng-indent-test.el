;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'ert-x)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

(ert-deftest haskell-tng-indent-file-tests ()
  ;; Three indentation regression tests are possible:
  ;;
  ;;   1. newline-and-indent with the rest of the file deleted (append)
  ;;   2. newline-and-indent with the rest of the file intact (insert)
  ;;   3. indent-line-function at the beginning of each line (re-indent)
  ;;
  ;; each with alternative indentation suggestions.
  ;;
  ;; Expectations could use lines of symbols such as | and . or digits to
  ;; indicate where the indentation(s) go.
  ;;
  ;; Test 1 involves a lot of buffer refreshing and will be very slow.

  (should (have-expected-newline-indent-insert (testdata "src/layout.hs")))
  (should (have-expected-newline-indent-insert (testdata "src/medley.hs")))
  ;; TODO more tests
  )

;; TODO enable this test and get it passing, which requires a TAB command that
;; will insert whitespace and move point to end. Workaround is to use abbrevs or
;; yasnippets for things like "import" that have fixed indentations.
;;
;; (ert-deftest haskell-tng-indent-custom-tests ()
;;   (with-temp-buffer
;;     (insert-file-contents (testdata "src/medley.hs"))
;;     (haskell-tng-mode)
;;     ;; import TAB should jump to column 17
;;     (goto-char 511)
;;     (ert-simulate-command '(forward-word))
;;     (ert-simulate-command '(indent-for-tab-command))
;;     (ert-simulate-command '(indent-for-tab-command))
;;     (should (equal (point) 528))
;;     ))

(defun current-line-string ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (- (line-beginning-position 2) 1)))

(defun haskell-tng-indent-test:newline-indent-insert ()
  (let (indents)
    (while (not (eobp))
      (end-of-line)
      ;; the command loop is necessary for this/last-command
      (cl-flet ((RET ()
                     (ert-simulate-command '(newline-and-indent))
                     (current-column))
                (TAB ()
                     (ert-simulate-command '(indent-for-tab-command))
                     (current-column)))

        (let ((line (current-line-string))
              (prime (RET))
              alts)
          (while (and (TAB)
                      (not (eq (current-column) prime))
                      (not (member (current-column) alts)))
            (push (current-column) alts))
          (push `(,line . (,prime . ,(reverse alts))) indents)
          ;; unfortunately killing resets this-command so we don't test double
          ;; newline insertions, which could accidentally trigger alts only.
          (kill-whole-line))))
    (reverse indents)))

(defun haskell-tng-indent-test:indents-to-string (indents)
  "INDENTS is a list of INDENT.

INDENT is a non-empty list of (LINE . (INDENT . ALTS)) where LINE
is the string line of code before the indentation, INDENT is the
integer suggested next line indentation column and ALTS is a list
of integer alternative indentations."
  (s-join "\n" (-flatten
                (-map #'haskell-tng-indent-test:indent-to-string indents))))

(defun haskell-tng-indent-test:indent-to-string (indent)
  (let* ((line (car indent))
         (prime (cadr indent))
         (alts (cddr indent))
         (widest (-max (cdr indent)))
         repr)
    (--dotimes (+ 1 widest)
      (push
       (cond
        ((eq it prime) "v")
        ((member it alts) ".")
        (t " "))
       repr))
    (list line (s-join "" (reverse repr)))))

(defun have-expected-newline-indent-insert (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:newline-indent-insert)))
   "insert.indent"))

;;; haskell-tng-indent-test.el ends here
