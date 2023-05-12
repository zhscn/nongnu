;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-testutils)

(require 'haskell-tng-mode)

;; TODO line after `instance ... where' has too much indent
;; TODO records assigning / copy by label
;; TODO records of functions
;; TODO ImplicitParams in type signatures (without parens)

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



;; ;; this test is slow and is a useful benchmark
;; (ert-deftest haskell-tng-append-indent-file-tests ()
;;   (require 'profiler)
;;   (profiler-start 'cpu)
;;   (should (have-expected-append-indent (testdata "src/medley.hs")))
;;   (profiler-report)
;;   (profiler-report-write-profile "indentation.profile")
;;   (profiler-stop)
;;   (profiler-find-profile "../indentation.profile"))

(ert-deftest haskell-tng-append-indent-file-tests-indentation ()
  (should (have-expected-append-indent (testdata "src/indentation.hs"))))
(ert-deftest haskell-tng-append-indent-file-tests-options1 ()
  (let ((haskell-tng-typelead 1))
    (should (have-expected-append-indent (testdata "src/indentation-options1.hs")))))
(ert-deftest haskell-tng-append-indent-file-tests-options2 ()
  (let ((haskell-tng-typelead 1)
        (haskell-tng-aligntypes t))
    (should (have-expected-append-indent (testdata "src/indentation-options2.hs")))))

(ert-deftest haskell-tng-insert-indent-file-tests-indentation ()
  (should (have-expected-insert-indent (testdata "src/indentation.hs"))))
(ert-deftest haskell-tng-insert-indent-file-tests-options1 ()
  (let ((haskell-tng-typelead 1))
    (should (have-expected-insert-indent (testdata "src/indentation-options1.hs")))))
(ert-deftest haskell-tng-insert-indent-file-tests-options2 ()
  (let ((haskell-tng-typelead 1)
        (haskell-tng-aligntypes t))
    (should (have-expected-insert-indent (testdata "src/indentation-options2.hs")))))

;; TODO reindenting needs attention, it's all over the radar
;; (ert-deftest haskell-tng-reindent-file-tests ()
;;   (should (have-expected-reindent (testdata "src/indentation.hs"))))

(defun haskell-tng--indent-test-work (mode)
  "MODE can be 'insert, 'reindent, or 'append."
  (let (indents lines)
    (pcase mode
      ('append
       (setq lines (split-string (buffer-string) (rx ?\n)))
       (delete-region (point-min) (point-max))

       ;; WORKAROUND https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36432
       ;;
       ;; SMIE doesn't request forward tokens from the lexer when the point is
       ;; at point-max, so add some whitespace at the end.
       ;;
       ;; TODO fix the bug properly, in SMIE.
       (save-excursion
         (insert "\n\n"))))
    (while (pcase mode
             ('append lines)
             (_ (not (eobp))))
      ;; the command loop is necessary for this/last-command
      (when lines
        (insert (pop lines)))
      (cl-flet ((RET ()
                     (end-of-line)
                     (ert-simulate-command '(newline-and-indent))
                     (current-column))
                (TAB ()
                     (ert-simulate-command '(indent-for-tab-command))
                     (current-column)))

        (let ((orig (current-indentation))
              (line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
              (prime (pcase mode
                       ((or 'insert 'append) (RET))
                       ('reindent (TAB))))
              alts)
          (while (and (TAB)
                      (not (eq (current-column) prime))
                      (not (member (current-column) alts)))
            (push (current-column) alts))
          (push `(,(pcase mode ((or 'insert 'append) t))
                  ,line . (,prime . ,(reverse alts)))
                indents)
          ;; unfortunately killing resets this-command so we can't test double
          ;; newline insertions, which could accidentally trigger alts only.
          (pcase mode
            ('insert
             (beginning-of-line)
             (unless (eobp)
               (kill-whole-line)))
            ('append
             (beginning-of-line)
             (unless (eobp)
               (delete-region (point) (line-end-position))))
            ('reindent
             (indent-line-to orig)
             (ert-simulate-command '(forward-line)))))))
    (reverse indents)))

(defun haskell-tng--indent-test-indents-to-string (indents)
  "INDENTS is a list of INDENT.

INDENT is a non-empty list of (RET . (LINE . (INDENT . ALTS)))
where RET is t for newline insertions (i.e. LINE is a string of
the previous line) and nil for reindent (i.e. LINE is a string of
the current line).

INDENT is the integer suggested next line indentation column and
ALTS is a list of integer alternative indentations."
  (s-join "\n" (seq-mapcat #'haskell-tng--indent-test-indent-to-string indents)))

(defun haskell-tng--indent-test-indent-to-string (indent)
  (let* ((ret (car indent))
         (line (cadr indent))
         (prime (caddr indent))
         (alts (cdddr indent))
         (widest (seq-max (cddr indent)))
         repr)
    (dotimes (it (+ 1 widest))
      (push
       (cond
        ((eq it prime) "v")
        ((member it alts)
         (let ((i (seq-position alts it)))
           (if (< i 9)
               (number-to-string (+ 1 i))
             ".")))
        (t " "))
       repr))
    (let ((indents (s-join "" (reverse repr))))
      (if ret
          (list line indents)
        (list indents line)))))

(defun have-expected-insert-indent (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng--indent-test-indents-to-string
      (haskell-tng--indent-test-work 'insert)))
   "insert.indent"))

(defun have-expected-reindent (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng--indent-test-indents-to-string
      (haskell-tng--indent-test-work 'reindent)))
   "reindent"))

(defun have-expected-append-indent (file)
  (haskell-tng--testutils-assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng--indent-test-indents-to-string
      (haskell-tng--indent-test-work 'append)))
   "append.indent"))

;;; haskell-tng-indent-test.el ends here
