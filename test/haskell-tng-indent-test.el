;;; haskell-tng-indent-test.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

(require 'ert)
(require 'ert-x)
(require 's)

(require 'haskell-tng-mode)

(require 'haskell-tng-testutils
         "test/haskell-tng-testutils.el")

;; FIXME implement more indentation rules
;;
;; TODO multiline type signatures
;; TODO user config for typesig preference (incompatible versions)
;; TODO ImplicitParams in type signatures (without parens)
;; TODO if/then/else

;; TODO reindenting needs attention, it's all over the radar

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

(ert-deftest haskell-tng-append-indent-file-tests ()
  (should (have-expected-append-indent (testdata "src/indentation.hs")))

  ;;(should (have-expected-append-indent (testdata "src/layout.hs")))

  ;; this test is slow
  ;; (require 'profiler)
  ;; (profiler-start 'cpu)
  ;; (should (have-expected-append-indent (testdata "src/medley.hs")))
  ;; (profiler-report)
  ;; (profiler-report-write-profile "indentation.profile")
  ;; (profiler-stop)
  ;; (profiler-find-profile "../indentation.profile")
  )


(ert-deftest haskell-tng-indent-file-tests ()
  (should (have-expected-insert-indent (testdata "src/indentation.hs")))

  ;; (should (have-expected-insert-indent (testdata "src/layout.hs")))
  ;; (should (have-expected-insert-indent (testdata "src/medley.hs")))
  )

(ert-deftest haskell-tng-reindent-file-tests ()
  (should (have-expected-reindent (testdata "src/indentation.hs")))

  ;; (should (have-expected-reindent (testdata "src/layout.hs")))
  ;; (should (have-expected-reindent (testdata "src/medley.hs")))
  )


(defun haskell-tng-indent-test:work (mode)
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
       ;; TODO fix the bug properly, in SMIE
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
             (when (not (eobp))
               (kill-whole-line)))
            ('append
             (beginning-of-line)
             (when (not (eobp))
               (delete-region (point) (line-end-position))))
            ('reindent
             (indent-line-to orig)
             (ert-simulate-command '(forward-line)))))))
    (reverse indents)))

(defun haskell-tng-indent-test:indents-to-string (indents)
  "INDENTS is a list of INDENT.

INDENT is a non-empty list of (RET . (LINE . (INDENT . ALTS)))
where RET is t for newline insertions (i.e. LINE is a string of
the previous line) and nil for reindent (i.e. LINE is a string of
the current line).

INDENT is the integer suggested next line indentation column and
ALTS is a list of integer alternative indentations."
  (s-join "\n" (-flatten
                (-map #'haskell-tng-indent-test:indent-to-string indents))))

(defun haskell-tng-indent-test:indent-to-string (indent)
  (let* ((ret (car indent))
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
      (if ret
          (list line indents)
        (list indents line)))))

(defun have-expected-insert-indent (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:work 'insert)))
   "insert.indent"))

(defun have-expected-reindent (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:work 'reindent)))
   "reindent"))

(defun have-expected-append-indent (file)
  (haskell-tng-testutils:assert-file-contents
   file
   #'haskell-tng-mode
   (lambda ()
     (haskell-tng-indent-test:indents-to-string
      (haskell-tng-indent-test:work 'append)))
   "append.indent"))

;;; haskell-tng-indent-test.el ends here
