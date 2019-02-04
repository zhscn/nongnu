;;; haskell-tng-syntax.el --- Syntax Table for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  A grossly simplified description of the Haskell language providing fast
;;  editor commands and visual feedback. Defer advanced analysis to later stages
;;  such as font-lock and SMIE lexing / parsing, where it is needed.
;;
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Syntax-Tables
;;
;;; Code:

(require 'dash)

(defconst haskell-tng:syntax-table
  (let ((table (make-syntax-table)))
    (map-char-table
     #'(lambda (k v)
         ;; reset the (surprisingly numerous) defaults
         (let ((class (syntax-class v)))
           (when (-contains? '(1 4 5 6 9) class)
             (modify-syntax-entry k "_" table))))
     (char-table-parent table))

    ;; many of the following chars are already defined by the defaults, but we
    ;; repeat them (at least ASCII) to explicitly match Haskell2010 section 2.2.
    ;; Chars that appear in multiple classes are only assigned to the final
    ;; class they are given.

    ;; whitechar
    (--each (string-to-list "\r\n\f\v \t")
      (modify-syntax-entry it " " table))

    ;; ascSymbol
    (--each (string-to-list "!#$%&*+./<=>?@\\^|-~:")
      (modify-syntax-entry it "_" table))

    ;; TODO: should be iff _ is alone or first char
    ;; small (underscore is a lowercase letter)
    (modify-syntax-entry ?_ "w" table)

    ;; some special (treated like punctuation)
    (--each (string-to-list ",;")
      (modify-syntax-entry it "." table))

    ;; apostrophe as a symbol, not delimiter
    (modify-syntax-entry ?\' "_" table)

    ;; string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; parens and pairs (infix functions)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\` "$`" table)

    ;; comments (subsuming pragmas)
    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  "_ 123" table)
    (--each (string-to-list "\r\n\f\v")
      (modify-syntax-entry it ">" table))

    table)
  "Haskell syntax table.")

(defun haskell-tng:syntax-propertize (start end)
  "For some context-sensitive syntax entries."
  (haskell-tng:syntax:char-delims start end)
  (haskell-tng:syntax:escapes start end))

;; TODO doesn't handle the following correctly
;;
;;   foo' 'a' 2
(defun haskell-tng:syntax:char-delims (start end)
  "Matching apostrophes are string delimiters (literal chars)."
  (goto-char start)
  (while (re-search-forward "'\\\\?.'" end t)
    (let ((open (match-beginning 0))
          (close (- (point) 1)))
      (put-text-property open (1+ open) 'syntax-table '(7 . ?\'))
      (put-text-property close (1+ close) 'syntax-table '(7 . ?\')))))

;; TODO or look for non-string backslashes. While we're at it, we could mark
;; everything up to the -> with an apat property / category. Alternatively this
;; would need to be in the lexer (and fontification would miss out).
(defun haskell-tng:syntax:escapes (start end)
  "Backslash inside String is an escape character."
  (goto-char start)
  (while (re-search-forward "\\\\" end t)
    (when (nth 3 (syntax-ppss))
      (put-text-property (- (point) 1) (point)
                         'syntax-table '(9 . ?\\)))))

;; EXT:ExplicitForAll should turn dots into punctuation

(provide 'haskell-tng-syntax)
;;; haskell-tng-syntax.el ends here
