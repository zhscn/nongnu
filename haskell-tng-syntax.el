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
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Syntax-Table-Internals
;;
;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst haskell-tng--syntax-table
  (let ((table (make-syntax-table)))
    (map-char-table
     (lambda (k v)
       ;; reset the (surprisingly numerous) defaults
       (let ((class (syntax-class v)))
         (when (seq-contains-p '(1 4 5 6 9) class)
           (modify-syntax-entry k "_" table))))
     (char-table-parent table))

    ;; many of the following chars are already defined by the defaults, but we
    ;; repeat them (at least ASCII) to explicitly match Haskell2010 section 2.2.
    ;; Chars that appear in multiple classes are only assigned to the final
    ;; class they are given.

    ;; whitechar
    (seq-do
     (lambda (it) (modify-syntax-entry it " " table))
     (string-to-list "\r\n\f\v \t"))

    ;; ascSymbol
    (seq-do
     (lambda (it) (modify-syntax-entry it "_" table))
     (string-to-list "!#$%&*+./<=>?\\^|-~:"))
    ;; TODO ! is a keyword when using LANG Strict*
    ;; TODO # can be used for primitives

    ;; TODO: debatable. User nav vs fonts and lexing. getting "word boundaries"
    ;;       is important, same for apostrophe. small (underscore is a lowercase
    ;;       letter)
    (modify-syntax-entry ?_ "w" table)

    ;; FIXME use categories more heavily, which would involve doing lexing here.
    ;; It would make the fontification regexps simpler and faster, and would be
    ;; reused by the SMIE lexer (fixing many problems with the backwards lexer).
    ;; We might not need zero-length matchers because greedy matching will be
    ;; much simpler.

    ;; some special (treated like punctuation)
    (seq-do
     (lambda (it) (modify-syntax-entry it "." table))
     (string-to-list ",;@"))

    ;; apostrophe as a word, not delimiter
    (modify-syntax-entry ?\' "w" table)

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
    (modify-syntax-entry ?-  "_ 123" table) ;; TODO --> is not a comment
    (seq-do
     (lambda (it) (modify-syntax-entry it ">" table))
     (string-to-list "\r\n\f\v"))

    table)
  "Haskell syntax table.")

(defun haskell-tng--syntax-propertize (start end)
  "For some context-sensitive syntax entries."
  (let (case-fold-search)
    (haskell-tng--syntax-char-delims start end)
    (haskell-tng--syntax-fqn-punct start end)
    (haskell-tng--syntax-string-escapes start end)
    (haskell-tng--syntax-escapes start end)))

(defun haskell-tng--syntax-char-delims (start end)
  "Matching apostrophes are string delimiters (literal chars)."
  (goto-char start)
  ;;  should handle: foo' 'a' 2 because ' is a word here
  (while (re-search-forward (rx word-start "'" (? "\\") not-newline "'") end t)
    (let ((open (match-beginning 0))
          (close (- (point) 1)))
      (put-text-property open (1+ open) 'syntax-table '(7 . ?\'))
      (put-text-property close (1+ close) 'syntax-table '(7 . ?\')))))

(defun haskell-tng--syntax-fqn-punct (start end)
  "dot/period is typically a symbol, unless it is used in a
module or qualifier, then it is punctuation."
  (goto-char start)
  (while (re-search-forward (rx word-end ".") end t)
    (let ((dot (match-beginning 0)))
      (put-text-property dot (1+ dot) 'syntax-table '(1)))))

(defun haskell-tng--syntax-string-escapes (start end)
  "Backslash before quotes is a string escape.

This needs to run before `haskell-tng--syntax-escapes' or string
detection will not work correctly.

There is an expected false positive: an operator has an odd
number of \ as its final characters and is called with a literal
string or char as the 2nd parameter and no whitespace."
  (goto-char start)
  (while (re-search-forward
          (rx "\\" (syntax string-quote))
          end t)
    (let* ((escape (match-beginning 0))
           (before (haskell-tng--syntax-count-escapes escape t)))
      (when (cl-evenp before) ;; makes sure it's a real escape
        (put-text-property escape (1+ escape) 'syntax-table '(9))))))

(defun haskell-tng--syntax-count-escapes (pos &optional skip-whitespace)
  "Count the number of escapes before POS.
Even means the next char is not escaped.

SKIP-WHITESPACE can be used to ignore whitespace gaps."
  (if (= (point-min) pos) 0
    (let ((c (char-before pos)))
      (if (and skip-whitespace (or (= ?\n c) (= 32 (char-syntax c))))
          (haskell-tng--syntax-count-escapes (1- pos) t)
        (if (= c ?\\)
            (1+ (haskell-tng--syntax-count-escapes (1- pos)))
          0)))))

(defun haskell-tng--syntax-escapes (start end)
  "Backslash inside String is an escape character \n."
  ;; TODO does this pull its weight? (slow, requires a ppss)
  (goto-char start)
  (while (re-search-forward (rx "\\") end t)
    (let ((escape (match-beginning 0)))
      (if (= 9 (car (syntax-after escape))) ;; already calculated
          (forward-char 1)
        (unless (looking-at (rx (+ (| space ?\n)) "\\"))
          (let ((before (haskell-tng--syntax-count-escapes escape t)))
            (unless (and (cl-oddp before)
                         (looking-back (rx (| space ?\n)) (1- escape)))
              (when (and (cl-evenp before)
                         (nth 3 (syntax-ppss)))
                (put-text-property escape (1+ escape) 'syntax-table '(9)))
              (when (= 9 (car (syntax-after escape)))
                ;; next char is escaped, so no need to check it
                (forward-char 1)))))))))

(provide 'haskell-tng-syntax)
;;; haskell-tng-syntax.el ends here
