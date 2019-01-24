;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE lexer, precedence table (providing s-expression navigation), and
;;  indentation rules.
;;
;;  Note that we don't need to support every aspect of the Haskell language in
;;  these grammar rules: only the parts that are relevant for the features that
;;  are provided.
;;
;;  If we had access to all the operators in scope, and their fixity, we could
;;  create file-specific precendences. However, the complexity-to-benefit payoff
;;  is minimal.
;;
;;  Users may consult the SMIE manual to customise their indentation rules:
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE
;;
;;  The Haskell2010 report's sections 2.7 and 10.3 are particularly pertinent:
;;
;;  https://www.haskell.org/onlinereport/haskell2010/haskellch2.html
;;  https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
;;
;;; Code:

(require 'smie)
(require 'haskell-tng-font-lock)

;; FIXME: massive hack. Holds an ordered list of (position . level) that close
;; an inferred layout block. Convert into a (cached) function call to calculate
;; the relevant WLDOs for a given point.
(defvar-local haskell-tng-smie:wldos nil)

;; State: a list of tokens to return at the current point ending with `t' as an
;; indicator that all virtual tokens have been processed. `nil' means to proceed
;; as normal.
;;
;; FIXME cache invalidation
(defvar-local haskell-tng-smie:multi nil)

;; Function to scan forward for the next token.
;;
;; - Called with no argument should return a token and move to its end.
;; - If no token is found, return nil or the empty string.
;; - It can return nil when bumping into a parenthesis, which lets SMIE
;;   use syntax-tables to handle them in efficient C code.
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Lexer
(defun haskell-tng-smie:forward-token ()
  (interactive) ;; for testing
  (forward-comment (point-max)) ;; TODO: move to after virtual token generation
  (cond
   ;; TODO: remove this hack
   ((eobp)
    "}")

   ;; reading from state
   ((stringp (car haskell-tng-smie:multi))
    (pop haskell-tng-smie:multi))

   (t
    (let ((done-multi (pop haskell-tng-smie:multi))
          (case-fold-search nil)
          (offside (car haskell-tng-smie:wldos)))
      (cl-flet ((virtual-end () (= (point) (car offside)))
                (virtual-semicolon () (= (current-column) (cdr offside))))
        (cond
         ;; layout
         ((and offside
               (not done-multi)
               (or (virtual-end) (virtual-semicolon)))
          (setq haskell-tng-smie:multi '(t))
          (while (and offside (virtual-end))
            (push "}" haskell-tng-smie:multi)
            (pop haskell-tng-smie:wldos)
            (setq offside (car haskell-tng-smie:wldos)))
          (when (and offside (virtual-semicolon))
            (setq haskell-tng-smie:multi
                  (-insert-at (- (length haskell-tng-smie:multi) 1)
                              ";" haskell-tng-smie:multi)))
          (pop haskell-tng-smie:multi))

         ;; syntax tables (supported by `smie-indent-forward-token')
         ((looking-at (rx (| (syntax open-parenthesis)
                             (syntax close-parenthesis)
                             (syntax string-quote)
                             (syntax string-delimiter))))
          nil)

         ;; layout detection
         ((looking-at (rx word-start (| "where" "let" "do" "of") word-end))
          (save-match-data
            (forward-word)
            (forward-comment (point-max))
            (when (not (looking-at "{"))
              (push (haskell-tng:layout-close-and-level) haskell-tng-smie:wldos)
              (setq haskell-tng-smie:multi '("{" t))))
          (haskell-tng-smie:last-match))

         ;; regexps
         ((or
           ;; known identifiers
           (looking-at haskell-tng:regexp:reserved)
           ;; symbols
           (looking-at (rx (+ (| (syntax word) (syntax symbol))))))
          (haskell-tng-smie:last-match))

         ;; single char
         (t
          (forward-char)
          (string (char-before)))))))))

(defun haskell-tng:layout-of-next-token ()
  (save-excursion
    (forward-comment (point-max))
    (current-column)))

(defun haskell-tng:layout-close-and-level (&optional pos)
  "A cons cell of the closing point for the layout beginning at POS, and level."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((level (current-column))
          (close (or (haskell-tng:paren-close) (point-max))))
      (catch 'closed
        (while (not (eobp))
          (forward-line)
          (forward-comment (point-max))
          (when (< close (point))
            (throw 'closed (cons close level)))
          (when (< (current-column) level)
            (throw 'closed (cons (point) level))))
        (cons (point-max) level)))))

(defun haskell-tng-smie:last-match ()
  (goto-char (match-end 0))
  (match-string-no-properties 0))

;; TODO a haskell grammar
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Grammar
(defvar haskell-tng-smie:grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("if" exp "then" inst "else" inst)
            (id "<-" exp)
            (id "=" exp)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("(" exps ")")
           ("{" exps "}"))
      (exps (exps "," exps) (exp)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "+") (assoc "*")))))

;; TODO indentation rules
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Indentation
(defvar haskell-tng-smie:rules nil)

(defun haskell-tng-smie:setup ()
  (smie-setup
   haskell-tng-smie:grammar
   haskell-tng-smie:rules
   :forward-token #'haskell-tng-smie:forward-token
   ;; TODO :backward-token #'haskell-tng-smie:backward-token
   ))

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
