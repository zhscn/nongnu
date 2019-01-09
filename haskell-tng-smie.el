;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
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

;; Function to scan forward for the next token.
;; - Called with no argument should return a token and move to its end.
;; - If no token is found, return nil or the empty string.
;; - It can return nil when bumping into a parenthesis, which lets SMIE
;;   use syntax-tables to handle them in efficient C code.
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Lexer
(defun haskell-tng-smie:forward-token ()
  (interactive) ;; for testing
  (forward-comment (point-max))
  (unless (eobp)
    (let ((case-fold-search nil)
          (syntax (char-syntax (char-after))))
      (cond
       ;; TODO detect newlines with significant whitespace

       ;; parens
       ((or (= syntax ?\() (= syntax ?\))) nil)

       ;; TODO match paired delimiters

       ;; regexps
       ((or
         ;; known identifiers
         (looking-at haskell-tng:regexp:reserved)
         ;; symbols
         (looking-at (rx (+ (| (syntax word) (syntax symbol)))))
         ;; whatever the current syntax class is
         (looking-at (rx-to-string `(+ (syntax ,syntax)))))
        (goto-char (match-end 0))
        (match-string-no-properties 0))))))

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
