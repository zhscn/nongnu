;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE precedence table, providing s-expression navigation, and indentation
;;  rules.
;;
;;  Note that we don't support every aspect of the Haskell language. e.g. if we
;;  had access to the fixity of operators in scope we could create file-specific
;;  rules.
;;
;;  Users may consult the SMIE manual to customise their indentation rules:
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE
;;
;;; Code:

(require 'smie)

(require 'haskell-tng-font-lock)
(require 'haskell-tng-lexer)

;; FIXME a haskell grammar that doesn't have warnings during the tests

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Grammar
;; https://www.haskell.org/onlinereport/haskell2010/haskellch3.html
;;
;; exp       infixexp :: [context =>] type         (expression type signature)
;;     |     infixexp
;;
;; infixexp  lexp qop infixexp                     (infix operator application)
;;     |     - infixexp                            (prefix negation)
;;     |     lexp
;;
;; lexp      \ apat1 … apatn -> exp                (lambda abstraction, n ≥ 1)
;;     |     let decls in exp                      (let expression)
;;     |     if exp [;] then exp [;] else exp      (conditional)
;;     |     case exp of { alts }                  (case expression)
;;     |     do { stmts }                          (do expression)
;;     |     fexp
;;
;; fexp      [fexp] aexp                           (function application)
;;
;; aexp      qvar                                  (variable)
;;     |     gcon                                  (general constructor)
;;     |     literal
;;     |     ( exp )                               (parenthesized expression)
;;     |     ( exp1 , … , expk )                   (tuple, k ≥ 2)
;;     |     [ exp1 , … , expk ]                   (list, k ≥ 1)
;;     |     [ exp1 [, exp2] .. [exp3] ]           (arithmetic sequence)
;;     |     [ exp | qual1 , … , qualn ]           (list comprehension, n ≥ 1)
;;     |     ( infixexp qop )                      (left section)
;;     |     ( qop⟨-⟩ infixexp )                   (right section)
;;     |     qcon { fbind1 , … , fbindn }          (labeled construction, n ≥ 0)
;;     |     aexp⟨qcon⟩ { fbind1 , … , fbindn }    (labeled update, n  ≥  1)
(defvar haskell-tng-smie:grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (exp
       ;; TODO context
       ;;(infixexp "::" context "=>" type)
       (infixexp "::" type)
       (infixexp)
       )

      ;; TODO update the lexer to provide a virtual token for infix but keep
      ;; popular operators with important fixity.
      (infixexp
       (lexp "$" infixexp)
       (lexp "+" infixexp)
       (lexp "-" infixexp)
       ;; (lexp "*" infixexp)
       ;; (lexp "/" infixexp)
       ;; (lexp "<$>" infixexp)
       ;; (lexp "<*>" infixexp)
       ;; (lexp ">>=" infixexp)
       ;; (lexp "`should`" infixexp)
       ;; (lexp "&" infixexp)

       ;;("-" infixexp) ;; can't be opener and neither
       (lexp)
       )

      ;; TODO to support terminators as separators
      ;;(insts (insts ";" insts) (inst))

      ;; ;; FIXME these seem to break everything
      (lexp
       ("if" exp "then" exp "else" exp)
       ;; TODO apats
       ;;("let" decls "in" exp)
       ;;("case" exp "of" alts)
       ;;("do" stmts)
       ;; TODO where?
       ;; TODO fexp
       )

      ;; (decls
      ;;  ;;("{" decls "}")
      ;;  (decls ";" decls)
      ;;  (decl))
      ;; (decl
      ;;  (id "=" exp))
      ;; (alts
      ;;  ;;("{" alts "}")
      ;;  (alts ";" alts)
      ;;  (alt))
      ;; (alt
      ;;  (id "->" exp))
      ;; (stmts
      ;;  ;;("{" stmts "}")
      ;;  (stmts ";" stmts)
      ;;  (stmt))
      ;; (stmt
      ;;  (id "<-" exp))

      )

    ;; operator precedences
    ;;'((assoc ";"))
    ;;'((assoc ","))
    ;; TODO arrange by fixity
    '((assoc "$"))
    '((assoc "+") (assoc "-"))
    ;; '((assoc "*"))
    ;; '((assoc "/"))
    ;; '((assoc "<$>"))
    ;; '((assoc "<*>"))
    ;; '((assoc ">>="))
    ;; '((assoc "&"))

    )))

;; TODO indentation rules
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Indentation
;;
;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63
(defvar haskell-tng-smie:rules nil)

(defun haskell-tng-smie:setup ()
  (add-to-list
   'after-change-functions
   #'haskell-tng-layout:cache-invalidation)

  (add-to-list
   'after-change-functions
   #'haskell-tng-lexer:state-invalidation)

  (smie-setup
   haskell-tng-smie:grammar
   haskell-tng-smie:rules
   :forward-token #'haskell-tng-lexer:forward-token
   :backward-token #'haskell-tng-lexer:backward-token))

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
