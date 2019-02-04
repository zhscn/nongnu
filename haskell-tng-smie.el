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
(defvar haskell-tng-smie:grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (exp
       ;; TODO context
       ;;(infixexp "::" context "=>" type)
       (infixexp "::" type)
       (infixexp))

      ;; TODO monkey patch `smie-indent-forward' to support regexps in `assoc'
      (infixexp
       (lexp "$" infixexp) ;; TODO arrange by fixity
       (lexp "+" infixexp)
       (lexp "-" infixexp)
       (lexp "*" infixexp)
       (lexp "/" infixexp)
       (lexp "<$>" infixexp)
       (lexp "<*>" infixexp)
       (lexp ">>=" infixexp)
       (lexp "`should`" infixexp)
       (lexp "&" infixexp)
       ("-" infixexp)
       (lexp))

      (lexp
       ;; TODO apats, it is hard to formalise...
       ;;("\\" apats "->" exp)
       ("let" "{" decls "}" "in" exp)
       ("if" exp "then" exp "else" exp)
       ("case" exp "of" "{" alts "}")
       ("do" "{" stmts "}")
       ;; TODO fexp
       )

      ;; TODO formal decls definition
      (decls
       (decls ";" decls)
       (decl))
      (decl
       (id "=" exp))
      ;; TODO formal alts definition
      (alts
       (alts ";" alts)
       (alt))
      (alt
       (id "->" exp))
      ;; TODO formal stmts definition
      (stmts
       (stmts ";" stmts)
       (stmt))
      (stmt
       (id "<-" exp))

      ))))

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
