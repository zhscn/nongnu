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

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Grammar
;; https://www.haskell.org/onlinereport/haskell2010/haskellch3.html
;;
;; Many of these grammar rules cannot be expressed in SMIE because Haskell uses
;; whitespace separators a lot, whereas the BNF must use non-terminals.
;;
;; Rules that are unbounded on the right tend to misbehave quite badly, since
;; there is no way to know how to end the s-expression. For example, the BNF
;; definition of a TYPE will typically extend to an arbitrary point later in the
;; file. We cannot use `;' to end the TYPE definition, because symbols cannot be
;; closers and "neither" (i.e. infix) at the same time. An option is always to
;; push contextual functionality into the lexer, but one must draw a line
;; somewhere.
;;
;; We do not include rules unless they have an impact on indentation. Navigation
;; with a more complete grammar has been shown to be less than satisfactory,
;; therefore there is no reason to do more than is needed.
(defvar haskell-tng-smie:grammar
  ;; see docs for `smie-prec2->grammar'
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)

      ;; commas only allowed in brackets
      (context
       ("(" context ")")
       (context "," context))

      ;; operators
      ;; TODO lexer should identify / normalise operators.
      (infixexp
       (id "$" infixexp)
       (id "*" infixexp)
       (id "+" infixexp)
       (id))

      ;; TODO lexer should identify / normalise ids, consid, etc.

      ;; WLDOs
      (wldo
       ("where" decls)
       ("let" decls)
       ("do" stmts)
       ("of" alts))
      (decls
       ("{" decls "}")
       (decls ";" decls)
       (id "=" id))
      (stmts
       ("{" stmts "}")
       (stmts ";" stmts)
       (id "<-" id))
      (alts
       ("{" alts "}")
       (alts ";" alts)
       (id "->" id))

      )

    ;; operator precedences
    '((assoc ";")
      (assoc ",")
      )

    )))

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Indentation
;;
;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63
(defun haskell-tng-smie:rules (method arg)
  ;; see docs for `smie-rules-function'
  ;; FIXME implement and test indentation
  (pcase (cons method arg)
    (`(:elem . basic) smie-indent-basic)
    (`(,_ . ",") (smie-rule-separator method))
    (`(:after . "=") smie-indent-basic)
    (`(:before . ,(or `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent)))))

(defun haskell-tng-smie:setup ()
  (setq-local smie-indent-basic 2)

  (add-to-list
   'after-change-functions
   #'haskell-tng-layout:cache-invalidation)

  (add-to-list
   'after-change-functions
   #'haskell-tng-lexer:state-invalidation)

  (smie-setup
   haskell-tng-smie:grammar
   #'haskell-tng-smie:rules
   :forward-token #'haskell-tng-lexer:forward-token
   :backward-token #'haskell-tng-lexer:backward-token))

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
