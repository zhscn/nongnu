;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE precedence table, providing s-expression navigation, and indentation
;;  rules.
;;
;;  Although SMIE provides the primary indentation suggest (when the user types
;;  RETURN), we cycle through alternative candidates on TAB. The philosophy is
;;  not to try and get indentation right 100% of the time, but to get it right
;;  90% of the time and make it so easy to fix it that it doesn't get in the
;;  way.
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
;; https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/parser/Parser.y
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
      (list
       ("(" list ")")
       ("[" list "]") ;; includes DataKinds
       (list "," list))

      ;; operators all have the same precedence
      (infixexp
       (id ":" infixexp) ;; keyword infix
       (id "':" infixexp) ;; DataKinds
       (id "SYMID" infixexp))

      ;; WLDOs
      (wldo
       (block "where" block)
       ("let" block "in")
       ("do" block)
       ("case" id "of" block))
      (block
       ("{" block "}")
       (block ";" block)
       (id "=" id)
       (id "<-" id)
       (id "->" id)
       )

      (logic
       ("if" id "then" id "else" id))
      )

    ;; operator precedences
    '((assoc ";" ",")
      )

    )))

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Indentation
;;
;; ideas for an indentation tester
;; https://github.com/elixir-editors/emacs-elixir/blob/master/test/test-helper.el#L52-L63
(defun haskell-tng-smie:rules (method arg)
  ;; see docs for `smie-rules-function'
  ;; TODO implement indentation
  (pcase (cons method arg)
    (`(:elem . basic) smie-indent-basic)
    (`(,_ . ",") (smie-rule-separator method))
    (`(:after . "=") smie-indent-basic)
    (`(:before . ,(or `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent)))))

;; FIXME tests for indentation, including the cycled choices

;; TODO decide either to set indent-line-function or to wrap around
;; smie-indent-calculate with

;; (add-hook 'smie-indent-functions
;;           #'haskell-tng:smie-indent nil 'local)

(defvar haskell-tng:smie-indent-nested-call nil)

(defun haskell-tng:smie-indent ()
  (cond
   ;; When we're not in the top-level call to smie-indent-calculate, so just do
   ;; nothing and let the other rules do their job.
   (haskell-tng:smie-indent-nested-call nil)
   ;; When cycling, return the next indentation.
   ((eq this-command last-command)
    (haskell-tng:return-next-stashed-indentation-column))
   ;; When we're in the top-level call to smie-indent-calculate, take control
   ;; and return a non-nil value to prevent the other rules from being used.
   (t
    (let ((haskell-tng:smie-indent-nested-call t)
          (n (haskell-tng:get-number-of-closing-braces-at-bol))
          (indentations ()))
      (dotimes (i n)
        (haskell-tng:tell-lexer-there-are-N-closing-braces-at-bol i)
        (push (smie-indent-calculate) indentations))
      (haskell-tng:stash-indentation-columns indentations)
      (haskell-tng:return-next-stashed-indentation-column)))))

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
