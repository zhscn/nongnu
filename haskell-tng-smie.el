;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE precedence table and indentation rules.
;;
;;  Although SMIE provides the primary indentation suggestion, we cycle through
;;  heuristic alternative candidates on subsequent presses of TAB.
;;
;;  The philosophy is not to get indentation right 100% of the time, but to get
;;  it right 90% of the time and make it so easy to fix it that it doesn't get
;;  in the way.
;;
;;  Interactive indentation is ambiguous in a whitespace sensitive language
;;  because it is not known if the user wishes to continue the previous line,
;;  create a new line at the same level, or close off the block. We try to err
;;  on the side of "staying at the same level" (not escaping or closing a
;;  previous line) when we can.
;;
;;  All the good ideas are from Stefan Monnier, all the bad ones are mine.
;;
;;; Code:

(require 'smie)

(require 'haskell-tng-font-lock)
(require 'haskell-tng-lexer)

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Grammar
;; https://www.haskell.org/onlinereport/haskell2010/haskellch3.html
;; https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/parser/Parser.y
;;
;; See also the documentation for `smie-bnf->prec2' which tends to be more up to
;; date.
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
       ("{" list "}")
       ("[" list "]") ;; includes DataKinds
       (list "," list))

      ;; operators all have the same precedence
      (infixexp
       (id ":" infixexp) ;; keyword infix
       (id "':" infixexp) ;; DataKinds
       (id "$" infixexp) ;; special case
       (id "SYMID" infixexp))

      ;; WLDOs
      (wldo
       ("module" blk "where" blk)
       (blk "where" blk)
       ("let" blk "in")
       ("let" blk)
       ("do" blk)
       ("case" id "of" blk)
       ("\\case" blk) ;; LambdaCase
       )
      (blk
       ("{" blk "}")
       (blk ";" blk)
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

(defvar haskell-tng-smie:debug nil)
(defun haskell-tng-smie:debug (command)
  "An alternative to RETURN that outputs SMIE debugging
information, to aid in the creation of new rules."
  (let ((output " *haskell-tng-smie*"))
    (when (get-buffer output)
      (kill-buffer output))
    (let ((haskell-tng-smie:debug (get-buffer-create output))
          (inhibit-read-only t))
      (with-current-buffer haskell-tng-smie:debug
        (read-only-mode 1))
      (call-interactively command)
      (display-buffer output))))
(defun haskell-tng-smie:debug-newline ()
  (interactive)
  (haskell-tng-smie:debug #'newline-and-indent))
(defun haskell-tng-smie:debug-tab ()
  (interactive)
  (haskell-tng-smie:debug #'indent-for-tab-command))

;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Indentation
;;
;; See also the documentation for `smie-rules-function', which tends to be more
;; up to date.
;;
;; The concept of "virtual indentation" can be confusing. This function is
;; called multiple times for a single indentation command. `:before' does not
;; mean that we are indenting the next token, but is a request for the virtual
;; indentation of that token. For example, consider a `do' block, we may get an
;; `:after' and a `:before' for `do' which may be at column 20 but virtually at
;; column 0.
(defun haskell-tng-smie:rules (method arg)
  ;; see docs for `smie-rules-function'
  (when haskell-tng-smie:debug
    (let ((sym (symbol-at-point)))
      (with-current-buffer haskell-tng-smie:debug
        (insert (format "RULES: %S %S %S\n" method arg sym)))))

  ;; FIXME core indentation rules
  (pcase method

    (:elem
     (pcase arg
       ((or 'args 'basic) 0)

       ;; TODO consult a local table, populated by an external tool, containing
       ;; the parameter requirements for function calls. For simple cases, we
       ;; should be able to infer if the user wants to terminate ; or continue
       ;; "" the current line.
       ('empty-line-token
        ;; BUG smie-rule-next-p needs smie--after to be defined
        (setq smie--after (point))
        (when (smie-rule-next-p ";" "}") ";"))
       ))

    ;; Patterns of the form
    ;;
    ;;   {TOKEN TOKEN HEAD ; A ; B ; ...}
    ;;
    ;; get called with `:list-intro "HEAD"` when indenting positions A and B.
    (:list-intro
     (pcase arg
       ((or "<-" "=" "$") t)
       ))

    (:after
     (pcase arg
       ((or "let" "do" "of" "=" "in" "$" "->") 2)
       ("\\case" 2) ;; LambdaCase
       ("where" (if (smie-rule-parent-p "module") 0 2))
       ))

    (:before
     (pcase arg
       ;; first entries in a WLDO should aim to have the smallest indentation
       ;; possible. i.e. prefer
       ;;
       ;; blah = bloo where
       ;;   bloo = blu
       ;;
       ;; not
       ;;
       ;; blah = bloo where
       ;;               bloo = blu
       ((or "{" "where" "let" "do" "case" "$" "->")
        (smie-rule-parent))
       ("\\case" ;; LambdaCase
        (smie-rule-parent))
       ))

    ))

(defconst haskell-tng-smie:return
  '(comment-indent-new-line
    newline-and-indent
    newline
    haskell-tng-smie:debug-newline)
  "Users with custom newlines should add their command.")

(defvar-local haskell-tng-smie:indentations nil)
(defun haskell-tng-smie:indent-cycle ()
  "When invoked more than once, returns an alternative indentation level."
  ;; There is a design choice here: either we compute all the indentation levels
  ;; (including a recursive call to `smie-indent-calculate') and put them into a
  ;; ring that we cycle, or we push/pop with recalculation. We choose the
  ;; latter, because cache invalidation is easier.
  (if (or (member this-command haskell-tng-smie:return)
          (not
           (or (eq this-command last-command)
               (member last-command haskell-tng-smie:return))))
      (setq haskell-tng-smie:indentations nil)
    ;; TAB+TAB or RETURN+TAB
    (when (null haskell-tng-smie:indentations)
      (let ((prime (current-column)))
        (setq haskell-tng-smie:indentations
              (append
               ;; TODO backtab cycle in reverse
               (-remove-item prime (haskell-tng-smie:indent-alts))
               (list prime))))))
  (when haskell-tng-smie:debug
    (when-let (alts haskell-tng-smie:indentations)
      (with-current-buffer haskell-tng-smie:debug
        (insert (format "ALTS: %S\n" alts)))))
  (pop haskell-tng-smie:indentations))

(defun haskell-tng-smie:indent-alts ()
  "Returns a list of alternative indentation levels for the
current line."
  (let ((pos (point))
        indents)
    (save-excursion
      (end-of-line 0)
      (re-search-backward haskell-tng:regexp:toplevel nil t)
      (when-let (new (haskell-tng-smie:relevant-alts pos t))
        (setq indents (append new indents))))

    ;; alts are easier to use when ordered
    (setq indents (sort indents '<))

    ;; previous / next line should be top priority alts
    (--each '(1 -1)
      (save-excursion
        (forward-line it)
        (when-let (new (haskell-tng-smie:relevant-alts (point-at-eol) (< it 0)))
          (setq indents (append new indents)))))

    ;; TODO if this list is empty, return current+2

    (-distinct indents)))

(defun haskell-tng-smie:relevant-alts (bound before)
  "A list of indentation levels from point to BOUND.

BEFORE is t if the line appears before the indentation."
  (let ((start (point))
        relevant)
    (while (< (point) bound)
      (when (not
             (looking-at
              (rx (* space) (| "where" "let" "do") word-end)))
        (push (current-indentation) relevant))
      (when
          (and
           before
           (re-search-forward
            (rx symbol-start "<-" (+ " "))
            (line-end-position)
            t))
        (push (current-column) relevant))
      (forward-line))
    (goto-char start)
    (while (< (point) bound)
      (when (haskell-tng-layout:has-virtual-at-point)
        (push (current-column) relevant))
      (forward-char))
    relevant))

(defun haskell-tng-smie:setup ()
  (add-hook
   'after-change-functions
   #'haskell-tng-layout:cache-invalidation
   nil 'local)

  (add-hook
   'after-change-functions
   #'haskell-tng-lexer:state-invalidation
   nil 'local)

  (add-hook
   'smie-indent-functions
   #'haskell-tng-smie:indent-cycle
   nil 'local)

  (smie-setup
   haskell-tng-smie:grammar
   #'haskell-tng-smie:rules
   :forward-token #'haskell-tng-lexer:forward-token
   :backward-token #'haskell-tng-lexer:backward-token)
  )

;; SMIE wishlist, in order of desirability:
;;
;; 1. if the lexer could return lists of tokens.
;;
;; 2. exposing the parse tree so that s-expression navigation could be
;;    implemented and tests could be easier to write.
;;
;; 3. if the Emacs regexps allowed arbitrary zero-width matches, especially if
;;    syntax "categories" were allowed. i.e. in the Java regexp engine these are
;;    (?=X) (?!X) (?<=X) (?<!X) where X can be an Emacs syntax category. A
;;    limitation with Emacs regexps is that they only support word-start,
;;    word-end, symbol-start and symbol-end zero-width matchers.
;;
;; 4. ambiguous tokens. e.g. the word "via" is a keyword in a specific location,
;;    but can otherwise be used as a varid. I'd like to be able to lex it as (or
;;    "via" "VARID") so that it can appear in multiple places in the grammar.

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
