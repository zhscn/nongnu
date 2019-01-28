;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE lexer, precedence table (providing s-expression navigation), and
;;  indentation rules. The lexer is stateful in order to support virtual tokens,
;;  and Layout aware, see `haskell-tng-layout.el' for more details.
;;
;;  Note that we don't support every aspect of the Haskell language. e.g. if we
;;  had access to all the operators in scope, and their fixity, we could create
;;  file-specific precendences. However, the complexity-to-benefit payoff is
;;  minimal.
;;
;;  Users may consult the SMIE manual to customise their indentation rules:
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE
;;
;;; Code:

(require 'smie)

(require 'haskell-tng-font-lock)
(require 'haskell-tng-layout)

;; The list of virtual tokens that must be played back at point, or `t' to
;; indicate that virtual tokens have already been played back at point and
;; normal lexing may continue.
(defvar-local haskell-tng-smie:state nil)

;; A cons cell of the last known direction and point when forward or backward
;; lexing was called. Used to invalidate `haskell-tng-smie:state' during
;; read-only navigation.
(defvar-local haskell-tng-smie:last nil)

(defun haskell-tng-smie:state-invalidation (_beg _end _pre-length)
  "For use in `after-change-functions' to invalidate the state of
the lexer."
  (when haskell-tng-smie:state
    (setq haskell-tng-smie:state nil)))

;; Implementation of `smie-forward-token' for Haskell, i.e.
;;
;; - Called with no argument should return a token and move to its end.
;; - If no token is found, return nil or the empty string.
;; - It can return nil when bumping into a parenthesis, which lets SMIE
;;   use syntax-tables to handle them in efficient C code.
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE-Lexer
;;
;; Note that this implementation is stateful as it can play back multiple
;; virtual tokens at a single point. This lexer could be made stateless if SMIE
;; were to support a 4th return type: a list of any of the above.
(defun haskell-tng-smie:forward-token ()
  (unwind-protect
      (let (case-fold-search)
        (when (and haskell-tng-smie:state
                   (not (equal haskell-tng-smie:last `(forward . ,(point)))))
          (setq haskell-tng-smie:state nil))

        (if (consp haskell-tng-smie:state)
            ;; continue replaying virtual tokens
            (haskell-tng-smie:replay-virtual)

          (forward-comment (point-max))

          ;; TODO: performance. Only request virtuals when they make sense...
          ;; e.g. on newlines, or following a WLDO (assuming a comment-aware
          ;; lookback is fast).
          (setq haskell-tng-smie:state
                (unless haskell-tng-smie:state
                  (haskell-tng-layout:virtuals-at-point)))

          (cond
           ;; new virtual tokens
           (haskell-tng-smie:state
            (haskell-tng-smie:replay-virtual))

           ;; syntax tables (supported by `smie-indent-forward-token')
           ((looking-at (rx (| (syntax open-parenthesis)
                               (syntax close-parenthesis)
                               (syntax string-quote)
                               (syntax string-delimiter))))
            nil)

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
            (string (char-before))))))

    ;; save the state
    (setq haskell-tng-smie:last `(forward . ,(point)))))

(defun haskell-tng-smie:replay-virtual ()
  ";; read a virtual token from state, set 't when all done"
  (unwind-protect
      (pop haskell-tng-smie:state)
    (unless haskell-tng-smie:state
      (setq haskell-tng-smie:state 't))))

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
  (add-to-list
   'after-change-functions
   #'haskell-tng-layout:cache-invalidation)

  (add-to-list
   'after-change-functions
   #'haskell-tng-smie:state-invalidation)

  (smie-setup
   haskell-tng-smie:grammar
   haskell-tng-smie:rules
   :forward-token #'haskell-tng-smie:forward-token
   ;; FIXME :backward-token #'haskell-tng-smie:backward-token
   ))

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
