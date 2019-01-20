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

;; FIXME: massive hack. Holds an ordered list of (position . level) that close
;; an inferred layout block. This could be turned into a (cached) function call
;; plus some state in wldo-state.
(defvar-local haskell-tng-smie:wldos nil)

;; FIXME: massive hack. State of previous lexeme. Unsure how to remove this.
;; Ideally we would be able to return multiple tokens to SMIE and we wouldn't
;; need this.
;;
;; TODO: refactor so this stores the list of tokens to return at the current
;; point, and some information allowing cache invalidation.
(defvar-local haskell-tng-smie:wldo-state nil)

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
  (forward-comment (point-max))
  (if (eobp)
      "}"
    (let ((case-fold-search nil)
          (syntax (char-syntax (char-after)))
          (wldo-state haskell-tng-smie:wldo-state)
          (offside (car haskell-tng-smie:wldos)))
      (setq haskell-tng-smie:wldo-state nil)
      (cond
       ;; layout
       ((and (eq wldo-state 'start) (not (looking-at "{")))
        (push (haskell-tng:layout-close-and-level) haskell-tng-smie:wldos)
        (setq haskell-tng-smie:wldo-state 'middle)
        "{")
       ((when-let (close (car offside))
          (= (point) close))
        (pop haskell-tng-smie:wldos)
        "}")
       ((when-let (level (cdr offside))
          (and
           (= (current-column) level)
           (not (eq wldo-state 'middle))))
        (setq haskell-tng-smie:wldo-state 'middle)
        ";")

       ;; parens
       ((member syntax '(?\( ?\) ?\" ?$)) nil)

       ;; layout detection
       ((looking-at (rx word-start (| "where" "let" "do" "of") word-end))
        (setq haskell-tng-smie:wldo-state 'start)
        (haskell-tng-smie:last-match))

       ;; regexps
       ((or
         ;; known identifiers
         (looking-at haskell-tng:regexp:reserved)
         ;; symbols
         (looking-at (rx (+ (| (syntax word) (syntax symbol)))))
         ;; whatever the current syntax class is
         (looking-at (rx-to-string `(+ (syntax ,syntax)))))
        (haskell-tng-smie:last-match))))))

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
