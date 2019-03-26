;;; haskell-tng-lexer.el --- Haskell Lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE compatible lexer, (sadly) stateful in order to support virtual tokens.
;;  See `haskell-tng-layout.el' for more details.
;;
;;; Code:

(require 'smie)

(require 'haskell-tng-font-lock)
(require 'haskell-tng-layout)

;; The list of virtual tokens that must be played back at point, or `t' to
;; indicate that virtual tokens have already been played back at point and
;; normal lexing may continue.
(defvar-local haskell-tng-lexer:state nil)

;; A cons cell of the last known direction and point when forward or backward
;; lexing was called. Used to invalidate `haskell-tng-lexer:state' during
;; read-only navigation.
(defvar-local haskell-tng-lexer:last nil)

;; syntax-tables supported by SMIE
(defconst haskell-tng-lexer:fast-syntax
  (rx (| (syntax open-parenthesis)
         (syntax close-parenthesis)
         (syntax string-quote)
         (syntax string-delimiter))))

(defun haskell-tng-lexer:state-invalidation (_beg _end _pre-length)
  "For use in `after-change-functions' to invalidate the state of
the lexer."
  (when haskell-tng-lexer:state
    (setq haskell-tng-lexer:state nil)))

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
;;
;; Any changes to this function must be reflected in
;; `haskell-tng-lexer:backward-token'.
(defun haskell-tng-lexer:forward-token ()
  (unwind-protect
      (let (case-fold-search)
        (haskell-tng-lexer:check-last 'forward)

        (if (consp haskell-tng-lexer:state)
            ;; continue replaying virtual tokens
            (haskell-tng-lexer:replay-virtual)

          (forward-comment (point-max))

          ;; TODO: performance. Only request virtuals when they make sense...
          ;; e.g. on newlines, or following a WLDO (assuming a comment-aware
          ;; lookback is fast).
          (setq haskell-tng-lexer:state
                (unless haskell-tng-lexer:state
                  (haskell-tng-layout:virtuals-at-point)))

          (cond
           ;; new virtual tokens
           (haskell-tng-lexer:state
            (haskell-tng-lexer:replay-virtual))

           ((eobp) nil)

           ;; syntax tables (supported by `smie-indent-forward-token')
           ((looking-at haskell-tng-lexer:fast-syntax) nil)

           ;; known identifiers
           ((looking-at haskell-tng:regexp:reserved)
            (haskell-tng-lexer:last-match))
           ((looking-at haskell-tng:regexp:varid)
            (haskell-tng-lexer:last-match nil "VARID"))
           ((looking-at haskell-tng:regexp:conid)
            (haskell-tng-lexer:last-match nil "CONID"))
           ;; TODO symid
           ;; TODO literals

           ((or
             ;; known identifiers
             (looking-at haskell-tng:regexp:reserved)
             ;; symbols
             (looking-at (rx (+ (| (syntax word) (syntax symbol))))))
            (haskell-tng-lexer:last-match))

           ;; single char
           (t
            (forward-char)
            (string (char-before))))))

    ;; save the state
    (haskell-tng-lexer:set-last 'forward)))

;; Implementation of `smie-backward-token' for Haskell, matching
;; `haskell-tng-lexer:forward-token'.
(defun haskell-tng-lexer:backward-token ()
  (unwind-protect
      (let (case-fold-search)
        (haskell-tng-lexer:check-last 'backward)

        (if (consp haskell-tng-lexer:state)
            (haskell-tng-lexer:replay-virtual 'reverse)

          (setq haskell-tng-lexer:state
                (unless haskell-tng-lexer:state
                  ;; TODO semicolon cannot be used as a separator and a line end
                  ;; in the grammar rules, so should we emit multiple tokens?
                  (haskell-tng-layout:virtuals-at-point)))

          (if haskell-tng-lexer:state
              (haskell-tng-lexer:replay-virtual 'reverse)

            (forward-comment (- (point)))
            (let ((lbp (min (point) (line-beginning-position))))
             (cond
              ((bobp) nil)
              ((looking-back haskell-tng-lexer:fast-syntax (- (point) 1)) nil)
              ;; known identifiers
              ((looking-back haskell-tng:regexp:reserved (- (point) 8))
               (haskell-tng-lexer:last-match 'reverse))
              ((looking-back haskell-tng:regexp:varid lbp 't)
               (haskell-tng-lexer:last-match 'reverse "VARID"))
              ((looking-back haskell-tng:regexp:conid lbp 't)
               (haskell-tng-lexer:last-match 'reverse "CONID"))
              ((looking-back (rx (+ (| (syntax word) (syntax symbol)))) lbp 't)
               (haskell-tng-lexer:last-match 'reverse))
              (t
               (forward-char -1)
               (string (char-after))))))))

    (haskell-tng-lexer:set-last 'backward)))

(defun haskell-tng-lexer:set-last (direction)
  (setq haskell-tng-lexer:last (cons direction (point))))

(defun haskell-tng-lexer:check-last (direction)
  (when (and haskell-tng-lexer:state
             (not (equal haskell-tng-lexer:last (cons direction (point)))))
    (setq haskell-tng-lexer:state nil)))

(defun haskell-tng-lexer:replay-virtual (&optional reverse)
  "read a virtual token from state, set 't when all done"
  (unwind-protect
      (if reverse
          (unwind-protect
              (car (last haskell-tng-lexer:state))
            (setq haskell-tng-lexer:state
                  (butlast haskell-tng-lexer:state)))
        (pop haskell-tng-lexer:state))
    (unless haskell-tng-lexer:state
      (setq haskell-tng-lexer:state 't))))

(defun haskell-tng-lexer:last-match (&optional reverse alt)
  (goto-char (if reverse (match-beginning 0) (match-end 0)))
  (or alt (match-string-no-properties 0)))

(provide 'haskell-tng-lexer)
;;; haskell-tng-lexer.el ends here
