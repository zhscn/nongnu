;;; haskell-tng-lexer.el --- Haskell Lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE compatible lexer, (sadly) stateful in order to support virtual tokens.
;;
;;  Outputs `=>' instead of `->' when in type position and attempts to classify
;;  most symbols into CONID / SYMID / VARID / etc classes.
;;
;;  See `haskell-tng-layout.el' for more details.
;;
;;; Code:

;; See also
;;
;; https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/parser/Lexer.x
;; https://github.com/carymrobbins/intellij-haskforce/blob/master/src/com/haskforce/parsing/_HaskellParsingLexer.flex
;; https://github.com/typelead/intellij-eta/blob/eta-ide/plugin/src/main/eta/IntelliJ/Plugin/Eta/Lang/Lexer/EtaParsingLexer.hs
;; https://www.haskell.org/hugs/downloads/2006-09/hugs98-Sep2006.tar.gz (src/parser.y)
;; https://github.com/haskell-lisp/yale-haskell/blob/master/parser/lexer.scm
;; https://bnfc.digitalgrammars.com/
;;
;; We could potentially use FFI + Flex to do the lexing for us, giving us access
;; to much more powerful regexp rules (Emacs doesn't support zero width
;; matchers, and the backwards regexps are not as greedy as they could be) but
;; we would probably have to write a SMIE replacement since Flex doesn't do
;; backwards parsing, and we'd need to write an FFI interface that may introduce
;; performance problems (converting Emacs buffers into the Flex input format).

(require 'smie)

(require 'haskell-tng-rx)
(require 'haskell-tng-layout)

;; The list of virtual tokens that must be played back at point, or `t' to
;; indicate that virtual tokens have already been played back at point and
;; normal lexing may continue.
(defvar-local haskell-tng--lexer-state nil)

;; A cons cell of the last known direction and point when forward or backward
;; lexing was called. Used to invalidate `haskell-tng--lexer-state' during
;; read-only navigation.
(defvar-local haskell-tng--lexer-last nil)

;; syntax-tables supported by SMIE
(defconst haskell-tng--lexer-fast-syntax
  (rx (| (syntax open-parenthesis)
         (syntax close-parenthesis)
         (syntax string-quote)
         (syntax string-delimiter))))

(defun haskell-tng--lexer-state-invalidation (_beg _end _pre-length)
  "For use in `after-change-functions' to invalidate the state of
the lexer."
  (when haskell-tng--lexer-state
    (setq haskell-tng--lexer-state nil)))

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
;; `haskell-tng--lexer-backward-token'.
(defun haskell-tng--lexer-forward-token ()
  (unwind-protect
      (let (case-fold-search)
        (haskell-tng--lexer-check-last 'forward)

        (if (consp haskell-tng--lexer-state)
            ;; continue replaying virtual tokens
            (haskell-tng--lexer-replay-virtual)

          (forward-comment (point-max))

          (setq haskell-tng--lexer-state
                (unless haskell-tng--lexer-state
                  (haskell-tng--layout-virtuals-at-point)))

          (cond
           ;; new virtual tokens
           (haskell-tng--lexer-state
            (haskell-tng--lexer-replay-virtual))

           ((eobp) nil)

           ;; reserved keywords take precedence
           ((looking-at haskell-tng--rx-c-reserved-hack)
            (pcase (haskell-tng--lexer-last-match)
              (":" "CONSYM")
              ("':" "KINDSYM") ;; DataKinds
              ((and "->" (guard (haskell-tng--lexer-arrow-is-type))) "=>")
              (other other)))

           ;; syntax tables (supported by `smie-indent-forward-token')
           ((looking-at haskell-tng--lexer-fast-syntax) nil)

           ;; known identifiers
           ;;
           ;; Ordering is important because regexps are greedy.
           ((looking-at haskell-tng--rx-c-qual)
            ;; Matches qualifiers separately from identifiers because the
            ;; backwards lexer is not greedy enough. Qualifiers are not
            ;; interesting from a grammar point of view so we ignore them.
            (haskell-tng--lexer-last-match nil "")
            (haskell-tng--lexer-forward-token))
           ((looking-at (rx "'["))
            ;; DataKinds
            (null (goto-char (+ (point) 1))))
           ((looking-at haskell-tng--rx-c-kindsym)
            ;; caveat: doesn't include typelevel lists, see fast-syntax
            (haskell-tng--lexer-last-match nil "KINDSYM"))
           ((looking-at haskell-tng--rx-c-kindid)
            (haskell-tng--lexer-last-match nil "KINDID"))
           ((looking-at haskell-tng--rx-c-consym)
            (haskell-tng--lexer-last-match nil "CONSYM"))
           ((looking-at haskell-tng--rx-c-conid)
            (haskell-tng--lexer-last-match nil "CONID"))
           ((looking-at haskell-tng--rx-c-varid)
            (haskell-tng--lexer-last-match nil "VARID"))
           ((looking-at haskell-tng--rx-c-symid)
            (haskell-tng--lexer-last-match nil "SYMID"))
           ;; TODO numeric literals

           ;; unknown things
           ((looking-at (rx (+ (| (syntax word) (syntax symbol)))))
            (haskell-tng--lexer-last-match))
           ;; single char
           (t
            (forward-char)
            (string (char-before))))))

    ;; save the state
    (haskell-tng--lexer-set-last 'forward)))

;; Implementation of `smie-backward-token' for Haskell, matching
;; `haskell-tng--lexer-forward-token'.
(defun haskell-tng--lexer-backward-token ()
  (unwind-protect
      (let (case-fold-search)
        (haskell-tng--lexer-check-last 'backward)

        (if (consp haskell-tng--lexer-state)
            (haskell-tng--lexer-replay-virtual 'reverse)

          (setq haskell-tng--lexer-state
                (unless haskell-tng--lexer-state
                  (haskell-tng--layout-virtuals-at-point)))

          (if haskell-tng--lexer-state
              (haskell-tng--lexer-replay-virtual 'reverse)

            (forward-comment (- (point)))
            (let ((lbp (line-beginning-position)))
             (cond
              ((bobp) nil)
              ((looking-back haskell-tng--rx-c-reserved-hack
                             (max lbp (- (point) 8)) 't)
               (pcase (haskell-tng--lexer-last-match 'reverse)
                 (":" "CONSYM")
                 ("':" "KINDSYM") ;; DataKinds
                 ((and "->" (guard (haskell-tng--lexer-arrow-is-type))) "=>")
                 (other other)))
              ((looking-back haskell-tng--lexer-fast-syntax
                             (max lbp (- (point) 1)))
               nil)
              ((looking-back haskell-tng--rx-c-qual lbp 't)
               (haskell-tng--lexer-last-match 'reverse "")
               (haskell-tng--lexer-backward-token))
              ((and (looking-at (rx "["))
                    (looking-back (rx "'") (- (point) 1)))
               ;; non-trivial inversion
               (goto-char (- (point) 1))
               (haskell-tng--lexer-backward-token))
              ((looking-back haskell-tng--rx-c-kindsym lbp 't)
               (haskell-tng--lexer-last-match 'reverse "KINDSYM"))
              ((looking-back haskell-tng--rx-c-kindid lbp 't)
               (haskell-tng--lexer-last-match 'reverse "KINDID"))
              ((looking-back haskell-tng--rx-c-consym lbp 't)
               (haskell-tng--lexer-last-match 'reverse "CONSYM"))
              ((looking-back haskell-tng--rx-c-conid lbp 't)
               (haskell-tng--lexer-last-match 'reverse "CONID"))
              ((looking-back haskell-tng--rx-c-varid lbp 't)
               (haskell-tng--lexer-last-match 'reverse "VARID"))
              ((looking-back haskell-tng--rx-c-symid lbp 't)
               (haskell-tng--lexer-last-match 'reverse "SYMID"))
              ((looking-back (rx (+ (| (syntax word) (syntax symbol)))) lbp 't)
               (haskell-tng--lexer-last-match 'reverse))
              (t
               (forward-char -1)
               (string (char-after))))))))

    (haskell-tng--lexer-set-last 'backward)))

(defun haskell-tng--lexer-set-last (direction)
  (setq haskell-tng--lexer-last (cons direction (point))))

(defun haskell-tng--lexer-check-last (direction)
  (when (and haskell-tng--lexer-state
             (not (equal haskell-tng--lexer-last (cons direction (point)))))
    (setq haskell-tng--lexer-state nil)))

(defun haskell-tng--lexer-replay-virtual (&optional reverse)
  "read a virtual token from state, set 't when all done"
  (unwind-protect
      (if reverse
          (unwind-protect
              (car (last haskell-tng--lexer-state))
            (setq haskell-tng--lexer-state
                  (butlast haskell-tng--lexer-state)))
        (pop haskell-tng--lexer-state))
    (unless haskell-tng--lexer-state
      (setq haskell-tng--lexer-state 't))))

(defun haskell-tng--lexer-last-match (&optional reverse alt)
  (goto-char (if reverse (match-beginning 0) (match-end 0)))
  (or alt (match-string-no-properties 0)))

(defun haskell-tng--lexer-arrow-is-type ()
  "Assuming that we just parsed a `->' arrow, search backwards to
find reserved tokens that can disambiguate the usecase as a type,
returning non-nil for types."
  (save-excursion
    (when (re-search-backward
           ;; TODO this list can probably be expanded to most (if not all) of
           ;; the keywords. This is very conservative.
           (rx (| (: symbol-start (char ?\\))
                  (: symbol-start (| "::" "of" "=>") symbol-end)))
           nil t)
      (member (match-string 0) '("::" "=>")))))

(provide 'haskell-tng-lexer)
;;; haskell-tng-lexer.el ends here
