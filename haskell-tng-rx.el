;;; haskell-tng-rx.el --- Internal: regular expressions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  `rx' expressions and their compiled regexps; used by lexing, syntax table,
;;  fontification and more.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `rx' patterns that are reused as a very simple form of BNF grammar.
(defconst haskell-tng--rx-consym '(: ":" (+ (syntax symbol))))
(defconst haskell-tng--rx-conid '(: upper (* word)))
(defconst haskell-tng--rx-varid '(: (any lower ?_) (* word)))
(defconst haskell-tng--rx-symid `(| (+ (syntax symbol))
                                   (: "`" ,haskell-tng--rx-varid "`")))
(defconst haskell-tng--rx-qual `(+ (: ,haskell-tng--rx-conid (char ?.))))
(defconst haskell-tng--rx-kindsym `(: "'" ,haskell-tng--rx-consym)) ;; DataKinds
(defconst haskell-tng--rx-kindid `(: "'" ,haskell-tng--rx-conid)) ;; DataKinds

(defconst haskell-tng--keywords
  '("case" "class" "data" "default" "deriving" "do" "else"
    "foreign" "if" "import" "in" "infix" "infixl"
    "infixr" "instance" "let" "module" "newtype" "of"
    "then" "type" "where"))

(defun haskell-tng--rx-reserved (hack)
  "reservedid / reservedop.

This is a function, not a constant, because the lexer needs a
hack that would break fontification.

WORKAROUND https://debbugs.gnu.org/cgi/bugreport.cgi?bug=35119

TL;DR: regexps don't see some non-capture boundaries outside the
limit, so use POINT as a hint during lexing. If used in
fontification, a carefully positioned point in e.g. <--> would
give false positives." `(|
    (: word-start (| ,@haskell-tng--keywords "_") word-end)
    (: symbol-start "\\case" word-end) ;; LambdaCase
    (: "{..}") ;; RecordWildCards
    (: word-start "':" symbol-end) ;; DataKinds (consider foo':bar)
    (: ,(if hack
            '(| symbol-start word-end point)
          '(| symbol-start word-end))
       ;; EXT:UnicodeSyntax (also grammar)
       (| ".." "::" ":" "=" "|" "<-" "->" "@" "~" "=>")
       ,(if hack
            '(| symbol-end word-start point)
          '(| symbol-end word-start))
    )
    (| "[]" "()") ;; empty list / void
    (: symbol-start (char ?\\)
       ;; don't include ops like \\
       ,(if hack
            '(| space word-start point)
          '(| space word-start)))))

(defconst haskell-tng--rx-newline
  '(| ?\n
      (: symbol-start "--" (+ (not (any ?\n))) ?\n))
  "Newline or line comment.")

;; TODO consider using rx's eval to reuse forms instead of backticks

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiled regexps
;;
;; Word/symbol boundaries to help backwards regexp searches to be greedy and
;; are not in the BNF form as it breaks composability.
(defconst haskell-tng--rx-c-reserved
  (rx-to-string (haskell-tng--rx-reserved nil)))
(defconst haskell-tng--rx-c-reserved-hack
  (rx-to-string (haskell-tng--rx-reserved t)))
(defconst haskell-tng--rx-c-qual
  (rx-to-string `(: symbol-start ,haskell-tng--rx-qual)))
(defconst haskell-tng--rx-c-kindsym
  (rx-to-string `(: word-start ,haskell-tng--rx-kindsym)))
(defconst haskell-tng--rx-c-kindid
  (rx-to-string `(: word-start ,haskell-tng--rx-kindid)))
(defconst haskell-tng--rx-c-consym
  (rx-to-string haskell-tng--rx-consym))
(defconst haskell-tng--rx-c-conid
  (rx-to-string `(: word-start ,haskell-tng--rx-conid)))
(defconst haskell-tng--rx-c-varid
  (rx-to-string `(| (: word-start ,haskell-tng--rx-varid)
                    ;; TODO symids in brackets (==)
                    (: symbol-start (char ??) ,haskell-tng--rx-varid) ;; ImplicitParams
                    )))
(defconst haskell-tng--rx-c-symid
  (rx-to-string haskell-tng--rx-symid))

(provide 'haskell-tng-rx)
;;; haskell-tng-rx.el ends here
