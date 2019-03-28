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
;; Word/symbol boundaries to help backwards regexp searches to be greedy
(defconst haskell-tng:rx:consym '(: (or "'" ":") ;; Datakinds
                                    (+ (syntax symbol))))
(defconst haskell-tng:rx:conid '(: word-start upper (* word)))
(defconst haskell-tng:rx:varid '(: word-start (any lower ?_) (* (any word))))
(defconst haskell-tng:rx:symid '(: (+ (syntax symbol))))
(defconst haskell-tng:rx:qual `(: symbol-start
                                  (+ (: ,haskell-tng:rx:conid (char ?.)))))

(defconst haskell-tng:rx:reserved
  '(|
    (: word-start
       (| "case" "class" "data" "default" "deriving" "do" "else"
          "foreign" "if" "import" "in" "infix" "infixl"
          "infixr" "instance" "let" "module" "newtype" "of"
          "then" "type" "where" "_")
       word-end)
    (: symbol-start
       (| ".." ":" "::" "=" "|" "<-" "->" "@" "~" "=>")
       symbol-end)
    (: symbol-start (char ?\\)))
  "reservedid / reservedop")

(defconst haskell-tng:rx:toplevel
  ;; TODO multi-definitions, e.g. Servant's :<|>
  `(: line-start (group (| ,haskell-tng:rx:varid
                           (: "(" (+? (syntax symbol)) ")")))
      symbol-end))
;; note that \n has syntax `comment-end'
(defconst haskell-tng:rx:newline
  '(| (syntax comment-end)
      (: symbol-start
         "--"
         (+ (not (syntax comment-end)))
         (syntax comment-end)))
  "Newline or line comment.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiled regexps
(defconst haskell-tng:regexp:reserved
  (rx-to-string haskell-tng:rx:reserved))
(defconst haskell-tng:regexp:qual
  (rx-to-string haskell-tng:rx:qual))
(defconst haskell-tng:regexp:consym
  (rx-to-string haskell-tng:rx:consym))
(defconst haskell-tng:regexp:conid
  (rx-to-string haskell-tng:rx:conid))
(defconst haskell-tng:regexp:varid
  (rx-to-string haskell-tng:rx:varid))
(defconst haskell-tng:regexp:symid
  (rx-to-string haskell-tng:rx:symid))

(provide 'haskell-tng-rx)
;;; haskell-tng-rx.el ends here
