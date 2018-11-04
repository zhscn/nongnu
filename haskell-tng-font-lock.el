;;; haskell-tng-font-lock.el --- Fontification for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  A fontification scheme for Haskell with a goal to visually differentiate
;;  between values and types, requiring multi-line analysis.
;;
;;  The detection of complex language constructs is not considered, for
;;  simplicity and speed. Maybe one day we could use
;;  https://github.com/tree-sitter/tree-sitter-haskell for 100% accurate
;;  parsing, but until that day, we do it the idiomatic Emacs way (with hacks
;;  and more hacks).
;;
;;  We try very hard to use only single line fontifications, since multiline
;;  introduces both a performance and maintenance penalty. For this reason, some
;;  very unusual styles of Haskell, although valid, may not be supported. For
;;  example, comments in unusual positions and line breaks after contextual
;;  markers (e.g. multiline imports may pick up incorrect colours).
;;
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Font-Lock-Mode
;;
;;; Code:

(defgroup haskell-tng:faces nil
  "Haskell font faces."
  :group 'haskell-tng)

(defface haskell-tng:keyword
  '((t :inherit font-lock-keyword-face))
  "Haskell reserved names and operators."
  :group 'haskell-tng:faces)

(defface haskell-tng:module
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Haskell modules (packages)."
  :group 'haskell-tng:faces)

(defface haskell-tng:type
  '((t :inherit font-lock-type-face))
  "Haskell types."
  :group 'haskell-tng:faces)

(defface haskell-tng:constructor
  '((t :inherit font-lock-constant-face))
  "Haskell constructors."
  :group 'haskell-tng:faces)

;; TODO: types (signatures, classes and imports)
;;
;; TODO: pragmas
;;
;; TODO: numeric / char primitives?
;;
;; TODO: haddock

(setq
 haskell-tng:keywords
 ;; These regexps use the `rx' library so we can reuse common subpatterns. It
 ;; also increases the readability of the code and, in many cases, allows us to
 ;; do more work in a single regexp instead of multiple passes.
 (let* ((conid '(: upper (* wordchar)))
        (qual `(: (+ (: ,conid (char ?.)))))
        (consym '(: ":" (+ (syntax symbol)))))
   `(;; reservedid / reservedop
     (,(rx-to-string
        '(|
          (: word-start
             (| "case" "class" "data" "default" "deriving" "do" "else"
                "foreign" "if" "import" "in" "infix" "infixl"
                "infixr" "instance" "let" "module" "newtype" "of"
                "then" "type" "where" "_")
             word-end)
          (: symbol-start
             (| ".." ":" "::" "=" "|" "<-" ">" "->" "@" "~" "=>")
             symbol-end)
          (: symbol-start (char ?\\))))
      . 'haskell-tng:keyword)

     ;; modules
     (,(rx-to-string `(: symbol-start "module" symbol-end (+ space)
                         symbol-start (group (opt ,qual) ,conid) symbol-end))
      1 'haskell-tng:module)

     ;; imports
     (,(rx-to-string '(: word-start "import" word-end)) ;; anchor matcher
      (,(rx-to-string `(: point (+ space) (group word-start "qualified" word-end)))
       nil nil (1 'haskell-tng:keyword))
      (,(rx-to-string `(: point
                          (opt (+ space) word-start "qualified" word-end)
                          (+ space) word-start (group (opt ,qual) ,conid) word-end))
       nil nil (1 'haskell-tng:module))
      (,(rx-to-string `(: point (+? (not (any ?\()))
                          word-start (group (| "hiding" "as")) word-end
                          (opt (+ space) word-start (group ,conid) word-end)))
       nil nil (1 'haskell-tng:keyword) (2 'haskell-tng:module nil t)))

     ;; uses of F.Q.N.s
     (,(rx-to-string `(: symbol-start (+ (: ,conid "."))))
      . 'haskell-tng:module)

     ;; constructors
     (,(rx-to-string `(: symbol-start (| ,conid ,consym) symbol-end))
      . 'haskell-tng:constructor)

     )))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
