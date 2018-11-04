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

(defface haskell-tng:toplevel
  '((t :inherit font-lock-function-name-face))
  "Haskell top level declarations."
  :group 'haskell-tng:faces)

;; TODO: pragmas
;;
;; TODO: numeric / char primitives?
;;
;; TODO: haddock, different face vs line comments, and some markup.
;;
;; TODO: multiline support for imports and type detection.
;;
;; TODO: consider comments where we currently check for spaces.
;;
;; TODO: consider ; in the "until the end of the line" searches.

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

     ;; types (multi-line support would improve this)
     ;; TODO bracketed types (when are these allowed)
     (,(rx-to-string '(: (|
                          (: line-start (+ space) "->")
                          (: symbol-start "::" symbol-end))
                         (+ space)
                         (group (+? (not (syntax comment-start))))
                         (| ?\; (syntax comment-start) line-end)))
      (1 'haskell-tng:type keep))
     (,(rx-to-string `(: line-start "data" (+ space)
                         (group (| ,conid ,consym))))
      (1 'haskell-tng:type))
     (,(rx-to-string `(: line-start (| "class" "instance") (+ space)
                         (group (+? anything))
                         (+ space) "where"))
      (1 'haskell-tng:type keep))
     ;; TypeApplications
     (,(rx-to-string `(: symbol-start "@" (+ space)
                         (group (opt ,qual) (| ,conid ,consym))))
      (1 'haskell-tng:type))

     ;; modules
     (,(rx-to-string `(: symbol-start "module" symbol-end (+ space)
                         symbol-start (group (opt ,qual) ,conid) symbol-end))
      1 'haskell-tng:module)

     ;; imports (multi-line support would improve this)
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
       nil nil (1 'haskell-tng:keyword) (2 'haskell-tng:module nil t))
      (,(rx-to-string `(: symbol-start (group (| ,conid ,consym)) symbol-end
                          (* space) "(..)"))
       nil nil (1 'haskell-tng:constructor))
      (,(rx-to-string `(: symbol-start (group (| ,conid ,consym)) symbol-end))
       nil nil (1 'haskell-tng:type)))

     ;; top-level
     (,(rx-to-string `(: line-start
                         (group (|
                                 (: (any lower ?_) (* wordchar))
                                 (: "(" (+? (syntax symbol)) ")")))
                         symbol-end))
      . 'haskell-tng:toplevel)

     ;; uses of F.Q.N.s
     (,(rx-to-string `(: symbol-start (+ (: ,conid "."))))
      . 'haskell-tng:module)

     ;; constructors
     (,(rx-to-string `(: symbol-start (| ,conid ,consym) symbol-end))
      . 'haskell-tng:constructor)

     )))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
