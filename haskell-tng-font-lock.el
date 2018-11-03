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

;; TODO: types (signatures, classes and imports)
;;
;; TODO: pragmas
;;
;; TODO: numeric / char primitives?

;; FIXME: consider using rx instead of regexes... there are a lot of escapes
;; that obfuscate the meaning, plus we could use DRY.

(setq
 haskell-tng:keywords
 ;; These regexps use the `rx' library so we can reuse common subpatterns. It
 ;; also increases the readability of the code and, in many cases, allows us to
 ;; do more work in a single regexp instead of multiple passes.
 (let ((conid '(: upper (* wordchar)))
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

     ;; TODO: anchored matchers
     ;; TODO: contextual / multiline support for the import region.
     ;; qualified/hiding/as are keywords when used in imports
     ("\\_<import\\_>[[:space:]]+\\_<\\(qualified\\)\\_>" 1 'haskell-tng:keyword)
     ("\\_<import\\_>[^(]+?\\_<\\(hiding\\|as\\)\\_>" 1 'haskell-tng:keyword)
     ("\\_<import\\_>\\(?:[[:space:]]\\|qualified\\)+\\_<\\([[:upper:]]\\(?:\\.\\|\\w\\)*\\)\\_>"
      1 'haskell-tng:module)
     ("\\_<import\\_>[^(]+?\\_<as[[:space:]]+\\([[:upper:]]\\w+\\)"
      1 'haskell-tng:module)

     ;; introducing modules
     (,(rx-to-string '(: symbol-start "module" symbol-end (+ space)
                         symbol-start (group upper (* wordchar)) symbol-end))
      1 'haskell-tng:module)

     ;; uses of F.Q.N.s
     (,(rx-to-string `(: symbol-start (+ (: ,conid "."))))
      . 'haskell-tng:module)

     ;; constructors
     (,(rx-to-string `(: symbol-start (| ,conid ,consym) symbol-end))
      . 'haskell-tng:constructor)

     )))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
