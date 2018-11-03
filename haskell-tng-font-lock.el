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

(defface haskell-tng:package
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Haskell packages."
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
 `((,(regexp-opt '("case" "class" "data" "default" "deriving" "do" "else"
                   "foreign" "if" "import" "in" "infix" "infixl"
                   "infixr" "instance" "let" "module" "newtype" "of"
                   "then" "type" "where" "_")
                 'words)
    . 'haskell-tng:keyword) ;; reservedid
   (,(regexp-opt '(".." ":" "::" "=" "|" "<-" ">" "->" "@" "~" "=>")
                 'symbols)
    . 'haskell-tng:keyword) ;; reservedop
   ;; lambda syntax may be followed by a trailing symbol
   ("\\_<\\(\\\\\\)" . 'haskell-tng:keyword)

   ;; TODO: contextual / multiline support for the import region.
   ;; qualified/hiding/as are keywords when used in imports
   ("\\_<import\\_>[[:space:]]+\\_<\\(qualified\\)\\_>" 1 'haskell-tng:keyword)
   ("\\_<import\\_>[^(]+?\\_<\\(hiding\\|as\\)\\_>" 1 'haskell-tng:keyword)
   ("\\_<import\\_>\\(?:[[:space:]]\\|qualified\\)+\\_<\\([[:upper:]]\\(?:\\.\\|\\w\\)*\\)\\_>"
    1 'haskell-tng:package)
   ("\\_<import\\_>[^(]+?\\_<as[[:space:]]+\\([[:upper:]]\\w+\\)"
    1 'haskell-tng:package)

   ("\\_<module\\_>[[:space:]]+\\_<\\([[:upper:]]\\w*\\)\\_>"
    1 'haskell-tng:package) ;; introducing modules

   ("\\_<\\(\\(?:[[:upper:]]\\w*\\.\\)+\\)"
    . 'haskell-tng:package) ;; uses of F.Q.N.s

   ("\\_<\\([[:upper:]]\\w*\\)\\_>" 0 'haskell-tng:constructor) ;; conid
   ("\\_<\\(:\\s_+\\)\\_>" 0 'haskell-tng:constructor) ;; consym
   ))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
