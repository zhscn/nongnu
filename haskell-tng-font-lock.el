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

(require 'subr-x)

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

;; TODO: a macro to call rx-to-string at runtime that doesn't need (: )
;;
;; TODO: regression tests https://github.com/Lindydancer/faceup
;;
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

(defconst haskell-tng:conid '(: upper (* wordchar)))
(defconst haskell-tng:consym '(: ":" (+ (syntax symbol))))
(defconst haskell-tng:toplevel
  `(: line-start (group (| (: (any lower ?_) (* wordchar))
                           (: "(" (+? (syntax symbol)) ")")))
      symbol-end))

;; TODO a macro that wraps these consts with short-form names

(setq
 haskell-tng:keywords
 ;; These regexps use the `rx' library so we can reuse common subpatterns. It
 ;; also increases the readability of the code and, in many cases, allows us to
 ;; do more work in a single regexp instead of multiple passes.
 (let ((conid haskell-tng:conid)
       (qual `(: (+ (: ,haskell-tng:conid (char ?.)))))
       (consym haskell-tng:consym)
       (toplevel haskell-tng:toplevel))
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
             (| ".." ":" "::" "=" "|" "<-" "->" "@" "~" "=>")
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
     ;; TODO types vs constructor highlighting.
     ;; needs a multi-line anchor.

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
     (,(rx-to-string toplevel)
      . 'haskell-tng:toplevel)

     ;; uses of F.Q.N.s
     (,(rx-to-string `(: symbol-start (+ (: ,conid "."))))
      . 'haskell-tng:module)

     ;; constructors
     (,(rx-to-string `(: symbol-start (| ,conid ,consym) symbol-end))
      . 'haskell-tng:constructor)

     )))

;; TODO: consider previous/next symbol instead of whole line detection in
;; font-lock-extend-region-functions for super duper hyper perf.

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defun haskell-tng:multiline-faces ()
  "For use in `font-lock-extend-region-functions'.

Detects multiline patterns, such as multiline `module', `import'
and type signatures, setting `font-lock-beg' / `font-lock-end'
appropriately, returning nil."
  (save-excursion
    ;; TODO break this logic into multiple deffuns so we can use the replay
    ;; logic as intended.
    (goto-char font-lock-end)
    (let ((detect-type (rx (| (: symbol-start "::" symbol-end)
                              (: line-start (+ space) "->" symbol-end)))))
      (when (re-search-backward detect-type font-lock-beg t)
       ;; we're in the middle of a type signature. Close any dangling parens or
       ;; scan until we see a line that doesn't start with ->
        (if-let (close (haskell-tng:closing-paren))
            (when (< font-lock-end close)
              (haskell-tng:debug-extend close)
              (setq font-lock-end close))
          ;; TODO scan forward
          nil)))

    ;; TODO: detect -> and move to the start of the type (unless its a lambda)
    ;; TODO: detect module / import and check if its multiline
    ;; TODO: detect unbalanced parens and scan back for import
    ))

(defun haskell-tng:debug-extend (to)
  (message "extending `%s' to `%s'!"
           (buffer-substring-no-properties font-lock-beg font-lock-end)
           (buffer-substring-no-properties font-lock-end to)))

;; TODO: this feels like something that would be in the stdlib...
(defun haskell-tng:closing-paren ()
  "If point is in an unbalanced parenthesis return the point that
closes it, otherwise nil."
  (let* ((scan (syntax-ppss))
         (open (nth 1 scan)))
    (when open
      (save-excursion
        (goto-char open)
        (when (looking-at "(")
          (ignore-errors
            (forward-list)
            (backward-char)
            (point)))))))

(defun haskell-tng:mark-block ()
  ;; TODO: this is kinda obscure, replace with mark-defun when it is defined
  "For use as `font-lock-mark-block-function'."
  (let ((toplevel (rx-to-string haskell-tng:toplevel)))
    (right-char)
    (re-search-forward toplevel (point-max) 'limit)
    (move-beginning-of-line nil)
    (set-mark (point))
    (re-search-backward toplevel (point-min) 'limit)))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
