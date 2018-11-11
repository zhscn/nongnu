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

;; TODO: regression tests https://github.com/Lindydancer/faceup
;;
;; TODO: pragmas
;;
;; TODO: numeric / char primitives?
;;
;; TODO: haddock, different face vs line comments, and some markup.

(defconst haskell-tng:conid '(: upper (* wordchar)))
(defconst haskell-tng:qual `(: (+ (: ,haskell-tng:conid (char ?.)))))
(defconst haskell-tng:consym '(: ":" (+ (syntax symbol)))) ;; TODO exclude ::, limited symbol set
(defconst haskell-tng:toplevel
  `(: line-start (group (| (: (any lower ?_) (* wordchar))
                           (: "(" (+? (syntax symbol)) ")")))
      symbol-end))
;; note that \n has syntax `comment-end'
(defconst haskell-tng:newline
  '(| (syntax comment-end)
      (: symbol-start
         "--"
         (+ (not (syntax comment-end)))
         (+ (syntax comment-end))))
  "Newline or line comment.")
(defconst haskell-tng:type
  ;; TODO literal types and generic lists ... eek!
  (let ((typepart `(| (+ (any ?\( ?\) ?\[ ?\]))
                      (+ (any lower ?_))
                      (: (opt ,haskell-tng:qual)
                         (| "::" ,haskell-tng:conid ,haskell-tng:consym)))))
    `(: (opt ,haskell-tng:newline) (+ (| space ,typepart))
        (* (opt ,haskell-tng:newline (+ space)) "->" (+ (| space ,typepart)))))
  "An explicit type")

;; TODO a macro that wraps these consts with short-form names

;; TODO use the levels support so users can turn off type fontification
(setq
 haskell-tng:keywords
 ;; These regexps use the `rx' library so we can reuse common subpatterns. It
 ;; also increases the readability of the code and, in many cases, allows us to
 ;; do more work in a single regexp instead of multiple passes.
 (let ((conid haskell-tng:conid)
       (qual haskell-tng:qual)
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

     ;; types
     (haskell-tng:explicit-type-paint
      (0 'haskell-tng:type keep))
     ;; TODO multiline data/newtype/class/instance types
     (,(rx-to-string `(: line-start "data" (+ space)
                         (group (| ,conid ,consym))))
      (1 'haskell-tng:type))
     (,(rx-to-string `(: line-start (| "class" "instance") (+ space)
                         (group (+? anything))
                         (+ space) "where"))
      (1 'haskell-tng:type keep))
     ;; TypeApplications
     (,(rx-to-string `(: symbol-start "@" (* space)
                         ;; TODO: more liberal type application
                         (group (opt ,qual) (| ,conid ,consym))))
      (1 'haskell-tng:type))

     ;; TODO: multiline module / import sections

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

(defvar haskell-tng:explicit-type-regex
  (rx-to-string `(: point "::" (* space) ,haskell-tng:type))
  "Cache of a regex internal to `haskell-tng:explicit-type'")
(defun haskell-tng:explicit-type (limit)
  "Matches an explicit type at point, bounded by a closing paren."
  (let ((end (min limit (or (haskell-tng:paren-close) limit))))
    (re-search-forward haskell-tng:explicit-type-regex end t)))
(defun haskell-tng:explicit-type-paint (limit)
  ;; ideally we would use an anchored `haskell-tng:explicit-type' with a `::'
  ;; trigger, but there is a bug in GNU Emacs where anchored functions receive a
  ;; much smaller `limit' than `font-lock-end' requested
  ;; https://lists.gnu.org/archive/html/emacs-devel/2018-11/msg00136.html
  "Matches an explicit type at point, bounded by a closing paren."
  (when (re-search-forward (rx symbol-start "::" symbol-end) limit t)
    (goto-char (match-beginning 0))
    (haskell-tng:explicit-type limit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below are `font-lock-extend-region-functions' procedures for extending the
;; region. Note that because we are using `font-lock-multiline' then multiline
;; patterns will always be rehighlighted as a group.
(eval-when-compile
  ;; NOTE: font-lock-end is non-inclusive.
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defconst haskell-tng:non-import
  ;; TODO: exclude more non-import/export characters. ideas: dots that aren't
  ;; (..) or part of a symbolic import, symbolic operators that are not
  ;; surrounded by parens.
  (rx (| ?\" ?\\))
  "Matches that should never exist in the parens of an import or export")
(defun haskell-tng:extend-parens-open ()
  "For use in `font-lock-extend-region-functions'.
Expand the region to include the opening parenthesis.
The caller loops until everything is opened."
  (goto-char font-lock-beg)
  ;; TODO: exit early if in comment
  (when-let (open (nth 1 (syntax-ppss)))
    (goto-char open)
    (when (looking-at "(")
      (unless (re-search-forward haskell-tng:non-import font-lock-beg t)
        ;;(haskell-tng:debug-extend open)
        (setq font-lock-beg open)))))

(defun haskell-tng:extend-parens-close ()
  "For use in `font-lock-extend-region-functions'.
Expand the region to include a closing parenthesis.
The caller loops until everything is closed."
  (goto-char font-lock-end)
  ;; TODO: exit early if in comment
  (when-let (close (haskell-tng:paren-close))
    (let ((end (+ 1 close)))
      (goto-char end)
      (unless (re-search-backward haskell-tng:non-import font-lock-end t)
        ;;(haskell-tng:debug-extend end)
        (setq font-lock-end end)))))

(defun haskell-tng:paren-close ()
  "Return the position of the next `)', if it closes the current paren depth."
  (interactive) ;; TODO for manual testing
  (save-excursion
    (when-let (close (ignore-errors (scan-lists (point) 1 1)))
      (goto-char (- close 1))
      (when (looking-at ")")
        (point)))))

(setq
  haskell-tng:beg-type
  ;; TODO: more restrictive search, add more like \ and =
  (rx symbol-start "::" symbol-end (* (not (any ?\\ ?=)))))
(defun haskell-tng:extend-type-open ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline type signatures are opened."
  (goto-char font-lock-beg)
  ;; TODO: exit early if in comment
  ;; TODO: maximum lookback for a type
  (when (re-search-backward haskell-tng:beg-type (point-min) t)
    (let ((beg (match-beginning 0)))
      (when (< beg font-lock-beg)
        (goto-char beg)
        ;; validate that it's actually a type
        (haskell-tng:explicit-type (point-max)) ;; would not be needed if backscan was more reliable
        (when (<= font-lock-beg (match-end 0))
          ;;(haskell-tng:debug-extend beg)
          (setq font-lock-beg beg)
          nil)))))

(defun haskell-tng:extend-type-close ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline type signatures are closed."
  (goto-char font-lock-end)
  ;; TODO: exit early if in comment
  (when (re-search-backward haskell-tng:beg-type font-lock-beg t)
    (let ((beg (match-beginning 0)))
      (goto-char beg)
      (haskell-tng:explicit-type (point-max))
      (let ((end (match-end 0)))
        (when (< font-lock-end end)
          ;;(haskell-tng:debug-extend end)
          (setq font-lock-beg end)
          nil)))))

(defun haskell-tng:extend-defns ()
  "Extends data, type, class and instance definitons to include their full type part."
  nil
  )

(defun haskell-tng:extend-module-open ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline `module' definitions are opened."
  nil)

(defun haskell-tng:extend-module-close ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline `module' definitions are closed."
  nil)

(defun haskell-tng:extend-import-open ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline `import' definitions are opened."
  nil)

(defun haskell-tng:extend-import-close ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline `import' definitions are closed."
  nil)

;; TODO multiline data / newtype / type definitions

(defun haskell-tng:debug-extend (to)
  (message "extending `%s' to include `%s'!"
           (buffer-substring-no-properties font-lock-beg font-lock-end)
           (if (<= to font-lock-beg)
               (buffer-substring-no-properties to font-lock-beg)
             (if (<= font-lock-end to)
                 (buffer-substring-no-properties font-lock-end to)
               "BADNESS! Reduced the region"))))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
