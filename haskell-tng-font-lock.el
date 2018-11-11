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

;; TODO: regression tests https://github.com/Lindydancer/faceup
;;
;; TODO: pragmas
;;
;; TODO: numeric / char primitives?
;;
;; TODO: haddock, different face vs line comments, and some markup.
;;
;; TODO use levels so users can turn off type fontification

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `rx' patterns that are reused as a very simple form of BNF grammar
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is the `font-lock-keywords' table of matchers and highlighters.
(defconst
 haskell-tng:keywords
 ;; These regexps use the `rx' library so we can reuse common subpatterns. It
 ;; also increases the readability of the code and, in many cases, allows us to
 ;; do more work in a single regexp instead of multiple passes.
 (let ((conid haskell-tng:conid)
       ;;(qual haskell-tng:qual)
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
     (haskell-tng:explicit-type
      (0 'haskell-tng:type keep))
     (haskell-tng:topdecl
      (1 'haskell-tng:type keep))

     ;; ;; TODO multiline data/newtype/class/instance types
     ;; (,(rx-to-string `(: line-start "data" (+ space)
     ;;                     (group (| ,conid ,consym))))
     ;;  (1 'haskell-tng:type))
     ;; (,(rx-to-string `(: line-start (| "class" "instance") (+ space)
     ;;                     (group (+? anything))
     ;;                     (+ space) "where"))
     ;;  (1 'haskell-tng:type keep))
     ;; ;; TypeApplications
     ;; (,(rx-to-string `(: symbol-start "@" (* space)
     ;;                     ;; TODO: more liberal type application
     ;;                     (group (opt ,qual) (| ,conid ,consym))))
     ;;  (1 'haskell-tng:type))

     ;; TODO: multiline module / import sections

     ;; modules
     ;; (,(rx-to-string `(: symbol-start "module" symbol-end (+ space)
     ;;                     symbol-start (group (opt ,qual) ,conid) symbol-end))
     ;;  1 'haskell-tng:module)

     ;; imports
     ;; (,(rx-to-string '(: word-start "import" word-end)) ;; anchor matcher
     ;;  (,(rx-to-string `(: point (+ space) (group word-start "qualified" word-end)))
     ;;   nil nil (1 'haskell-tng:keyword))
     ;;  (,(rx-to-string `(: point
     ;;                      (opt (+ space) word-start "qualified" word-end)
     ;;                      (+ space) word-start (group (opt ,qual) ,conid) word-end))
     ;;   nil nil (1 'haskell-tng:module))
     ;;  (,(rx-to-string `(: point (+? (not (any ?\()))
     ;;                      word-start (group (| "hiding" "as")) word-end
     ;;                      (opt (+ space) word-start (group ,conid) word-end)))
     ;;   nil nil (1 'haskell-tng:keyword) (2 'haskell-tng:module nil t))
     ;;  (,(rx-to-string `(: symbol-start (group (| ,conid ,consym)) symbol-end
     ;;                      (* space) "(..)"))
     ;;   nil nil (1 'haskell-tng:constructor))
     ;;  (,(rx-to-string `(: symbol-start (group (| ,conid ,consym)) symbol-end))
     ;;   nil nil (1 'haskell-tng:type)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `function' matchers for use in `font-lock-keywords', and reusable in
;; the `font-lock-extend-region-functions' below. These set the match region and
;; return nil if there is not match in the limited search.
;;
;; Avoid compiling any regexes (i.e. `rx-to-string'), prefer `defconst' and `rx'.

(defconst haskell-tng:type
  ;; TODO literal types and generic lists ... eek!
  ;; TODO be more explicit about where class constraints can appear
  (let ((newline haskell-tng:newline)
        (typepart `(| (+ (any ?\( ?\) ?\[ ?\]))
                      (+ (any lower ?_))
                      (: (opt ,haskell-tng:qual)
                         (| "::" "=>" ,haskell-tng:conid ,haskell-tng:consym)))))
    `(: (+ (| space ,typepart))
        (* (opt ,newline (+ space)) "->" (+ (| space ,typepart))))))
(defconst haskell-tng:explicit-type-regexp
      (rx-to-string
      `(: symbol-start "::" (* space) (opt ,haskell-tng:newline) ,haskell-tng:type)))
(defun haskell-tng:explicit-type (limit)
  "Matches an explicit type, bounded by a closing paren."
  (when (re-search-forward (rx symbol-start "::" symbol-end) limit t)
    (goto-char (match-beginning 0))
    (when-let (bounded (haskell-tng:paren-close))
      (setq limit (min limit (+ 1 bounded))))
    (re-search-forward
     haskell-tng:explicit-type-regexp
     limit t)))

(defconst haskell-tng:topdecl-regexp
      (rx-to-string
       `(: line-start (| "data" "type" "newtype" "class" "instance") symbol-end
           (group (+? anything))
           (|
            (>= 2 (: (* space) ,haskell-tng:newline))
            (: symbol-start (| "where" "=") symbol-end)))))
(defun haskell-tng:topdecl (limit)
  "Matches the left hand side of a data, type, newtype, class or instance in group 1."
  (re-search-forward haskell-tng:topdecl-regexp limit t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `font-lock-extend-region-functions' procedures for extending the
;; region. Note that because we are using `font-lock-multiline' then multiline
;; patterns will always be rehighlighted as a group.
;;
;; Avoid compiling any regexes (i.e. `rx-to-string'), prefer `defconst' and `rx'.
(eval-when-compile
  ;; NOTE: font-lock-end is non-inclusive.
  (defvar font-lock-beg)
  (defvar font-lock-end))

;; TODO: more aggressive non-type chars
(defconst haskell-tng:non-type "[^\\{}]")

(defconst haskell-tng:extend-explicit-type-regexp
      (rx-to-string
          `(: symbol-start "::" symbol-end
             (*? ,haskell-tng:non-type) point)))
(defun haskell-tng:extend-explicit-type ()
  "Multiline explicit type signatures are considered."
  (goto-char font-lock-end)
  (when (re-search-backward
         haskell-tng:extend-explicit-type-regexp
         font-lock-beg t)
    (goto-char (match-beginning 0))
    (haskell-tng:explicit-type (point-max))
    (haskell-tng:extend)))

(defconst haskell-tng:extend-topdecl-regexp
      (rx-to-string
       `(: line-start (| "data" "type" "newtype") symbol-end
           (*? ,haskell-tng:non-type) point)))
(defun haskell-tng:extend-topdecl ()
  "Multiline data, type and newtype definitions."
  (goto-char font-lock-end)
  (when (re-search-backward
         haskell-tng:extend-topdecl-regexp
         font-lock-beg t)
    (goto-char (match-beginning 0))
    (haskell-tng:topdecl (point-max))
    (haskell-tng:extend)))

(defun haskell-tng:extend-module ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline module definitions are opened."
  nil)

(defun haskell-tng:extend-import ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline import definitions are opened."
  nil)

;; TODO multiline data / newtype / type definitions
;; TODO delete the paren and type extender and rely on growing from a seed

(defun haskell-tng:debug-extend (to)
  (message "extending `%s' to include `%s'!"
           (buffer-substring-no-properties font-lock-beg font-lock-end)
           (if (<= to font-lock-beg)
               (buffer-substring-no-properties to font-lock-beg)
             (if (<= font-lock-end to)
                 (buffer-substring-no-properties font-lock-end to)
               "BADNESS! Reduced the region"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
(defun haskell-tng:paren-close ()
  "Return the position of the next `)', if it closes the current paren depth."
  (save-excursion
    (when-let (close (ignore-errors (scan-lists (point) 1 1)))
      (goto-char (- close 1))
      (when (looking-at ")")
        (point)))))

(defun haskell-tng:extend ()
  "Extend the `font-lock-end' if point is further ahead."
  (when (< font-lock-end (point))
    (haskell-tng:debug-extend (point))
    (setq font-lock-end (point))
    nil))

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
