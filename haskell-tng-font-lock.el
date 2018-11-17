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

     ;; Types
     (haskell-tng:explicit-type-keyword
      (1 'haskell-tng:type keep))
     (haskell-tng:topdecl
      (1 'haskell-tng:type keep))
     (haskell-tng:type
      (1 'haskell-tng:type keep))
     (haskell-tng:deriving
      (1 'haskell-tng:keyword keep)
      (2 'haskell-tng:type keep))

     ;; TODO types in import / export statements
     ;; TODO ExplicitNamespaces to disambiguate TypeOperators

     ;; TypeApplications (very conservative)
     (,(rx-to-string `(: symbol-start "@" (* space)
                         (group (opt ,qual) (| ,conid ,consym))))
      (1 'haskell-tng:type))

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
;; For these more complicated structures, the general rule is to find "negative
;; space" rather than to detect valid entries. Language extensions almost always
;; scupper any plan, e.g. TypeOperators and type literals.

(defun haskell-tng:topdecl (limit)
  "Matches the left hand side of a data, newtype, class or instance in group 1."
  (re-search-forward
   (rx
    line-start (| "data" "newtype" "class" "instance") symbol-end
    (group (+? anything))
    (|
     (: line-start symbol-start)
     (: symbol-start (| "where" "=") symbol-end)))
   limit t))

(defun haskell-tng:type (limit)
  "Matches types in group 1."
  (when (re-search-forward
         (rx line-start "type" symbol-end)
         limit t)
    (goto-char (match-beginning 0))
    (let ((indent (haskell-tng:indent-close)))
      (re-search-forward
       (rx line-start "type" symbol-end
           (+ space) (group (+ anything)))
       (min limit (or indent limit))))))

(defun haskell-tng:deriving (limit)
  "Matches a deriving section putting keywords in group 1, types in group 2."
  ;; DeriveAnyClass
  ;; DerivingStrategies
  ;; GeneralizedNewtypeDeriving
  ;; TODO DerivingVia
  ;; TODO StandaloneDeriving
  (when (re-search-forward
         (rx symbol-start "deriving" symbol-end)
         limit t)
    (goto-char (match-beginning 0))
    (let ((indent (haskell-tng:indent-close)))
      (re-search-forward
       (rx
        symbol-start "deriving" (+ space)
        (group (opt (| "anyclass" "stock" "newtype"))) (* space)
        ?\( (group (* anything)) ?\))
       (min limit (or indent limit)) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `font-lock-extend-region-functions' procedures for extending the
;; region. Note that because we are using `font-lock-multiline' then multiline
;; patterns will always be rehighlighted as a group.
(eval-when-compile
  ;; NOTE: font-lock-end is non-inclusive.
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defmacro haskell-tng:multiline (prefix trigger find)
  "Defines `font-lock-keywords' / `font-lock-extend-region-functions' entries.

TRIGGER is a referentially transparent form that produces a regexp.

FIND is a form that must behave the same as `re-search-forward',
i.e. setting the match groups and placing point after the match.
The variable `limit' is dynamically bound within this form.

The generated `haskell-tng:PREFIX-extend' uses searches
backwards from the end of the proposed region with TRIGGER. If a
match is found, then FIND is evaluated with an unlimited limit to
calculate the end position, which may extend the region.

The generated `haskell-tng:PREFIX-keyword' searches forward for
TRIGGER within the fontification limit. The point is reset to the
beginning of the TRIGGER's match and FIND is evaluated.

`font-lock-multiline' ensures that the full match is painted with
the multiline property and should not not require further
expansion.

Use `pp-macroexpand-expression' to debug."
  ;; TODO (perf) don't call FIND or extend if there is a multiline property
  ;; TODO simplify FIND to use paren-close / indent-close automatically?
  (let* ((name (symbol-name prefix))
         (regexp (intern (concat name "-regexp")))
         (match (intern (concat name "-keyword")))
         (extend (intern (concat name "-extend"))))
    `(progn
       (defconst ,regexp ,trigger)
       (defun ,match (limit)
         (when (re-search-forward ,regexp limit t)
           (goto-char (match-beginning 0))
           ,find))
       (defun ,extend ()
         (goto-char font-lock-end)
         (when (re-search-backward ,regexp font-lock-beg t)
           (goto-char (match-beginning 0)) ;; is this needed?
           (let ((limit (point-max))) ,find)
           (when (< font-lock-end (point))
             ;;(haskell-tng:debug-extend (point))
             (setq font-lock-end (point))
             nil))))))

(pp-macroexpand-expression
 '(haskell-tng:multiline
   haskell-tng:explicit-type
   (rx symbol-start "::" symbol-end)
   (let ((paren (haskell-tng:paren-close))
         (indent (haskell-tng:indent-close (- (point) 1))))
     (re-search-forward
      (rx symbol-start "::" symbol-end (group (+ anything)))
      (min limit (or paren limit) (or indent limit)) t))))

(haskell-tng:multiline
 haskell-tng:explicit-type
 (rx symbol-start "::" symbol-end)
 (let ((paren (haskell-tng:paren-close))
       (indent (haskell-tng:indent-close (- (point) 1))))
   (re-search-forward
    (rx symbol-start "::" symbol-end (group (+ anything)))
    (min limit (or paren limit) (or indent limit)) t)))

(defun haskell-tng:extend-topdecl ()
  "Multiline data, newtype, class and instance top level definitions."
  (goto-char font-lock-end)
  (when (re-search-backward
         (rx line-start (| "data" "newtype" "class" "instance") symbol-end)
         font-lock-beg t)
    (goto-char (match-beginning 0))
    (haskell-tng:topdecl (point-max))
    (haskell-tng:extend)))

(defun haskell-tng:extend-type ()
  "Multiline type top-level definitions."
  (goto-char font-lock-end)
  (when (re-search-backward
         (rx line-start "type" symbol-end)
         font-lock-beg t)
    (goto-char (match-beginning 0))
    (haskell-tng:type (point-max))
    (haskell-tng:extend)))

(defun haskell-tng:extend-deriving ()
  "Multiline deriving definitions."
  (goto-char font-lock-end)
  (when (re-search-backward
         (rx symbol-start "deriving" symbol-end)
         font-lock-beg t)
    (goto-char (match-beginning 0))
    (haskell-tng:deriving (point-max))
    (haskell-tng:extend)))

(defun haskell-tng:extend-module ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline module definitions are opened."
  nil)

(defun haskell-tng:extend-import ()
  "For use in `font-lock-extend-region-functions'.
Ensures that multiline import definitions are opened."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
(defun haskell-tng:paren-close (&optional pos)
  "The next `)', if it closes `POS's paren depth."
  (save-excursion
    (goto-char (or pos (point)))
    (when-let (close (ignore-errors (scan-lists (point) 1 1)))
      (goto-char (- close 1))
      (when (looking-at ")")
        (point)))))

(defun haskell-tng:indent-close (&optional pos)
  "The beginning of the line with indentation that closes `POS'."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((level (current-column)))
      (catch 'closed
        (while (and (forward-line) (not (eobp)))
          (when (<= (current-indentation) level)
            (throw 'closed (point))))
        nil))))

;; TODO: should these be in the macro?
(defun haskell-tng:extend ()
  "Extend the `font-lock-end' if point is further ahead."
  (when (< font-lock-end (point))
    ;;(haskell-tng:debug-extend (point))
    (setq font-lock-end (point))
    nil))

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
