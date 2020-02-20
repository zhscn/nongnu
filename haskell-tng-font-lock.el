;;; haskell-tng-font-lock.el --- Fontification for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  A fontification scheme for Haskell with a goal to visually differentiate
;;  between values and types.
;;
;;  It is not possible to be completely accurate for all language extensions.
;;  For example TypeOperators and TypeApplications allow constructs that can
;;  only be disambiguated by semantic rules via the symbol table from the
;;  imported modules.
;;
;;  If an extension has been considered, but not implemented, the marker EXT:
;;  will appear with the extension name.
;;
;;  The detection of complex language constructs is not considered, for
;;  simplicity and speed. Maybe one day we could use
;;  https://github.com/tree-sitter/tree-sitter-haskell for 100% accurate
;;  parsing, but until that day, we do it the idiomatic Emacs way (with hacks
;;  and more hacks).
;;
;;  Some useful tools to assist with keywords and extend-region:
;;
;;  - https://github.com/Lindydancer/highlight-refontification
;;  - https://github.com/Lindydancer/font-lock-profiler
;;  - https://github.com/Lindydancer/font-lock-studio
;;
;;  The Emacs Lisp manual should be consulted
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Font-Lock-Mode
;;  in addition to `C-h f font-lock-keywords'
;;
;;; Code:

(require 'haskell-tng-rx)
(require 'haskell-tng-util)

(defgroup haskell-tng-faces nil
  "Haskell font faces."
  :group 'haskell-tng)

(defface haskell-tng-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Haskell reserved names and operators."
  :group 'haskell-tng-faces)

;; TODO module when defining and importing should be different than when using
;; (which should make the text more subtle rather than highlighted)
(defface haskell-tng-module-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Haskell modules (packages)."
  :group 'haskell-tng-faces)

(defface haskell-tng-type-face
  '((t :inherit font-lock-type-face))
  "Haskell types."
  :group 'haskell-tng-faces)

(defface haskell-tng-conid-face
  '((t :inherit font-lock-constant-face))
  "Haskell constructors."
  :group 'haskell-tng-faces)

(defface haskell-tng-cpp-face
  '((t :inherit font-lock-preprocessor-face))
  "Uses of CPP"
  :group 'haskell-tng-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is the `font-lock-keywords' table of matchers and highlighters.
(defconst
  haskell-tng--font-lock-keywords
  ;; These regexps use the `rx' library so we can reuse common subpatterns. It
  ;; also increases the readability of the code and, in many cases, allows us to
  ;; do more work in a single regexp instead of multiple passes.
  (let ((conid haskell-tng--rx-conid)
        ;;(qual haskell-tng--rx-qual)
        (consym haskell-tng--rx-consym)
        (qual haskell-tng--rx-qual)
        (bigspace `(| space ,haskell-tng--rx-newline)))
    `((,(rx bol "#" (* nonl) eol) ;; TODO doesn't work for "#include 'c'"
       (0 'haskell-tng-cpp-face)) ;; CPP

      ;; reservedid / reservedop
      (,haskell-tng--rx-c-reserved
       . 'haskell-tng-keyword-face)

      ;; Some things are not technically keywords but are always special so make
      ;; sense to be fontified as such.
      (,(rx (any ?\( ?\) ?\[ ?\] ?\{ ?\} ?,))
       (0 'haskell-tng-keyword-face))

      ;; TypeFamilies
      (,(rx word-start "type" (+ space) (group "family") word-end)
       (1 'haskell-tng-keyword-face))
      ;; EXT:TypeFamilies (associated types, is this the right extension?)

      ;; Types
      (haskell-tng:font:explicit-type:keyword
       (1 'haskell-tng-type-face keep))
      (haskell-tng:font:topdecl:keyword
       (1 'haskell-tng-type-face keep))
      (haskell-tng:font:type:keyword
       (1 'haskell-tng-type-face keep))
      (haskell-tng:font:deriving:keyword
       (1 'haskell-tng-keyword-face keep)
       (2 'haskell-tng-type-face keep))
      ;; TODO everything after a data constructor is a type

      ;; TODO unnamed newtype fields should be a type, not a constructor
      ;; TODO bug, multiple standalone instance declarations in a row do not fire
      ;; TODO bug \Foo{foo} doesn't highlight correctly

      ;; EXT:TypeApplications: It is not easy to disambiguate between type
      ;; applications and value extractor in a pattern. Needs work.
      ;; (,(rx-to-string `(: symbol-start "@" (* space)
      ;;                     (group (? ,qual) (| ,conid ,consym))))
      ;;  (1 'haskell-tng-type-face))

      ;; imports
      (haskell-tng:font:import:keyword
       (,(rx-to-string
          `(: line-start "import" (+ space)
              (group (? word-start "qualified" word-end)) (* space)
              ;; EXT:PackageImports
              ;; EXT:Safe, EXT:Trustworthy, EXT:Unsafe
              (group word-start (* ,qual) ,conid word-end) (* ,bigspace)
              (group (? word-start "hiding" word-end)) (* space)))
        (haskell-tng--font-lock-multiline-anchor-rewind) nil
        (1 'haskell-tng-keyword-face)
        (2 'haskell-tng-module-face)
        (3 'haskell-tng-keyword-face))
       ;; TODO combinations of as and hiding
       (,(rx-to-string `(: word-start (group "as") word-end (+ space)
                           word-start (group ,conid) word-end))
        (haskell-tng--font-lock-multiline-anchor-rewind) nil
        (1 'haskell-tng-keyword-face)
        (2 'haskell-tng-module-face))
       (haskell-tng--font-lock-explicit-conids
        (haskell-tng--font-lock-multiline-anchor-rewind 1)
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (0 'haskell-tng-conid-face keep))
       (,(rx-to-string `(: word-start ,conid word-end))
        (haskell-tng--font-lock-multiline-anchor-rewind 1)
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (0 'haskell-tng-type-face keep))
       ;; EXT:ExplicitNamespaces
       )

      (haskell-tng:font:module:keyword
       (,(rx-to-string `(: word-start "module" word-end (+ space)
                           (group word-start (* ,qual) ,conid word-end)))
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (1 'haskell-tng-module-face))
       (haskell-tng--font-lock-explicit-conids
        (haskell-tng--font-lock-multiline-anchor-rewind 2)
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (0 'haskell-tng-conid-face keep))
       (,(rx-to-string `(: word-start ,conid word-end))
        (haskell-tng--font-lock-multiline-anchor-rewind 2)
        (haskell-tng--font-lock-multiline-anchor-rewind)
        (0 'haskell-tng-type-face keep)))

      ;; TODO pragmas
      ;; TODO numeric / char primitives?
      ;; TODO haddock, different face vs line comments, and some markup.

      ;; uses of F.Q.N.s
      ;; TODO should perhaps be in a different font than module/import use, e.g.
      ;; lighter not bolder.
      (,(rx-to-string `(: symbol-start (+ ,qual)))
       . 'haskell-tng-module-face)

      ;; constructors
      (,(rx-to-string `(: symbol-start (| ,conid ,consym) symbol-end))
       . 'haskell-tng-conid-face)

      )))

(defun haskell-tng--font-lock-multiline-anchor-rewind (&optional group jump end)
  "MATCH-ANCHORED moving point to group beginning (plus JUMP) and declaring LIMIT.
Can be used as PRE-FORM or POST-FORM, allowing anchors to
refontify the previously matched region.

If END is non-nil, use group end (plus JUMP).

If there is no match for GROUP, move to the end of the line, canceling this ANCHOR."
  (setq group (or group 0))
  (if (not (match-string group))
      (end-of-line)
    (goto-char (if end
                   (match-end group)
                 (match-beginning group)))
    (when jump
      (forward-char jump))
    (match-end 0)))

(defun haskell-tng--font-lock-explicit-conids (limit)
  "Finds paren blocks of constructors when in an import statement.
Some complexity to avoid matching on operators."
  (when (re-search-forward
         ;; TODO word not just lower should be ok
         (rx (any lower) (* space) "(")
         limit t)
    (let ((open (point)))
      (when-let (close (haskell-tng--util-paren-close))
        (when (<= close limit)
          (goto-char open)
          ;; TODO do not highlight field names as conids (e.g. typeclass methods)
          (re-search-forward (rx (+ anything)) close t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are `function' matchers for use in `font-lock-keywords' and
;; `font-lock-extend-region-functions' procedures for extending the region.
;;
;; For these more complicated structures, the general rule is to find "negative
;; space" rather than to detect valid entries. Language extensions almost always
;; scupper any plan, e.g. TypeOperators and type literals.
;;
;; Note that because we are using `font-lock-multiline', multiline patterns will
;; always be re-highlighted as a group.
(eval-when-compile
  ;; NOTE: font-lock-end is non-inclusive.
  (defvar font-lock-beg)
  (defvar font-lock-end))

(defconst haskell-tng--font-lock-extend-region-functions
  '(font-lock-extend-region-wholelines
    font-lock-extend-region-multiline)
  "Used in `font-lock-extend-region-functions'.
Automatically populated by `haskell-tng--font-lock-multiline'")

;; TODO (perf) don't extend if the TRIGGER has a multiline prop already or
;; consider only using multiline instead of trying to add custom
;; font-lock-extend-region-functions entries.
(defmacro haskell-tng--font-lock-multiline (name trigger find &rest limiters)
  "Defines `font-lock-keywords' / `font-lock-extend-region-functions' entries.

TRIGGER and FIND are forms that produce a regexp, which is
memoised by this macro. FIND typically begins by repeating
TRIGGER.

The generated `haskell-tng:PREFIX-extend' searches backwards for
TRIGGER from the end of the region. If a match is found, FIND is
called with no limit, which will extend the region if there is a
match.

The generated `haskell-tng:PREFIX-keyword' searches forward for
TRIGGER, limited to the fontification region. The point is reset
to the beginning of the TRIGGER's match and FIND is then
searched. This function is ideal for inclusion in the mode's
`font-lock-keywords' list and behaves like a regexp.

The LIMITERS are function names that are called if the TRIGGER
succeeds and may further restrict the FIND search limit."
  ;; TODO allow limiters to be the function calls, or regexps, avoiding trivial
  ;; functions (and refactor existing trivial functions into regexps). Taking a
  ;; function name is kinda weird.
  (declare (indent defun))
  (let* ((sname (concat "haskell-tng:font:" (symbol-name name)))
         (regexp-1 (intern (concat sname ":trigger")))
         (regexp-2 (intern (concat sname ":matcher")))
         (keyword (intern (concat sname ":keyword")))
         (extend (intern (concat sname ":extend"))))
    (cl-flet
        ((finder (lim)
                 `(re-search-forward
                   ,regexp-2
                   (seq-min (cons ,lim (seq-remove 'null (seq-map 'funcall ',limiters))))
                   t)))
      `(progn
         (defconst ,regexp-1 ,trigger)
         (defconst ,regexp-2 ,find)
         (defun ,extend ()
           (goto-char font-lock-end)
           (when (and
                  ;; lots of conservative checks to make sure we never extend
                  ;; from, or into, a comment or string.
                  (not (nth 8 (syntax-ppss)))
                  (re-search-backward ,regexp-1 font-lock-beg t)
                  (not (nth 8 (syntax-ppss))))
             ,(finder '(point-max))
             (when (and
                    (not (nth 8 (syntax-ppss)))
                    (< font-lock-end (point)))
               (setq font-lock-end (point))
               nil)))
         (defun ,keyword (limit)
           (when (and
                  (re-search-forward ,regexp-1 limit t)
                  ;; TODO if the last search got us into a string or comment. We
                  ;; should recurse, otherwise we miss valid matches in the
                  ;; region. This hack just tries once more.
                  (or
                   (not (nth 8 (syntax-ppss)))
                   (re-search-forward ,regexp-1 limit t))
                  (not (nth 8 (syntax-ppss))))
             (goto-char (match-beginning 0))
             ,(finder 'limit)))
         ;; TODO is this needed since we use multiline?
         ;;(add-to-list 'haskell-tng--font-lock-extend-region-functions ',extend t)
         ))))

(haskell-tng--font-lock-multiline explicit-type
                            (rx symbol-start "::" symbol-end)
                            (rx symbol-start "::" symbol-end (group (+ anything)))
                            haskell-tng--util-paren-close
                            haskell-tng--util-indent-close-previous
                            haskell-tng--util-type-ender)
;; TODO commas end a type signature in a record of functions (but can be used in tuples, so complex)
;; TODO since there is no way to exit based on context, we will match :: inside strings and comments

(haskell-tng--font-lock-multiline topdecl
                            (rx line-start (| "data" "newtype" "class" "instance") word-end)
                            (rx line-start (| "data" "newtype" "class" "instance") word-end
                                (group (+? anything))
                                (| (: line-start symbol-start)
                                   (: symbol-start (| "where" "=") symbol-end))))

;; TODO TypeFamilies type associations (not at line-start)
(haskell-tng--font-lock-multiline type
                            (rx line-start "type" word-end)
                            (rx line-start "type" word-end (group (+ anything)))
                            haskell-tng--util-indent-close)

;; DeriveAnyClass
;; DerivingStrategies
;; GeneralizedNewtypeDeriving
;; EXT:DerivingVia
;; EXT:StandaloneDeriving
(haskell-tng--font-lock-multiline deriving
                            (rx word-start "deriving" word-end)
                            (rx word-start "deriving" word-end
                                (+ space) (group (? (| "anyclass" "stock" "newtype") word-end))
                                ;; TODO support a lone derivation without brackets
                                (* space) ?\( (group (* anything)) ?\))
                            haskell-tng--util-indent-close)

(haskell-tng--font-lock-multiline import
                            (rx line-start "import" word-end)
                            (rx line-start "import" word-end
                                (+ (not (any ?\( )))
                                (? "(" (group (+ anything))))
                            haskell-tng--util-indent-close)

(haskell-tng--font-lock-multiline module
                            (rx line-start "module" word-end)
                            (rx line-start "module" word-end
                                (+ space)
                                (group word-start (+ (not (any space))) word-end)
                                (group (+ anything))
                                word-start "where" word-end)
                            haskell-tng--util-next-where)

(provide 'haskell-tng-font-lock)
;;; haskell-tng-font-lock.el ends here
