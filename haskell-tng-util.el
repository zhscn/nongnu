;;; haskell-tng-util.el --- Helpful Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;; Useful common utilities.
;;
;;; Code:

;; TODO move things to single use sites (twas premature abstraction!)

(eval-when-compile
  (require 'cl))
(require 'subr-x)
(require 'xdg)

(defun haskell-tng--util-paren-close (&optional pos)
  "The next `)', if it closes `POS's paren depth."
  (save-excursion
    (goto-char (or pos (point)))
    (when-let (close (ignore-errors (scan-lists (point) 1 1)))
      (goto-char (- close 1))
      (when (looking-at ")")
        (point)))))

;; TODO comment / paren aware, like haskell-tng:layout-of-next-token
;; TODO refactor to share code with haskell-tng:layout-of-next-token
(defun haskell-tng--util-indent-close (&optional pos)
  "The beginning of the line with indentation that closes `POS'."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((level (current-column)))
      (block closed
        (while (and (forward-line) (not (eobp)))
          (when (<= (current-indentation) level)
            (return-from closed (point))))))))

(defun haskell-tng--util-type-ender (&optional pos)
  ;; trivial, should just be called as an inline regexp
  (save-excursion
    (goto-char (or pos (point)))
    (re-search-forward (rx (| "<-" "=") symbol-end) nil t)))

(defun haskell-tng--util-next-where (&optional pos)
  ;; trivial, should just be called as an inline regexp
  "The next `where'"
  (save-excursion
    (goto-char (or pos (point)))
    (re-search-forward (rx word-start "where" word-end) nil t)))

(defun haskell-tng--util-indent-close-previous ()
  "Indentation closing the previous symbol."
  (save-excursion
    (forward-symbol -1)
    (haskell-tng--util-indent-close)))

(defun haskell-tng--util-locate-dominating-file (regexp)
  "`locate-dominating-file' but starting from `default-directory'
and taking a regexp."
  (locate-dominating-file
   default-directory
   (lambda (dir) (directory-files dir nil regexp))))

(defmacro haskell-tng--util-until (test &rest body)
  ;; https://lists.gnu.org/r/emacs-devel/2018-10/msg00250.html
  ;; by Stefan Monnier
  "Run BODY while TEST is non-nil, returning the final TEST."
  (let ((res (gensym "res")))
    `(let (,res)
       (while (not (setq ,res ,test)) ,@body)
       ,res)))

(defun haskell-tng--util-import-symbol (module &optional as sym)
  "Adds an import for MODULE."
  ;; TODO outsource to `hsimport' when it does de-duping and formatting.
  (save-excursion
    (goto-char (point-min))
    ;; TODO comment / text resilience
    (if (re-search-forward (rx line-start "import" word-end) nil t)
        (forward-line 0)
      (re-search-forward (rx line-start "module" word-end))
      (forward-line 1)
      (insert "\n"))
    (let ((beg (point)))
      (insert
       "import "
       (cond
        ((and (null as) (null sym))
         module)
        ((null as)
         (concat module " (" sym ")"))
        ((eq t as)
         (concat "qualified " module))
        (t
         (concat "qualified " module " as " as)))
       "\n")
      (message "Inserted `%s'" (string-trim (buffer-substring-no-properties beg (point)))))))

;; TODO needs a unit test
(defun haskell-tng--util-cached
    (work sym key &optional no-work reset)
  "A two-tier (variable and disk-based) cache over WORK.

The caller is responsible for flushing the cache. For
consistency, it is recommended that commands using this cache
flush the cache when the universal argument is provided."
  (haskell-tng--util-cached-variable
   (lambda ()
     (haskell-tng--util-cached-disk
      work
      key
      no-work
      reset))
   sym
   nil
   reset
   'no-nil))

(defun haskell-tng--util-cached-variable (work sym &optional no-work reset no-nil)
  "A variable cache over a function WORK.

If the SYM reference contains a cache of a previous call, it is
returned immediately.

Otherwise WORK is called with no parameters and saved to SYM.

Errors are NOT cached.

nil return values are cached unless NO-NIL is non-nil.

NO-WORK skips WORK and only queries the cache.

RESET sets the variable to nil before doing anything."
  (when reset
    (set sym nil))
  (when (not (symbol-value sym))
    (unless no-work
      (set sym (funcall work))
      (unless (or (symbol-value sym) no-nil)
        (set sym 'cached-nil))))
  (pcase (symbol-value sym)
    ('cached-nil nil)
    (cached cached)))

;; TODO max-age (fallback to disk if WORK fails)
(defun haskell-tng--util-cached-disk (work key &optional no-work reset)
  "A disk-based cache over a function WORK.

If the cache contains a file matching the KEY string (which must
be filesystem safe), it is parsed as an s-expressed and returned.

Otherwise WORK is called with no parameters and saved to the KEY.

Errors are NOT cached.

nil return values are NOT cached.

NO-WORK skips WORK and only queries the cache.

RESET deletes the cache if it exists."
  (let ((cache-file
         (concat (xdg-cache-home) "/haskell-tng/" key ".gz")))
    (when (and reset (file-exists-p cache-file))
      (delete-file cache-file))
    (if (file-exists-p cache-file)
        (haskell-tng--util-read cache-file)
      (unless no-work
        (when-let (result (funcall work))
          (haskell-tng--util-write cache-file result)
          result)))))

(defun haskell-tng--util-read (file)
  (let (jka-compr-verbose)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (ignore-errors (read (current-buffer))))))

(defun haskell-tng--util-write (file var)
  (let (jka-compr-verbose)
    (with-temp-file file
      (make-directory (file-name-directory file) 'create-parents)
      (prin1 var (current-buffer)))))

(provide 'haskell-tng-util)
;;; haskell-tng-util.el ends here
