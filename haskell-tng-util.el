;;; haskell-tng-util.el --- Helpful Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;; Useful common utilities.
;;
;;; Code:

;; TODO move things to single use sites (twas premature abstraction!)

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
      (catch 'closed
        (while (and (forward-line) (not (eobp)))
          (when (<= (current-indentation) level)
            (throw 'closed (point))))
        nil))))

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

(defun haskell-tng--import-symbol (module as &optional sym)
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
     "\n")))

;; TODO split into two calls: disk and local
;; TODO needs a unit test
;; TODO a macro that expands out the local variable
(defun haskell-tng--hsinspect-cached
    (fn args local disk &optional no-work flush-cache)
  "A two-tier cache over a FN that takes ARGS.
The caller is responsible for flushing the cache. For
consistency, it is recommended that commands using this cache
flush the cache when the universal argument is provided.

If the LOCAL reference contains a cache of a previous call, it is
returned immediately.

If DISK expands to a file that exists in the cache directory, it
is read as an s-expression, saved to LOCAL, and returned. Callers
are advised to version their DISK cache as it is persisted
between restarts and software upgrades.

Otherwise FN is called with ARGS and saved to both LOCAL and
DISK.

Errors are not cached, nil return values are cached in LOCAL but
not in DISK.

NO-WORK skips FN and only queries the caches.

FLUSH-CACHE forces both LOCAL and DISK to be invalidated."
  (when flush-cache
    (set local nil))
  (when (not (symbol-value local))
    (let ((cache-file-name
           (concat (xdg-cache-home) "/haskell-tng/" disk ".gz")))
      (when (and flush-cache (file-exists-p cache-file-name))
        (delete-file cache-file-name))
      (if (file-exists-p cache-file-name)
          (set
           local
           (with-temp-buffer
             ;; TODO set jka-compr-verbose to nil to disable messages (currently
             ;;      giving useful debugging hints so left on).
             (insert-file-contents cache-file-name)
             (goto-char (point-min))
             (ignore-errors (read (current-buffer)))))
        (unless (or no-work
                    (eq 'cached-nil (symbol-value local)))
          (set local 'cached-nil)
          (set local (apply fn args))
          (if-let (cache (symbol-value local))
              (with-temp-file cache-file-name
                (make-directory (file-name-directory cache-file-name) 'create-parents)
                (prin1 cache (current-buffer)))
            (set local 'cached-nil))))))

  (when (not (eq 'cached-nil (symbol-value local)))
    (symbol-value local)))

(provide 'haskell-tng-util)
;;; haskell-tng-util.el ends here
