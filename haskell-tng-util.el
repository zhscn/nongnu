;;; haskell-tng-util.el --- Helpful Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;; Useful common utilities.
;;
;;; Code:

(require 'subr-x)

(defun haskell-tng:paren-close (&optional pos)
  "The next `)', if it closes `POS's paren depth."
  (save-excursion
    (goto-char (or pos (point)))
    (when-let (close (ignore-errors (scan-lists (point) 1 1)))
      (goto-char (- close 1))
      (when (looking-at ")")
        (point)))))

;; TODO comment / paren aware, like haskell-tng:layout-of-next-token
;; TODO refactor to share code with haskell-tng:layout-of-next-token
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

(defun haskell-tng:do-bind (&optional pos)
  ;; trivial, should just be called as an inline regexp
  "The next `<-'"
  (save-excursion
    (goto-char (or pos (point)))
    (re-search-forward "<-" nil t)))

(defun haskell-tng:next-where (&optional pos)
  ;; trivial, should just be called as an inline regexp
  "The next `where'"
  (save-excursion
    (goto-char (or pos (point)))
    (re-search-forward (rx word-start "where" word-end) nil t)))

(defun haskell-tng:indent-close-previous ()
  "Indentation closing the previous symbol."
  (save-excursion
    (forward-symbol -1)
    (haskell-tng:indent-close)))

(defun haskell-tng:locate-dominating-file (regexp)
  "`locate-dominating-file' but starting from `default-directory'
and taking a regexp."
  (locate-dominating-file
   default-directory
   (lambda (dir) (directory-files dir nil regexp))))

(provide 'haskell-tng-util)
;;; haskell-tng-util.el ends here
