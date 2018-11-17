;;; haskell-tng-util.el --- Helpful Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
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

(provide 'haskell-tng-util)
;;; haskell-tng-util.el ends here
