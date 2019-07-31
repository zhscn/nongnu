;;; haskell-tng-imenu.el --- imenu support -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Creates a summary of the current file for navigation purposes.
;;
;;; Code:

(require 'imenu)

(require 'haskell-tng-rx)

(defun haskell-tng--imenu-create ()
  "Creates a `imenu--index-alist' for the current buffer."
  ;; Simple elements in the alist look like (INDEX-NAME . POSITION).
  ;; A nested sub-alist element looks like (INDEX-NAME . SUB-ALIST).

  (let ((entries `(,imenu--rescan-item)))
    (save-excursion
      (goto-char (point-min))

      (re-search-forward (rx bol "module" word-end))
      (push `("module" . ,(match-beginning 0)) entries)

      (when (re-search-forward (rx bol "import" word-end) nil t)
        (push `("imports" . ,(match-beginning 0)) entries))
      ;; ignore all other imports
      (while (re-search-forward (rx bol "import" word-end) nil t))

      (while
          ;; TODO ignore comments
          ;; TODO type / data / class / instance
          ;; TODO nested defns (use lexer not rx)
          ;; TODO inline symid defns `a <*> b ='
          ;; TODO consym defns `a :<|> b :<|> c ='
          (re-search-forward
           (rx-to-string
            `(: bol
                (group
                 (| ,haskell-tng--rx-varid
                    (: "(" ,haskell-tng--rx-symid ")")))))
           nil t)
        (let ((name (match-string 0))
              (pos (match-beginning 0)))
          (when (not (string-match haskell-tng--rx-c-reserved name))
            (push `(,name . ,pos) entries)))))

    (seq-uniq
     (reverse entries)
     (lambda (a1 a2) (equal (car a1) (car a2)))))
  )

(provide 'haskell-tng-imenu)
;;; haskell-tng-imenu.el ends here
