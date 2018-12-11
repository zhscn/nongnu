;;; haskell-tng-smie.el --- SMIE Rules for Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2018 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  SMIE lexer, precedence table (providing s-expression navigation), and
;;  indentation rules.
;;
;;  https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#SMIE
;;
;;; Code:

(require 'smie)

  ;; (defvar sample-keywords-regexp
  ;;      (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" ":=" "=")))
  ;;    (defun sample-smie-forward-token ()
  ;;      (forward-comment (point-max))
  ;;      (cond
  ;;       ((looking-at sample-keywords-regexp)
  ;;        (goto-char (match-end 0))
  ;;        (match-string-no-properties 0))
  ;;       (t (buffer-substring-no-properties
  ;;           (point)
  ;;           (progn (skip-syntax-forward "w_")
  ;;                  (point))))))
  ;;    (defun sample-smie-backward-token ()
  ;;      (forward-comment (- (point)))
  ;;      (cond
  ;;       ((looking-back sample-keywords-regexp (- (point) 2) t)
  ;;        (goto-char (match-beginning 0))
  ;;        (match-string-no-properties 0))
  ;;       (t (buffer-substring-no-properties
  ;;           (point)
  ;;           (progn (skip-syntax-backward "w_")
  ;;                  (point))))))

(provide 'haskell-tng-smie)
;;; haskell-tng-smie.el ends here
