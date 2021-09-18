;;; idle-highlight-mode.el --- Highlight the word the point is on -*- lexical-binding: t -*-

;; Copyright (C) 2008-2011 Phil Hagelberg, Cornelius Mika

;; Author: Phil Hagelberg, Cornelius Mika
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Version: 1.1.3
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Based on some snippets by fledermaus from the Emacs channel.

;; M-x idle-highlight-mode sets an idle timer that highlights all
;; occurrences in the buffer of the word under the point.

;; Enabling it in a hook is recommended. But you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-coding-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (idle-highlight-mode t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

(require 'thingatpt)


(defgroup idle-highlight nil "Highlight other occurrences of the word at point." :group 'faces)

(defface idle-highlight
  '((t (:inherit region)))
  "Face used to highlight other occurrences of the word at point."
  :group 'idle-highlight)

(defcustom idle-highlight-exceptions '("end")
  "List of words to be excepted from highlighting."
  :group 'idle-highlight
  :type '(repeat string))

(defcustom idle-highlight-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'idle-highlight
  :type 'float)

(defvar-local idle-highlight--regexp nil "Buffer-local regexp to be idle-highlighted.")

(defvar idle-highlight--global-timer nil "Timer to trigger highlighting.")

(defsubst idle-highlight--ignore-context ()
  "Return non-nil when in a context that should be ignored."
  ;; In a string.
  (nth 3 (syntax-ppss)))

(defun idle-highlight--word-at-point ()
  "Highlight the word under the point."
  (when idle-highlight-mode
    (idle-highlight--unhighlight)
    (let ((target-symbol (symbol-at-point)))
      (when
        (and
          target-symbol (not (idle-highlight--ignore-context))
          ;; Symbol characters.
          (looking-at-p "\\s_\\|\\sw"))
        (let ((target (symbol-name target-symbol)))
          (when (not (member target idle-highlight-exceptions))
            (setq idle-highlight--regexp (concat "\\<" (regexp-quote target) "\\>"))
            (highlight-regexp idle-highlight--regexp 'idle-highlight)))))))

(defsubst idle-highlight--unhighlight ()
  "Clear current highlight."
  (when idle-highlight--regexp
    (unhighlight-regexp idle-highlight--regexp)
    (setq idle-highlight--regexp nil)))

;;;###autoload
(define-minor-mode idle-highlight-mode
  "Idle-Highlight Minor Mode."
  :group 'idle-highlight
  (if idle-highlight-mode
    (progn
      (unless idle-highlight--global-timer
        (setq idle-highlight--global-timer
          (run-with-idle-timer idle-highlight-idle-time :repeat 'idle-highlight--word-at-point))))
    (idle-highlight--unhighlight)))

(provide 'idle-highlight-mode)
;;; idle-highlight-mode.el ends here
