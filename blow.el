;;; blow.el --- Blow away mode lighters  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-06-08
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience
;; URL: https://codeberg.org/akib/emacs-blow

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Mode lighters are indicator that make us aware that a mode is enable.
;; This helpful, but when there are too many of them, it becomes a problem,
;; because often lighters with the important information gets truncate due
;; to the non-important lighters.  So we blow them, and they are usually
;; the lighters of the important, perhaps indispensible, modes for you, so
;; that you can see and focus on what matters.
;;
;; Using this package is simple:
;;
;; ┌────
;; │ ;; Enable `blow-mode'.
;; │ (blow-mode +1)
;; │
;; │ ;; Blow the lighter of `gcmh-mode'.
;; │ (blow 'gcmh-mode)
;; │
;; │ ;; Change the lighter of `eldoc-mode' to `ELispDoc' (with a preceding
;; │ ;; space to differentiate from other lighters.
;; │ (blow 'eldoc-mode " ElispDoc")
;; │
;; │ ;; Show the lighter of `paredit-mode' unless the major mode is
;; │ ;; `emacs-lisp-mode'.
;; │ (blow 'paredit-mode '(:eval (unless (eq major-mode 'emacs-lisp-mode)
;; │                               (blow-original-lighter 'paredit-mode))))
;; │
;; │ ;; Revert `eldoc-mode' lighter.
;; │ (blow-revert 'eldoc-mode)
;; └────
;;
;; Or, if you prefer customizing variable, set `blow-mode-list' from
;; custom, or customize it from your init file like the following:
;;
;; ┌────
;; │ ;; You can also use `setq', but you would need to reenable `blow-mode'
;; │ ;; for changes to effect.
;; │ (customize-set-variable
;; │  blow-mode-list
;; │
;; │  ;; Blow the lighter of `gcmh-mode'.
;; │  '((gcmh-mode nil)
;; │
;; │    ;; Change the lighter of `eldoc-mode' to `ELispDoc' (with a
;; │    ;; preceding space to differentiate from other lighters.
;; │    (eldoc-mode " ElispDoc")
;; │
;; │    ;; Show the lighter of `paredit-mode' unless the major mode is
;; │    ;; `emacs-lisp-mode'.
;; │    (paredit-mode (:eval (unless (eq major-mode 'emacs-lisp-mode)
;; │                           (blow-original-lighter 'paredit-mode))))))
;; │
;; │ ;; Enable mode to take effect.
;; │ (blow-mode +1)
;; └────

;;; Code:

(defgroup blow nil
  "Blow away mode lighters."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-blow")
  :prefix "blow-")

(defvar blow-mode)

(defun blow--set-mode-list (symbol value)
  "Set SYMBOL's default value to VALUE, SYMBOL should be `blow-mode-list'."
  (set-default symbol value)
  (when blow-mode
    (blow--setup-all-buffers)))

(defcustom blow-mode-list nil
  "Mode lighters to blow.

Each element is a list cell of form (MODE REPLACEMENT), where mode is the
mode name and REPLACEMENT is a mode line template (see `mode-line-format')
to use instead of the mode's own lighter.

If a mode appears more than once, the first one takes effect.

Don't modify this variable from Lisp programs, use `blow' instead."
  :type '(repeat (list (symbol :tag "Mode")
                       (sexp :tag "Mode line template")))
  :set #'blow--set-mode-list)

(defvar blow--original-lighters nil
  "Hash table of modes and their original lighters or nil.")

(defun blow--hash-exists-p (key table)
  "Return t if KEY is in hash table TABLE."
  (let ((default (make-symbol "blow--nonexistant")))
    (not (eq (gethash key table default) default))))

(defun blow--puthash-unless-exists (key value table)
  "Put KEY and VALUE in TABLE unless KEY already exists."
  (unless (blow--hash-exists-p key table)
    (puthash key value table)))

(defun blow--replace-lighters-on-all-buffer (replacement-lighters)
  "Replace lighters on all buffers with REPLACEMENT-LIGHTERS."
  (let ((major-mode-replacement-lighters
         (make-hash-table :test 'eq
                          :size (length blow-mode-list))))
    (maphash (lambda (mode replacement)
               (let ((pair (assq mode minor-mode-alist)))
                 (if (not pair)
                     (puthash mode replacement
                              major-mode-replacement-lighters)
                   (blow--puthash-unless-exists
                    mode (cadr pair) blow--original-lighters)
                   (setf (cadr pair) replacement))))
             replacement-lighters)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let* ((default (make-symbol "blow--nonexistant"))
               (replacement
                (gethash major-mode major-mode-replacement-lighters
                         default)))
          (unless (eq replacement default)
            (blow--puthash-unless-exists
             major-mode mode-name blow--original-lighters)
            (setq mode-name replacement)))))))

(defun blow--setup-all-buffers ()
  "Blow mode lighters on all buffers."
  (unless blow--original-lighters
    (setq blow--original-lighters (make-hash-table)))
  (let ((changed-lighters
         (make-hash-table :test 'eq
                          :size (length blow-mode-list))))
    (dolist (entry blow-mode-list)
      (unless (and (blow--hash-exists-p
                    (car entry) blow--original-lighters)
                   (eq (gethash (car entry) blow--original-lighters)
                       (cadr entry)))
        (blow--puthash-unless-exists (car entry) (cadr entry)
                                     changed-lighters)))
    (maphash (lambda (key value)
               (blow--puthash-unless-exists key value changed-lighters))
             blow--original-lighters)
    (blow--replace-lighters-on-all-buffer changed-lighters)))

;;;###autoload
(defun blow (mode &optional replacement)
  "Blow mode lighter of MODE and use REPLACEMENT as it's lighter."
  (let ((entry (assq mode blow-mode-list)))
    (when entry
      (setq blow-mode-list (delq entry blow-mode-list)))
    (push (list mode replacement) blow-mode-list))
  (when blow-mode
    (blow--setup-all-buffers)))

(defun blow-revert (mode)
  "Revert the blown mode lighter of MODE."
  (let ((entry (assq mode blow-mode-list)))
    (when entry
      (setq blow-mode-list (delq entry blow-mode-list))))
  (when blow-mode
    (blow--setup-all-buffers)))

;;;###autoload
(define-minor-mode blow-mode
  "Minor mode to blow away mode lighters."
  :global t
  :lighter " Blow"
  (if blow-mode
      (blow--setup-all-buffers)
    (let ((blow-mode-list nil))
      (blow--setup-all-buffers)
      (setq blow--original-lighters nil))))

(defun blow-original-lighter (mode)
  "Return the original lighter of mode MODE."
  (if (and blow-mode (blow--hash-exists-p mode blow--original-lighters))
      (gethash mode blow--original-lighters)
    (cadr (assq mode minor-mode-alist))))

(provide 'blow)
;;; blow.el ends here
