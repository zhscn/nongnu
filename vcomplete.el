;;; vcomplete.el --- Visual completions -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Daniel Semyonov

;; Author: Daniel Semyonov <cmstr@dsemy.com>
;; Maintainer: Daniel Semyonov <cmstr@dsemy.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, matching
;; URL: https://dsemy.com/software/vcomplete

;; This file is not part of GNU Emacs.

;; vcomplete is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; vcomplete is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Vcomplete provides a minor mode enhancing the default completion
;; list buffer, providing visual aids for selecting completions and
;; performing other actions.
;;
;; Usage:
;;
;; Enable ‘vcomplete-mode’:
;;
;; (vcomplete-mode)
;;
;; For use-package users:
;;
;; (use-package vcomplete
;;   :config
;;   (vcomplete-mode))
;;
;; When vcomplete-mode is active:
;; - The completion list buffer opens and updates automatically (see
;;   ‘vcomplete-auto-update’).
;; - The completion list buffer can be controlled through the
;;   minibuffer (during minibuffer completion) or the current buffer
;;   (during in-buffer completion), if it's visible.
;; - The current completion is highlighted in the completion list
;;   buffer.
;;
;; C-n moves point to the next completion.
;;
;; C-p moves point to the previous completion.
;;
;; M-RET (C-M-m) chooses the completion at point.
;;
;; C-x k kills the buffer associated with the completion at point.
;;
;; More commands can be added through ‘vcomplete-command-map’:
;;
;; (define-key vcomplete-command-map [?\C-a] #'my-command)
;;
;; You can customize ‘vcomplete-mode’ using the customize interface:
;;
;; M-x customize-group RET vcomplete RET
;;
;; For more information see the (Vcomplete) info node.

;;; Code:

;;;; Customization:

(defgroup vcomplete nil
  "Visual completions."
  :link '(custom-manual "(Vcomplete)Top")
  :group 'convenience
  :prefix "vcomplete-")

(defcustom vcomplete-auto-update t
  "Whether the ‘*Completions*’ buffer should open and update automatically.
Non-nil means automatically open and update.
Otherwise, operate according to ‘completion-auto-help’."
  :type '(radio
          (const :tag "Automatically open and update" t)
          (const :tag "Operate according to ‘completion-auto-help’" nil))
  :package-version '(vcomplete . 0.1))

;;;; Completion commands:

(defmacro vcomplete-with-completions-buffer (&rest body)
  "Evaluate BODY with the ‘*Completions*’ buffer temporarily current.
While evaluating body, BUFFER and WINDOW are locally bound to the
‘*Completions*’ buffer and window respectively."
  (declare (indent 0))
  `(when-let ((buffer (get-buffer "*Completions*"))
              (window (get-buffer-window buffer)))
     (save-current-buffer
       (set-buffer buffer)
       (unless (eq major-mode 'completion-list-mode)
         (user-error
          "The ‘*Completions*’ buffer is set to an incorrect mode"))
       ,@body)))

(defvar vcomplete-current-completion nil
  "Currently selected completion.
Note that this variable is never cleared, so it can be used to get the
last selected completion after completion has ended.")

(defvar vcomplete--last-completion-overlay nil
  "Last overlay created in the ‘*Completions*’ buffer.")

(defun vcomplete--move-n-completions (n)
  "Move N completions in the ‘*Completions*’ buffer.
The completion selected is marked with an overlay."
  (vcomplete-with-completions-buffer
    (next-completion n)
    (when (= (point) (point-max)) (next-completion -1))
    (let ((beg (set-window-point window (point))) end)
      (unless (or (= beg (point-min))
                  (= beg (point-max)))
        (save-excursion
          (pcase (char-before (re-search-forward " \t\\|\n\\|\\'"))
            (?\t (setq end (- (point) 2)))
            (?\n (setq end (- (point) 1)))
            (_ (setq end (point)))))
        (setq vcomplete-current-completion
              (buffer-substring-no-properties beg end))
        (when vcomplete--last-completion-overlay
          (delete-overlay vcomplete--last-completion-overlay))
        (overlay-put
         (setq vcomplete--last-completion-overlay
               (make-overlay beg end))
         'face 'highlight)))))

(defun vcomplete-next-completion (&optional n)
  "Move to the next item in the ‘*Completions*’ buffer.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (vcomplete--move-n-completions (or n 1))
  (setq this-command 'vcomplete--no-update))

(defun vcomplete-prev-completion (&optional n)
  "Move to the previous item in the ‘*Completions*’ buffer.
With prefix argument N, move N items (negative N means move forward)."
  (interactive "p")
  (vcomplete--move-n-completions (- (or n 1)))
  (setq this-command 'vcomplete--no-update))

(defun vcomplete-choose-completion ()
  "Choose the completion at point in the ‘*Completions*’ buffer."
  (interactive)
  (when-let ((buf (get-buffer "*Completions*")))
    (switch-to-completions)
    (choose-completion)))

(defun vcomplete-kill-buffer ()
  "Kill the buffer associated with the current completion (if it exists)."
  (interactive)
  (if-let ((buf (get-buffer vcomplete-current-completion)))
      (when (kill-buffer-ask buf)
        (minibuffer-completion-help))
    (user-error "‘%s’ is not a valid buffer"
                vcomplete-current-completion)))

(defvar vcomplete-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-n] #'vcomplete-next-completion)
    (define-key map [?\C-p] #'vcomplete-prev-completion)
    (define-key map [?\C-\M-m] #'vcomplete-choose-completion)
    (define-key map [?\C-x ?k] #'vcomplete-kill-buffer)
    map)
  "Key map for ‘vcomplete-mode’ commands.")

(defun vcomplete--update()
  "Update the ‘*Completions*’ buffer when completing in the minibuffer."
  (while-no-input
    (redisplay)
    (unless (eq this-command 'vcomplete--no-update)
      (minibuffer-completion-help))))

(defun vcomplete--setup ()
  "Setup ‘vcomplete-mode’."
  (if (minibufferp)
      (progn
        (when (and vcomplete-auto-update minibuffer-completion-table)
          (add-hook 'post-command-hook
                    #'vcomplete--update nil t))
        (use-local-map (make-composed-keymap vcomplete-command-map
                                             (current-local-map))))
    (if completion-in-region-mode
        (when-let ((map (assq #'completion-in-region-mode
                              minor-mode-overriding-map-alist)))
          (setcdr map vcomplete-command-map))
      (remove-hook 'post-command-hook #'vcomplete--update t))))

;;;; Visual completion mode:

;;;###autoload
(define-minor-mode vcomplete-mode
  "Minor mode enhancing the default completion list buffer, providing
visual aids for selecting completions and performing other actions.

The following bindings are active during in-buffer and minibuffer
completion:

\\{vcomplete-command-map}"
  :global t
  (if vcomplete-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'vcomplete--setup)
        (add-hook 'completion-in-region-mode-hook #'vcomplete--setup))
    (remove-hook 'minibuffer-setup-hook #'vcomplete--setup)
    (remove-hook 'completion-in-region-mode-hook #'vcomplete--setup)))

(provide 'vcomplete)

;;; vcomplete.el ends here
