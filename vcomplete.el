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
;; list buffer, providing visual aids for selecting completions.
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
;; - The currently selected completion is highlighted in the
;;   completion list buffer.
;;
;; C-n moves point to the next completion.
;;
;; C-p moves point to the previous completion.
;;
;; M-RET (C-M-m) chooses the completion at point.
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
  :package-version '(vcomplete . 0.1)
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'vcomplete--reset-vars)
           (vcomplete--reset-vars))))

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

(defun vcomplete-current-completion (pos)
  "Get the completion candidate at POS.
The completion candidate is returned as a list of the form:
 (COMPLETION-STRING . (BEGINNING . END))
If no completion is found, return nil."
  (unless (derived-mode-p 'completion-list-mode)
    (error "Not in a valid completion list buffer"))
  ;; Modified from code in ‘choose-completion’.
  (let (beg end noop)
    (cond
     ((and (not (eobp)) (get-text-property pos 'mouse-face))
      (setq end pos beg (1+ pos)))
     ((and (not (bobp))
           (get-text-property (1- pos) 'mouse-face))
      (setq end (1- pos) beg pos))
     (t (setq noop t)))
    (unless noop
      (setq beg (previous-single-property-change beg 'mouse-face))
      (setq end (or (next-single-property-change end 'mouse-face)
                    (point-max)))
      `(,(buffer-substring-no-properties beg end) . (,beg . ,end)))))

(defvar vcomplete--last-completion-overlay nil
  "Last overlay created in the ‘*Completions*’ buffer.")

(defun vcomplete--highlight-completion-at-point ()
  "Highlight the completion at point in the ‘*Completions*’ buffer."
  (while-no-input
    (redisplay)
    (let ((cur (vcomplete-current-completion (point))))
      (when vcomplete--last-completion-overlay
        (delete-overlay vcomplete--last-completion-overlay))
      (when-let ((pos (cdr cur)))
        (overlay-put
         (setq vcomplete--last-completion-overlay
               (make-overlay (car pos) (cdr pos)))
         'face 'highlight)))))

(defun vcomplete--move-n-completions (n)
  "Move N completions in the ‘*Completions*’ buffer."
  (vcomplete-with-completions-buffer
    (next-completion n)
    (set-window-point window (point))
    (vcomplete--highlight-completion-at-point)))

(defun vcomplete-next-completion (&optional n)
  "Move to the next item in the ‘*Completions*’ buffer.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (vcomplete--move-n-completions (or n 1)))

(defun vcomplete-prev-completion (&optional n)
  "Move to the previous item in the ‘*Completions*’ buffer.
With prefix argument N, move N items (negative N means move forward)."
  (interactive "p")
  (vcomplete--move-n-completions (- (or n 1))))

(defun vcomplete-choose-completion ()
  "Choose the completion at point in the ‘*Completions*’ buffer."
  (interactive)
  (when-let ((buf (get-buffer "*Completions*")))
    (switch-to-completions)
    (choose-completion)))

(defvar vcomplete-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-n] #'vcomplete-next-completion)
    (define-key map [?\C-p] #'vcomplete-prev-completion)
    (define-key map [?\C-\M-m] #'vcomplete-choose-completion)
    map)
  "Key map for ‘vcomplete-mode’ commands.")

;;;; Visual completion mode:

(defvar vcomplete--last-string nil
  "Last pending completion string.")

(defun vcomplete--set-last-string-in-minibuffer ()
  "Set ‘vcomplete--last-string’ in a minibuffer."
  (setq vcomplete--last-string (minibuffer-contents)))

(defun vcomplete--string-in-region ()
  "Return a substring according to the markers in ‘completion-in-region--data’."
  (when completion-in-region--data
    (buffer-substring (car completion-in-region--data)
                      (cadr completion-in-region--data))))

(defun vcomplete--set-last-string-in-region ()
  "Set ‘vcomplete--last-string’ in-region."
  (setq vcomplete--last-string
        (vcomplete--string-in-region)))

(defun vcomplete--update-in-minibuffer ()
  "Update the completion list when completing in a minibuffer."
  (while-no-input
    (redisplay)
    (unless (string= (minibuffer-contents) vcomplete--last-string)
      (minibuffer-completion-help))))

(defun vcomplete--update-in-region ()
  "Update the completion list when completing in-region."
  (while-no-input
    (redisplay)
    (if (get-buffer-window "*Completions*")
        (unless (string= (vcomplete--string-in-region)
                         vcomplete--last-string)
          (completion-help-at-point))
      (completion-in-region-mode -1))))

(defun vcomplete--reset-vars ()
  "Reset variables used by Vcomplete to their default values."
  (setq vcomplete--last-completion-overlay nil
        vcomplete--last-string nil)
  (remove-hook 'pre-command-hook #'vcomplete--set-last-string-in-minibuffer t)
  (remove-hook 'pre-command-hook #'vcomplete--set-last-string-in-region t)
  (remove-hook 'post-command-hook #'vcomplete--update-in-region t)
  (remove-hook 'post-command-hook #'vcomplete--update-in-minibuffer t)
  (remove-hook 'post-command-hook #'vcomplete--highlight-completion-at-point t))

(defun vcomplete--setup-completions ()
  "Setup ‘vcomplete-mode’ for the ‘*Completions*’ buffer."
  (add-hook 'post-command-hook
            #'vcomplete--highlight-completion-at-point nil t))

(defun vcomplete--setup-current ()
  "Setup ‘vcomplete-mode’ for the current buffer."
  (vcomplete--reset-vars)
  (if (minibufferp)
      (progn
        (when (and vcomplete-auto-update minibuffer-completion-table)
          (add-hook 'pre-command-hook
                    #'vcomplete--set-last-string-in-minibuffer nil t)
          (add-hook 'post-command-hook
                    #'vcomplete--update-in-minibuffer nil t))
        (use-local-map (make-composed-keymap vcomplete-command-map
                                             (current-local-map))))
    (when-let ((map (assq #'completion-in-region-mode
                          minor-mode-overriding-map-alist)))
      (when vcomplete-auto-update
        (add-hook 'pre-command-hook
                  #'vcomplete--set-last-string-in-region nil t)
        (add-hook 'post-command-hook
                  #'vcomplete--update-in-region nil t))
      (setcdr map vcomplete-command-map))))

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
        (vcomplete--reset-vars)
        (add-hook 'completion-list-mode-hook #'vcomplete--setup-completions)
        (add-hook 'minibuffer-setup-hook #'vcomplete--setup-current)
        (add-hook 'minibuffer-exit-hook #'vcomplete--reset-vars)
        (add-hook 'completion-in-region-mode-hook #'vcomplete--setup-current))
    (vcomplete--reset-vars)
    (remove-hook 'completion-list-mode-hook #'vcomplete--setup-completions)
    (remove-hook 'minibuffer-setup-hook #'vcomplete--setup-current)
    (remove-hook 'minibuffer-exit-hook #'vcomplete--reset-vars)
    (remove-hook 'completion-in-region-mode-hook #'vcomplete--setup-current)))

(provide 'vcomplete)

;;; vcomplete.el ends here
