;;; vcomplete.el --- Visual completions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Daniel Semyonov

;; Author: Daniel Semyonov <daniel@dsemy.com>
;; Maintainer: Vcomplete Mailing List <~dsemy/vcomplete-devel@lists.sr.ht>
;; Version: 1.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, matching
;; URL: https://dsemy.com/projects/vcomplete

;; This file is not part of GNU Emacs.

;; Vcomplete is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Vcomplete is distributed in the hope that it will be useful, but
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
;; Enable `vcomplete-mode':
;;
;; (vcomplete-mode)
;;
;; For use-package users:
;;
;; (use-package vcomplete
;;   :config
;;   (vcomplete-mode))
;;
;; When `vcomplete-mode' is active:
;; - The completion list buffer opens and updates automatically (see
;;   `vcomplete-auto-update').
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
;; More commands can be added through `vcomplete-command-map':
;;
;; (define-key vcomplete-command-map [?\C-a] #'my-command)
;;
;; You can customize `vcomplete-mode' using the customize interface:
;;
;; M-x customize-group RET vcomplete RET
;;
;; For more information see the (Vcomplete) info node.

;;; Code:

(eval-when-compile (require 'subr-x))

;;;; Customization:

(defgroup vcomplete nil
  "Visual completions."
  :link '(custom-manual "(Vcomplete)Top")
  :group 'convenience
  :prefix "vcomplete-")

(defcustom vcomplete-auto-update t
  "Whether the `*Completions*' buffer should open and update automatically.
Non-nil means automatically open and update.
Otherwise, operate according to `completion-auto-help'."
  :link '(info-link "(Vcomplete)Customization")
  :type '(radio
          (const :tag "Automatically open and update" t)
          (const :tag "Operate according to `completion-auto-help'" nil))
  :risky t
  :package-version '(vcomplete . 0.1))

(defcustom vcomplete-no-update-commands
  '(vcomplete-next-completion
    vcomplete-prev-completion
    vcomplete-choose-completion
    minibuffer-complete-and-exit
    minibuffer-force-complete-and-exit
    completion-at-point
    choose-completion)
  "List of commands which shouldn't cause the `*Completions*' buffer to update."
  :link '(info-link "(Vcomplete)Completion commands")
  :type '(hook :tag "Commands")
  :risky t
  :package-version '(vcomplete . 1.1))

(defcustom vcomplete-search-range 'visible
  "Range of search for a `*Completions*' window during completion.
- t means consider all windows on all frames.
- `visible' means consider all windows on all visible frames.
- 0 (the number zero) means consider all windows on all visible and
  iconified frames.
- Any other value means consider all windows on the selected frame and
  no others."
  :link '(info-link "(Vcomplete)Customization")
  :type '(radio (const :tag "All windows on all frames" t)
                (const :tag "All windows on all visible frames" visible)
                (const
                 :tag "All windows on all visible and iconified frames" 0)
                (const
                 :tag "All windows on the currently selected frame"
                 nil))
  :risky t
  :package-version '(vcomplete . 1.2))

(defface vcomplete-highlight '((t :inherit highlight))
  "Face for highlighting completions."
  :package-version '(vcomplete . 1.1))

;;;; Completion commands:

(defun vcomplete--get-completions-window ()
  "Return the window associated with the `*Completions*' buffer).
This function only searches the frames specified in `vcomplete-search-range'."
  (get-buffer-window "*Completions*" vcomplete-search-range))

(defmacro vcomplete-with-completions-buffer (&rest body)
  "Evaluate BODY with the `*Completions*' buffer temporarily current.
While evaluating BODY, BUFFER and WINDOW are locally bound to the
`*Completions*' buffer and window respectively."
  (declare (debug (&rest form)))
  `(when-let ((buffer (get-buffer "*Completions*"))
              (window (vcomplete--get-completions-window)))
     (save-current-buffer
       (set-buffer buffer)
       (unless (derived-mode-p 'completion-list-mode)
         (user-error
          "The `*Completions*' buffer is set to an incorrect mode"))
       ,@body)))

(defun vcomplete-current-completion (pos)
  "Get the completion candidate at POS.
The completion candidate is returned as a list of the form:
 (COMPLETION-STRING . (BEGINNING . END))
If no completion is found, return nil.
An error is thrown when the current buffer
isn't a completion list buffer."
  (unless (derived-mode-p 'completion-list-mode)
    (error "Not in a valid completion list buffer"))
  ;; Modified from code in `choose-completion'.
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
  "Last overlay created in the `*Completions*' buffer.")
(put 'vcomplete--last-completion-overlay 'risky-local-variable t)

(defun vcomplete--highlight-completion-at-point ()
  "Highlight the completion at point in the `*Completions*' buffer."
  (let ((cur (vcomplete-current-completion (point))))
    (when vcomplete--last-completion-overlay
      (delete-overlay vcomplete--last-completion-overlay))
    (when-let ((pos (cdr cur)))
      (overlay-put
       (setq vcomplete--last-completion-overlay
             (make-overlay (car pos) (cdr pos)))
       'face 'vcomplete-highlight))))

(defun vcomplete--move-n-completions (n)
  "Move N completions in the `*Completions*' buffer."
  (vcomplete-with-completions-buffer
   (next-completion n)
   (set-window-point window (point))
   (vcomplete--highlight-completion-at-point)))

(defun vcomplete-next-completion (&optional n)
  "Move to the next item in the `*Completions*' buffer.
With prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (vcomplete--move-n-completions (or n 1)))

(defun vcomplete-prev-completion (&optional n)
  "Move to the previous item in the `*Completions*' buffer.
With prefix argument N, move N items (negative N means move forward)."
  (interactive "p")
  (vcomplete--move-n-completions (- (or n 1))))

(defun vcomplete-choose-completion ()
  "Choose the completion at point in the `*Completions*' buffer."
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
  "Key map for completion commands.")

;;;; Vcomplete mode:

(defun vcomplete--update-minibuffer (&rest _args)
  "Update the completion list when completing in a minibuffer."
  (while-no-input
    (redisplay)
    (unless (memq this-command vcomplete-no-update-commands)
      (minibuffer-completion-help))))

(defun vcomplete--update-in-region (&rest _args)
  "Update the completion list when completing in-region."
  (while-no-input
    (redisplay)
    (unless (memq this-command vcomplete-no-update-commands)
      (completion-help-at-point))))

;; This function is required (to be in the local `post-command-hook')
;; since `after-change-functions' runs before the `*Completions*'
;; buffer is closed, so `completion-in-region-mode' can't be
;; immediately disabled through `vcomplete--update-in-region'.
(defun vcomplete--disable-completion-in-region ()
  "Stop completion in region when there is no visible `*Completions*' buffer."
  (unless (vcomplete--get-completions-window)
    (completion-in-region-mode -1)))

(defun vcomplete--setup-completions ()
  "Setup the `*Completions*' buffer for highlighting the completion at point."
  (add-hook 'post-command-hook
            #'vcomplete--highlight-completion-at-point nil t))

(defun vcomplete--reset-completions ()
  "Stop highlighting the completion at point in the `*Completions*' buffer."
  (when-let ((buf (get-buffer "*Completions*")))
    (with-current-buffer buf
      (remove-hook 'post-command-hook
                   #'vcomplete--highlight-completion-at-point t))))

(defun vcomplete--setup-minibuffer ()
  "Setup visual completions for the minibuffer."
  (when minibuffer-completion-table ; Ensure completion is in progress.
    (setq vcomplete--last-completion-overlay nil)
    (when vcomplete-auto-update
      (vcomplete--update-minibuffer)
      (add-hook 'after-change-functions
                #'vcomplete--update-minibuffer nil t))
    (use-local-map (make-composed-keymap vcomplete-command-map
                                         (current-local-map)))))

(defun vcomplete--setup-in-region ()
  "Setup visual completions for the current buffer."
  (remove-hook 'after-change-functions
               #'vcomplete--update-in-region t)
  (remove-hook 'post-command-hook
               #'vcomplete--disable-completion-in-region t)
  ;; This has the nice side effect of also checking whether
  ;; `completion-in-region-mode' is active.
  (when-let ((map (assq #'completion-in-region-mode
                        minor-mode-overriding-map-alist)))
    (setq vcomplete--last-completion-overlay nil)
    (when vcomplete-auto-update
      (add-hook 'after-change-functions
                #'vcomplete--update-in-region nil t)
      (add-hook 'post-command-hook
                #'vcomplete--disable-completion-in-region nil t))
    (setcdr map vcomplete-command-map)))

;;;###autoload
(define-minor-mode vcomplete-mode
  "Toggle Vcomplete mode.

When Vcomplete mode is enabled, the completion list buffer appears and
updates automatically (by default), and the completion at point in the
completions list buffer is highlighted.

The following bindings are available during completion:

\\{vcomplete-command-map}"
  :global t
  (if vcomplete-mode
      (progn
        (add-hook 'completion-list-mode-hook #'vcomplete--setup-completions)
        (add-hook 'minibuffer-setup-hook #'vcomplete--setup-minibuffer)
        (add-hook 'completion-in-region-mode-hook #'vcomplete--setup-in-region))
    (remove-hook 'completion-list-mode-hook #'vcomplete--setup-completions)
    (vcomplete--reset-completions)
    (remove-hook 'minibuffer-setup-hook #'vcomplete--setup-minibuffer)
    (remove-hook 'completion-in-region-mode-hook #'vcomplete--setup-in-region)))

(provide 'vcomplete)

;;; vcomplete.el ends here
