;;; swsw.el --- Simple window switching -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Semyonov

;; Author: Daniel Semyonov <cmstr@dsemy.com>
;; Maintainer: Daniel Semyonov <cmstr@dsemy.com>
;; Version: 2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://dsemy.com/software/swsw

;; This file is not part of GNU Emacs.

;; swsw is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; swsw is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; swsw (simple window switching) provides a minor mode for switching
;; to windows using IDs assigned to them automatically.
;;
;; Usage:
;;
;; Enable ‘swsw-mode’:
;;
;; (swsw-mode)
;;
;; For use-package users:
;;
;; (use-package swsw
;;   :config
;;   (swsw-mode))
;;
;; When swsw-mode is active:
;; - A window ID is displayed using a mode line lighter or a display
;;   function (see ‘swsw-display-function’).
;; - Window IDs are assigned to all windows on all frames except for
;;   the minibuffer(by default, see ‘swsw-scope’).
;;
;; C-x o ID switches focus to the window which corresponds to ID.
;;
;; C-x o m switches focus to the minibuffer if it's active.
;;
;; C-x o 0 ID deletes the window which corresponds to ID.
;;
;; More commands can be added through ‘swsw-command-map’:
;;
;; (define-key swsw-command-map [?a] #'my-command)
;;
;; You can customize ‘swsw-mode’ using the customize interface:
;;
;; M-x customize-group RET swsw RET
;;
;; For more information see the (swsw) info node.

;;; Code:

;; Avoid byte-compilation warnings.
(eval-when-compile
  (defvar swsw-display-function)
  (defvar swsw-command-map))

;;;; Customization:

(defgroup swsw nil
  "Simple window switching."
  :group 'convenience
  :prefix "swsw-")

(defun swsw--set-id-chars (sym chars)
  "Set the variable ‘swsw-id-chars’.
Check that the new list has at least two elements, set SYM’s value to
CHARS, and call ‘swsw-update’."
  (when (< (length chars) 2)
    (user-error "‘swsw-id-chars’ should contain at least two characters"))
  (set-default sym chars)
  (when (fboundp 'swsw-update)
    (swsw-update)))

(defcustom swsw-id-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Base set of characters from which window IDs are constructed.
This list should contain at least two characters."
  :type '(repeat character)
  :set #'swsw--set-id-chars)

(defun swsw--set-scope (sym scope)
  "Set the variable ‘swsw-scope’.
Set SYM’s value to SCOPE and call ‘swsw-update’."
  (set-default sym scope)
  (when (fboundp 'swsw-update)
    (swsw-update)))

(defcustom swsw-scope t
  "Scope of all window operations.
t means consider all windows on all existing frames.
0 (the number zero) means consider all windows on all visible and
  iconified frames.
‘visible’ means consider all windows on all visible frames.
‘current’ means consider only the currently selected frame."
  :type '(radio (const :tag "All windows on all frames" t)
                (const
                 :tag "All windows on all visible and iconified frames" 0)
                (const :tag "All windows on all visible frames" visible)
                (const
                 :tag "All window on the currently selected frame"
                 current))
  :set #'swsw--set-scope)

(defun swsw--set-display-function (sym fun)
  "Set the variable ‘swsw-display-function’.
Call the previous display function with nil as the sole argument
\(turning it off), set SYM's value to FUN, and call FUN with t as the
sole argument (turning it on)."
  (unless (or (not (boundp 'swsw-display-function))
              (eq swsw-display-function 'lighter))
    (funcall swsw-display-function nil))
  (set-default sym fun)
  (unless (eq fun 'lighter)
    (funcall fun t)))

(defcustom swsw-display-function 'lighter
  "Function used to display the ID of each window.
This function is called with t as the sole argument when enabling
‘swsw-mode’, and with nil as the sole argument when disabling it.
If set to ‘lighter’, use the mode line lighter of ‘swsw-mode’."
  :type '(radio (const :tag "Mode line lighter" lighter)
                (function :tag "Display function"))
  :set #'swsw--set-display-function)

(defcustom swsw-id-format " <%s>"
  "Format string for the window ID.
%s is replaced with a representation of the window's ID."
  :type '(string))

;;;; Window tracking:

(defvar swsw--id-counter nil
  "Counter which determines the next possible ID.")

(defvar swsw--id-map (make-sparse-keymap)
  "Key map for window ID selection.")

(defvar swsw-window-count 0
  "Amount of windows that have been assigned an ID.")

(defun swsw--get-scope ()
  "Return the current scope in which windows should be tracked."
  (if (eq swsw-scope 'current)
      (selected-frame)
    swsw-scope))

(defun swsw--get-id-length ()
  "Return the current length of a window ID."
  (let ((windows (length (window-list-1 nil nil (swsw--get-scope)))))
    ;; If there is only one window, return 1.
    (if (= windows 1) 1
      (ceiling (log windows (length swsw-id-chars))))))

(defun swsw--next-id ()
  "Get the next available ID."
  (let ((len (length swsw-id-chars)) (adv-flag t) id)
    (setq swsw--id-counter
          (mapcar (lambda (elt)
                    (push (nth elt swsw-id-chars) id)
                    ;; Advance ‘swsw--id-counter’.
                    (when adv-flag
                      (if (= len (setq elt (1+ elt)))
                          (setq elt 0)
                        (setq adv-flag nil)))
                    elt)
                  swsw--id-counter))
    id))

(defun swsw-update-window (window)
  "Update information for WINDOW."
  (let ((id (if (window-minibuffer-p window)
                (progn
                  (setq swsw-window-count (1+ swsw-window-count))
                  nil)
              (swsw--next-id))))
    (when id
      (define-key swsw--id-map (apply #'vector id)
        `(lambda ()
           (interactive)
           (funcall last-command ,window)))
      (set-window-parameter window 'swsw-id id)
      (setq swsw-window-count (1+ swsw-window-count)))))

(defun swsw-update (&optional _frame)
  "Update information for all windows."
  (setq swsw--id-map (make-sparse-keymap))
  (set-keymap-parent swsw--id-map swsw-command-map)
  (setq swsw--id-counter nil
        swsw-window-count 0)
  (let ((acc 0) (len (swsw--get-id-length)))
    (while (< acc len)
      (push 0 swsw--id-counter)
      (setq acc (1+ acc))))
  (walk-windows #'swsw-update-window nil (swsw--get-scope)))

;;;; Display functions:

(defun swsw-format-id (window)
  "Format an ID string for WINDOW."
  (format swsw-id-format
          (apply #'string (window-parameter window 'swsw-id))))

(defun swsw--mode-line-display ()
  "Display window IDs at the beginning of the mode line."
  (setq-default mode-line-format
                `((swsw-mode
                   (:eval (swsw-format-id (selected-window))))
                  ,@(assq-delete-all
                     `swsw-mode
                     (default-value `mode-line-format))))
  (force-mode-line-update t))

(defun swsw--mode-line-hide ()
  "Remove window IDs from the beginning of the mode line."
  (setq-default mode-line-format
                (assq-delete-all
                 'swsw-mode
                 (default-value 'mode-line-format)))
  (force-mode-line-update t))

(defun swsw-mode-line-display-function (switch)
  "Display window IDs at the beginning of the mode line.
Display window IDs if SWITCH isn't nil, and disable displaying window
IDs if SWITCH is nil.
This display function respects ‘swsw-id-format’."
  (if switch
      (swsw--mode-line-display)
    (swsw--mode-line-hide)))

(defun swsw-mode-line-conditional-display-function (switch)
  "Display window IDs at the beginning of the mode line, conditionally.
Add a hook to ‘swsw-before-command-hook’ which displays window IDs on
the mode line and add a hook to ‘swsw-after-command-hook’ which hides
window IDs from the mode line if SWITCH isn't nil, and remove those
hooks if SWITCH is nil.
This display function respects ‘swsw-id-format’."
  (if switch
      (progn
        (add-hook 'swsw-before-command-hook #'swsw--mode-line-display)
        (add-hook 'swsw-after-command-hook #'swsw--mode-line-hide))
    (remove-hook 'swsw-before-command-hook #'swsw--mode-line-display)
    (remove-hook 'swsw-after-command-hook #'swsw--mode-line-hide)))

;;;; Window commands:

(defun swsw--run-window-command (fun)
  "Run FUN as a window command.
Run ‘swsw-before-command-hook’, set ‘this-command’ to FUN and set a
transient map for ID selection which runs ‘swsw-after-command-hook’ on
exit."
  (run-hooks 'swsw-before-command-hook)
  (setq this-command fun)
  (set-transient-map swsw--id-map (lambda ()
                                    (run-hooks
                                     'swsw-after-command-hook))))

(defun swsw-select ()
  "Start window selection.
If less than three windows have been assigned an ID, switch to the
window returned by ‘next-window’.
Otherwise, window selection allows either choosing a window by its ID
\(switching to it), or using a window manipulation command.
This command is intended to be used only when ‘swsw-mode’ is enabled."
  (interactive)
  (if (< swsw-window-count 3)
      (select-window (next-window))
    (swsw--run-window-command #'select-window)))

(defun swsw-select-minibuffer ()
  "Select the active minibuffer window (if it exists).
This command is intended to be used only when ‘swsw-mode’ is enabled."
  (interactive)
  (let ((window (active-minibuffer-window)))
    (if window (select-window window)
      (message "There is no active minibuffer window"))))

(defun swsw-delete ()
  "Start window deletion.
If less than three windows have been assigned an ID, delete the window
returned by ‘next-window’.
Otherwise, window deletion allows either choosing a window by its ID
\(deleting it), or using a window manipulation command.
This command is intended to be used only when ‘swsw-mode’ is enabled."
  (interactive)
  (if (< swsw-window-count 3)
      (let ((window (next-window)))
        (unless (or (minibufferp (window-buffer window))
                    (minibufferp)) ; Selected window.
          (delete-window window)))
    (swsw--run-window-command #'delete-window)))

(defvar swsw-command-map (let ((map (make-sparse-keymap)))
                           (define-key map [?o] #'swsw-select)
                           (define-key map [?m] #'swsw-select-minibuffer)
                           (define-key map [?0] #'swsw-delete)
                           map)
  "Key map for window commands.
This key map is set as the parent of ‘swsw--id-map’ during ID
selection.")

;;;; Simple window switching mode:

;;;###autoload
(define-minor-mode swsw-mode
  "Minor mode for managing windows using an ID assigned to them
automatically."
  :global t
  :lighter
  (:eval (when (eq swsw-display-function 'lighter)
           (swsw-format-id (selected-window))))
  :keymap '(keymap (?\C-x . (keymap (?o . swsw-select))))
  (if swsw-mode
      (progn
        (swsw-update)
        (unless (eq swsw-display-function 'lighter)
          (funcall swsw-display-function t))
        (add-hook 'window-configuration-change-hook #'swsw-update)
        (add-hook 'minibuffer-setup-hook #'swsw-update)
        (add-hook 'minibuffer-exit-hook #'swsw-update)
        (add-hook 'after-delete-frame-functions #'swsw-update))
    (unless (eq swsw-display-function 'lighter)
      (funcall swsw-display-function nil))
    (remove-hook 'window-configuration-change-hook #'swsw-update)
    (remove-hook 'minibuffer-setup-hook #'swsw-update)
    (remove-hook 'minibuffer-exit-hook #'swsw-update)
    (remove-hook 'after-delete-frame-functions #'swsw-update)))

(provide 'swsw)

;;; swsw.el ends here
