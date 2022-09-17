;;; swsw.el --- Simple window switching -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Daniel Semyonov

;; Author: Daniel Semyonov <daniel@dsemy.com>
;; Maintainer: swsw Mailing List <~dsemy/swsw-devel@lists.sr.ht>
;; Version: 2.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://dsemy.com/projects/swsw

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
;; Enable `swsw-mode':
;;
;; (swsw-mode)
;;
;; For use-package users:
;;
;; (use-package swsw
;;   :config
;;   (swsw-mode))
;;
;; When `swsw-mode' is active:
;; - A window ID is displayed using a mode line lighter or a display
;;   function (see `swsw-display-lighter').
;; - Window IDs are assigned to all windows on all frames except for
;;   the minibuffer (by default, see `swsw-scope').
;; - `other-window' (C-x o by default) is remapped to `swsw-select'.
;;
;; C-x o ID   switches focus to the window which corresponds to ID.
;;
;; C-x o 0 ID deletes the window which corresponds to ID.
;;
;; C-x o 1 ID makes the window which corresponds to ID the sole window of
;;            its frame.
;;
;; C-x o 2 ID splits the window which corresponds to ID from below.
;;
;; C-x o 3 ID splits the window which corresponds to ID from the right.
;;
;; C-x 0 4 ID displays the buffer of the next command in the window which
;;            corresponds to ID.
;;
;; C-x 0 t ID swaps the states of the current window and the window which
;;            corresponds to ID.
;;
;; C-x o m    switches focus to the minibuffer if it's active.
;;
;; More commands can be added through `swsw-command-map':
;;
;; (define-key swsw-command-map (kbd "z") #'my-command)
;;
;; You can customize `swsw-mode' using the customize interface:
;;
;; M-x customize-group RET swsw RET
;;
;; For more information see info node `(swsw)'.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  ;; Avoid byte-compilation warnings.
  (defvar swsw-mode)
  (defvar swsw-command-map))

;;;; Customization:

(defgroup swsw nil
  "Simple window switching."
  :link '(custom-manual "(swsw) Top")
  :group 'convenience
  :prefix "swsw-")

(defcustom swsw-id-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Base set of characters from which window IDs are constructed.
This list should contain at least two characters."
  :link '(info-link "(swsw) Customization")
  :type '(repeat character)
  :initialize #'custom-initialize-changed
  :set (lambda (sym chars)
         (unless (nth 1 chars)
           (user-error
            "`swsw-id-chars' should contain at least two characters"))
         (set-default sym chars)
         (swsw--update))
  :risky t
  :package-version '(swsw . 1.0))

(defcustom swsw-scope t
  "Scope of all window operations.
- t means consider all windows on all existing frames.
- 0 (the number zero) means consider all windows on all visible and
  iconified frames.
- `visible' means consider all windows on all visible frames.
- `current' means consider only the currently selected frame."
  :link '(info-link "(swsw) Customization")
  :type '(radio (const :tag "All windows on all frames" t)
                (const
                 :tag "All windows on all visible and iconified frames" 0)
                (const :tag "All windows on all visible frames" visible)
                (const
                 :tag "All windows on the currently selected frame"
                 current))
  :initialize #'custom-initialize-changed
  :set (lambda (sym scope)
         (set-default sym scope)
         (swsw--update))
  :risky t
  :package-version '(swsw . 1.1))

(defcustom swsw-minimum 3
  "Minimum number of tracked windows for which interactive selection occurs."
  :link '(info-link "(swsw) Window Commands")
  :type 'integer
  :risky t
  :package-version '(swsw . 2.3))

(define-obsolete-variable-alias 'swsw-display-function
  'swsw-display-lighter "version 2.2 of the swsw package"
  "Function used to display the ID of each window.
This function is called with t as the sole argument when enabling
simple window switching, and with nil as the sole argument when
disabling it.")

(defcustom swsw-display-lighter t
  "Whether or not to show a mode line lighter.
- non-nil means show a mode line lighter.
- nil means don't show a mode line lighter.

This variable can also accept a \"display function\" for backwards
compatibility (see `swsw-display-function')."
  :link '(info-link "(swsw) Display functions")
  :type '(radio (const :tag "Show mode line lighter" t)
                (const :tag "Don't show mode line lighter" nil))
  :set (lambda (sym fun)
         (and (boundp sym) (functionp (symbol-value sym))
              (funcall (symbol-value sym) nil))
         (set-default sym fun)
         (when (functionp fun)
           (funcall fun t)))
  :package-version '(swsw . 2.2))

(defcustom swsw-mode-hook nil
  "Hook run when enabling or disabling simple window switching."
  :link '(info-link "(swsw) Display functions")
  :type 'hook
  :options '(swsw-mode-line-display-function
             swsw-mode-line-conditional-display-function)
  :initialize #'custom-initialize-changed
  :set (lambda (sym hooks)
         (let ((swsw-mode nil))
           (when (boundp sym) (run-hooks sym)))
         (set-default sym hooks)
         (run-hooks sym))
  :package-version '(swsw . 1.0))

(defcustom swsw-id-format " <%s>"
  "Format string for the window ID.
%s is replaced with a representation of the window's ID."
  :link '(info-link "(swsw) Customization")
  :type 'string
  :package-version '(swsw . 1.0))

;;;; Window tracking:

(defvar swsw--id-counter nil
  "Counter which determines the next possible ID.")
(put 'swsw--id-counter 'risky-local-variable t)

(defvar swsw--id-map (make-sparse-keymap)
  "Key map for window ID selection.")

(defvar swsw-window-count 0
  "Amount of windows that have been assigned an ID.")
(put 'swsw-window-count 'risky-local-variable t)

(defvar swsw--current-frame nil
  "Current frame (set by `swsw--update'), used to detect frame changes.")
(put 'swsw--current-frame 'risky-local-variable t)

(defun swsw--get-scope ()
  "Return the current scope in which windows should be tracked."
  (if (eq swsw-scope 'current)
      (selected-frame)
    swsw-scope))

(defun swsw--get-id-length ()
  "Return the current length of a window ID."
  (if-let ((windows (length (window-list-1 nil nil (swsw--get-scope))))
           ((= windows 1)))
      1 ; If there is only one window, return 1.
    (ceiling (log windows (length swsw-id-chars)))))

(defun swsw--next-id ()
  "Get the next available ID."
  (let ((len (length swsw-id-chars)) (adv-flag t) id)
    (setq swsw--id-counter
          ;; Translate the current value of the counter to the
          ;; corresponding ID.
          (mapcar (lambda (elt)
                    (push (nth elt swsw-id-chars) id)
                    ;; Advance `swsw--id-counter'.
                    (when adv-flag
                      (if (= len (setq elt (1+ elt)))
                          (setq elt 0)
                        (setq adv-flag nil)))
                    elt)
                  swsw--id-counter))
    id))

(defun swsw--update-window (window)
  "Update information for WINDOW."
  (when-let ((id (if (window-minibuffer-p window)
                     (progn
                       (setq swsw-window-count (1+ swsw-window-count))
                       nil)
                   (swsw--next-id))))
    ;; Create a key sequence from the ID, which corresponds to a
    ;; command which calls the last command (with the corresponding
    ;; window as the sole argument).
    ;; This allows controlling which command is invoked when
    ;; choosing an ID by setting `this-command' in a command which
    ;; sets the transient map to `swsw--id-map'.
    (define-key swsw--id-map (apply #'vector id)
                `(lambda ()
                   (interactive)
                   (funcall last-command ,window)))
    (set-window-parameter window 'swsw-id id)
    (setq swsw-window-count (1+ swsw-window-count))))

;; This is a separate function only to prevent running `swsw--update'
;; on any window state change.
(defun swsw--update-frame ()
  "Run `swsw--update' if the current frame isn't `swsw--current-frame'.
This check is skipped (and this function does nothing) if `swsw-scope'
is t."
  (unless (or (eq (swsw--get-scope) t)
              (eq swsw--current-frame (selected-frame)))
    (swsw--update)))

(defun swsw--update (&optional _frame)
  "Update information for all windows."
  (setq swsw--id-map (make-sparse-keymap))
  (set-keymap-parent swsw--id-map swsw-command-map)
  (setq swsw--id-counter nil
        swsw-window-count 0
        swsw--current-frame (selected-frame))
  ;; Clear and resize `swsw--id-counter' according to the ID length.
  ;; `swsw--id-counter' is treated as a base-N number where N is the
  ;; length of `swsw-id-chars' and each digit M represents the Mth
  ;; char in `swsw-id-chars'.
  (dotimes (_var (swsw--get-id-length))
    (push 0 swsw--id-counter))
  (walk-windows #'swsw--update-window nil (swsw--get-scope)))

;;;; Display functions:

(defun swsw-format-id (window)
  "Format an ID string for WINDOW."
  (format-spec
   swsw-id-format
   `((?s . ,(apply #'string (window-parameter window 'swsw-id))))))

(defun swsw--mode-line-display ()
  "Display window IDs at the beginning of the mode line."
  (setq-default mode-line-format
                `((swsw-mode
                   (:eval (swsw-format-id (selected-window))))
                  ,@(assq-delete-all
                     'swsw-mode
                     (default-value 'mode-line-format))))
  (force-mode-line-update t))

(defun swsw--mode-line-hide ()
  "Remove window IDs from the beginning of the mode line."
  (setq-default mode-line-format
                (assq-delete-all
                 'swsw-mode
                 (default-value 'mode-line-format)))
  (force-mode-line-update t))

(defun swsw-mode-line-display-function (&optional switch)
  "Display window IDs at the beginning of the mode line.
Display window IDs if simple window switching is enabled, and disable
displaying window IDs if simple window switching is disabled.
This display function respects `swsw-id-format'.

It is also possible to supply a single SWITCH argument, which will
override the value of `swsw-mode';
this form is obsolete since version 2.2 of the swsw package."
  (declare (advertised-calling-convention nil "2.2"))
  (if (or switch swsw-mode)
      (swsw--mode-line-display)
    (swsw--mode-line-hide)))

(defun swsw-mode-line-conditional-display-function (&optional switch)
  "Display window IDs at the beginning of the mode line during window selection.
Add a hook to `swsw-before-command-hook' which displays window IDs on
the mode line and add a hook to `swsw-after-command-hook' which hides
window IDs from the mode line if simple window switching is enabled,
and remove those hooks if simple window switching is disabled.
This display function respects `swsw-id-format'.

It is also possible to supply a single SWITCH argument, which will
override the value of `swsw-mode';
this form is obsolete since version 2.2 of the swsw package."
  (declare (advertised-calling-convention nil "2.2"))
  (if (or switch swsw-mode)
      (progn
        (add-hook 'swsw-before-command-hook #'swsw--mode-line-display)
        (add-hook 'swsw-after-command-hook #'swsw--mode-line-hide))
    (remove-hook 'swsw-before-command-hook #'swsw--mode-line-display)
    (remove-hook 'swsw-after-command-hook #'swsw--mode-line-hide)))

;;;; Window commands:

(defun swsw-run-window-command (fun)
  "Run FUN as a window command.
Run `swsw-before-command-hook', set `this-command' to FUN and set a
transient map for ID selection which runs `swsw-after-command-hook' on
exit."
  (run-hooks 'swsw-before-command-hook)
  (setq this-command fun)
  (set-transient-map swsw--id-map
                     (lambda ()
                       (run-hooks 'swsw-after-command-hook))))

(defmacro swsw-define-window-command (name args &rest body)
  "Define NAME as a window command with DOCSTRING as its documentation string.

Inside BODY, WINDOW and PREFIX (symbols) are bound to the selected
window and the raw prefix argument, respectively.
If PREFIX is omitted or nil, the resulting command will not accept a
prefix argument.

If MINIBUFFER is non-nil, allow the minibuffer to be selected by
`next-window' (when there are less than `swsw-minimum' tracked windows).

For more information, see info node `(swsw) Window Commands'.

\(fn NAME (WINDOW [PREFIX] [MINIBUFFER]) [DOCSTRING] BODY...)"
  (declare (debug (&define [&name symbolp] listp [&optional stringp] def-body))
           (doc-string 3) (indent defun))
  (let* ((docstring (car body)) (window (car args))
         (prefix (cadr args)) (minibuffer (caddr args)))
    `(defun ,name ,(and prefix `(,prefix))
       ,(when (stringp docstring) (format "%s

If less than `swsw-minimum' windows have been assigned an ID,
use the window returned by `next-window' (according to the
value of `swsw-scope'%s).
Otherwise, either a window is selected using its ID or a separate
window command is chosen.

  This is a window command, intended to be used only when simple
  window switching is enabled; for more information, see info node
  `(swsw) Window Commands'.
" docstring (if minibuffer "" ", excluding the minibuffer")))
       (declare (modes swsw-mode)
                (interactive-only t))
       (interactive ,(and prefix "P"))
       (if-let ((f (lambda (,window)
                     ,@body))
                ((>= swsw-window-count swsw-minimum)))
           (swsw-run-window-command f)
         (funcall f (next-window nil (unless ,minibuffer 'exclude)
                                 (swsw--get-scope)))))))

(swsw-define-window-command swsw-select (window nil t)
  "Select a window."
  (select-window window))

(swsw-define-window-command swsw-delete (window)
  "Delete a window."
  (delete-window window))

(swsw-define-window-command swsw-delete-other (window)
  "Make a window the sole window of its frame."
  (delete-other-windows window))

(swsw-define-window-command swsw-split-window-below (window size)
  "Split a window from below.
If optional argument SIZE is omitted or nil, both windows get the same
height, or close to it.  If SIZE is positive, the upper window gets
SIZE lines.  If SIZE is negative, the lower window gets -SIZE lines."
  (split-window-below (and size (prefix-numeric-value size)) window))

(swsw-define-window-command swsw-split-window-right (window size)
  "Split a window from the right.
If optional argument SIZE is omitted or nil, both windows get the same
width, or close to it.  If SIZE is positive, the left-hand window gets
SIZE columns.  If SIZE is negative, the right-hand window gets -SIZE
columns.  Here, SIZE includes the width of the window’s scroll bar; if
there are no scroll bars, it includes the width of the divider column
to the window’s right, if any."
  (split-window-right (and size (prefix-numeric-value size)) window))

(defun swsw-display-buffer-selected-window (buffer alist)
  "Display BUFFER in the selected (through swsw) window.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

The function fails if ALIST has no `window' element or its value isn't a
live window, or if it is a minibuffer window or is dedicated to another
buffer; in that case return nil.
Otherwise, return the value of the `window' element.

This is an action function for buffer display, see Info
node ‘(elisp) Buffer Display Action Functions’.  It should be
called only by ‘display-buffer’ or a function directly or
indirectly called by the latter."
  (let ((window (cdr (assq 'window alist))))
    (unless (or (not (windowp window))
                (window-minibuffer-p window)
                (window-dedicated-p window))
      (window--display-buffer buffer window 'reuse alist))))

(swsw-define-window-command swsw-selected-window-prefix (window)
  "Display the buffer of the next command in a window."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (setq alist (append `((window . ,window)) alist))
     (cons (swsw-display-buffer-selected-window buffer alist) 'reuse))
   nil (format "[swsw-window-%s]" (window-parameter window 'swsw-id)))
  (message "Display next command buffer in the selected window..."))

(swsw-define-window-command swsw-swap (window)
  "Swap the states of a window and the currently selected window."
  (window-swap-states nil window)
  (and (eq (current-buffer) (window-buffer window)) (swsw--update)))

(defun swsw-select-minibuffer ()
  "Select the active minibuffer window (if it exists)."
  (declare (modes swsw-mode))
  (interactive)
  (select-window (or (active-minibuffer-window)
                     (user-error "There is no active minibuffer window"))))

(defvar swsw-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?o] #'swsw-select)
    (define-key map [?0] #'swsw-delete)
    (define-key map [?1] #'swsw-delete-other)
    (define-key map [?2] #'swsw-split-window-below)
    (define-key map [?3] #'swsw-split-window-right)
    (define-key map [?4] #'swsw-selected-window-prefix)
    (define-key map [?t] #'swsw-swap)
    (define-key map [?m] #'swsw-select-minibuffer)
    map)
  "Key map for window commands.
This key map is set as the parent of `swsw--id-map' during ID
selection.")

;;;; swsw mode:

;;;###autoload
(define-minor-mode swsw-mode
  "Toggle swsw mode.

When swsw mode is enabled, window IDs are shown as mode line
lighters of the form \"<ID>\" (by default), and `other-window' is remapped to
`swsw-select' (a command used to select windows according to their ID).

The following key bindings are available after starting window
selection:

\\{swsw-command-map}"
  :global t
  :lighter (:eval (and swsw-display-lighter
                       (not (functionp swsw-display-lighter))
                       (swsw-format-id (selected-window))))
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap other-window] #'swsw-select)
            map)
  (if swsw-mode
      (progn
        (swsw--update)
        (when (functionp swsw-display-lighter)
          (funcall swsw-display-lighter))
        (add-hook 'window-configuration-change-hook #'swsw--update)
        (add-hook 'window-state-change-hook #'swsw--update-frame)
        (add-hook 'minibuffer-setup-hook #'swsw--update)
        (add-hook 'minibuffer-exit-hook #'swsw--update)
        (add-hook 'after-delete-frame-functions #'swsw--update))
    (when (functionp swsw-display-lighter)
      (funcall swsw-display-lighter))
    (remove-hook 'window-configuration-change-hook #'swsw--update)
    (remove-hook 'window-state-change-hook #'swsw--update-frame)
    (remove-hook 'minibuffer-setup-hook #'swsw--update)
    (remove-hook 'minibuffer-exit-hook #'swsw--update)
    (remove-hook 'after-delete-frame-functions #'swsw--update)))

(provide 'swsw)

;;; swsw.el ends here
