;;; swsw.el --- Simple window switching -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Semyonov

;; Author: Daniel Semyonov <cmstr@dsemy.com>
;; Version: 0.1
;; Package-Requires: ((emacs "23.1"))
;; Keywords: convenience
;; URL: https://sr.ht/~dsemy/swsw

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
;; windows using single character IDs shown on the mode line.

;;; Code:

;;;; Customization:

(defgroup swsw nil
  "Simple window switching."
  :group 'convenience
  :prefix "swsw-")

(defcustom swsw-id-chars-base '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Base set of characters from which window IDs are constructed."
  :group 'swsw
  :type '(repeat 'character))

(defcustom swsw-id-prompt "Window: "
  "Prompt to use when switching windows."
  :group 'swsw
  :type '(string))

;;;; Simple window switching minor mode:

(defvar swsw-id-chars swsw-id-chars-base
  "Characters from which window IDs can currently be constructed.")

(defvar swsw-window-list nil
  "List of active window plists.")

(defun swsw-reset ()
  "Reset information for all windows."
  (setq swsw-window-list nil
        swsw-id-chars swsw-id-chars-base))

(defun swsw-update (window)
  "Update information for WINDOW."
  (let ((id (pop swsw-id-chars)))
    (when id
      (push (list
             id
             :window window
             :buffer (window-buffer window))
            swsw-window-list)
      (set-window-parameter window 'swsw-id id))))

(defun swsw--reset-and-update ()
  "Run `swsw-reset', run `swsw-update' for all active windows and force a mode
line update for all windows."
  (swsw-reset)
  (walk-windows #'swsw-update 'no-minibuffer t)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode swsw-mode
  "Minor mode for selecting buffers by their ID."
  :global t
  (if swsw-mode
      (progn
        (walk-windows #'swsw-update 'no-minibuffer t)
        (setq-default mode-line-format
                      `((swsw-mode
                         (:eval (char-to-string
                                 (window-parameter (selected-window)
                                                   'swsw-id))))
                        ,@(assq-delete-all
                           'swsw-mode
                           (default-value 'mode-line-format))))
        (force-mode-line-update t)
        (add-hook 'window-configuration-change-hook #'swsw--reset-and-update))
    (setq swsw-window-list nil
          swsw-id-chars swsw-id-chars-base)
    (setq-default mode-line-format
                  (assq-delete-all
                   'swsw-mode
                   (default-value 'mode-line-format)))
    (remove-hook 'window-configuration-change-hook #'swsw--reset-and-update)))

(defun swsw-select (&optional id)
  "Select window by its ID."
  (interactive (unless (< (length swsw-window-list) 3)
                 (list (read-char-from-minibuffer (format swsw-id-prompt)))))
  (if id
      (let (window)
        (when (setq
               window (plist-get (cdr (assq id swsw-window-list)) :window))
          (select-window window)))
    (other-window 1)))

(provide 'swsw)

;; swsw.el ends here
