;;; swsw.el --- Simple window switching -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Semyonov

;; Author: Daniel Semyonov <cmstr@dsemy.com>
;; Version: 0.3
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

(defcustom swsw-id-chars '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Base set of characters from which window IDs are constructed."
  :group 'swsw
  :type '(repeat character))

(defcustom swsw-minibuffer-id ?m
  "ID reserved for the minibuffer."
  :group 'swsw
  :type '(character))

(defcustom swsw-mode-lighter-format " <%s>"
  "Format string for the lighter of `swsw-mode'.
%s is replaced with a representation of the window's ID."
  :group 'swsw
  :type '(string))

;;;; Simple window switching minor mode:

(defvar swsw-ids nil
  "IDs which can currently be assigned to windows.")

(defvar swsw-window-list nil
  "Alist of active active windows and their IDs.")

(defun swsw--get-possible-ids (&rest char-lists)
  "Return the Cartesian product of all CHAR-LISTS."
  (if char-lists
      (mapcan (lambda (inner)
                (mapcar (lambda (outer)
                          (cons outer inner))
                        (car char-lists)))
              (apply #'swsw--get-possible-ids (cdr char-lists)))
    (list nil)))

(defun swsw--get-id-length ()
  "Return the current length of a window ID."
  (let* ((windows (length (window-list-1)))
         (chars (length swsw-id-chars))
         (div (/ windows chars)))
    ;; Check the remainder to returning a longer length than necessary.
    (if (= 0 (mod windows chars))
        div
      (1+ div))))

(defun swsw-update ()
  "Update information for all windows."
  (setq swsw-window-list nil
        ;; Build a list of all possible IDs for the current length.
        swsw-ids (let ((acc 0) (len (swsw--get-id-length)) char-lists)
                   (while (< acc len)
                     (push swsw-id-chars char-lists)
                     (setq acc (1+ acc)))
                   (apply #'swsw--get-possible-ids char-lists)))
  (walk-windows #'swsw-update-window nil t))

(defun swsw-update-window (window)
  "Update information for WINDOW."
  (let ((id (if (window-minibuffer-p window)
                swsw-minibuffer-id
              (pop swsw-ids))))
    (when id
      (push (cons id window) swsw-window-list)
      (set-window-parameter window 'swsw-id id))))

(defun swsw-mode--lighter-format (window)
  "Format a `swsw-mode' mode line lighter for WINDOW."
  (format swsw-mode-lighter-format
          (reverse (apply #'string (window-parameter window 'swsw-id)))))

;;;###autoload
(define-minor-mode swsw-mode
  "Minor mode for selecting windows by their ID."
  :global t
  :lighter (:eval (swsw-mode--lighter-format (selected-window)))
  :keymap (make-sparse-keymap)
  (if swsw-mode
      (progn
        (swsw-update)
        (force-mode-line-update t)
        (add-hook 'window-configuration-change-hook #'swsw-update)
        (add-hook 'minibuffer-setup-hook #'swsw-update)
        (add-hook 'minibuffer-exit-hook #'swsw-update))
    (remove-hook 'window-configuration-change-hook #'swsw-update)
    (remove-hook 'minibuffer-setup-hook #'swsw-update)
    (remove-hook 'minibuffer-exit-hook #'swsw-update)))

(defun swsw--read-id (len)
  "Read a window ID of length LEN using `read-char'."
  (let ((acc 1) id)
    ;; Special case for the minibuffer.
    (if (eq (car (push (read-char) id)) swsw-minibuffer-id)
        id
      (while (< acc len)
        (push (read-char) id)
        (setq acc (1+ acc)))
      (list id))))

(defun swsw-select (&optional id)
  "Select window by its ID."
  ;; If there are less than 3 windows, don't get an ID.
  (interactive (unless (< (length swsw-window-list) 3)
                 (swsw--read-id (swsw--get-id-length))))
  (if id
      (let ((window (cdr (assoc id swsw-window-list))))
        (when window
          (select-window window)))
    (other-window 1)))

(provide 'swsw)

;; swsw.el ends here
