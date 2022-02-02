;;; why-this.el --- Minor mode for showing why the current line was changed -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Version: 1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: tools, convenience, vc
;; URL: https://codeberg.org/akib/emacs-why-this

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

;; This package shows the last commit which changed the current line.

;;; Code:

(defgroup why-this nil
  "Show why the current line was changed."
  :group 'tools
  :link '(url-link "https://codeberg.org/akib/emacs-why-this")
  :prefix "why-this-")

(defcustom why-this-backends nil
  "List of enabled backends."
  :type '(repeat (function :tag "Backend"))
  :group 'why-this)

(defcustom why-this-message-format "     %A, %T * %i"
  "Format string for formatting message.

All characters are written as is, except certain constructs which are
substituted by text describing the author, time or message:

  %a    Author name, as returned by the backend.
  %A    Author nick name.
  %T    Time when last changed, formatted as \"%d %B %Y\" (see
        `format-time-string').
  %i    Message.

The value can also be a function to do the formatting itself."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter function"))
  :group 'why-this)

(defcustom why-this-nick-name-alist nil
  "Alist of nick name of authors.

Each element is of the following form: (NICK . AUTHORS), where NICK is the
nick name and AUTHORS is list of the name of authors corresponding to
NICK."
  :type '(repeat (cons (string :tag "Nick")
                       (repeat (string :tag "Author")))))

(defcustom why-this-idle-delay 0.5
  "Idle delay for rendering."
  :type 'number
  :group 'why-this)

(defface why-this-face
  '((t :foreground "#a8a8a8"
       :background nil
       :italic t))
  "Face for Why-This data."
  :group 'why-this)

(defvar why-this--overlays nil
  "Overlays created by Why-This.")

(defvar why-this--idle-timer nil
  "Timer for rendering.")

(defvar-local why-this--backend nil
  "Backend for current buffer.")

(defun why-this-nick-name (author)
  "Return nick name of AUTHOR."
  (catch 'name
    (dolist (nick why-this-nick-name-alist)
      (when (member author (cdr nick))
        (throw 'name (car nick))))
    author))

(defun why-this-format-data (data)
  "Format DATA."
  (if (functionp why-this-message-format)
      (funcall why-this-message-format data)
    (let ((alist `((?a . (plist-get data :author))
                   (?A . (why-this-nick-name (plist-get data :author)))
                   (?T . (format-time-string "%d %B %Y"
                                             (plist-get data :time)))
                   (?i . (plist-get data :message)))))
      (replace-regexp-in-string
       "%."
       (lambda (str)
         (let ((char (aref str 1)))
           (if (eq char ?%)
               "%"
             (let ((sexp (cdr (assoc char alist))))
               (if sexp
                   (eval sexp `((data . ,data)))
                 str)))))
       why-this-message-format t t))))

(defvar why-this-mode)

(defun why-this--render ()
  "Render overlays."
  (while why-this--overlays
    (delete-overlay (car (pop why-this--overlays))))
  (when why-this-mode
    (let* ((begin (line-number-at-pos (if (use-region-p)
                                          (region-beginning)
                                        (point))))
           (end (1+ (line-number-at-pos (if (use-region-p)
                                            (region-end)
                                          (point)))))
           (backend why-this--backend)
           (data (funcall backend 'get-data (buffer-file-name) begin end)))
      (dolist (i (number-sequence 0 (- end begin 1)))
        (let ((pos (save-excursion
                     (goto-char (point-min))
                     (line-end-position (+ begin i)))))
          (let ((ov (make-overlay pos pos)))
            (overlay-put ov 'after-string
                         (propertize (why-this-format-data
                                      (append `(:backend ,backend)
                                              (nth i data)))
                                     'cursor t 'face 'why-this-face))
            (overlay-put ov 'why-this-line (+ begin i))
            (push (cons ov (current-buffer)) why-this--overlays)))))))

(defun why-this--render-non-blocking ()
  "Render overlays, but don't block Emacs."
  (while-no-input
    (why-this--render)))

(defun why-this--update-overlays ()
  "Update overlays."
  (let ((begin (line-number-at-pos (if (use-region-p)
                                       (region-beginning)
                                     (point))))
        (end (1+ (line-number-at-pos (if (use-region-p)
                                         (region-end)
                                       (point))))))
    (setq
     why-this--overlays
     (delq
      nil
      (mapcar
       (lambda (ov)
         (if (and (eq (cdr ov) (current-buffer))
                  (let ((line (line-number-at-pos
                               (overlay-start (car ov)))))
                    (and (>= line begin)
                         (< line end)
                         (eq line (overlay-get (car ov) 'why-this-line)))))
             (progn
               (let ((ov-start (overlay-start (car ov))))
                 (when (and (eq (line-number-at-pos)
                                (line-number-at-pos ov-start))
                            (> (point) ov-start))
                   (let ((pos (save-excursion
                                (goto-char ov-start)
                                (line-end-position))))
                     (move-overlay (car ov) pos pos))))
               ov)
           (delete-overlay (car ov))
           nil))
       why-this--overlays)))))

;;;###autoload
(defun why-this-supported-p ()
  "Return non-nil if the Why-This mode is support in current buffer.

Actually the supported backend is returned."
  (catch 'yes
    (dolist (backend why-this-backends)
      (let ((result (funcall backend 'supported-p)))
        (when result
          (throw 'yes backend))))
    nil))

;;;###autoload
(define-minor-mode why-this-mode
  "Toggle showing why the current line was changed."
  nil " Why-This" nil
  (setq why-this--backend (why-this-supported-p))
  (if (not why-this--backend)
      (setq why-this-mode nil)
    (if why-this-mode
        (progn
          (add-hook 'post-command-hook #'why-this--update-overlays nil t)
          (when why-this--idle-timer
            (cancel-timer why-this--idle-timer)
            (setq why-this--idle-timer nil))
          (setq why-this--idle-timer
                (run-with-idle-timer why-this-idle-delay t
                                     #'why-this--render-non-blocking)))
      (remove-hook 'post-command-hook #'why-this--update-overlays t)
      (cancel-timer why-this--idle-timer)
      (setq why-this--idle-timer nil))))

(provide 'why-this)
;;; why-this.el ends here
