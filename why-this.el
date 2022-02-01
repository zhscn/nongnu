;;; why-this.el --- Minor mode for showing why the current line was changed -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akib Azmain Turja.

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
  :prefix "why-this-")

(defcustom why-this-backends nil
  "List of enabled backends."
  :type '(repeat function)
  :group 'why-this)

(defcustom why-this-message-format "     %a, %T * %i"
  "Format string for formatting message.

It can also be a function to do the formatting itself."
  :type '(choice string function)
  :group 'why-this)

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

(defun why-this-format-data (data)
  "Format DATA."
  (if (functionp why-this-message-format)
      (funcall why-this-message-format data)
    (let ((alist `((?a . ,(plist-get data :author))
                   (?T . ,(format-time-string "%d %B %Y"
                                              (plist-get data :time)))
                   (?i . ,(plist-get data :message)))))
      (replace-regexp-in-string
       "%."
       (lambda (str)
         (let ((char (aref str 1)))
           (if (eq char ?%)
               "%"
             (or (cdr (assoc char alist)) str))))
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
            (push (cons ov (current-buffer)) why-this--overlays)))))))

(defun why-this--render-non-blocking ()
  "Render overlays, but don't block Emacs."
  (while-no-input
    (why-this--render)))

(defun why-this--delete-overlays ()
  "Delete all overlays unneeded overlays."
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
                         (< line end))))
             ov
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
          (add-hook 'post-command-hook #'why-this--delete-overlays nil t)
          (when why-this--idle-timer
            (cancel-timer why-this--idle-timer)
            (setq why-this--idle-timer nil))
          (setq why-this--idle-timer
                (run-with-idle-timer why-this-idle-delay t
                                     #'why-this--render-non-blocking)))
      (remove-hook 'post-command-hook #'why-this--delete-overlays t)
      (cancel-timer why-this--idle-timer)
      (setq why-this--idle-timer nil))))

;;; why-this.el ends here
