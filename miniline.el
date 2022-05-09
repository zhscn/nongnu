;;; miniline.el --- Modular status bar in echo area -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") minibuffer-line)
;; Keywords: calendar, hardware
;; URL: https://codeberg.org/akib/emacs-miniline

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

;; The echo area is unused most of the time.  Sometimes this empty area so
;; annoying that some people want to get rid of it.  This package makes it
;; useful by showing a status bar in it when it's unused.  This is
;; especially helpful when you use Emacs as your X window manager using
;; EXWM.
;;
;; Usage:
;;
;; Enable `miniline-mode' to display miniline.  You may want to put
;; (miniline-mode +1) in your init file.  There are several user options
;; you can customize, use `customize-group' to see and possibly customize
;; them.
;;
;; Miniline renders the status bar and it uses `minibuffer-line' package
;; to show the line in echo area.  To change the refresh rate, customize
;; the `minibuffer-line-refresh-interval' variable.  To change the face,
;; customize the `minibuffer-line' face.

;;; Code:

(require 'minibuffer-line)

(defgroup miniline nil
  "Modular status bar in echo bar."
  :group 'tools
  :link '(url-link "https://codeberg.org/akib/emacs-miniline")
  :prefix "miniline-")

(defcustom miniline-module-separator "  "
  "Separator to separate modules."
  :type 'string)

(defcustom miniline-group-separator "     "
  "Separator to separate groups."
  :type 'string)

(defcustom miniline-group-left nil
  "Modules to be placed on the left of `miniline'.

The value should be a list of functions.  Each function should return a
string to display, or nil in case there is to show."
  :type '(repeat function))

(defcustom miniline-group-middle '(miniline-module-time)
  "Modules to be placed on the middle of `miniline'.

The value should be a list of functions.  Each function should return a
string to display, or nil in case there is to show."
  :type '(repeat function))

(defcustom miniline-group-right nil
  "Modules to be placed on the right of `miniline'.

The value should be a list of functions.  Each function should return a
string to display, or nil in case there is to show."
  :type '(repeat function))

(defun miniline--render-group (modules)
  "Render MODULES."
  (mapconcat #'identity (delete nil (mapcar #'funcall modules))
             miniline-module-separator))

;;;###autoload
(defun miniline-render ()
  "Render Miniline."
  (with-temp-buffer
    (let ((bar "")
          (width (frame-width (window-frame (minibuffer-window))))
          (left (miniline--render-group
                 miniline-group-left))
          (middle (miniline--render-group
                   miniline-group-middle))
          (right (miniline--render-group
                  miniline-group-right)))

      ;; HACK: Emacs doesn't show the last character on terminal, so
      ;; decrease the width by one in that case.
      (unless (display-graphic-p)
        (setq width (1- width)))
      (unless (zerop (length left))
        (setq bar (concat left miniline-group-separator)))
      (unless (zerop (length middle))
        (setq bar (concat bar (make-list
			       (max 0 (- (/ (- width (length middle)) 2)
					 (length bar)))
                               ? )
                          middle miniline-group-separator)))
      (unless (zerop (length right))
        (setq bar (concat bar (make-list
			       (max 0 (- width (length right)
					 (length bar)))
                               ? )
                          right)))
      (replace-regexp-in-string
       "%" "%%" (format (format "%%-%i.%is" width width) bar)))))

;;;###autoload
(define-minor-mode miniline-mode
  "Toggle Miniline display."
  :init-value nil
  :lighter " Miniline"
  :keymap nil
  :global t
  (if miniline-mode
      (progn
        (setq minibuffer-line-format '(:eval (miniline-render)))
        (minibuffer-line-mode +1))
    (setq minibuffer-line-format
          (ignore-errors
            (eval (car (get 'minibuffer-line-format 'standard-value)))))
    (minibuffer-line-mode -1)))

(defcustom miniline-module-time-format "%a %b %d %H:%M"
  "Time format for time module."
  :type 'string)

(defun miniline-module-time ()
  "Module for showing time."
  (format-time-string miniline-module-time-format))

(defcustom miniline-module-battery-cache-for 60
  "Cache battery status for this many seconds.  Set to zero disable."
  :type 'number)

(defcustom miniline-module-battery-low-threshold 30
  "When battery is less than this many percent, treat it as low."
  :type 'number)

(defface miniline-module-battery-low-face
  '((t :inherit warning))
  "Face to use when battery is low.")

(defvar miniline--module-battery-cache nil
  "Cached battery status.

The value is a cons cell whose car is the status and cdr is the time when
it was recorded.")

(defun miniline-module-battery ()
  "Module for showing battery status."
  (when (or (not miniline--module-battery-cache)
            (>= (float-time
                 (time-since (cdr miniline--module-battery-cache)))
                miniline-module-battery-cache-for))
    (require 'battery nil t)
    (setq
     miniline--module-battery-cache
     (cons
      (let ((status (when (boundp 'battery-status-function)
                      (funcall battery-status-function))))
        (when (and (listp status)
                   (assq ?p status)
                   (assq ?L status)
                   (assq ?h status)
                   (assq ?m status))
          (let ((load (floor (string-to-number (alist-get ?p status))))
                (charging (string= (alist-get ?L status) "AC"))
                (remaining-hours (string-to-number (alist-get ?h status)))
                (remaining-minutes (mod (string-to-number
                                         (alist-get ?m status))
                                        60)))
            (concat
             (if (and (< load 30)
                      (not charging))
                 (propertize (format "%i%%" load) 'face
                             'miniline-module-battery-low-face)
               (format "%s%%" load))
             (if charging "+" " ")
             (if (eq load 100)
                 "(full)"
               (format "(%02i:%02i)" remaining-hours
                       remaining-minutes))))))
      (current-time))))
  (car miniline--module-battery-cache))

(defcustom miniline-module-temperature-cache-for 5
  "Cache CPU temperature for this many seconds.  Set to zero disable."
  :type 'number)

(defcustom miniline-module-temperature-high-threshold 70
  "When temperature is more than this many percent, treat it as high.

The value should be less than
`miniline-module-temperature-very-high-threshold'."
  :type 'number)

(defcustom miniline-module-temperature-very-high-threshold 90
  "When temperature is more than this many percent, treat it as very high.

The value should be more than
`miniline-module-temperature-high-threshold'."
  :type 'number)

(defface miniline-module-temperature-high-face
  '((t :inherit warning))
  "Face to use when temperature is high.")

(defface miniline-module-temperature-very-high-face
  '((t :inherit error))
  "Face to use when temperature is very high.")

(defvar miniline--module-temperature-cache nil
  "Cached CPU temperature.

The value is a cons cell whose car is the temperature and cdr is the time
when it was recorded.")

(defun miniline-module-temperature ()
  "Module for showing CPU temperature"
  (when (or (not miniline--module-temperature-cache)
            (>= (float-time
                 (time-since (cdr miniline--module-temperature-cache)))
                miniline-module-temperature-cache-for))
    (setq
     miniline--module-temperature-cache
     (cons
      (let ((zone 0))
        (with-temp-buffer
          (insert-file-contents
           (format "/sys/class/thermal/thermal_zone%i/temp" zone))
          (let* ((temp (/ (string-to-number (buffer-string)) 1000))
                 (str (concat (int-to-string temp)
                              (if (char-displayable-p ?°) "°" " ")
                              "C")))
            (if (>= temp miniline-module-temperature-high-threshold)
                (if (>= temp
                        miniline-module-temperature-very-high-threshold)
                    (propertize str 'face
                                'miniline-module-temperature-high-face)
                  (propertize str 'face
                              'miniline-module-temperature-very-high-face))
              str))))
      (current-time))))
  (car miniline--module-temperature-cache))

(defcustom miniline-module-network-speeds-cache-for 1
  "Cache network speed values for this many seconds.  Set to zero disable."
  :type 'number)

(defvar miniline--module-network-speeds-last-byte-counts nil
  "Total sent bytes and received bytes previously calculated.")

(defvar miniline--module-network-speeds-cache nil
  "Cached network speeds.

The value is a cons cell whose car is the speeds and cdr is the time when
it was recorded.")

(defun miniline-module-network-speeds ()
  "Module for showing network speeds."
  (when (or (not miniline--module-network-speeds-cache)
            (>= (float-time
                 (time-since (cdr miniline--module-network-speeds-cache)))
                miniline-module-network-speeds-cache-for))
    (setq
     miniline--module-network-speeds-cache
     (cons
      (let ((previous miniline--module-network-speeds-last-byte-counts)
            (current
             (cons (current-time)
                   (with-temp-buffer
                     (insert-file-contents "/proc/net/dev")
                     (let ((regexp "^[\s\t]*\\(.*\\):")
                           (sent 0)
                           (received 0))
                       (goto-char (point-min))
                       (while (search-forward-regexp regexp nil t)
                         (setq received (+ received
                                           (read (current-buffer)))))
                       (goto-char (point-min))
                       (while (search-forward-regexp regexp nil t)
                         (forward-word 8)
                         (setq sent (+ sent (read (current-buffer)))))
                       (cons sent received))))))
        (let* ((speeds
                (if previous
                    (let ((delta (float-time
                                  (time-subtract (car current)
                                                 (car previous)))))
                      (cons (/ (float (- (car (cdr current))
                                         (car (cdr previous))))
                               delta)
                            (/ (float (- (cdr (cdr current))
                                         (cdr (cdr previous))))
                               delta)))
                  '(0.0 . 0.0)))
               (formatter
                (lambda (rate)
                  (let ((unit "B"))
                    (cond
                     ((>= rate (* 1024 1024 1000))
                      (setq unit "GB")
                      (setq rate (/ rate (* 1024 1024 1000))))
                     ((>= rate (* 1024 1000))
                      (setq unit "MB")
                      (setq rate (/ rate (* 1024 1000))))
                     ((>= rate 1000)
                      (setq unit "KB")
                      (setq rate (/ rate 1000))))
                    (if (string= unit "B")
                        (when (>= rate 100)
                          (setq rate (floor rate)))
                      (when (>= rate 10)
                        (setq rate (floor rate))))
                    (format (if (floatp rate)
                                (format "%%%i.%if %%s/s"
                                        (if (string= unit "B") 4 3)
                                        (if (string= unit "B")
                                            (if (>= rate 10) 1 2)
                                          1))
                              (format "%%%ii %%s/s"
                                      (if (string= unit "B") 4 3)))
                            rate unit)))))
          (setq miniline--module-network-speeds-last-byte-counts current)
          (format "↑ %s ↓ %s"
                  (funcall formatter (car speeds))
                  (funcall formatter (cdr speeds)))))
      (current-time))))
  (car miniline--module-network-speeds-cache))

(defcustom miniline-module-cpu-cache-for 1
  "Cache CPU load values for this many seconds.  Set to zero disable."
  :type 'number)

(defvar miniline--module-cpu-cache nil
  "Cached CPU load.

The value is a cons cell whose car is the CPU load and cdr is the time when
it was recorded.")

(defvar miniline--module-cpu-last-times nil
  "Last CPU idle and total calculated.")

(defvar miniline--module-cpu-count nil
  "Count of processor or CPU.")

(defun miniline--module-cpu-calculate-load (cpu)
  "Calculate CPU load."
  (let* ((last-cell
          (let ((cell (assoc-string cpu miniline--module-cpu-last-times)))
            (unless cell
              (setq miniline--module-cpu-last-times
                    (cons (cons cpu nil) miniline--module-cpu-last-times))
              (setq cell (assoc-string cpu
                                       miniline--module-cpu-last-times)))
            cell))
         (last (cdr last-cell))
         (now
          (let ((total 0)
                (active 0)
                (data nil))
            (goto-char (point-min))
            (search-forward cpu)
            (setq data (mapcar (lambda (_) (read (current-buffer)))
                               (number-sequence 0 7)))
            (setq total (apply #'+ data))
            (setq active (- total (+ (nth 3 data)
                                     (nth 4 data))))
            (cons total active))))
    (setcdr last-cell now)
    (* 100
       (if last
           (let ((diff-total (- (car now) (car last)))
                 (diff-active (- (cdr now) (cdr last))))
             (if (zerop diff-total)
                 0.0
               (/ (float diff-active) diff-total)))
         0.0))))

(defun miniline-module-cpu ()
  "Module for showing CPU loads."
  (when (or (not miniline--module-cpu-cache)
            (>= (float-time
                 (time-since (cdr miniline--module-cpu-cache)))
                miniline-module-cpu-cache-for))
    (setq
     miniline--module-cpu-cache
     (cons
      (with-temp-buffer
        (insert-file-contents "/proc/stat")
        (unless miniline--module-cpu-count
          (setq miniline--module-cpu-count
                (string-to-number (shell-command-to-string "nproc"))))
        (format
         "%3i%%%s"
         (miniline--module-cpu-calculate-load "cpu")
         (if (display-graphic-p)
             (concat
              " "
              (mapconcat
               (lambda (i)
                 (let ((load (miniline--module-cpu-calculate-load
                              (format "cpu%i" i))))
                   (cond
                    ((>= load 87.5)
                     (propertize (string #x2588) 'face 'bold))
                    ((>= load 75)
                     (propertize (string #x2587) 'face 'bold))
                    ((>= load 62.5)
                     (propertize (string #x2586) 'face 'bold))
                    ((>= load 50)
                     (propertize (string #x2585) 'face 'bold))
                    ((>= load 37.5)
                     (propertize (string #x2584) 'face 'bold))
                    ((>= load 25)
                     (propertize (string #x2583) 'face 'bold))
                    ((>= load 12.5)
                     (propertize (string #x2582) 'face 'bold))
                    (t
                     (propertize
                      (string #x2581) 'face
                      '(:weight bold :inherit font-lock-comment-face))))))
               (number-sequence 0 (1- miniline--module-cpu-count)) ""))
           "")))
      (current-time))))
  (car miniline--module-cpu-cache))

(provide 'miniline)
;;; miniline.el ends here
