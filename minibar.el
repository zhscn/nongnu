;;; minibar.el --- Modular status bar in minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Version: 0.3
;; Package-Requires: ((emacs "27.2"))
;; Keywords: calendar, hardware
;; URL: https://codeberg.org/akib/emacs-minibar

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

;; The echo area is unused most of the time.  Sometimes this empty
;; area so annoying that some people want to get rid of it.  This
;; package makes it useful by showing a status bar in it when it's
;; unused.  This is especially helpful when you use Emacs as your X
;; window manager using EXWM.
;;
;; Usage:
;;
;; Enable `minibar-mode' to display minibar.  You may want to put
;; (minibar-mode +1) in your init file.  There are several user
;; options you can customize, use `customize-group' to discover and
;; possibly customize them.

;;; Code:

(defgroup minibar nil
  "Modular status bar in echo bar."
  :group 'tools
  :link '(url-link "https://codeberg.org/akib/emacs-minibar")
  :prefix "minibar-")

(defcustom minibar-module-separator "  "
  "Separator to separate modules."
  :type 'string)

(defcustom minibar-group-separator "     "
  "Separator to separate groups."
  :type 'string)

(defcustom minibar-group-left nil
  "Modules to be placed on the left of Minibar.

The value should be a list of functions.  Each function should return
a string to display, or nil in case there is to show."
  :type '(repeat function))

(defcustom minibar-group-middle '(minibar-module-time)
  "Modules to be placed on the middle of Minibar.

The value should be a list of functions.  Each function should return
a string to display, or nil in case there is to show."
  :type '(repeat function))

(defcustom minibar-group-right nil
  "Modules to be placed on the right of Minibar.

The value should be a list of functions.  Each function should return
a string to display, or nil in case there is to show."
  :type '(repeat function))

(defcustom minibar-update-interval 1
  "Update Minibar every this many seconds while idling."
  :type 'number)

(defface minibar-face
  '((t :inherit default))
  "Default face of Minibar.")

(defvar minibar--update-timer nil
  "Timer to update Minibar.")

(defun minibar--render-group (modules)
  "Render MODULES."
  (mapconcat #'identity (delete nil (mapcar #'funcall modules))
             minibar-module-separator))

(defun minibar-update ()
  "Update Minibar."
  (let* ((width (window-max-chars-per-line (minibuffer-window)))
         (left (minibar--render-group
                minibar-group-left))
         (middle (minibar--render-group
                  minibar-group-middle))
         (right (minibar--render-group
                 minibar-group-right))
         (middle-offset (/ (- width (length middle)) 2))
         (left (if (string-empty-p left)
                   left
                 (concat left minibar-group-separator)))
         (middle (if (string-empty-p middle)
                     middle
                   (concat middle minibar-group-separator)))
         (empty (- width (length left) (length middle)
                   (length right)))
         (left-empty (max (min empty (- middle-offset (length left)))
                          0))
         (right-empty (max (- empty left-empty) 0)))
    (with-temp-buffer
      (let ((text (format (format "%%-%i.%is" width width)
                          (concat left (make-string left-empty ? )
                                  middle (make-string right-empty ? )
                                  right))))
        (add-face-text-property 0 width 'minibar-face t text)
        (with-current-buffer (get-buffer-create " *Minibuf-0*")
          (erase-buffer)
          (insert text))))))

(defun minibar--cancel-timer ()
  "Cancel timer for updating Minibar."
  (when minibar--update-timer
    (cancel-timer minibar--update-timer)
    (setq minibar--update-timer nil)))

(defun minibar--start-timer ()
  "Start timer to update Minibar."
  (minibar--cancel-timer)
  (setq minibar--update-timer
        (run-with-timer t minibar-update-interval #'minibar-update)))

;;;###autoload
(define-minor-mode minibar-mode
  "Toggle Minibar display."
  :lighter " Minibar"
  :global t
  (if minibar-mode
      (progn
        (add-hook 'pre-command-hook #'minibar--cancel-timer)
        (add-hook 'post-command-hook #'minibar--start-timer))
    (remove-hook 'pre-command-hook #'minibar--cancel-timer)
    (remove-hook 'post-command-hook #'minibar--start-timer)
    (when minibar--update-timer
      (cancel-timer minibar--update-timer)
      (setq minibar--update-timer nil))
    (with-current-buffer (get-buffer-create " *Minibuf-0*")
      (erase-buffer))))

(defcustom minibar-module-time-format "%a %b %d %H:%M"
  "Time format for time module."
  :type 'string)

(defun minibar-module-time ()
  "Module for showing time."
  (format-time-string minibar-module-time-format))

(defcustom minibar-module-battery-cache-for 60
  "Cache battery status for this many seconds.  Set to zero disable."
  :type 'number)

(defcustom minibar-module-battery-low-threshold 30
  "When battery is less than this many percent, treat it as low."
  :type 'number)

(defface minibar-module-battery-low-face
  '((t :inherit warning))
  "Face to use when battery is low.")

(defvar minibar--module-battery-cache nil
  "Cached battery status.

The value is a cons cell whose car is the status and cdr is the time
when it was recorded.")

(defun minibar-module-battery ()
  "Module for showing battery status."
  (when (or (not minibar--module-battery-cache)
            (>= (float-time
                 (time-since (cdr minibar--module-battery-cache)))
                minibar-module-battery-cache-for))
    (require 'battery nil t)
    (setq
     minibar--module-battery-cache
     (cons
      (let ((status (when (boundp 'battery-status-function)
                      (funcall battery-status-function))))
        (when (and (listp status)
                   (assq ?p status)
                   (assq ?B status)
                   (assq ?h status)
                   (assq ?m status))
          (let ((load (floor (string-to-number
                              (alist-get ?p status))))
                (state (alist-get ?b status))
                (remaining-hours (string-to-number
                                  (alist-get ?h status)))
                (remaining-minutes (mod (string-to-number
                                         (alist-get ?m status))
                                        60)))
            (concat
             (if (and (< load minibar-module-battery-low-threshold)
                      (not (zerop (length state))))
                 (propertize (format "%i%%" load) 'face
                             'minibar-module-battery-low-face)
               (format "%s%%" load))
             (format "%1s" state)
             (if (eq load 100)
                 "(full)"
               (format "(%02i:%02i)" remaining-hours
                       remaining-minutes))))))
      (current-time))))
  (car minibar--module-battery-cache))

(defcustom minibar-module-temperature-cache-for 5
  "Cache CPU temperature for this many seconds.  Set to zero disable."
  :type 'number)

(defcustom minibar-module-temperature-high-threshold 70
  "When temperature is more than this many percent, treat it as high.

The value should be less than
`minibar-module-temperature-very-high-threshold'."
  :type 'number)

(defcustom minibar-module-temperature-very-high-threshold 90
  "When temperature is more than this many percent, treat it as very high.

The value should be more than
`minibar-module-temperature-high-threshold'."
  :type 'number)

(defface minibar-module-temperature-high-face
  '((t :inherit warning))
  "Face to use when temperature is high.")

(defface minibar-module-temperature-very-high-face
  '((t :inherit error))
  "Face to use when temperature is very high.")

(defvar minibar--module-temperature-cache nil
  "Cached CPU temperature.

The value is a cons cell whose car is the temperature and cdr is the
time when it was recorded.")

(defun minibar-module-temperature ()
  "Module for showing CPU temperature."
  (when (or (not minibar--module-temperature-cache)
            (>= (float-time
                 (time-since (cdr minibar--module-temperature-cache)))
                minibar-module-temperature-cache-for))
    (setq
     minibar--module-temperature-cache
     (cons
      (let ((zone 0))
        (with-temp-buffer
          (insert-file-contents
           (format "/sys/class/thermal/thermal_zone%i/temp" zone))
          (let* ((temp (/ (string-to-number (buffer-string)) 1000))
                 (str (concat (int-to-string temp)
                              (if (char-displayable-p ?°) "°" " ")
                              "C")))
            (if (>= temp minibar-module-temperature-high-threshold)
                (propertize
                 str 'face
                 (if (>=
                      temp
                      minibar-module-temperature-very-high-threshold)
                     'minibar-module-temperature-very-high-face
                   'minibar-module-temperature-high-face))
              str))))
      (current-time))))
  (car minibar--module-temperature-cache))

(defcustom minibar-module-network-speeds-cache-for 1
  "Cache network speed values for this many seconds.

Set to zero disable."
  :type 'number)

(defvar minibar--module-network-speeds-last-byte-counts nil
  "Total sent bytes and received bytes previously calculated.")

(defvar minibar--module-network-speeds-cache nil
  "Cached network speeds.

The value is a cons cell whose car is the speeds and cdr is the time
when it was recorded.")

(defun minibar-module-network-speeds ()
  "Module for showing network speeds."
  (when (or (not minibar--module-network-speeds-cache)
            (>= (float-time
                 (time-since
                  (cdr minibar--module-network-speeds-cache)))
                minibar-module-network-speeds-cache-for))
    (setq
     minibar--module-network-speeds-cache
     (cons
      (let ((previous minibar--module-network-speeds-last-byte-counts)
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
          (setq minibar--module-network-speeds-last-byte-counts
                current)
          (format "↑ %s ↓ %s"
                  (funcall formatter (car speeds))
                  (funcall formatter (cdr speeds)))))
      (current-time))))
  (car minibar--module-network-speeds-cache))

(defcustom minibar-module-cpu-cache-for 1
  "Cache CPU load values for this many seconds.  Set to zero disable."
  :type 'number)

(defvar minibar--module-cpu-cache nil
  "Cached CPU load.

The value is a cons cell whose car is the CPU load and cdr is the time
when it was recorded.")

(defvar minibar--module-cpu-last-times nil
  "Last CPU idle and total calculated.")

(defvar minibar--module-cpu-count nil
  "Count of processor or CPU.")

(defun minibar--module-cpu-calculate-load (cpu)
  "Calculate CPU load."
  (let* ((last-cell
          (let ((cell (assoc-string
                       cpu minibar--module-cpu-last-times)))
            (unless cell
              (setq minibar--module-cpu-last-times
                    (cons (cons cpu nil)
                          minibar--module-cpu-last-times))
              (setq cell (assoc-string
                          cpu minibar--module-cpu-last-times)))
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

(defun minibar-module-cpu ()
  "Module for showing CPU load."
  (when (or (not minibar--module-cpu-cache)
            (>= (float-time
                 (time-since (cdr minibar--module-cpu-cache)))
                minibar-module-cpu-cache-for))
    (setq
     minibar--module-cpu-cache
     (cons
      (with-temp-buffer
        (insert-file-contents "/proc/stat")
        (unless minibar--module-cpu-count
          (setq minibar--module-cpu-count
                (string-to-number (shell-command-to-string "nproc"))))
        (format
         "%3i%%%s"
         (minibar--module-cpu-calculate-load "cpu")
         (if (seq-some #'char-displayable-p
                       ;; Characters ▁▂▃▄▅▆▇█
                       (number-sequence #x2581 #x2588))
             (concat
              " "
              (mapconcat
               (lambda (i)
                 (let ((load (minibar--module-cpu-calculate-load
                              (format "cpu%i" i))))
                   (let ((char (seq-some
                                (lambda (e)
                                  (and (>= load (car e))
                                       (char-displayable-p (cdr e))
                                       (cdr e)))
                                '((87.5 . ?█)     ; #x2588
                                  (75 . ?▇)       ; #x2587
                                  (62.5 . ?▆)     ; #x2586
                                  (50 . ?▅)       ; #x2585
                                  (37.5 . ?▄)     ; #x2584
                                  (25  . ?▃)      ; #x2583
                                  (12.5 . ?▂))))) ; #x2582
                     (if char
                         (propertize (string char) 'face 'bold)
                       (propertize
                        (if (char-displayable-p ?▁)  ;  #x2581
                            "▁"
                          " ")
                        'face
                        '( :weight bold
                           :inherit font-lock-comment-face))))))
               (number-sequence 0 (1- minibar--module-cpu-count)) ""))
           "")))
      (current-time))))
  (car minibar--module-cpu-cache))

(provide 'minibar)
;;; minibar.el ends here
