;;; camera.el --- Take picture with your camera -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-07-16
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm
;; Homepage: https://codeberg.org/akib/emacs-camera

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

;; This package allows you to take photos from Emacs.

;; Open camera with M-x camera and take photos with SPC or
;; M-x camera-capture.

;;; Code:

(defgroup camera nil
  "Take picture with your camera."
  :group 'comm
  :link '(url-link "https://codeberg.org/akib/emacs-camera")
  :prefix "camera-")

(defcustom camera-save-directory "~/Pictures"
  "Directory where to store captured pictures."
  :type 'directory)

(defcustom camera-picture-save-file-name-format
  "emacs-camera-%Y%m%d%H%M%S%3N"
  "Format string for saving pictures.

This string is passed to `format-time-string' with capture time to get the
base file name (file name without extension), and the file name extension
is added to it.

The value can also be a function of no argument returning the base file
name."
  :type '(choice string function))

(defcustom camera-framerate 40
  "Frame rate of camera (per second)."
  :type 'number)

(defcustom camera-capture-frame-function #'camera-capture-frame-ffmpeg
  "Function to capture a frame.

It should return an image of the frame and try to make the size of the
frame SIZE."
  :type 'function
  :options (list #'camera-capture-frame-ffmpeg))

(defcustom camera-ffmpeg-video-device "/dev/video0"
  "Device to use as camera."
  :type 'file)

(defvar-local camera--current-frame nil
  "Current frame shown on buffer.")

(defvar-local camera--update-frame-timer nil
  "Timer to update frame shown on buffer.")

(defun camera--get-text-area-size ()
  "Return the size of text area in the selected window."
  (cons (* (window-max-chars-per-line) (frame-char-width))
        (* (window-text-height) (frame-char-height))))

(defun camera--update-frame (buffer)
  "Update frame shown on buffer.

Don't do anything if current buffer is not BUFFER."
  (when (eq buffer (current-buffer))
    (let* ((inhibit-read-only t)
           (size (camera--get-text-area-size))
           (image (funcall camera-capture-frame-function size)))
      (erase-buffer)
      (insert-image image "[frame]")
      (setq camera--current-frame image))
    (when camera--update-frame-timer
      (cancel-timer camera--update-frame-timer))
    (setq camera--update-frame-timer
          (run-with-idle-timer (+ (time-to-seconds (current-idle-time))
                                  (/ (float camera-framerate)))
                               nil #'camera--update-frame
                               (current-buffer)))))

(defun camera--pre-command ()
  "Cancel timer to update frame shown on buffer."
  (when camera--update-frame-timer
    (cancel-timer camera--update-frame-timer)
    (setq camera--update-frame-timer nil)))

(defun camera--post-command ()
  "Start timer to update frame shown on buffer."
  (when camera--update-frame-timer
    (cancel-timer camera--update-frame-timer))
  (setq camera--update-frame-timer
        (run-with-idle-timer (/ (float camera-framerate)) nil
                             #'camera--update-frame (current-buffer))))

(defun camera-capture ()
  "Capture a picture."
  (interactive)
  (let* ((image camera--current-frame)
         (file-name
          (expand-file-name
           (concat
            (if (functionp camera-picture-save-file-name-format)
                (funcall camera-picture-save-file-name-format)
              (format-time-string camera-picture-save-file-name-format))
            "."
            (symbol-name (plist-get (cdr image) :type)))
           camera-save-directory)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (plist-get (cdr image) :data))
      (write-region (point-min) (point-max) file-name nil t))))

(defvar camera-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'camera-capture)
    map)
  "Keymap for Camera mode.")

(define-derived-mode camera-mode special-mode "Camera"
  "Major mode for using camera."
  (setq-local buffer-read-only t)
  (setq-local cursor-type nil)
  (add-hook 'pre-command-hook #'camera--pre-command nil t)
  (add-hook 'post-command-hook #'camera--post-command nil t))

(defun camera ()
  "Open camera."
  (interactive)
  (with-current-buffer (generate-new-buffer "*Camera*")
    (camera-mode)
    (display-buffer (current-buffer))
    (camera--update-frame (current-buffer))))

(defun camera-capture-frame-ffmpeg (size)
  "Capture a frame from the camera with `ffmpeg'.

Return an image of the frame.  Try to make the size of the frame SIZE."
  (let ((temp-file (make-temp-file "emacs-camera-ffmpeg-" nil ".jpeg")))
    (unwind-protect
        ;; TODO: Refractor.
        (with-temp-buffer

          ;; `ffmpeg' complains when file exists.
          (when (file-exists-p temp-file)
            (delete-file temp-file))
          (let ((process (start-file-process
                          "camera-ffmpeg" (current-buffer)
                          "ffmpeg" "-f" "video4linux2" "-s"
                          (format "%ix%i" (car size) (cdr size))
                          "-i" camera-ffmpeg-video-device "-frames" "1"
                          temp-file)))
            (while (process-live-p process)
              (sleep-for 0.05)))
          (erase-buffer)
          (set-buffer-multibyte nil)
          (insert-file-contents-literally temp-file)
          (create-image (buffer-string) 'jpeg t))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'camera)
;;; camera.el ends here
