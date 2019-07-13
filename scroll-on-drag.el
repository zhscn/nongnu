;;; scroll-on-drag.el --- Interactive scrolling. -*- lexical-binding: t -*-

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://github.com/ideasman42/emacs-scroll-on-drag
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interactive scrolling which can be cancelled by pressing escape.

;;; Usage

;; (scroll-on-drag) ; Interactively scroll the current buffer
;;

;;; Code:

(defcustom scroll-on-drag-style 'line-by-pixel
  "The method of scrolling."
  :group 'scroll-on-drag
  :type
  '
  (choice
    (const :tag "Line" line)
    (const :tag "Pixel" pixel)
    (const :tag "Line-By-Pixel" line-by-pixel)))

(defcustom scroll-on-drag-motion-style 'scroll-with-cursor
  "The method of scrolling."
  :group 'scroll-on-drag
  :type
  '
  (choice
    (const :tag "Scroll With Cursor" scroll-with-cursor)
    (const :tag "Scroll" scroll)
    (const :tag "Cursor" cursor)))

(defcustom scroll-on-drag-delay 0.01
  "Idle time between scroll updates."
  :group 'scroll-on-drag
  :type  'float)

(defcustom scroll-on-drag-motion-scale 0.1
  "Scroll speed multiplier."
  :group 'scroll-on-drag
  :type  'float)

(defcustom scroll-on-drag-motion-power 2.0
  "Non-linear scroll power (1.0 for linear speed, 4.0 for fast acceleration)."
  :group 'scroll-on-drag
  :type  'float)

(defun scroll-on-drag-internal ()
  "Main scrolling function."
  (let*
    (
      ;; Don't run unnecessary logic when scrolling.
      (inhibit-point-motion-hooks t)

      (has-scrolled nil)
      (scroll-timer nil)
      ;; Cursor offset
      (delta 0)
      (delta-prev 0)

      ;; Only for 'line-by-pixel' style.
      (delta-px-accum 0)

      ;; Avoid calling everywhere.
      (this-window (selected-window))

      ;; Restoration position.
      (restore-window-start (window-start))
      (restore-point (point))

      (mouse-y-fn
        (cond
          ((eq scroll-on-drag-style 'line)
            (lambda () (cdr (cdr (mouse-position)))))
          (t
            (lambda () (cdr (cdr (mouse-pixel-position)))))))

      ;; Reference to compare all mouse motion to.
      (y-init (funcall mouse-y-fn))

      (mouse-y-delta-scale-fn
        ;; '(f * scale) ^ power', then truncate to int.
        (lambda (delta)
          (let ((f (float delta)))
            (truncate
              (copysign (expt (* (abs f) scroll-on-drag-motion-scale) scroll-on-drag-motion-power) f)))))

      ;; Scroll wrapper, uses line sign.
      (scroll-by-lines-scroll-with-cursor-offset-lines
        (if (eq scroll-on-drag-motion-style 'scroll-with-cursor)
          ;; Will be bellow scroll margin when the cursor is at the top.
          (max scroll-margin (count-lines (window-start) (point)))
          0))

      (scroll-by-lines-scroll-with-cursor-offset-lines-min
        (if (eq scroll-on-drag-motion-style 'scroll-with-cursor)
          ;; Will be bellow scroll margin when the cursor is at the top.
          (save-excursion
            (goto-char (point-min))
            (forward-line scroll-by-lines-scroll-with-cursor-offset-lines)
            (point))
          0))

      (scroll-by-lines-scroll-with-cursor-offset-lines-max
        (if (eq scroll-on-drag-motion-style 'scroll-with-cursor)
          ;; Will be bellow scroll margin when the cursor is at the top.
          (save-excursion
            (goto-char (point-max))
            (forward-line (- scroll-by-lines-scroll-with-cursor-offset-lines))
            (point))
          0))

      (scroll-by-lines-fn
        (cond

          ((eq scroll-on-drag-motion-style 'scroll)
            ;; -------------------------
            ;; Scroll: Regular Scrolling

            (lambda (lines)
              (if (< lines 0)
                (condition-case nil
                  (scroll-down (- lines))
                  (beginning-of-buffer nil))
                (condition-case nil
                  (scroll-up lines)
                  (end-of-buffer
                    (let ((lines (- (count-lines (window-start) (point-max)) 1)))
                      (when (> lines 0) (scroll-up lines))))))))


          ((eq scroll-on-drag-motion-style 'cursor)
            ;; --------------
            ;; Scroll: Cursor

            (lambda (lines)
              (forward-line lines)))

          ((eq scroll-on-drag-motion-style 'scroll-with-cursor)
            ;; ---------------------------------
            ;; Scroll: Scroll with Cursor Motion

            (lambda (lines)
              (forward-line lines)
              (when (< lines 0)
                (when (< (point) scroll-by-lines-scroll-with-cursor-offset-lines-min)
                  (goto-char scroll-by-lines-scroll-with-cursor-offset-lines-min)))
              (set-window-start
                this-window
                (save-excursion
                  (forward-line (- scroll-by-lines-scroll-with-cursor-offset-lines))
                  (point))
                t)))))

      ;; Per-pixel Scroll Up
      (scroll-up-by-pixels-fn
        (lambda (delta)
          (when (< delta 0) (error "Can't scroll by negative numbers"))
          (let*
            (
              (char-height (frame-char-height))
              (scroll-px-prev (window-vscroll nil t))
              (scroll-px-next (+ scroll-px-prev delta))
              (scroll-px (mod scroll-px-next char-height))
              (lines (/ scroll-px-next char-height)))
            (if (eq lines 0)
              (set-window-vscroll nil scroll-px t)
              (if (ignore-errors (scroll-up lines))
                (set-window-vscroll nil scroll-px t)
                (set-window-vscroll nil 0 t))))))

      ;; Per-pixel Scroll Down
      ;; TODO: flickers (needs redisplay, seems to be a bug/limit in emacs).
      (scroll-down-by-pixels-fn
        (lambda (delta)
          (when (< delta 0) (error "Can't scroll by negative numbers"))
          (let*
            (
              (char-height (frame-char-height))
              (scroll-px-prev (window-vscroll nil t))
              (scroll-px-next (- scroll-px-prev delta))
              (scroll-px (mod scroll-px-next char-height))
              (lines (+ (/ (* -1 scroll-px-next) char-height) (if (< scroll-px-next 0) 1 0))))
            (if (eq lines 0)
              (set-window-vscroll nil scroll-px t)
              (if (ignore-errors (scroll-down lines))
                (set-window-vscroll nil scroll-px t)
                (set-window-vscroll nil 0 t))))))

      ;; Calls 'timer-update-fn'.
      (timer-start-fn
        (lambda (timer-update-fn)
          (setq scroll-timer
            (run-with-timer
              scroll-on-drag-delay
              nil
              #'(lambda () (funcall timer-update-fn timer-update-fn))))))

      ;; Stops calling 'timer-update-fn'.
      (timer-stop-fn
        (lambda ()
          (when scroll-timer
            (cancel-timer scroll-timer)
            (setq scroll-timer nil))))

      (timer-update-fn
        (cond

          ((eq scroll-on-drag-style 'line)
            ;; -------------
            ;; Style: "line"

            (lambda (self-fn)
              (funcall scroll-by-lines-fn lines)
              (funcall timer-start-fn self-fn)))

          ((eq scroll-on-drag-style 'line-by-pixel)
            ;; ----------------------
            ;; Style: "line-by-pixel"

            (lambda (self-fn)
              (let
                (
                  (char-height (frame-char-height))
                  (delta-scaled (funcall mouse-y-delta-scale-fn delta)))
                (setq delta-px-accum
                  (+ delta-scaled delta-px-accum))
                (let ((lines (/ delta-px-accum char-height)))
                  (unless (eq lines 0)
                    (setq delta-px-accum
                      (- delta-px-accum (* lines char-height)))
                    (funcall scroll-by-lines-fn lines))))
              (funcall timer-start-fn self-fn)))

          ((eq scroll-on-drag-style 'pixel)
            ;; --------------
            ;; Style: "pixel"

            (lambda (self-fn)
              (let ((delta-scaled (funcall mouse-y-delta-scale-fn delta)))
                (if (< delta-scaled 0)
                  (funcall scroll-down-by-pixels-fn (- delta-scaled))
                  (funcall scroll-up-by-pixels-fn delta-scaled))
                (funcall timer-start-fn self-fn))))))

      (scroll-reset-fn
        (lambda ()
          (funcall timer-stop-fn)
          (setq delta-prev 0)
          (setq y-init (funcall mouse-y-fn))))

      (scroll-restore-fn
        (lambda ()
          (goto-char restore-point)
          (set-window-start this-window restore-window-start t))))

    ;; ---------------
    ;; Main Event Loop

    (track-mouse
      (while
        (let ((event (read-event)))
          (cond
            ;; Escape restores initial state, restarts scrolling.
            ((eq event 'escape)
              (funcall scroll-reset-fn)
              (funcall scroll-restore-fn)
              t)

            ;; Space keeps current position, restarts scrolling.
            ((eq event ?\s)
              (funcall scroll-reset-fn)
              t)
            ((mouse-movement-p event)
              (setq delta (- (funcall mouse-y-fn) y-init))
              (if (eq delta 0)
                (funcall timer-stop-fn)
                (when (eq delta-prev 0)
                  (setq has-scrolled t)
                  (funcall timer-stop-fn)
                  (funcall timer-update-fn timer-update-fn)))
              (setq delta-prev delta)
              t)
            ;; Cancel...
            (t nil))))

      (when (eq scroll-on-drag-style 'pixel)
        (set-window-vscroll nil 0 t)))

    (funcall timer-stop-fn)

    ;; Result so we know if any scrolling occurred,
    ;; allowing a fallback action on 'click'.
    has-scrolled))

;;;###autoload
(defun scroll-on-drag ()
  "Interactively scroll (typically on click event).
Returns true when scrolling took place, otherwise nil."
  (interactive)
  (scroll-on-drag-internal))

;;;###autoload
(defmacro scroll-on-drag-with-fallback (&rest body)
  "A macro to scroll and perform a different action on click.
Optional argument BODY Hello."
  `(lambda () (interactive) (unless (scroll-on-drag-internal) ,@body)))

(provide 'scroll-on-drag)

;;; scroll-on-drag.el ends here
