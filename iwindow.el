;;; iwindow.el --- Interactively manipulate windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-07-31
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: frames
;; URL: https://codeberg.org/akib/emacs-iwindow

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

;; The default command for switching window in `other-window', which
;; is great for two windows, but unpredicatable and cumbersome when
;; there are more windows.  This package aims to solve that, by
;; allowing to select any window with a few keystrokes.

;; Usage
;; -----

;; Use `iwindow-select' to select window.  It is recommended to bind
;; it to somewhere, for example:

;;     (global-set-key (kbd "C-x o") #'iwindow-select)

;;; Code:

(require 'cl-lib)

(defgroup iwindow nil
  "Interactively manipulate windows."
  :group 'frames
  :link '(url-link "https://codeberg.org/akib/emacs-iwindow")
  :prefix "iwindow-")

(defcustom iwindow-selection-keys
  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "List of keys to use to select window.

Each element should be a key that `read-key' can return."
  :type '(repeat string))

(defcustom iwindow-decoration-functions
  (list #'iwindow-show-keys-on-mode-line
        #'iwindow-highlight-window)
  "Hook run to decorate candidate window.

For example, a function may modify mode line on each window to show
the key sequence to choose it, or change candidate window's
background.

Each function is run with a alist WINDOWS and a function CALLBACK.
Each element ALIST is of form (WINDOW . KEYS), where WINDOW is a
window and KEYS is the list of keys to press to choose WINDOW.  The
function should make any changes (e.g. let-bind a variable), call
CALLBACK and revert the changes it has done."
  :type 'hook
  :options (list #'iwindow-show-keys-on-mode-line
                 #'iwindow-highlight-window))

(defcustom iwindow-highlight-faces
  '((default . iwindow-highlight-default)
    (fringe . iwindow-highlight-fringe))
  "Alist of face and their replacements in candidate windows."
  :type '(alist :key-type (face :tag "Face")
                :value-type (face :tag "Replacement")))

(defface iwindow-highlight-default '((t :background "blue"))
  "`default' face for highlighted windows.")

(defface iwindow-highlight-fringe '((t :background "blue"))
  "`fringe' face for highlighted windows.")

(defun iwindow--make-decision-tree (windows start end)
  "Make a decision tree from window in WINDOWS from START to END.

Return an object OPTION, where OPTION is either a window, or a list of
form (OPTION...), whose length of no more than the length of
`iwindow-selection-keys'."
  (if (eql (- end start) 1)
      (aref windows start)
    (let ((result nil)
          (option-count (length iwindow-selection-keys)))
      (dotimes (i option-count)
        (let* ((max-descendent-option-count
                (ceiling (/ (float (- end start)) option-count)))
               (s (min (+ start (* i max-descendent-option-count))
                       end))
               (e (min (+ s max-descendent-option-count) end)))
          (unless (eql s e)
            (push (iwindow--make-decision-tree windows s e) result))))
      (nreverse result))))

(defun iwindow--decorate-windows (tree payload)
  "Setup candidate windows in TREE and call PAYLOAD."
  (let ((current-window (selected-window))
        (windows nil)
        (decorators nil))
    (cl-labels ((walk
                 (tree keys)
                 (if (windowp tree)
                     (push (cons tree (reverse keys))
                           windows)
                   (seq-map-indexed
                    (lambda (node index)
                      (walk node
                            (cons (nth index iwindow-selection-keys)
                                  keys)))
                    tree))))
      (walk tree nil))
    (run-hook-wrapped 'iwindow-decoration-functions
                      (lambda (fn) (ignore (push fn decorators))))
    (cl-labels ((call-decorators
                 (fns)
                 (with-selected-window current-window
                   (if fns
                       (funcall (car fns) windows
                                (lambda ()
                                  (call-decorators (cdr fns))))
                     (funcall payload)))))
      (call-decorators (nreverse decorators)))))

(defun iwindow--ask (tree)
  "Given decision tree TREE, ask user for the decision.

Return the window chosen."
  (if (windowp tree)
      (prog1
          tree
        (redraw-display))
    (let ((option nil)
          (choices (cl-mapcar #'cons iwindow-selection-keys
                                     tree)))
      (iwindow--decorate-windows
       tree
       (lambda ()
         (redraw-display)
         (while (not option)
           (let ((key (read-key)))
             (if (eql key ?\C-g)
                 (keyboard-quit)
               (if-let ((choice (alist-get key choices)))
                   (setq option choice)
                 (message "Unbound key: %c (press C-g to quit)" key)
                 (ding)))))))
      (iwindow--ask option))))

;;;###autoload
(defun iwindow-choose (&optional predicate)
  "Interactively choose a window.  Return the window chosen by user.

When PREDICATE is given, for each window WINDOW call PREDICATE with
WINDOW and ignore WINDOW when PREDICATE returns nil."
  (when (< (safe-length iwindow-selection-keys) 2)
    (user-error
     "Atleast two keys needed to select window, please customize `%S'"
     'iwindow-selection-characters))
  (let ((windows (if predicate
                     (seq-filter predicate (window-list))
                   (window-list))))
    (when windows
      (if (cdr windows)                 ; (> (length windows) 1)
          (let* ((tree (iwindow--make-decision-tree
                        (vconcat windows) 0 (length windows))))
            (iwindow--ask tree))
        (car windows)))))

(defun iwindow-show-keys-on-mode-line (windows callback)
  "Change mode line of windows to show the keys to choose the window.

WINDOWS and CALLBACK is described in the docstring of
`iwindow-decoration-functions', which see."
  (let ((original-mode-lines nil))
    (cl-labels ((setup-windows
                 (window-list)
                 (with-selected-window (caar window-list)
                   (unless (assq (current-buffer) original-mode-lines)
                     (push (cons (current-buffer) mode-line-format)
                           original-mode-lines))
                   (let ((mode-line-format
                          `(:eval
                            (if-let ((keys
                                      (alist-get (selected-window)
                                                 ',windows)))
                                (mapconcat
                                 (apply-partially #'string ? )
                                 keys "")
                              ',(alist-get (current-buffer)
                                           original-mode-lines)))))
                     (if (cdr window-list)
                         (setup-windows (cdr window-list))
                       (funcall callback))))))
      (setup-windows windows))))

(defun iwindow-highlight-window (windows callback)
  "Highlight all candidate windows.

WINDOWS and CALLBACK is described in the docstring of
`iwindow-decoration-functions', which see."
  (let ((buffers nil)
        (sym (make-symbol "iwindow-parameter")))
    (cl-labels ((setup-windows
                 (window-list)
                 (with-selected-window (caar window-list)
                   (cl-letf* (((window-parameter nil sym) sym)
                              (face-remapping-alist
                               face-remapping-alist))
                     (unless (memq (current-buffer) buffers)
                       (dolist (pair iwindow-highlight-faces)
                         (face-remap-add-relative
                          (car pair)
                          `(:filtered (:window ,sym ,sym)
                                      ,(cdr pair)))))
                     (push (current-buffer) buffers)
                     (if (cdr window-list)
                         (setup-windows (cdr window-list))
                       (funcall callback))))))
      (setup-windows windows))))

;;;###autoload
(defun iwindow-select ()
  "Interactively select a window."
  (interactive)
  (when-let ((window (iwindow-choose
                      (lambda (window)
                        (not (eq window (selected-window)))))))
    (select-window window)))

;;;###autoload
(defun iwindow-swap ()
  "Swap buffers of current window and WINDOW."
  (interactive)
  (when-let ((window (iwindow-choose
                      (lambda (window)
                        (not (eq window (selected-window)))))))
    (unless (eq (window-frame window) (selected-frame))
      (select-frame-set-input-focus (window-frame window)))
    (let ((current-buffer (window-buffer (selected-window))))
      (set-window-buffer (selected-window) (window-buffer window))
      (set-window-buffer window current-buffer)
      (select-window window))))

(provide 'iwindow)
;;; iwindow.el ends here
