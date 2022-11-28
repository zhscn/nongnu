;;; testcover-mark-line.el --- Mark whole line with Testcover -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-10-11
;; Version: 0.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp utility
;; Homepage: https://codeberg.org/akib/emacs-testcover-mark-line

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

;; Testcover is a visual code-coverage tool, that marks the form not
;; completed tested.  It only highlights the last character of a form,
;; which sometimes don't attract attention and is hard to see,
;; especially when your code is heavy highlighted.  This package
;; highlights the whole line, which can easily get your attention.

;; Enable with M-x testcover-mark-line-mode, and you're done.

;;; Code:

(require 'cl-lib)
(require 'testcover)

(defgroup testcover-mark-line nil
  "Mark whole line with Testcover."
  :group 'testcover
  :link '(url-link
          "https://codeberg.org/akib/emacs-testcover-mark-line")
  :prefix "testcover-mark-line-")

(defface testcover-mark-line-nohits
  '((t :extend t :inherit testcover-nohits))
  "Face for lines with forms that had no hits.")

(defface testcover-mark-line-1value
  '((t :extend t :inherit testcover-1value))
  "Face for line with forms that always produced the same value.")

(defun testcover-mark-line--mark-advice (fn &rest args)
  "Call FN with ARGS with appropiate wrapper."
  (let ((ovs nil)
        (curr-ov nil))
    (prog1
        (cl-letf* ((make-overlay (symbol-function #'make-overlay))
                   (overlay-put (symbol-function #'overlay-put))
                   ((symbol-function #'make-overlay)
                    (lambda (beg end &optional buffer &rest args)
                      (when (= (- end beg) 1)
                        (setq curr-ov
                              (copy-sequence
                               `( ,beg ,end
                                  ,(or buffer (current-buffer))
                                  nil))))
                      (apply make-overlay beg end buffer args)))
                   ((symbol-function #'overlay-put)
                    (lambda (ov prop val &rest args)
                      (when (and curr-ov (eq prop 'face))
                        (setf (nth 3 curr-ov) val)
                        (push curr-ov ovs)
                        (setq curr-ov nil))
                      (apply overlay-put ov prop val args))))
          (apply fn args))
      (let ((lines nil))
        (dolist (ov ovs)
          (with-current-buffer (nth 2 ov)
            (let* ((line (cons (nth 2 ov)
                               (save-excursion
                                 (goto-char (nth 0 ov))
                                 (line-beginning-position))))
                   (face
                    (if (eq (nth 3 ov) 'testcover-nohits)
                        (nth 3 ov)
                      (let ((prev (alist-get line lines)))
                        (if (and (eq (nth 3 ov) 'testcover-1value)
                                 (not (eq prev 'testcover-nohits)))
                            (nth 3 ov)
                          prev)))))
              (setf (alist-get line lines) face))))
        (dolist (line lines)
          (with-current-buffer (caar line)
            (save-excursion
              (goto-char (cdar line))
              (let ((ov (make-overlay
                         (point) (line-beginning-position 2))))
                (overlay-put ov 'face
                             (pcase (cdr line)
                               ('testcover-nohits
                                'testcover-mark-line-nohits)
                               ('testcover-1value
                                'testcover-mark-line-1value)
                               (face face)))))))))))

;;;###autoload
(defun testcover-mark-line-mark-all ()
  "Mark all forms that didn't get completely tested, with lines."
  (declare (interactive-only t))
  (interactive)
  (cl-letf* (((symbol-function #'testcover-mark)
              (apply-partially #'testcover-mark-line--mark-advice
                               (symbol-function #'testcover-mark))))
    (call-interactively #'testcover-mark-all)))

;;;###autoload
(define-minor-mode testcover-mark-line-mode
  "Minor mode to mark whole line with testcover."
  :global t
  (if testcover-mark-line-mode
      (advice-add #'testcover-mark :around
                  #'testcover-mark-line--mark-advice)
    (advice-remove #'testcover-mark
                   #'testcover-mark-line--mark-advice)))

(provide 'testcover-mark-line)
;;; testcover-mark-line.el ends here
