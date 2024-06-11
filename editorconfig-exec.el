;;; editorconfig-exec.el --- Get EditorConfig info via executable  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2024 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>

;; This file is part of EditorConfig Emacs Plugin.

;; EditorConfig Emacs Plugin is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; EditorConfig Emacs Plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; EditorConfig Emacs Plugin. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-obsolete-variable-alias
  'edconf-exec-path
  'editorconfig-exec-path
  "0.5")
(defcustom editorconfig-exec-path
  "editorconfig"
  "Path to EditorConfig executable.

Used by `editorconfig--execute-editorconfig-exec'."
  :group 'editorconfig
  :type 'string)

(defun editorconfig--execute-editorconfig-exec (filename)
  "Execute EditorConfig core with FILENAME and return output."
  (if filename
      (with-temp-buffer
        (let ((remote (file-remote-p filename))
              (remote-localname (file-remote-p filename
                                               'localname)))
          (display-warning '(editorconfig editorconfig--execute-editorconfig-exec)
                           (format "editorconfig--execute-editorconfig-exec: filename: %S | remote: %S | remote-localname: %S"
                                   filename
                                   remote
                                   remote-localname)
                           :debug)
          (if remote
              (progn
                (cd (concat remote "/"))
                (setq filename remote-localname))
            (cd "/")))
        (display-warning '(editorconfig editorconfig--execute-editorconfig-exec)
                         (format "editorconfig--execute-editorconfig-exec: default-directory: %S | filename: %S"
                                 default-directory
                                 filename
                                 )
                         :debug)
        (if (eq 0
                (process-file editorconfig-exec-path nil t nil filename))
            (buffer-string)
          (editorconfig-error (buffer-string))))
    ""))

(defun editorconfig--parse-properties (props-string)
  "Create properties hash table from PROPS-STRING."
  (let ((props-list (split-string props-string "\n"))
        (properties (make-hash-table)))
    (dolist (prop props-list properties)
      (let ((key-val (split-string prop " *= *")))
        (when (> (length key-val) 1)
          (let ((key (intern (car key-val)))
                (val (mapconcat #'identity (cdr key-val) "")))
            (puthash key val properties)))))))

(defun editorconfig-get-properties-from-exec (filename)
  "Get EditorConfig properties of file FILENAME.

This function uses value of `editorconfig-exec-path' to get properties."
  (if (executable-find editorconfig-exec-path)
      (editorconfig--parse-properties (editorconfig--execute-editorconfig-exec filename))
    (editorconfig-error "Unable to find editorconfig executable")))

(defun editorconfig-get-properties (filename)
  "Get EditorConfig properties for file FILENAME.

It calls `editorconfig-get-properties-from-exec' if
`editorconfig-exec-path' is found, otherwise
`editorconfig-core-get-properties-hash'."
  (if (and (executable-find editorconfig-exec-path)
           (not (file-remote-p filename)))
      (editorconfig-get-properties-from-exec filename)
    (require 'editorconfig-core)
    (editorconfig-core-get-properties-hash filename)))

(provide 'editorconfig-exec)
;;; editorconfig-exec.el ends here.
