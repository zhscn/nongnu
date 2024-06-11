;;; editorconfig-core-handle.el --- Tests editorconfig-core-handle  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2022 EditorConfig Team

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

;; Tests editorconfig-core-handle

;;; Code:

(require 'editorconfig-core-handle "../editorconfig-core-handle")

(defconst editorconfig--fixtures (expand-file-name "fixtures/"
                                   (file-name-directory load-file-name))
  "Path to fixtures.")

(set-variable 'vc-handled-backends nil)

(ert-deftest test-editorconfig-core-handle ()
  ;; handle.ini
  (let* ((conf (expand-file-name "handle.ini" editorconfig--fixtures))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "b.js" editorconfig--fixtures))
                   '((("key2" . "value2")))))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "a.js" editorconfig--fixtures))
                   '((("key1" . "value1")) (("key2" . "value2"))))))
  ;; Test twice for checking cache
  (let* ((conf (expand-file-name "handle.ini" editorconfig--fixtures))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "b.js" editorconfig--fixtures))
                   '((("key2" . "value2")))))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "a.js" editorconfig--fixtures))
                   '((("key1" . "value1")) (("key2" . "value2"))))))

  ;; handle2.ini
  (let* ((conf (expand-file-name "handle2.ini" editorconfig--fixtures))
         (handle (editorconfig-core-handle conf)))
    (should-not (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "b.js" editorconfig--fixtures))
                   nil))
    (should (equal (editorconfig-core-handle-get-properties
                    handle (expand-file-name "a.js" editorconfig--fixtures))
                   '((("key" . "value"))))))

  ;; For checking various normal whitespace (line breaks, horizontal space, vertical space, etc.)
  (let* ((conf (concat default-directory
                       "ert-tests/whitespaces/example-editorconfig.txt"))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-p handle)))
  )

;;; editorconfig-core-handle.el ends here
