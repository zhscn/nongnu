;;; editorconfig.el --- Tests editorconfig  -*- lexical-binding: t -*-

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

;; Tests editorconfig

;;; Code:

(require 'editorconfig "../editorconfig")

(set-variable 'vc-handled-backends nil)

(defun display-warning (type message &optional level buffer-name)
  "When testing overwrite this function to throw error when called."
  (unless (eq level :debug)
    (error "display-warning called: %S %S %S %S"
           type
           message
           level
           buffer-name)))

(defmacro with-visit-file (path &rest body)
  "Visit PATH and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((buf (find-file-noselect ,path)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (kill-buffer buf))))

;;; interactive

(ert-deftest interactive-test-01 nil
  "This test should not run on Travis"
  :tags '(:interactive)
  (should t))

;;; noninteractive, will run on Travis

(ert-deftest has-feature-01 nil
  "minimally working - provides 'editorconfig"
  (should (featurep 'editorconfig)))

(defvar editorconfig-ert-dir
  (expand-file-name "plugin-tests/test_files/"
                    (file-name-directory (macroexp-file-name))))

(defvar editorconfig-secondary-ert-dir
  (expand-file-name "test_files_secondary/"
                    (file-name-directory (macroexp-file-name))))

(defvar editorconfig-local-variables-ert-dir
  (expand-file-name "local_variables/"
                    (file-name-directory (macroexp-file-name))))

(ert-deftest test-editorconfig nil
  "Check if properties are applied."
  (editorconfig-mode 1)

  (with-visit-file (expand-file-name "3_space.txt" editorconfig-ert-dir)
    (should (eq tab-width 3))
    (should (eq indent-tabs-mode nil)))

  (with-visit-file (expand-file-name "4_space.py" editorconfig-ert-dir)
    (should (eq python-indent-offset 4))
    (should (eq tab-width 8))
    (should (eq indent-tabs-mode nil)))
  (editorconfig-mode -1))

(ert-deftest test-lisp-use-default-indent nil
  (editorconfig-mode 1)

  (with-visit-file (expand-file-name "2_space.el"
                                     editorconfig-secondary-ert-dir)
    (should (eq lisp-indent-offset 2)))

  (let ((editorconfig-lisp-use-default-indent t))
    (with-visit-file (expand-file-name "2_space.el"
                                       editorconfig-secondary-ert-dir)
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 2))
    (with-visit-file (expand-file-name "2_space.el"
                                       editorconfig-secondary-ert-dir)
      (should (eq lisp-indent-offset nil))))

  (let ((editorconfig-lisp-use-default-indent 4))
    (with-visit-file (expand-file-name "2_space.el"
                                       editorconfig-secondary-ert-dir)
      (should (eq lisp-indent-offset 2))))
  (editorconfig-mode -1))

(ert-deftest test-trim-trailing-ws nil
  (editorconfig-mode 1)
  (with-visit-file (expand-file-name "trim.txt" editorconfig-ert-dir)
    (should (memq 'delete-trailing-whitespace
                  before-save-hook)))
  (with-visit-file (expand-file-name "trim.txt" editorconfig-ert-dir)
    (read-only-mode 1)
    (should (not (memq 'delete-trailing-whitespace
                       before-save-hook))))
  (editorconfig-mode -1))

(ert-deftest test-charset nil
  (editorconfig-mode 1)
  (with-visit-file (expand-file-name "latin1.txt" editorconfig-ert-dir)
    (set-buffer-file-coding-system 'undecided-unix)
    (should (eq buffer-file-coding-system
                'iso-latin-1-unix)))
  (with-visit-file (expand-file-name "utf-16be.txt" editorconfig-ert-dir)
    (set-buffer-file-coding-system 'undecided-unix)
    (should (eq buffer-file-coding-system
                'utf-16be-with-signature-unix)))
  (editorconfig-mode -1))


(ert-deftest test-local-variables nil
  (editorconfig-mode 1)
  (with-visit-file (expand-file-name "file_locals.rb"
                                     editorconfig-local-variables-ert-dir)
    (should (eq tab-width 9))
    (should (eq ruby-indent-level 7)))

  (with-visit-file (expand-file-name "dir_locals.c"
                                     editorconfig-local-variables-ert-dir)
    (should (eq tab-width 9))
    (should (eq c-basic-offset 7)))

  (let ((editorconfig-override-file-local-variables nil))
    (with-visit-file (expand-file-name "file_locals.rb"
                                       editorconfig-local-variables-ert-dir)
      (should (eq tab-width 5))
      (should (eq ruby-indent-level 3))))

  (let ((editorconfig-override-dir-local-variables nil))
    (with-visit-file (expand-file-name "dir_locals.c"
                                       editorconfig-local-variables-ert-dir)
      (should (eq tab-width 5))
      (should (eq c-basic-offset 3))))
  (editorconfig-mode -1))

(ert-deftest test-file-type-emacs nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (expand-file-name "c.txt" editorconfig-secondary-ert-dir)
    (should (eq major-mode 'conf-unix-mode)))
  (editorconfig-mode -1))

(ert-deftest test-file-type-ext nil
  :expected-result t  ;; Ignore failure
  (editorconfig-mode 1)
  (with-visit-file (expand-file-name "a.txt" editorconfig-secondary-ert-dir)
    (should (eq major-mode 'conf-unix-mode)))

  (with-visit-file (expand-file-name "bin/perlscript"
                                     editorconfig-secondary-ert-dir)
    (should (eq major-mode 'perl-mode))
    (should (eq perl-indent-level 5)))
  (editorconfig-mode -1))

(ert-deftest test-hack-properties-functions nil
  (editorconfig-mode 1)
  (add-hook 'editorconfig-hack-properties-functions
            (lambda (props)
              (puthash 'indent_size "5" props)))
  (with-visit-file (expand-file-name "4_space.py" editorconfig-ert-dir)
    (should (eq python-indent-offset 5)))
  (setq editorconfig-hack-properties-functions nil)
  (editorconfig-mode -1))

;;; editorconfig.el ends here
