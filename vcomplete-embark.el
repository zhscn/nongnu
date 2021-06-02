;;; vcomplete-embark.el --- Embark integration for Vcomplete -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Daniel Semyonov

;; Author: Daniel Semyonov <daniel@dsemy.com>
;; Maintainer: Daniel Semyonov <daniel@dsemy.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, matching
;; URL: https://dsemy.com/software/vcomplete

;; This file is not part of GNU Emacs.

;; Vcomplete is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Vcomplete is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Embark integration for Vcomplete.
;; This integration is not tested regularly, and might have issues.

;;; Code:

(require 'vcomplete)
(require 'embark)

(declare-function embark-target-completion-at-point "ext:embark")
(declare-function embark-completions-buffer-candidates "ext:embark")

(defun vcomplete-embark--eliminate-delay (fun &rest args)
  "Call FUN with ARGS and `minibuffer-message-timeout' locally set to `0'."
  (let ((minibuffer-message-timeout 0))
    (apply fun args)))

(defun vcomplete-embark--advise-commands ()
  "Advise Embark commands with `vcomplete-embark--eliminate-delay'."
  (dolist (cmd '(embark-act embark-default-action))
    (if vcomplete-mode
        (advice-add cmd :around #'vcomplete-embark--eliminate-delay)
      (advice-remove cmd #'vcomplete-embark--eliminate-delay))))

(add-hook 'vcomplete-mode-hook
          #'vcomplete-embark--advise-commands)

;; In case `vcomplete-mode' is enabled already.
(vcomplete-embark--advise-commands)

(defun vcomplete-embark-current-completion (&optional relative)
  "Call `embark-target-completion-at-point' in the `*Completions*' buffer.
If the completions are file names and RELATIVE is non-nil, return
relative path."
  (when (and vcomplete-mode
             (or (minibufferp) completion-in-region-mode))
    (vcomplete-with-completions-buffer
      (embark-target-completion-at-point relative))))

(defun vcomplete-embark-collect-candidates ()
  "Call `embark-completions-buffer-candidates' in the `*Completions*' buffer.
If the completions are file names and RELATIVE is non-nil, return
relative path."
  (when (and vcomplete-mode
             (or (minibufferp) completion-in-region-mode))
    (vcomplete-with-completions-buffer
      (embark-completions-buffer-candidates))))

(add-hook 'embark-target-finders
          #'vcomplete-embark-current-completion)
(add-hook 'embark-candidate-collectors
          #'vcomplete-embark-collect-candidates)

(provide 'vcomplete-embark)

;;; vcomplete-embark.el ends here
