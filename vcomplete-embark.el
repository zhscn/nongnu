;;; vcomplete-embark.el --- Embark integration for Vcomplete -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Daniel Semyonov

;; Author: Daniel Semyonov <daniel@dsemy.com>
;; Maintainer: Daniel Semyonov <daniel@dsemy.com>
;; Version: 1.2
;; Package-Requires: ((emacs "25.1") (embark "0.10"))
;; Keywords: convenience, matching
;; URL: https://dsemy.com/projects/vcomplete

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
;;
;; Usage:
;;
;; Enable `vcomplete-embark-mode':
;;
;; (vcomplete-embark-mode)
;;
;; For use-package users:
;;
;; (use-package vcomplete
;;   :config
;;   (vcomplete-mode)
;;   (vcomplete-embark-mode))
;;
;; When `vcomplete-embark-mode' is active:
;; - Embark commands (like `embark-act') will operate on the
;;   highlighted completion in the completion list buffer (when
;;   available).
;;
;; For more information see the (Vcomplete) and (Embark) info nodes.

;;; Code:

(eval-when-compile
  (require 'vcomplete))

(declare-function embark-target-completion-at-point "ext:embark")

(defun vcomplete-embark--reset-auto-update ()
  "Re-enable `vcomplete-auto-update'."
  (setq vcomplete-auto-update t)
  (remove-hook 'embark-post-action-hook #'vcomplete-embark--reset-auto-update))

(defun vcomplete-embark--inhibit-auto-update (fun &rest args)
  "Inhibit `vcomplete-auto-update' while performing Embark actions.
When `vcomplete-auto-update' is nil, just call FUN with ARGS."
  (if (not vcomplete-auto-update)
      (apply fun args)
    (setq vcomplete-auto-update nil)
    ;; This hook is needed because the last part of this function will not
    ;; run if an Embark action has been performed (marked below).
    (add-hook 'embark-post-action-hook #'vcomplete-embark--reset-auto-update)
    (apply fun args)
    ;; If an action has been performed this part will not run.
    (vcomplete-embark--reset-auto-update)))

(defun vcomplete-embark-current-completion (&optional relative)
  "Call `embark-target-completion-at-point' in the `*Completions*' buffer.
If the completions are file names and RELATIVE is non-nil, return
relative path."
  (when (and vcomplete-mode
             (or (minibufferp) completion-in-region-mode))
    (vcomplete-with-completions-buffer
     (embark-target-completion-at-point relative))))

;;;###autoload
(define-minor-mode vcomplete-embark-mode
  "Toggle Vcomplete's Embark integration.

When Vcomplete's Embark integration is enabled, Embark commands (like
`embark-act') will operate on the highlighted completion in the
`*Completions*' buffer."
  :global t
  :group 'vcomplete
  (require 'embark)
  (if vcomplete-embark-mode
      (progn
        (add-hook 'embark-target-finders
                  #'vcomplete-embark-current-completion)
        (advice-add 'embark--prompt-for-action :around
                    #'vcomplete-embark--inhibit-auto-update))
    (remove-hook 'embark-target-finders
                 #'vcomplete-embark-current-completion)
    (advice-remove 'embark--prompt-for-action
                   #'vcomplete-embark--inhibit-auto-update)))

(provide 'vcomplete-embark)

;;; vcomplete-embark.el ends here
