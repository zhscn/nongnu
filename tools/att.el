;; -*- lexical-binding: t; -*-

;; FIXME: ELisp files should not make such changes to the state when they're
;; loaded, since Emacs feels free to load files from `load-path' without
;; the user requesting it explicitly (e.g. to look for possible completions).
;; IOW, move those side-effects to a function.
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (concat default-directory "/tools/"))

(require 'pacmacs-rr)

(toggle-debug-on-error)

(defconst att-result-file-path "./att.txt")
(defvar att-it-case-path "./it-cases/it-case03.eld")

(defun att-replayer-finished ()
  (let ((coding-system-for-write 'utf-8))
    (write-region (format "Average Tick Time: %fms"
                          (pacmacs--average-tick-time))
                  nil
                  att-result-file-path nil 0))
  (kill-emacs 0))

(add-hook 'pacmacs-replay-finished-hook #'att-replayer-finished)
(pacmacs--start-it-replayer att-it-case-path)
