;;; haskell-tng-layout.el --- Significant Whitespace of Haskell -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Calculates "layout" according to Haskell2010 sections 2.7 and 10.3:
;;
;;  https://www.haskell.org/onlinereport/haskell2010/haskellch2.html
;;  https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
;;
;;  This algorithm must expose a stateless API so that stateless lexing is
;;  possible (see SMIE). However, as the layout is queried for every token
;;  during lexing, this must also be very efficient. Therefore, caching is used
;;  internally.
;;
;;; Code:

;; Notes on caching
;;
;; Small brain is to parse the entire buffer, invalidated on any change.
;;
;; Big brain would store a record of the region that has been edited and reparse
;; only the layouts that have changed. The invalidation may be a simple case of
;; dismissing everything (including CLOSE parts) after any point that has been
;; edited or trying to track insertions.
;;
;; Galaxy brain caching would use properties and put dirty markers on inserted
;; or deleted regions. Also this could give lightning fast lookup at point on
;; cache hits.
;;
;; Anything more complicated that small brain needs improved testing.

(require 'haskell-tng-util)

;; Easiest cache... full buffer parse with full invalidation on any insertion.
(defvar-local haskell-tng-layout:cache nil)

(defun haskell-tng-layout:cache-invalidation (_beg _end _pre-length)
  "For use in `after-change-functions' to invalidate the state of
the layout engine."
  (when haskell-tng-layout:cache
    (setq haskell-tng-layout:cache nil)))

;; TODO a visual debugging option would be great, showing virtuals as overlays

(defun haskell-tng-layout:virtuals-at-point ()
  "List of virtual `{' `}' and `;' at point, according to the
Haskell2010 Layout rules.

Designed to be called repeatedly, managing its own caching."
  (unless haskell-tng-layout:cache
    (haskell-tng-layout:rebuild-cache-full))

  (let ((pos (point)))
   (catch 'done
     (let (breaks
           closes)
       (dolist (block haskell-tng-layout:cache)
         (let ((open (car block))
               (close (cadr block))
               (lines (cddr block)))
           ;;(message "BLOCK = %S (%s, %s, %s)" block open close lines)
           (when (and (<= open pos) (<= pos close))
             (when (= open pos)
               (throw 'done '("{")))
             (when (= close pos)
               (push "}" closes))
             (dolist (line lines)
               (when (= line pos)
                 (push ";" breaks))))))
       (append (reverse closes) (reverse breaks))))))

(defun haskell-tng-layout:rebuild-cache-full ()
  (let (case-fold-search
        cache)
    (save-excursion
      (goto-char 0)
      (while (not (eobp))
        (when-let (wldo (haskell-tng-layout:next-wldo))
          (push wldo cache))))
    (setq haskell-tng-layout:cache (reverse cache))))

(defun haskell-tng-layout:next-wldo ()
  (catch 'wldo
    (while (not (eobp))
      (forward-comment (point-max))
      (cond
       ((looking-at (rx word-start (| "where" "let" "do" "of") word-end))
        (goto-char (match-end 0))
        (forward-comment (point-max))
        (when (not (looking-at "{"))
          (throw 'wldo (haskell-tng-layout:wldo))))

       (t (skip-syntax-forward "^-"))))))

(defun haskell-tng-layout:wldo ()
  "A list holding virtual `{', then `}', then virtual `;' in order.

Assumes that point is at the beginning of the first token after a
WLDO that is using the offside rule."
  (save-excursion
    (let* ((open (point))
           seps
           (level (current-column))
           (limit (or (haskell-tng:paren-close) (point-max)))
           (close (catch 'closed
                    (while (not (eobp))
                      (forward-line)
                      (forward-comment (point-max))
                      (when (and (= (current-column) level)
                                 (not (eobp)))
                        (push (point) seps))
                      (when (< limit (point))
                        (throw 'closed limit))
                      (when (< (current-column) level)
                        (throw 'closed (point))))
                    (point-max))))
      `(,open . (,close . ,(reverse seps))))))

(provide 'haskell-tng-layout)
;;; haskell-tng-layout.el ends here
