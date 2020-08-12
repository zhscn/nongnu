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
;; Small brain parses the entire buffer, invalidated by any change.
;;
;; Big brain parses toplevel regions of interest, invalidated by any changes.
;; This is what we do.
;;
;; Galaxy brain caching would use properties and put dirty markers on inserted
;; or deleted regions. Would give fast lookup at point.

(eval-when-compile
  (require 'cl-macs))

(require 'haskell-tng-util)

;; A alist of lists of (OPEN . (CLOSE . SEPS)) positions, keyed by (START . END)
;;
;; Regions are exclusive of START and inclusive of END, and do not overlap. This
;; is because START would only ever contain CLOSE or SEP, not OPEN.
;;
;; Instead of a list, may also be t, indicating that there is no relevant layout
;; in a region.
(defvar-local haskell-tng--layout-cache nil)

;; We only need to invalidate regions that are on or after the beginning of user
;; edits. But doing the pruning will slow down insertions. We store the smallest
;; point that the user edits to invalidate on access.
(defvar-local haskell-tng--layout-cache-invalid nil)
(defun haskell-tng--layout-cache-invalidation (beg _end _pre-length)
  "For use in `after-change-functions' to invalidate the state of
the layout engine."
  (setq
   haskell-tng--layout-cache-invalid
   (min beg (or haskell-tng--layout-cache-invalid
                most-positive-fixnum))))

(defun haskell-tng--layout-pruned-cache ()
  (let ((beg haskell-tng--layout-cache-invalid))
    (if beg
        (setq
         haskell-tng--layout-cache-invalid nil
         haskell-tng--layout-cache
         (seq-filter
          (lambda (it) (<= (cdar it) beg))
          haskell-tng--layout-cache))
      haskell-tng--layout-cache)))

;; TODO a visual debugging option would be great, showing virtuals as overlays

;; EXT:NonDecreasingIndentation

(defun haskell-tng--layout-virtuals-at-point ()
  "List of virtual `{' `}' and `;' at point, according to the
Haskell2010 Layout rules.

Designed to be called repeatedly, managing its own caching."
  (when-let (cache (haskell-tng--layout-at-point))
    (let ((pos (point))
          opens breaks closes)
      (dolist (block cache)
        (pcase block
          (`(,open . (,close . ,seps))
           (when (and open (= open pos))
             (push "{" opens))
           (when (and close (= close pos))
             (push "}" closes))
           (dolist (sep seps)
             (when (= sep pos)
               (push ";" breaks))))))
      (append opens closes breaks))))

(defun haskell-tng--layout-has-virtual-at-point ()
  "t if there is a virtual at POINT"
  ;; avoids a measured performance hit (append indentation)
  (when-let (cache (haskell-tng--layout-at-point))
    (seq-find
     (lambda (it) (member (point) it))
     cache)))

(defun haskell-tng--layout-at-point ()
  "Returns the relevant virtual tokens for the current point,
using a cache if available."
  (when-let
      (layout (or
               (cdr (seq-find
                     (lambda (it) (and (<  (caar it) (point))
                                       (<= (point) (cdar it))))
                     (haskell-tng--layout-pruned-cache)))
               (haskell-tng--layout-rebuild-cache-at-point)))
    (unless (eq layout t) layout)))

(defun haskell-tng--layout-rebuild-cache-at-point ()
  (let ((toplevel (rx bol (or word-start "("))))
    (if (and (looking-at toplevel) (not (bobp)))
        ;; min is exclusive, so go back one.
        (save-excursion
          (forward-char -1)
          (haskell-tng--layout-rebuild-cache-at-point))
      (let* ((min
              (save-excursion
                (end-of-line 1)
                (or (re-search-backward toplevel nil t) 0)))
             (max
              (save-excursion
                (end-of-line 1)
                (or (and (re-search-forward toplevel nil t)
                         (match-beginning 0))
                    (point-max))))
             (module
              (save-excursion
                (goto-char min)
                (looking-at (rx word-start "module" word-end))))
             (before-module
              (save-excursion
                (goto-char max)
                (looking-at (rx word-start "module" word-end))))
             case-fold-search
             cache)

        ;; `module ... where { ... }' special cases:
        ;;
        ;; 1. before module, nothing
        ;; 2. after module, only an open
        ;; 3. eob, extra close
        ;; 4. everywhere else, extra sep
        (when module
          (push `(,max nil) cache))
        (unless (or module before-module)
          (if (eq max (point-max))
              (push `(nil ,max) cache)
            (push `(nil nil ,max) cache))
          (save-excursion
            (goto-char min)
            (while (< (point) max)
              (when-let (wldo (haskell-tng--layout-next-wldo max))
                (push wldo cache)))))

        ;; TODO remove this sanity check when we are happy
        ;; a sanity check that all points are within the bounds
        (cl-flet ((good (type p)
                        (when (and p (or (<= p min) (< max p)))
                          (message "BUG: LAYOUT %S at %S" type p))))
          (dolist (block cache)
            (pcase block
              (`(,open . (,close . ,seps))
               (good 'OPEN open)
               (good 'CLOSE close)
               (dolist (sep seps)
                 (good 'SEP sep))))))

        (let ((key (cons min max))
              (value (or (reverse cache) t)))
          (push (cons key value) haskell-tng--layout-cache)
          value)))))

(defun haskell-tng--layout-next-wldo (limit)
  (cl-block wldo
    (while (< (point) limit)
      (forward-comment limit)
      (cond
       ((looking-at (rx symbol-start
                        (| "\\case" ;; LambdaCase
                           "where" "let" "do" "of")
                        word-end))
        (goto-char (match-end 0))
        (forward-comment limit)
        (unless (looking-at "{")
          (cl-return-from wldo
            (haskell-tng--layout-wldo
             (min (or (haskell-tng--util-paren-close) (point-max))
                  limit)))))

       (t (skip-syntax-forward "^-"))))))

(defun haskell-tng--layout-wldo (limit)
  "A list holding virtual `{', then `}', then virtual `;' in
order between point and LIMIT.

Assumes that point is at the beginning of the first token after a
WLDO that is using the offside rule."
  (save-excursion
    (let* ((open (point))
           seps
           (level (current-column))
           (close (cl-block closed
                    (while (< (point) limit)
                      (forward-line)
                      (forward-comment limit)
                      (when (and (< (point) limit)
                                 (= (current-column) level)
                                 (not (looking-at
                                       (rx bol (or "," ")" "]" "}")))))
                        (push (point) seps))
                      (when (<= limit (point))
                        (cl-return-from closed limit))
                      (when (< (current-column) level)
                        (cl-return-from closed (point))))
                    limit)))
      `(,open . (,close . ,(reverse seps))))))

(provide 'haskell-tng-layout)
;;; haskell-tng-layout.el ends here
