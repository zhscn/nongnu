
(setq load-path (cons "../geiser/elisp" (cons "." load-path)))
(require 'geiser)
(load "geiser-stklos.el")
;(eval-when-compile
(require 'cl-lib)
;)


(ert-deftest geiser-stklos--loaded ()
  (should (member 'geiser-stklos features)))

(ert-deftest find-close-par ()
  29
  (with-temp-buffer
    (insert "(let* ((let 'let) (let* let)) let)")
    (goto-char 7)
    (geiser-stklos--find-close-par)))

(ert-deftest find-close-par-2 ()
  18
  (with-temp-buffer
    (insert "(let* ((let 'let) (let* let)) let)")
    (goto-char 10)
    (geiser-stklos--find-close-par 8)))


;; this one could have many more variants
(ert-deftest geiser-stklos--symbol-begin ()
  5
  (with-temp-buffer
    (insert "(abc def (ghijkl))")
    (goto-char 7)
    (geiser-stklos--symbol-begin nil)))
