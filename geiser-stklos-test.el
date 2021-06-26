
(setq load-path (cons "../geiser/elisp" (cons "." load-path)))
(require 'geiser)
(load "geiser-stklos.el")
;(eval-when-compile
(require 'cl-lib)
;)


(ert-deftest geiser-stklos--loaded ()
  (should (member 'geiser-stklos features)))

;; Go to char in position 7 and find the closing parenthesis
;; of the form -- which is in position 29
(ert-deftest find-close-par ()
  (should (= 30
             (with-temp-buffer
               (insert "(let* ((let 'let) (let* let)) let)")
               (goto-char 7)
               (geiser-stklos--find-close-par)))))
          
;; Go to char in position 10 and find the closing parenthesis
;; of the form -- which is in position 18
(ert-deftest find-close-par-2 ()
  (should (= 18
             (with-temp-buffer
               (insert "(let* ((let 'let) (let* let)) let)")
               (goto-char 10)
               (geiser-stklos--find-close-par 8)))))



;; this one could have many more variants
;; Go to char in position 7 and find the beginning of the symbol
;; -- which is in position 6
(ert-deftest geiser-stklos--symbol-begin ()
  (should (= 6
             (with-temp-buffer
               (insert "(abc def (ghijkl))")
               (goto-char 7)
               (geiser-stklos--symbol-begin nil)))))



