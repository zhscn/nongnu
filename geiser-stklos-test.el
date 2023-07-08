
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

;; Go to char in position 5. This is NOT inside a module, so :f should
;; be returned.
(ert-deftest get-module-1 ()
  (should (eq :f
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-library y (begin (define z 10)))\n(define w 9)")
                (goto-char 5)
                (geiser-stklos--get-module)))))

;; Go to char in position 62. This is NOT inside a module, so :f should
;; be returned.
(ert-deftest get-module-2 ()
  (should (eq :f
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-library y (begin (define z 10)))\n(define w 9)")
                (goto-char 62)
                (geiser-stklos--get-module)) )))

;; Go to char in positions 19, 33 and 43. This is NOT inside a module, so :f should
;; be returned.
(ert-deftest get-module-3 ()
  (should (eq 'y
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-library y (begin (define z 10)))\n(define w 9)")
                (goto-char 19)
                (geiser-stklos--get-module)))))

(ert-deftest get-module-4 ()
  (should (eq 'y
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-library y (begin (define z 10)))\n(define w 9)")
                (goto-char 33)
                (geiser-stklos--get-module)))))

(ert-deftest get-module-5 ()
  (should (eq 'y
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-library y (begin (define z 10)))\n(define w 9)")
                (goto-char 43)
                (geiser-stklos--get-module)))))

(ert-deftest get-module-6 ()
  (should (eq 'y
              (with-temp-buffer
                (insert "(define x 1)\n\n(define-module y  (define z 10))\n(define w 9)")
                (goto-char 29)
                (geiser-stklos--get-module)))))

;; this one could have many more variants
;; Go to char in position 7 and find the beginning of the symbol
;; -- which is in position 6
(ert-deftest geiser-stklos--symbol-begin ()
  (should (= 6
             (with-temp-buffer
               (insert "(abc def (ghijkl))")
               (goto-char 7)
               (geiser-stklos--symbol-begin nil)))))


(ert-deftest geiser-stklos--symbol-begin-2 ()
  (should (= 14
             (with-temp-buffer
               (insert "(abc def (gh ijk () lmn))")
               (goto-char 15)
               (geiser-stklos--symbol-begin nil)))))

(ert-deftest import-command ()
  (let ((s (geiser-stklos--import-command "some-module")))
    (should (and (stringp s)
                 (string= s "(require \"some-module\")")))))

(ert-deftest exit-command ()
  (let ((s (geiser-stklos--exit-command)))
    (should (and (stringp s)
                 (string= s "(exit 0)")))))


;; geiser-stklos-guess
(ert-deftest guess ()
  (should (eq nil
              (with-temp-buffer
                (insert "(let* ((let 'let) (let* let)) let)")
                (goto-char 10)
                (geiser-stklos--guess)))))

