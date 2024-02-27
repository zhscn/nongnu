;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2024 Vivek Das Moapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar test-base32-source-dir nil)

(eval-and-compile
  (require 'loadhist)
  (let ((load-path load-path)
        (this-file (or load-file-name
                       byte-compile-current-file
                       buffer-file-name)))
    (setq test-base32-source-dir
          (expand-file-name (concat (file-name-directory this-file) "/..")))
    (message "running tests in %s" test-base32-source-dir)
    (add-to-list 'load-path test-base32-source-dir)
    (mapc #'require '(base32))
    (mapcar (lambda (F) (message "%S loaded from %S" F (feature-file F))) features)))

(defvar test-base32-test-data
       '((""       . "")
         ("f"      . "MY======")
         ("fo"     . "MZXQ====")
         ("foo"    . "MZXW6===")
         ("foob"   . "MZXW6YQ=")
         ("fooba"  . "MZXW6YTB")
         ("foobar" . "MZXW6YTBOI======")
         ;; now try with the = section stripped.
         ;; naming no providers called dropbox *cough*
         ("f"      . "MY")
         ("fo"     . "MZXQ")
         ("foo"    . "MZXW6")
         ("foob"   . "MZXW6YQ")
         ("foobar" . "MZXW6YTBOI")))

(defun test-base32-compare-payloads (a b)
  (or (equal a b)
      (and (stringp a)
           (stringp b)
           (or  (and (string-match "^\\([a-z0-9]+\\)=+$" b)
                     (equal a (match-string 1 b)))
                (and (string-match "^\\([a-z0-9]+\\)=+$" a)
                     (equal b (match-string 1 a)))))))

(defun test-base32-xcode (x)
  (let ((decoded (car x))
        (encoded (cdr x))
        test-decoded
        test-encoded
        stub)
     (setq stub         (format "Checking decode of %S from %S: " decoded encoded)
           test-decoded (base32-decode encoded))
     (message "%s: %s [%S %S]"
              (if (equal test-decoded decoded) "PASS" "FAIL") stub
              test-decoded decoded)
     (setq stub         (format "Checking encode of %S from %S: " encoded decoded)
           test-encoded (base32-encode decoded))
     (message "%s: %s [%S %S]"
              (if (test-base32-compare-payloads test-encoded encoded) "PASS" "FAIL")
              stub
              test-encoded encoded) ))

(mapc #'test-base32-xcode test-base32-test-data)
