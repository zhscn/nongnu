;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022 Vivek Das Moapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar test-totp-source-dir nil)

(eval-and-compile
  (let ((load-path load-path)
        (this-file (or load-file-name
                       byte-compile-current-file
                       buffer-file-name)))
    (setq test-totp-source-dir
          (expand-file-name (concat (file-name-directory this-file) "/..")))
    (message "running tests in %s" test-totp-source-dir)
    (add-to-list 'load-path test-totp-source-dir)
    (require 'totp-auth)
    (require 'totp-interop)))

(defun 0b (byte)
  "Byte to 8-character string formatter."
  (let ((str (make-string 8 ?0)))
    (setq byte (logand #xff byte))
    (dotimes (i 8)
      (if (< 0 (logand byte (logand #xff (expt 2 i)))) (aset str i ?1)))
    (reverse str)))

(defun 0b4 (uint)
  "Uint32 to spce-separated binary string formater."
  (format "%8s %8s %8s %8s"
          (0b (logand #xff (lsh uint -24)))
          (0b (logand #xff (lsh uint -16)))
          (0b (logand #xff (lsh uint  -8)))
          (0b (logand #xff uint))))

;; This is from RFC 6238
;;  The test token shared secret uses the ASCII string value
;;  "12345678901234567890".  With Time Step X = 30, and the Unix epoch as
;;  the initial value to count time steps, where T0 = 0, the TOTP
;;  algorithm will display the following values for specified modes and
;;  timestamps.
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;; NB - the above comment about the secret is a LIE. The secret is actually
;; the digits 123456789 repeated to 20, 32 and 64 bytes depending on the
;; HMAC hash algorithm:
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;; +-------------+--------------+------------------+----------+--------+
;; |  Time (sec) |   UTC Time   | Value of T (hex) |   TOTP   |  Mode  |
;; +-------------+--------------+------------------+----------+--------+
;; |      59     |  1970-01-01  | 0000000000000001 | 94287082 |  SHA1  |
;; |             |   00:00:59   |                  |          |        |
;; |      59     |  1970-01-01  | 0000000000000001 | 46119246 | SHA256 |
;; |             |   00:00:59   |                  |          |        |
;; |      59     |  1970-01-01  | 0000000000000001 | 90693936 | SHA512 |
;; |             |   00:00:59   |                  |          |        |
;; |  1111111109 |  2005-03-18  | 00000000023523EC | 07081804 |  SHA1  |
;; |             |   01:58:29   |                  |          |        |
;; |  1111111109 |  2005-03-18  | 00000000023523EC | 68084774 | SHA256 |
;; |             |   01:58:29   |                  |          |        |
;; |  1111111109 |  2005-03-18  | 00000000023523EC | 25091201 | SHA512 |
;; |             |   01:58:29   |                  |          |        |
;; |  1111111111 |  2005-03-18  | 00000000023523ED | 14050471 |  SHA1  |
;; |             |   01:58:31   |                  |          |        |
;; |  1111111111 |  2005-03-18  | 00000000023523ED | 67062674 | SHA256 |
;; |             |   01:58:31   |                  |          |        |
;; |  1111111111 |  2005-03-18  | 00000000023523ED | 99943326 | SHA512 |
;; |             |   01:58:31   |                  |          |        |
;; |  1234567890 |  2009-02-13  | 000000000273EF07 | 89005924 |  SHA1  |
;; |             |   23:31:30   |                  |          |        |
;; |  1234567890 |  2009-02-13  | 000000000273EF07 | 91819424 | SHA256 |
;; |             |   23:31:30   |                  |          |        |
;; |  1234567890 |  2009-02-13  | 000000000273EF07 | 93441116 | SHA512 |
;; |             |   23:31:30   |                  |          |        |
;; |  2000000000 |  2033-05-18  | 0000000003F940AA | 69279037 |  SHA1  |
;; |             |   03:33:20   |                  |          |        |
;; |  2000000000 |  2033-05-18  | 0000000003F940AA | 90698825 | SHA256 |
;; |             |   03:33:20   |                  |          |        |
;; |  2000000000 |  2033-05-18  | 0000000003F940AA | 38618901 | SHA512 |
;; |             |   03:33:20   |                  |          |        |
;; | 20000000000 |  2603-10-11  | 0000000027BC86AA | 65353130 |  SHA1  |
;; |             |   11:33:20   |                  |          |        |
;; | 20000000000 |  2603-10-11  | 0000000027BC86AA | 77737706 | SHA256 |
;; |             |   11:33:20   |                  |          |        |
;; | 20000000000 |  2603-10-11  | 0000000027BC86AA | 47863826 | SHA512 |
;; |             |   11:33:20   |                  |          |        |
;; +-------------+--------------+------------------+----------+--------+

(defconst test-totp-data
  '((         59 sha1   "94287082")
    (         59 sha256 "46119246")
    (         59 sha512 "90693936")
    ( 1111111109 sha1   "07081804")
    ( 1111111109 sha256 "68084774")
    ( 1111111109 sha512 "25091201")
    ( 1111111111 sha1   "14050471")
    ( 1111111111 sha256 "67062674")
    ( 1111111111 sha512 "99943326")
    ( 1234567890 sha1   "89005924")
    ( 1234567890 sha256 "91819424")
    ( 1234567890 sha512 "93441116")
    ( 2000000000 sha1   "69279037")
    ( 2000000000 sha256 "90698825")
    ( 2000000000 sha512 "38618901")
    (20000000000 sha1   "65353130")
    (20000000000 sha256 "77737706")
    (20000000000 sha512 "47863826")))

(defun totp-check-results (label target digits algorithm &rest generated-otp)
  (let ((i 0)
        (want  (string-to-number (substring target (- digits))))
        (stamp (format-time-string "%Y-%m-%d %H:%M:%S" totp-override-time t))
        have)
    (dolist (otp generated-otp)
      (setq have (string-to-number (car otp)))
      (or (equal want have)
          (error "%s TOTP #%d @%d (%s), %S %d digits did not match: %S vs %S"
                 label i totp-override-time stamp algorithm digits want have))
      (setq i (1+ i)))
    (message "%8s TOTP @%d (%s) OK" label totp-override-time stamp)
    i))

(defun test-totp-secret (algo)
  "The TOTP RFC test secret is the digits repeated up to N bytes,
where N depends on the HMAC algorithm."
  (hmac-xor (make-string (cond ((eq algo 'sha1)   20)
                               ((eq algo 'sha256) 32)
                               ((eq algo 'sha512) 64))
                         0)
            "1234567890"))

;; sha1 is the default algo so don't pass it:
;; likewise 6 is the 
(defun test-totp-check-parameters (params)
  (let ((totp-override-time (nth 0 params))
        (algorithm          (nth 1 params))
        (result-8           (nth 2 params))
        key)
    (setq key (base32-encode (test-totp-secret algorithm)))
    (dolist (digits '(8 7 6))
      (cond
       ((and (eq digits 6) (eq algorithm 'sha1))
        (totp-check-results "sha1:6" result-8 digits algorithm
                            (totp-generate-otp key)
                            (totp-generate-otp key digits)
                            (totp-generate-otp key nil    nil nil algorithm)
                            (totp-generate-otp key digits nil nil algorithm)))
       ((eq digits 6)
        (totp-check-results (format "%s:6" algorithm) result-8 digits algorithm
                            (totp-generate-otp key nil    nil nil algorithm)
                            (totp-generate-otp key digits nil nil algorithm)))
       ((eq algorithm 'sha1)
        (totp-check-results (format "sha1:%d" digits) result-8 digits algorithm
                            (totp-generate-otp key digits nil nil)
                            (totp-generate-otp key digits nil nil algorithm)))
       (t
        (totp-check-results (format "%s:%d" algorithm digits)
                            result-8 digits algorithm
                            (totp-generate-otp key digits nil nil algorithm)))))
    t))

(defconst test-totp-import-expected-results
  '(("tests/single-otp-url.txt" .
     (((:service . "moomins")
       (:secret . "deadbeefdeadbeefdeadbeefdeadbeef")
       (:user . "vivek@collabora.com")
       (:digits . 6))))
    ("tests/multi-otp-url.txt" .
     (((:service . "smurfs")
       (:secret . "abadideaabadideaabadideaabadidea")
       (:user . "gargamel")
       (:digits . 8))
      ((:service . "moomins")
       (:user . "snork-maiden")
       (:secret . "deadbeefdeadbeefdeadbeefdeadbeef")
       (:digits . 6))))
    ("tests/single-otp-QR-code.png" .
     (((:service . "Test QR import")
       (:secret . "deadbeefdeadbeefdeadbeef")
       (:digits . 7))))
    )
  "Expected import values from test files")

(defun test-totp-compare-alist-list (a b &optional label)
  "Compare two lists of alists. 
The lists must have the same number of elements, in the same order. 
The alists need only have the same keys returning the same values (as 
determined by `assoc`).
The alists may have 'masked' entries (having later keys that match
earlier ones, whic would therefore not be returned by `assoc`)."
  (when (equal (length a) (length b))
    (let ((ok t) (i 0) item-a item-b key-lol j)
      (while (and ok (< i (length a)))
        (setq item-a   (nth i a)
              item-b   (nth i b)
              key-lol  (list (mapcar 'car item-a) (mapcar 'car item-b))
              i        (1+ i)
              j        nil)
        (dolist (key-list key-lol)
          (mapc (lambda (k &optional dir)
                  (setq dir (if j "rev" "fwd"))
                  (if (equal (cdr (assoc k item-a)) (cdr (assoc k item-b)))
                      (message "%s[%d]%-9S (%s) OK" (or label "") i k dir)
                    (message "%s[%d]%-9S (%s) FAILED" (or label "") i k dir)
                    (message "%S → %S vs %S"
                             k
                             (assoc k item-a)
                             (assoc k item-b))
                    (setq ok nil)))
                key-list)
          (setq j (not j))))
      ok)))

(defun test-totp-import ()
  (let ((close-enough t))
    (mapc
     (lambda (test-file &optional expected actual)
       (setq expected (cdr (assoc test-file test-totp-import-expected-results))
             actual   (totp-load-file
                       (expand-file-name test-file test-totp-source-dir)))
       (if (test-totp-compare-alist-list expected actual test-file)
           (message "%s imported OK" test-file)
         (message "%s import FAILED" test-file)
         (setq close-enough nil)))
     (mapcar 'car test-totp-import-expected-results))
    close-enough))

;; (totp-unwrap-otp-blob "otpauth://totp/Test%20QR%20import?secret=deadbeefdeadbeefdeadbeef&digits=7&algorithm=SHA256")
;; (insert (pp (totp-load-file "single-otp-QR-code.png")))



(defun test-totp ()
  (mapc 'test-totp-check-parameters test-totp-data)
  (test-totp-import))

(test-totp)
