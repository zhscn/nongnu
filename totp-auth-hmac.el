;;; hmac.el --- RFC6238 HMAC -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Moapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; HMAC message hashing for RFC6238 support.
;; TODO - check if this can be dropped for gnutls-hash-mac et al

;;; Code:
(defun totp-auth-hmac-algo-block-size (algo)
  "Return the expected block size of the given hash algorithm ALGO.
ALGO may be one of: \\='(md5 sha1 sha224 sha256 sha384 sha512)."
  (cl-case algo
    ((md5 sha1 sha224 sha256)  64)
    ((sha384 sha512)          128)))

(defun totp-auth-hmac-xpad (algo char)
  "Create a pad of the block size for ALGO, consisting of CHAR.
ALGO is one known to ‘totp-auth-hmac-algo-block-size’."
  (let ((block-size (totp-auth-hmac-algo-block-size (or algo 'sha1))))
    (make-string block-size char)))

(defun totp-auth-hmac-opad (&optional algo)
  "Create an RFC6238 HMAC outer pad.
ALGO defaults to sha1 (see ‘totp-auth-hmac-algo-block-size’)."
  (totp-auth-hmac-xpad algo #x5c))

(defun totp-auth-hmac-ipad (&optional algo)
  "Create an RFC6238 HMAC inner pad.
ALGO defaults to sha1 (see ‘totp-auth-hmac-algo-block-size’)."
  (totp-auth-hmac-xpad algo #x36))

(defun totp-auth-hmac-xor (a b)
  "Xor two byte strings A and B together."
  (let ((la (length a))
        (lb (length b))
        len out)
    (setq len (max la lb)
          out (make-string len 0))
    (dotimes (i len)
      (aset out i
            (logxor (logand #xff (aref a (mod i la)))
                    (logand #xff (aref b (mod i lb))))))
    out))

(defun totp-auth-hmac-blockify (algo data)
  "Return a string based on DATA and the block size of `secure-hash' ALGO.
See ’totp-auth-hmac-algo-block-size’ and ‘secure-hash’.
If DATA is too short, it is zero-padded to the right.
If DATA is too long, it is hashed according to ALGO and zero-padded.
If DATA is already the right length, it is simply returned."
  (let ((block-size  (totp-auth-hmac-algo-block-size algo))
        (data-length (length data)))
    (cond ((= data-length block-size) data)
          ((< data-length block-size)
           (concat data (make-string (- block-size data-length) 0)))
          ((> data-length block-size)
           (setq data (secure-hash algo data nil nil t))
           (totp-auth-hmac-blockify algo data))) ))

(defun totp-auth-hmac (key message &optional algo human)
  "Return the RFC6238 HMAC of MESSAGE with KEY.
The HMAC is created by ’secure-hash’.
ALGO defaults to sha1 (see ‘totp-auth-hmac-algo-block-size’).
HUMAN determines whether the returned HMAC is binary data or
human-readable."
  (let (hkey opad ipad inner outer)
    (setq algo  (or algo 'sha1)
          hkey  (totp-auth-hmac-blockify algo key)
          opad  (totp-auth-hmac-opad algo)
          ipad  (totp-auth-hmac-ipad algo)
          inner (concat (totp-auth-hmac-xor hkey ipad) message)
          inner (secure-hash algo inner nil nil t)
          outer (totp-auth-hmac-xor hkey opad))
    (secure-hash algo (concat outer inner) nil nil (not human))))

(provide 'totp-auth-hmac)
;;; totp-auth-hmac.el ends here
