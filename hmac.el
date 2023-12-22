;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022 Vivek Das Moapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun hmac-algo-block-size (algo)
  (cl-case algo
    ((md5 sha1 sha224 sha256)  64)
    ((sha384 sha512)          128)))

(defun hmac-xpad (algo char)
  (let ((block-size (hmac-algo-block-size (or algo 'sha1))))
    (make-string block-size char)))

(defun hmac-opad (&optional algo)
  (hmac-xpad algo #x5c))

(defun hmac-ipad (&optional algo)
  (hmac-xpad algo #x36))

(defun hmac-xor (a b)
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

(defun hmac-blockify (algo data)
  "Return a string based on DATA the same size as the block-size of
`secure-hash` algorithm ALGO.
If DATA is too short, it is zero-padded to the right.
If DATA is too long, it is hashed according ot ALGO and the result
is zero-padded to the required length.
If DATA is already the right length, it is simply returned."
  (let ((block-size  (hmac-algo-block-size algo))
        (data-length (length data)))
    (cond ((= data-length block-size) data)
          ((< data-length block-size)
           (concat data (make-string (- block-size data-length) 0)))
          ((> data-length block-size)
           (setq data (secure-hash algo data nil nil t))
           (hmac-blockify algo data))) ))

(defun hmac (key message &optional algo human)
  (let (hkey opad ipad inner outer)
    (setq algo  (or algo 'sha1)
          hkey  (hmac-blockify algo key)
          opad  (hmac-opad algo)
          ipad  (hmac-ipad algo)
          inner (concat (hmac-xor hkey ipad) message)
          inner (secure-hash algo inner nil nil t)
          outer (hmac-xor hkey opad))
    (secure-hash algo (concat outer inner) nil nil (not human))))

(provide 'hmac)
