;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Mohapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

(eval-and-compile
  (let ((load-path load-path)
        (this-file (or load-file-name
                       byte-compile-current-file
                       buffer-file-name)))
    (when (not (and (locate-library "base32")
                    (locate-library "hmac")))
      (add-to-list 'load-path (file-name-directory this-file)))
    (require 'totp))
  (require 'mailcap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file implements import/export functionality for common OTP exchange
;; formats like otpauth URLs and QR encoded OTP secrets

;; It isn't necessary for usual day-to-day totp.el use, only when
;; You need to get your TOTP secrets into or out of the totp.el system.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial protobuffer support so we can decode otpauth-migration URLs
(defconst totp-pb-types [:varint :i64 :len :start :end :i32])

(defun totp-pb-type (n)
  "Look up the protobuffer type by its serialisation numeric code."
  (when (< n (length totp-pb-types))
    (aref totp-pb-types n)))

(defun totp-pb-read-varint (bytes &optional pos)
  "Reads a varint from a protobuffer-compliant  array, vector or string BYTES 
starting at offset POS.
Returns a cons of (VALUE . BYTES-READ)"
  (let ((u64 0)
        b10 byte collected vbyte-count)
    (or pos (setq pos 0))
    ;; VARINTs are 0-9 bytes with the high bit set
    ;; followed 1 byte with the high bit unset
    (while (eq #x80 (logand #x80 (setq byte (aref bytes pos))))
      (setq collected (cons (logand #x7f byte) collected)
            pos       (1+ pos)))
    (setq collected   (nreverse (cons byte collected))
          collected   (concat collected)
          vbyte-count (length collected))
    ;; VARINTs can threfore be no more than 10 bytes of encoded data
    (if (> vbyte-count 10)
        (cons nil vbyte-count);; varint overflow
      (when (= vbyte-count 10)
        ;; If there are 10 bytes then the first byte must be 0x1
        ;; as 9 varint encoding bytes gives 9×7 = 63 bits, which
        ;; only leaves 1 bit. 
        (setq b10 (aref collected 9)))
      (dotimes (i (length collected))
        (setq u64 (+ u64 (lsh (aref collected i) (* i 7)))))
      (if (and b10 (not (eq b10 1)))
          (cons nil vbyte-count)
        (cons u64 vbyte-count))) ))

(defun totp-pb-read-tag (buf &optional pos)
  "Read a protobuffer tag, which is (field-number << 3 | type).
from array, vector or string BUF at offset POS.
Returns a structure: ((FIELD . TYPE) . BYTES-READ)
Where TYPE should be :varint :i64 :len or :i32"
  (let ((decoded (totp-pb-read-varint buf pos)) type field)
    (setq field (car decoded)
          type  (totp-pb-type (logand #x7 field))
          field (lsh field -3))
    (setcar decoded (cons field type))
    decoded))

(defun totp-pb-read-raw (buf len &optional pos)
  "Read LEN bytes from a vector or string BUF at offset POS (default 0).
Returns a unibyte string containing those bytes."
  (let ((raw    (make-string len 0))
        (offset (or pos 0)))
    (dotimes (i len)
      (aset raw i (logand #xff (aref buf (+ i offset)))))
    (string-make-unibyte raw)))

(defun totp-pb-read-len (buf &optional pos)
  "Read a variable-length byte string from a protobuffer compliant
source string or vector BUF at offset POS."
  (let (pb len bytes offset read)
    (setq offset (or pos 0)
          pb     (totp-pb-read-varint buf offset)
          len    (car pb)
          read   (cdr pb)
          ;;x      (message "--- want %d bytes (ate %d)" len read)
          offset (+ offset read)
          bytes  (totp-pb-read-raw buf len offset))
    (cons bytes (+ read len))))

(defconst totp-pb-otpauth-migration-field-map 
  [nil :secret (:service . :user) :service :algo :digits :type nil])

(defun totp-pb-otpauth-migration-translate-field (field val)
  "Translate a FIELD number (1-6) and VAL into cons cell(s) suitable
for use in the return value of `totp-unwrap-otp-blob`"
  (let (key)
    (setq key (and (< 0 field)
                   (> (length totp-pb-otpauth-migration-field-map) field)
                   (aref totp-pb-otpauth-migration-field-map field)))
    (cond ((eq key nil)     nil)
          ((eq key :algo)   nil) ;; not yet handled
          ((eq key :type)   nil) ;; can only be TOTP or HOTP, so unimportant
          ((eq key :digits) (when (numberp val)
                              (setq val (+ (* val 2) 4))
                              (cons :digits (if (memq val '(6 8)) val 6))))
          ((consp key)      (if (and (stringp val)
                                     (string-match "^\\(.+\\)?:\\(.+\\)" val))
                                (list (cons (car key) (match-string 1 val))
                                      (cons (cdr key) (match-string 2 val)))
                              (cons (car key) val)))
          ((eq key :secret) (if (stringp val) (cons :secret (base32-encode val))))
          (t                (cons key val))) ))

(defun totp-pb-decode-migration-item (buf)
  "Unpack a secret and metadata from an otpauth-migration URL fragment BUF."
  (let ((offset 0)
        (what :tag)
        res pb-item pb-value pb-field slot)
    (while (< offset (length buf))
      (setq pb-item  (cond
                      ((eq what :tag)    (totp-pb-read-tag buf offset))
                      ((eq what :len)    (totp-pb-read-len buf offset))
                      ((eq what :varint) (totp-pb-read-varint buf offset))
                      (t (error "Unhandled type: %S" what)))
            pb-value (car pb-item)
            offset   (+ (cdr pb-item) offset))
      ;; next     (if (eq what :tag) (cdr pb-value) :tag))
      (if (eq what :tag)
          (setq what (cdr pb-value) pb-field (car pb-value))
        (setq slot (totp-pb-otpauth-migration-translate-field pb-field pb-value)
              what :tag)
        (when slot
          (if (consp (cdr slot))
              (setq res (cons (car slot)
                              (cons (cadr slot)
                                    res)))
            (setq res (cons slot res)))) ))
    ;;(with-current-buffer (get-buffer-create "*migrate*")
    ;;  (insert (pp res) "\n---\n"))
    res))

(defun totp-pb-decode-migration-data (buf &optional pos)
  (let (offset pb-item pb-value what next result item i)
    (setq offset (or pos 0)
          i      0
          what   :tag)
    (while (< offset (length buf))
      (setq pb-item  (cond
                      ((eq what :tag)    (totp-pb-read-tag buf offset))
                      ((eq what :len)    (totp-pb-read-len buf offset))
                      ((eq what :varint) (totp-pb-read-varint buf offset))
                      (t (error "Unhandled type: %S" what)))
            pb-value (car pb-item)
            offset   (+ (cdr pb-item) offset)
            next     (if (eq what :tag) (cdr pb-value) :tag))
      (if (eq what :len)
          (when (setq item (totp-pb-decode-migration-item pb-value))
            (setq result (cons item result))))
      (setq what next i (1+ i)))
    result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun totp-unwrap-otpauth-migration-url (u)
  "Unpack an otpauth-migration url U and extract the parts we care about.
Similar to `totp-unwrap-otpauth-url' but works on otpauth-migration:// URLs
and returns a list of 0 or more secret srtuctures instead of just one."
  (let (query data)
    (setq u     (url-path-and-query u)
          query (cdr u)
          query (url-parse-query-string query)
          data  (cadr (assoc "data" query))
          data  (base64-decode-string data))
    (totp-pb-decode-migration-data data)))

(defun totp-parse-buffer-otp-urls (&optional buffer)
  "Search for otpauth and otpauth-migration URLs in BUFFER
(the current buffer by default) and return a list of all the 
OTP secrets+metadata by calling `totp-unwrap-otp-blob' on them."
  (let (result url-string url)
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward "\\(otpauth\\(?:-migration\\)?://.*\\)$" nil t)
        (setq url-string (match-string 1))
        (if (string-prefix-p "otpauth-migration://" url-string)
            (when (setq url (url-generic-parse-url url-string))
              (setq result
                    (append result (totp-unwrap-otpauth-migration-url url))))
          (setq result
                (cons (totp-unwrap-otp-blob url-string) result)))))
    result))

(defun totp-load-image-file (file)
  "Use `totp-file-import-command' to extract the contents of FILE
and process the results with `totp-parse-buffer-otp-urls'."
  (let ((args (mapcar (lambda (a) (if (equal "@file@" a) file a))
                      (cdr totp-file-import-command))))
    (with-temp-buffer
      (apply 'call-process (car totp-file-import-command) nil t nil args)
      (totp-parse-buffer-otp-urls)) ))

(defun totp-find-hmac-key-by-class (class len)
  (let ((pattern (format "\\b%s\\{%d\\}\\b" class len)))
    (and (re-search-forward pattern nil t) (match-string 0))))

(defun totp-find-hmac-key ()
  (let ((b32-class (concat "[" base32-dictionary "]")))
    (or (totp-find-hmac-key-by-class b32-class 20)
        (totp-find-hmac-key-by-class b32-class 32)
        (totp-find-hmac-key-by-class b32-class 64))))

(defun totp-load-file (file)
  "Load secret(s) from FILE. FILE may be:
  - a single base32 encoded TOTP secret
  - any number of otpauth:// scheme URLs
  - any number of otpauth-migration:// scheme URLs
  - a mix of entries encoded in the above URL schemes
  - a QR code understood by `totp-file-import-command'.\n
Returns a list of TOTP secret alists - that is: Each element of
the returned list is a structure returned by `totp-unwrap-otp-blob'."
  (let (mime-type result)
    (setq file      (expand-file-name file)
          mime-type (mailcap-extension-to-mime (file-name-extension file)))
    (if (string-match "^image/" (or mime-type ""))
        (totp-load-image-file file)
      (with-temp-buffer
        (when (ignore-errors (insert-file-contents file))
          (goto-char (point-min))
          (or (totp-parse-buffer-otp-urls)
              (and (goto-char (point-min))
                   (setq result (totp-find-hmac-key))
                   (list (totp-unwrap-otp-blob result)))) )) )))


(defun totp-import-file (file)
  "Import an RFC6238 TOTP secret or secrets from FILE.
FILE is processed by ‘totp-load-file’ and each secret extracted
is passed to ‘totp-save-secret’."
  (interactive "fImport OTP Secret(s) from: ")
  (mapc #'totp-save-secret (totp-load-file file)))

(provide 'totp-interop)
