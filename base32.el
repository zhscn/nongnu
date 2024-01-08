;;; base32.el --- Base32 support -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Moapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Vivek Das Mohapatra <vivek@etla.org>
;; Keywords: tools
;; URL: https://gitlab.com/fledermaus/totp.el
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; A base32 encoder/decoder.

;;; Code:
(defconst base32-dictionary
  [?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
   ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?2 ?3 ?4 ?5 ?6 ?7 ?=]
  "The default base32 dictionary.
\nTakes the form of a vector which contains a character for each
possible 5bit value (0-31) at that index, plus a padding character
at index 32.")

(defun base32-lsh (v c)
  "Suppress opinionated (and in our case wrong) warning about ’lsh’."
  (with-suppressed-warnings ((suspicious lsh))
    (lsh v c)))

(defun base32-thesaurus (&optional dictionary)
  "Make a reverse lookup base32 for DICTIONARY.
The 5bit value corresponding to each encoding character is stored at the
index of that character.
The padding character is mapped to 0.
Dictionary should match ‘base32-dictionary’ in format."
  (or dictionary (setq dictionary base32-dictionary))
  (let ((b32-max 0) b32-values)
    (dotimes (i (length dictionary))
      (if (> (aref dictionary i) b32-max) (setq b32-max (aref dictionary i))))
    (setq b32-values (make-vector (1+ b32-max) 0))
    (dotimes (i (length dictionary))
      (aset b32-values (aref dictionary i) i))
    (aset b32-values (aref dictionary (1- (length dictionary))) 0)
    b32-values))

(defun base32--nth-5bit-get (str offset)
  "Get the 5bit value at OFFSET * 5 bits from the start of STR."
  (let (start-char start-bit end-bit op-char result r2)
    (setq start-char   (/ (* offset 5) 8)
          start-bit    (mod (* offset 5) 8)
          end-bit      (+ start-bit 4)
          op-char      (aref str start-char))
    ;; left shift the target byte's contents to discard
    ;; the high bits we don't need (eg if our 5-bit offsets
    ;; start us at bit 0 1 >2< of the target byte, then
    ;; discard the 0th and 1st bits).
    ;; then right shift by 3 bits so we have the highest 3
    ;; bits set to zero (since we want a 5-bit value):
    (setq result (logand #xff (base32-lsh op-char start-bit))
          result (logand #xff (base32-lsh result -3)))
    ;; now check to see if we need some bits from the next vyte:
    (when (> end-bit 7)
      (setq end-bit (- end-bit 7) ;; work out the first bit we don't want
            op-char (logand #xff (aref str (1+ start-char)))
            ;; right shift the unwanted bits to discard them
            ;; eg if we wanted 2 bits from byte 0 (rightmost bits 6 and 7)
            ;; then we want bits 8, 9, and 10, aka bits 0, 1, and 2
            ;; from byte 1. which means we discard 5 bits by right shifting:
            r2      (logand #xff (base32-lsh op-char (- end-bit 8)))
            ;; combine the wanted bits:
            result  (logior (logand result #x1f) (logand r2 #x1f))))
    result))

(defun base32--nth-5bit-set (str offset value)
  "Set the 5bit value at OFFSET * 5 bits from the start of STR to VALUE."
  (let (start-char start-bit end-bit op-char set-bits mask discard)
    (setq value      (logand #x1f value)
          start-char (/ (* offset 5) 8)
          start-bit  (mod (* offset 5) 8)
          end-bit    (+ start-bit 4)
          op-char    (aref str start-char))
    ;; value is a 5-bit value, shifted rightwards by start-bit bits
    ;; but lefwards by 3 bits because we're writing into the start
    ;; of an 8-bit slot.
    ;; mask is five contiguous set bits starting at the same offset
    (setq set-bits (logand #xff (base32-lsh value (- 0 -3 start-bit)))
          mask     (logand #xff (base32-lsh #x1f  (- 0 -3 start-bit)))
          ;; turn off the masked bits in the target
          op-char  (logand op-char (lognot mask))
          ;; set the target bits to set-bits
          op-char  (logior op-char set-bits))
    (aset str start-char op-char)
    ;; now deal with any bits that spilled over into the next byte:
    (when (> end-bit 7)
      (setq start-char (1+ start-char)
            ;; number of bits from value we have dealt with
            discard    (- 12 end-bit)
            ;; discard the 3 dead bits and the dealt with bits
            set-bits   (logand #xff (base32-lsh value (+ 3 discard)))
            op-char    (logand #xff (aref str start-char))
            op-char    (logand op-char (lognot #xf8))
            op-char    (logior op-char set-bits))
      (aset str start-char op-char))
    str))

(defun base32-encode (input &optional dictionary)
  "Encode INPUT bytes according to DICTIONARY.
DICTIONARY defaults to ‘base32-dictionary’."
  (or dictionary (setq dictionary base32-dictionary))
  (let (input-byte-count input-shortfall output-padding output output-length)
    (setq input-byte-count (length input)
          input-shortfall  (mod input-byte-count 5))
    (when (> input-shortfall 0)
      (setq input-shortfall (- 5 input-shortfall))
      (setq input (concat input (make-string input-shortfall 0))
            output-padding (cond ((eq input-shortfall 4) 6)
                                 ((eq input-shortfall 3) 4)
                                 ((eq input-shortfall 2) 3)
                                 ((eq input-shortfall 1) 1))))
    (setq output-length (/ (* (length input) 8) 5)
          output        (make-vector output-length 0))
    (dotimes (i output-length)
      (aset output i (aref dictionary (base32--nth-5bit-get input i))))
    (when output-padding
      (let ((padc (or (aref dictionary 32) ?=)))
        (dotimes (i output-padding)
          (aset output (- output-length i 1) padc))))
    (concat output)))

(defun base32-decode (input &optional dictionary)
  "Decode INPUT bytes according to DICTIONARY.
DICTIONARY defaults to ‘base32-dictionary’."
  (let ((thesaurus
         (base32-thesaurus (or dictionary base32-dictionary)))
        input-byte-count input-shortfall
        output output-byte-count output-shorten chunk)
    (setq input-byte-count (length input)
          input-shortfall  (mod input-byte-count 8)
          output-shorten    0)
    (cond ((memq input-shortfall '(6 4 3 1)) ;; needs padding?
           (setq input            (concat input (make-string input-shortfall ?=))
                 input-byte-count (+ input-shortfall input-byte-count)
                 output-shorten   (cond ((eq -6 input-shortfall) -4)
                                        ((eq -4 input-shortfall) -3)
                                        ((eq -3 input-shortfall) -2)
                                        ((eq -1 input-shortfall) -1)
                                        (t 0))))
          ((eq 0 input-shortfall) ;; already padded
           (setq output-shorten
                 (cond ((equal (substring input -6) "======") -4)
                       ((equal (substring input -4)   "====") -3)
                       ((equal (substring input -3)    "===") -2)
                       ((equal (substring input -1)      "=") -1)
                       (t 0))))
          (t (error "Invalid base32 payload length: %d" input-byte-count)))
    (setq output-byte-count (* (/ input-byte-count 8) 5)
          output            (make-string output-byte-count 0))
    (dotimes (i input-byte-count)
      (setq chunk (aref thesaurus (aref input i)))
      ;;(message "b32 de %03d %c 0x%02x" i (aref input i) chunk)
      (base32--nth-5bit-set output i chunk))
    (if (< output-shorten 0) (substring output 0 output-shorten) output)))

(provide 'base32)
;;; base32.el ends here
