;; Copyright (C) 2015-2016 Codingteam  -*- lexical-binding: t; -*-

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'dash)

(if t (require 'cask "~/.cask/cask.el")) ;Don't load when compiling compile.el

(let ((bundle (cask-initialize default-directory)))
  (require 'bytecomp)
  (let* ((byte-compile-error-on-warn t)
         (load-path (cons (cask-path bundle) (cask-load-path bundle)))
         (compilation-failed
          (->> (cask-files bundle)
               (-filter (-lambda (file)
                          (and (file-regular-p file)
                               (equal (file-name-extension file) "el"))))
               ;; FIXME: Why not use #'byte-compile-file especially, since that
               ;; function doesn't accept the second arg we pass to it!
               (-map (-lambda (file)
                       (byte-compile-file file nil)))
               (-any #'null))))
    (cask-clean-elc bundle)
    (when compilation-failed
      (kill-emacs 1))))
