;;; haskell-tng-testutils.el --- Test Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Miscellaneous testing utilities that are not required by the application.
;;
;;  We do not use `test-helper.el' because flycheck doesn't know to include it.
;;
;;; Code:

(require 'ert)
(require 'ert-x)
(require 'faceup)
(require 's)
(require 'shut-up)

(defun haskell-tng--testutils-assert-file-contents
    (file mode to-string suffix)
  "For FILE, enable MODE and run TO-STRING and compare with the golden data in FILE.SUFFIX.

Will fail and write out the expected version to FILE.SUFFIX.

Alternatively, if MODE is a buffer object, run TO-STRING there instead."
  (let* ((golden (concat file "." suffix))
         (expected (with-temp-buffer
                     (when (file-exists-p golden)
                       (insert-file-contents golden))
                     (buffer-string)))
         (got (cond
               ((bufferp mode)
                (with-current-buffer mode
                  (funcall to-string)))
               (t
                (with-temp-buffer
                  (insert-file-contents file)
                  (funcall mode)
                  (funcall to-string))))))
    (or (equal got expected)
        ;; writes out the new version on failure
        (progn
          (write-region got nil golden)
          nil))))

(defmacro testdata (file)
  (expand-file-name
   file
   (when load-file-name
     (file-name-directory load-file-name))))

(defun is-comment-at-point ()
  ;; this could be sped up by storing all comment regions in an alist
  (or (nth 8 (syntax-ppss))
      (looking-at "--")
      (and (looking-at "-")
           (looking-back "-" 1)))
  )

;; Not using `faceup-defexplainer' because it doesn't write over files.
(defun buffer-to-faceup-string ()
  (font-lock-fontify-region (point-min) (point-max))
  (faceup-markup-buffer))

(provide 'haskell-tng-testutils)
;;; haskell-tng-testutils.el ends here
