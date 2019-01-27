;;; haskell-tng-testutils.el --- Test Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2019 Tseen She
;; License: GPL 3 or any later version

;;; Commentary:
;;
;;  Miscellaneous testing utilities that are not required by the application.
;;
;;; Code:

(defmacro haskell-tng-testutils:this-lisp-directory ()
  (expand-file-name
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory)))

(defun haskell-tng-testutils:assert-file-contents
  (file mode to-string suffix)
  "For FILE, enable MODE and run TO-STRING and compare with the golden data in FILE.SUFFIX.

Will fail and write out the expected version to FILE.SUFFIX."
  (let* ((golden (concat file "." suffix))
         (expected (with-temp-buffer
                     (insert-file-contents golden)
                     (buffer-string)))
         (got (with-temp-buffer
                  (insert-file-contents file)
                  (funcall mode)
                  (funcall to-string))))
    (or (equal got expected)
        ;; writes out the new version on failure
        (progn
          (write-region got nil golden)
          nil))))

(defun testdata (file)
  (expand-file-name
   file
   (haskell-tng-testutils:this-lisp-directory)))

(provide 'haskell-tng-testutils)
;;; haskell-tng-testutils.el ends here
