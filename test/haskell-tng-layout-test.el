;;; haskell-tng-layout-test.el --- Tests for significant whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Tseen She
;; License: GPL 3 or any later version

(require 'haskell-tng-mode)

(require 'dash)
(require 'ert)
(require 's)

(defun haskell-tng-layout-test:parse-to-string ()
  (goto-char 0)
  (let (tokens)
   (while (not (eobp))
     (when-let (virtuals (haskell-tng-layout:virtuals-at-point))
       (push (s-join "" virtuals) tokens))
     (push (string (char-after)) tokens)
     (forward-char))
   (s-join "" (reverse tokens))))

;; TODO share principle with SMIE (and maybe faceup) tests
(defun have-expected-layout (file)
  (let* ((backup-inhibited t)
         (filename (expand-file-name
                    file
                    (haskell-tng:this-lisp-directory)))
         (golden (concat filename ".layout"))
         (expected (with-temp-buffer
                     (insert-file-contents golden)
                     (buffer-string)))
         (got (with-temp-buffer
                  (insert-file-contents filename)
                  ;; TODO mode should be a parameter
                  (haskell-tng-mode)
                  (haskell-tng-layout-test:parse-to-string))))
    (or (equal got expected)
        ;; TODO make this a setting
        ;; writes out the new version on failure
        (progn
          (write-region got nil golden)
          nil))))

(ert-deftest haskell-tng-layout-file-tests ()
  ;; the Haskell2010 test case
  (should (have-expected-layout "src/layout.hs"))

  (should (have-expected-layout "src/medley.hs"))
  )

;;; haskell-tng-layout-test.el ends here
