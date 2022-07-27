;;; devhelp.el --- Browse documentation in Devhelp format  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-07-26
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://codeberg.org/akib/emacs-devhelp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; In this unfortunate world, many documentations are using various HTML
;; based format, instead of using the excellent Texinfo and Info format.
;; This makes integrating of these manuals with Emacs hard, although not
;; impossible.

;; This package make tries to integrate one of those stupid formats,
;; Devhelp, with Emacs.

;; Usage
;; -----

;; M-x devhelp and you are good to go.  But you use a system that isn't FHS
;; (Filesystem Hierarchy Standard) compliant, then you would need to change
;; it.  For example, you have to put the following in the init file for GNU
;; Guix:

;;     (setq devhelp-search-directories
;;           '("/run/current-system/profile/share/doc/"
;;             "/run/current-system/profile/share/gtk-doc/html/"
;;             "~/.guix-profile/share/doc/"
;;             "~/.guix-profile/share/gtk-doc/html/"))

;; You can also bookmark pages, with the standard `bookmark-set' function.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dom)
(require 'shr)
(require 'bookmark)

(defgroup devhelp nil
  "Browse documentation in Devhelp format."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-devhelp")
  :prefix "devhelp-")

(defcustom devhelp-use-variable-pitch-font t
  "Non-nil means use variable pitch (proportional) font."
  :type 'boolean)

(defcustom devhelp-text-width nil
  "Window width to use for HTML rendering.

Integer means use that many columns.  Nil means use full window width."
  :type '(choice (integer :tag "Fixed width in characters")
		 (const :tag "Use the width of the window" nil)))

(defcustom devhelp-toc-group-books-by-language t
  "Non-nil means group books by language in table of contents."
  :type 'boolean)

(defcustom devhelp-index-group-keywords-by-type t
  "Non-nil means group keywords by type in index."
  :type 'boolean)

(defcustom devhelp-search-directories
  '("/usr/share/doc/" "/usr/share/gtk-doc/html/" "/usr/local/share/doc/"
    "/usr/local/share/gtk-doc/html/")
  "List of directories to search for Devhelp books.

Note that on GNU Guix, Nix or other FHS (Filesystem Hierarchy Standard)
non-compliant distributions, the default value won't work.  For GNU Guix,
set it to '(\"/run/current-system/profile/share/doc/\"
\"/run/current-system/profile/share/gtk-doc/html/\"
\"~/.guix-profile/share/doc/\" \"~/.guix-profile/share/gtk-doc/html/\")."
  :type '(repeat directory))

(defvar devhelp--books nil
  "Necessary data extracted from Devhelp file.

Each element is a list returned by `devhelp--parse-devhelp-file'.")

(defvar devhelp--history nil
  "History of pages visited by the user.

The value is of form (CURRENT . STACK), where STACK is the stack of user
visited pages, and CURRENT is the index of current page in STACK.  Each
element of STACK is a list of form (TITLE FILE BUFFERPOS), where TITLE is
the title of the page, FILE is the FILE name followed by optional target
\(see `url-generic-parse-url') and BUFFERPOS is the position in buffer.
FILE can also be the symbol `toc', meaning the table of contents page.")

(defun devhelp--parse-devhelp-file (file)
  "Parse Devhelp file FILE and return book data.

Return a list of form (TITLE NAME LANGUAGE PATH CHAPTERS KEYWORDS), where
TITLE is the title of book, NAME is the name of the book, LANGUAGE is the
 programming language name, PATH is the absolute path to it, CHAPTER is a
list of form (SECTION...) and KEYWORDS is a list of form (KEYWORD...).
SECTION is a list of form (NAME PATH SUB-SECTIONS...), where NAME is the
name of the section, PATH is the absolute path to the file and SUB-SECTION
is a list of form (SECTION...).  KEYWORD is a list of (NAME TYPE PATH),
where NAME  the keyword name, TYPE is the type of keyword and PATH is the
absolute path to it."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (let* ((dom (with-temp-buffer
                (insert-file-contents file)
                (libxml-parse-xml-region (point-min) (point-max))))
         (version (pcase (if (string= (file-name-extension file) "gz")
                             (file-name-extension
                              (file-name-sans-extension file))
                           (file-name-extension file))
                    ("devhelp" 1)
                    ("devhelp2" 2)
                    (_ (error "Invalid Devhelp file"))))
         (base (expand-file-name (or (dom-attr dom 'base) "")
                                 (file-name-directory file))))
    (cl-labels ((children-by-tag
                 (tree tag)
                 (mapcan (lambda (node)
                           (when (and (listp node) (eq (dom-tag node) tag))
                             (list node)))
                         (dom-children tree)))
                (process-section
                 (sec)
                 `(,(dom-attr sec 'name)
                   ,(expand-file-name (dom-attr sec 'link) base)
                   ,(mapcar #'process-section (children-by-tag
                                               sec 'sub)))))
      `(,(or (dom-attr dom 'title) "Untitled")
        ,(or (dom-attr dom 'name) (file-name-base file))
        ,(or (dom-attr dom 'language) "any")
        ,(expand-file-name (dom-attr dom 'link) base)
        ,(mapcar #'process-section
                 (mapcan (lambda (tree) (children-by-tag tree 'sub))
                         (children-by-tag dom 'chapters)))
        ,(mapcar
          (lambda (kw)
            `(,(dom-attr kw 'name)
              ,(or (dom-attr kw 'type) "function")
              ,(expand-file-name (dom-attr kw 'link) base)))
          (mapcan (lambda (tree)
                    (children-by-tag
                     tree (if (eq version 1) 'function 'keyword)))
                  (children-by-tag dom 'functions)))))))

(defun devhelp--search-for-books ()
  "Search for Devhelp books in `devhelp-search-directories'.

Return a list of book data returned by `devhelp--parse-devhelp-file' for
each valid Devhelp books."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (let ((books nil))
    (dolist (path devhelp-search-directories)
      (when (file-directory-p path)
        (dolist (dir (directory-files path))
          (when (and (file-directory-p (expand-file-name dir path))
                     (not (member dir '("." ".."))))
            (catch 'stop
              (dolist (suffix '(".devhelp2" ".devhelp2.gz"
                                ".devhelp" ".devhelp.gz"))
                (let ((file (expand-file-name
                             (concat dir suffix)
                             (expand-file-name dir path))))
                  (when (file-regular-p file)
                    (ignore-errors
                      (push (devhelp--parse-devhelp-file file) books)
                      (throw 'stop nil))))))))))
    (sort books (lambda (a b) (string< (car a) (car b))))))

(defun devhelp--file-to-url (file)
  "Convert path FILE to URL and return it."
  (concat "file://" (and (memq system-type '(windows-nt ms-dos)) "/")
          file))

(defun devhelp-toc ()
  "Open table of contents, of all books.

If a single file was opened, only show that book's table of contents."
  (interactive)
  (when (cdr devhelp--history)
    (if (eq (nth 1 (nth (car devhelp--history) (cdr devhelp--history)))
            'history)
        (setf (nthcdr (car devhelp--history) (cdr devhelp--history))
              (nthcdr (1+ (car devhelp--history)) (cdr devhelp--history)))
      (setf (nth 2 (nth (car devhelp--history) (cdr devhelp--history)))
            (point))
      (setf (cdr devhelp--history) (nthcdr (car devhelp--history)
                                           (cdr devhelp--history)))))
  (setf (car devhelp--history) 0)
  (push (list nil 'toc (point-min)) (cdr devhelp--history))
  (devhelp--toc-1)
  (goto-char (point-min)))

(defun devhelp--toc-1 ()
  "Show table of contents.

See `devhelp-toc' for more details."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "<html><head><title>Table of contents</title></head><body><ul>"
     (let ((book-tocs
            (mapcar
             (lambda (book)
               (cl-labels ((section-to-html
                            (section)
                            (concat
                             "<li>"
                             (format
                              "<a href=%S>%s</a>"
                              (devhelp--file-to-url (nth 1 section))
                              (nth 0 section))
                             (when (nth 2 section)
                               (format
                                "<ul>%s</ul>"
                                (mapconcat #'section-to-html
                                           (nth 2 section) "")))
                             "</li>")))
                 (cons
                  (nth 2 book)
                  (concat
                   (format
                    "<b><a href=%S>%s</a></b>"
                    (devhelp--file-to-url (nth 3 book)) (nth 0 book))
                   (when (nth 4 book)
                     (format
                      "<ul>%s</ul>"
                      (mapconcat #'section-to-html (nth 4 book) "")))))))
             devhelp--books)))
       (if (not devhelp-toc-group-books-by-language)
           (mapconcat (lambda (toc) (format "<li>%s</li>" (cdr toc)))
                      book-tocs "")
         (let ((groups nil))
           (dolist (toc book-tocs)
             (if-let ((pair (assoc (car toc) groups)))
                 (setf (cdr pair) (nconc (cdr pair) (list (cdr toc))))
               (push (cons (car toc) (list (cdr toc))) groups)))
           (setq groups (sort groups (lambda (a b)
                                       (string< (car a) (car b)))))
           (mapconcat
            (lambda (group)
              (when (cdr group)
                (format
                 "<li><b><u>Language: %s</u></b><ul>%s</ul></li>"
                 (capitalize (car group))
                 (mapconcat (lambda (toc) (format "<li>%s</li>" toc))
                            (cdr group) ""))))
            groups ""))))
     "</ul></body></html>")
    (devhelp--render-html)
    (goto-char (point-min))))

(defun devhelp-index ()
  "Show index, of all books.

If a single file was opened, only show that book's index."
  (interactive)
  (when (cdr devhelp--history)
    (if (eq (nth 1 (nth (car devhelp--history) (cdr devhelp--history)))
            'history)
        (setf (nthcdr (car devhelp--history) (cdr devhelp--history))
              (nthcdr (1+ (car devhelp--history)) (cdr devhelp--history)))
      (setf (nth 2 (nth (car devhelp--history) (cdr devhelp--history)))
            (point))
      (setf (cdr devhelp--history) (nthcdr (car devhelp--history)
                                           (cdr devhelp--history)))))
  (setf (car devhelp--history) 0)
  (push (list nil 'index (point-min)) (cdr devhelp--history))
  (devhelp--index-1)
  (goto-char (point-min)))

(defun devhelp--index-1 ()
  "Show table of contents.

See `devhelp-index' for more details."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert
     "<html><head><title>Index</title></head><body><ul>"
     (let ((keywords (mapcan (lambda (book)
                            (copy-sequence (nth 5 book)))
                          devhelp--books)))
       (sort keywords (lambda (a b) (string< (car a) (car b))))
       (if (not devhelp-index-group-keywords-by-type)
           (mapconcat (lambda (keyword)
                        (format "<li>%s <a href=%S>%s</a></li>"
                                (capitalize (nth 1 keyword))
                                (devhelp--file-to-url (nth 2 keyword))
                                (nth 0 keyword)))
                      keywords "")
         (let ((groups nil))
           (dolist (keyword keywords)
             (let ((entry (format "<a href=%S>%s</a>"
                                  (devhelp--file-to-url (nth 2 keyword))
                                  (nth 0 keyword))))
               (if-let ((pair (assoc (nth 1 keyword) groups)))
                   (setf (cdr pair) (nconc (cdr pair) (list entry)))
                 (push (cons (nth 1 keyword) (list entry)) groups))))
           (setq groups (sort groups (lambda (a b)
                                       (string< (car a) (car b)))))
           (mapconcat
            (lambda (group)
              (when (cdr group)
                (format
                 "<li><b><u>Type: %s</u></b><ul>%s</ul></li>"
                 (capitalize (car group))
                 (mapconcat (lambda (entry) (format "<li>%s</li>" entry))
                            (cdr group) ""))))
            groups ""))))
     "</ul></body></html>")
    (devhelp--render-html)
    (goto-char (point-min))))

(defun devhelp--set-title (title)
  "Set the title of current page to TITLE."
  (setq header-line-format
        (when title (replace-regexp-in-string "%" "%%" title)))
  (setf (car (nth (car devhelp--history) (cdr devhelp--history))) title))

(defun devhelp-browse-url (&optional event)
  "Follow link under point/mouse.

EVENT is a mouse event, if any."
  (interactive (list last-nonmenu-event))
  (mouse-set-point event)
  (let ((link (get-text-property (point) 'shr-url)))
    (devhelp--browse-url-1 link)))

(defun devhelp--browse-url-1 (link)
  "Goto url specified in LINK."
  (if (not (stringp link))
      (user-error "No link under point")
    (if-let ((url (url-generic-parse-url link))
             ((string= (url-type url) "file"))
             (file (url-filename url)))
        (progn
          (when (cdr devhelp--history)
            (setf (nth 2 (nth (car devhelp--history)
                              (cdr devhelp--history)))
                  (point))
            (setf (cdr devhelp--history)
                  (nthcdr (car devhelp--history)
                          (cdr devhelp--history))))
          (setf (car devhelp--history) 0)
          (push (list nil (url-recreate-url (url-parse-make-urlobj
                                             nil nil nil nil nil file
                                             (url-target url)))
                      (point-min))
                (cdr devhelp--history))
          (devhelp--set-title "Untitled")
          (devhelp--render-html-file
           (if (memq system-type '(windows-nt ms-dos))
               (substring file 1)
             file))
          (goto-char (point-min))
          (when (and (url-target url)
                     (not (string-empty-p (url-target url))))
            (text-property-search-forward 'shr-target-id (url-target url)))
          (recenter 0)
          (setf (nth 2 (nth 0 (cdr devhelp--history))) (point)))
      (browse-url url))))

(defun devhelp--render-html (&optional base)
  "Render HTML in current buffer.

When BASE is given, use it to make relative URLs absolute."
  (let ((shr-map
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map shr-map)
           (let ((fn (if (eq (nth 1 (nth (car devhelp--history)
                                         (cdr devhelp--history)))
                             'history)
                         #'devhelp-history-goto #'devhelp-browse-url)))
             (define-key map (kbd "RET") fn)
             (define-key map [mouse-2] fn))
           map))
        (shr-external-rendering-functions
         (append `((title
                    . ,(lambda (dom)
                         (devhelp--set-title (car (dom-children dom))))))
                 shr-external-rendering-functions))
        (shr-use-fonts devhelp-use-variable-pitch-font)
        (shr-width devhelp-text-width)
        (dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document (if base `(base ((href . ,base)) (,dom)) dom))
    (goto-char (point-min))))

(defun devhelp--render-html-file (file)
  "Load and render HTML file FILE in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents file)
    (devhelp--render-html (devhelp--file-to-url file))))

(defun devhelp--history-goto (n)
  "Goto Nth page in `devhelp--history'."
  (if (not (< -1 n (if (eq (nth 1 (nth (car devhelp--history)
                                       (cdr devhelp--history)))
                           'history)
                       (1- (length (cdr devhelp--history)))
                     (length (cdr devhelp--history)))))
      (user-error (concat (if (< n 0) "Beginning" "End") " of history"))
    (when (eq (nth 1 (nth (car devhelp--history) (cdr devhelp--history)))
              'history)
      (setf (nthcdr (car devhelp--history) (cdr devhelp--history))
            (nthcdr (1+ (car devhelp--history)) (cdr devhelp--history))))
    (setf (nth 2 (nth (car devhelp--history)
                      (cdr devhelp--history)))
          (point))
    (setf (car devhelp--history) n)
    (if (symbolp (nth 1 (nth n (cdr devhelp--history))))
        (pcase (nth 1 (nth n (cdr devhelp--history)))
          ('toc (devhelp--toc-1))
          ('index (devhelp--index-1)))
      (let* ((url (url-generic-parse-url
                   (devhelp--file-to-url
                    (nth 1 (nth n (cdr devhelp--history))))))
             (file (url-filename url)))
        (devhelp--render-html-file
         (if (memq system-type '(windows-nt ms-dos))
             (substring file 1)
           file))))
    (goto-char (nth 2 (nth n (cdr devhelp--history))))
    (recenter)))

(defun devhelp-history-back (&optional n)
  "Go to the previous page.

When prefix argument N is given, go to Nth previous page."
  (interactive "p")
  (devhelp--history-goto (+ (car devhelp--history) n)))

(defun devhelp-history-forward (&optional n)
  "Go to the next page.

When prefix argument N is given, go to Nth next page."
  (interactive "p")
  (devhelp--history-goto (- (car devhelp--history) n)))

(defun devhelp-history-goto (&optional event)
  "Go to history link under point.

This only works in history page.

EVENT is a mouse event, if any."
  (interactive (list last-nonmenu-event))
  (mouse-set-point event)
  (unless (eq (nth 1 (nth (car devhelp--history)
                          (cdr devhelp--history)))
              'history)
    (user-error "Not in history page"))
  (devhelp--history-goto
   (string-to-number (get-text-property (point) 'shr-url))))

(defun devhelp-history ()
  "Show history."
  (interactive)
  (setf (nth 2 (nth (car devhelp--history) (cdr devhelp--history)))
        (point))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "<html><head><title>History of visited pages</title></head>"
            "<body><h1>History of visited pages</h1><ul>")
    (dolist (i (number-sequence 0 (1- (length (cdr devhelp--history)))))
      (insert (format "<li><a href=\"%i\">%s</a></li>" i
                      (let ((title (car (nth i (cdr devhelp--history)))))
                        (if (eq i (car devhelp--history))
                            (format "<i>%s</i>" title)
                          title)))))
    (insert "</ul></body></html>")
    (push (list nil 'history (point-min)) (nthcdr (car devhelp--history)
                                                  (cdr devhelp--history)))
    (devhelp--render-html)))

(defun devhelp--directory ()
  "List all available Devhelp books."
  (message "Composing Devhelp directory...")
  (setq devhelp--books (devhelp--search-for-books))
  (devhelp-toc)
  (message "Composing Devhelp directory...done"))

(defun devhelp--open-file (file)
  "Open FILE in current buffer."
  (setq devhelp--books (list (devhelp--parse-devhelp-file file)))
  (devhelp--browse-url-1 (devhelp--file-to-url
                          (nth 3 (car devhelp--books)))))

;;;###autoload
(defun devhelp (file buffer)
  "Browse documentation in Devhelp format.

Interactively, when a non-numeric prefix argument is given, the Devhelp
file name is read interactively from the minibuffer.  When a numeric
argument N is given, a buffer named \"*devhelp*<N>\" is selected.

Optional argument BUFFER specifies the BUFFER to use, it can be a live
buffer or a buffer name.  If BUFFER is a buffer name and the buffer doesn't
exist, it is created.

Optional argument FILE specifies the file to open, the default is to open
the conbined table of contents of all available Devhelp books."
  (interactive
   (list (when (and current-prefix-arg (not (numberp current-prefix-arg)))
           (read-file-name "Devhelp file name: " nil nil t))
         (when (numberp current-prefix-arg)
           (format "*devhelp*<%s>" current-prefix-arg))))
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (get-buffer-create (or buffer "*devhelp*"))
    (devhelp-mode)
    (display-buffer (current-buffer))
    (if (not file)
        (devhelp--directory)
      (devhelp--open-file file))))

(defun devhelp--make-bookmark-record ()
  "Make a bookmark record."
  (let ((entry (nth (car devhelp--history) (cdr devhelp--history))))
    (when (eq (nth 1 entry) 'history)
      (error "Can't bookmark history page"))
    `(,(nth 0 entry)
      (filename . ,(when (stringp (nth 1 entry)) (nth 1 entry)))
      (file . ,(nth 1 entry))
      (position . ,(nth 2 entry))
      (handler . devhelp-bookmake-jump))))

;;;###autoload
(defun devhelp-bookmake-jump (bookmark)
  "Jump to BOOKMARK."
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (generate-new-buffer "*devhelp*")
    (devhelp-mode)
    (display-buffer (current-buffer))
    (setq devhelp--books (devhelp--search-for-books))
    (setq devhelp--history
          `(0 . ((nil ,(bookmark-prop-get bookmark 'file)
                      ,(bookmark-prop-get bookmark 'position)))))
    (if (symbolp (bookmark-prop-get bookmark 'file))
        (pcase (bookmark-prop-get bookmark 'file)
          ('toc (devhelp--toc-1))
          ('index (devhelp--index-1)))
      (let ((file (url-filename
                   (url-generic-parse-url
                    (devhelp--file-to-url
                     (bookmark-prop-get bookmark 'file))))))
        (devhelp--set-title "Untitled")
        (devhelp--render-html-file
         (if (memq system-type '(windows-nt ms-dos))
             (substring file 1)
           file))
        (goto-char (min (max (bookmark-prop-get bookmark 'position)
                             (point-min))
                        (point-max)))
        (recenter 0)
        (setf (nth 2 (nth 0 (cdr devhelp--history))) (point))))))

(defvar devhelp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'devhelp-index)
    (define-key map (kbd "d") #'devhelp-toc)
    (define-key map (kbd "l") #'devhelp-history-back)
    (define-key map (kbd "r") #'devhelp-history-forward)
    (define-key map (kbd "L") #'devhelp-history)
    map)
  "Keymap for Devhelp mode.")

(define-derived-mode devhelp-mode special-mode "Devhelp"
  "Major mode for browsing Devhelp books."
  (setq-local devhelp--books nil)
  (setq-local devhelp--history (copy-sequence '(nil . nil)))
  (setq-local bookmark-make-record-function
              #'devhelp--make-bookmark-record))

(provide 'devhelp)
;;; devhelp.el ends here
