;;; workroom.el --- Named rooms for work without irrelevant distracting buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Version: 2.3.1
;; Package-Requires: ((emacs "25.1") (project "0.3.0") (compat "28.1.2.2"))
;; Keywords: tools, convenience
;; URL: https://codeberg.org/akib/emacs-workroom

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

;; Workroom provides named "workrooms" (or workspaces), somewhat
;; similar to multiple desktops in GNOME.

;; Each workroom has own set of buffers, allowing you to work on
;; multiple projects without getting lost in all buffers.

;; Each workroom also has its own set of views.  Views are just named
;; window configurations.  They allow you to switch to another window
;; configuration without losing your well-planned window setup.

;; You can also bookmark a workroom to restore them at a later time,
;; possibly in another Emacs session.  You can also save your
;; workrooms in your desktop.

;; Usage
;; ═════

;; There is always a workroom named "master", which contains all live
;; buffers.  Removing any buffer from this workroom kills that buffer.
;; You can't kill this workroom, but you can customize the variable
;; `workroom-default-room-name' to change its name.

;; All the useful commands can be called with following key sequences:

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;  Key          Command
;; ───────────────────────────────────────────
;;  `C-x c s'    `workroom-switch'
;;  `C-x c S'    `workroom-switch-view'
;;  `C-x c d'    `workroom-kill'
;;  `C-x c D'    `workroom-kill-view'
;;  `C-x c C-d'  `workroom-kill-with-buffers'
;;  `C-x c r'    `workroom-rename'
;;  `C-x c R'    `workroom-rename-view'
;;  `C-x c c'    `workroom-clone'
;;  `C-x c C'    `workroom-clone-view'
;;  `C-x c m'    `workroom-bookmark'
;;  `C-x c M'    `workroom-bookmark-multiple'
;;  `C-x c b'    `workroom-switch-to-buffer'
;;  `C-x c a'    `workroom-add-buffer'
;;  `C-x c k'    `workroom-kill-buffer'
;;  `C-x c K'    `workroom-remove-buffer'
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Here the prefix key sequence is `C-x c', but you can customize
;; `workroom-command-map-prefix' to change it.

;; You might want to remap ~switch-to-buffer~, ~kill-buffer~ and other
;; commands with Workroom-aware commands by adding something like the
;; following to your init file:

;; ┌────
;; │ (global-set-key [remap switch-to-buffer]
;; │                 #'workroom-switch-to-buffer)
;; │ (global-set-key [remap kill-buffer] #'workroom-kill-buffer)
;; └────

;; You can save all your workroom in your desktop by enabling
;; `workroom-desktop-save-mode' mode.

;; You can create a workroom containing only your project buffer with
;; `workroom-switch-to-project-workroom'.  You can also enable
;; `workroom-auto-project-workroom-mode', it'll switch to (creating if
;; needed) the project's workroom when you open a file.

;; If you want to completely automate managing workroom buffer list,
;; check out the docstrings of `workroom-buffer-manager-function',
;; `workroom-set-buffer-manager-function' and
;; `workroom-buffer-manager-data'.

;;; Code:

(require 'cl-lib)
(require 'bookmark)
(require 'project)
(require 'compat)


;;;; User Options.

(defgroup workroom nil
  "Named rooms for work without irrelevant distracting buffers."
  :group 'convenience
  :group 'tools
  :prefix "workroom-"
  :link '(url-link "https://codeberg.org/akib/emacs-workroom"))

(defcustom workroom-command-map-prefix (kbd "C-x c")
  "Prefix key of Workroom commands.

Customizing this variable automatically takes effects.  However,
changing from Lisp program doesn't immediately take effect.  Call
`workroom-rebind-command-map-prefix' for changes to take effect.
Alternatively you can re-enable Workroom mode which will do that for
you."
  :type 'key-sequence
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'workroom-rebind-command-map-prefix)
           (workroom-rebind-command-map-prefix))))

(defcustom workroom-default-room-name "master"
  "Name of the default workroom.

This workroom contains all live buffers of the current Emacs session.

Workroom mode must be reenabled for changes to take effect, or the
name can be manually changed with `workroom-rename'."
  :type 'string)

(defcustom workroom-default-view-name "main"
  "Name of the default view."
  :type 'string)

(defcustom workroom-default-buffer-name "*scratch*"
  "Name of the buffer used as the default fallback buffer."
  :type 'string)

(defcustom workroom-buffer-handler-alist
  '((bookmark :encoder workroom-encode-buffer-bookmark
              :decoder workroom-decode-buffer-bookmark))
  "Alist of functions to encode/decode buffer to/from readable object.

Each element of the list is of the form (IDENTIFIER . (:encoder
ENCODER :decoder DECODER)), where ENCODE is a function to encode
buffer to writable object and DECODER is a function to decode a
writable object returned by ENCODER and create the corresponding
buffer.  ENCODER is called with a single argument BUFFER, where BUFFER
is the buffer to encode.  It should return nil if it can't encode
BUFFER.  DECODER is called with a single argument OBJECT, where OBJECT
is the object to decode.  It should not modify window configuration.
IDENTIFIER is used to get the appropiate decoder function for a
object.

Each element of the list tried to encode a buffer.  When no encoder
function can encode the buffer, the buffer is not saved.

NOTE: If you change IDENTIFIER, all buffers encoded with the previous
value can't restored."
  :type '(alist
          :key-type (symbol :tag "Identifier")
          :value-type (list (const :encoder)
                            (function :tag "Encoder function")
                            (const :decoder)
                            (function :tag "Decoder function"))))

(defcustom workroom-mode-lighter
  '(:eval
    (let ((face (if (workroom-member-buffer-p
                     (workroom-current-room) (current-buffer))
                    'compilation-info
                  'warning)))
      `(" WR["
        (:propertize ,(workroom-name (workroom-current-room))
                     face ,face)
        "]["
        (:propertize ,(workroom-view-name (workroom-current-view))
                     face ,face)
        "]")))
  "Format of Workroom mode lighter.

The value is a mode line terminal like `mode-line-format'."
  :type 'sexp)

(defcustom workroom-mode-hook nil
  "Normal hook run when toggling Workroom mode."
  :type 'hook)

(defcustom workroom-before-switch-hook nil
  "Normal hook run before switching workroom or view."
  :type 'hook)

(defcustom workroom-switch-hook nil
  "Normal hook run after switching workroom or view."
  :type 'hook)

(defvar workroom-command-map
  (let ((keymap (make-sparse-keymap)))
    ;; NOTE: Be sure to keep the commentary and README up to date.
    (define-key keymap (kbd "s") #'workroom-switch)
    (define-key keymap (kbd "S") #'workroom-switch-view)
    (define-key keymap (kbd "d") #'workroom-kill)
    (define-key keymap (kbd "D") #'workroom-kill-view)
    (define-key keymap (kbd "C-d") #'workroom-kill-with-buffers)
    (define-key keymap (kbd "r") #'workroom-rename)
    (define-key keymap (kbd "R") #'workroom-rename-view)
    (define-key keymap (kbd "c") #'workroom-clone)
    (define-key keymap (kbd "C") #'workroom-clone-view)
    (define-key keymap (kbd "m") #'workroom-bookmark)
    (define-key keymap (kbd "M") #'workroom-bookmark-multiple)
    (define-key keymap (kbd "b") #'workroom-switch-to-buffer)
    (define-key keymap (kbd "a") #'workroom-add-buffer)
    (define-key keymap (kbd "k") #'workroom-kill-buffer)
    (define-key keymap (kbd "K") #'workroom-remove-buffer)
    keymap)
  "Keymap containing all useful commands of Workroom.")

(defvar workroom-mode-map (make-sparse-keymap)
  "Keymap for Workroom mode.")

(defun workroom-rebind-command-map-prefix ()
  "Rebind command prefix key sequence `workroom-command-map-prefix'."
  (substitute-key-definition
   workroom-command-map nil workroom-mode-map)
  (define-key workroom-mode-map workroom-command-map-prefix
              workroom-command-map))


;;;; Workroom and View Manipulation.

(cl-defstruct (workroom--room
               (:constructor workroom--make-room)
               (:copier workroom--copy-room))
  "Structure for workroom."
  (name nil :documentation "Name of the workroom.")
  (buffer-manager
   nil
   :documentation "The function handling the buffer list.")
  (buffer-manager-data
   nil
   :documentation "The data stored by the buffer manager function.")
  (view-list nil :documentation "List of views of the workroom.")
  (default-p
   nil
   :documentation "Whether the workroom is the default one.")
  (view-history
   nil
   :documentation "`completing-read' history of view names."))

(cl-defstruct (workroom--view
               (:constructor workroom--make-view)
               (:copier workroom--copy-view))
  "Structure for view of workroom."
  (name nil :documentation "Name of the view.")
  (window-config
   nil
   :documentation "Window configuration of the view.")
  (window-config-writable
   nil
   :documentation "Writable window configuration of the view.")
  (frame nil :documentation "The frame showing the view, or nil."))

(defvar workroom--dont-clear-new-view nil
  "Non-nil mean don't clear empty new views.")

(defvar workroom--rooms nil
  "List of currently live workrooms.")

(defvar workroom-room-history nil
  "`completing-read' history list of workroom names.")

(defvar workroom--view-history nil
  "`completing-read' history list of workroom view names.

This is let-bound before using, the history is saved into the
workroom's view-history slot.  Use `workroom-view-history' to access
that.")

(defvar workroom-mode)

(defun workroomp (object)
  "Return non-nil if OBJECT is a workroom object."
  (workroom--room-p object))

(defun workroom-name (room)
  "Return the name of workroom ROOM."
  (workroom--room-name room))

(defun workroom-live-p (room)
  "Return t if ROOM is a live workroom."
  (not (not (workroom-name room))))

(defun workroom-buffer-manager-function (room)
  "Return the function to manage the member buffers of workroom ROOM."
  (workroom--room-buffer-manager room))

(defun workroom-set-buffer-manager-function
    (room function &optional do-not-initialize &rest args)
  "Set the buffer manager function of workroom ROOM.

FUNCTION is the buffer manager function and ARGS is the arguments to
it initialization procedure.  Call FUNCTION with ROOM, `:initialize',
followed by ARGS, unless DO-NOT-INITIALIZE is non-nil.

FUNCTION is a function taking two or more arguments.  The function
shouldn't be an uninterned symbol or lambda/closure.  The first
argument is ROOM, the workroom.  The second one is ACTION, it specify
what to do.  ACTION can any of:

`:initialize'
  Do initialization for workroom ROOM.  Element of ARGS is passed as
  extra arguments in proper order.

`:list-buffers'
  List of member buffers of workroom ROOM.  No extra arguments.

`:add-buffer'
  Add BUFFER as a member of workroom ROOM.  BUFFER is the third
  argument.

`:remove-buffer'
  Remove BUFFER from the member list of workroom ROOM.  BUFFER is the
  third argument.

`:member-buffer-p'
  Return non-nil if BUFFER is a member buffer of workroom ROOM.
  BUFFER is the third argument.

`:clone'
  Clone buffer list from workroom SOURCE to workroom ROOM.  SOURCE is
  the third argument is a workroom.  `:initialize' is not called on
  ROOM, the function must do the initialization itself if required.

`:encode'
  Encode the buffer manager data and return it.  No extra arguments.
  DATA is the writable encoded buffer manager data.  DATA is passed as
  the third argument of ACTION `:load' to load the data.

`:load'
  Load the data previously encoded with `:encode'.  The third argument
  is the encoded data DATA that ACTION `:encode' returned.  The fourth
  argument is the list of buffers to add to it, BUFFERS.  BUFFERS
  contains some or all of the buffers, that were member of the
  workroom ACTION `:encode' was called with, just after the call.
  `:initialize' is not called on ROOM, the function must do the
  initialization itself if required.

To set it, use (`setf' (`workroom-buffer-manager-function' ROOM)
FUNCTION), where FUNCTION is the buffer manager function."
  (when (workroom-default-p room)
    (error "Cannot change buffer manager of the default workroom"))
  (setf (workroom--room-buffer-manager room) function)
  (unless do-not-initialize
    (apply function room :initialize args)))

(defun workroom-buffer-manager-data (room)
  "Return the data stored by the buffer manager of workroom ROOM.

This is reserved for the buffer manager of ROOM, this should be used
by only the buffer manager and associated stuffs.

To set it, use (`setf' (`workroom-buffer-manager-data' ROOM) DATA),
where DATA is the data to store.  The data can be modified with side
effect, it is not unaltered."
  (workroom--room-buffer-manager-data room))

(gv-define-setter workroom-buffer-manager-data (function room)
  `(setf (workroom--room-buffer-manager-data ,room) ,function))

(defun workroom-view-list (room)
  "Return the views of workroom ROOM."
  (workroom--room-view-list room))

(defun workroom-default-p (room)
  "Return non-nil if workroom ROOM is the default workroom."
  (workroom--room-default-p room))

(defun workroom-view-history (room)
  "Completing read history of view of workroom ROOM."
  (workroom--room-view-history room))

(defun workroom-view-p (object)
  "Return non-nil if OBJECT is a view object."
  (workroom--view-p object))

(defun workroom-view-name (view)
  "Return the name of view VIEW."
  (workroom--view-name view))

(defun workroom-view-live-p (room)
  "Return t if ROOM is a live view."
  (not (not (workroom-view-name room))))

(defun workroom-view-window-configuration (view &optional writable)
  "Return the window configuration of view VIEW.

If WRITABLE is non-nil, return a window configuration that can be
written to a string (or file) and read back.

This is expensive, because it can recalculate the window configuration
and returns a copy of it."
  (when (frame-live-p (workroom--view-frame view))
    (setf (workroom--view-window-config (workroom-current-view))
          (workroom--frame-window-config
           (workroom--view-frame view)))
    (setf (workroom--view-window-config-writable
           (workroom-current-view))
          (workroom--frame-window-config
           (workroom--view-frame view) 'writable)))
  (copy-tree (if writable
                 (workroom--view-window-config-writable view)
               (workroom--view-window-config view))))

(defun workroom-view-frame (view)
  "Return the frame showing the view VIEW, or nil if none."
  (let ((frame (workroom--view-frame view)))
    (when frame
      (if (frame-live-p frame)
          frame
        (setf (workroom--view-frame view) nil)
        nil))))

(defun workroom-list ()
  "Return the list of workrooms.

A copy is returned, so it can be modified with side-effects."
  (copy-sequence workroom--rooms))

(defun workroom-get (name)
  "Return the workroom named NAME.

If no such workroom exists, return nil."
  (cl-find name workroom--rooms
           :key #'workroom-name
           :test #'string=))

(defun workroom-get-create (name)
  "Return the workroom named NAME.

If no such workroom exists, create a new one named NAME and return
that."
  (let ((room (workroom-get name)))
    (unless room
      (setq room (workroom--make-room
                  :name name
                  :buffer-manager #'workroom--default-buffer-manager))
      (workroom--default-buffer-manager room :initialize)
      (push room workroom--rooms))
    room))

(defun workroom-get-default ()
  "Return the default workroom."
  (cl-find-if #'workroom-default-p workroom--rooms))

(defun workroom-generate-new-room-name (name)
  "Return a string that isn't the name of any workroom based on NAME.

If there is no live workroom named NAME, then return NAME.  Otherwise
modify NAME by appending `<NUMBER>', incrementing NUMBER (starting at
2) until an unused name is found, and then return that name."
  (if (not (workroom-get name))
      name
    (cl-block nil
      (let ((n 2))
        (while t
          (let ((str (format "%s<%i>" name n)))
            (when (not (workroom-get str))
              (cl-return str))
            (cl-incf n)))))))

(defun workroom-generate-new-room (name)
  "Create and return a workroom with a name based on NAME.

Choose the workroom's name using `workroom-generate-new-room-name'."
  (workroom-get-create (workroom-generate-new-room-name name)))

(defun workroom-view-get (room name)
  "Return the view of ROOM named NAME.

If no such view exists, return nil."
  (cl-find name (workroom-view-list room)
           :key #'workroom-view-name
           :test #'string=))

(defun workroom-view-get-create (room name)
  "Return the view of ROOM named NAME.

If no such view exists, create a new one named NAME and return that."
  (let ((view (workroom-view-get room name)))
    (unless view
      (setq view (workroom--make-view :name name))
      (setf (workroom--room-view-list room)
            (nconc (workroom--room-view-list room) (list view))))
    view))

(defun workroom-generate-new-view-name (room name)
  "Return a string that isn't the name of any view of ROOM.

If there is no live view named NAME in ROOM, then return NAME.
Otherwise modify NAME by appending `<NUMBER>', incrementing NUMBER
\(starting at 2) until an unused name is found, and then return that
name."
  (if (not (workroom-view-get room name))
      name
    (cl-block nil
      (let ((n 2))
        (while t
          (let ((str (format "%s<%i>" name n)))
            (unless (workroom-view-get room str)
              (cl-return str))
            (cl-incf n)))))))

(defun workroom-generate-new-view (room name)
  "Create and return a view of ROOM with a name based on NAME.

Choose the view's name using `workroom-generate-new-view-name'."
  (workroom-view-get-create
   room (workroom-generate-new-view-name room name)))

(defun workroom-buffer-list (room)
  "Return the buffer list of workroom ROOM."
  (funcall (workroom--room-buffer-manager room) room :list-buffers))

(defun workroom-member-buffer-p (room buffer)
  "Return non-nil if BUFFER is a member buffer of ROOM."
  (funcall (workroom--room-buffer-manager room)
           room :member-buffer-p buffer))

(defun workroom-current-room (&optional frame)
  "Return the current workroom of FRAME."
  (frame-parameter frame 'workroom-current-room))

(defun workroom-current-view (&optional frame)
  "Return the current view of FRAME."
  (frame-parameter frame 'workroom-current-view))

(defun workroom-previous-room-list (&optional frame)
  "Return the list of the previous workrooms of FRAME."
  (frame-parameter frame 'workroom-previous-room-list))

(defun workroom--read (prompt &optional def require-match predicate)
  "Read the name of a workroom and return it as a string.

Prompt with PROMPT, where PROMPT should be a string without trailing
colon and/or space.

Return DEF when input is empty, where DEF is either a string or nil.

REQUIRE-MATCH and PREDICATE is same as in `completing-read'."
  (completing-read
   (format-prompt prompt def) (mapcar #'workroom-name workroom--rooms)
   predicate require-match  nil 'workroom-room-history def))

(defun workroom--read-to-switch ( prompt &optional def require-match
                                  predicate)
  "Read the name of a workroom other than current one and return it.

See `workroom--read' for PROMPT, DEF, REQUIRE-MATCH and PREDICATE."
  (workroom--read
   prompt def require-match
   (lambda (cand)
     (and (not (equal (workroom-name (workroom-current-room))
                      (if (consp cand) (car cand) cand)))
          (or (not predicate) (funcall predicate cand))))))

(defun workroom--read-multiple ( prompt &optional def require-match
                                 predicate)
  "Read the name of some workrooms and return it as a list of strings.

Prompt with PROMPT, where PROMPT should be a string without trailing
colon and/or space.

Return DEF when input is empty, where DEF is either a string or nil.

REQUIRE-MATCH and PREDICATE is same as in `completing-read-multiple'."
  (completing-read-multiple
   (format-prompt prompt def) (mapcar #'workroom-name workroom--rooms)
   predicate require-match nil 'workroom-room-history def))

(defun workroom--read-view ( room prompt &optional def require-match
                             predicate)
  "Read the name of a view of ROOM and return it as a string.

Prompt with PROMPT, where PROMPT should be a string without trailing
colon and/or space.

Return DEF when input is empty, where DEF is either a string or nil.

REQUIRE-MATCH and PREDICATE is same as in `completing-read'."
  (let ((workroom--view-history (workroom-view-history room)))
    (prog1
        (completing-read
         (format-prompt prompt def)
         (mapcar #'workroom-view-name (workroom-view-list room))
         predicate require-match nil 'workroom-room-history def)
      (setf (workroom--room-view-history room)
            workroom--view-history))))

(defun workroom--read-view-to-switch ( room prompt &optional def
                                       require-match predicate)
  "Read the name of a non-current view of ROOM and return it.

See `workroom--read' for PROMPT, DEF, REQUIRE-MATCH and PREDICATE."
  (workroom--read-view
   room prompt def require-match
   (if (eq room (workroom-current-room))
       (lambda (cand)
         (and (not (equal (workroom-view-name (workroom-current-view))
                          (if (consp cand) (car cand) cand)))
              (or (not predicate) (funcall predicate cand))))
     predicate)))

(defun workroom--read-member-buffer ( room prompt &optional def
                                      require-match predicate)
  "Read the name of a member buffer of ROOM.

ROOM should be a `workroom'.  Prompt with PROMPT, where PROMPT should
be a string.  DEF, REQUIRE-MATCH and PREDICATE is same as in
`read-buffer'."
  (let ((read-buffer-function nil))
    (read-buffer
     prompt def require-match
     (lambda (cand)
       (and (workroom-member-buffer-p
             room (get-buffer (if (consp cand) (car cand) cand)))
            (or (not predicate) (funcall predicate cand)))))))

(defun workroom--read-non-member-buffer ( room prompt &optional def
                                          require-match predicate)
  "Read the name of a buffer which isn't a member of ROOM.

ROOM should be a `workroom'.  Prompt with PROMPT, where PROMPT should
be a string.  DEF, REQUIRE-MATCH and PREDICATE is same as in
`read-buffer'."
  (let ((read-buffer-function nil))
    (read-buffer
     prompt def require-match
     (lambda (cand)
       (and (not (workroom-member-buffer-p
                  room (get-buffer
                        (if (consp cand) (car cand) cand))))
            (or (not predicate) (funcall predicate cand)))))))

(defun workroom-read-buffer-function ( prompt &optional def
                                       require-match predicate)
  "Read buffer function restricted to buffers of the current workroom.

PROMPT, DEF, REQUIRE-MATCH and PREDICATE is same as in `read-buffer'."
  (workroom--read-member-buffer
   (workroom-current-room) prompt def require-match predicate))

(defun workroom--frame-window-config (&optional frame writable)
  "Return a object describing the window configuration in FRAME.

If WRITABLE, return a writable object."
  (window-state-get (frame-root-window frame) writable))

(defun workroom--load-window-config (state)
  "Load window configuration STATE."
  (if state
      (cl-labels
          ((sanitize (entry)
             (cond
              ;; Do nothing.
              ((or (not (consp entry))
                   (atom (cdr entry)))
               entry)
              ;; A leaf window, modify this.
              ((eq (car entry) 'leaf)
               (let ((writable nil))
                 (let ((buffer (car (alist-get 'buffer (cdr entry)))))
                   ;; Buffer name is a string, the state was obtained
                   ;; with non-nil WRITABLE argument to
                   ;; `window-state-get'.
                   (when (stringp buffer)
                     (setq writable t))
                   ;; If the buffer shown in the window is dead,
                   ;; replace it with the fallback buffer, with the
                   ;; point at the very beginning.
                   (unless (buffer-live-p (get-buffer buffer))
                     (let ((fallback (get-buffer-create
                                      workroom-default-buffer-name)))
                       (with-current-buffer fallback
                         ;; Change buffer.
                         (setf (car (alist-get 'buffer (cdr entry)))
                               (if writable
                                   (buffer-name fallback)
                                 fallback))
                         ;; Set point.
                         (setf (alist-get
                                'point
                                (cdr (alist-get 'buffer (cdr entry))))
                               (if writable
                                   (point-min)
                                 (copy-marker
                                  (point-min)
                                  window-point-insertion-type)))
                         ;; Set `window-start'.
                         (setf (alist-get
                                'start
                                (cdr (alist-get 'buffer (cdr entry))))
                               (if writable
                                   (point-min)
                                 (copy-marker (point-min))))))))
                 ;; Remove references to dead buffers with
                 ;; the fallback buffer.
                 (let ((prev (alist-get 'prev-buffers (cdr entry))))
                   (setf
                    (alist-get 'prev-buffers (cdr entry))
                    (mapcar
                     (lambda (entry)
                       (if (buffer-live-p (get-buffer (car entry)))
                           entry
                         (let ((fallback
                                (get-buffer-create
                                 workroom-default-buffer-name)))
                           (with-current-buffer fallback
                             (if writable
                                 (list (buffer-name fallback)
                                       (point-min) (point-min))
                               (list
                                fallback
                                (copy-marker (point-min))
                                (copy-marker
                                 (point-min)
                                 window-point-insertion-type)))))))
                     prev)))
                 (let ((next (alist-get 'next-buffers (cdr entry))))
                   (setf (alist-get 'next-buffers (cdr entry))
                         (mapcar
                          (lambda (buffer)
                            (if (buffer-live-p (get-buffer buffer))
                                buffer
                              (let ((buffer
                                     (get-buffer-create
                                      workroom-default-buffer-name)))
                                (if writable
                                    (buffer-name buffer)
                                  buffer))))
                          next))))
               entry)
              ;; Recurse.
              (t
               (mapcar #'sanitize entry)))))
        ;; Sanitize window state (remove references to non-existant
        ;; buffers) before loading it.
        (window-state-put (cons (car state) (sanitize (cdr state)))
                          (frame-root-window) 'safe))
    (unless workroom--dont-clear-new-view
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) nil)
      (switch-to-buffer workroom-default-buffer-name))))

(defun workroom--barf-unless-enabled ()
  "Signal `user-error' unless Workroom mode is enabled."
  (unless workroom-mode
    (user-error "Workroom mode is not enabled")))

(defmacro workroom--require-mode-enable (&rest body)
  "Execute BODY if Workroom mode is enabled, otherwise signal error."
  (declare (indent 0))
  `(progn
     (workroom--barf-unless-enabled)
     ,@body))

(defun workroom-switch-view (room view &optional no-record)
  "Switch to view VIEW in workroom ROOM.

If called interactively, prompt for view to switch.  If prefix
argument is given, ask for workroom to switch before.

ROOM is should be workroom object, or a name of a workroom object.
VIEW is should be a view object, or a name of a view object.  VIEW
should be in the workroom ROOM.

ROOM defaults to the current workroom, and VIEW defaults to the last
selected view of ROOM.

When the optional argument NO-RECORD is non-nil, don't record the
switch."
  (interactive
   (workroom--require-mode-enable
     (let ((room (if current-prefix-arg
                     (workroom--read-to-switch
                      "Switch to workroom"
                      (let ((def (cl-find-if-not
                                  (apply-partially
                                   #'eq (workroom-current-room))
                                  (workroom-previous-room-list))))
                        (when def
                          (workroom-name def))))
                   (workroom-current-room))))
       (when (stringp room)
         (setq room (if (string-empty-p room)
                        (workroom-get-default)
                      (workroom-get-create room))))
       (let ((view
              (workroom--read-view-to-switch
               room "Switch to view"
               (let ((def
                      (cl-find-if
                       (lambda (view)
                         (and (not (eq view (workroom-current-view)))
                              (null (workroom-view-frame view))))
                       (workroom-view-list room))))
                 (when def
                   (workroom-view-name def))))))
         (when (string-empty-p view)
           (setq view workroom-default-view-name))
         (list room view)))))
  (workroom--barf-unless-enabled)
  (setq room
        (if (stringp room)
            (if (string-empty-p room)
                (error
                 "Empty string for workroom name is not allowed")
              (workroom-get-create room))
          (or room (workroom-current-room))))
  (setq view
        (if (stringp view)
            (if (string-empty-p view)
                (error "Empty string for view name is not allowed")
              (workroom-view-get-create room view))
          (or view
              (cl-find-if
               (lambda (view) (null (workroom-view-frame view)))
               (workroom-view-list room))
              (cl-find-if
               (lambda (view)
                 (or (null (workroom-view-frame view))
                     (eq (workroom-view-frame view)
                         (selected-frame))))
               (workroom-view-list room))
              (let ((v (workroom-view-get-create
                        room workroom-default-view-name)))
                (if (and (workroom-view-frame v)
                         (not (eq (workroom-view-frame v)
                                  (selected-frame))))
                    (workroom-generate-new-view
                     room workroom-default-view-name)
                  v)))))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (unless (workroom-view-p view)
    (signal 'wrong-type-argument (cons 'workroom-view-p view)))
  (when (and (not (eq view (workroom-current-view)))
             (workroom-view-frame view))
    (error "Cannot switch to a view already in use in another frame"))
  (unless (eq view (workroom-current-view))
    (run-hooks 'workroom-before-switch-hook)
    (unless (eq room (workroom-current-room))
      (when (and (not no-record) (workroom-current-room))
        (push (workroom-current-room)
              (frame-parameter nil 'workroom-previous-room-list)))
      (set-frame-parameter nil 'workroom-current-room room))
    (when (workroom-current-view)
      (setf (workroom--view-window-config (workroom-current-view))
            (workroom--frame-window-config))
      (setf (workroom--view-window-config-writable
             (workroom-current-view))
            (workroom--frame-window-config nil 'writable))
      (setf (workroom--view-frame (workroom-current-view)) nil)
      (setf (workroom--room-view-list room)
            (cons view (delq view (workroom--room-view-list room)))))
    (set-frame-parameter nil 'workroom-current-view view)
    (setf (workroom--view-frame view) (selected-frame))
    (workroom--load-window-config (workroom--view-window-config view))
    (run-hooks 'workroom-switch-hook)))

(defun workroom-switch (room)
  "Switch to workroom ROOM.

ROOM is should be workroom object, or a name of a workroom object."
  (interactive
   (workroom--require-mode-enable
     (list
      (workroom--read-to-switch
       "Switch to workroom"
       (cond
        ((and (eq (car (workroom-previous-room-list))
                  (workroom-current-room))
              (> (length (workroom-previous-room-list)) 1))
         (workroom-name (cadr (workroom-previous-room-list))))
        ((car (workroom-previous-room-list))
         (workroom-name (car (workroom-previous-room-list)))))))))
  (setq room
        (if (stringp room)
            (if (string-empty-p room)
                (error
                 "Empty string for workroom name is not allowed")
              (workroom-get-create room))
          room))
  (unless (eq room (workroom-current-room))
    (workroom-switch-view room nil)))

(defun workroom-kill (room)
  "Kill workroom ROOM.

ROOM is should be a workroom, or a name of a workroom."
  (interactive
   (workroom--require-mode-enable
     (list
      (workroom--read
       "Kill workroom" (workroom-name (workroom-current-room)) t
       (lambda (cand)
         (not
          (workroom-default-p
           (workroom-get (if (consp cand) (car cand) cand)))))))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (unless (workroomp room)
    (signal 'wrong-type-argument (cons 'workroomp room)))
  (when (workroom-default-p room)
    (error "Cannot kill default workroom"))
  (when (eq room (workroom-current-room))
    (workroom-switch
     (let ((r (cond
               ((and (eq (car (workroom-previous-room-list))
                         (workroom-current-room))
                     (> (length (workroom-previous-room-list)) 1))
                (cadr (workroom-previous-room-list)))
               ((car (workroom-previous-room-list)))
               (t (workroom-get-default)))))
       (if (eq r room)
           (workroom-get-default)
         r))))
  (setf (workroom--room-name room) nil)
  (setq workroom--rooms (delete room workroom--rooms))
  (dolist (frame (frame-list))
    (set-frame-parameter
     frame 'workroom-previous-room-list
     (delete room
             (frame-parameter frame 'workroom-previous-room-list)))))

(defun workroom-kill-with-buffers (room &optional kill-all)
  "Kill workroom ROOM with all its buffers.

A buffer is killed only if it doesn't belong to any other non-default
workroom.  However, interactively, when the prefix argument KILL-ALL
is given, kill all buffer regardless of other workrooms.

ROOM is should be a workroom, or a name of a workroom."
  (interactive
   (workroom--require-mode-enable
     (list
      (workroom--read
       "Kill workroom (with its buffers)"
       (workroom-name (workroom-current-room)) t
       (lambda (cand)
         (not
          (workroom-default-p
           (workroom-get (if (consp cand) (car cand) cand))))))
      current-prefix-arg)))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (unless (workroomp room)
    (signal 'wrong-type-argument (cons 'workroomp room)))
  (let ((buffers (workroom-buffer-list room)))
    (workroom-kill room)
    (let ((rooms (remove (workroom-get-default) workroom--rooms)))
      (dolist (buffer buffers)
        (and (or kill-all
                 (cl-every
                  (lambda (room)
                    (not (workroom-member-buffer-p room buffer)))
                  rooms))
             (kill-buffer buffer))))))

(defun workroom-kill-view (room view)
  "Kill view VIEW of workroom ROOM.

VIEW is should be a view object, or a name of a view object.  VIEW
should be in the workroom ROOM."
  (interactive
   (workroom--require-mode-enable
     (let ((room (if current-prefix-arg
                     (workroom-get
                      (workroom--read
                       "Kill view of workroom"
                       (workroom-name (workroom-current-room)) t))
                   (workroom-current-room))))
       (list room
             (workroom-view-get
              room
              (workroom--read-view
               room "Kill view"
               (when (eq room (workroom-current-room))
                 (workroom-view-name (workroom-current-view)))))))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (setq view (if (stringp view)
                 (or (workroom-view-get room view)
                     (signal 'wrong-type-argument
                             (cons 'workroom-view-p room)))
               view))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (unless (workroom-view-p view)
    (signal 'wrong-type-argument (cons 'workroom-view-p view)))
  (when (and room view)
    (when (eq view (workroom-current-view))
      (workroom-switch-view
       room (or (cl-find-if-not (apply-partially #'eq view)
                                (workroom-view-list room))
                (workroom-view-get-create
                 room workroom-default-view-name))))
    (setf (workroom--view-name view) nil)
    (setf (workroom--room-view-list room)
          (delete view (workroom--room-view-list room)))))

(defun workroom-rename (room new-name)
  "Rename workroom ROOM to NEW-NAME.

ROOM is should be workroom object, or a name of a workroom object."
  (interactive
   (workroom--require-mode-enable
     (let ((room
            (workroom--read
             "Rename workroom" (workroom-name (workroom-current-room))
             t)))
       (list room (read-string (format-message
                                "Rename workroom `%s' to: " room))))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (setf (workroom--room-name room) new-name))

(defun workroom-rename-view (room view new-name)
  "Rename view VIEW of workroom ROOM to NEW-NAME."
  (interactive
   (workroom--require-mode-enable
     (let* ((room
             (if current-prefix-arg
                 (workroom-get
                  (workroom--read
                   "Parent workroom" (workroom-name
                                      (workroom-current-room))
                   t))
               (workroom-current-room)))
            (view (workroom--read-view
                   room (format-message "Rename view of workroom `%s'"
                                        (workroom-name room))
                   (when (eq room (workroom-current-room))
                     (workroom-view-name (workroom-current-view)))
                   t)))
       (list room view
             (read-string (format-message
                           "Rename view `%s' of workroom `%s' to: "
                           view (workroom-name room)))))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (setq view (if (stringp view)
                 (or (workroom-view-get room view)
                     (signal 'wrong-type-argument
                             (cons 'workroom-view-live-p room)))
               view))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (unless (workroom-view-live-p view)
    (signal 'wrong-type-argument (cons 'workroom-view-live-p view)))
  (setf (workroom--view-name view) new-name))

(defun workroom-clone (room name)
  "Create a clone of workroom ROOM named NAME."
  (interactive
   (workroom--require-mode-enable
     (let ((room (workroom--read
                  "Clone workroom"
                  (workroom-name (workroom-current-room)) t)))
       (list room (read-string "Name of cloned workroom: ")))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (let ((clone
         (workroom--make-room
          :name name
          :view-list (mapcar
                      (lambda (view)
                        (workroom--make-view
                         :name (workroom--view-name view)
                         :window-config
                         (workroom-view-window-configuration view)
                         :window-config-writable
                         (workroom--view-window-config-writable
                          view)))
                      (workroom-view-list room))
          :buffer-manager (workroom--room-buffer-manager room))))
    (funcall (workroom--room-buffer-manager room) clone :clone room)
    (push clone workroom--rooms)
    clone))

(defun workroom-clone-view (room view name)
  "Create a clone of view VIEW named NAME in workroom ROOM."
  (interactive
   (workroom--require-mode-enable
     (let* ((room
             (if current-prefix-arg
                 (workroom-get
                  (workroom--read
                   "Parent workroom"
                   (workroom-name (workroom-current-room)) t))
               (workroom-current-room)))
            (view (workroom--read-view
                   room (format-message "Clone view of workroom `%s'"
                                        (workroom-name room))
                   (when (eq room (workroom-current-room))
                     (workroom-view-name (workroom-current-view)))
                   t)))
       (list room view (read-string "Name of cloned view: ")))))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (setq view (if (stringp view)
                 (or (workroom-view-get room view)
                     (signal 'wrong-type-argument
                             (cons 'workroom-view-live-p room)))
               view))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (unless (workroom-view-live-p view)
    (signal 'wrong-type-argument (cons 'workroom-view-live-p view)))
  (let ((clone
         (workroom--make-view
          :name name
          :window-config (workroom-view-window-configuration view)
          :window-config-writable
          (workroom--view-window-config-writable view))))
    (setf (workroom--room-view-list room)
          (nconc (workroom--room-view-list room) (list clone)))
    clone))

(defun workroom-add-buffer (buffer &optional room)
  "Add BUFFER to workroom ROOM.

ROOM should be a workroom object or a string.  When ROOM is a string,
the workroom object with that string as the name is used.  When ROOM
is a workroom object, add BUFFER to it.  If ROOM is nil, add BUFFER to
the room of the selected frame.

If ROOM is the default workroom, do nothing."
  (interactive
   (workroom--require-mode-enable
     (list (get-buffer-create
            (workroom--read-non-member-buffer
             (workroom-current-room) "Add buffer: "
             (unless (workroom-member-buffer-p
                      (workroom-current-room) (current-buffer))
               (current-buffer))))
           nil)))
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               (or room (workroom-current-room))))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (unless (workroom-member-buffer-p room buffer)
    (funcall (workroom--room-buffer-manager room)
             room :add-buffer buffer)))

(defun workroom-remove-buffer (buffer &optional room)
  "Remove BUFFER from workroom ROOM.

ROOM should be a `workroom'.  When ROOM is a `workroom' object, remove
BUFFER from it.  If ROOM is nil, remove BUFFER to the room of the
selected frame.

If ROOM is the default workroom, kill buffer."
  (interactive
   (workroom--require-mode-enable
     (list (get-buffer
            (workroom--read-member-buffer
             (workroom-current-room)
             "Remove buffer: "
             (when (workroom-member-buffer-p
                    (workroom-current-room) (current-buffer))
               (current-buffer))
             t))
           nil)))
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               (or room (workroom-current-room))))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (when (workroom-member-buffer-p room buffer)
    (funcall (workroom--room-buffer-manager room)
             room :remove-buffer buffer)))

(defun workroom-switch-to-buffer ()
  "Like `switch-to-buffer' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (declare (interactive-only "Use `switch-to-buffer' instead."))
  (interactive)
  (if (or current-prefix-arg (not workroom-mode))
      (call-interactively #'switch-to-buffer)
    (let ((read-buffer-function #'workroom-read-buffer-function))
      (call-interactively #'switch-to-buffer))))

(defun workroom-kill-buffer ()
  "Like `kill-buffer' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (declare (interactive-only "Use `kill-buffer' instead."))
  (interactive)
  (if (or current-prefix-arg (not workroom-mode))
      (call-interactively #'kill-buffer)
    (let ((read-buffer-function #'workroom-read-buffer-function))
      (call-interactively #'kill-buffer))))

(defun workroom--default-buffer-manager (room action &rest args)
  "The default buffer manager of workrooms.

Set as the buffer manager function of ROOM with
`workroom-set-buffer-manager-function', which see.  The value of
ACTION and ARGS are also described there."
  (setf (workroom-buffer-manager-data room)
        (cl-delete-if-not #'buffer-live-p
                          (workroom-buffer-manager-data room)))
  (pcase (cons action args)
    ('(:initialize)
     (setf (workroom-buffer-manager-data room)
           (list
            (if (equal workroom-default-buffer-name "*scratch*")
                (get-scratch-buffer-create)
              (get-buffer-create workroom-default-buffer-name)))))
    ('(:list-buffers)
     (copy-sequence (workroom-buffer-manager-data room)))
    (`(:add-buffer ,buffer)
     (push buffer (workroom-buffer-manager-data room)))
    (`(:remove-buffer ,buffer)
     (setf (workroom-buffer-manager-data room)
           (delq buffer (workroom-buffer-manager-data room))))
    (`(:member-buffer-p ,buffer)
     (memq buffer (workroom-buffer-manager-data room)))
    (`(:clone ,source)
     (setf (workroom-buffer-manager-data room)
           (copy-sequence (workroom-buffer-manager-data source))))
    ('(:encode)
     ;; Nothing, we'll get the buffer list through the fourth
     ;; argument of `:load'.
     )
    (`(:load ,_data ,buffers)
     (setf (workroom-buffer-manager-data room)
           (copy-sequence buffers)))))

(defun workroom--default-room-buffer-manager (room action &rest args)
  "The buffer manager of the default workroom.

Set as the buffer manager function of ROOM with
`workroom-set-buffer-manager-function', which see.  The value of
ACTION and ARGS are also described there."
  (pcase (cons action args)
    ('(:initialize)
     ;; Nothing.
     )
    ('(:list-buffers)
     (buffer-list))
    (`(:add-buffer ,_buffer)
     ;; Nothing, all live buffers are members.
     )
    (`(:remove-buffer ,buffer)
     ;; All live buffer are members, so the buffer must die to leave
     ;; us.
     (kill-buffer buffer))
    (`(:member-buffer-p ,buffer)
     ;; All live buffer are members.
     (buffer-live-p buffer))
    (`(:clone ,_source)
     ;; There can't be two default workrooms, so this function can't
     ;; manage two workrooms.  We'll hand over responsibilities to
     ;; the default buffer manager.
     (workroom-set-buffer-manager-function
      room #'workroom--default-buffer-manager 'do-not-initialize)
     (setf (workroom-buffer-manager-data room) (buffer-list)))
    ('(:encode)
     ;; Nothing, the default workroom can't be encoding (but can
     ;; indeed be saved, see the action `:load').
     )
    (`(:load ,data ,buffers)
     ;; There can't be two default workrooms, so this function can't
     ;; manage two workrooms.  We'll hand over responsibilities to
     ;; the default buffer manager.
     (workroom-set-buffer-manager-function
      room #'workroom--default-buffer-manager 'do-not-initialize)
     (workroom--default-buffer-manager room :load data buffers))))


;;;; Buffer Menu Integration.

(defun workroom--list-buffers-noselect ()
  "Setup buffer menu to not include any non-member buffer."
  (let* ((room (workroom-current-room))
         (buffer (list-buffers-noselect
                  nil (workroom-buffer-list room))))
    (with-current-buffer buffer
      (remove-hook 'tabulated-list-revert-hook
                   #'list-buffers--refresh t)
      (add-hook 'tabulated-list-revert-hook
                (lambda ()
                  (list-buffers--refresh
                   (workroom-buffer-list (workroom-current-room))))
                nil t))
    buffer))

(defun workroom-buffer-menu ()
  "Like `buffer-menu' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (interactive)
  (if (or current-prefix-arg (not workroom-mode))
      (call-interactively #'buffer-menu)
    (switch-to-buffer (workroom--list-buffers-noselect))))

(defun workroom-list-buffers ()
  "Like `list-buffers' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (interactive)
  (if (or current-prefix-arg (not workroom-mode))
      (call-interactively #'list-buffers)
    (display-buffer (workroom--list-buffers-noselect))))


;;;; Electric Buffer List Integration.

(defun workroom-electric-buffer-list ()
  "Like `electric-buffer-list' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (interactive)
  (if (or current-prefix-arg (not workroom-mode))
      (call-interactively #'electric-buffer-list)
    (cl-letf* ((list-buffers-noselect
                (symbol-function #'list-buffers-noselect))
               ((symbol-function #'list-buffers-noselect)
                (lambda (&optional arg buffers)
                  (funcall
                   list-buffers-noselect arg
                   (cl-union buffers (workroom-buffer-list
                                      (workroom-current-room)))))))
      (call-interactively #'electric-buffer-list))))


;;;; IBuffer Integration.

(defvar workroom--ibuffer-room nil
  "Buffer-local variable containing the workroom IBuffer is showing.")

(defvar workroom--in-workroom-ibuffer nil
  "Non-nil means inside `workroom-ibuffer'.")

(declare-function ibuffer-filter-buffers "ibuffer"
                  (ibuffer-buf last bmarklist all))

(defun workroom--ibuffer-filter-buffers-advice (buffers)
  "Filter workroom member buffers from BUFFERS."
  (when workroom--in-workroom-ibuffer
    (setq-local workroom--ibuffer-room workroom--in-workroom-ibuffer))
  (if workroom--ibuffer-room
      (cl-remove-if-not (lambda (entry)
                          (workroom-member-buffer-p
                           workroom--ibuffer-room (car entry)))
                        buffers)
    buffers))

(defun workroom--ibuffer-forget-workroom (&optional _ buffer &rest _)
  "Unbind `workroom--ibuffer-room' in IBuffer buffer BUFFER."
  (unless workroom--in-workroom-ibuffer
    (setq buffer (or buffer "*Ibuffer*"))
    (when (and (if (buffer-live-p buffer)
                   (string= (buffer-name buffer) "*Ibuffer*")
                 (and (string= buffer "*Ibuffer*")
                      (get-buffer buffer)))
               (buffer-local-value 'workroom--ibuffer-room
                                   (get-buffer buffer)))
      (with-current-buffer "*Ibuffer*"
        (kill-local-variable 'workroom--ibuffer-room)))))

(defun workroom-ibuffer ()
  "Like `ibuffer' but restricted to current workroom.

When prefix arg is given or Workroom mode is disabled, don't
restrict."
  (interactive)
  (let ((workroom--in-workroom-ibuffer
         (and (not (or current-prefix-arg (not workroom-mode)))
              (workroom-current-room))))
    (call-interactively #'ibuffer)))


;;;; Mode.

(defun workroom--frame-manage-p (frame)
  "Return non-nil if workroom should manage FRAME."
  (and (not (frame-parameter frame 'parent-frame))
       (eq (frame-parameter frame 'minibuffer) t)))

(defun workroom--init-frame (frame)
  "Initialize frame FRAME."
  (when (workroom--frame-manage-p frame)
    (let ((default (workroom-get-default)))
      (with-selected-frame frame
        (workroom-switch-view
         default (workroom-generate-new-view
                  default workroom-default-view-name))))))

;;;###autoload
(define-minor-mode workroom-mode
  "Toggle workroom mode."
  :lighter (:eval workroom-mode-lighter)
  :global t
  (if workroom-mode
      (progn
        (workroom-mode -1)
        (setq workroom-mode t)
        (workroom-rebind-command-map-prefix)
        (let ((workroom--dont-clear-new-view t)
              (default-room (workroom-get-default)))
          (unless default-room
            (setq
             default-room
             (workroom--make-room
              :name workroom-default-room-name
              :buffer-manager #'workroom--default-room-buffer-manager
              :default-p t))
            (workroom--default-room-buffer-manager
             default-room :initialize)
            (push default-room workroom--rooms))
          (unless (equal (workroom-name default-room)
                         workroom-default-room-name)
            (setf (workroom--room-name default-room)
                  workroom-default-room-name))
          (mapc #'workroom--init-frame (frame-list))
          (add-hook 'after-make-frame-functions
                    #'workroom--init-frame))
        (advice-add #'ibuffer :before
                    #'workroom--ibuffer-forget-workroom)
        (advice-add #'ibuffer-filter-buffers :filter-return
                    #'workroom--ibuffer-filter-buffers-advice))
    (advice-remove #'ibuffer #'workroom--ibuffer-forget-workroom)
    (advice-remove #'ibuffer-filter-buffers
                   #'workroom--ibuffer-filter-buffers-advice)
    (dolist (frame (frame-list))
      (when (frame-parameter frame 'workroom-current-room)
        (set-frame-parameter frame 'workroom-current-room nil)
        (set-frame-parameter frame 'workroom-current-view nil)
        (set-frame-parameter frame 'workroom-previous-room-list nil)))
    (setq workroom--rooms nil)
    (remove-hook 'after-make-frame-functions #'workroom--init-frame)))


;;;; Workroom Encoding/Decoding.

(defun workroom--encode-view-1 (view)
  "Encode view VIEW to a writable object."
  (list :name (workroom-view-name view)
        :window-config (workroom-view-window-configuration
                        view 'writable)))

(defun workroom--decode-view-1 (object)
  "Decode encoded view OBJECT to a view."
  (workroom--make-view
   :name (plist-get object :name)
   :window-config (plist-get object :window-config)
   :window-config-writable (plist-get object :window-config)))

(defun workroom--encode-room-1 (room)
  "Encode workroom ROOM to a writable object.

The buffers are not encoded, they must be encoded separately."
  (list :name (workroom-name room)
        :view-list (mapcar #'workroom--encode-view-1
                           (workroom-view-list room))
        :buffer-manager (workroom-buffer-manager-function room)
        :buffer-manager-data (funcall
                              (workroom-buffer-manager-function room)
                              room :encode)))

(defun workroom--decode-room-1 (object buffers)
  "Decode encoded workroom OBJECT to a workroom.

BUFFERS should be a list of the buffer that were the member of ROOM
when ROOM was encoded."
  (let ((room (workroom--make-room
               :name (workroom-generate-new-room-name
                      (plist-get object :name))
               :view-list (mapcar #'workroom--decode-view-1
                                  (plist-get object :view-list))
               :buffer-manager (plist-get object :buffer-manager))))
    (funcall (plist-get object :buffer-manager) room :load
             (plist-get object :buffer-manager-data) buffers)
    room))


;;;; Buffer Encoding/Decoding.

(defun workroom--encode-buffers (buffers)
  "Encode the buffers in the list BUFFERS to writable objects."
  (let* ((objects '(nil))
         (tail objects))
    (dolist (buffer buffers)
      (cl-block nil
        (dolist (entry workroom-buffer-handler-alist nil)
          (when-let ((object (funcall (plist-get (cdr entry) :encoder)
                                      buffer)))
            (setf (cdr tail)
                  (list (list :name (buffer-name buffer)
                              :encoding (car entry)
                              :object object)))
            (setq tail (cdr tail))
            (cl-return)))))
    (cdr objects)))

(defun workroom--decode-buffers (objects)
  "Restore the buffers encoded in OBJECTS."
  (let* ((buffers '(nil))
         (tail buffers))
    (dolist (object objects)
      (let ((decoder
             (plist-get (alist-get (plist-get object :encoding)
                                   workroom-buffer-handler-alist)
                        :decoder)))
        (setf (cdr tail)
              (list (cons (plist-get object :name)
                          (when decoder
                            (funcall decoder
                                     (plist-get object :object))))))
        (setq tail (cdr tail))))
    (cdr buffers)))

(defun workroom-encode-buffer-bookmark (buffer)
  "Encode BUFFER using `bookmark-make-record'."
  (with-current-buffer buffer
    (ignore-errors
      (bookmark-make-record))))

(defun workroom-decode-buffer-bookmark (object)
  "Decode OBJECT using `bookmark-jump'."
  (let ((buffer nil))
    (bookmark-jump object (lambda (buf) (setq buffer buf)))
    buffer))


;;;; Bookmark Integration.

(defun workroom--read-bookmark (prompt)
  "Prompt with PROMPT, read a bookmark name, don't require match."
  (bookmark-maybe-load-default-file)
  (completing-read
   prompt (lambda (string predicate action)
            (if (eq action 'metadata)
                '(metadata (category . bookmark))
              (complete-with-action action bookmark-alist string
                                    predicate)))
   nil nil nil 'bookmark-history))

;;;###autoload
(defun workroom-bookmark-jump-to-room (bookmark)
  "Jump to the workroom in bookmark BOOKMARK."
  (workroom--barf-unless-enabled)
  (let ((data (cdr (alist-get 'data (bookmark-get-bookmark-record
                                     bookmark)))))
    (pcase (plist-get data :version)
      (1
       (let* ((buffers (mapcar #'cdr
                               (cl-delete-if
                                #'null
                                (workroom--decode-buffers
                                 (plist-get data :buffers)))))
              (room (workroom--decode-room-1
                     (plist-get data :room) buffers)))
         (push room workroom--rooms)
         (workroom-switch room)))
      (version
       (error "Unsuppported bookmark version %i" version))))
  (set-buffer (window-buffer)))

(defun workroom-bookmark (room name no-overwrite)
  "Save workroom ROOM to a bookmark named NAME.

ROOM can be a workroom, or a name of a workroom.

If NO-OVERWRITE is nil or prefix argument is given, don't overwrite
any previous bookmark with the same name."
  (interactive
   (list (workroom--read "Bookmark workroom" nil t)
         (workroom--read-bookmark "Save to bookmark: ")
         current-prefix-arg))
  (workroom--barf-unless-enabled)
  (setq room (if (stringp room)
                 (or (workroom-get room)
                     (signal 'wrong-type-argument
                             (cons 'workroom-live-p room)))
               room))
  (unless (workroom-live-p room)
    (signal 'wrong-type-argument (cons 'workroom-live-p room)))
  (bookmark-store
   name `((data . (workroom
                   :version 1
                   :room ,(workroom--encode-room-1 room)
                   :buffers ,(workroom--encode-buffers
                              (workroom-buffer-list room))))
          (handler . workroom-bookmark-jump-to-room))
   no-overwrite))

;;;###autoload
(defun workroom-bookmark-jump-to-room-set (bookmark)
  "Restore the workroom set in bookmark BOOKMARK."
  (workroom--barf-unless-enabled)
  (let ((data (cdr (alist-get 'data (bookmark-get-bookmark-record
                                     bookmark)))))
    (pcase (plist-get data :version)
      (1
       (let ((buffers (cl-delete-if
                       #'null
                       (workroom--decode-buffers
                        (plist-get data :buffers)))))
         (dolist (wr (plist-get data :rooms))
           (let ((buffer-list (cl-delete-if
                               #'null
                               (mapcar (lambda (name)
                                         (alist-get name buffers))
                                       (plist-get wr :buffers)))))
             (push (workroom--decode-room-1 (plist-get wr :room)
                                            buffer-list)
                   workroom--rooms)))))
      (version
       (error "Unsuppported bookmark version %i" version))))
  (set-buffer (window-buffer)))

(defun workroom-bookmark-multiple (rooms name no-overwrite)
  "Save the workrooms ROOMS to a bookmark named NAME.

If NO-OVERWRITE is nil or prefix argument is given, don't overwrite
any previous bookmark with the same name."
  (interactive
   (list (workroom--read-multiple "Bookmark workrooms" nil t)
         (workroom--read-bookmark "Save to bookmark: ")
         current-prefix-arg))
  (workroom--barf-unless-enabled)
  (let ((wrs rooms))
    (while wrs
      (setf (car wrs)
            (if (stringp (car wrs))
                (or (workroom-get (car wrs))
                    (signal 'wrong-type-argument
                            (cons 'workroom-live-p (car wrs))))
              (car wrs)))
      (unless (workroom-live-p (car wrs))
        (signal 'wrong-type-argument
                (cons 'workroom-live-p (car wrs))))
      (pop wrs)))
  (bookmark-store
   name
   `((data . (workroom-set
              :version 1
              :rooms ,(mapcar
                       (lambda (wr)
                         (list :room (workroom--encode-room-1 wr)
                               :buffers (mapcar
                                         #'buffer-name
                                         (workroom-buffer-list wr))))
                       rooms)
              :buffers ,(workroom--encode-buffers
                         (cl-remove-duplicates
                          (apply #'append
                                 (mapcar #'workroom-buffer-list
                                         rooms))))))
     (handler . workroom-bookmark-jump-to-room-set))
   no-overwrite))


;;;; Desktop Integration.

(defun workroom--desktop-restore (object)
  "Restore all workrooms from OBJECT recorded in desktop file."
  (pcase (plist-get object :version)
    (1
     ;; Restore default workroom name and views.
     (let ((def-room (workroom-get-default))
           (room-name-alist nil))
       (let ((room (plist-get object :default-room)))
         (workroom-rename def-room (plist-get room :name))
         (dolist (view (workroom--room-view-list def-room))
           (setf (workroom--view-name view) nil))
         (setf (workroom--room-view-list def-room)
               (mapcar #'workroom--decode-view-1
                       (plist-get room :view-list)))
         (setf (workroom--room-view-history def-room) nil)
         ;; We use room-name-alist to map names to rooms, because the
         ;; room names in OBJECT may not be used as the names of the
         ;; newly create rooms (maybe because they are is use, for
         ;; example).
         (push (cons (plist-get room :name) def-room)
               room-name-alist))
       ;; Restore other workrooms.
       (dolist (wr (plist-get object :other-rooms))
         (let* ((buffers (cl-delete-if #'null
                                       (mapcar
                                        #'get-buffer
                                        (plist-get wr :buffers))))
                (room (workroom--decode-room-1
                       (plist-get wr :room) buffers)))
           (push room workroom--rooms)
           (push (cons (plist-get (plist-get wr :room) :name) room)
                 room-name-alist)))
       ;; Switch to views.
       (let ((active-views (plist-get object :active-views)))
         (let ((selected-frame (selected-frame)))
           (dolist (frame (frame-list))
             (when (workroom--frame-manage-p frame)
               (select-frame frame 'norecord)
               (set-frame-parameter frame 'workroom-current-room nil)
               (set-frame-parameter frame 'workroom-current-view nil)
               (set-frame-parameter frame 'workroom-previous-room-list
                                    nil)
               (let* ((view (pop active-views))
                      (room (cdr (assoc-string
                                  (car view) room-name-alist))))
                 (if view
                     (workroom-switch-view
                      room
                      (workroom-view-get room (cdr view)))
                   (workroom-switch-view
                    def-room
                    (workroom-generate-new-view
                     def-room workroom-default-view-name))))))
           (select-frame selected-frame 'norecord)))))
    (version
     (error "Unsuppported workroom with version %i in desktop file"
            version))))

(defun workroom--desktop-inject-restore-code ()
  "Inject workroom restore code in desktop file."
  ;; Inject restoring code.
  (when workroom-mode
    (insert
     "
;; Workroom section:
"
     (let ((print-level nil)
           (print-length nil)
           (fn-sym (intern (format "workroom--desktop-restore-%s"
                                   (format-time-string "%s%N")))))
       (prin1-to-string
        `(progn
           (defun ,fn-sym ()
             "Restore workrooms."
             (remove-hook 'desktop-after-read-hook #',fn-sym)
             (fmakunbound #',fn-sym)
             (when (bound-and-true-p workroom-mode)
               (workroom--desktop-restore
                ',(list
                   :version 1
                   :default-room (workroom--encode-room-1
                                  (workroom-get-default))
                   :other-rooms
                   (mapcar
                    (lambda (room)
                      (list :room (workroom--encode-room-1 room)
                            :buffers (mapcar
                                      #'buffer-name
                                      (workroom-buffer-list room))))
                    (cl-remove-if #'workroom-default-p
                                  workroom--rooms))
                   :active-views
                   (mapcar
                    (lambda (frame)
                      (with-selected-frame frame
                        (cons (workroom-name (workroom-current-room))
                              (workroom-view-name
                               (workroom-current-view)))))
                    (cl-remove-if-not #'workroom--frame-manage-p
                                      (frame-list)))))))
           (add-hook 'desktop-after-read-hook #',fn-sym))))
     ?\n)))

(define-minor-mode workroom-desktop-save-mode
  "Toggle saving workrooms with desktop mode."
  :global t
  :require 'workroom
  (if workroom-desktop-save-mode
      (add-hook 'desktop-save-hook
                #'workroom--desktop-inject-restore-code)
    (remove-hook 'desktop-save-hook
                 #'workroom--desktop-inject-restore-code)))


;;;; Project Integration.

(defun workroom--project-buffer-manager (room action &rest args)
  "The buffer manager for a project.

Set as the buffer manager function of ROOM with
`workroom-set-buffer-manager-function', which see.  The value of
ACTION and ARGS are also described there.  This function take an
argument while setting as the buffer manager, PROJECT, the project."
  (setf (plist-get (workroom-buffer-manager-data room)
                   :whitelist)
        (cl-delete-if-not
         #'buffer-live-p
         (plist-get (workroom-buffer-manager-data room)
                    :whitelist)))
  (setf (plist-get (workroom-buffer-manager-data room)
                   :blacklist)
        (cl-delete-if-not
         #'buffer-live-p
         (plist-get (workroom-buffer-manager-data room)
                    :blacklist)))
  (pcase (cons action args)
    (`(:initialize ,project)
     (setf (workroom-buffer-manager-data room)
           (list :project project)))
    ('(:list-buffers)
     (cl-remove-if
      (let ((blacklist
             (plist-get (workroom-buffer-manager-data room)
                        :blacklist)))
        (lambda (buffer) (memq buffer blacklist)))
      (append (plist-get (workroom-buffer-manager-data room)
                         :whitelist)
              (project-buffers
               (plist-get (workroom-buffer-manager-data room)
                          :project)))))
    (`(:add-buffer ,buffer)
     ;; Remove from blacklist.
     (setf (plist-get (workroom-buffer-manager-data room)
                      :blacklist)
           (delete buffer
                   (plist-get (workroom-buffer-manager-data room)
                              :blacklist)))
     ;; If it's still not in the list, whitelist it.
     (unless (workroom--project-buffer-manager
              room :member-buffer-p buffer)
       (push buffer (plist-get (workroom-buffer-manager-data room)
                               :whitelist))))
    (`(:remove-buffer ,buffer)
     ;; Remove from whitelist.
     (setf (plist-get (workroom-buffer-manager-data room)
                      :whitelist)
           (delete buffer
                   (plist-get (workroom-buffer-manager-data room)
                              :whitelist)))
     ;; If it's still in the list, blacklist it.
     (when (workroom--project-buffer-manager
            room :member-buffer-p buffer)
       (push buffer (plist-get (workroom-buffer-manager-data room)
                               :blacklist))))
    (`(:member-buffer-p ,buffer)
     (and (not (memq buffer
                     (plist-get (workroom-buffer-manager-data room)
                                :blacklist)))
          (or (memq buffer
                    (plist-get (workroom-buffer-manager-data room)
                               :whitelist))
              (string-prefix-p
               (expand-file-name
                (file-name-as-directory
                 (project-root
                  (plist-get (workroom-buffer-manager-data room)
                             :project))))
               (expand-file-name
                (buffer-local-value 'default-directory buffer))))))
    (`(:clone ,source)
     (cl-destructuring-bind (&key project whitelist blacklist)
         (workroom-buffer-manager-data source)
       (setf (workroom-buffer-manager-data room)
             (list :project project
                   :whitelist (copy-sequence whitelist)
                   :blacklist (copy-sequence blacklist)))))
    ('(:encode)
     (cl-destructuring-bind (&key project _whitelist blacklist)
         (workroom-buffer-manager-data room)
       (list :project-root (project-root project)
             :blacklist (mapcar #'buffer-name blacklist))))
    (`(:load ,data ,buffers)
     (let ((project (project-current
                     nil (plist-get data :project-root))))
       (if project
           (setf (workroom-buffer-manager-data room)
                 (list :project project
                       :whitelist (cl-set-difference
                                   buffers (project-buffers project))
                       :blacklist (cl-delete-if
                                   #'null
                                   (mapcar
                                    #'get-buffer
                                    (plist-get data :blacklist)))))
         ;; The project no longer exists, so hand over the buffers
         ;; to the plain default manager.
         (workroom-set-buffer-manager-function
          room #'workroom--default-buffer-manager
          'do-not-initialize)
         (workroom--default-buffer-manager
          room :load data buffers))))))

(defun workroom-switch-to-project-workroom (name project-root)
  "Switch to a workroom NAME with all buffers in the current project.

Prompt for PROJECT-ROOT if the project root can't be found, or if the
prefix argument is given."
  (interactive
   (let* ((project
           (if current-prefix-arg
               (project-current nil (project-prompt-project-dir))
             (project-current 'maybe-prompt)))
          (root (project-root project))
          (name (file-name-base (directory-file-name
                                 (project-root project)))))
     (list
      (read-string
       (format-message "Workname name for project `%s': " name)
       name 'workroom-room-history name)
      root)))
  (workroom-switch name)
  (workroom-set-buffer-manager-function
   (workroom-current-room) #'workroom--project-buffer-manager nil
   (project-current nil project-root)))

(defun workroom--project-switch-to-appropiate-room ()
  "Switch the appropiate workroom for current buffer."
  (let ((project (project-current))
        (room nil))
    (when project
      (cl-block nil
        (dolist (wr (workroom-list))
          (when (and (eq (workroom-buffer-manager-function wr)
                         #'workroom--project-buffer-manager)
                     (equal (plist-get
                             (workroom-buffer-manager-data wr)
                             :project)
                            project))
            (setq room wr)
            (cl-return))))
      (if room
          (workroom-switch room)
        (let ((workroom--dont-clear-new-view t))
          (workroom-switch-to-project-workroom
           (file-name-base (directory-file-name
                            (project-root project)))
           (project-root project)))))))

(define-minor-mode workroom-auto-project-workroom-mode
  "Toggle automatically creating project workrooms."
  :lighter " WR-Project"
  :global t
  :require 'workroom
  :group 'project
  (if workroom-auto-project-workroom-mode
      (add-hook 'find-file-hook
                #'workroom--project-switch-to-appropiate-room)
    (remove-hook 'find-file-hook
                 #'workroom--project-switch-to-appropiate-room)))


;;;; Winner Integration.

(defvar winner-ring-alist)
(defvar winner-ring-size)

(defvar workroom--winner-alist nil
  "Alist of views and window configuration rings.")

(defun workroom--winner-before-switch ()
  "Save winner undo list."
  (let ((entry (assq (selected-frame) winner-ring-alist)))
    (when entry
      (setf (alist-get (workroom-current-view) workroom--winner-alist)
            (cdr entry))
      ;; A window configuration change is going to happen shortly due
      ;; to changing view.  We don't wanna record that.
      (setf (cdr entry) (make-ring 1)))))

(defun workroom--winner-after-switch ()
  "Restore save winner undo list."
  (let ((entry (assq (selected-frame) winner-ring-alist)))
    (when entry
      (setf (cdr entry) (or (alist-get (workroom-current-view)
                                       workroom--winner-alist)
                            (make-ring winner-ring-size))))))

(define-minor-mode workroom-winner-mode
  "Toggle Workroom integration with Winner."
  :lighter " WR-Winner"
  :global t
  :require 'workroom
  :group 'winner
  (if workroom-winner-mode
      (progn
        (add-hook 'workroom-before-switch-hook
                  #'workroom--winner-before-switch)
        (add-hook 'workroom-switch-hook
                  #'workroom--winner-after-switch))
    (remove-hook 'workroom-before-switch-hook
                 #'workroom--winner-before-switch)
    (remove-hook 'workroom-switch-hook
                 #'workroom--winner-after-switch)))

(provide 'workroom)
;;; workroom.el ends here
