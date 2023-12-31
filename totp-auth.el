;;; totp-auth.el --- RFC6238 TOTP -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Mohapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Vivek Das Mohapatra <vivek@etla.org>
;; Keywords: 2FA two-factor totp otp password
;; URL: https://gitlab.com/fledermaus/totp.el
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; totp-auth.el - Time-based One Time Password support for emacs
;;
;; This package generates RFC6238 Time-based One Time Passwords
;; and displays them (as well as optionally copying them to
;; the clipboard/primary selection), updating them as they expire.
;;
;; It retrieves the shared secrets used to generate TOTP tokens
;; with ‘auth-sources’ and/or the freedesktop secrets API (aka
;; Gnome Keyring or KWallet).
;;
;; You can call it with the command ‘totp’, ie:
;;
;;    M-x totp RET
;;
;; You can tab-complete based on the label of the secret and a
;; buffer displaying the token will be displayed (and updated as
;; the displayed token expires).
;;
;; If you want to import TOTP secrets from other apps you can call:
;;
;;   M-x totp-import-file RET
;;
;; If you want the latest generated token automatically
;; copied to your GUI's selection for easy pasting, you
;; can customize ‘totp-auto-copy-password’.

;;; Code:
(eval-and-compile
  (let ((load-path load-path)
        (this-file (or load-file-name
                       byte-compile-current-file
                       buffer-file-name)))
    (when (not (and (locate-library "base32")
                    (locate-library "hmac")))
      (add-to-list 'load-path (file-name-directory this-file)))
    (require 'base32)
    (require 'hmac))
  (require 'auth-source)
  (require 'bindat)
  (require 'url-parse)
  (require 'url-util)
  (require 'mailcap))

(defgroup totp-auth nil "Time-based One Time Passwords."
  :prefix "totp"
  :group 'data)

(defconst totp-xdg-schema "org.freedesktop.Secret.TOTP")

(defcustom totp-alt-xdg-schemas
  '("com.github.bilelmoussaoui.Authenticator")
  "A list of fallback XDG schemas which are associated with TOTP secrets.
This is used only to read TOTP secrets stored by other applications."
  :type '(repeat string)
  :group 'totp-auth)

(defcustom totp-minimum-ui-grace 3
  "The minimum time to expiry a TOTP must have for interactive use.
If the generated token has less then this much time to live then
interactive code MAY instead generate the next TOTP in sequence
and wait until it is valid before giving it to the user.
Noninteractive TOTP code MUST return TOTP values along with their
lifespan (at the time of generation) and their absolute expiry time."
  :type  'integer
  :group 'totp)

(defcustom totp-max-tokens 1024
  "The maximum number of tokens totp will try to fetch and process."
  :group 'totp-auth
  :type  'integer)

(defcustom totp-file-import-command '("zbarimg" "-q" "@file@")
  "The command and parameters used to parse a QR code image.
@file@ is a placeholder for the file name."
  :group 'totp-auth
  :type  '(repeat string))

(defcustom totp-secrets-create-item-workaround t
  "The replace parame of freedesktop secrets CreateItem is unreliable.
If this option is on (the default) then we attempt
delete duplicated secrets when we save a secret via this API.\n
If it is off then you are likely to end up with multiple copies of
a secret if you ever re-import it."
  :group 'totp-auth
  :type  'boolean)

(defcustom totp-auto-copy-password '(PRIMARY CLIPBOARD)
  "If set \\[totp] will copy tokens into the selected copy/paste backends.
The behaviour is implemented by ‘totp-update-paste-buffers’ as follows:
 - When the token is generated, it is placed in the selected copy areas
 - If the copy area still contains the previous value when the token
   expires and is regenerated it is replaced with the new value."
  :group 'totp-auth
  :type '(choice
          (const :tag "Off" nil)
          (set :tag "Choose Copy Method(s)"
           (const :tag "Primary (middle-click etc)"  PRIMARY)
           (const :tag "Clipboard (Paste, C-y, C-v)" CLIPBOARD)
           (const :tag "Secondary"                   SECONDARY))))

(defcustom totp-display-token-method nil
  "Choose the TOTP token display mechanism.
A Custom function it must accept a ‘totp-generate-otp’ SECRET
and optional LABEL as its first two arguments."
  :group 'totp-auth
  :type '(choice
          (const :tag "Notification if possible, otherwise TOTP buffer" nil)
          (const :tag "Desktop notification" totp-display-token-notification)
          (const :tag "TOTP buffer" totp-display-token-buffer)
          (function :tag "Custom function")))

(defcustom totp-auth-sources nil
  "Serves the same purpose as ‘auth-sources’, but for the TOTP package.
If unset (the default) this will be initialised to a list
consisting of the contents of ‘auth-sources’ with the freedesktop
secrets service login session prepended to it, if it is available."
  :group 'totp-auth
  :type `(repeat :tag "Authentication Sources"
                 (choice
                  (string :tag "Just a file")
                  (const :tag "Default Secrets API Collection" default)
                  (const :tag "Login Secrets API Collection" "secrets:Login")

                  (const :tag "Default internet Mac OS Keychain"
                         macos-keychain-internet)

                  (const :tag "Default generic Mac OS Keychain"
                         macos-keychain-generic)

                  (list :tag "Source definition"
                        (const :format "" :value :source)
                        (choice :tag "Authentication backend choice"
                                (string :tag "Authentication Source (file)")
                                (list
                                 :tag "Secret Service API/KWallet/GNOME Keyring"
                                 (const :format "" :value :secrets)
                                 (choice :tag "Collection to use"
                                         (string :tag "Collection name")
                                         (const :tag "Default" default)
                                         (const :tag "Login" "Login")
                                         (const
                                          :tag "Temporary" "session")))
                                (list
                                 :tag "Mac OS internet Keychain"
                                 (const :format ""
                                        :value :macos-keychain-internet)
                                 (choice :tag "Collection to use"
                                         (string :tag "internet Keychain path")
                                         (const :tag "default" default)))
                                (list
                                 :tag "Mac OS generic Keychain"
                                 (const :format ""
                                        :value :macos-keychain-generic)
                                 (choice :tag "Collection to use"
                                         (string :tag "generic Keychain path")
                                         (const :tag "default" default))))
                        (repeat :tag "Extra Parameters" :inline t
                                (choice :tag "Extra parameter"
                                        (list
                                         :tag "Host"
                                         (const :format "" :value :host)
                                         (choice :tag "Host (machine) choice"
                                                 (const :tag "Any" t)
                                                 (regexp
                                                  :tag "Regular expression")))
                                        (list
                                         :tag "Protocol"
                                         (const :format "" :value :port)
                                         (choice
                                          :tag "Protocol"
                                          (const :tag "Any" t)
                                          ,@auth-source-protocols-customize))
                                        (list :tag "User" :inline t
                                              (const :format "" :value :user)
                                              (choice
                                               :tag "Personality/Username"
                                               (const :tag "Any" t)
                                               (string
                                                :tag "Name"))))))
                  (sexp :tag "A data structure (external provider)"))))

(defun totp-auth-sources ()
  "Initialise variable ‘totp-auth-sources’ if necessary and return it."
  (or totp-auth-sources
      (let ((case-fold-search t) secret-collection collection-list)
        ;; pick a freedesktop collection that matches "login" or
        ;; "Login" or similar:
        (setq collection-list
              (ignore-errors (secrets-list-collections)))
        (mapc (lambda (s)
                (if (string-match "^login$" s)
                    (setq secret-collection (concat "secrets:" s))))
              collection-list)
        ;; add the freedesktop login collection we found to our auth
        ;; source list _if_ 'default isn't already there:
        (setq totp-auth-sources
              (if (and secret-collection
                       (not (memq   'default          auth-sources))
                       (not (member secret-collection auth-sources)))
                  (copy-sequence (cons secret-collection auth-sources))
                (copy-sequence auth-sources)))
        totp-auth-sources)))

(defun totp-wrap-otpauth-url (s)
  "Take a TOTP secret S and encode it as an otpauth url.
This is not an exact reverse of ‘totp-unwrap-otpauth-url’ since that function
ignores some otpauth attributes for compatibility with other authenticators."
  (let ((service (cdr (assq :service s)))
        (user    (cdr (assq :user    s)))
        (secret  (cdr (assq :secret  s)))
        (digits  (cdr (assq :digits  s)))
        (allowed (cons ?@ url-unreserved-chars)))
    (or (memq digits '(6 8 10))
        (setq digits 6))
    (if (> (length user) 0)
        (format "otpauth://totp/%s%%3A%s?secret=%s;digits=%d"
                (url-hexify-string service allowed)
                (url-hexify-string user    allowed)
                (url-hexify-string secret  allowed) digits)
      (format "otpauth://totp/%s?secret=%s;digits=%d"
              (url-hexify-string service allowed)
              (url-hexify-string secret  allowed) digits)) ))

(defun totp-unwrap-otpauth-url (u)
  "Unpack an otpauth url U and extract the bits we care about.
Some settings (eg the chunk size) are ignored because they've
never been handled by google authenticator either, which just uses
the default."
  (let (srv query secret digits user)
    (setq u       (url-path-and-query u)
          srv     (replace-regexp-in-string "^/" "" (car u))
          srv     (url-unhex-string srv)
          query   (url-parse-query-string (cdr u))
          secret  (cadr (assoc "secret" query))
          digits  (cadr (assoc "digits" query)))
    (setq digits (if digits (string-to-number digits) 6)
          digits (if (< digits 6) 6 (if (> digits 10) 10 digits)))
    (if (string-match "^\\(.*?\\):\\(.*\\)" srv)
        (setq service (match-string 1 srv)
              user    (match-string 2 srv))
      (setq service srv))
    `((:service . ,service)
      (:user    . ,user   )
      (:secret  . ,secret )
      (:digits  . ,digits )) ))

(defun totp-unwrap-otp-blob (blob &optional label)
  "Unwrap a stored TOTP BLOB.
BLOB may be either an otpauth URL or a bare base32 encoded TOTP secret
Returns an alist of the form:\n
  ((:service . \"SOME-SERVICE-LABEL\")
   (:user    . \"SOME-USER-IDENT\")
   (:secret  . \"deadbeefdeadbeefdeadbeefdeadbeef\")
   (:digits  . 6))\n
Note that :user may be nil, :digits defaults to 6 if unspecified,
and service will default to LABEL if the stored blob was simply the
base32 encoded secret.\n
The secret will NOT be base32 decoded."
  (let ((u (url-generic-parse-url blob)))
    (if (equal (url-type u) "otpauth")
        ;; otpauth:// URL. extract the bits we care about:
        (totp-unwrap-otpauth-url u)
      ;; bare base32 encoded secret. make some stuff up:
      `((:secret  . ,(url-filename u))
        (:digits  . 6)
        (:service . ,label))) ))

(defun totp-storage-backends (&optional encrypted)
  "Return a list of available storage backends based on ‘auth-sources’.
If ENCRYPTED is true then only encrypted backends are considered.
Each entry is an alist of the form:
  ((:source    . function ‘auth-source-backend’ object)
   (:handler   . :secrets for a desktop secrets API or :default)
   (:encrypted . t if the backend is nontrivially encrypted, nil otherwise))"
  (delq nil
        (mapcar (lambda (s &optional secure type source handler)
                  (setq source  (slot-value s 'source)
                        type    (slot-value s 'type)
                        secure  (or (eq type 'secrets)
                                    (eq type 'plist)
                                    (equal (file-name-extension source) "gpg"))
                        handler (if (eq type 'secrets) :secrets :default))
                  (if (and encrypted (not secure))
                      nil
                    (list (cons :source    s)
                          (cons :handler   handler)
                          (cons :encrypted secure))))
                (mapcar #'auth-source-backend-parse (totp-auth-sources)))))

(defun totp-get-secrets-from-secrets-source (source)
  "Return an alist of secrets from SOURCE (a desktop secrets API auth-source).
The car of each cell will be the label by which the secrets API identifies
this secret, the cdr will be an alist as returned by ‘totp-unwrap-otp-blob’."
  (let (found vault next)
    (setq vault (slot-value source 'source))
    (mapc
     (lambda (schema)
       (mapc
        (lambda (label)
          (setq next  (secrets-get-secret vault label)
                ;;x   (message "secret:%S" next)
                ;;x   (message "attr  :%S" (secrets-get-attributes vault label))
                next  (totp-unwrap-otp-blob next label)
                next  (cons label next)
                found (cons next found)))
        (secrets-search-items vault :xdg:schema schema)))
     (cons totp-xdg-schema totp-alt-xdg-schemas))
    found))

(defun totp-get-secrets-from-default-source (source)
  "Return an alist of secrets from SOURCE (an auth-secrets source).
The car of each cell will be a [user@]host label and the cdr will be the
TOTP secret."
  (let (found)
    (mapc (lambda (x &optional host user secret label otpmeta)
            (setq host   (plist-get x :host  )
                  user   (plist-get x :user  )
                  secret (plist-get x :secret))
            (if (and host user)
                (setq label (concat user "@" host))
              (setq label (or user host)))
            (if (functionp secret) (setq secret (funcall secret)))
            (setq otpmeta (totp-unwrap-otp-blob secret label)
                  found   (cons (cons label otpmeta) found)))
          (auth-source-search-backends (list source)
                                       (list :port "totp")
                                       totp-max-tokens nil nil
                                       '(:port :secret)))
    found))

(defun totp-get-secrets-from-backend (backend)
  "Fetch secrets from a specific auth-source BACKEND."
  (when (cdr (assq :encrypted backend))
    (let (source)
      (setq source (cdr (assq :source  backend)))
      (cond ((eq (cdr (assq :handler backend)) :secrets)
             (totp-get-secrets-from-secrets-source source))
            (t
             (totp-get-secrets-from-default-source source))) )))

(defun totp-same-secret (a b)
  "Test whether secrets A and B are the same.
\nNOTE: This is not a strict test of equality - rather we are checking to
see if the user and service components of the secret identifier are the
same, ie probably intended for the same target."
  (and (equal (assq :service a) (assq :service b))
       (equal (assq :user    a) (assq :user    b))))

(defun totp-get-backend-for-secret (s)
  "Return the backend in which secret S is store, or the default backend."
  (let (backends vault default target secrets)
    (setq backends (totp-storage-backends)
          default  (car backends))
    (while (and (not target) backends)
      (setq vault    (car backends)
            secrets  (totp-get-secrets-from-backend vault)
            backends (cdr backends))
      (if (cl-member s secrets :test 'totp-same-secret)
          (setq target vault)))
    (or target default)))

(defun totp-secret-make-label (secret)
  "Take a ‘totp-unwrap-otp-blob’ structure SECRET and generate a label from it.
The label will be based on its user and service fields."
  (let (user srv-host)
    (setq user     (cdr (assq :user secret))
          srv-host (cdr (or (assq :service secret)
                            (assq :host    secret))))
    (if (and user srv-host)
        (concat user "@" srv-host)
      (or user srv-host "nobody@unknown"))))

(defun totp-secret-make-label-and-wrapper (secret &optional label)
  "Take a ‘totp-unwrap-otp-blob’ SECRET and generate a LABEL and otpauth URL.
LABEL is used as the default label.  If not supplied, ine is generated for you
by ‘totp-secret-make-label’."
  (let ((wrapped (totp-wrap-otpauth-url secret)))
    (if (not label)
        (setq label (totp-secret-make-label secret)))
    (cons label wrapped)))

(defun totp-get-item-attribute (item attribute)
  "Take a freedesktop secrets ITEM and return its ATTRIBUTE value."
  (ignore-errors
    (cadr (assoc attribute (cdr (assoc "Attributes" item))))))

(defun totp-save-secret-to-secrets-source (source secret &optional label)
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to the freedesktop Secrets Service.
The secret is saved with the with description LABEL.\n
SOURCE is an auth-source representing the Secrets Service Collection
to save in (usually the login keyring).\n
If LABEL is not supplied, one is constructed based on the contents
of SECRET.
Gnome Keyring and KWallet are examples of the freedesktop secrets services."
  (let (payload vault created)
    (setq payload (totp-secret-make-label-and-wrapper secret label)
          vault   (slot-value source 'source))
    ;; (message "(secrets-create-item %S %S %S :xdg:schema %S)"
    ;;          vault
    ;;          (car payload)
    ;;          (cdr payload)
    ;;          totp-xdg-schema)
    (setq created (secrets-create-item vault
                                       (car payload)
                                       (cdr payload)
                                       :xdg:schema totp-xdg-schema))
    ;; de-duplicate by hand:
    (when totp-secrets-create-item-workaround
      (let (path props schema maybe-dup)
        (setq stored (cdr (assq :secret secret)) ;; secret we just stored
              path   (secrets-collection-path vault))
        (dolist (item-path (secrets-get-items path))
          (when (not (equal created item-path))
            (setq props  (secrets-get-item-properties item-path)
                  schema (totp-get-item-attribute props "xdg:schema"))
            (when (equal totp-xdg-schema schema)
              (setq maybe-dup (secrets-get-secret vault item-path)
                    maybe-dup (totp-unwrap-otp-blob maybe-dup)
                    maybe-dup (cdr (assq :secret maybe-dup))) ;; another secret
              (when (equal stored maybe-dup) ;; new and old secrets are equal
                (secrets-delete-item vault item-path)))))))
    created))

(defun totp-save-secret-to-default-source (source secret &optional label)
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to the auth-source SOURCE.
\nSOURCE is any valid auth-source except a freedesktop Secrets Service.\n
LABEL is used as a hint when constructing the host attribute of the
stored secret if it is both supplied and the secret does not have a
host value."
  (let (payload user host password saver)
    (setq user     (or (cdr (assq :user secret)) "-")
          payload  (totp-secret-make-label-and-wrapper secret label)
          host     (or (cdr (or (assq :service secret)
                               (assq :host    secret)))
                      (car payload))
          password (cdr payload))
    (setq saver (apply (slot-value source 'create-function)
                 `(:backend ,source
                            :host    ,host
                            :user    ,user
                            :port    "totp"
                            :secret  ,password
                            :create  t))
          saver (and saver
                     (car saver)
                     (plist-get (car saver) :save-function)))
    (if saver
        (funcall saver)
      (message "No saver for secret %s in backend %S" (car payload) source))))

(defun totp-save-secret (secret &optional backend)
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to BACKEND.
\nIf BACKEND is unspecified search the available secret sources for SECRET
and save to the first one that contains it.\n
If SECRET is not found (see ‘totp-get-backend-for-secret’) then choose
the first encrypted backend returned by ‘totp-storage-backends’."
  (if (not backend)
      (setq backend (or (totp-get-backend-for-secret secret)
                        (car (totp-storage-backends :encrypted)))))
  (let ((source (cdr (assq :source backend))))
    (cond ((eq (cdr (assq :handler backend)) :secrets)
           (totp-save-secret-to-secrets-source source secret))
          (t
           (totp-save-secret-to-default-source source secret)))))

(defun totp-secrets ()
  "Fetch a list of all known TOTP secrets."
  (apply 'nconc (mapcar #'totp-get-secrets-from-backend (totp-storage-backends))))

(defun totp-hmac-message (counter)
  "Take COUNTER (an integer) and return its 8-byte big-endian representation."
  (let ((hi-4 (logand #xffffffff (lsh counter -32)))
        (lo-4 (logand #xffffffff counter)))
    (bindat-pack '((:hi4 u32) (:lo4 u32))
                 `((:hi4 . ,hi-4)
                   (:lo4 . ,lo-4)))))

(defun totp-truncate-hash (hmac-hash)
  "Given a 20 byte string or vector HMAC-HASH:
Use the lowest 4 bits of the final byte as an offset,
Read 4 bytes starting at that offset as a big-endian 32-bit integer,
with the highest bit forced to 0 (ie a 31 bit integer)."
  (let (offset b0 b1 b2 b3)
    (setq offset (logand #x0f (aref hmac-hash (1- (length hmac-hash))))
          b0     (logand #x7f (aref hmac-hash offset))
          b1     (logand #xff (aref hmac-hash (+ 1 offset)))
          b2     (logand #xff (aref hmac-hash (+ 2 offset)))
          b3     (logand #xff (aref hmac-hash (+ 3 offset))))
    (logior (lsh b0 24) (lsh b1 16) (lsh b2 8) b3)))

(defvar totp-override-time nil
  "This value is used instead of the seconds since epoch if it is set.")

(defun totp-generate-otp (secret &optional digits offset chunk algo)
  "Given the following:
- a string (or ‘totp-unwrap-otp-blob’ struct) SECRET
- a length DIGITS (default 6)
- an integer time skew OFFSET (default 0)
- a time slice size CHUNK (default 30)
- a cryptographic hash algorithm ALGO (default sha1)
Return (TOTP TTL EXPIRY) where TOTP is the time-based one time password,
TTL is the number of seconds the password is good for at the time of generation
and EXPIRY is the seconds after the epoch when the TOTP expires."
  (if (listp secret)
      (setq secret (cdr (assq :secret secret))))
  (let ((digits     (or digits   6))
        (offset     (or offset   0))
        (chunk      (or chunk   30))
        (algo       (or algo 'sha1))
        (secret     (base32-decode (upcase secret)))
        (now        (or totp-override-time (floor (time-to-seconds))))
        then counter ttl expiry msg hash totp)
    (setq then    (- now offset)
          counter (/ then chunk)
          ttl     (- chunk (% now  chunk))
          expiry  (+ now  chunk)
          msg     (totp-hmac-message counter)
          hash    (hmac secret msg algo)
          totp    (% (totp-truncate-hash hash) (expt 10 digits)))
    (let ((fmt (format "%%0%dd" digits)))
      (setq totp (format fmt totp)))
    (list totp ttl expiry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI code
(defvar totp-display-ttl    nil)
(defvar totp-display-label  nil)
(defvar totp-display-expiry nil)
(defvar totp-display-secret nil)
(defvar totp-display-oldpwd nil)

(defun totp-update-paste-buffers (old new)
  "For each copy/paste buffer selected by ‘totp-auto-copy-password’:
Update the contents to password NEW (if it contains password OLD,
or if OLD is unset)."
  ;;(message "totp-update-paste-buffers %S (%S)" old new totp-auto-copy-password)
  (mapc (lambda (type &optional ok)
          (with-demoted-errors "gui get/set selection error: %S"
            (setq ok (if old (equal old (gui-get-selection type)) t))
            (if ok (gui-set-selection type (or new "")))))
        totp-auto-copy-password))

(defun totp-cancel-this-timer ()
  "Cancel the timer whose callback this is called from."
  (let ((n 1) (cancelled 0) f cb cb-args)
    (while (and (setq f (backtrace-frame n #'totp-cancel-this-timer))
                (not (car f)))
      (setq n (1+ n)))
    (when (and f (car f))
      (setq cb      (cadr f)
            cb-args (cddr f))
      (dolist (timer timer-list)
        (when (and (eq (timer--function timer) cb)
                   (equal (timer--args timer) cb-args))
          (cancel-timer timer)
          (setq cancelled (1+ cancelled)))))
    cancelled))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOTP buffer based UI
(defun totp-update-token-display (buf &optional otp token)
  "Update a TOTP token display buffer BUF with the lifespan and current token.
Will also call ‘totp-update-paste-buffers’.
OTP and TOKEN are used internally and need not be passed."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (erase-buffer)
        (if (or (not totp-display-ttl)
                (not totp-display-expiry)
                (not totp-display-oldpwd))
            ;; metadata unset, need to generate TOTP
            (setq otp                 (totp-generate-otp totp-display-secret)
                  token               (nth 0 otp)
                  totp-display-ttl    (nth 1 otp)
                  totp-display-expiry (nth 2 otp))
          ;; metadata already set, work out our new ttl:
          (setq token totp-display-oldpwd
                totp-display-ttl
                (floor (- (time-to-seconds) totp-display-expiry))))
        ;; regenerate metadata if the ttl is <= 0
        (if (>= 0 totp-display-ttl)
            (setq otp (totp-generate-otp totp-display-secret)
                  token               (nth 0 otp)
                  totp-display-ttl    (nth 1 otp)
                  totp-display-expiry (nth 2 otp)))
        ;; update the copy/paste buffers if necessary:
        (totp-update-paste-buffers totp-display-oldpwd token)
        (setq totp-display-oldpwd token)
        ;; display the current token
        (insert (format "TOTP %s [%02ds]: %s\n"
                        totp-display-label totp-display-ttl token)))
    (totp-cancel-this-timer)))

(defun totp-display-token-buffer (secret &optional label)
  "Display buffer with the current token for SECRET with label LABEL."
  (let (ui-buffer)
    (or label
        (setq label (totp-secret-make-label secret)))
    (setq ui-buffer (get-buffer-create (format "*TOTP %s*" label)))
    (set-buffer ui-buffer)
    (mapc 'make-local-variable '(totp-display-ttl
                                 totp-display-label
                                 totp-display-expiry
                                 totp-display-oldpwd
                                 totp-display-secret))
    (setq totp-display-label  label
          totp-display-secret (cdr (assq :secret secret))
          totp-display-oldpwd nil
          totp-display-ttl    nil
          totp-display-expiry nil)
    (pop-to-buffer ui-buffer)
    (run-with-timer 0 1 #'totp-update-token-display ui-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Notification based UI
(defun totp-notification-action (id key secret)
  "Handle a desktop notification “copy” action.
ID is the freedesktop notification id.
KEY is the action (we currently only handle \"copy\").
SECRET is a suitable argument for ‘totp-generate-otp’.
\nCopy the current OTP token for SECRET with `totp-update-paste-buffers',
then close the notification.
\nIf the current token is about to expire (see ‘totp-minimum-ui-grace’)
then wait until it is time to renew the token before doing anything."
  (when (equal "copy" key)
    (let (otp ttl token)
      (setq otp (totp-generate-otp secret)
            ttl (nth 1 otp))
      (when (>= totp-minimum-ui-grace ttl)
        (sit-for ttl)
        (setq otp (totp-generate-otp secret)))
      (setq token (nth 0 otp))
      (let ((totp-auto-copy-password (or totp-auto-copy-password '(PRIMARY))))
        (totp-update-paste-buffers nil token)))
    (notifications-close-notification id)))

(defun totp-update-token-notification (id label secret)
  (let (otp text ttl)
    (setq otp  (totp-generate-otp secret)
          ttl  (nth 1 otp)
          text (if (>= totp-minimum-ui-grace ttl)
                   "Generating…  [⌛]"
                 (format "%s  [%02ds]" (nth 0 otp) ttl)))
    (notifications-notify
     :title       label
     :replaces-id id
     :body        text
     :actions    '("default" "Close" "copy" "Copy")
     :timeout     0
     :resident    t)))

(defun totp-display-token-notification (secret &optional label)
  "Display a notification with the current token for SECRET with label LABEL."
  (or label
      (setq label (totp-secret-make-label secret)))
  (let (nid update)
    (setq update (timer-create)
          nid    (notifications-notify
                  :title     label
                  :body      "Generating…  [⌛]"
                  :actions  '("default" "Close" "copy" "Copy")
                  :timeout   0
                  :resident  t
                  :on-action (lambda (id key)
                               (totp-notification-action id key secret))
                  :on-close  (lambda (_id _key) (cancel-timer update))))
    (timer-set-time     update (current-time) 1)
    (timer-set-function update
                        #'totp-update-token-notification (list nid label secret))
    (timer-activate     update)
    update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic UI
(defun totp-display-token (secret &optional label)
  "Display the TOTP token for secret according to ‘totp-display-token-method’.
SECRET is a string or structure consumable by ‘totp-generate-otp’,
LABEL is a label or description of the secret (eg its user and service
information).
LABEL will be initialised by ‘totp-secret-make-label’ if unset."
  (or label
      (setq label (totp-secret-make-label secret)))
  (if totp-display-token-method
      (funcall totp-display-token-method secret label)
    (if (ignore-errors (and (require 'notifications)
                            (notifications-get-server-information)))
        (totp-display-token-notification secret label)
      (totp-display-token-buffer secret label))))

;;;###autoload
(defun totp (&optional secret label)
  "Generate a TOTP token for SECRET, identified by LABEL, and show it."
  (interactive
   (let ((secrets (totp-secrets)) (completion-styles '(substring)) key)
     (setq key (completing-read "Generate TOTP: " secrets))
     (list (cdr (assoc key secrets)) key)))
  (totp-display-token secret label))

(autoload 'totp-import-file "totp-interop"
  "Import an RFC6238 TOTP secret or secrets from FILE.
FILE is processed by ‘totp-load-file’ and each secret extracted
is passed to ‘totp-save-secret’."
  t)

(provide 'totp-auth)
;;; totp-auth.el ends here
