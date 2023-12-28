;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Copyright © 2022,2023 Vivek Das Mohapatra <vivek@etla.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Vivek Das Mohapatra <vivek@etla.org>
;; Keywords: 2FA two-factor totp otp

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

(defgroup totp nil "Time-based One Time Passwords"
  :prefix "totp"
  :group 'data)

(defconst totp-xdg-schema "org.freedesktop.Secret.TOTP")

(defcustom totp-alt-xdg-schemas
  '("com.github.bilelmoussaoui.Authenticator")
  "A list of fallback XDG schemas which are associated with TOTP secrets."
  :type '(repeat string)
  :group 'totp)

(defcustom totp-minimum-ui-grace 3
  "The minimum time to expiry a TOTP must have for the interactive
UI to show it to you. If the generated token has less then this much time 
to live then UI code should instead generate the next TOTP in sequence
and wait until it is valid before showing it to the user.\n
Noninteractive TOTP code must return TOTP values along with their 
lifespan (at the time of generation) and their absolute expiry time,
and may also return the next TOTP value and the same information for that
as well."
  :type  'integer
  :group 'totp)

(defcustom totp-max-tokens 1024
  "The maximum number of tokens totp will try to fetch and process."
  :group 'totp
  :type  'integer)

(defcustom totp-file-import-command '("zbarimg" "-q" "@file@")
  "The command and parameters used to parse a QR code image.
@file@ is a placeholder for the file name."
  :group 'totp
  :type  '(repeat string))

(defcustom totp-secrets-create-item-workaround t
  "The replace parameter of org.freedesktop.Secrets.CreateItem
is unreliable. If this option is on (the default) then we attempt
delete duplicated secrets when we save a secret via this API.\n
If it is off then you are likely to end up with multiple copies of
a secret if you ever re-import it."
  :group 'totp
  :type  'boolean)

(defcustom totp-auth-sources nil
  "Serves the same purpose as ‘auth-sources’, but for the TOTP
package. If unset (the default) this will be initialised to a list
consisting of the contents of ‘auth-sources’ with the freedesktop
secrets service login session prepended to it, if it is available."
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
  "Initialise ‘totp-auth-sources’ if necessary and return it."
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
  "Unpack an otpauth url and extract the bits we care about.
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
  "Unwrap a stored TOTP BLOB - either an otpauth URL or a bare
base32 encoded TOTP secret, and return an alist of the form:\n
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

;; (totp-unwrap-otp-blob
;;  (concat "otpauth://totp/"
;;          "moomins%3Avivek@example.org"
;;          "?secret=deadbeefdeadbeefdeadbeefdeadbeef&digits=6"))
;; (totp-unwrap-otp-blob "deadbeefdeadbeefdeadbeefdeadbeef"
;; "something")

(defun totp-storage-backends (&optional encrypted)
  "Return a list of available storage backends based on `auth-sources`.
If ENCRYPTED is true then only encrypted backends are considered.
Each entry is an alist of the form:
  ((:source    . `auth-source-backend` object)
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
this secret, the cdr will be an alist as returned by `totp-unwrap-otp-blob`."
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
  (when (cdr (assq :encrypted backend))
    (let (source)
      (setq source (cdr (assq :source  backend)))
      (cond ((eq (cdr (assq :handler backend)) :secrets)
             (totp-get-secrets-from-secrets-source source))
            (t
             (totp-get-secrets-from-default-source source))) )))

(defun totp-same-secret (a b)
  "Test whether secrets A and B are the same.\n
NOTE: This is not a strict test of equality - rather we are checking to 
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
  "Take a ‘totp-unwrap-otp-blob’ structure SECRET and generate a label
from it (based on its user and service fields)."
  (let (user srv-host)
    (setq user     (cdr (assq :user secret))
          srv-host (cdr (or (assq :service secret)
                            (assq :host    secret))))
    (if (and user srv-host)
        (concat user "@" srv-host)
      (or user srv-host "nobody@unknown"))))

(defun totp-secret-make-label-and-wrapper (secret &optional label)
  (let ((wrapped (totp-wrap-otpauth-url secret)))
    (if (not label)
        (setq label (totp-secret-make-label secret)))
    (cons label wrapped)))

(defun totp-get-item-attribute (item attribute)
  (ignore-errors
    (cadr (assoc attribute (cdr (assoc "Attributes" item))))))

(defun totp-save-secret-to-secrets-source (source secret &optional label)
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to the freedesktop
Secrets Service (eg gnome-keyring or kwallet) with description LABEL.\n
SOURCE is an auth-source representing the Secrets Service Collection
to save in (usually the login keyring).\n
If LABEL is not supplied, one is constructed based on the contents
of SECRET."
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
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to the auth-source SOURCE.\n
SOURCE is any valid auth-source except a freedesktop Secrets Service.\n
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
  "Save SECRET (see ‘totp-unwrap-otp-blob’) to BACKEND.\n
If BACKEND is unspecified search the available secret sources for SECRET
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
  "Take counter (an integer) and return an 8-byte string containing
its big-endian (highest-to-lowest byte) representation."
  (let ((hi-4 (logand #xffffffff (lsh counter -32)))
        (lo-4 (logand #xffffffff counter)))
    (bindat-pack '((:hi4 u32) (:lo4 u32))
                 `((:hi4 . ,hi-4)
                   (:lo4 . ,lo-4)))))

(defun totp-truncate-hash (hmac-hash)
  "Given a 20 byte string or vector:
Use the lowest 4 bits of the final byteas an offset,
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
  "Given:
  a string (or ‘totp-unwrap-otp-blob’ struct) SECRET
  a TOTP length DIGITS (default 6)
  an integer time skew OFFSET (default 0)
  a time slice size CHUNK (default 30)
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

(defvar totp-display-ttl    nil)
(defvar totp-display-label  nil)
(defvar totp-display-expiry nil)
(defvar totp-display-secret nil)

(defun totp-cancel-timer (fun buf)
  "Cancel timers which call FUN with buffer BUF as the first argument."
  (dolist (timer timer-list)
    (if (and (eq (timer--function timer) fun)
             (eq (car (timer--args timer)) buf))
        (cancel-timer timer))))

(defun totp-update-token-display (buf &optional otp token)
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (erase-buffer)
        (if (or (not totp-display-ttl)
                (not totp-display-expiry))
            ;; metadata unset, need to generate TOTP
            (setq otp                 (totp-generate-otp totp-display-secret)
                  token               (nth 0 otp)
                  totp-display-ttl    (nth 1 otp)
                  totp-display-expiry (nth 2 otp))
          ;; metadata already set, work out our new ttl:
          (setq totp-display-ttl
                (floor (- (time-to-seconds) totp-display-expiry))))
        ;; regenerate metadata if the ttl is <= 0
        (if (>= 0 totp-display-ttl)
            (setq otp (totp-generate-otp totp-display-secret)
                  token               (nth 0 otp)
                  totp-display-ttl    (nth 1 otp)
                  totp-display-expiry (nth 2 otp)))
        (insert (format "TOTP %s [%02ds]: %s\n"
                        totp-display-label totp-display-ttl token)))
    (totp-cancel-timer #'totp-update-token-display buf)))

(defun totp-display-token (secret &optional label)
  (let (ui-buffer)
    (or label
        (setq label (totp-secret-make-label secret)))
    (setq ui-buffer (get-buffer-create (format "*TOTP %s*" label)))
    (set-buffer ui-buffer)
    (mapc 'make-local-variable '(totp-display-ttl
                                 totp-display-label
                                 totp-display-expiry
                                 totp-display-secret))
    (setq totp-display-label  label
          totp-display-secret (cdr (assq :secret secret))
          totp-display-ttl    nil
          totp-display-expiry nil)
    (pop-to-buffer ui-buffer)
    (run-with-timer 0 1 #'totp-update-token-display ui-buffer)))

(defun totp (&optional secret label)
  (interactive
   (let ((secrets (totp-secrets)) (completion-styles '(substring)) key)
     (setq key (completing-read "Generate TOTP: " secrets))
     (list (cdr (assoc key secrets)) key)))
  (totp-display-token secret label))

(provide 'totp)
