# totp-auth.el - Time-based One Time Password support for emacs

This package generates RFC6238 Time-based One Time Passwords
(in other words, what Google Authenticator implements)
and displays them (as well as optionally copying them to
the clipboard/primary selection), updating them as they expire.

It retrieves the shared secrets used to generate TOTP tokens
with ‘auth-sources’ and/or the freedesktop secrets API (aka
Gnome Keyring or KWallet).

## Getting started

This can be installed from melpa and melpa-stable as totp-auth.
Or if you prefer, you can:

 - drop the .el files somewhere in your load path and invoke one of:
   - (require 'totp-auth)
   - (load-file "totp-auth")
   - (load-library "totp-auth")
 - (load-file "/full/path/to/totp-auth.el")

totp-auth.el will look find its copies of base32.el and totp-auth-hmac.el in
its own directory by default.

If you want to import TOTP secrets you can invoke:

  M-x totp-auth-import-file RET

This supports:

  - simple base32 encoded TOTP secrets (max 1 per file)
  - otpauth:// scheme URLs in a text file (any number per file)
  - otpauth-migration:// scheme URLs in a text file (any number per file)
  - a mix of the above URL schemes in a text file
  - QR codes encoding any mix of the URL schemes above

QR codes require ‘zbarimg’ to import.

Once you have at least one secret available, invoke:

  M-x totp-auth RET

You can tab-complete based on the label of the secret and a
buffer displaying the token will be displayed (and updated as
the displayed token expires).

If you want to export your secrets you can invoke:

  M-x totp-auth-export-file RET 

Which will export to either a text file (or a PGP encrypted file
if you have ‘epa-file’ set up) or a QR code, in either otpauth://
or otpauth-migration:// format.

QR code generation requires ‘qrencode’.

## Security

Your secrets can be fetched from any auth-source source.

Your secrets will only ever be stored by this package in
auth-source backends that are encrypted.

You secret(s) are only held in memory while a TOTP is being
generated, or while a TOTP display buffer is being updated.
