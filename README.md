# totp.el - Time-based One Time Password support for emacs

This generates RFC6238 Time-based One Time Passwords,
(in other words, what Google Authenticator implements).

## Getting started

This will be an elpa/melpa/marmalade style package at some point,
but for now either:

 - drop the .el files somewhere in your load path and invoke one of:
   - (require 'totp)
   - (load-file "totp")
   - (load-library "totp")
 - (load-file "/full/path/to/totp.el")

totp.el will look find its copies of base32.el and hmac.el in
its own directory by default.

If you want to import TOTP secrets you can invoke:

  M-x totp-auth-import-file RET

This supports:

  - simple base32 encoded TOTP secrets (max 1 per file)
  - otpauth:// scheme URLs in a text file (any number per file)
  - otpauth-migration:// scheme URLs in a text file (any number per file)
  - a mix of the above URL schemes in a text file
  - QR codes encoding any mix of the URL schemes above

Once you have at least one secret available, invoke:

  M-x totp RET

You can tab-complete based on the label of the secret and a
buffer displaying the token will be displayed (and updated as
the displayed token expires).

## Security

Your secrets can be fetched from any auth-source source.

Your secrets will only ever be stored by this package in
auth-source backends that are encrypted.

You secret(s) are only held in memory while a TOTP is being
generated, or while a TOTP display buffer is being updated.
