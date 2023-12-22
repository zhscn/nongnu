# totp.el - Time-based One Time Password support for emacs

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

If you want to import TOTP secrets you can load totp-interop
and invoke:

  M-x totp-import-file RET

This supports:

  - simple base32 encoded TOTP secrets (max 1 per file)
  - otpauth:// scheme URLs in a text file (any number per file)
  - otpauth-migration:// scheme URLs in a text file (any number per file)
  - a mix of the above URL scemes in a text file
  - QR codes encoding any mix of the URLs above

Once you have at least one secret available, invoke:

  M-x totp RET

You can tab-complete based on the label of the secret and a
buffer displaying the token will be displayed (unless that
token is about to expire - right now you just get a placeholder
message but next TODO is to make that automatically delay and
then generate a new token for you).
