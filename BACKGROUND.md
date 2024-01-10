# Time Based One-Time Passwords

[This type of] OTP is described in [RFC-6238.html](RFC-6238.html "RFC6238")
and can be of two tpes:

  * Event based (HOTP)
  * Time based (TOTP)

The fundamentals are essentially the same - a secret shared by the the
authoriser and the user is used to generate passwords based on an additional
piece of information (a counter).

## Event-Based (HOTP)

In HOTP (H for Event-based. Don't @ me, that's what they called it) the
counter is either incremented by a pre-agreed amount each time it is used
(tricky to keep in sync if more than one device is used to log in from)
or can be supplied to the user as a challenge at the point of authorisation.

## Time-Based (TOTP)

In TOTP the counter is the number of N `second` intervals since a given EPOCH,
where N and EPOCH are pre-agreed (They're always 30 second intervals since
the unix epoch, at least in every implementation encountered so far).

## Implementation Details

Both mechanisms generate 10 digit passwords, but in practice only 6
are shown to the user (the spec allows for any number, but again, I've
never actually observed that).

The Hash algorithm used can also vary. I've only ever seen SHA1 in use.

Google authenticator actually ignores everything except the shared secret,
so in practice it's always a ```{ time_t=0, 30s, SHA1, 6-digit }``` password.

## Import/Export

There are two main "encodings" of secret-transfer, either of which may
be stored in a QR code (a fairly common extra wrapper).

## otpauth:// URLs

These URLs are fairly straightforward. They hold one secret per URL.

  ```
  otpauth://totp/SERVICE:USER?secrets=SECRET;digits=DIGITS
  otpauth://totp/SERVICE:USER?secrets=SECRET
  otpauth://totp/SERVICE?secrets=SECRET;digits=DIGITS
  otpauth://totp/SERVICE?secrets=SECRET
  ```

Where:

  * SECRET is a base32 encoded sequence of bytes,
    * base32 alphabet: `ABCDEDGHIJKLMNOPQRSTUVWXYZ234567`
    * base32 padding: `=`
  * SERVICE is the service name (eg "Mom’s Friendly Robot Company")
  * USER is the user name (eg "Bender B. Rodriguez") and may be omitted
  * DIGITS is a number from 6 to 10 and may be omitted

## otpauth-migration:// URLs

These URLs are more complex and less human-decipherable.
They were reverse-engineered by [Alexander Bakker](https://alexbakker.me/post/parsing-google-auth-export-qr-code.html "Alexander Bakker")
They may hold a number of secrets per url and take the form:

  ```
  otpauth-migration://offline?data=BLOB
  ```

BLOB is a base64 (not base32!) encoded protobuf payload.
It consists of:

  * Any number of field 1 `len` each containing an otp-parameter
  * field 2 `varint` or `int32`: version
  * field 3 `varint` or `int32`: batch size (number of otp-parameters)
  * field 4 `varint` or `int32`: index (which URL is a set this is)
  * field 5 `varint` or `int32`: a batch id

Apart from the otp-parameter repeating field They're not hugely of interest to
us, except that we can tell if there's a URL missing from a batch.

Each otp-parameter in turn consists of:

  * field 1 `len`: secret (_not_ encoded - a raw byte string)
  * field 2 `len`: "service" or "service\:user"
  * field 3 `len`: "service" (yes, this overlaps with field 2)
  * field 4 `varint`: "algo"
    * 0 - ? (ie `sha1`), 1 - `sha1`, 2 - `sha256`, 3 - `sha512`, 4 - `md5`
  * field 5 `varint`: "digits"
    * 0 = `?` (ie `6`), 1 - `6`, 2 - `8`
    * Yes, this a is a properly bonkers way of encoding a 1 digit number
  * field 6 `varint`: "type"
    * 0 = `?`, 1 = `HOTP`, 2 = `TOTP`
  * field 7 `varint`: "counter"
    * only used for `HOTP`

Everything except the secret is technically optional, most are ignored
by many implementations.
