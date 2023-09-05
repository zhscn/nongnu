# flymake-guile

Flymake Backend for [GNU Guile][guile] using `guild` compiler.

# Requirements

- Emacs >= 26
- Guile >= 2.0
- Geiser (optional)

# Installation

Verify that `guild` is in your `PATH`, then add `flymake-guile.el`
to your Emacs load-path.

Alternatively, you can install it using [Guix][guix]. Clone the repository and then:
```bash
guix package -f flymake-guile/guix.scm
```

## Usage
Verify that `flymake-mode` is enabled ad then add the following to
your Emacs configuration:

```elisp
(add-hook 'scheme-mode-hook #'flymake-guile)
```

Emacs 29 and later:

```elisp
(use-package flymake-guile
  :ensure t
  :hook (scheme-mode-hook . flymake-guile))
```

## Customization

- `flymake-guile-warnings` The warning level or a list of warning
  types (default level 3), see: `guild compile -W help` for more detail.
- `flymake-guile-guild-executable` Name of the Guile `guild` executable.
- `flymake-guile-guild-args` Additional arguments for `guild compile`.

## License
![Licenses GPLv3](https://www.gnu.org/graphics/gplv3-127x51.png
"License under GPLv3")

See [COPYING](COPYING) for more detail.

[guile]: https://www.gnu.org/software/guile/
[geiser]: https://www.nongnu.org/geiser/
[guix]: https://guix.gnu.org/
