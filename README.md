# Geiser for STklos

See the Geiser manual for usage.

# Supported features

* evaluation of sexps, definitions, regions and whole buffers
* loading Scheme files
* adding paths to `load-path`
* macroexpansion
* symbol completion
* listing of module exported symbols
* autodoc (signature of procedurs and values of symbols are displayed in the minibuffer
  when the mouse hovers over their names)
* symbol documentation (docstrings for procedures, and values of variables)

# Unsupported features

* finding the definition of a symbol (no support in STklos)
* seeing callees and callers of a procedure (no support in STklos)
* looking up symbols in the manual (would need to download the index from STklos manual and parse the DOM of its index; a bit too much, maybe someday...)

# Notes:

* Squarify (alternating between "[" and "(" ) only works when the cursor is inside a form

# Installation

There are many ways to install Geiser and Geiser-STklos. One possibility is
to add this to your Emacs configuration:

```
(setq-default geiser-active-implementations
              '(stklos
                guile)

(add-to-list 'load-path "~/PATH_TO/geiser-guile")
(add-to-list 'load-path "~/PATH_TO/geiser-stklos")

(load-file "~/PATH_TO_geiser/elisp/geiser.el") ;; only if not installed via MELPA

(require 'geiser)
(require 'geiser-guile)
(require 'geiser-stklos)
```

# Bugs

See the issue tracker in Gitlab.


# About the implementation

Geiser support for a Scheme implementation consists of two programs:
one on Emacs' side, and one on Scheme's side.

The Emacs Lisp part is in the file `geiser-stklos.el`
The STklos part is in `geiser-stklos.stk`. The STklos implementation is inside a module called `GEISER`.

In both files, I tried to include comments explaining what each function does.

## Credits

Thanks to [Jao](https://gitlab.com/jaor) and co-developers for creating and maintaining Geiser, and for helping the development of this package.
Thanks also to Erick Gallesio and his co-developers for [STklos](https://stklos.net).

## License
Copyright (C) 2020 Jerônimo Pellegrini

Authors: Jerônimo Pellegrini

This program is free software; you can redistribute and/or modify it under the terms of the BSD 3-Clause "New" or "Revised" License.
You should have received a copy of the license along with this program. If not, see https://spdx.org/licenses/BSD-3-Clause.html.
