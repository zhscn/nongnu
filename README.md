# Geiser for STklos

See the Geiser manual for usage.

# Supported features

* evaluation of sexps, definitions, regions and whole buffers
* loading Scheme files
* adding paths to `load-path`
* macroexpansion
* symbol completion
* listing of module exported symbols

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


# About the implementation:

The following functions were defined on the STklos side:

* `geiser:eval`
* `geiser:load-file`
* `geiser:add-to-load-path`
* `geiser:macroexpand`
* `geiser:no-values`
* `geiser:symbol-documentation`
* `geiser:module-exports`
* `geiser:module-completions`
* `geiser:completions`

They have comments that help understand how they work.


## Credits

Thanks to [Jao](https://gitlab.com/jaor) and  co-developers for creating and maintaining Geiser, and for helping the development of this package.
Thanks also to Erick Gallesio and his co-developers for [STklos](https://stklos.net).

## License
Copyright (C) 2020 Jerônimo Pellegrini

Authors: Jerônimo Pellegrini

This program is free software; you can redistribute and/or modify it under the terms of the BSD 3-Clause "New" or "Revised" License.
You should have received a copy of the license along with this program. If not, see https://spdx.o
rg/licenses/BSD-3-Clause.html.
