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

## From MELPA

Geiser-STklos is available on MELPA, so `M-x install-package` followed by `geiser-stklos`
should get it installed, and this is the recommended method.

## From source

There are other ways to install Geiser and Geiser-STklos. One possibility is
to add this to your Emacs configuration (Guile is used here as an example
of another supported implementation, used along with Geiser-STklos):

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

### Testing

Geiser-STklos comes with two test suites -- one for the Emacs Lisp part of
the system (using `ert`), and one for the STklos part (using STklos' own test
system). To run both suites:

```
make test
```

The STklos tests will create a log file called `TEST.LOG`. The Emacs Lisp tests
will write to `test-emacs-stdout.log`, but only when errors occur.

The tests are very rudimentary because writing a full test suite for the Emacs
Lisp part would be a bit complex, although not impossible: we'd need to start
STklos, load geiser on it, and feed it some forms before starting the tests.

# Bugs

See the issue tracker in Gitlab.

IMPORTANT: when writing STklos keywords, use the sharp sign (`:#a` instead of `:a`),
otherwise Geiser will treat is as a symbol (`|:a|`), not a keyword.

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
