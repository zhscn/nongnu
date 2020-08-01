# Geiser for STklos

See the Geiser anual for usage.

# Unsupported features

* finding the definition of a symbol (no support in STklos)
* seeing callees and callers of a procedure (no support in STklos)
* looking up symbols in the manual (would need to download the index from STklos manual and parse the DOM of its index; a bit too much, maybe someday...)

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
