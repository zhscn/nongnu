# Haskell Mode: The Next Generation

This is an exploratory alternative to [`haskell-mode`](https://github.com/haskell/haskell-mode/) that answers the question *how would we support Haskell in GNU Emacs if we started today?*

## Why?

`haskell-mode` is almost 30 years old and has accumulated more than 25,000 lines of code. It's too much to handle.

Meanwhile, the GNU Emacs ecosystem has evolved to provide many features that `haskell-mode` independently implemented, such as [`projectile`](https://github.com/bbatsov/projectile), [`comint`](https://masteringemacs.org/article/comint-writing-command-interpreter), [`highlight-symbol`](https://melpa.org/##/highlight-symbol), [`pretty-symbols`](https://github.com/drothlis/pretty-symbols), [`company`](http://company-mode.github.io), [`yasnippet`](http://joaotavora.github.io/yasnippet/), [`polymode`](https://github.com/polymode/polymode), [`smartparens`](https://github.com/Fuco1/smartparens), [SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html) and [LSP](https://github.com/emacs-lsp/lsp-mode/), to name just a few.

## Approach

The goal of this friendly fork is to produce software that any Haskell developer can use, understand and build upon ([Emacs Lisp](https://www.gnu.org/software/emacs/manual/elisp.html) is fun to learn).

This can be achieved by preferring a simple and small codebase targeting [Haskell2010](https://www.haskell.org/onlinereport/haskell2010/), with automated tests for every feature.

Old versions of `ghc` and extensions to the Haskell language may not be supported, to reduce the complexity of the codebase. For example, [literate Haskell](https://wiki.haskell.org/Literate_programming) will not be supported, and `ghc` language extensions must be justified on a per-case basis.

If it is possible to implement a feature using another minor mode, or command line tool, then we would prefer not to accept the feature.

## Issue Tracker

Bug reports and feature requests are a source of anxiety for maintainers, and encourage an unhealthy customer / supplier relationship between users and contributors.

Instead, and following the [anarchical spirit of Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf), we encourage discussions and debate around code contributions. Merge requests can be raised by anybody and discussed by anybody, and do not need to be complete. A failing automated test is the only way to report a bug. If the maintainers are convinced by the technical merit and quality of a proposal, they may accept it.

## Install

Check out the source code repository, type `cask build`, and add to your load path:

```lisp
(add-to-list 'load-path (expand-file-name "~/Projects/haskell-mode"))
(require 'haskell-mode-autoloads)
```

There are no plans to distribute any other way: this encourages contributor engagement.

## Plan

This is the status of core features:

- Navigation:
  - [x] performance-minded `syntax-table`
  - [ ] `font-lock` to visually distinguish types and values
  - [ ] `sexp` navigation (SMIE)
  - [ ] `projectile` / [`fast-tags`](https://github.com/elaforge/fast-tags) integration for `TAGS`
  - [ ] hoogle CLI jump-to-source
  - [ ] `imenu` population
- Editing:
  - [ ] indentation (SMIE)
  - [ ] `abbrev` table
  - [ ] `yasnippet` templates
  - [ ] `smartparens` compatibility (or something from stdlib)
  - [ ] `LANGUAGE` management
  - [ ] `import` management (via hoogle and [`hsimport`](https://hackage.haskell.org/package/hsimport))
- Compiling:
  - [ ] `haskell-compile` for build tool agnostic interaction with `ghc`
  - [ ] `comint-mode` based `ghc` repl

Compatibility with `lsp-mode` / [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) is important for more advanced IDE features.

## Future Plans

Some blue sky features are being considered but may be best as independent projects:

- `.cabal` editing / navigation
- [`.hie`](https://ghc.haskell.org/trac/ghc/wiki/HIEFiles) files as a parser backend and many type based queries.
- lightweight interactive commands ([`dante`](https://github.com/jyp/dante) / [`intero`](https://github.com/commercialhaskell/intero)) (or perhaps redundant with `.hie`).
  - `:type` at point
  - `:browse` `company-backend`
  - `:doc` at point
- [`flycheck`](http://www.flycheck.org/en/latest/) integration with `haskell-compile`
  - and [`hlint`](https://github.com/ndmitchell/hlint)
  - and for faster feedback, [`ghcid`](https://github.com/ndmitchell/ghcid)
- [visualise values as types](https://twitter.com/jyothsnasrin/status/1039530556080283648)
- [`djinn`](https://hackage.haskell.org/package/djinn) / [`justdoit`](https://hackage.haskell.org/package/ghc-justdoit) integration
- is there a solution to thinking "right to left" vs writing "left to right"?
- identify trivial / helper functions and forward their `edit-definition` to another location.
- Code gen
  - two spaces after an `import` converts to the missing `qualified` (or is this indentation?)
  - `instance` boilerplate
- Refactoring
  - be compatible with [`apply-refact`](https://github.com/mpickering/apply-refact) / [`hlint-refactor-mode`](https://github.com/mpickering/hlint-refactor-mode)
  - convert wildcard import to explicit list
  - insert explicit list of exports
