# Changelog

All notable changes to [opam-switch-mode](https://github.com/ProofGeneral/opam-switch-mode)
will be documented in this file, in reverse chronological order.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

### Fixed

### Changed

- **README.md**: Use vanilla (not GitHub-flavored) Markdown syntax for snippets
- Replace (redraw-display) with (force-mode-line-update t)
- Memoize (opam-switch-mode-lighter) to speed-up mode-line updates

### Removed

## [1.4] - 2023-07-11

### Added

- (opam-switch-mode-lighter): Indicate the switch name in the minibuffer, closes [#11](https://github.com/ProofGeneral/opam-switch-mode/issues/11)

### Fixed

### Changed

- **README.md**: Document the feature from [1.3]

### Removed

## [1.3] - 2023-07-11

### Added

- New hook: opam-switch-before-change-opam-switch-hook
- (opam-switch-set-switch): Call (run-hooks 'opam-switch-before-change-opam-switch-hook) before changing the env

### Fixed

### Changed

### Removed

## [1.2] - 2023-07-11

### Added

- **NEWS.md**: Changelog

### Fixed

- **README.md**: Markdown badges
- (opam-switch--reset-env) unexpectedly cleared exec-path, closes [#13](https://github.com/ProofGeneral/opam-switch-mode/issues/13)
- (opam-switch--get-current-switch) returned "_opam" for local switches
- (opam-switch--set-env) raised "No opam-root directory in PATH" for local switches, closes [#12](https://github.com/ProofGeneral/opam-switch-mode/issues/12)

### Changed

- Replace menu-bar's name (s/Opam-switch/OPSW/) so it matches mode-bar's name
- Change mode-bar's first element, so it expands to "OPSW - Opam Switch Mode"
- **release.sh**: auto-update **NEWS.md**

### Removed

## [1.1] - 2023-06-20

### Added

- **release.sh**: new script

### Fixed

- tweaks and cleanups by [**@monnier**](https://github.com/monnier)

### Changed

### Removed

## [1.0] - 2022-11-15

- Initial release, distributed on MELPA.

<!-- bottom -->
[Unreleased]: https://github.com/ProofGeneral/opam-switch-mode/compare/1.4...HEAD
[1.4]: https://github.com/ProofGeneral/opam-switch-mode/compare/1.3...1.4
[1.3]: https://github.com/ProofGeneral/opam-switch-mode/compare/1.2...1.3
[1.2]: https://github.com/ProofGeneral/opam-switch-mode/compare/1.1...1.2
[1.1]: https://github.com/ProofGeneral/opam-switch-mode/compare/1.0...1.1
[1.0]: https://github.com/ProofGeneral/opam-switch-mode/releases/tag/1.0