(use-modules (guix packages)
	     (guix git)
	     (guix build-system emacs)
	     ((guix licenses) #:prefix license:))

(package
  (name "emacs-flymake-guile")
  (version "0.5")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system emacs-build-system)
  (home-page "https://framagit.org/flymake-backends/flymake-guile")
  (synopsis "GNU Guile support for Flymake")
  (description
   "This package provides a Flymake backend for GNU Guile using @code{guild
compile}.")
  (license license:gpl3+))
