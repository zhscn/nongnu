(use-modules (guix packages)
	     (guix build-system emacs)
	     (guix git-download)
	     ((guix licenses) #:prefix license:)
	     (gnu packages emacs-xyz))

(package
  (name "emacs-flymake-guile")
  (version "0.4")
  (source
   (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://framagit.org/flymake-backends/flymake-guile.git")
       (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "06x237qhvnbyxngbqinjg417n341h652jkagr1a5whximlsaw2c8"))))
  (build-system emacs-build-system)
  (propagated-inputs (list emacs-flymake-quickdef))
  (home-page "https://framagit.org/flymake-backends/flymake-guile")
  (synopsis "GNU Guile support for Flymake")
  (description
   "This package provides a Flymake backend for GNU Guile using @code{guild
compile}.")
  (license license:gpl3+))
