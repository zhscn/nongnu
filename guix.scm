(use-modules (guix packages)
	     (guix build-system emacs)
	     (guix git-download)
	     ((guix licenses) #:prefix license:)
	     (gnu packages emacs-xyz))

(package
  (name "emacs-flymake-guile")
  (version "0.3")
  (source
   (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://framagit.org/flymake-backends/flymake-guile.git")
       (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "044k5rjc6bxpb1bsnlclc5n86vvj8gqkf974np9kcb3fgdrwvbqf"))))
  (build-system emacs-build-system)
  (propagated-inputs (list emacs-flymake-quickdef))
  (home-page "https://framagit.org/flymake-backends/flymake-guile")
  (synopsis "GNU Guile support for Flymake")
  (description
   "This package provides a Flymake backend for GNU Guile using @code{guild
compile}.")
  (license license:gpl3+))
