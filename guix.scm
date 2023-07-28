(use-modules (guix packages)
	     (guix build-system emacs)
	     (guix git-download)
	     ((guix licenses) #:prefix license:)
	     (gnu packages emacs-xyz))

(package
  (name "emacs-flymake-guile")
  (version "0.2")
  (source
   (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://framagit.org/flymake-backends/flymake-guile")
       (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0p96kbsh50ypjfzgxik6zj7lacas8az2s9fcmcraxd0b35y9bwbc"))))
  (build-system emacs-build-system)
  (propagated-inputs (list emacs-flymake-quickdef))
  (home-page "https://framagit.org/flymake-backends/flymake-guile")
  (synopsis "GNU Guile support for Flymake")
  (description
   "This package provides a Flymake backend for GNU Guile using @code{guild
compile}.")
  (license license:gpl3+))
