###################
Idle Highlight Mode
###################

Simple symbol highlighting package for Emacs that performs well with large files.

Usage
=====

Commands
--------

``idle-highlight-mode``
   Enable idle highlight mode for this buffer.
``global-idle-highlight-mode``
   Enable idle highlight mode for all buffers.


Customization
-------------

Global Settings
^^^^^^^^^^^^^^^

``idle-highlight``
   Face used for highlighting the symbol.
``idle-highlight-exceptions``: ``nil``
   Words to exclude from highlighting.
``idle-highlight-exceptions-face``: ``'(font-lock-keyword-face font-lock-string-face)``
   Faces to exclude from highlighting (defaults to ignore keywords & strings).
``idle-highlight-exclude-point``: ``nil``
   When non-nil, don't highlight the symbol under the cursor.
``idle-highlight-idle-time``: ``0.35``
   Delay before highlighting (in seconds).
``global-idle-highlight-ignore-modes``: ``nil``
   A list of modes that won't enable spell-checking from ``global-idle-highlight-mode``.

Buffer Local Settings
^^^^^^^^^^^^^^^^^^^^^

``global-idle-highlight-ignore-buffer``
   When not ``nil``, the buffer won't enable spell-checking from ``global-idle-highlight-mode``.

   This may also be a function that takes a single buffer argument,
   where returning ``nil`` will enable spell-checking, anything else will not.

   This example shows idle-highlight being disabled for ORG mode and for read-only buffers.

   .. code-block:: elisp

      (setq idle-highlight-ignore-modes (list 'org-mode))
      (setq global-idle-highlight-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf)))

      (global-idle-highlight-mode)


Installation
============

.. code-block:: elisp

   (use-package idle-highlight-mode
     :config (setq idle-highlight-idle-time 0.2)

     :hook ((prog-mode text-mode) . idle-highlight-mode)))

Hints
-----

You may wish to set this to a different value for each mode, e.g:

.. code-block:: elisp

   (add-hook 'after-change-major-mode-hook
     (lambda ()
       (when (derived-mode-p 'c-mode)
         (setq-local idle-highlight-exceptions '("unsigned" "signed" "long" "int" "shot" "char")))
       (when (derived-mode-p 'python-mode)
         (setq-local idle-highlight-exceptions '("list" "tuple" "int" "float" "str" "bool")))))
