####################
Idle  Highlight Mode
####################

Simple highlighting package for Emacs.

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
``idle-highlight-exceptions``
   Words to exclude from highlighting.

   You may wish to set this to a different value for each mode, e.g:

   .. code-block:: elisp

      (setq-local idle-highlight-exceptions '("end" "begin"))

``idle-highlight-idle-time``
   Delay before highlighting (in seconds).
``global-idle-highlight-ignore-modes`` nil
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
