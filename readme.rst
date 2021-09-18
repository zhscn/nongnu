####################
Idle  Highlight Mode
####################

Simple highlighting package for Emacs.

Usage
=====

Customization
-------------

``idle-highlight``
   Face used for highlighting the symbol.
``idle-highlight-exceptions``
   Words to exclude from highlighting.
``idle-highlight-idle-time``
   Delay before highlighting (in seconds).

Installation
============

.. code-block:: elisp

   (use-package idle-highlight-mode
     :config (setq idle-highlight-idle-time 0.2)

     :hook ((prog-mode text-mode) . idle-highlight-mode)))
