Scroll on Drag
==============


Motivation
----------

Without this, there was no quick way to scroll at highly varied speeds,
either a few lines, or halfway down a large file.

This package exposes ``scroll-on-drag`` where you can click and drag up/down to scroll
at increasing speeds based on the drag distance.

*Note that this is similar to auto-scroll in Firefox.*


Usage
-----

- This is not a mode, instead it is a function you can bind to a key
  (typically middle mouse) which scrolls until the button is released.
- Pressing Escape cancels, restoring the previous scroll point.
- Pressing Space keeps the current scroll position, restarting the scroll action.
- Returns ``t`` when scrolled, otherwise ``nil``, allowing click events to be passed through.

  You can use the middle mouse button paste (or any other action)
  if no scrolling is performed.

  .. code-block:: elisp

     (global-set-key
       [down-mouse-2]
       (lambda ()
         (interactive)
         (unless (scroll-on-drag)
           (mouse-yank-primary t))))

  Or when using evil mode.

  .. code-block:: elisp

     (define-key evil-normal-state-map [down-mouse-2]
       (lambda ()
         (interactive)
         (unless (scroll-on-drag)
           (mouse-yank-primary t))))



Customization
-------------

While the defaults seem to work well, these values can be customized.

``scroll-on-drag-motion-style``: scroll-with-cursor.
   :scroll-with-cursor: Scroll the window and move the cursor.
   :scroll: Scroll the window (let emacs constrain the cursor).
   :cursor: Only move the cursor.
``scroll-on-drag-delay``: 0.01, typically in range [0.005 .. 0.1]
   Time between scroll updates.
``scroll-on-drag-motion-scale``: 0.1, typically in range [0.01 .. 1.0]
   Scale cursor motion,
   measured in pixels to make scrolling easier to control.
``scroll-on-drag-motion-power``: 2, typically in range [1.0 .. 4.0]
   Values greater than 1.0 apply non-linear scaling,
   this gives control when scrolling individual lines while allowing much
   greater speed without having to move the mouse a long distance.


TODO
----

- Pixel based scrolling

  While this is implemented,
  there seems to be a problem that causes jitter when scrolling up.
