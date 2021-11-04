
##############
Scroll on Drag
##############

This package exposes ``scroll-on-drag`` where you can click and drag up/down to scroll
at increasing speeds based on the drag distance.

Available via `melpa <https://melpa.org/#/scroll-on-drag>`__.


Motivation
==========

Having an interactive scroll action that runs a highly varied speeds,
either a few lines, or halfway down a large file.

*Note that this is similar to auto-scroll in Firefox.*


Features
========

Smooth Scroll
   Especially useful when scrolling slowly
   *(snaps to the closest line on completion).*
Non-Linear Speed
   Larger cursor motion increases scroll speed increasingly
   allowing a drag motion to scroll down the entire document, or only a few lines.
   See ``cursor-on-drag-motion-accelerate``.
Cancel Support
   You can cancel the scroll action for peeking at other parts of the file.
Un-intrusive
   Unlike some minor modes that adjust the behavior of scrolling,
   this can be bound to a key and won't impact scrolling in general.


Usage
=====

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
=============

While the defaults seem to work well, these values can be customized.

``scroll-on-drag-smooth``: t
   Smooth (pixel) scroll *(snapped to line on completion).*
``scroll-on-drag-clamp``: nil
   Prevent scrolling past the end of the buffer.
``scroll-on-drag-delay``: 0.01, typically in range [0.005 .. 0.1]
   Time between scroll updates.
``scroll-on-drag-motion-scale``: 0.25, typically in range [0.01 .. 1.0]
   Scale cursor motion, to make scrolling easier to control.
``scroll-on-drag-motion-accelerate``: 0.3, typically in range [0.0 .. 1.0]
   Values greater than 0.0 apply non-linear scaling,
   this gives control when scrolling individual lines while allowing much
   greater speed without having to move the mouse a long distance.
``scroll-on-drag-follow-mouse``
   When non-nil, scroll the window under the mouse cursor (even when it's not active)
   returning focus on completion.
