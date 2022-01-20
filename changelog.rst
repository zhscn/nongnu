- In development (2022-01-20)
  - Fix #2 use symbol instead of word bounds when scanning items to highlight.
  - Fix #1 font face detection when overlays were in use (such as ``hl-line-mode``).
  - Fix highlighting with multiple windows sharing one buffer.
  - Add ``idle-highlight-visible-buffers`` to support highlighting all buffers with the current symbol.
  - Add ``idle-highlight-exceptions-syntax`` so the characters used in the syntax-table used can be customized.
  - Support setting ``idle-highlight-exceptions`` to a function that takes the word as an argument.
  - Support setting ``idle-highlight-exceptions-face`` to a function that takes list of faces as an argument.
  - Add ``idle-highlight-exclude-point`` option to exclude the current word from highlighting.
  - Add ``idle-highlight-exceptions-face`` to support excluding words by face.
  - Add ``global-idle-highlight-mode`` (globalized minor mode).
  - Disable the repeating idle timer when the minor mode isn't active.

- Version 1.1.3 (2012-08-21)

  - Add configurations for excepted words and what idle time to use.

- Version 1.1.2 (2011-08-18)

  - Highlight only when point is on a symbol character.
  - Remove previous highlights if there's nothing to highlight at point.

- Version 1.1.1 (2011-05-29)

  - Implement idle highlighting as a minor mode.

- Version 1.0 (2011-05-29)

  Initial commit.
