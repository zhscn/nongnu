-- | Idealised indentation scenarios.
--
--   Bugs and unexpected behaviour in (re-)indentation may be documented here.
module Indentation where

-- A basic `do` block using virtual indentation to suggest the whitespace
basic_do = do
  -- TODO do should have virtual indentation of 0, so this is at 2
  foo = blah blah blah
  -- TODO should suggest that bar is a binding
  bar = blah blah
        blah -- manual continuation, should be 1st alt TODO
        blah -- continue what we were doing, should be the SMIE rule

-- TODO `do` with manual layout
-- TODO nested `do`


-- TODO coproduct definitions, the | should align with =

-- TODO lists
