-- | Idealised indentation scenarios.
--
--   Bugs and unexpected behaviour in (re-)indentation may be documented here.
--
--   Lines marked "manual correction" indicate where we expect the user to
--   re-indent because it goes against our prediction. In some of these cases,
--   we could improve the guess with semantic information (e.g. if we know that
--   the RHS of a bind is only partially applied, then we probably mean to
--   continue that line instead of start a new one).
module Indentation where

import Foo.Bar
import Foo.Baz hiding ( gaz,
                        baz
                      )

basic_do = do
  foo <- blah blah blah
  bar <- blah blah
         blah -- manual correction
         blah -- manual correction
  sideeffect
  sideeffect' blah
  let baz = blah blah
            blah -- manual correction
      gaz = blah
      haz =
        blah
  pure faz -- manual correction

nested_do = -- manual correction
  do foo <- blah
     do bar <- blah -- same level as foo
        baz -- same level as bar

nested_where a b = foo a b
  where -- manual correction
    foo = bar baz -- indented
    baz = blah blah -- same level as foo
      where -- manual correction
        gaz a = blah -- indented
        faz = blah -- same level as gaz

let_in a b = let
  blah = bloo
  wobble _ = fish
  in
    flibble blah

implicit_let foo bar =
  let ?foo = foo
      ?bar = bar
  in  rar

case_of wibble = case wibble of
  Nothing   ->
    ""
  Just fish ->
    fish

lambda_case = \case
  Nothing   -> ""
  Just fish -> fish

dollars f Nothing = f $
  "" ""
  ""
dollars f (Just a) = f $ \s ->
  a

not_dollars = do
  db' <- liftIO $ readMVar db
  shouldGoHere <$>
    here

data Wibble = Wibble Int
            | Wobble Int
            | Vibble Int

lists1 = [ foo
         , bar
         , [ blah
           , blah
           , blah ]
         ]

lists2 = [
  foo
, bar
]

lists3 = [ foo ,
           bar ]

tuples1 = ( foo
          , bar
          , ( blah
            , blah
            , blah )
          )

tuples2 = (
  foo
, bar
)

tuples3 = ( foo ,
            bar )
