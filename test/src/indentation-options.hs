-- | Indentation scenarios to test indentation options.
module Indentation where

types4 ::
   ( Monad m )
  => a
  -> b
  -> c

types5 :: Monad m
       => (?log :: HasLogger m)
       => a
       -> b
       -> c

