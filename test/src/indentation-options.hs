-- | Indentation scenarios to test indentation options.
module Indentation where

typesig :: a -> b -> c

types1 ::
     Monad m
  => a
  -> b
  -> c

types2 ::
   ( Monad m )
  => a
  -> b
  -> c

types3 :: (Monad m, MemberLogger m)
  => a
  -> b
  -> c

types4 :: Monad m
       => (?log :: HasLogger m)
       => a
       -> b
       -> c

types5 :: (Monad m, MonadReader Foo m) =>
          (?log :: HasLogger m) =>
          a -> b -> c
