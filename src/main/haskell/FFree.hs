{-# LANGUAGE GADTs #-}

module FFree where

import Control.Monad (ap)

data FFree f a where
  Pure   :: a                       -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

data FReaderWriter i o x where
  Get ::      FReaderWriter i o i
  Put :: o -> FReaderWriter i o ()

type IT i o a = FFree (FReaderWriter i o) a

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

instance Functor (FFree f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fx k) = Impure fx ((fmap f) . k)

instance Applicative (FFree f) where
  pure  = Pure
  (<*>) = ap

instance Monad (FFree f) where
  return = Pure
  (>>=) (Pure a) k = k a
  (>>=) (Impure fx k') k = Impure fx (k' >>> k)

