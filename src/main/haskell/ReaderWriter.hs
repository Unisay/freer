module ReaderWriter where

import Control.Applicative
import Control.Monad (liftM, ap)

data IT i o a = Pure a
              | Get (i -> IT i o a)
              | Put o (() -> IT i o a)

runRdWriter :: Monoid o => i -> IT i o a -> (a, o)
runRdWriter i m = loop mempty m
  where
    loop acc (Pure x) = (x, acc)
    loop acc (Get k) = loop acc (k i)
    loop acc (Put o k) = loop (acc `mappend` o) (k ())

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

instance Functor (IT i o) where
  fmap = liftM

instance Applicative (IT i o) where
  pure  = return
  (<*>) = ap

instance Monad (IT i o) where
  return = Pure
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >>> k)
  Put x k' >>= k = Put x (k' >>> k)
