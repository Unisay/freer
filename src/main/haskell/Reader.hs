module Reader where

import Control.Applicative
import Control.Monad (liftM, ap)

data It i a = Pure a
            | Get (i -> It i a)

ask :: It i i
ask = Get Pure

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

addGet :: Int -> It Int Int
addGet x = ask >>= \i -> return (i + x)

addN :: Int -> It Int Int
addN n = f 0 where
  f :: Int -> It Int Int
  f = foldl (>>>) return (replicate n addGet)

runReader :: i -> It i a -> a
runReader _ (Pure v) = v
runReader x (Get k) = runReader x (k x)

feedAll :: [i] -> It i a -> a
feedAll _ (Pure v) = v
feedAll [] _ = error "end of stream"
feedAll (h : t) (Get k) = feedAll t (k h)

instance (Show i, Show a) => Show (It i a) where
  -- show :: a -> String
  show (Pure v) = "Pure(" ++ (show v) ++ ")"
  show (Get k) = "Get(i -> It i a)"

instance Functor (It i) where
  fmap = liftM

instance Applicative (It i) where
  pure  = return
  (<*>) = ap

instance Monad (It i) where
  -- return :: a -> It i a
  return = Pure
  -- (>>=) :: It i a -> (a -> It i b) -> It i b
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >>> k)
