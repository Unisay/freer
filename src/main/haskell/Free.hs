module Free where

import Control.Applicative
import Control.Monad (liftM, ap)

data Free f a = Pure a
              | Impure (f (Free f a))

data ReaderWriter i o x = Get (i -> x)
                        | Put o (() -> x)

type IT i o a = Free (ReaderWriter i o) a
type FFRee g  = Free (Lan g)

test :: IT Int Int Int
test =
  Impure (Get (\i ->
    Impure (Put (i + 1) (\_ ->
      Pure (i + 1)))))

instance Functor (ReaderWriter i o) where
 fmap f (Get g) = Get (f . g)
 fmap f (Put o' g) = Put o' (f . g)

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure  = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= k = k a
  Impure f >>= k = Impure (fmap (>>= k) f)


data Lan g a where
  FMap :: (x -> a) -> g x -> Lan g a

instance Functor (Lan g) where
  fmap h (FMap h' gx) = FMap (h . h') gx


