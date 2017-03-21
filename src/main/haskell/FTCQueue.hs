module FTCQueue where

-- FTCQueue m a b represents the composition of one or more functions
-- of the general shape a -> m b
type FTCQueue m a b

-- constructs a one-element sequence
tsingleton :: (a -> m b) -> FTCQueue m a b

-- adds a new element at the right edge
|> :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b

-- concatenates two sequences
|><| :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b


data ViewL m a b where
  TOne :: (a -> m b) -> ViewL m a b
  :|   :: (a -> m x) -> (FTCQueue m x b)

-- removes the element from the left edge
tviewl :: FTCQueue m a b -> ViewL m a b
