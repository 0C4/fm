
module Utils where

import Prelude hiding (guard)
import Control.Monad (MonadPlus, mzero)
ite :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ite p f h a = if p a then f a else h a


guard :: MonadPlus m => Bool -> m a -> m a
guard b a | b = a | True = mzero


toggle :: (Eq a, Enum a, Bounded a) => a -> a
toggle = ite (== maxBound) (const minBound) succ


