
module Utils where

import Prelude hiding (guard, either)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (withReaderT)

ite :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ite p f h a = if p a then f a else h a


guard :: MonadPlus m => Bool -> m a -> m a
guard b a | b = a | True = mzero


toggle :: (Eq a, Enum a, Bounded a) => a -> a
toggle = ite (== maxBound) (const minBound) succ


-- a shorthand nabbed from xmonad
io :: MonadIO m => IO a -> m a
io = liftIO


-- reverse function application
(<&) :: a -> (a -> b) -> b
(<&) = flip ($)
infixr 0 <&

-- reverse fmap application
(^&) :: Functor f => f a -> (a -> b) -> f b
(^&) = flip fmap
infixl 4 ^&
