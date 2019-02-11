-- | Shared utility functions
module Utils where

import Prelude hiding (guard)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class
import System.IO (hPutStrLn, stderr)


unit :: Monad m => m ()
unit = return ()


(?:) :: Bool -> (a,a) -> a
(?:) b (x,y) | b = x | True = y
infixr 9 ?:
{-# INLINE (?:) #-}


ite :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ite p f h a = p a ?: (f a, h a)


guard :: MonadPlus m => Bool -> m a -> m a
guard b a = b ?: (a, mzero)


-- | Cycle through the data type
toggle :: (Eq a, Enum a, Bounded a) => a -> a
toggle = ite (== maxBound) (const minBound) succ


-- | Shorthand nabbed from xmonad
io :: MonadIO m => IO a -> m a
io = liftIO


-- | Reverse function application
(<&) :: a -> (a -> b) -> b
(<&) = flip ($)
infixr 0 <&


-- | Reverse fmap application
(^&) :: Functor f => f a -> (a -> b) -> f b
(^&) = flip fmap
infixl 4 ^&

-- | Reverse function chaining
(=>>) :: (a -> b) -> (b -> c) -> a -> c
(=>>) = flip (.)
infixr 9 =>>
