
-- | Shared utility functions
--
--
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


toggle :: (Eq a, Enum a, Bounded a) => a -> a
toggle = ite (== maxBound) (const minBound) succ


-- shorthand nabbed from xmonad
io :: MonadIO m => IO a -> m a
io = liftIO


-- reverse function application
(<&) :: a -> (a -> b) -> b
(<&) x y = y x
infixr 0 <&

-- reverse fmap application
(^&) :: Functor f => f a -> (a -> b) -> f b
(^&) = flip fmap
infixl 4 ^&

-- reverse function chaining
(=>>) :: (a -> b) -> (b -> c) -> a -> c
(=>>) = flip (.)
infixr 9 =>>



-- * * * * * * * * * --
-- FOR TESTING ONLY  --
-- * * * * * * * * * --

out :: Show a => Fm a -> Fm ()
out = ask >>= err
  where
    err = io . hPutStrLn stderr . show

-- * * * * * * * * * --
--        END        --
-- * * * * * * * * * --
