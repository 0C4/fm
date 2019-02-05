
module Fm where


-- | Testing a recompile of a program from a config of a Haskell file.
-- Based off xmonad

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default, def)


fm :: Config -> IO ()
fm c = return ()


data Config = MkConf { myInt :: Int }


defaultConfig :: Config
defaultConfig = MkConf 0


io :: MonadIO m => IO a -> m a
io = liftIO


{-
recomile :: MonadIO m => Bool -> m Bool
recompile s = io $ do
  dir  <- getFmDir
  let binn = "xmonad-" ++ arch ++ "-" ++ os
      bin = dir </>



 where
   getFmDir :: MonadIO m => m String
   getFmDir = io $
-}

