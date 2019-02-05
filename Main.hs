main :: IO ()
main = return ()


-- | Testing a recompile of a program from a config of a Haskell file.
-- Based off xmonad-0.15's implimentation, significantly trimmed down.

import Control.Monad.IO.Class (MonadIO, liftIO)





getFmDataDir = undefined


recomile :: MonadIO m => Bool -> m Bool
recompile s = io $ do
  dir  <- getFmDir
  let binn = "xmonad-" ++ arch ++ "-" ++ os
      bin = dir </>



 where
   getFmDir :: MonadIO m => m String
   getFmDir = io $





---
io = liftIO
