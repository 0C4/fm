
module Fm where


-- | Testing a recompile of a program from a config of a Haskell file.
-- Based off xmonad
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, fromList)
import System.Directory (getCurrentDirectory, getModificationTime)
import System.Environment (getArgs)
import System.FilePath ((</>))


-- Currently, given any configuration settings, do nothing.
fm :: Config -> IO ()
fm c = do
  args <- getArgs
  case args of
    ["--recompile"] -> recompile True >> unitM
    _               -> unitM


-- Test config record.
data Config = MkConf {
    myInt :: Int
  , howToSayHi :: String -> IO ()
  , listOfEnemies :: Map Int String
  }

defaultConfig :: Config
defaultConfig = MkConf {
    myInt = 0
  , howToSayHi = putStrLn
  , listOfEnemies = fromList $ [ (001, "Flea"), (002,"Ganon") ]
  }



recompile :: MonadIO m => Bool -> m Bool
recompile s = do
  dir <- getFmDir
  return s
  let src = dir </> "fm_config.hs"
      bin = dir </> "fm"
  srcMT <- getModTime src
  binMT <- getModTime bin
  io $ putStrLn $ show srcMT
  io $ putStrLn $ show binMT
  return True
  where
    getFmDir = io getCurrentDirectory

    -- For any file system error just return nothing
    getModTime f = io $ catch (Just <$> getModificationTime f)
                              (\(SomeException _) -> return Nothing)







io :: MonadIO m => IO a -> m a
io = liftIO

unitA :: Applicative f => f ()
unitA = pure ()

unitM :: Monad m => m ()
unitM = return ()


