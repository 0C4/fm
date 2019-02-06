
module Fm where


-- | Testing a recompile of a program from a config of a Haskell file.
-- Based off xmonad
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, fromList)
import System.Directory (getCurrentDirectory, getModificationTime)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (waitForProcess, proc,)

-- Currently, given any configuration settings, do nothing.
fm :: Config -> IO ()
fm c = do
  args <- getArgs
  if | args `elem` ["-r", "--recompile"] -> recompile True >>= putStrLn . show
     | otherwise                         -> putStrLn $ show c
 where


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


catchIOErr :: (FilePath -> IO a) -> FilePath -> IO (Maybe a)
catchIOErr act f = catch (Just <$> act f) (\(SomeException _) -> return Nothing)


catchIOErrBool :: (a -> IO Bool) -> a -> IO Bool
catchIOErrBool act s = catch (act s) (\(SomeException _) -> return False)


io :: MonadIO m => IO a -> m a
io = liftIO


unitA :: Applicative f => f ()
unitA = pure ()


unitM :: Monad m => m ()
unitM = return ()


ghc :: [String] -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
ghc = createProcess . proc "ghc"




-- | Atomically Recompile
--
-- Recompile with config file when any of the following are true:
--
--      * do-recompile is 'True'
--
--      * an executable for fm does not exist/cannot be found
--
--      * the found executable is older than fm.hs or any file in
--        the souce directory
--
--
-- 'False' is returned if there are compilation errors. 
--
recompile :: MonadIO m => Bool -> m Bool
recompile s = io $ do
  dir <- getCurrentDirectory
  let src = dir </> "fm_config.hs"
      bin = dir </> "fm"
      make = dir </> "MAKE"
  b <- waitForProcess undefined >>=
           \case
             ExitSuccess   -> return True
             ExitFailure _ -> return False
  \case
    True  -> do {- atomic replacement -}
    False -> do {- nothing/clean up   -}
  --srcMT <- getModTime src
  --binMT <- getModTime bin
