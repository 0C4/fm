{-# LANGUAGE
    MultiWayIf,
    LambdaCase
#-}

module Fm where


-- | Testing a recompile of a program from a config of a Haskell file.
-- Based off xmonad
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, fromList)
import System.Directory (getCurrentDirectory, getModificationTime, renameFile,
                         removeFile)
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>),(<.>))
import System.Process
import GHC.IO.Handle

-- Currently, given any configuration settings, do nothing.
fm :: Config -> IO ()
fm c = do
  args <- getArgs
  case args of
    ["--recompile"] -> recompile True >>= putStrLn . show
    _               -> putStrLn $ show c




-- Test config record.
data Config = MkConf {
    myInt :: Int
  , howToSayHi :: String -> String
  , listOfEnemies :: Map Int String
  }

instance Show Config where
  show c = show (myInt c) ++ (unlines $ foldl (\x y-> x ++ [h y]) [] (listOfEnemies c))
        where
          h x = (howToSayHi c) x


defaultConfig :: Config
defaultConfig = MkConf {
    myInt = 0
  , howToSayHi = id
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
      temp = bin <.> "recompiling"
      --cleanUp = mapM_ (catchIOErr removeFile) [src <.> "hi", src <.> "o"]
  e <- waitForProcess =<< do (_,_,_,h)<- ghc ["-o",temp,src]
                             return h
  --
  case e of
     ExitSuccess   -> do
       renameFile temp bin
       cleanUp
       return True
     ExitFailure n -> do
       cleanUp
       return False

 where
  --srcMT <- getModTime src
  --binMT <- getModTime bin
