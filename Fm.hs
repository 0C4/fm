{-# LANGUAGE LambdaCase #-}

module Fm where

import Core

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map, fromList)
import System.Directory (getCurrentDirectory, getModificationTime, renameFile, removeFile)
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>),(<.>))
import System.Process


-- Currently, given any configuration setting, show the config and do nothing.
fm :: Conf -> IO ()
fm c = launch $ build (def {config = c }) =<< getArgs


launch :: LaunchParams -> IO ()
launch l = unitM


build :: LaunchParams -> [String] -> IO ()
build parms args = case args of

  -- ^ Either no arguments were passed, or args were exhausted
  -- Thus launch the program
  []              -> launch parms


  -- ^ Print help information
  ("--help":_)       -> help

  ("--recompile":rs) -> recompile True >>= putStrLn . show


  -- ^ Specify a config file to recompile with.
  (("--recompile-with=":fp):rs) -> undefined


  -- ^ Don't touch/require any config files.
  -- Same as passing "recompile-with" without an arg.
  -- Overrides --recompile
  ("--clean":rs) -> deffined


  -- ^ Print statistics of CPU usage on exit.
  ("--profile":rs) -> undefined


  -- ^
  (x:xs)           -> do
    hPutStrLn stderr "Fm: unreadable argument of \"" ++ x ++ "\". Attempting to continue"





data LaunchParams = LaunchParams {
    config     :: Config
  , profile    :: Bool
  , configPath :: Maybe FilePath
  , clean      :: Bool
  }



defaultLaunchParams = LaunchParams {
    profile = False
  , configPath = Nothing
  , clean = False
  }








-- Test config record.
data Config = MkConf {
    myInt :: Int
  , howToSayHi :: String -> String
  , listOfEnemies :: Map Int String
  }

instance Show Config where
  show c = unlines
              [ show (myInt c)
              , unlines $ foldl (\x y-> x ++ [howToSayHi c $ y]) [] (listOfEnemies c)
              ]


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






-- | (atomic) recompile
--
-- Currently recompiles, but is very error prone (read: hacky)
-- Atomically replace only if successful in recompile
--
-- TODO: handle possible custom make scrips
-- TODO: only recompile if source has been updated (? hash instead ot stamp?)
-- TODO: handle source being in possibly different dir than config file
-- TODO: more complex feed about (ie at traces)
-- TODO: add prog lauch if successful recompile and atomic replacement?
-- TODO: include following spec
--           """Recompile with config file when any of the following are true:
--                 * do-recompile is 'True'
--                 * an executable for fm does not exist/cannot be found
--                 * the found executable is older than fm.hs or any file in
--                      the source directory
--          """
--
-- "--make" https://downloads.haskell.org/~ghc/5.02/docs/set/make-mode.html
recompile :: MonadIO m => Bool -> m Bool
recompile s = io $ do
  dir <- getCurrentDirectory
  let src  = dir </> "fm_config.hs"
      bin  = dir </> "fm"
      temp = bin <.> "recompiling"
  e <- waitForProcess =<< runProcess "ghc"
                                     ["--make","-Wall","-fforce-recomp","-o",temp,src]
                                     (Just dir)  Nothing Nothing Nothing Nothing
  case e of
     ExitSuccess   -> do
       renameFile temp bin
       return True
     ExitFailure _ -> do
       return False

  -- Old/currently unused but kept for future use
  --cleanUp = mapM_ (catchIOErr removeFile) [src <.> "hi", src <.> "o"]
  --srcMT <- getModTime src
  --binMT <- getModTime bin










{--copy file
main :: IO ()
main = printCode =<< cp =<< getArgs

printCode :: ExitCode -> IO ()
printCode  ExitSuccess    = putStderr "File successfully copied"
printCode (ExitFailure n) = putStderr $ "Failed to copy file with exit code: " ++ show n
cp :: [String] -> IO ExitCode
cp = waitForProcess <=< forth <=< createProcess . shell .  ("cp " ++) . unwords

-- to go into utils file
putStderr = {- liftIO . -} hPutStrLn stderr
forth (_,_,_,x) = return x
--}
