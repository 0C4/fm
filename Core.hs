{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

--module Core where

import Utils

import Brick
import Brick.BChan

import Prelude hiding (filter)
import Control.Lens ((^.), (.~), (%~))
import Control.Lens.TH (makeLenses)
import Control.Monad (ap, liftM2)
import Control.Monad.Base
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (modify)
import Data.Default
import Data.Map
import qualified Data.List as L (filter)
import Data.Time
import Data.Typeable (Typeable)
import System.Directory
import System.Directory.Internal (Permissions(..))
import System.Exit (ExitCode(..))
import System.IO




type KeyMask = String
type KeySym = String


-- | (Env)ironment: the current state of the program.
data Env = Env {

  -- Updated periodically without user input
    _time             :: UTCTime
  , _freeDiskSpace    :: Int
  , _userHomeDir      :: FilePath

  -- Updated frequently in response to user input
  , _executingLock    :: Maybe Int
  , _focus            :: FilePath
  , _fileStack        :: ()--filestack
  , _parentStack      :: ()--filestack
  , _focusDirCount    :: Int
  , _focusPos         :: Int
  , _focusModTime     :: Int --ClockTime
  , _focusPermissions :: () {-Permissions-}
  , _focusOwner       :: String
  , _focusType        :: String
  }


defaultEnv :: Env
defaultEnv = Env {
    _time = 0
  , _freeDiskSpace = 0
  , _userHomeDir = ""
  , _executingLock = Nothing
  , _focus = ""
  , _fileStack = ()
  , _parentStack = ()
  , _focusDirCount = 0
  , _focusPos = 0
  , _focusModTime = 0
  , _focusPermissions = ()
  , _focusOwner = ""
  , _focusType = ""
  }


data DisplayMode = MillerColumns | MidnightCommander | SingleColumn
                 deriving (Read, Show, Eq, Enum, Bounded)


data PermissionsFormat = Symbolic | Octal
                       deriving (Read, Show, Eq)


-- | Config, read-only
data Conf = Conf {

  -- hooks
    _logHook     :: !(Fm ()) -- ^ The action to perform when logging
  , _startupHook :: !(Fm ()) -- ^ The action to perform on startup
  , _exitHook    :: !(Fm ()) -- ^ Action to perform upon regular exit
  , _keys                :: !(Map (KeyMask, KeySym) (Fm ()))
                        -- ^ a mapping of key presses to actions
  -- display
  , _progName           :: String
  , _colorScheme        :: ()
  , _displayMode        :: DisplayMode
  , _columnRatios       :: (Int,Int,Int)
  , _silentCommands     :: Bool -- ^ whether user wish command result to print
  , _saveConfigUponQuit :: Bool
  , _sort               :: Bool
  , _sortCaseSensitive  :: Bool
  , _sortDirsFirst      :: Bool
  , _sortReverse        :: Bool
  , _showHidden         :: Bool
  , _permissionsFormat  :: PermissionsFormat
  , _displayDiskUsage   :: Bool
  , _displaySize        :: Bool
  , _displayPermissions :: Bool
  , _displayOwner       :: Bool
  , _displayStatusBar   :: Bool
  , _displayPath        :: Bool

  -- previewing
  , _countContents       :: Bool
  , _previewDirectories  :: Bool
  , _previewTextFiles    :: Bool
  , _maxPreviewSize      :: Int

  -- logging
  , _saveLog             :: Maybe FilePath -- ^ save to a log
  }


defaultConf :: Conf
defaultConf = Conf {
    _logHook = unit
  , _startupHook = unit
  , _exitHook  = unit
  , _keys = empty
  , _progName = ""
  , _colorScheme = ()
  , _displayMode = MillerColumns
  , _columnRatios = (1,3,4)
  , _silentCommands = True
  , _saveConfigUponQuit = True
  , _sort = True
  , _sortCaseSensitive = True
  , _sortDirsFirst = True
  , _sortReverse = True
  , _showHidden = True
  , _permissionsFormat = Symbolic
  , _displayDiskUsage = True
  , _displaySize = True
  , _displayPermissions = True
  , _displayOwner = True
  , _displayStatusBar = True
  , _displayPath = True
  , _countContents = True
  , _previewDirectories = True
  , _previewTextFiles = True
  , _maxPreviewSize = 0
  , _saveLog = Nothing
  }



-- | The Fm Monad
--
-- Note: although transforming over IO (so in reality the state and conf
-- could be merged), they are seperated since conf will only be occationally
-- modified while the state (Env) is regularly updated. Think: file in focus
-- vs key bindings.
--
-- Note: Writter monad (for logging) is mimicked with use of State
--
newtype Fm a = Fm (ReaderT Conf (StateT Env IO) a)
             deriving (Functor, Applicative, Monad, MonadIO,
                       MonadState Env, MonadReader Conf, MonadBase IO)


instance Default a => Default (Fm a) where
  def = return def



modify :: (MonadBase b m, MonadState s m) => (s -> b s) -> m ()
modify f = get >>= f =>> liftBase >>= put




-- * * * * * * * * * --
-- FOR TESTING ONLY  --
-- * * * * * * * * * --

makeLenses ''Env
makeLenses ''Conf

instance Show Conf where
  show Conf{..} = unlines [ "Display Mode: " ++ show _displayMode
                          , "Program Name: " ++ _progName
                          , "Max File Size for previewing: " ++ show _maxPreviewSize
                          , "Save Log: " ++ show _saveLog
                          ]

runFm :: Conf -> Env -> Fm a -> IO (a, Env)
runFm c s (Fm a) = runStateT (runReaderT a c) s

out :: (Show r, MonadIO m, MonadReader r m) => m ()
out = ask >>= err

err :: (MonadIO m, Show a) => a -> m ()
err = io . hPrint stderr

ui :: Fm (Widget ())
ui = do
  s <- get
  c <- io $ listDirectory $ focus s
  let msg = unlines c
  return $ str msg

changeDir :: Fm ()
changeDir = modify (\env -> do d <- getCurrentDirectory
                               let e = focus .~ env d
                               return e
                   )

newtype Event = EventUpdateTime Tm.LocalTime

main :: IO ()
main = do
  chan <- newBChan 5
  void . forkIO $ forever $ do
    t <- getTime
    BCh.writeBChan chan $ EventUpdateTime t
    threadDelay $ 1 * 1000000
  t <- getTime

  (x,s) <- runFm defaultConf defaultEnv (changeDir *> ui)

  let st = s { _time = t }
      u = hBox borderDemos
        <=> B.hBorder
        <=> colorDemo

  simpleMain x

-- | Builds screen widget.
visualize :: Fm (Widget ())
visualize = do
  s <- get
  r <- ask
  let format = show $ r ^. format
  --
  unit

-- * * * * * * * * * --
--        END        --
-- * * * * * * * * * --
