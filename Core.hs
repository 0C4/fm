{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Core where

import Utils
import FileStack (FileStack(..), flatten)

import Prelude hiding (filter)
import Control.Monad (ap, liftM2)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.State (StateT(..), MonadState)
import Data.Default (Default,def)
import qualified Data.List as L (filter)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import System.Directory (getCurrentDirectory)
import System.Directory.Internal (Permissions(..))
import System.Exit (ExitCode(..))
import System.Time (ClockTime(..), getClockTime)


deriving instance Read ClockTime


type FileStack = Seq FileData


next :: a -> Seq a -> a
next = undefined


-- | (Env)ironment: the current state of the program.
data Env = Env {
  -- Updated periodically without user input
    time             :: ClockTime
  , freeDiskSpace    :: Int
  , userHomeDir      :: FilePath
  , configDir        :: FilePath
  , log              :: Map UTCTime Text
  -- Updated frequently in response to user input
  , executingLock    :: Maybe Int
  , fileStack        :: FileStack
  , parentStack      :: FileStack
  , focusDirCount    :: Int
  , focusPos         :: Int
  , focusModTime     :: ClockTime
  , focusPermissions :: Permissions
  , focusOwner       :: Text
  , focusType        :: Text
  }


modifyStack :: (FileStack -> FileStack) -> Env -> Env
modifyStack f env = let x = fileStack env
                    in env { fileStack = f x }


modifyParentStack :: (FileStack -> FileStack) -> Env -> Env
modifyParentStack f env = let x = parentStack env
                          in env { parentStack = f x }


peekFocus :: Env -> FilePath
peekFocus = focus . focusStack


flattenStack :: Env -> [FilePath]
flattenStack = flatten . fileStack


flattenParentStack :: Env -> [FilePath]
flattenParentStack = flatten . parentStack


data Sorting
  = SortNone      -- ^ No enforced sorting, return results as system provides
  | SortID        -- ^ By file id number
  | SortAZ        -- ^ By file name
  | SortDateMod   -- ^ By date modified
  | SortDateCreat -- ^ By date created
  | SortSize      -- ^ By size: # contents for dirs, #bytes for files
  | SortType      -- ^ By type, alphabetically
  deriving (Read, Show, Eq, Enum, Bounded)


data BorderStyle
  = BorderNone
  | BorderSolid
  | BorderDash
  deriving (Read, Show, Eq, Enum, Bounded)


data ViewMode
  = ViewMiller
  | ViewMC
  | ViewSingle
  deriving (Read, Show, Eq, Enum, Bounded)


data PermissionsFormat
  = PermNum
  | PermAlph
  deriving (Read, Show, Eq)


-- | Config
-- Read-only currently. Would like to be able to toggle conf on the fly
data Conf = Conf {
    cProgName            :: Text
  , cKeyBindings         :: ()
  , cColorScheme         :: ()
  , cViewMode            :: ViewMode
  , cColumnRatios        :: (Int,Int,Int)
  , cSilentCommands      :: Bool -- ^ whether user wish command result to print
  , cSaveConfigUponQuit  :: Bool
  , cSort                :: Directory -> Directory
  , cSortCaseSensitive   :: Bool
  , cSortDirsFirst       :: Bool
  , cSortReverse         :: Bool
  , cShowHidden          :: Bool
  , cPermissionsFormat   :: PermissionsFormat
  , cDisplaySpecialFiles :: Bool
  , cDisplayDiskUsage    :: Bool
  , cDisplaySize         :: Bool
  , cDisplayPermissions  :: Bool
  , cDisplayOwner        :: Bool
  , cDisplayStatusBar    :: Bool
  , cDisplayPath         :: Bool
  , cCountContents       :: Bool
  , cDrawBorders         :: BorderStyle
  , cPreviewDirectories  :: Bool
  , cPreviewTextFiles    :: Bool
  , cMaxPreviewSize      :: Int
  , cSaveLog             :: Bool -- ^ whether to save log upon exit
  , cMaxLogSize          :: Int  -- ^ maximum number of log entries
  , cLogFile             :: Maybe FilePath
  } deriving (Show, Eq, Read)



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
             deriving (Functor, Monad, MonadFail, MonadIO,
                       MonadState Env, MonadReader Conf, Typeable)


instance Applicative Fm where
  pure = return
  (<*>) = ap


instance Semigroup a => Semigroup (Fm a) where
  (<>) = liftM2 (<>)


instance (Monoid a) => Monoid (Fm a) where
  mempty  = return mempty
  mappend = liftM2 mappend


instance Default a => Default (Fm a) where
  def = return def


runFm :: Conf -> Env -> Fm a -> IO (a, Env)
runFm c s (Fm a) = runStateT (runReaderT a c) s


scrollUp :: Fm ()
scrollUp = io $ scrollUp' =<< get
  where
    scrollUp' :: Env -> IO Env
    scrollUp' = undefined
