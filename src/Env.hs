{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Env where

import Data.Map (Map, empty)
import Data.Text (Text)
import Data.Time(UTCTime)
import System.Directory.Internal (Permissions(..))
import System.Exit (ExitCode(..))
import System.Time (ClockTime(..))


data Sorting
  = Undefined   -- ^ No enforced sorting, return results as system provides
  | SortID      -- ^ Arrange based on file id number
  | SortAZ      -- ^ Arrange by file name
  | SortDateMod -- ^ Arrange by date modified
  | SortDateCreat -- ^ Arrange by date created
  | SortSize    -- ^ Arrange by size (by # subdirs for dirs, #bytes for files)
  | SortType    -- ^ Sort with similar types together [dirs, PDFs, TXTs, ect.]
  deriving (Read, Show, Eq, Enum, Bounded)


data BorderStyle
  = None
  | Solid
  | Dash
  deriving (Read, Show, Eq, Enum, Bounded)


data ViewMode
  = Miller
  | MC
  | Single
  deriving (Read, Show, Eq, Enum, Bounded)


data PermissionsFormat
  =  Numerical
  | Traditional deriving (Read, Show, Eq)


deriving instance Read ClockTime


-- | Logging Config: <Url about spaceleaks with writer monad>
data Env = Env {

    eClockTime     :: ClockTime
  , eFreeDiskSpace :: Int       -- ^ remaining disk space
  , eLoginName     :: String    -- ^ user name
  , eExitOnError   :: Bool      -- ^ should we exit immediately on any error
  , eExitCode      :: ExitCode  -- ^ incase of critical program crash/issue
  , eUserHomeDir   :: FilePath  -- ^ localtion of home directory
  , eConfigDir     :: FilePath  -- ^ localtion of config file

-- | Focus State: updated frequently in response to user input
  , sFocus                 :: FilePath
  , sFocusDir              :: FilePath
  , sFocusDirCount         :: Int
  , sFocusPos              :: Int
  , sFocusModificationTime :: ClockTime
  , sFocusPermissions      :: Permissions
  , sFocusOwner            :: Text
  , sFileTypeClassify      :: Text

-- | Config: loaded from config + can be changed on the fly
  , cKeyBindings        :: () -- placeholder for now
  , cColorScheme        :: () -- placeholder for now
  , cViewMode           :: ViewMode
  , cColumnRatios       :: (Int,Int,Int)
  , cSilentCommands     :: Bool -- ^ whether user wish command results to print
  , cSaveConfigUponQuit :: Bool
  , cSort               :: Sorting
  , cSortCaseSensitive  :: Bool
  , cSortDirsFirst      :: Bool
  , cSortReverse        :: Bool
  , cShowHidden         :: Bool
  , cDisplaySpecialEntries :: Bool
  , cDisplayDiskUsage   :: Bool
  , cDisplaySize        :: Bool
  , cDisplayPermissions :: Bool
  , cDisplayOwner       :: Bool
  , cDisplayStatusBar   :: Bool
  , cDisplayPath        :: Bool
  , cCountContents      :: Bool
  , cDrawBorders        :: BorderStyle
  , cPreviewDirectories :: Bool
  , cPreviewTextFiles   :: Bool
  , cMaxPreviewSize     :: Int
  , cSaveLog            :: Bool -- ^ whether to save log upon exit
  , cMaxLogSize         :: Int  -- ^ maximum number of log entries
  , cLogFile            :: Maybe FilePath -- ^ location to write output to (appended)

-- | Log: <Url about spaceleaks with writer monad>
  , lLog   :: Map UTCTime Text

  } deriving (Show, Eq, Read)


defaultEnv :: Env
defaultEnv = Env {
  eClockTime = TOD 0 0,
  eFreeDiskSpace = 0,
  eLoginName = "",
  eExitOnError = True,
  eExitCode = ExitSuccess,
  eUserHomeDir = "",
  eConfigDir = "",
  sFocus = "",
  sFocusDir = "",
  sFocusDirCount = 0,
  sFocusPos = 0,
  sFocusModificationTime = TOD 0 0,
  sFocusPermissions = Permissions True True False True,
  sFocusOwner = "",
  sFileTypeClassify = "",
  cKeyBindings = (),
  cColorScheme = (),
  cViewMode = Miller,
  cColumnRatios = (1,3,4),
  cSilentCommands = False,
  cSaveConfigUponQuit = True,
  cSort = SortAZ,
  cSortCaseSensitive = False,
  cSortDirsFirst = False,
  cSortReverse = False,
  cShowHidden = True,
  cDisplaySpecialEntries = False,
  cDisplayDiskUsage = True,
  cDisplaySize = True,
  cDisplayPermissions = True,
  cDisplayOwner = True,
  cDisplayStatusBar = True,
  cDisplayPath = True,
  cCountContents = True,
  cDrawBorders = None,
  cPreviewDirectories = True,
  cPreviewTextFiles = True,
  cMaxPreviewSize = 50,
  cSaveLog = True,
  cMaxLogSize = 100,
  lLog = empty,
  cLogFile = Nothing
  }
