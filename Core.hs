{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Core where

import Utils
import FileStack (FileStack(..), flatten)

import Prelude hiding (filter)
import Control.Monad (ap, liftM2)
import Control.Monad.Base
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Strict (modify) {- MonadState s m => (s -> s) -> m ()-}
import Data.Default
import qualified Data.List as L (filter)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import System.Directory (getCurrentDirectory)
import System.Directory.Internal (Permissions(..)
import System.Exit (ExitCode(..))
import System.Time (ClockTime(..), getClockTime)



-- | For our enumerated data types, the default is always the first data constructor.
instance Bounded a => Default a where
  def = minBound


type FileStack = Seq FileData


type Folder l = Folder {
   }


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


flattenparentstack :: Env -> [FilePath]
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


readSorting :: Sorting -> (Folder l -> Folder l)
readSorting = undefined


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


-- | Config, read-only
data Conf = Conf
  -- hooks
  { logHook            :: !(Fm ()) -- ^ The action to perform when logging
  , startupHook        :: !(Fm ()) -- ^ The action to perform on startup
  , exitHook           :: !(Fm ()) -- ^ Action to perform upon regular exit


  -- key bindings
  , keys         :: !(Map (KeyMask, KeySym) (Fm ()))
                    -- ^ a mapping of key presses to actions

  -- display
  , progName            :: Text
  , colorScheme         :: ()
  , biewMode            :: ViewMode
  , columnRatios        :: (Int,Int,Int)
  , silentCommands      :: Bool -- ^ whether user wish command result to print
  , saveConfigUponQuit  :: Bool
  , sort                :: Folder l -> Folder l
  , sortCaseSensitive   :: Bool
  , sortDirsFirst       :: Bool
  , sortReverse         :: Bool
  , showHidden          :: Bool
  , permissionsFormat   :: PermissionsFormat
  , displayDiskUsage    :: Bool
  , displaySize         :: Bool
  , displayPermissions  :: Bool
  , displayOwner        :: Bool
  , displayStatusBar    :: Bool
  , displayPath         :: Bool
  , drawBorders         :: BorderStyle

  -- previewing
  , countContents       :: Bool
  , previewDirectories  :: Bool
  , previewTextFiles    :: Bool
  , maxPreviewSize      :: Int

  -- logging
  , saveLog             :: Maybe FilePath -- ^ save to a log
  , maxLogSize          :: Int  -- ^ maximum number of log entries
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
             deriving (Functor, Applicative, Monad, MonadFail, MonadIO,
                       MonadState Env, MonadReader Conf, MonadBase IO, Typeable)


instance Default a => Default (Fm a) where
  def = return def


runFm :: Conf -> Env -> Fm a -> IO (a, Env)
runFm c s (Fm a) = runStateT (runReaderT a c) s


putBase :: MonadBase b m, MonadState s m => (s -> b s) -> m a
putBase f = get >>= f =>> liftBase >>= put






-- | FOR TESTING ONLY
--
writeOutConfigData :: MonadIO m => Fm a -> Fm ()
writeOutConfigData c = do
  a <- ask
  hPutStrLn stder





{-

-- | FileStack
-- Holds the list of directory contents
data FileStack = FileStack {
    focus :: FilePath
  , up    :: [FilePath]
  , down  :: [FilePath]
  } deriving (Read, Show, Eq)


emptyStack :: FileStack
emptyStack = FileStack "" [] []


filter :: (FilePath -> Bool) -> FileStack -> FileStack
filter p (FileStack f ls rs) =
  case L.filter p (f:rs) of
    f':rs' -> FileStack f' (L.filter p ls) rs'
    []     -> case L.filter p ls of
                f':ls' -> FileStack f' ls' []
                []     -> emptyStack


-- if focus is empty but up or down are not, shift so focus is in right spot
normalize :: FileStack -> FileStack
normalize = filter (not . null)


flatten :: FileStack -> [FilePath]
flatten (FileStack f ls rs) = reverse ls ++ [f] ++ rs


merge :: FileStack -> FileStack -> FileStack
merge fs1 fs2 = let rh = down fs1
                in  fs1 { down = rh ++ flatten fs2 }


differentiate :: [FilePath] -> FileStack
differentiate []     = emptyStack
differentiate (f:fs) = FileStack f [] fs


-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: FileStack -> FileStack
reverseStack (FileStack t ls rs) = FileStack t rs ls


focusUp, focusDown, focusTop, focusBot :: FileStack -> FileStack
focusUp fs@(FileStack f (l:ls) rs)
  | null (l:ls)  = fs
  | otherwise    = FileStack l ls (f:rs)
focusDown fs@(FileStack f ls (r:rs))
  | null (r:rs)  = fs
  | otherwise    = FileStack r (f:ls) rs
focusTop fs@(FileStack f ls rs)
  | null ls      = fs
  | otherwise    = FileStack x [] (xs ++ [f] ++ rs) where (x:xs) = reverse ls
focusBot fs@(FileStack f ls rs)
  | null rs      = fs
  | otherwise    = FileStack x (xs ++ [f] ++ ls) where (x:xs) = reverse rs


moveFocusTo :: FilePath -> FileStack -> FileStack
-}
