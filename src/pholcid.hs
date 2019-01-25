{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}


import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.State (MonadState)
import Data.Default (Default, def)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16)
import System.Directory (getCurrentDirectory, Permissions)
import System.Exit (ExitCode(..))
import System.Time (ClockTime)


data Sorting = Undefined
             -- ^ No enforced sorting, return results as system provides
             | SortFileID
             -- ^ Arrange based on file id number
             | SortAlphabetical
             -- ^ Arrange by file name
             | SortDateModified
             -- ^ Arrange by date modified
             | SortDateCreated
             -- ^ Arrange by date created
             | SortSize
             -- ^ Arrange by size (by # subdirs for dirs, #bytes for files)
             | SortType
             -- ^ Sort with similar types together [dirs, PDFs, TXTs, ect.]
             deriving (Read, Show, Eq, Enum, Bounded)


data BorderSyles = None
                 | Solid
                 | Dash
                 deriving (Read, Show, Eq, Enum, Bounded)


data ViewMode = Miller
              | MC
              | Single
              deriving (Read, Show, Eq, Enum, Bounded)


data PermissionsFormat = Numerical | Traditional deriving (Read, Show, Eq)


data State = State {

-- | Environmental State: updated periodically without user input
    eClockTime     :: ClockTime -- ^
  , eFreeDiskSpace :: Word64    -- ^ remaining disk space
  , eLoginName     :: String    -- ^ user name
  , eExitOnError   :: Bool      -- ^ should we exit immediately on any error
  , eExitCode      :: ExitCode  -- ^ incase of critical program crash/issue
  , eUserHomeDir   :: FilePath  -- ^ localtion of home directory
  , eConfigDir     :: FilePath  -- ^ localtion of config file

-- | Focus State: updated frequently in response to user input
  , sFocus                 :: FilePath
  , sFocusDir              :: FilePath
  , sFocusDirCount         :: Word64
  , sFocusPos              :: Word64
  , sFocusModificationTime :: UTCTime
  , sFocusPermissions      :: Permissions
  , sFocusOwner            :: Text
  , sFileTypeClassify      :: Text

-- | Config: loaded from config + can be changed on the fly
  , cKeyBindings        :: () -- placeholder for now
  , cColorScheme        :: () -- placeholder for now
  , cViewMode           :: ViewMode
  , cColumnRatios       :: [Word8, Word8, Word8]
  , cSilentCommands     :: Bool -- ^ whether user wish commend results to print
  , cSaveConfigUponQuit :: Bool
  , cSort               :: Sorting
  , cSortCaseSensitive  :: Bool
  , cSortDirsFirst      :: Bool
  , cSortReverse        :: Bool
  , cShowHidden         :: Bool
  , cDisplaySpecialEntries  :: Bool
  , cDisplayDiskUsage   :: Bool
  , cDisplaySize        :: Bool
  , cDisplayPermissions :: Bool
  , cDisplayOwner       :: Bool
  , cDisplayStatusBar   :: Bool
  , cDisplaySelectionInTitleBar :: Bool
  , cAutomaticallyCountFiles    :: Bool
  , cDrawBorders        :: BorderStyle
  , cHostNameInTitleBar :: Bool
  , cPreviewDirectories :: Bool
  , cPreviewFiles       :: Bool
  , cPreviewFileTypes   :: [Text]
  , cMaxPreviewSize     :: Int

-- | Logging Config: <Url about spaceleaks with writer monad>
  , wSaveLog    :: Bool             -- ^ whether to save log upon exit
  , wMaxLogSize :: Int              -- ^ maximum number of log entries
  , wLog        :: Map UTCTime Text -- ^ the log
  , wLogFile    :: Maybe FilePath   -- ^ location to write output to (appended)


  -- Commented out entries have a counter part in ranger
  -- but are not implimented in pholcid yet/ever.
  -- TODO     cFreezeFiles
  -- TODO     cLineNumbers
  -- TODO     Handle/Display file/dir tags
  -- TODO     cMetadataDeepSearch
  -- TODO     direct git enabling?
  -- PERHAPS  Bookmarking?
  -- PERHAPS  Tabs <settings>
  -- PERHAPS  Wraped scrolling
  -- UNLIKELY Preview Images
  -- NEVER    Mouse-based features/settings
  } deriving (Show, Eq, Read)

instance Default State where
  def = undefined 


initializeState :: Maybe FilePath -- ^ Config file to use
                -> State          -- ^ Default state to use
                -> IO (IORef State)
initializeState f d = do

{-
instance Default State where
  def = State { sClockTime :: ClockTime
              , sFreeDiskSpace = 0
               --
              , sFocus = " "
              , sFocusDir = " "
              , sFocusDirCount = 0
              , sFocusPos = 0
              , sFocusModificationTime :: UTCTime
              , sFocusOwner = " "
              , sFocusPermissions = Permissions { readable = True
                                                , writeable = False
                                                , executable = False
                                                , searchable = False }
              --
              , sViewMode = Miller
              , sSort :: Arrange
              , sSortCaseSensitive = True
              , sSortDirsFirst = True
              , sSortReverse = False
              , sShowHidden = True
              , sAutomaticallyCountFiles = True
              , sColumnRatios = [1, 3, 4]
              , sDrawBorders = None
              , sHostNameInTitleBar = True
              , sPreviewDirectories = True
              , sPreviewFiles = True
              , sPreviewFileExtentions = ["txt", "md", "markdown", "hs", "py", "c"]
              , sDontPreviewFileExtentions = ["pdf", "docx", "o"]
              , sPreviewImages = False
              , sShowSelectionInTitleBar = True
              }

-}
--localState :: IO State
--localState = (\d -> def { focus = d }) `liftM` getCurrentDirectory


newtype Ph a = Pholcid { release :: ReaderT (IORef State) IO a }
               deriving (Functor, Applicative, Monad, MonadIO,
                         MonadReader (IORef State), MonadBase IO)


instance MonadBase IO Ph where
    liftBase = Ph . ReaderT . const
--instance Functor Ph where
--  fmap f p = Pholcid $ fmap f $ release p

atomicModify :: (a -> b) -> IORef a -> IO b
atomicModify fun ref = atomicModifyIORef ref (atomicModify' fun)
                    where
                       atomicModify' f v = (v, f v)


atomicModifyToIORef :: (a -> b) -> IORef a -> IO (IORef b)
atomicModifyToIORef fun ref = atomicModify fun ref >>= newIORef



--toogleHidden :: Ph a -> Ph a
--toogleHidden = local (\s -> let s' = not $ showHidden s
--                            in  s { showHidden = s' }
--                     )

-- gerald hanies
-- book
-- senior historian of CIA


{-
class Monad m => MonadState s (m :: * -> *) | m -> s where
  get :: m s
  put :: s -> m ()
  state :: (s -> (a, s)) -> m a
  {-# MINIMAL state | get, put #-}


class Monad m => MonadReader r (m :: * -> *) | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  {-# MINIMAL (ask | reader), local #-}




-- instance [safe] Monad m a => MonadReader r (ReaderT r m) a
-- instance [safe] MonadState s m => MonadState s (ReaderT r m)
--                 m = (ReaderT (IORef State) IO)
                 --s = a




main :: IO ()
main = do
  putStrLn $ "Testing making route paths\n" ++ replicate 15 '-'
  pwd <- getCurrentDirectory
  putStrLn $ "Current directory is:\n\t" ++ pwd
  p <- parseAbsDir pwd
  putStrLn $ show $ pathDepth p
  let s = Pointer p ()
  putStrLn $ show s
  putStrLn $ show $ depth s
  skipLn 3
  putStrLn $ "Testing out swaping contents of pointer"
  let z = s $> "Testing"
  putStrLn $ show z
  skipLn 3
  putStrLn $ "Depth of root = " ++ (show $ depth root)
 where
  skipLn = putStr . concat . flip replicate "\n"
 -- curDir
 -- putStrLn curRoute


-- | Pointers are simply Paths with possible extra data, typically
-- extra data is file (meta)data or directory contents
data Pointer b t val = Pointer (Path b t) Metadata val
                       deriving (Eq, Ord, Functor)

-- | Metadata
-- Placeholder definition until made more rebust
data Metadata = Metadata String

toPath :: Pointer b t val -> Path b t
toPath (Pointer p _) = p


pathEq :: Pointer b t val -> Pointer b t bal -> Bool
pathEq (Pointer p0 _) (Pointer p1 _) = p0 == p1


rootPath :: Path b t  -- DANGEROUS!
rootPath = Path.Internal.Path "/"


root :: Pointer b t ()
root = return ()


instance Applicative (Pointer b t) where
  pure v = Pointer rootPath v
  (Pointer _ f) <*> p = fmap f p


instance Monad (Pointer b t) where
  (Pointer p v) >>= f = f v


instance Comonad (Pointer b t) where
  extract (Pointer _ v) = v
  extend f x@(Pointer p m _) = Pointer p m $ f x


instance Show val => Show (Pointer b t val) where
  show x = unlines $ [ "Pointer:"
                     , "\tPath: " ++ (show $ toPath x)
                     , "\tContents: " ++ (show $ extract x)
                     ]


pathDepth :: Path b t -> Word16
pathDepth p = auxPathDepth 0 p
           where
              auxPathDepth :: Word16 -> Path b1 t1 -> Word16
              auxPathDepth y p'
                 | toFilePath p' == toFilePath rootPath
                         = y
                 | y > 10 = y
                 | otherwise
                         = auxPathDepth (y+1) (parent p')


depth :: Pointer b t a -> Word16
depth = pathDepth . toPath


swapPath :: Path b t -> Pointer b0 t0 val -> Pointer b t val
swapPath p (Pointer _ val) = Pointer p val


pathReturn :: Path b t -> Pointer b t ()
pathReturn p = swapPath p $ return ()


point :: (Path b t -> val) -> Path b t -> Pointer b t val
point f p = Pointer p $ f p


pointM :: Monad m => (Path b t -> m val) -> Path b t -> m (Pointer b t val)
pointM f p = do
  v <- f p
  return $ Pointer p v


data Args = SF Char   Args
          -- ^ Simple flag
          | DF String Args
          -- ^ Double flag
          | LN String Args
          | ECL
-}



{-
About concurrency::

getAllocationCounter :: IO Int

$ liftM2 (-) getAllocationCounter getAllocationCounter
0

$ liftM2 (-) getAllocationCounter getAllocationCounter
0

$ liftM2 (-) getAllocationCounter getAllocationCounter
0

$ liftM2 (-) getAllocationCounter (threadDelay 100000 >> getAllocationCounter)
-1224

$ liftM2 (-) getAllocationCounter (threadDelay 10000 >> getAllocationCounter)
-1224

$ liftM2 (-) getAllocationCounter (threadDelay 1000 >> getAllocationCounter)
-1224

$ liftM2 (-) getAllocationCounter (threadDelay 10 >> getAllocationCounter)
-760

$ liftM2 (-) getAllocationCounter (threadDelay 1 >> getAllocationCounter)
-1192

$ liftM2 (-) getAllocationCounter (threadDelay 0 >> getAllocationCounter)
48

$ liftM2 (-) getAllocationCounter getAllocationCounter
0

$ liftM2 (-) getAllocationCounter (threadDelay (-1) >> getAllocationCounter)
80

$ liftM2 (-) getAllocationCounter (threadDelay (-100000000) >> getAllocationCounter) 
80





is if-then-else in haskell syntactic sugar for MultiWayIf?
-- unlikely either is ss for the other
-- but odds seem more that if-then-else is ss for the same thing
-- multiwayif gets ds'd into 

$ sequence_ $ fmap (putStrLn . show <=< getUserEntryForID) [0,1,4,500,501,502,503]

UserEntry {userName = "root", userPassword = "*", userID = 0, userGroupID = 0, userGecos = "System Administrator", homeDirectory = "/var/root", userShell = "/bin/sh"}

UserEntry {userName = "daemon", userPassword = "*", userID = 1, userGroupID = 1, userGecos = "System Services", homeDirectory = "/var/root", userShell = "/usr/bin/false"}

UserEntry {userName = "_uucp", userPassword = "*", userID = 4, userGroupID = 4, userGecos = "Unix to Unix Copy Protocol", homeDirectory = "/var/spool/uucp", userShell = "/usr/sbin/uucico"}

UserEntry {userName = "messagebus", userPassword = "*", userID = 500, userGroupID = 500, userGecos = "Message Bus", homeDirectory = "/var/empty", userShell = "/usr/bin/false"}

UserEntry {userName = "davidheras", userPassword = "********", userID = 501, userGroupID = 20, userGecos = "David Heras", homeDirectory = "/Users/davidheras", userShell = "/bin/bash"}

UserEntry {userName = "ptme", userPassword = "********", userID = 502, userGroupID = 20, userGecos = "PT  ME", homeDirectory = "/Users/ptme", userShell = "/bin/bash"}

UserEntry {userName = "macports", userPassword = "*", userID = 503, userGroupID = 501, userGecos = "MacPorts", homeDirectory = "/opt/local/var/macports/home", userShell = "/usr/bin/false"}
-}
