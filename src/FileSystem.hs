module FileSystem.Utils (  
                        ) where




import System.FilePath
import System.Posix.ByteString (RawFilePath)
import System.Posix.Files.ByteString (FileStatus)


-- | Pointers are simply Paths with possible extra data. All typeclass instances
-- are defined under the assumption the extra data is file (meta)data or
-- directory contents
data Pointer b t val = Pointer (Path b t) val
                       deriving (Eq, Ord, Functor)


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
  extend f x@(Pointer p _) = Pointer p $ f x


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

m
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

