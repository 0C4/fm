module FileStack where


import Prelude hiding (filter)


import Data.Sequence hiding (adjust)

adjust = adjust'

-- | FileStack
-- Holds the list of directory contents
--
-- Data constructor not exported. Used /smart/ constructor /directory/ below.
--
--
--
-- /Seq/ vs /Map/
-- - /Speed/: Seq is much faster for the operations we need. It requires a
-- bit more bookkeeping on the state side but the payoff is retained.
-- - /Order Maintence/: Seq keeps elements in order while Map requires keeping
-- indexes in order for elements to maintain order.
--
--
-- /Seq/ vs /Stack/ (from XMonad)
-- Originally attempted using Stack. 
-- - /Speed/: directories could have thousands of entries making lists
-- incredibly inefficent. Plus the need to access "ends" of a directory means
-- needing faster
-- - /Bookkeeping/: the stack
-- - /Reverse/: Same as lists
data Directory = Directory {
     contents :: Seq FileData
   , focus :: FilePath
   }




data Empty

head, tail :: Seq a -> Maybe a
head s = lookup 0 s
tail s = lookup i s where i = length s - 1


-- | Seq-lookup version of the "maybe" function. Useful function for returning
-- to original spot if look up fails.

-- maybeLookup :: (Seq a) -> Int -> Seq(Seq a) -> Seq a
maybeLookup :: a -> Int -> Seq a -> a
maybeLookup a n s = maybe a id $ lookup n s



data Struct = Map FilePath Directory


directory :: FilePath -> [FilePath] -> Directory
directory = undefined




zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c

pullData :: FilePath -> Directory
pullData 


-- if focus is empty but up or down are not, shift so focus is in right spot
normalize :: FileStack -> FileStack
normalize = undefined


merge :: FileStack -> FileStack -> FileStack
merge fs1 fs2 = fs1 `M.union` M.mapKeys ((+) (n1 + n2)) fs2
                where n1 = fst $ M.findMax fs1
                      n2 = fst $ M.findMin fs2


fromList :: [FilePath] -> FileStack
fromList = M.fromList . zip [1..]


reverse :: FileStack -> FileStack
reverse = M.mapKeys negate


focusUp, focusDown :: FileStack -> FileStack
focusUp = M.mapKeys (+1)
focusDown = M.mapKeys (-1)


focusTop, focusBottom :: FileStack -> FileStack
focusTop fs = M.mapKeys (+n) fs where n = fst $ M.findMin fs
focusBottom fs = M.mapKeys (-n) fs where n = fst $ M.findMax fs




data Type = TypeFile | TypeDir


getFileType :: File -> FileType
getFileType = aux <$> isDirectory


data File = File {
    filePath     :: FilePath
  , fileType     :: Type
  , size         :: Int
  , permissions  :: Permissions
  , dateCreated  :: UTCTime
  , dateModified :: UTCTime
  }
