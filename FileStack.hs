module FileStack where

import Prelude hiding (filter)
import qualified Data.List as L (filter)
























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


-------------
