import System.Directory (getCurrentDirectory)
import Path (parseAbsDir)

main :: IO ()
main = do
  putStrLn $ "Testing making route paths\n" ++ replicate 15 '-'
  curDir <- getCurrentDirectory
  curRoute <-
  putStrLn curRoute

-- | Route
--
-- A pathway down to the currently focused directory
--
-- Base: if browsing a file system, then equal to $home
--       if browsing within dir, then equal to that dir
--
-- Note: the filepath parsers all fail on empty strings
--
--     Usually home directory but can be relative to the desired "base dir"
--     |
-- [Root]
--       \
--        [Node]     A intermediate directory
--              \    |
--               [Node]
--                     \
--                      [Node]                | Either:
--                            \               | Head - pointed at local data
--                             [Node]         | Node - pointing at dir
--                                   \        /
--                                    [Head] | [Node]
--
-- Note: based on the actual data constructors we can shove a head in the middle
-- of a chain. Which is a bit like storing some data "a" in the middle other than
-- a path segment. Its generally not a problem since the constructors are not
-- exposed outside of this module. There is also a function normalize below that
-- excises any middle heads in a path way incase that situation comes up.
--
-- Note: there is no functionality to having data on a node "up" the chain
-- towards the base.
--
-- Advantages of "Route" representation:
--   -
--   -
data Route b t a
  = Base (Path   b Dir)
  | Node (Path Rel   t)   (Route b t a)
  | Head (Path Rel   t) a (Route b t a)
    deriving (Eq, Functor, Typeable)


-- | Basic pretty-printing of route
instance Show a => Show (Route b t a) where
  show (Base p)     = "\tRoute: " ++ show p
  show (Node p r)   = show r ++ show p
  show (Head p c r) = show r ++ show p ++ "\n\tContents: " ++ show c


-- |
instance Functor (Route b t) where
  fmap _ (Base p)     = Base p
  fmap _ (Node p r)   = Node p r
  fmap f (Head p c r) = Head p (f c) r


depth :: Route b t a -> Int
depth (Base _)     = 0
depth (Node _ r)   = depth r + 1
depth (Head _ r a) = depth r + 1


up :: Int -> Route b t a -> Route b t a
up _ (Base p)     = b
up d (Node p r)   = case d of
                      0 -> Node p r
                      _ -> up (n-1) r
up d (Head p a r) = case d of
                      0 -> Head p a r
                      _ -> up (n-1) r


parentDir :: Route b t a -> Route b t a
parentDir = up 1


pathSeperator :: Char
pathSeperator = '/'

absdirToRoute :: MonadThrow m => FilePath -> m (Route Abs Dir a)
absdirToRoute fp = do
  p <- parseAbsDir

  -- soemthing with cases
  -- take parent and dirname, check , and recursively build up the route
