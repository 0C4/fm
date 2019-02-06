import Fm

import Data.Map (fromList)

-- Using Haskell source file as your config file:
-- Basically writing your own main function.
main :: IO ()
main = fm {- defaultConfig -} myConfig



myConfig :: Config
myConfig = MkConf {
    myInt = 666
  , howToSayHi = \x -> unwords ["HELLO",x,", YOU ROCK"]
  , listOfEnemies = fromList [ (666, "SATAN"), (-001, "God") ]
  }
