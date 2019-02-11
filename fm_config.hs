import Fm


import Data.Map (fromList)


main :: IO ()
main = fm defaultConfig




-- Sample for testing recompile --
myConfig :: Config
myConfig = MkConf {
    myInt = 666
  , howToSayHi = \x -> unwords ["HELLO", x ++ ",", "YOU ROCK"]
  , listOfEnemies = fromList [ (666, "SATAN"), (-001, "God") ]
  }
