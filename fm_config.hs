import Fm



-- Using Haskell source file as your config file:
-- Basically writing your own main function.
main :: IO ()
main = fm defaultConfig



myConfig :: Config
myConfig = MkConf {
    myInt = 666
  , howToSayHi = (\x -> putStrLn $ unwords ["HELLO",x,", YOU ROCK"]
  , listOfEnemies = fromList $ [ (666, "SATAN"), (-001, "God") ]
  }
