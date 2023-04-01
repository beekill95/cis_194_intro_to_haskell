module Main where

import Control.Monad.Random
import Risk
import System.IO
import Text.Read (readMaybe)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

armySize :: String -> IO Army
armySize p = do
  size <- readMaybe <$> prompt p
  case size of
    Just s -> return s
    Nothing -> armySize p

main :: IO ()
main = do
  attackers <- armySize "Attackers: "
  defenders <- armySize "Defenders: "
  prob <- fmap show $ evalRandIO $ successProb $ Battlefield attackers defenders
  putStrLn $ "Success prob: " ++ prob
