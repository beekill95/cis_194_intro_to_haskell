module Main where

import AParser (Parser (runParser))
import Control.Monad (forever)
import SExpr

main :: IO ()
main = do
  putStrLn "Simple S-expr Parser"
  putStrLn "Input S-expr in the prompt. Quit using Ctrl-C."
  forever $ do
    putStr "> "
    l <- getLine
    print $ runParser parseSExpr l
