module Main where

import Codec.Compression.GZip (compress)
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  putStrLn "Enter a file to compress> "
  line <- getLine
  case line of
    "" -> return ()
    name -> do
      handle print $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()

      main
  where
    compressFile path = L.writeFile (path ++ ".gz") . compress
