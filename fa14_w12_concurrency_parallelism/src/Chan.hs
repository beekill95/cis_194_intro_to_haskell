module Chan where

import Control.Concurrent

chanExample :: IO ()
chanExample = do
  ch <- newChan
  _ <- forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"
  readChan ch >>= print
  readChan ch >>= print
