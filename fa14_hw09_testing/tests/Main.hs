module Main where

import RingTests
import System.Exit
import Test.QuickCheck

main = do
  propTestsResult <- quickCheckResult propTests
  if isSuccess propTestsResult then exitSuccess else exitFailure