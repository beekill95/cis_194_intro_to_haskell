module Main where

import Exercise01
import System.Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "> exercise 01 <" exercise01Tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess