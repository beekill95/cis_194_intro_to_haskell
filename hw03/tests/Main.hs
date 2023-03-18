module Main where

import Exercise01Tests
import Exercise02Tests
import Exercise03Tests
import System.Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "Exercise 1 Tests" exercise01Tests,
      TestLabel "Exercise 2 Tests" exercise02Tests,
      TestLabel "Exercise 3 Tests" exercise03Tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
