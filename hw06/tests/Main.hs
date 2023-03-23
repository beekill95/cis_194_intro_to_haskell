module Main where

import Exercise01
import Exercise02
import Exercise03
import Exercise07
import System.Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "Exercise 1 Tests" exercise01Tests,
      -- TestLabel "Exercise 2 Tests" exercise02Tests,
      TestLabel "Exercise 3 Tests" exercise03Tests,
      TestLabel "Exercise 7 Tests" exercise07Tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
