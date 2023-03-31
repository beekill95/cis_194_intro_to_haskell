module Main where

import Exercise01
import Exercise02
import Exercise03
import System.Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "> exercise 01 <" exercise01Tests,
      TestLabel "> exercise 02 <" exercise02Tests,
      TestLabel "> exercise 03 <" exercise03Tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess