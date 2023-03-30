module Main where

import Exercise01
import Exercise02
import Exercise03
import Exercise04
import Exercise05
import System.Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ TestLabel "+> exercise 01 <+" exercise01Tests,
      TestLabel "+> exercise 02 <+" exercise02Tests,
      TestLabel "+> exercise 03 <+" exercise03Tests,
      TestLabel "+> exercise 04 <+" exercise04Tests,
      TestLabel "+> exercise 05 <+" exercise05Tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess