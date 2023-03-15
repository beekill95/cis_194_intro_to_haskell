module Main where

import HanoiTowersTests
import System.Exit
import Test.HUnit
import ValidatingCreditCardsTests

-- Collect all tests.
tests :: Test
tests =
  TestList
    [ TestLabel "creditCardsTests" creditCardsTests,
      TestLabel "hanoiTowersTests" hanoiTowersTests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
