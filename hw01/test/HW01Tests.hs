module Main where

import System.Exit
import Test.HUnit
import ValidatingCreditCardsTests

-- Collect all tests.
tests :: Test
tests =
  TestList
    [ TestLabel "creditCardsTests" creditCardsTests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
