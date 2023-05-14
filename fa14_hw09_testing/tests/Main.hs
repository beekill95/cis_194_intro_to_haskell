module Main where

import BSTTests
import ParserTests
import RingTests
import System.Exit
import Test.HUnit
import Test.QuickCheck

propertyTests =
  conjoin
    [ counterexample "Ring Tests" ringTests,
      counterexample "BST Tests" bstTests
    ]

unitTests = parserTests

main = do
  propTestsResult <- quickCheckResult propertyTests
  unitTestsResult <- runTestTT unitTests
  if isSuccess propTestsResult && failures unitTestsResult == 0
    then exitSuccess
    else exitFailure
