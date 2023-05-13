module Main where

import BSTTests
import RingTests
import System.Exit
import Test.QuickCheck

propertyTests =
  conjoin
    [ counterexample "Ring Tests" ringTests,
      counterexample "BST Tests" bstTests
    ]

main = do
  propTestsResult <- quickCheckResult propertyTests
  if isSuccess propTestsResult then exitSuccess else exitFailure