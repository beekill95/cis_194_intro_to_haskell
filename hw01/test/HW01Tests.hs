module Main where

import System.Exit
import Test.HUnit
import ValidatingCreditCards

-- Exercise 1
-- Tests for `toDigits()`.
testNegativeNumber :: Test
testNegativeNumber =
  TestCase
    ( assertEqual "should return empty list" [] (toDigits (-17))
    )

testZeroNumber :: Test
testZeroNumber =
  TestCase
    ( assertEqual "should return empty list" [] (toDigits 0)
    )

testNumberToDigits :: Test
testNumberToDigits =
  TestCase
    ( assertEqual
        "should return correct order"
        [1, 2, 3, 4]
        (toDigits 1234)
    )

-- Tests for `toDigitsRev()`.
testNegativeNumberRev :: Test
testNegativeNumberRev =
  TestCase
    ( assertEqual "should return empty list" [] (toDigitsRev (-17))
    )

testZeroNumberRev :: Test
testZeroNumberRev =
  TestCase
    ( assertEqual "should return empty list" [] (toDigitsRev 0)
    )

testNumberToDigitsRev :: Test
testNumberToDigitsRev =
  TestCase
    ( assertEqual
        "should return correct order"
        [4, 3, 2, 1]
        (toDigitsRev 1234)
    )

-- Exercise 2
-- Tests for `doubleEveryOther()`.
testDoubleEmptyList :: Test
testDoubleEmptyList =
  TestCase
    ( assertEqual "should just return empty list" [] (doubleEveryOther [])
    )

testDoubleSingleElementList :: Test
testDoubleSingleElementList =
  TestCase
    ( assertEqual "should not double the element" [1] (doubleEveryOther [1])
    )

testDoubleTwoElementsList :: Test
testDoubleTwoElementsList =
  TestCase
    ( assertEqual
        "should only double the first element"
        [4, 1]
        (doubleEveryOther [2, 1])
    )

testDoubleThreeElementsList :: Test
testDoubleThreeElementsList =
  TestCase
    ( assertEqual
        "should only double the second element"
        [3, 4, 1]
        (doubleEveryOther [3, 2, 1])
    )

testDoubleFourElementsList :: Test
testDoubleFourElementsList =
  TestCase
    ( assertEqual
        "should only double the first element and the third element"
        [8, 3, 4, 1]
        (doubleEveryOther [4, 3, 2, 1])
    )

-- Collect all tests.
tests :: Test
tests =
  TestList
    [ -- Exercise 1
      TestLabel "testNegativeNumber" testNegativeNumber,
      TestLabel "testZeroNumber" testZeroNumber,
      TestLabel "testNumberToDigits" testNumberToDigits,
      TestLabel "testNegativeNumberRev" testNegativeNumberRev,
      TestLabel "testZeroNumberRev" testZeroNumberRev,
      TestLabel "testNumberToDigitsRev" testNumberToDigitsRev,
      -- Exercise 2
      TestLabel "testDoubleEmptyList" testDoubleEmptyList,
      TestLabel "testDoubleSingleElementList" testDoubleSingleElementList,
      TestLabel "testDoubleTwoElementsList" testDoubleTwoElementsList,
      TestLabel "testDoubleThreeElementsList" testDoubleThreeElementsList,
      TestLabel "testDoubleFourElementsList" testDoubleFourElementsList
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
