module Main where

import System.Exit
import Test.HUnit
import ValidatingCreditCards

-- Exercise 1
-- Tests for `toDigits()`.
testNegativeNumber :: Test
testNegativeNumber = TestCase (assertEqual "should return empty list" [] (toDigits (-17)))

testZeroNumber :: Test
testZeroNumber = TestCase (assertEqual "should return empty list" [] (toDigits 0))

testNumberToDigits :: Test
testNumberToDigits = TestCase (assertEqual "should return correct order" [1, 2, 3, 4] (toDigits 1234))

-- Tests for `toDigitsRev()`.
testNegativeNumberRev :: Test
testNegativeNumberRev = TestCase (assertEqual "should return empty list" [] (toDigitsRev (-17)))

testZeroNumberRev :: Test
testZeroNumberRev = TestCase (assertEqual "should return empty list" [] (toDigitsRev 0))

testNumberToDigitsRev :: Test
testNumberToDigitsRev = TestCase (assertEqual "should return correct order" [4, 3, 2, 1] (toDigitsRev 1234))

tests :: Test
tests =
  TestList
    [ TestLabel "testNegativeNumber" testNegativeNumber,
      TestLabel "testZeroNumber" testZeroNumber,
      TestLabel "testNumberToDigits" testNumberToDigits,
      TestLabel "testNegativeNumberRev" testNegativeNumberRev,
      TestLabel "testZeroNumberRev" testZeroNumberRev,
      TestLabel "testNumberToDigitsRev" testNumberToDigitsRev
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
