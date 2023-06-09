module ValidatingCreditCardsTests (creditCardsTests) where

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

testsExercise01 :: Test
testsExercise01 =
  TestList
    [ TestLabel "testNegativeNumber" testNegativeNumber,
      TestLabel "testZeroNumber" testZeroNumber,
      TestLabel "testNumberToDigits" testNumberToDigits,
      TestLabel "testNegativeNumberRev" testNegativeNumberRev,
      TestLabel "testZeroNumberRev" testZeroNumberRev,
      TestLabel "testNumberToDigitsRev" testNumberToDigitsRev
    ]

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

testsExercise02 :: Test
testsExercise02 =
  TestList
    [ TestLabel "testDoubleEmptyList" testDoubleEmptyList,
      TestLabel "testDoubleSingleElementList" testDoubleSingleElementList,
      TestLabel "testDoubleTwoElementsList" testDoubleTwoElementsList,
      TestLabel "testDoubleThreeElementsList" testDoubleThreeElementsList,
      TestLabel "testDoubleFourElementsList" testDoubleFourElementsList
    ]

-- Exercise 3.
-- Test for `sumDigits()`.
testSumDigitsEmptyList :: Test
testSumDigitsEmptyList =
  TestCase
    ( assertEqual
        "should equal 0"
        0
        (sumDigits [])
    )

testSumDigitsSingleOneDigitNumber :: Test
testSumDigitsSingleOneDigitNumber =
  TestCase
    ( assertEqual
        "should return the number"
        5
        (sumDigits [5])
    )

testSumDigitsSingleTwoDigitNumber :: Test
testSumDigitsSingleTwoDigitNumber =
  TestCase
    ( assertEqual
        "should add the two digits together"
        6
        (sumDigits [15])
    )

testSumDigitsMixedNumbers :: Test
testSumDigitsMixedNumbers =
  TestCase
    ( assertEqual
        "should return the sum of all digits."
        (1 + 6 + 7 + 1 + 2 + 5)
        (sumDigits [16, 7, 12, 5])
    )

testsExercise03 :: Test
testsExercise03 =
  TestList
    [ TestLabel "testSumDigitsEmptyList" testSumDigitsEmptyList,
      TestLabel "testSumDigitsSingleOneDigitNumber" testSumDigitsSingleOneDigitNumber,
      TestLabel "testSumDigitsSingleTwoDigitNumber" testSumDigitsSingleTwoDigitNumber,
      TestLabel "testSumDigitsMixedNumbers" testSumDigitsMixedNumbers
    ]

-- Exercise 4.
testInvalidCreditCardNumber :: Test
testInvalidCreditCardNumber =
  TestCase
    ( assertEqual
        "should return invalid"
        False
        (validate 4012888888881882)
    )

testValidCreditCardNumber :: Test
testValidCreditCardNumber =
  TestCase
    ( assertEqual
        "should return valid"
        True
        (validate 4012888888881881)
    )

testsExercise04 :: Test
testsExercise04 =
  TestList
    [ TestLabel "testInvalidCreditCardNumber" testInvalidCreditCardNumber,
      TestLabel "testValidCreditCardNumber" testValidCreditCardNumber
    ]

-- All credit cards tests.
creditCardsTests :: Test
creditCardsTests =
  TestList
    [ TestLabel "Exercise 01" testsExercise01,
      TestLabel "Exercise 02" testsExercise02,
      TestLabel "Exercise 03" testsExercise03,
      TestLabel "Exercise 04" testsExercise04
    ]