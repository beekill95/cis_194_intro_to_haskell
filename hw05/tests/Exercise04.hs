module Exercise04 where

import Calc
import Parser
import Test.HUnit

testExpression :: Expr a => String -> Maybe a
testExpression = parseExp lit add mul

-- Test for Integer.
maybeInteger :: Maybe Integer -> Maybe Integer
maybeInteger = id

integerTests :: Test
integerTests =
  TestList
    [ TestLabel "test literal" $
        TestCase $
          assertEqual
            "should return integer literal"
            (Just 7)
            (maybeInteger $ testExpression "7"),
      TestLabel "test addition" $
        TestCase $
          assertEqual
            "should correct integer addition"
            (Just 17)
            (maybeInteger $ testExpression "7+10"),
      TestLabel "test multiplication" $
        TestCase $
          assertEqual
            "should correct integer multiplication"
            (Just 70)
            (maybeInteger $ testExpression "7*10"),
      TestLabel "test invalid expression" $
        TestCase $
          assertEqual
            "should correct integer multiplication"
            Nothing
            (maybeInteger $ testExpression "7*10+"),
      TestLabel "test complex expression" $
        TestCase $
          assertEqual
            "should correct integer multiplication"
            (Just (-7))
            (maybeInteger $ testExpression "(3 * -4) + 5")
    ]

-- Test Bool expressions.
maybeBool :: Maybe Bool -> Maybe Bool
maybeBool = id

boolTests :: Test
boolTests =
  TestList
    [ TestLabel "test positive integers" $
        TestCase $
          assertEqual "should return True" (Just True) (maybeBool $ testExpression "17"),
      TestLabel "test negative integers" $
        TestCase $
          assertEqual "should return False" (Just False) (maybeBool $ testExpression "-17"),
      TestLabel "test addition between postive and negative integers" $
        TestCase $
          assertEqual "should return True" (Just True) (maybeBool $ testExpression "-15 + 17"),
      TestLabel "test addition between postive and positive integers" $
        TestCase $
          assertEqual "should return True" (Just True) (maybeBool $ testExpression "15 + 17"),
      TestLabel "test addition between negative and negative integers" $
        TestCase $
          assertEqual "should return False" (Just False) (maybeBool $ testExpression "-15 + (-17)"),
      TestLabel "test multiplication between negative and positive integers" $
        TestCase $
          assertEqual "should return False" (Just False) (maybeBool $ testExpression "-15 * 17"),
      TestLabel "test multiplication between positive and positive integers" $
        TestCase $
          assertEqual "should return True" (Just True) (maybeBool $ testExpression "15 * 17"),
      TestLabel "test multiplication between negative and negative integers" $
        TestCase $
          assertEqual "should return False" (Just False) (maybeBool $ testExpression "-15 * -17"),
      TestLabel "test invalid expression" $
        TestCase $
          assertEqual "should return Nothing" Nothing (maybeBool $ testExpression "-15 * -")
    ]

-- Test MinMax expressions.
minmaxTests :: Test
minmaxTests =
  TestList
    [ TestLabel "test with literal" $
        TestCase $
          assertEqual "should return correct literal" (Just (MinMax 7)) (testExpression "7"),
      TestLabel "test with addition between positive values" $
        TestCase $
          assertEqual "should return correct maximum" (Just (MinMax 9)) (testExpression "7 + 9"),
      TestLabel "test with addition between negative values" $
        TestCase $
          assertEqual "should return correct maximum" (Just (MinMax (-7))) (testExpression "-7 + (-9)"),
      TestLabel "test with addition between positive and negative values" $
        TestCase $
          assertEqual "should return correct maximum" (Just (MinMax 7)) (testExpression "7 + (-9)"),
      TestLabel "test multiplication between positive values" $
        TestCase $
          assertEqual "should return correct minimum" (Just (MinMax 7)) (testExpression "7 * 9"),
      TestLabel "test multiplication between negative values" $
        TestCase $
          assertEqual "should return correct minimum" (Just (MinMax (-9))) (testExpression "-7 * (-9)"),
      TestLabel "test multiplication between positive and negative values" $
        TestCase $
          assertEqual "should return correct minimum" (Just (MinMax (-9))) (testExpression "7 * (-9)"),
      TestLabel "test multiplication between positive and negative values" $
        TestCase $
          assertEqual "should return Nothing" Nothing (testExpression "7 +" :: Maybe MinMax)
    ]

-- Test Mod7 expressions.
mod7Tests :: Test
mod7Tests =
  TestList
    [ TestLabel "test with literal" $
        TestCase $
          assertEqual "should return correct literal" (Just (Mod7 1)) (testExpression "8"),
      TestLabel "test with addition" $
        TestCase $
          assertEqual "should return correct literal" (Just (Mod7 1)) (testExpression "8 + 7"),
      TestLabel "test with multiplication" $
        TestCase $
          assertEqual "should return correct literal" (Just (Mod7 0)) (testExpression "8 * 7")
    ]

-- Collect all tests.
exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel "test integer" integerTests,
      TestLabel "test bool" boolTests,
      TestLabel "test minmax" minmaxTests,
      TestLabel "test mod7" mod7Tests
    ]