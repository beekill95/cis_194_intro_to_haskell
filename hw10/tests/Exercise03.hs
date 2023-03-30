module Exercise03 where

import AParser
import Test.HUnit

abParserTests :: Test
abParserTests =
  TestList
    [ TestLabel "+> parse a valid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            (Just (('a', 'b'), "cdef"))
            (runParser abParser "abcdef"),
      TestLabel "+> parse an invalid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            Nothing
            (runParser abParser "aebcdf")
    ]

abParser'Tests :: Test
abParser'Tests =
  TestList
    [ TestLabel "+> parse a valid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            (Just ((), "cdef"))
            (runParser abParser_ "abcdef"),
      TestLabel "+> parse an invalid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            Nothing
            (runParser abParser_ "aebcdf")
    ]

intPairTests :: Test
intPairTests =
  TestList
    [ TestLabel "+> parse a valid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            (Just ([12, 35], ""))
            (runParser intPair "12 35"),
      TestLabel "+> parse an invalid string <+" $
        TestCase $
          assertEqual
            "should return the correct result"
            Nothing
            (runParser intPair "aebcdf")
    ]

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "+> `abParser` <+" abParserTests,
      TestLabel "+> `abParser_` <+" abParser'Tests,
      TestLabel "+> `intPair` <+" intPairTests
    ]
