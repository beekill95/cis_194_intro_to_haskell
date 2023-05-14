module ParserTests where

import Ring
import Test.HUnit

-- Exercise 9: Write at least 2 unit tests
-- for each parser.

integerParserTests :: Test
integerParserTests =
  TestList
    [ TestLabel "Read valid integer" $
        TestCase $
          assertEqual
            "should return correct integer"
            (Just (177 :: Integer, "abc"))
            (parse "177abc"),
      TestLabel "Read invalid integer" $
        TestCase $
          assertEqual
            "should return Nothing"
            (Nothing :: Maybe (Integer, String))
            (parse "abc177")
    ]

mod5ParserTests :: Test
mod5ParserTests =
  TestList
    [ TestLabel "Read valid Mod5" $
        TestCase $
          assertEqual
            "should return correct value"
            (Just (mkMod 177, "abc"))
            (parse "177abc"),
      TestLabel "Read invalid Mod5" $
        TestCase $
          assertEqual
            "should return Nothing"
            (Nothing :: Maybe (Mod5, String))
            (parse "abc177")
    ]

mat2x2ParserTests :: Test
mat2x2ParserTests =
  TestList
    [ TestLabel "Read valid Mat2x2" $
        TestCase $
          assertEqual
            "should return correct Mat2x2"
            (Just (MkMat 2 3 1 4, "abc"))
            (parse "[[2,3][1,4]]abc"),
      TestLabel "Read invalid Mat2x2" $
        TestCase $
          assertEqual
            "should return correct Mat2x2"
            (Nothing :: Maybe (Mat2x2, String))
            (parse "[[2,3]1,4]]abc")
    ]

boolParserTests :: Test
boolParserTests =
  TestList
    [ TestLabel "Read valid boolean" $
        TestCase $
          assertEqual
            "should parse True"
            (Just (True, " abc"))
            (parse "True abc"),
      TestLabel "Read valid boolean" $
        TestCase $
          assertEqual
            "should return False"
            (Just (False, " abc"))
            (parse "False abc"),
      TestLabel "Read invalid boolean" $
        TestCase $
          assertEqual
            "should return Nothing"
            (Nothing :: Maybe (Bool, String))
            (parse "177abc")
    ]

-- Collect all tests.
parserTests =
  TestList
    [ TestLabel "Integer Parser" integerParserTests,
      TestLabel "Mod5 Parser" mod5ParserTests,
      TestLabel "Mat2x2 Parser" mat2x2ParserTests,
      TestLabel "Bool Parser" boolParserTests
    ]
