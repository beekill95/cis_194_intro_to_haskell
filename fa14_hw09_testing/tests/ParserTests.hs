module ParserTests where

import Ring
import Test.HUnit

-- Exercise 9: Write at least 2 unit tests
-- for each parser.

integerParserTests :: Test
integerParserTests =
  TestList
    []

mod5ParserTests :: Test
mod5ParserTests = TestList []

mat2x2ParserTests :: Test
mat2x2ParserTests = TestList []

boolParserTests :: Test
boolParserTests = TestList []

-- Collect all tests.
parserTests =
  TestList
    [ TestLabel "Integer Parser" integerParserTests,
      TestLabel "Mod5 Parser" mod5ParserTests,
      TestLabel "Mat2x2 Parser" mat2x2ParserTests,
      TestLabel "Bool Parser" boolParserTests
    ]
