module Exercise04 where

import AParser
import Control.Applicative
import Test.HUnit

exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel "> Test `empty` 1 <" $
        TestCase $
          assertEqual
            "should return a parser always fails"
            Nothing
            (runParser (empty :: Parser Char) "abc"),
      TestLabel "> Test `empty` 2 <" $
        TestCase $
          assertEqual
            "should return a parser always fails"
            Nothing
            (runParser (empty :: Parser Int) "123")
    ]
