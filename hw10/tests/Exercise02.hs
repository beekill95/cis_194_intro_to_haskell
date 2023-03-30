module Exercise02 where

import AParser
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "+> Test `pure` <+" $
        TestCase $
          assertEqual
            "should return a parser that return the value as is"
            (Just ('C', ""))
            (runParser (pure 'C') "abc")
    ]
