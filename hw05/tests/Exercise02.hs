module Exercise02 where

import Calc
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "test with empty equation" $
        TestCase $
          assertEqual "should return Nothing" Nothing (evalStr ""),
      TestLabel "test with invalid equation" $
        TestCase $
          assertEqual "should return Nothing" Nothing (evalStr "2+3*"),
      TestLabel "test with equation 1" $
        TestCase $
          assertEqual "should return correct result" (Just 20) (evalStr "(2+3)*4"),
      TestLabel "test with equation 2" $
        TestCase $
          assertEqual "should return correct result" (Just 14) (evalStr "2+3*4")
    ]
