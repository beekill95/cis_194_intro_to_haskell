module Exercise02 where

import Fibonacci
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "first number" $
        TestCase $
          assertEqual "should return [0]" [0] (take 1 fibs2),
      TestLabel "first two numbers" $
        TestCase $
          assertEqual "should return [0, 1]" [1, 0] (take 2 fibs2),
      TestLabel "first three numbers" $
        TestCase $
          assertEqual "should return [0, 1, 1]" [1, 1, 0] (take 3 fibs2),
      TestLabel "first 10 numbers" $
        TestCase $
          assertEqual
            "should return correct Fibonacci sequence"
            [34, 21, 13, 8, 5, 3, 2, 1, 1, 0]
            (take 10 fibs2),
      TestLabel "first 15 numbers" $
        TestCase $
          assertEqual
            "should return correct Fibonacci sequence"
            [377, 233, 144, 89, 55, 34, 21, 13, 8, 5, 3, 2, 1, 1, 0]
            (take 15 fibs2)
    ]
