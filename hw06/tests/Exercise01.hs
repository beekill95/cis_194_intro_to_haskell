module Exercise01 where

import Fibonacci
import Test.HUnit

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "first number" $
        TestCase $
          assertEqual "should return [0]" [0] (take 1 fibs1),
      TestLabel "first two numbers" $
        TestCase $
          assertEqual "should return [0, 1]" [0, 1] (take 2 fibs1),
      TestLabel "first three numbers" $
        TestCase $
          assertEqual "should return [0, 1, 1]" [0, 1, 1] (take 3 fibs1),
      TestLabel "first 10 numbers" $
        TestCase $
          assertEqual
            "should return correct Fibonacci sequence"
            [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
            (take 10 fibs1),
      TestLabel "first 15 numbers" $
        TestCase $
          assertEqual
            "should return correct Fibonacci sequence"
            [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
            (take 15 fibs1)
    ]