module Exercise07 where

import Fibonacci
import Test.HUnit

exercise07Tests :: Test
exercise07Tests =
  TestList
    [ TestLabel "n = 0" $ TestCase $ assertEqual "should equal 0" 0 (fib4 0),
      TestLabel "n = 1" $ TestCase $ assertEqual "should equal 1" 1 (fib4 1),
      TestLabel "n = 2" $ TestCase $ assertEqual "should equal 1" 1 (fib4 2),
      TestLabel "n = 3" $ TestCase $ assertEqual "should equal 2" 2 (fib4 3),
      TestLabel "n = 4" $ TestCase $ assertEqual "should equal 3" 3 (fib4 4),
      TestLabel "n = 10" $ TestCase $ assertEqual "should equal 55" 55 (fib4 10),
      TestLabel "n = 14" $ TestCase $ assertEqual "should equal 377" 377 (fib4 14),
      -- I don't really know the correct answer,
      -- Just want to check if the function runs fast or not.
      TestLabel "n = 10000000" $ TestCase $ assertBool "should always true" (377 /= fib4 1000000)
    ]
