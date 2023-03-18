module Exercise03Tests where

import Hw04
import Test.HUnit

xorTests :: Test
xorTests =
  TestList
    [ TestLabel "test with empty list" $
        TestCase $
          assertEqual "should return False" False (xor []),
      TestLabel "test with list of False" $
        TestCase $
          assertEqual "should return False" False (xor [False, False, False]),
      TestLabel "test with list of 1 True " $
        TestCase $
          assertEqual "should return True" True (xor [False, True, False, False]),
      TestLabel "test with list of 2 True " $
        TestCase $
          assertEqual "should return False" False (xor [False, True, False, True, False])
    ]

map'Tests :: Test
map'Tests =
  TestList
    [ TestLabel "test with empty list" $
        TestCase $
          assertEqual "should return empty list" [] (map' (+ 2) []),
      TestLabel "test with non-empty list" $
        TestCase $
          assertEqual
            "should return the same result as map"
            (map (+ 2) [1 .. 5])
            (map' (+ 2) [1 .. 5])
    ]

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "xor Tests" xorTests,
      TestLabel "map' Tests" map'Tests
    ]