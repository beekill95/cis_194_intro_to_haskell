module Exercise03Tests where

import Hw04
import Test.HUnit

xorTests :: Test
xorTests = TestList []

map'Tests :: Test
map'Tests = TestList []

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "xor Tests" xorTests,
      TestLabel "map' Tests" map'Tests
    ]