module Exercise01Tests where

import Hw04
import Test.HUnit

fun1Tests :: Test
fun1Tests =
  TestList
    [ TestLabel "test with empty list" $
        TestCase $
          assertEqual
            "should return 1"
            (fun1Old [])
            (fun1 []),
      TestLabel "test with array from 0 to 9" $
        TestCase $
          assertEqual
            "should return result similar to the old one"
            (fun1Old [0 .. 9])
            (fun1 [0 .. 9])
    ]

fun2Tests :: Test
fun2Tests =
  TestList
    [ TestLabel "test with 1" $
        TestCase $
          assertEqual "shold return 0" 0 (fun2 1),
      TestLabel "test with 2" $
        TestCase $
          assertEqual
            "should return result similar to the old implementation"
            (fun2Old 2)
            (fun2 2),
      TestLabel "test with 3" $
        TestCase $
          assertEqual
            "should return result similar to the old implementation"
            (fun2Old 3)
            (fun2 3),
      TestLabel "test with 4" $
        TestCase $
          assertEqual
            "should return result similar to the old implementation"
            (fun2Old 4)
            (fun2 4),
      TestLabel "test with 5" $
        TestCase $
          assertEqual
            "should return result similar to the old implementation"
            (fun2Old 5)
            (fun2 5)
    ]

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "fun1 tests" fun1Tests,
      TestLabel "fun2 tests" fun2Tests
    ]