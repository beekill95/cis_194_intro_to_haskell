module Exercise04Tests where

import Hw04
import Test.HUnit

exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel "test with n = 0" $
        TestCase $
          assertEqual "should return an empty list" [] (sieveSundaram 0),
      TestLabel "test with n = 1" $
        TestCase $
          assertEqual "should return list of all primes < 4" [3] (sieveSundaram 1),
      TestLabel "test with n = 2" $
        TestCase $
          assertEqual "should return a list of all primes < 6" [3, 5] (sieveSundaram 2),
      TestLabel "test with n = 5" $
        TestCase $
          assertEqual
            "should return a list of all primes < 12"
            [3, 5, 7, 11]
            (sieveSundaram 5),
      TestLabel "test with n = 10" $
        TestCase $
          assertEqual
            "should return a list of all primes < 22"
            [3, 5, 7, 11, 13, 17, 19]
            (sieveSundaram 10),
      TestLabel "test with n = 20" $
        TestCase $
          assertEqual
            "should return a list of all primes < 42"
            [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
            (sieveSundaram 20)
    ]