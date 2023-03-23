module Exercise03 where

import Fibonacci
import Test.HUnit

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "take 3 elements" $
        TestCase $
          assertEqual
            "should return 3 elements"
            [3, 4, 5]
            (take 3 $ streamToList $ Stream [3 ..]),
      TestLabel "take 10 elements" $
        TestCase $
          assertEqual
            "should return 10 repeated elements"
            [3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            (take 10 $ streamToList $ Stream [3 ..])
    ]