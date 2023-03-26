module Exercise03 where

import JoinList
import Scrabble
import Test.HUnit

-- Tests for `scoreString`.
scoreStringTests :: Test
scoreStringTests =
  TestList
    [ TestLabel "test with empty string" $
        TestCase $
          assertEqual "should return 0" (Score 0) (scoreString ""),
      TestLabel "test with 'yay' string" $
        TestCase $
          assertEqual "should return 9" (Score 9) (scoreString "yay"),
      TestLabel "test with 'haskell!' string" $
        TestCase $
          assertEqual "should return 14" (Score 14) (scoreString "haskell!")
    ]

-- Tests for `scoreLine`.
scoreLineTests :: Test
scoreLineTests =
  TestList
    [ TestLabel "test with given example" $
        TestCase $
          assertEqual
            "should return correct join list"
            (Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!"))
            (scoreLine "yay " +++ scoreLine "haskell!")
    ]

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "scoreString" scoreStringTests,
      TestLabel "scoreLine" scoreLineTests
    ]