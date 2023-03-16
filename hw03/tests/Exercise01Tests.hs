module Exercise01Tests where

import Golf
import Test.HUnit

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel
        "test with empty string"
        ( TestCase (assertEqual "should return empty list" [] (skips ""))
        ),
      TestLabel
        "test with string"
        ( TestCase
            ( assertEqual
                "should return array of strings"
                ["ABCD", "BD", "C", "D"]
                (skips "ABCD")
            )
        ),
      TestLabel
        "test skips with `hello!` string"
        ( TestCase
            ( assertEqual
                "should return correct array"
                ["hello!", "el!", "l!", "l", "o", "!"]
                (skips "hello!")
            )
        ),
      TestLabel
        "test with empty list"
        ( TestCase
            ( assertEqual
                "should return empty list"
                []
                (skips [] :: [[Int]])
            )
        ),
      TestLabel
        "test with integer list"
        ( TestCase
            ( assertEqual
                "should return a list of 1 element"
                [[1]]
                (skips [1])
            )
        ),
      TestLabel
        "test with boolean list"
        ( TestCase
            ( assertEqual
                "should return a list of two elements"
                [[True, False], [False]]
                (skips [True, False])
            )
        )
    ]
