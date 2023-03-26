module Exercise01 where

import JoinList
import Test.HUnit

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "join with empty list on the left-hand side" $
        TestCase $
          assertEqual
            "should return the rhs list"
            (Single "1" "1")
            (Empty +++ Single "1" "1"),
      TestLabel "join with empty list on the right-hand side" $
        TestCase $
          assertEqual
            "should return the lhs list"
            (Single "1" "1")
            (Single "1" "1" +++ Empty),
      TestLabel "join with two single lists" $
        TestCase $
          assertEqual
            "should return append list"
            (Append "a1" (Single "a" "a") (Single "1" "1"))
            (Single "a" "a" +++ Single "1" "1"),
      TestLabel "join a single list (lhs) with a single list (rhs)" $
        TestCase $
          assertEqual
            "should return append list"
            (Append "ba1" (Single "b" "b") (Append "a1" (Single "a" "a") (Single "1" "1")))
            (Single "b" "b" +++ Append "a1" (Single "a" "a") (Single "1" "1")),
      TestLabel "join a append list (lhs) with a single list (rhs)" $
        TestCase $
          assertEqual
            "should return append list"
            (Append "a1b" (Append "a1" (Single "a" "a") (Single "1" "1")) (Single "b" "b"))
            (Append "a1" (Single "a" "a") (Single "1" "1") +++ Single "b" "b"),
      TestLabel "join two append lists" $
        TestCase $
          assertEqual
            "should return append list"
            ( Append
                "a1b2"
                (Append "a1" (Single "a" "a") (Single "1" "1"))
                (Append "b2" (Single "b" "b") (Single "2" "2"))
            )
            ( Append "a1" (Single "a" "a") (Single "1" "1")
                +++ Append "b2" (Single "b" "b") (Single "2" "2")
            )
    ]