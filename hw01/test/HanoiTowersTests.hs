module HanoiTowersTests (hanoiTowersTests) where

import HanoiTowers
import Test.HUnit

testEmptyDisks :: Test
testEmptyDisks =
  TestCase
    ( assertEqual "should return empty moves" [] (hanoi 0 "a" "b" "c")
    )

testOneDisk :: Test
testOneDisk =
  TestCase
    ( assertEqual "should return one move" [("a", "c")] (hanoi 1 "a" "b" "c")
    )

testTwoDisks :: Test
testTwoDisks =
  TestCase
    ( assertEqual
        "should return 3 moves"
        -- Here is a bit different from the homework,
        -- In the homework, the moves are listed for moving from first peg to the second peg,
        -- But here, the moves are for from first peg to the last peg.
        [("a", "b"), ("a", "c"), ("b", "c")]
        (hanoi 2 "a" "b" "c")
    )

testThreeDisks :: Test
testThreeDisks =
  TestCase
    ( assertEqual
        "should return 6 moves"
        [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")]
        (hanoi 3 "a" "b" "c")
    )

hanoiTowersTests :: Test
hanoiTowersTests =
  TestList
    [ TestLabel "testEmptyDisks" testEmptyDisks,
      TestLabel "testOneDisk" testOneDisk,
      TestLabel "testTwoDisks" testTwoDisks,
      TestLabel "testThreeDisks" testThreeDisks
    ]