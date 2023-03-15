module HanoiTowersTests (hanoiTowersTests) where

import HanoiTowers
import Test.HUnit

-- Exercise 5: Hanoi Towers with 3 pegs.
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

-- Exercise 6: Hanoi Towers with 4 pegs.
testHanoi4EmptyDisk :: Test
testHanoi4EmptyDisk =
  TestCase
    ( assertEqual "should return no moves" [] (hanoi4 0 "a" "b" "c" "d")
    )

testHanoi4OneDisk :: Test
testHanoi4OneDisk =
  TestCase
    ( assertEqual "should return one move" [("a", "d")] (hanoi4 1 "a" "b" "c" "d")
    )

testHanoi4TwoDisks :: Test
testHanoi4TwoDisks =
  TestCase
    ( assertEqual
        "should return three moves"
        [("a", "b"), ("a", "d"), ("b", "d")]
        (hanoi4 2 "a" "b" "c" "d")
    )

testHanoi4ThreeDisks :: Test
testHanoi4ThreeDisks =
  TestCase
    ( assertEqual
        "should return 5 moves"
        [("a", "b"), ("a", "c"), ("a", "d"), ("c", "d"), ("b", "d")]
        (hanoi4 3 "a" "b" "c" "d")
    )

testHanoi4FourDisks :: Test
testHanoi4FourDisks =
  TestCase
    ( assertEqual
        "should return 9 moves"
        [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("a", "d"), ("c", "d"), ("b", "a"), ("b", "d"), ("a", "d")]
        (hanoi4 4 "a" "b" "c" "d")
    )

-- Collect all Hanoi Towers tests.
hanoiTowersTests :: Test
hanoiTowersTests =
  TestList
    [ TestLabel "testEmptyDisks" testEmptyDisks,
      TestLabel "testOneDisk" testOneDisk,
      TestLabel "testTwoDisks" testTwoDisks,
      TestLabel "testThreeDisks" testThreeDisks,
      -- Hanoi Towers 4 pegs.
      TestLabel "testHanoi4NoDisk" testHanoi4EmptyDisk,
      TestLabel "testHanoi4OneDisk" testHanoi4OneDisk,
      TestLabel "testHanoi4TwoDisks" testHanoi4TwoDisks,
      TestLabel "testHanoi4ThreeDisks" testHanoi4ThreeDisks,
      TestLabel "testHanoi4FourDisks" testHanoi4FourDisks
    ]