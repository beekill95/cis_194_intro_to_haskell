module Exercise02 where

import JoinList
import Sized
import Test.HUnit

-- Utilities for checking our `indexJ` implementation.
-- jlToList :: JoinList m a -> [a]
-- jlToList Empty = []
-- jlToList (Single _ a) = [a]
-- jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- (!!?) :: [a] -> Int -> Maybe a
-- [] !!? _ = Nothing
-- _ !!? i | i < 0 = Nothing
-- (x : xs) !!? 0 = Just x
-- (_ : xs) !!? i = xs !!? (i - 1)

-- indexListFromJoinList :: Int -> JoinList m a -> Maybe a
-- indexListFromJoinList i = (!!? i) . jlToList

-- Exercise 2.1 tests.
exercise021Tests :: Test
exercise021Tests =
  TestList
    [ TestLabel "test index to empty" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (indexJ 0 (Empty :: JoinList Size String)),
      TestLabel "test 0 index to Single list" $
        TestCase $
          assertEqual
            "should return just the item"
            (Just "a")
            (indexJ 0 (Single (Size 1) "a")),
      TestLabel "test positive index to Single list" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (indexJ 5 (Single (Size 1) "a")),
      TestLabel "test negative index to Single list" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (indexJ (-5) (Single (Size 1) "a")),
      TestLabel "test valid index to Append list" $
        TestCase $
          assertEqual
            "should return the correct item"
            (Just "b")
            ( indexJ 1 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            ),
      TestLabel "test valid index to Append list 2" $
        TestCase $
          assertEqual
            "should return the correct item"
            (Just "a")
            ( indexJ 0 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            ),
      TestLabel "test valid index to Append list 3" $
        TestCase $
          assertEqual
            "should return the correct item"
            (Just "c")
            ( indexJ 2 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            ),
      TestLabel "test invalid index to Append list" $
        TestCase $
          assertEqual
            "should return the correct item"
            Nothing
            ( indexJ 3 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            )
    ]

-- Exercise 2.2 tests.
exercise022Tests :: Test
exercise022Tests =
  TestList
    [ TestLabel "drop empty list" $
        TestCase $
          assertEqual
            "should return empty"
            (Empty :: JoinList Size String)
            (dropJ 5 Empty),
      TestLabel "drop a single list" $
        TestCase $
          assertEqual
            "should return empty"
            Empty
            (dropJ 5 (Single (Size 1) "a")),
      TestLabel "drop negative elements from a single list" $
        TestCase $
          assertEqual
            "should return that list unchanged"
            (Single (Size 1) "a")
            (dropJ (-5) (Single (Size 1) "a")),
      TestLabel "drop 1 element from an append list" $
        TestCase $
          assertEqual
            "should return the right-hand branch"
            ( Append
                (Size 2)
                (Single (Size 1) "b")
                (Single (Size 1) "c")
            )
            ( dropJ 1 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            ),
      TestLabel "drop 2 elements from an append list" $
        TestCase $
          assertEqual
            "should return the single list"
            (Single (Size 1) "c")
            ( dropJ 2 $
                Append
                  (Size 3)
                  (Single (Size 1) "a")
                  ( Append
                      (Size 2)
                      (Single (Size 1) "b")
                      (Single (Size 1) "c")
                  )
            )
    ]

-- Exercise 2.3 tests.
exercise023Tests :: Test
exercise023Tests =
  TestList
    [ TestLabel "take elements from empty list" $
        TestCase $
          assertEqual
            "should return empty list"
            (Empty :: JoinList Size String)
            (takeJ 5 Empty),
      TestLabel "take negative number of elements from list" $
        TestCase $
          assertEqual
            "should return empty list"
            (Empty :: JoinList Size String)
            (takeJ (-5) (Single (Size 1) "a")),
      TestLabel "take zero elements from list" $
        TestCase $
          assertEqual
            "should return empty list"
            (Empty :: JoinList Size String)
            (takeJ 0 (Single (Size 1) "a")),
      TestLabel "take a single element from a single list" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            (Single (Size 1) "a")
            (takeJ 1 (Single (Size 1) "a")),
      TestLabel "take two elements from a single list" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            (Single (Size 1) "a")
            (takeJ 2 (Single (Size 1) "a")),
      TestLabel "take single element from an append list" $
        TestCase $
          assertEqual
            "should return only the single list on the left hand side"
            (Single (Size 1) "a")
            (takeJ 1 (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b"))),
      TestLabel "take two elements from an append list" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b"))
            (takeJ 2 (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b"))),
      TestLabel "take two elements from a nested append list" $
        TestCase $
          assertEqual
            "should return an append list with two elements"
            (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b"))
            ( takeJ
                2
                ( Append
                    (Size 3)
                    (Single (Size 1) "a")
                    ( Append
                        (Size 2)
                        (Single (Size 1) "b")
                        (Single (Size 1) "c")
                    )
                )
            )
    ]

-- Tests.
exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "exercise 2.1" exercise021Tests,
      TestLabel "exercise 2.2" exercise022Tests,
      TestLabel "exercise 2.3" exercise023Tests
    ]