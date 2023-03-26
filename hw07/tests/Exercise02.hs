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

-- Tests.
exercise02Tests :: Test
exercise02Tests =
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