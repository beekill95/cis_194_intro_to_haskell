{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module BSTTests where

import BST
import Test.QuickCheck

-- TESTING CODE. (Students aren't expected to understand this yet, but it
-- might be interesting to read, anyway!)

instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized mk_tree

mk_tree :: Arbitrary a => Int -> Gen (BST a)
mk_tree 0 = return Leaf
mk_tree n =
  frequency
    [ (1, return Leaf),
      ( 2,
        Node
          <$> mk_tree (n `div` 2)
          <*> arbitrary
          <*> mk_tree (n `div` 2)
      )
    ]

prop_ordered :: BST Int -> Bool
prop_ordered x = isBST x == is_sorted (getElements x)
  where
    is_sorted [] = True
    is_sorted [_] = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

test :: IO ()
test = quickCheck prop_ordered

bstTests =
  conjoin
    [ counterexample "Test Ordered" prop_ordered
    ]