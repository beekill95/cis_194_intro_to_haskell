{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module BSTTests where

import BST
import System.Random
import Test.QuickCheck

-- TESTING CODE. (Students aren't expected to understand this yet, but it
-- might be interesting to read, anyway!)

instance (Arbitrary a) => Arbitrary (BST a) where
  arbitrary = sized mk_tree

mk_tree :: (Arbitrary a) => Int -> Gen (BST a)
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

-- Exercise 7: An Arbitrary instance of BST that creates
-- proper binary search trees.

genBST :: (Ord a, Random a) => a -> a -> Gen (BST a)
genBST lowerBound upperBound
  | lowerBound >= upperBound = return Leaf
  | otherwise =
      sized $
        \size ->
          frequency
            [ (1, return Leaf),
              ( size,
                do
                  x <- choose (lowerBound, upperBound)
                  leftNode {- resize (size - 1) -} <- genBST lowerBound x
                  rightNode {- resize (size - 1) -} <- genBST x upperBound
                  return (Node leftNode x rightNode)
              )
            ]

-- Tests

prop_ordered :: BST Int -> Bool
prop_ordered x = isBST x == is_sorted (getElements x)
  where
    is_sorted [] = True
    is_sorted [_] = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

test :: IO ()
test = quickCheck prop_ordered

-- Tests Collection

bstTests =
  conjoin
    [ counterexample "Test Ordered" prop_ordered
    ]
