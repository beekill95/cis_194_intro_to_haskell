{- BST.hs
   Part of an implementation of a binary search tree.
-}

module BST where

import Control.Applicative

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving (Show)

-- | Is the tree empty?
isEmpty :: BST a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- | Exercise 6: Rewritten `isBSTBetween` and `isBST` using Ord type class.
-- | Is the tree a BST between the given endpoints?
-- isBSTBetween' ::
--   -- | comparison function
--   (a -> a -> Ordering) ->
--   -- | lower bound, if one exists
--   Maybe a ->
--   -- | upper bound, if one exists
--   Maybe a ->
--   -- | tree to test
--   BST a ->
--   Bool
-- isBSTBetween' _ _ _ Leaf = True
-- isBSTBetween' cmp m_lower m_upper (Node left x right) =
--   isBSTBetween' cmp m_lower (Just x) left
--     && isBSTBetween' cmp (Just x) m_upper right
--     && case m_lower of
--       Just lower -> lower `cmp` x /= GT
--       Nothing -> True
--     && case m_upper of
--       Just upper -> x `cmp` upper /= GT
--       Nothing -> True

-- | Is this a valid BST?
-- isBST :: (a -> a -> Ordering) -> BST a -> Bool
-- isBST cmp = isBSTBetween cmp Nothing Nothing

-- | Is the tree a BST between the given endpoints?
isBSTBetween :: Ord a => Maybe a -> Maybe a -> BST a -> Bool
isBSTBetween m_lower m_upper (Node left x right) =
  isBSTBetween m_lower (Just x) left
    && isBSTBetween (Just x) m_upper right
    && case m_lower of
      Just lower -> lower <= x
      Nothing -> True
    && case m_upper of
      Just upper -> x <= upper
      Nothing -> True

-- | Is this a valid BST?
isBST :: Ord a => BST a -> Bool
isBST = isBSTBetween Nothing Nothing

-- | Get a list of the elements in sorted order
getElements :: BST a -> [a]
getElements Leaf = []
getElements (Node left x right) = getElements left ++ x : getElements right
