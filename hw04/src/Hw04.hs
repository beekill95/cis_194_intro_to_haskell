{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hw04 where

-- Exercise 1: Wholemeal Programming.
-- Reinmplement these functions into more Haskell like.
fun1Old :: [Integer] -> Integer
fun1Old [] = 1
fun1Old (x : xs)
  | even x = (x - 2) * fun1Old xs
  | otherwise = fun1Old xs

fun1 :: [Integer] -> Integer
fun1 = foldr ((*) . minusTwo) 1 . keepEven

keepEven :: [Integer] -> [Integer]
keepEven = filter even

minusTwo :: Integer -> Integer
minusTwo = (-) 2

fun2Old :: Integer -> Integer
fun2Old 1 = 0
fun2Old n
  | even n = n + fun2Old (n `div` 2)
  | otherwise = fun2Old (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . keepEven . takeWhileNot1 . iterate calculate
  where
    calculate :: Integer -> Integer
    calculate n = if even n then n `div` 2 else 3 * n + 1

    takeWhileNot1 :: [Integer] -> [Integer]
    takeWhileNot1 = takeWhile (/= 1)

-- Exercise 2: Folding with trees.
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Implement an insert function that generate a balanced binary tree,
-- in other words, an AVL tree.
foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr balancedInsert Leaf

balancedInsert :: (Ord a) => a -> Tree a -> Tree a
balancedInsert x Leaf = Node 0 Leaf x Leaf
balancedInsert new tree = fst $ insert new tree
  where
    insert :: (Ord a) => a -> Tree a -> (Tree a, [InsertLocation])
    insert x Leaf = (Node 0 Leaf x Leaf, [])
    insert x (Node _ leftSubtree nodeValue rightSubtree)
      | x <= nodeValue =
          let (newLeftTree, locations) = insert x leftSubtree
              newBalancedLeftTree = balanceTree newLeftTree locations
              newHeight = 1 + highestSubtreesHeight newBalancedLeftTree rightSubtree
           in (Node newHeight newBalancedLeftTree nodeValue rightSubtree, LeftTree : locations)
      | otherwise =
          let (newRightTree, locations) = insert x rightSubtree
              newBalancedRightTree = balanceTree newRightTree locations
              newHeight = 1 + highestSubtreesHeight leftSubtree newBalancedRightTree
           in (Node newHeight leftSubtree nodeValue newBalancedRightTree, RightTree : locations)

    balanceTree :: Tree a -> [InsertLocation] -> Tree a
    balanceTree maybeUnbalancedTree locations =
      if isBalancedTree maybeUnbalancedTree
        then maybeUnbalancedTree
        else case locations of
          [] -> Leaf
          [_] -> Leaf
          (LeftTree : LeftTree : _) -> rotateRight maybeUnbalancedTree
          (RightTree : RightTree : _) -> rotateLeft maybeUnbalancedTree
          (RightTree : LeftTree : _) -> (rotateRight . rotateLeft) maybeUnbalancedTree
          (LeftTree : RightTree : _) -> (rotateLeft . rotateRight) maybeUnbalancedTree

isBalancedTree :: Tree a -> Bool
isBalancedTree Leaf = True
isBalancedTree (Node _ Leaf _ Leaf) = True
isBalancedTree (Node _ (Node leftHeight _ _ _) _ Leaf) = leftHeight == 0
isBalancedTree (Node _ Leaf _ (Node rightHeight _ _ _)) = rightHeight == 0
isBalancedTree (Node _ (Node leftHeight _ _ _) _ (Node rightHeight _ _ _)) =
  abs (leftHeight - rightHeight) < 2

-- For reference: Unbalanced insertion with the inserted locations.
-- unbalancedInsert :: (Ord a) => a -> Tree a -> (Tree a, [InsertLocation])
-- unbalancedInsert x Leaf = (Node 0 Leaf x Leaf, [])
-- unbalancedInsert x (Node _ leftSubtree nodeValue rightSubtree)
--   | x <= nodeValue =
--       let (newLeftTree, locations) = unbalancedInsert x leftSubtree
--           nodeHeight = 1 + highestSubtreesHeight newLeftTree rightSubtree
--        in (Node nodeHeight newLeftTree nodeValue rightSubtree, LeftTree : locations)
--   | otherwise =
--       let (newRightTree, locations) = unbalancedInsert x rightSubtree
--           nodeHeight = 1 + highestSubtreesHeight leftSubtree newRightTree
--        in (Node nodeHeight leftSubtree nodeValue newRightTree, RightTree : locations)

-- Data type for indicating where the node was inserted into the tree.
data InsertLocation = LeftTree | RightTree

highestSubtreesHeight :: Tree a -> Tree a -> Integer
highestSubtreesHeight Leaf Leaf = -1
highestSubtreesHeight (Node height _ _ _) Leaf = height
highestSubtreesHeight Leaf (Node height _ _ _) = height
highestSubtreesHeight (Node leftHeight _ _ _) (Node rightHeight _ _ _) = max leftHeight rightHeight

-- Rotate trees.
rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft t@(Node _ _ _ Leaf) = t
rotateLeft (Node _ pLeftTree pNode (Node _ cLeftTree cNode cRightTree)) =
  Node newHeightOfTree newLeftTree cNode cRightTree
  where
    newHeightOfLeftTree = 1 + highestSubtreesHeight pLeftTree cLeftTree
    newLeftTree = Node newHeightOfLeftTree pLeftTree pNode cLeftTree
    newHeightOfTree = 1 + highestSubtreesHeight newLeftTree cRightTree

rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight t@(Node _ Leaf _ _) = t
rotateRight (Node _ (Node _ cLeftTree cNode cRightTree) pNode pRightTree) =
  Node newTreeHeight cLeftTree cNode newRightTree
  where
    newHeightOfRightTree = 1 + highestSubtreesHeight cRightTree pRightTree
    newRightTree = Node newHeightOfRightTree cRightTree pNode pRightTree
    newTreeHeight = 1 + highestSubtreesHeight cLeftTree newRightTree

-- Exercise 3: More Folds.
-- Implement xor function,
-- which equates to true only if there are an odd number of True values,
-- and does not matter how many False there are.
-- Must use fold.
xor :: [Bool] -> Bool
xor = odd . foldr incrementWhenTrue 0
  where
    incrementWhenTrue :: Bool -> Integer -> Integer
    incrementWhenTrue flag = if flag then (+) 1 else id

-- Implement `map` function as `foldr`.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- Exercise 4: Finding primes.
-- Finding all odd prime numbers up to 2n + 2 using function composition.
-- https://en.wikipedia.org/wiki/Sieve_of_Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n
  | n == 0 = []
  | otherwise = map ((+ 1) . (2 *)) $ filter (`notElem` excludedNumbers) [1 .. n]
  where
    excludedNumbers = sundaramExcludedNumbers n

sundaramExcludedNumbers :: Integer -> [Integer]
sundaramExcludedNumbers n = map (uncurry excludedNumber) ij
  where
    k = (n - 2) `div` 2

    excludedNumber :: Integer -> Integer -> Integer
    excludedNumber i j = i + j + 2 * i * j

    ij :: [(Integer, Integer)]
    ij = concatMap validIJs [1 .. k]

    validIJs :: Integer -> [(Integer, Integer)]
    validIJs i = map (i,) (takeWhile ((<= n) . excludedNumber i) [i ..])