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

-- Implement an insert function that generate a balanced binary tree.
-- TODO
foldTree :: [a] -> Tree a
foldTree _ = Leaf

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