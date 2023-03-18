{-# OPTIONS_GHC -Wall #-}

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