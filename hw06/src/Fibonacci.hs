module Fibonacci where

import Data.Foldable

-- Exercise 1: Recursive Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2: Efficient Fibonacci that requires only O(n) addition operations.
-- Failed attempt: won't work with infinite list,
-- must provide the length of the resulting Fibonacci sequence.
fibs2Failed :: Integer -> [Integer]
fibs2Failed n = foldl fibArray [] [0 .. n]
  where
    fibArray :: [Integer] -> a -> [Integer]
    fibArray [] _ = [0]
    fibArray [_] _ = [1, 0]
    fibArray xs@(f1 : f2 : _) _ = (f1 + f2) : xs

-- TODO
fibs2 :: [Integer]
fibs2 = []
  where
    n = 0

-- Exercise 3: Stream.
newtype Stream a = Stream a

streamToList :: Stream a -> [a]
streamToList (Stream x) = repeat x

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList