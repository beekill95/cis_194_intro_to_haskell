{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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
newtype Stream a = Stream [a]

streamToList :: Stream a -> [a]
streamToList (Stream x) = x

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4: Utilities to work with Stream.
streamRepeat :: a -> Stream a
streamRepeat = Stream . repeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x) = Stream $ map f x

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream $ f `iterate` x

-- Exercise 5: Few more tools with stream.
nats :: Stream Integer
nats = Stream [0 ..]

-- TODO
ruler :: Stream Integer
ruler = Stream []

-- Exercise 6: Fibonacci via stream.
x :: Stream Integer
x = Stream $ 0 : 1 : [0, 0 ..]

instance Num (Stream Integer) where
  fromInteger n = Stream $ n : [0, 0 ..]
  negate = streamMap negate
  (+) (Stream lhs) (Stream rhs) = Stream $ zipWith (+) lhs rhs

  -- TODO
  (*) (Stream lhs) (Stream rhs) = Stream []

instance Fractional (Stream Integer) where
  -- TODO
  (/) (Stream lhs) (Stream rhs) = Stream []

-- Exercise 7: Fibonacci via matrix multiplication.
data Matrix = Matrix (Integer, Integer) (Integer, Integer)

instance Num Matrix where
  (*) (Matrix (a, b) (c, d)) (Matrix (x, y) (z, k)) =
    Matrix (a * x + b * y, a * y + b * k) (c * x + d * z, c * y + d * k)

fiboMatrix :: Integer -> Matrix
fiboMatrix 1 = Matrix (1, 1) (1, 0)
fiboMatrix n = fiboMatrix 1 ^ n

fib4 :: Integer -> Integer
fib4 n = x
  where
    (Matrix (_, x) (_, _)) = fiboMatrix n