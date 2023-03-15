module ValidatingCreditCards
  ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits,
  )
where

-- Exercise 1
-- Implement `toDigits()` which turns any positive numbers to a list of digits,
-- and `toDigitsRev()` which do a similar thing the resulting list is reversed.
toDigits :: Integer -> [Integer]
toDigits number = reverseList (toDigitsRev number)

toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number > 0 = (number `mod` 10) : toDigitsRev (number `div` 10)
  | otherwise = []

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (head : rest) = reverseList rest ++ [head]

-- Exercise 2
-- Implement `doubleEveryOther()` that double every digit that is second-to-last,
-- fourth-to-last, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther (first : second : rest)
  | even (intListLength rest) = [first * 2, second] ++ doubleEveryOther rest
  | otherwise = [first, second * 2] ++ doubleEveryOther rest

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (first : rest) = intListLength rest + 1

-- Exercise 3
-- Implement `sumDigits()` to sum all digits.
-- All two-digit numbers are broken into 2 digits and sum with the rest.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (first : rest)
  | first < 0 = sumDigits rest
  | first < 10 = first + sumDigits rest
  | first < 100 = (first `div` 10) + (first `mod` 10) + sumDigits rest