module Main where

import Ring
import RingTests
import Test.QuickCheck

mat2x2Ring :: (Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool) -> (Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)
mat2x2Ring = id

mod5Ring :: (Mod5 -> Mod5 -> Mod5 -> Bool) -> (Mod5 -> Mod5 -> Mod5 -> Bool)
mod5Ring = id

integerRing :: (Integer -> Integer -> Integer -> Bool) -> (Integer -> Integer -> Integer -> Bool)
integerRing = id

boolRing :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool)
boolRing = id

main = do
  quickCheck $ prop_ringProps mat2x2Ring
  quickCheck $ prop_ringProps mod5Ring
  quickCheck $ prop_ringProps integerRing
  quickCheck $ prop_ringProps boolRing