module Main where

import Ring
import RingTests
import Test.QuickCheck (quickCheck)

mat2x2 :: (Mat2x2 -> Bool) -> (Mat2x2 -> Bool)
mat2x2 = id

main = quickCheck $ mat2x2 prop_ringProp_4
