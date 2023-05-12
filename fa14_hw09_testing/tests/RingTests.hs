module RingTests where

import Ring
import Test.QuickCheck

-- Exercise 1: Write Arbitrary instances for Mod5 and Mat2x2.
instance Arbitrary Mod5 where
  arbitrary = mkMod <$> arbitrary

instance Arbitrary Mat2x2 where
  arbitrary = MkMat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = shrinkMat2x2

-- Exercise 2: Write `shrink` method for Mat2x2.
-- Hint: use zipLongest
shrinkMat2x2 :: Mat2x2 -> [Mat2x2]
shrinkMat2x2 (MkMat a b c d) = zipLongestWith4 MkMat a b c d as bs cs ds
  where
    as = shrink a
    bs = shrink b
    cs = shrink c
    ds = shrink d

zipLongestWith :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipLongestWith _ _ _ [] [] = []
zipLongestWith f a b (h : as) [] = f h b : zipLongestWith f a b as []
zipLongestWith f a b [] (h : bs) = f a h : zipLongestWith f a b [] bs
zipLongestWith f a b (ah : as) (bh : bs) = f ah bh : zipLongestWith f a b as bs

zipLongestWith4 :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> [a] -> [b] -> [c] -> [d] -> [e]
zipLongestWith4 f a b c d as bs cs ds = abcd
  where
    ab = zipLongestWith f a b as bs
    abc = zipLongestWith (\abf y -> abf y) (f a b) c ab cs
    abcd = zipLongestWith (\abcf y -> abcf y) (f a b c) d abc ds

-- Exercise 3: Test 9 properties of rings using QuickCheck
-- These properties can be found in: https://en.wikipedia.org/wiki/Ring_(mathematics)#Definition

-- Prop 1: A ring is associative under addition:
-- (a + b) + c = a + (b + c)
-- TODO