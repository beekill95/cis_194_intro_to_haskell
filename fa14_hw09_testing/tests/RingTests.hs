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
shrinkMat2x2 (MkMat a b c d) = case (as, bs, cs, ds) of
    ([], [], [], []) -> []
    ([], _, _, _) -> []
  where
    as = shrink a
    bs = shrink b
    cs = shrink c
    ds = shrink d
