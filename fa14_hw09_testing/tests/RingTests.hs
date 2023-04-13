module RingTests where

import Ring
import Test.QuickCheck

-- Exercise 1: Write Arbitrary instances for Mod5 and Mat2x2.
instance Arbitrary Mod5 where
  arbitrary = mkMod <$> arbitrary

instance Arbitrary Mat2x2 where
  arbitrary = MkMat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  -- Exercise 2: Write `shrink` method for Mat2x2.
  shrink _ = []
