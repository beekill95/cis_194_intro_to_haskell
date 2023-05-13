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

-- Prop 1: A ring is associative under addition: (a + b) + c = a + (b + c)
prop_ringProp_1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ringProp_1 a b c = (a `add` b) `add` c == a `add` (b `add` c)

-- Prop 2: A ring is is communitive under addition: a + b = b + a
prop_ringProp_2 :: (Ring a, Eq a) => a -> a -> Bool
prop_ringProp_2 a b = a `add` b == b `add` a

-- Prop 3: There is an element `0` such that a + 0 = a
prop_ringProp_3 :: (Ring a, Eq a) => a -> Bool
prop_ringProp_3 a = a `add` addId == a

-- Prop 4: For each a, there exists -a such that (a + -a) = 0
prop_ringProp_4 :: (Ring a, Eq a) => a -> Bool
prop_ringProp_4 a = a `add` addInv a == addId

-- Prop 5: A ring is a monoid under multiplication.
prop_ringProp_5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ringProp_5 a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

-- Prop 6: There is an element `1` such that a . 1 = a
prop_ringProp_6 :: (Ring a, Eq a) => a -> Bool
prop_ringProp_6 a = a `mul` mulId == a

-- Prop 6a: and a . 1 = a
prop_ringProp_6a :: (Ring a, Eq a) => a -> Bool
prop_ringProp_6a a = mulId `mul` a == a

-- Prop 7: Multiplication is left-distributive under addition.
prop_ringProp_7 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ringProp_7 a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

-- Prop 8: Multiplication is right-distributive under addition.
prop_ringProp_8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ringProp_8 a b c = (a `add` b) `mul` c == (a `mul` c) `add` (b `mul` c)

-- Exercise 4: One prop to rule them all.
prop_ringProps :: (Ring a, Eq a, Arbitrary a, Show a) => ((a -> a -> a -> Bool) -> (a -> a -> a -> Bool)) -> Property
prop_ringProps f =
  conjoin
    [ counterexample "Ring property #1" $ f prop_ringProp_1,
      counterexample "Ring property #2" $ f (\a b _ -> prop_ringProp_2 a b),
      counterexample "Ring property #3" $ f (applyFst prop_ringProp_3),
      counterexample "Ring property #4" $ f (applyFst prop_ringProp_4),
      counterexample "Ring property #5" $ f prop_ringProp_5,
      counterexample "Ring property #6" $ f (applyFst prop_ringProp_6),
      counterexample "Ring property #6a" $ f (applyFst prop_ringProp_6a),
      counterexample "Ring property #7" $ f prop_ringProp_7,
      counterexample "Ring property #8" $ f prop_ringProp_8
    ]

applyFst :: (a -> r) -> a -> b -> c -> r
applyFst f a _ _ = f a

-- Exercise 5: Property-based tests for all ring types.
mat2x2Ring :: (Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool) -> (Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)
mat2x2Ring = id

mod5Ring :: (Mod5 -> Mod5 -> Mod5 -> Bool) -> (Mod5 -> Mod5 -> Mod5 -> Bool)
mod5Ring = id

integerRing :: (Integer -> Integer -> Integer -> Bool) -> (Integer -> Integer -> Integer -> Bool)
integerRing = id

boolRing :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool)
boolRing = id

propTests =
  conjoin
    [ counterexample "Ring Properties for Mat2x2" $ prop_ringProps mat2x2Ring,
      counterexample "Ring Properties for Mod5" $ prop_ringProps mod5Ring,
      counterexample "Ring Properties for Integer" $ prop_ringProps integerRing,
      counterexample "Ring Properties for Boolean" $ prop_ringProps boolRing
    ]

-- The test above will fail for boolean, property #4
-- which is: there exist a value (-a) such that (a) + (-a) = 0.
-- For boolean ring, we can see that the ring needs to have at least 3 different values:
-- a, -a, and 0.
-- But boolean has only 2 values: True and False
-- Thus, it cannot satisfy this property.