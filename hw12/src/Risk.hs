{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

battle :: Battlefield -> Rand StdGen Battlefield
battle b = undefined

-- This function will implement the rule of a duel between attackers and defenders.
-- Unlike in a battle where there is a limit of how many attackers and defenders are allowed,
-- in a duel, all attackers and defenders participate in the duel.
-- A duel begins with rolling a dice for each attackers and defenders.
-- Then, the values of the dice for attachers/defenders are sorted and matched in pairs.
-- If attacker's dice's value is larger, then the attacker wins, otherwise, the defender wins.
-- The function will then return the result of the duel,
-- which is how many attackers and defenders left.
duel :: Battlefield -> Rand StdGen Battlefield
duel (Battlefield attackers defenders) = do
  attDices <- rollDices attackers
  defDices <- rollDices defenders
  return
    ( foldr
        (\(a, d) b -> attack a d b)
        (Battlefield 0 0)
        $ zipLongest (DV 0) (DV 0) attDices defDices
    )
  where
    rollDices :: Army -> Rand StdGen [DieValue]
    rollDices n = sort <$> mapM (const die) [1 .. n]

    attack :: DieValue -> DieValue -> Battlefield -> Battlefield
    attack attDice defDice (Battlefield a d)
      | attDice > defDice = Battlefield (a + 1) d
      | otherwise = Battlefield a (d + 1)

-- Utilities.
zipLongest :: a -> b -> [a] -> [b] -> [(a, b)]
zipLongest _ _ [] [] = []
zipLongest aDef bDef [] (x : xs) = (aDef, x) : zipLongest aDef bDef [] xs
zipLongest aDef bDef (x : xs) [] = (x, bDef) : zipLongest aDef bDef xs []
zipLongest aDef bDef (a : as) (b : bs) = (a, b) : zipLongest aDef bDef as bs
