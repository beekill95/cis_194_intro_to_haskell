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

-- Simulate a battle.
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = do
  (attackersLeft, defendersLeft) <- duel (sAttackers, sDefenders)
  return $ Battlefield (rAttackers + attackersLeft) (rDefenders + defendersLeft)
  where
    (rAttackers, sAttackers) = sendAttackers attackers
    (rDefenders, sDefenders) = sendDefenders defenders

-- Logic for sending army for a duel.
sendAttackers :: Army -> (Army, Army)
sendAttackers n
  | n <= 0 = (0, 0)
  | n == 1 = (1, 0)
  | n == 2 = (1, 1)
  | n == 3 = (1, 2)
  | otherwise = (n - 3, 3)

sendDefenders :: Army -> (Army, Army)
sendDefenders n
  | n <= 0 = (0, 0)
  | n == 1 = (0, 1)
  | otherwise = (n - 2, 2)

-- This function will implement the rule of a duel between attackers and defenders.
-- Unlike in a battle where there is a limit of how many attackers and defenders are allowed,
-- in a duel, all attackers and defenders participate in the duel.
-- A duel begins with rolling a dice for each attackers and defenders.
-- Then, the values of the dice for attachers/defenders are sorted and matched in pairs.
-- If attacker's dice's value is larger, then the attacker wins, otherwise, the defender wins.
-- The function will then return the result of the duel,
-- which is how many attackers and defenders left.
duel :: (Army, Army) -> Rand StdGen (Army, Army)
duel (attackers, defenders) = do
  attDices <- rollDices attackers
  defDices <- rollDices defenders
  return
    ( foldr
        (\(a, d) b -> attack a d b)
        (0, 0)
        $ zipLongest (DV 0) (DV 0) attDices defDices
    )
  where
    rollDices :: Army -> Rand StdGen [DieValue]
    rollDices n = sort <$> mapM (const die) [1 .. n]

-- Logic for determining who will win the fight.
attack :: DieValue -> DieValue -> (Army, Army) -> (Army, Army)
attack attDice defDice (a, d)
  | attDice > defDice = (a + 1, d)
  | otherwise = (a, d + 1)

-- Utilities.
zipLongest :: a -> b -> [a] -> [b] -> [(a, b)]
zipLongest _ _ [] [] = []
zipLongest aDef bDef [] (x : xs) = (aDef, x) : zipLongest aDef bDef [] xs
zipLongest aDef bDef (x : xs) [] = (x, bDef) : zipLongest aDef bDef xs []
zipLongest aDef bDef (a : as) (b : bs) = (a, b) : zipLongest aDef bDef as bs
