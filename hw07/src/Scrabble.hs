module Scrabble where

import qualified Data.Char as Char

newtype Score = Score Int
  deriving (Eq, Show, Ord)

instance Monoid Score where
  mempty = Score 0

instance Semigroup Score where
  (Score lhs) <> (Score rhs) = Score $ lhs + rhs

score :: Char -> Score
score c = case cCap of
  'A' -> Score 1
  'B' -> Score 3
  'C' -> Score 3
  'D' -> Score 2
  'E' -> Score 1
  'F' -> Score 4
  'G' -> Score 2
  'H' -> Score 4
  'I' -> Score 1
  'J' -> Score 8
  'K' -> Score 5
  'L' -> Score 1
  'M' -> Score 3
  'N' -> Score 1
  'O' -> Score 1
  'P' -> Score 3
  'Q' -> Score 10
  'R' -> Score 1
  'S' -> Score 1
  'T' -> Score 1
  'U' -> Score 1
  'V' -> Score 4
  'W' -> Score 4
  'X' -> Score 8
  'Y' -> Score 4
  'Z' -> Score 10
  _ -> mempty
  where
    cCap = Char.toUpper c

scoreString :: String -> Score
scoreString = foldr (\c s -> s <> score c) mempty