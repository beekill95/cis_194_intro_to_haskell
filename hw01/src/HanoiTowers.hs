module HanoiTowers where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from temporary to
  | n == 0 = []
  | n == 1 = [(from, to)]
  | otherwise =
      hanoi (n - 1) from to temporary
        ++ [(from, to)]
        ++ hanoi (n - 1) temporary from to