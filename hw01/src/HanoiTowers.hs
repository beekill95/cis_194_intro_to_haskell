module HanoiTowers where

type Peg = String

type Move = (Peg, Peg)

-- Exercise 5: Hanoi Towers with 3 pegs.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from temporary to
  | n == 0 = []
  | n == 1 = [(from, to)]
  | otherwise =
      hanoi (n - 1) from to temporary
        ++ [(from, to)]
        ++ hanoi (n - 1) temporary from to

-- Exercise 6: Hanoi Towers with 4 pegs.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n from temp1 temp2 dest
  | n == 0 = []
  | n == 1 = [(from, dest)]
  | n == 2 = [(from, temp1), (from, dest), (temp1, dest)]
  | otherwise =
      hanoi4 (n - 2) from temp2 dest temp1
        ++ [(from, temp2), (from, dest), (temp2, dest)]
        ++ hanoi4 (n - 2) temp1 from temp2 dest