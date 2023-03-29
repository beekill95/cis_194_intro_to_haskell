module Party where

import Data.Tree
import Employee
import Text.Read (readMaybe)

-- Exercise 1.1: add a new employee to guest list and update total fun score.
-- Assume that the employee doesn't exist in the list,
-- and their direct subordinates are not in the list.
-- Basically, it will just simply add new employee to the list, and calculate the score.
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = fun}) (GL emps score) = GL (emp : emps) (fun + score)

-- Exercise 1.2: Make GuestList an instance of Monoid and Semigroup.
instance Semigroup GuestList where
  (<>) (GL lhsGuests lhsScore) (GL rhsGuests rhsScore) = GL (lhsGuests ++ rhsGuests) (lhsScore + rhsScore)

instance Monoid GuestList where
  mempty = GL [] 0

-- Exercise 1.3: Find between the two guest lists which group has more fun.
moreFun :: GuestList -> GuestList -> GuestList
moreFun lhs rhs = case lhs `compare` rhs of
  LT -> rhs
  GT -> lhs
  EQ -> lhs

-- Exercise 2: Tree fold.
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node {rootLabel = root, subForest = trees}) =
  f root (map (treeFold e f) trees)

-- Exercise 3: Implement function `nextLevel` that will compute two guest lists:
-- + The best possible guest list if we invite the boss (the employee at the root of the tree)
-- + The best possible guest list if we don't invite the boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e ls = (glCons e withoutSubordinates, withSubordinates)
  where
    (withSubordinates, withoutSubordinates) = foldr (<>) mempty ls

-- Exercise 4: Implement function `maxFun` that takes a company hierarchy
-- and outputs a fun-maximizing guest list.
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold mempty nextLevel

-- Exercise 5: Implement an IO method that reads
-- the company's hierarchy, and then prints out
-- formatted guest list.
readCompany :: String -> Maybe (Tree Employee)
readCompany = readMaybe

main :: IO ()
main = do
  putStrLn "TODO"