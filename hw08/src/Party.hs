module Party where

import Employee

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