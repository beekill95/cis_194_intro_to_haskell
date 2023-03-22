module Calc where

import ExprT

-- Exercise 1: First version of the calculator.
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Mul lhs rhs) = eval lhs * eval rhs