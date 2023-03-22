module Calc where

import ExprT
import Parser

-- Exercise 1: First version of the calculator.
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Mul lhs rhs) = eval lhs * eval rhs

-- Exercise 2: Evaluate from string to integer.
evalStr :: String -> Maybe Integer
evalStr s = eval <$> strToExprT s
  where
    strToExprT = parseExp Lit Add Mul