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

-- Exercise 3: Type class expression Expr.
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4: Make Integer, Bool, MinMax, and Mod7 as instances of Expr.
instance Expr Integer where
  lit n = n
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0 = False
    | otherwise = True

  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax lhs) (MinMax rhs) = lit $ max lhs rhs
  mul (MinMax lhs) (MinMax rhs) = lit $ min lhs rhs

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 lhs) (Mod7 rhs) = lit $ (lhs + rhs) `mod` 7
  mul (Mod7 lhs) (Mod7 rhs) = lit $ (lhs * rhs) `mod` 7