{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Applicative
import qualified Data.Map as M
import ExprT
import Parser
import qualified StackVM as S

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

-- Exercise 5: CPU instructions generation.
instance Expr S.Program where
  lit n = [S.PushI n]
  add lhs rhs = lhs ++ rhs ++ [S.Add]
  mul lhs rhs = lhs ++ rhs ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6: Storing intermediate results in variables.
class HasVars a where
  var :: String -> a

data VarExprT
  = VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = const $ Just n

  -- First, + operator is apply (via <$>) to the lhs Maybe Integer
  -- to yield a Maybe function that is waiting to receive another argument.
  -- Next, <*> is used to apply the rhs Maybe Integer to the Maybe function.
  add lhs rhs vars = (+) <$> lhs vars <*> rhs vars
  mul lhs rhs vars = (*) <$> lhs vars <*> rhs vars

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vars expr = expr $ M.fromList vars