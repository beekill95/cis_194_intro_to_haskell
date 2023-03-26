{-# OPTIONS_GHC -Wall #-}

module JoinList where

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

-- Exercise 1: Append two join lists.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty rhs = rhs
(+++) lhs Empty = lhs
(+++) lhs@(Single mL _) rhs@(Single mR _) = Append (mL <> mR) lhs rhs
(+++) lhs@(Single mL _) rhs@(Append mR _ _) = Append (mL <> mR) lhs rhs
(+++) lhs@(Append mL _ _) rhs@(Single mR _) = Append (mL <> mR) lhs rhs
(+++) lhs@(Append mL _ _) rhs@(Append mR _ _) = Append (mL <> mR) lhs rhs

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m