{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

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

-- Exercise 2 Utilities
sizeToInt :: Sized a => a -> Int
sizeToInt = getSize . size

-- Exercise 2.1: Annotation for fast indexing into a JoinList.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append _ lhs Empty) = indexJ i lhs
indexJ i (Append _ Empty rhs) = indexJ i rhs
indexJ i (Append _ lhs@(Single s _) rhs)
  | i < sInt = indexJ i lhs
  | otherwise = indexJ (i - sInt) rhs
  where
    sInt = sizeToInt s
indexJ i (Append _ lhs@(Append s _ _) rhs)
  | i < sInt = indexJ i lhs
  | otherwise = indexJ (i - sInt) rhs
  where
    sInt = sizeToInt s

-- Exercise 2.2: Drop first n elements from joinlists.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l
  | n <= 0 = l
dropJ _ (Single _ _) = Empty
dropJ n (Append s lhs rhs)
  | n >= sInt = Empty
  | otherwise = case lhs of
      Empty -> dropJ n rhs
      (Single sS _) -> dropJ n lhs +++ dropJ (n - sSInt) rhs
        where
          sSInt = sizeToInt sS
      (Append aS _ _) -> dropJ n lhs +++ dropJ (n - aSInt) rhs
        where
          aSInt = sizeToInt aS
  where
    sInt = sizeToInt s