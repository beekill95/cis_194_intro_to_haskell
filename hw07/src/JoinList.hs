{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Buffer
import qualified Data.Foldable as F
import Scrabble
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

-- Exercise 2.3: Take first n elements from join lists.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _
  | n <= 0 = Empty
takeJ _ l@(Single _ _) = l
takeJ n l@(Append s lhs rhs)
  | n >= sInt = l
  | otherwise = case lhs of
      Empty -> takeJ n rhs
      (Single sS _) -> takeJ n lhs +++ takeJ (n - sSInt) rhs
        where
          sSInt = sizeToInt sS
      (Append aS _ _) -> takeJ n lhs +++ takeJ (n - aSInt) rhs
        where
          aSInt = sizeToInt aS
  where
    sInt = sizeToInt s

-- Exercise 3: Scrabble score.
scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine s = Single (scoreString s) s

-- Exercise 4: Make JoinList an instance of Buffer.
-- Here, each Single will contain a line.
instance Buffer (JoinList (Score, Size) String) where
  toString = F.foldr (++) ""

  -- TODO
  fromString _ = Empty
  line = indexJ

  replaceLine 0 c Empty = Single (scoreString c, Size 1) c
  replaceLine 0 c (Single _ _) = Single (scoreString c, Size 1) c
  replaceLine 0 _ l = l
  replaceLine n _ l | n < 0 = l
  replaceLine n c l = case l of
    (Append as lhs rhs) ->
      if n >= sizeToInt as
        then l
        else case lhs of
          Empty ->
            if n == 0
              then replaceLine n c lhs +++ rhs
              else lhs +++ replaceLine (n - 1) c rhs
          (Single s _) ->
            if n < sInt
              then replaceLine n c lhs +++ rhs
              else lhs +++ replaceLine (n - sInt) c rhs
            where
              sInt = sizeToInt s
          (Append s _ _) ->
            if n < sInt
              then replaceLine n c lhs +++ rhs
              else lhs +++ replaceLine (n - sInt) c rhs
            where
              sInt = sizeToInt s
    _ -> l

  numLines Empty = getSize mempty
  numLines (Single s _) = sizeToInt s
  numLines (Append s _ _) = sizeToInt s

  value Empty = s
    where
      (Score s) = mempty
  value (Single (Score s, _) _) = s
  value (Append (Score s, _) _ _) = s

instance F.Foldable (JoinList m) where
  foldMap _ Empty = mempty
  foldMap f (Single _ x) = f x
  foldMap f (Append _ lhs rhs) = foldMap f lhs <> foldMap f rhs