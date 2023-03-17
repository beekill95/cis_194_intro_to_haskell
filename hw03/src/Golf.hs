{-# OPTIONS_GHC -Wall #-}

module Golf (skips, localMaxima) where

-- Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips elements = map (uncurry takeEvery) elementsWithHopLength
  where
    elementsWithHopLength = zip [0 ..] $ replicate nbElements elements
    nbElements = length elements

takeEvery :: Int -> [a] -> [a]
takeEvery n elements = case remainedElements of
  [] -> []
  (first : rest) -> first : takeEvery n rest
  where
    remainedElements = drop n elements

-- Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima elements = foldr accumulateMaximas [] maximas
  where
    maximas = map maxima $ makeTriplets elements

accumulateMaximas :: Maybe Integer -> [Integer] -> [Integer]
accumulateMaximas Nothing maximas = maximas
accumulateMaximas (Just m) maximas = m : maximas

maxima :: (Integer, Integer, Integer) -> Maybe Integer
maxima (a, b, c) = if b > a && b > c then Just b else Nothing

makeTriplets :: [Integer] -> [(Integer, Integer, Integer)]
makeTriplets [] = []
makeTriplets [_] = []
makeTriplets [_, _] = []
makeTriplets (first : second : third : rest) =
  ( first,
    second,
    third
  )
    : makeTriplets (second : third : rest)