{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Golf (skips, localMaxima, histogram) where

import Data.Char

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

-- Exercise 3 Histogram
histogram :: [Integer] -> String
histogram elements = showBins binsCounted $ largestCount binsCounted
  where
    bins = map (,0 :: Integer) ([0 .. 9] :: [Integer])
    binsCounted = foldr increment bins elements

type Bin = (Integer, Integer)

increment :: Integer -> [Bin] -> [Bin]
increment _ [] = []
increment x ((binKey, binCount) : rest) =
  if x == binKey
    then (binKey, binCount + 1) : rest
    else (binKey, binCount) : increment x rest

largestCount :: [Bin] -> Integer
largestCount [] = 0
largestCount ((_, count) : rest) = max count $ largestCount rest

showBins :: [Bin] -> Integer -> String
showBins bins 0 =
  map (const '=') bins
    ++ "\n"
    ++ map (\(b, _) -> intToDigit $ fromIntegral b) bins
showBins bins level =
  map (\(_, count) -> if count >= level then '*' else ' ') bins
    ++ "\n"
    ++ showBins bins (level - 1)