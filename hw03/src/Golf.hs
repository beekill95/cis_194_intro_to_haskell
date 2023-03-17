module Golf (skips) where

-- Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips elements = map (uncurry takeEvery) elementsWithHopLength
  where
    elementsWithHopLength = zip [0 ..] $ replicate nbElements elements
    nbElements = length elements

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n elements = case remainedElements of
  [] -> []
  (first : rest) -> first : takeEvery n rest
  where
    remainedElements = drop n elements
