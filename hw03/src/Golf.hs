module Golf (skips) where

-- Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips [] = []
skips (head : rest) = (head : rest) : skips rest
  where
    enumerate = zip [1 ..]
