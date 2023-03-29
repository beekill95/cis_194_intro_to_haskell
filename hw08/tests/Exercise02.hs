module Exercise02 where

import Data.Tree
import Party
import Test.HUnit

tree =
  Node
    { rootLabel = 5,
      subForest =
        [ Node {rootLabel = 1, subForest = []},
          Node {rootLabel = 2, subForest = []},
          Node
            { rootLabel = 3,
              subForest =
                [ Node {rootLabel = 4, subForest = []}
                ]
            }
        ]
    }

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel " test count number of nodes" $
        TestCase $
          assertEqual
            " should return correct number"
            5
            (treeFold 0 (\_ cs -> 1 + sum cs) tree),
      TestLabel " test sum all root labels" $
        TestCase $
          assertEqual
            " should return correct sum"
            15
            (treeFold 0 (\v cs -> v + sum cs) tree)
    ]