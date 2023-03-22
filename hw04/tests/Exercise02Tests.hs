module Exercise02Tests where

import Hw04
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "Test insert empty list" $
        TestCase $
          assertEqual "should return Leaf" Leaf (foldTree ""),
      TestLabel "Test insert one-element list" $
        TestCase $
          assertEqual "should return a tree with 1 node" (Node 0 Leaf 'A' Leaf) (foldTree "A"),
      TestLabel "Test insert two-element list" $
        TestCase $
          assertEqual
            "should return a tree with right subtree only"
            (Node 1 Leaf 'A' (Node 0 Leaf 'B' Leaf))
            (foldTree "BA"),
      TestLabel "Test insert three-element list" $
        TestCase $
          assertEqual
            "should return a balanced tree"
            (Node 1 (Node 0 Leaf 'A' Leaf) 'B' (Node 0 Leaf 'C' Leaf))
            (foldTree "CBA"),
      TestLabel "Test insert multiple elements list" $
        TestCase $
          assertEqual
            "should return a balanced tree"
            ( Node
                3
                (Node 1 (Node 0 Leaf 'A' Leaf) 'B' (Node 0 Leaf 'C' Leaf))
                'D'
                ( Node
                    2
                    ( Node
                        1
                        (Node 0 Leaf 'E' Leaf)
                        'F'
                        (Node 0 Leaf 'G' Leaf)
                    )
                    'H'
                    (Node 1 Leaf 'I' (Node 0 Leaf 'J' Leaf))
                )
            )
            (foldTree "JIHGFEDCBA"),
      TestLabel "Test insert multiple elements list 2" $
        TestCase $
          assertEqual
            "should return a balanced tree"
            ( Node
                3
                ( Node
                    2
                    ( Node
                        1
                        (Node 0 Leaf 'A' Leaf)
                        'B'
                        (Node 0 Leaf 'C' Leaf)
                    )
                    'D'
                    ( Node
                        1
                        (Node 0 Leaf 'E' Leaf)
                        'F'
                        (Node 0 Leaf 'G' Leaf)
                    )
                )
                'H'
                ( Node
                    2
                    (Node 0 Leaf 'I' Leaf)
                    'J'
                    (Node 1 Leaf 'K' (Node 0 Leaf 'L' Leaf))
                )
            )
            (foldTree "LKJIHGFEDCBA")
    ]
