module Exercise02Tests where

import Log
import LogAnalysis
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel
        "test insert unknown to empty tree"
        ( TestCase
            ( assertEqual
                "should return empty tree"
                Leaf
                (insert (Unknown "message") Leaf)
            )
        ),
      TestLabel
        "test insert unknown to a tree"
        ( TestCase
            ( assertEqual
                "should return the tree unchanged"
                (Node Leaf (LogMessage Info 5 "message") Leaf)
                (insert (Unknown "message") (Node Leaf (LogMessage Info 5 "message") Leaf))
            )
        ),
      TestLabel
        "test insert message with invalid timestamp to empty tree"
        ( TestCase
            ( assertEqual
                "should return empty tree"
                Leaf
                (insert (LogMessage Info (-1) "message") Leaf)
            )
        ),
      TestLabel
        "test insert message with invalid timestamp to a tree"
        ( TestCase
            ( assertEqual
                "should return the tree unchanged"
                (Node Leaf (LogMessage Info 5 "message") Leaf)
                (insert (LogMessage Info (-1) "message") (Node Leaf (LogMessage Info 5 "message") Leaf))
            )
        ),
      TestLabel
        "test insert a message to a tree with Unknown node"
        ( TestCase
            ( assertEqual
                "should return the tree unchanged"
                (Node Leaf (Unknown "message") Leaf)
                (insert (LogMessage Info (-1) "message") (Node Leaf (Unknown "message") Leaf))
            )
        ),
      TestLabel
        "test insert a valid message to a leaf node"
        ( TestCase
            ( assertEqual
                "should return a new tree with the message"
                (Node Leaf (LogMessage Info 5 "message") Leaf)
                (insert (LogMessage Info 5 "message") Leaf)
            )
        ),
      TestLabel
        "test insert a valid message to the left node"
        ( TestCase
            ( assertEqual
                "should return a new tree with the message inserted to the left"
                (Node (Node Leaf (LogMessage Info 4 "new message") Leaf) (LogMessage Info 5 "message") Leaf)
                (insert (LogMessage Info 4 "new message") (Node Leaf (LogMessage Info 5 "message") Leaf))
            )
        ),
      TestLabel
        "test insert a message with the same timestamp"
        ( TestCase
            ( assertEqual
                "should return a new tree with the message inserted to the left"
                (Node (Node Leaf (LogMessage Info 5 "new message") Leaf) (LogMessage Info 5 "message") Leaf)
                (insert (LogMessage Info 5 "new message") (Node Leaf (LogMessage Info 5 "message") Leaf))
            )
        ),
      TestLabel
        "test insert a valid message the the right node"
        ( TestCase
            ( assertEqual
                "should return a new tree with the message inserted to the right"
                (Node Leaf (LogMessage Info 5 "message") (Node Leaf (LogMessage Info 6 "new message") Leaf))
                (insert (LogMessage Info 6 "new message") (Node Leaf (LogMessage Info 5 "message") Leaf))
            )
        )
    ]