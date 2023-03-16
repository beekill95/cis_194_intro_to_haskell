module Exercise04Tests where

import Log
import LogAnalysis
import Test.HUnit

tree :: MessageTree
tree =
  Node
    (Node Leaf (LogMessage Warning 5 "Flange is due for a check-up") Leaf)
    (LogMessage Info 6 "Completed armadillo processing")
    ( Node
        (Node Leaf (LogMessage Warning 8 "Burning out, rest a bit!") Leaf)
        (LogMessage (Error 65) 8 "Bad pickle-flange interaction detected")
        Leaf
    )

expectedMessages :: [LogMessage]
expectedMessages =
  [ LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 6 "Completed armadillo processing",
    LogMessage Warning 8 "Burning out, rest a bit!",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected"
  ]

exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel
        "test traverse empty tree"
        (TestCase (assertEqual "should return empty list" [] (inOrder Leaf))),
      TestLabel
        "test traverse tree"
        ( TestCase
            ( assertEqual
                "should return messages in correct order"
                expectedMessages
                (inOrder tree)
            )
        )
    ]
