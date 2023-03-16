module Exercise03Tests where

import Log
import LogAnalysis
import Test.HUnit

messages :: [LogMessage]
messages =
  [ LogMessage Info 6 "Completed armadillo processing",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    Unknown "The Taiwanese girl sang the song beautifully",
    LogMessage Warning 8 "Burning out, rest a bit!"
  ]

expectedTree :: MessageTree
expectedTree =
  Node
    (Node Leaf (LogMessage Warning 5 "Flange is due for a check-up") Leaf)
    (LogMessage Info 6 "Completed armadillo processing")
    ( Node
        (Node Leaf (LogMessage Warning 8 "Burning out, rest a bit!") Leaf)
        (LogMessage (Error 65) 8 "Bad pickle-flange interaction detected")
        Leaf
    )

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel
        "test build message tree with empty message list"
        ( TestCase
            (assertEqual "should return empty tree" Leaf (build []))
        ),
      TestLabel
        "test build message tree with many messages"
        ( TestCase
            (assertEqual "should return correct tree" expectedTree (build messages))
        )
    ]