module Exercise05Tests where

import Log
import LogAnalysis
import Test.HUnit

messages :: [LogMessage]
messages =
  [ LogMessage Info 6 "Completed armadillo processing",
    LogMessage Info 1 "Nothing to report",
    LogMessage (Error 99) 10 "Flange failed!",
    LogMessage Info 4 "Everything normal",
    LogMessage Info 11 "Initiating self-destruct sequence",
    LogMessage (Error 70) 3 "Way too many pickles",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 7 "Out for lunch, back in two time steps",
    LogMessage (Error 20) 2 "Too many pickles",
    LogMessage Info 9 "Back from lunch"
  ]

expectedSevereMessageContents :: [String]
expectedSevereMessageContents =
  [ "Way too many pickles",
    "Bad pickle-flange interaction detected",
    "Flange failed!"
  ]

exercise05Tests :: Test
exercise05Tests =
  TestList
    [ TestLabel
        "test with empty list"
        ( TestCase (assertEqual "should return empty list" [] (whatWentWrong []))
        ),
      TestLabel
        "test with many messages"
        ( TestCase
            ( assertEqual
                "should return severe messages in order"
                expectedSevereMessageContents
                (whatWentWrong messages)
            )
        )
    ]