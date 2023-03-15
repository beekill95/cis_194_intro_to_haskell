module Exercise01Tests where

import Log
import LogAnalysis
import Test.HUnit

-- Test parsing info.
testParseInfoMissingTimeStamp :: Test
testParseInfoMissingTimeStamp =
  TestCase
    ( assertEqual
        "should return info message with timestamp=-1"
        (LogMessage Info (-1) "la la la")
        (parseMessage "I la la la")
    )

testParseInfoWithTimeStamp :: Test
testParseInfoWithTimeStamp =
  TestCase
    ( assertEqual
        "should return info message with correct timestamp"
        (LogMessage Info 29 "la la la")
        (parseMessage "I 29 la la la")
    )

-- Test parsing warning.
testParseWarningMissingTimeStamp :: Test
testParseWarningMissingTimeStamp =
  TestCase
    ( assertEqual
        "should return warning message with timestamp=-1"
        (LogMessage Warning (-1) "la la la")
        (parseMessage "W la la la")
    )

testParseWarningWithTimeStamp :: Test
testParseWarningWithTimeStamp =
  TestCase
    ( assertEqual
        "should return warning message with correct timestamp"
        (LogMessage Warning 29 "la la la")
        (parseMessage "W 29 la la la")
    )

-- Test parse error messages.
testParseErrorWithoutTimeStamp :: Test
testParseErrorWithoutTimeStamp =
  TestCase
    ( assertEqual
        "should return error with missing timestamp"
        (LogMessage (Error 2) (-1) "help help")
        (parseMessage "E 2 help help")
    )

testParseErrorWithTimeStamp :: Test
testParseErrorWithTimeStamp =
  TestCase
    ( assertEqual
        "should return error with correct timestamp"
        (LogMessage (Error 2) 562 "help help")
        (parseMessage "E 2 562 help help")
    )

testParseErrorWithInvalidErrorCode :: Test
testParseErrorWithInvalidErrorCode =
  TestCase
    ( assertEqual
        "should return unknown message"
        (Unknown "E invalid 562 help help")
        (parseMessage "E invalid 562 help help")
    )

-- Collect all tests.
exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "parse Info with missing timestamp" testParseInfoMissingTimeStamp,
      TestLabel "parse Info with timestamp" testParseInfoWithTimeStamp,
      TestLabel "parse Warning without timestamp" testParseWarningMissingTimeStamp,
      TestLabel "parse Warning with timestamp" testParseWarningWithTimeStamp,
      TestLabel "parse Error without timestamp" testParseErrorWithoutTimeStamp,
      TestLabel "parse Error with timestamp" testParseErrorWithTimeStamp,
      TestLabel "parse Error with invalid error code" testParseErrorWithInvalidErrorCode
    ]