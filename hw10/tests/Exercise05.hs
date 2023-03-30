module Exercise05 where

import AParser
import Test.HUnit

exercise05Tests :: Test
exercise05Tests =
  TestList
    [ TestLabel "> Test parse integer <" $
        TestCase $
          assertEqual
            "should parse integer and return empty"
            (Just ((), "abcd"))
            (runParser intOrUppercase "342abcd"),
      TestLabel "> Test parse uppercase <" $
        TestCase $
          assertEqual
            "should parse an uppercase character and return empty"
            (Just ((), "YZ"))
            (runParser intOrUppercase "XYZ"),
      TestLabel "> Test parse invalid string <" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (runParser intOrUppercase "foo")
    ]
