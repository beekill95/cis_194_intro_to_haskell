module Exercise02 where

import AParser
import SExpr
import Test.HUnit

spacesTests :: Test
spacesTests =
  TestList
    [ TestLabel "> test empty string <" $
        TestCase $
          assertEqual
            "should return no spaces parsed"
            (Just ("", ""))
            (runParser spaces ""),
      TestLabel "> test no spaces <" $
        TestCase $
          assertEqual
            "should return no spaces parsed"
            (Just ("", "abcd"))
            (runParser spaces "abcd"),
      TestLabel "> test one spaces <" $
        TestCase $
          assertEqual
            "should one space parsed"
            (Just (" ", ""))
            (runParser spaces " "),
      TestLabel "> test two spaces <" $
        TestCase $
          assertEqual
            "should two spaces parsed"
            (Just ("  ", ""))
            (runParser spaces "  "),
      TestLabel "> test two spaces with some characters <" $
        TestCase $
          assertEqual
            "should two spaces parsed"
            (Just ("  ", "abcdef"))
            (runParser spaces "  abcdef")
    ]

identTests :: Test
identTests =
  TestList
    [ TestLabel "> test empty string <" $
        TestCase $
          assertEqual "should return Nothing" Nothing (runParser ident ""),
      TestLabel "> test string starts with numbers <" $
        TestCase $
          assertEqual "should return Nothing" Nothing (runParser ident "123"),
      TestLabel "> test string starts with numbers and followed by characters <" $
        TestCase $
          assertEqual "should return Nothing" Nothing (runParser ident "123abc"),
      TestLabel "> test string with only 1 character <" $
        TestCase $
          assertEqual
            "should return 1 character parsed"
            (Just ("a", ""))
            (runParser ident "a"),
      TestLabel "> test string with many characters <" $
        TestCase $
          assertEqual
            "should return all characters parsed"
            (Just ("abce", ""))
            (runParser ident "abce"),
      TestLabel "> test string with many characters and followed by numbers <" $
        TestCase $
          assertEqual
            "should return all characters parsed"
            (Just ("abce134de", ""))
            (runParser ident "abce134de"),
      TestLabel "> test string two words separated by space <" $
        TestCase $
          assertEqual
            "should return 1 word parsed"
            (Just ("one", " two"))
            (runParser ident "one two")
    ]

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel "> `spaces` <" spacesTests,
      TestLabel "> `ident` <" identTests
    ]