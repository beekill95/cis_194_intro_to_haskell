module Exercise01 where

import AParser
import Data.Char
import SExpr
import Test.HUnit

zeroOrMoreTests :: Test
zeroOrMoreTests =
  TestList
    [ TestLabel "> test zero match <" $
        TestCase $
          assertEqual
            "should return Just with empty matches and the whole string"
            (Just ("", "abcdef"))
            (runParser (zeroOrMore $ satisfy isUpper) "abcdef"),
      TestLabel "> test one match <" $
        TestCase $
          assertEqual
            "should return Just with one match and the remained unmatched string"
            (Just ("A", "bcdef"))
            (runParser (zeroOrMore $ satisfy isUpper) "Abcdef"),
      TestLabel "> test two matches <" $
        TestCase $
          assertEqual
            "should return Just with two matches and the remained unmatched string"
            (Just ("AB", "cdef"))
            (runParser (zeroOrMore $ satisfy isUpper) "ABcdef"),
      TestLabel "> test three matches <" $
        TestCase $
          assertEqual
            "should return Just with three matches and the remained unmatched string"
            (Just ("ABC", "def"))
            (runParser (zeroOrMore $ satisfy isUpper) "ABCdef")
    ]

oneOrMoreTests :: Test
oneOrMoreTests =
  TestList
    [ TestLabel "> test zero match <" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (runParser (oneOrMore $ satisfy isUpper) "abcdef"),
      TestLabel "> test one match <" $
        TestCase $
          assertEqual
            "should return Just with one match and the remained unmatched string"
            (Just ("A", "bcdef"))
            (runParser (zeroOrMore $ satisfy isUpper) "Abcdef"),
      TestLabel "> test two matches <" $
        TestCase $
          assertEqual
            "should return Just with two matches and the remained unmatched string"
            (Just ("AB", "cdef"))
            (runParser (zeroOrMore $ satisfy isUpper) "ABcdef"),
      TestLabel "> test three matches <" $
        TestCase $
          assertEqual
            "should return Just with three matches and the remained unmatched string"
            (Just ("ABC", "def"))
            (runParser (zeroOrMore $ satisfy isUpper) "ABCdef")
    ]

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "> zeroOrMore <" zeroOrMoreTests,
      TestLabel "> oneOrMore <" oneOrMoreTests
    ]
