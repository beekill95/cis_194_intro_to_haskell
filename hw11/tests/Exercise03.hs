module Exercise03 where

import AParser (Parser (runParser))
import SExpr
import Test.HUnit

atomParserTests :: Test
atomParserTests =
  TestList
    [ TestLabel "> test parse integers <" $
        TestCase $
          assertEqual
            "should return integer"
            (Just (N 1385, ""))
            (runParser atomParser "1385"),
      TestLabel "> test parse integers followed by characters without spaces <" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (runParser atomParser "1385abc"),
      TestLabel "> test parse integers followed by characters with spaces <" $
        TestCase $
          assertEqual
            "should return Nothing"
            (Just (N 1385, " abc"))
            (runParser atomParser "1385 abc"),
      -- Identifier
      TestLabel "> test parse identifiers <" $
        TestCase $
          assertEqual
            "should return the identifier parsed"
            (Just (I "a", ""))
            (runParser atomParser "a"),
      TestLabel "> test parse multichar identifiers <" $
        TestCase $
          assertEqual
            "should return the identifier parsed"
            (Just (I "abc", ""))
            (runParser atomParser "abc"),
      TestLabel "> test parse identifier with numbers <" $
        TestCase $
          assertEqual
            "should return the identifier parsed"
            (Just (I "abc123", " 23"))
            (runParser atomParser "abc123 23")
    ]

sExprParserTests :: Test
sExprParserTests =
  TestList
    [ -- Atom expressions.
      TestLabel "> test atom expression (numbers) with trailing whitespaces <" $
        TestCase $
          assertEqual
            "should return numbers parsed"
            (Just (A (N 345), ""))
            (runParser parseSExpr "  345"),
      TestLabel "> test atom expression (identifiers) with trailing whitespaces <" $
        TestCase $
          assertEqual
            "should return identifier parsed"
            (Just (A (I "abc"), ""))
            (runParser parseSExpr "  abc"),
      TestLabel "> test atom expression (identifiers) with trailing whitespaces <" $
        TestCase $
          assertEqual
            "should return identifier parsed"
            (Just (A (I "abc123"), "  def"))
            (runParser parseSExpr "  abc123  def"),
      TestLabel "> test S-expression with just parenthesis <" $
        TestCase $
          assertEqual
            "should return empty s-expr"
            (Just (Comb [], ""))
            (runParser parseSExpr "()"),
      TestLabel "> test S-expression with a number <" $
        TestCase $
          assertEqual
            "should return empty s-expr"
            (Just (Comb [A (N 123)], ""))
            (runParser parseSExpr "(123)"),
      TestLabel "> test S-expression with an identifier <" $
        TestCase $
          assertEqual
            "should return a valid s-expr"
            (Just (Comb [A (I "abc123")], ""))
            (runParser parseSExpr "(abc123)"),
      TestLabel "> test S-expression with trailing whitespaces <" $
        TestCase $
          assertEqual
            "should return a valid s-expr"
            (Just (Comb [A (N 777), A (I "abc123")], ""))
            (runParser parseSExpr "     (    777       abc123      )"),
      TestLabel "> test S-expression with nested S-expr <" $
        TestCase $
          assertEqual
            "should return a valid s-expr"
            (Just (Comb [A (N 777), A (I "abc123"), Comb [A (N 888), A (I "def")]], ""))
            (runParser parseSExpr "(777 abc123 (888 def))"),
      TestLabel "> test invalid S-expression unmatched parenthesis <" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (runParser parseSExpr "(777 abc123 (888 def)"),
      TestLabel "> test empty string <" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (runParser parseSExpr "")
    ]

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "> `atomParser` <" atomParserTests,
      TestLabel "> `sExprParser` <" sExprParserTests
    ]
