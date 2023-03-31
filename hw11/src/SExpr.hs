{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*> zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> alphabeticParser <*> zeroOrMore (alphabeticParser <|> alphanumericParser)
  where
    alphabeticParser = satisfy isAlpha
    alphanumericParser = satisfy isAlphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

-- My S-expr parser.
parseSExpr :: Parser SExpr
parseSExpr = spaces *> (fmap A atomParser <|> fmap Comb combExprParser)
  where
    leftParenParser = spaces *> char '('
    rightParentParser = spaces *> char ')'
    combExprParser = leftParenParser *> zeroOrMore parseSExpr <* rightParentParser

atomParser :: Parser Atom
atomParser = identParser <|> integerParser
  where
    integerParser = fmap N posInt
    identParser = fmap I ident