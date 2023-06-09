{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1: Implement a Functor instance for Parser.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser parse) = Parser parse'
    where
      parse' = fmap (first f) . parse

-- Exercise 2: Implement an Applicative instance for Parser.
instance Applicative Parser where
  pure a = Parser (const (Just (a, "")))
  (<*>) (Parser p1) (Parser p2) = Parser parse
    where
      parse s = case p1 s of
        Just (f, r) -> fmap (first f) (p2 r)
        Nothing -> Nothing

-- Exercise 3: Simple parsers.
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = fmap (const ()) abParser

intPair :: Parser [Integer]
intPair = (\l _ r -> [l, r]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4: Implement an Alternative instance for Parser.
instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser p1) (Parser p2) = Parser parse
    where
      parse s = case p1 s of
        r@(Just _) -> r
        Nothing -> p2 s

-- Exercise 5: Implement a parser `intOrUppercase` which parses
-- either an integer value or an uppercase character.
intOrUppercase :: Parser ()
intOrUppercase = intParser <|> uppercaseParser
  where
    intParser = fmap (const ()) posInt
    uppercaseParser = fmap (const ()) (satisfy isUpper)