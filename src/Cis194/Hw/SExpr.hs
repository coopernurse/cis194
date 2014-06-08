{- CIS 194 HW 11
   due Monday, 8 April
-}

module Cis194.Hw.SExpr where

import Cis194.Hw.AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

--
-- zeroOrMore takes a parser as input and runs it consecutively as 
-- many times as possible (which could be none, if it fails right 
-- away), returning a list of the results. zeroOrMore always succeeds.
--
--  ghci> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" 
--        Just ("ABC","dEfgH")
--
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

--zeroOrMore p = Parser (\xs -> Just (zeroOrMoreRecur [] p xs))
--zeroOrMoreRecur :: [a] -> Parser a -> [Char] -> ([a], [Char])
--zeroOrMoreRecur acc p xs = case runParser p xs of
--  Nothing -> (acc, xs)
--  Just (a, rest) -> zeroOrMoreRecur (acc ++ [a]) p rest

--
-- oneOrMore is similar, except that it requires the input parser
-- to succeed at least once. If the input parser fails right away then
-- oneOrMore also fails.
--
-- Hint: To parse one or more occurrences of p, run p once
-- and then parse zero or more occurrences of p.
--
oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- First, spaces should parse a consecutive list of zero or
-- more whitespace characters.

spaces :: Parser String
spaces = zeroOrMore (char ' ')

-- Next, ident should parse an identifier, which for our
-- purposes will be an alphabetic character (use isAlpha)
-- followed by zero or more alphanumeric characters (use
-- isAlphaNum).

ident :: Parser String
ident = (\a b -> a : b) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseAtom :: Parser SExpr
parseAtom = (A . N <$> posInt) <|> (A . I <$> ident)

parseComb :: Parser SExpr
parseComb = Comb <$> ((char '(') *> some parseSExpr <* (char ')'))

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces

