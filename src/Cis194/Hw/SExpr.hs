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

-- Hint: To parse one or more occurrences of p, run p once
-- and then parse zero or more occurrences of p.

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some
{-oneOrMore p = (:) <$> p <*> zeroOrMore p-}

-- To parse zero or more occurrences of p, try parsing one
-- or more; if that fails, return the empty list.

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many
{-zeroOrMore p = oneOrMore p <|> pure []-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- First, spaces should parse a consecutive list of zero or
-- more whitespace characters.

spaces :: Parser String
spaces = many $ char ' '

-- Next, ident should parse an identifier, which for our
-- purposes will be an alphabetic character (use isAlpha)
-- followed by zero or more alphanumeric characters (use
-- isAlphaNum).

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> many (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
