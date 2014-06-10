{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import Control.Applicative
import Cis194.Hw.AParser
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

consP :: Parser a -> Parser [a] -> Parser [a]
consP p ps = liftA2 (:) p ps 

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = consP p (zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = consP p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = consP (satisfy isAlpha) (zeroOrMore (satisfy isAlphaNum))

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

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> 
              ((A <$> parseAtom) <|> 
              ((char '(') *> (Comb <$> zeroOrMore parseSExpr) <* (char ')'))) 
             <* spaces
