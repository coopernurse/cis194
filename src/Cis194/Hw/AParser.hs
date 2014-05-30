module Cis194.Hw.AParser where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Ex. 1 - implement a Functor instance for Parser

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f1 p = Parser f2
    where
      f2 xs = case runParser p xs of
        Nothing  -> Nothing
        (Just v) -> Just $ first f1 v

-- Ex. 2 - implement an Applicative instance for Parser
--
--  pure a represents the parser which consumes no input and successfully returns a result of a.
--  p1 <*> p2 represents the parser which ﬁrst runs p1 (which will consume some input and
-- produce a function), then passes the remaining input to p2 (which consumes more input
-- and produces some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should also fail (put another
-- way, p1 <*> p2 only succeeds if both p1 and p2 succeed).

-- Ex. 3a - Create a parser which expects to see the
-- characters ’a’ and ’b’ and returns them as a pair

abParser :: Parser (Char, Char)
abParser = Parser f
  where
    f xs = case runParser (char 'a') xs of
      (Just (c1, rest1)) -> case runParser (char 'b') rest1 of
        (Just (c2, rest2)) -> Just ((c1, c2), rest2)
        (_) -> Nothing
      (_) -> Nothing

-- Ex. 3b - Create a parser which acts in the same way as
-- abParser but returns () instead of 'a' and 'b'
abParser_ :: Parser ()
abParser_ = Parser f
  where
    f xs = case runParser (char 'a') xs of
      (Just (_, rest1)) -> case runParser (char 'b') rest1 of
        (Just (_, rest2)) -> Just ((), rest2)
        (_) -> Nothing
      (_) -> Nothing

-- Ex. 3c - Create a parser which reads two integer values
-- separated by a space and returns the integer values in a
-- list. You should use the provided posInt to parse the
-- integer values.

intPair :: Parser ([Integer])
intPair = Parser f
  where
    f xs = case runParser posInt xs of
      (Just (n1, rest1)) -> case runParser (char ' ') rest1 of
        (Just (_, rest2)) -> case runParser posInt rest2 of
          (Just (n2, rest3)) -> Just ([n1, n2], rest3)
          (_) -> Nothing
        (_) -> Nothing
      (_) -> Nothing


-- Ex. 4 - Write an Alternative instance for Parser
--
-- See: http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Applicative.html#t:Alternative
--
-- empty represents the parser which always fails.
-- p1 <|> p2 represents the parser which ﬁrst tries running p1. If p1 succeeds then p2 is
-- ignored and the result of p1 is returned.  Otherwise, if p1 fails, then p2 is tried instead.
--
-- Hint: there is already an Alternative instance for Maybe which you may find useful.


-- Ex. 5 - Implement a parser:
--
--  intOrUppercase :: Parser ()
--
-- which parses either an integer value or an uppercase character, and fails otherwise.






