module Cis194.Hw.AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
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
--
-- You may find it useful to implement:

first :: (a -> b) -> (a,c) -> (b,c)
first fx (a,c) = (fx a, c)

instance Functor Parser where  
  fmap f p = Parser (\xs -> case runParser p xs of
    Nothing  -> Nothing
    (Just t) -> Just $ first f t)

-- Ex. 2 - implement an Applicative instance for Parser
--
--  pure a represents the parser which consumes no input and successfully returns a result of a.
--  p1 <*> p2 represents the parser which ﬁrst runs p1 (which will consume some input and 
-- produce a function), then passes the remaining input to p2 (which consumes more input 
-- and produces some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should also fail (put another
-- way, p1 <*> p2 only succeeds if both p1 and p2 succeed).

instance Applicative Parser where
    pure a = Parser (\xs -> Just (a, xs))
    Parser p1 <*> Parser p2 = Parser (\xs -> case p1 xs of
      Nothing -> Nothing
      (Just (f, rest)) -> case p2 rest of
        Nothing -> Nothing
        (Just t) -> Just $ first f t)

-- Ex. 3a - Create a parser:
--
--   abParser :: Parser (Char, Char)
--
-- which expects to see the characters ’a’ and ’b’ and returns them as a pair

abParser :: Parser (Char, Char)
abParser = (\a b -> (a,b)) <$> char 'a' <*> char 'b'

-- Ex. 3b - Create a parser:
--
--   abParser_ :: Parser ()
--
-- which acts in the same way as abParser but returns () instead of 'a' and 'b'

toUnitParser :: Parser a -> Parser ()
toUnitParser = (<$>) (\x -> ()) 

abParser_ :: Parser ()
abParser_ = toUnitParser abParser

-- Ex. 3c - Create a parser:
--
--   intPair 
--
-- which reads two integer values separated by a space and returns the integer 
-- values in a list. You should use the provided posInt to parse the integer values.

intPair :: Parser [Integer]

--
-- 1st attempt - works, but verbose
--
--
--spaceParser :: Parser ()
--spaceParser = Parser f
--  where
--    f (' ':xs) = Just ((), xs)
--    f _ = Nothing
--
--intPair = Parser (\xs -> case runParser posInt xs of
--  Nothing -> Nothing
--  Just (x, xs') -> case runParser spaceParser xs' of
--    Nothing -> Nothing
--    Just (_, xs'') -> case runParser posInt xs'' of
--      Nothing -> Nothing
--      Just (y, xs''') -> Just ([x,y], xs'''))

--
-- 2nd attempt
--
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt

-- Ex. 4 - Write an Alternative instance for Parser
--
-- See: http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Applicative.html#t:Alternative
--
-- empty represents the parser which always fails.
-- p1 <|> p2 represents the parser which ﬁrst tries running p1. If p1 succeeds then p2 is
-- ignored and the result of p1 is returned.  Otherwise, if p1 fails, then p2 is tried instead.
--
-- Hint: there is already an Alternative instance for Maybe which you may find useful.

instance Alternative Parser where
    empty = Parser (\xs -> Nothing)
    Parser p1 <|> Parser p2 = Parser (\xs -> case p1 xs of
      j@(Just x) -> j
      Nothing    -> p2 xs)

-- Ex. 5 - Implement a parser:
--
--  intOrUppercase :: Parser ()
-- 
-- which parses either an integer value or an uppercase character, and fails otherwise.

intOrUppercase :: Parser ()
intOrUppercase = toUnitParser posInt <|> toUnitParser (satisfy isUpper)

