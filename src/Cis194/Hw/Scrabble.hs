{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid

-- Create a Scrabble module
-- that defines a Score type, a Monoid instance for Score, and the
-- following functions:
--
-- score :: Char -> Score
-- scoreString :: String -> Score
--

newtype Score = Score Int
                deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c
  | c == 'a' = Score 1
  | c == 'a' = Score 1
  | c == 'b' = Score 3
  | c == 'c' = Score 3
  | c == 'd' = Score 2
  | c == 'e' = Score 1
  | c == 'f' = Score 4
  | c == 'g' = Score 2
  | c == 'h' = Score 4
  | c == 'i' = Score 1
  | c == 'j' = Score 8
  | c == 'k' = Score 5
  | c == 'l' = Score 1
  | c == 'm' = Score 3
  | c == 'n' = Score 1
  | c == 'o' = Score 1
  | c == 'p' = Score 3
  | c == 'q' = Score 10
  | c == 'r' = Score 1
  | c == 's' = Score 1
  | c == 't' = Score 1
  | c == 'u' = Score 1
  | c == 'v' = Score 4
  | c == 'w' = Score 4
  | c == 'x' = Score 8
  | c == 'y' = Score 4
  | otherwise = Score 0

scoreString :: String -> Score
scoreString s = foldl1 (<>) $ map score s
