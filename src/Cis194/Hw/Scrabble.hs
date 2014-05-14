{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Cis194.Hw.Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

-- The score function should implement the tile scoring values as
-- shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any
-- characters not mentioned (punctuation, spaces, etc.) should be given
-- zero points.
score :: Char -> Score
score c 
  | uc `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = Score 1
  | uc `elem` ['D', 'G'] = Score 2
  | uc `elem` ['B', 'C', 'M', 'P'] = Score 3
  | uc `elem` ['F', 'H', 'V', 'W', 'Y'] = Score 4
  | uc `elem` ['K'] = Score 5
  | uc `elem` ['J', 'X'] = Score 8
  | uc `elem` ['Q', 'Z'] = Score 10
  | otherwise = Score 0
  where uc = toUpper c

scoreString :: String -> Score
scoreString s = mconcat $ map score s
