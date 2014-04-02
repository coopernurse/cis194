module Cis194.Hw.Golf where

import Data.Maybe

-- generate all the skips
-- using map and partial function application
skips :: [a] -> [[a]]
skips []  = []
skips x@(_:_) = map (skip x) [0..length x - 1]

-- skip every n items in a list
-- using drop, recursively
skip :: [a] -> Int -> [a]
skip l n = case drop n l of
  []   -> []
  x:xs -> x : skip xs n

-- find local maxima in a list
-- by chunking into sub-lists of length 3
-- and then comparing items in sub-lists
localMaxima :: [Integer] -> [Integer]
localMaxima xs = mapMaybe match (chunk 3 xs)
  where match (x:y:z:_) = if y > x && y > z then Just y else Nothing
        match _ = Nothing

-- chunk a list into sub-lists of length n
-- e.g. chunk 2 [1, 2, 3] -> [[1, 2], [2, 3]]
chunk :: Int -> [a] -> [[a]]
chunk n (x:l@(y:z:_)) = [x, y, z] : chunk n l
chunk _ _ = []

histogram :: [Integer] -> String
histogram _ = ""
