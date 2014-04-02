module Cis194.Hw.Golf where

import Data.Maybe
import Data.List

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
histogram = (++"==========\n0123456789\n") . toRows . digitFreqs

-- computes frequency for each digit given a list
-- using sentinel value + recursive 'span' to
-- compute frequency of each digit in list
digitFreqs :: [Integer] -> [Int]
digitFreqs l1 = digitFreqs' 0 $ sort l1
  where digitFreqs' n l2
          | n == 10 = []
          | otherwise = case (span (==n) l2) of
          (left, right) -> length left : digitFreqs' (n+1) right

-- convert a list of frequencies to a String
-- representing our rows
-- e.g. [0,1,0,0,0,0,0,0,0,0] -> " *        \n"
-- by recursively mapping our row to the mark function
-- and short-circuiting when we have no non-zero freqs
toRows :: [Int] -> String
toRows list = case (find (>0) list) of
  Nothing -> ""
  Just _  -> toRows (map (+(-1)) list) ++ (map (mark) list ++ "\n")
  where mark n = if n > 0 then '*' else ' '
