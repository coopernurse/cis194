module Cis194.Hw.Golf where

import Data.Maybe
import Data.List

-- generate all the skips
-- using map and partial function application
skips :: [a] -> [[a]]
skips []  = []
skips x@(_:_) = map (skip x) [0..length x - 1]
  where skip l n = case drop n l of
                     []   -> []
                     x:xs -> x : skip xs n

-- find local maxima in a list
localMaxima :: [Integer] -> [Integer]
localMaxima l = mapMaybe id $ comp l
  where
    comp (x:r@(y:z:_)) = (if y > x && y > z then Just y else Nothing) : comp r
    comp _ = []

histogram :: [Integer] -> String
histogram = (++"==========\n0123456789\n") . toRows . digitFreqs
  where
    dot n = if n > 0 then '*' else ' '
    toRows list = case (find (>0) list) of
      Nothing -> ""
      Just _  -> toRows (map (+(-1)) list) ++ (map (dot) list ++ "\n")

-- computes frequency for each digit given a list
-- using sentinel value + recursive 'span' to
-- compute frequency of each digit in list
digitFreqs :: [Integer] -> [Int]
digitFreqs l1 = digitFreqs' 0 $ sort l1
  where digitFreqs' n l2
          | n == 10 = []
          | otherwise = case (span (==n) l2) of
          (left, right) -> length left : digitFreqs' (n+1) right
