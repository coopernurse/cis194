module Cis194.Hw.Golf where

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

localMaxima :: [Integer] -> [Integer]
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""
