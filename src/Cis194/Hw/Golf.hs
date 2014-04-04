module Cis194.Hw.Golf where

{- 
  Map a range of ints from 0 to the length of the array -1
  into a list of every n elements of the given list by creating
  a range of every n elements and using (v !!) to map those indices
  into elements of the original list.
 -}
skips :: [a] -> [[a]]
skips v = map a [0..l]
      where l = length v - 1
            a n = map (v !!) [n,2*n+1..l] 

{- 
  Aliases localMaxima to l to save characters.
  If the list has 3 elements, if the middle elment is greater the others, 
  cons it on to the localMaxima of the rest of the list. Otherwise just return
  the maxima of the rest of the list.
 -}
localMaxima :: [Integer] -> [Integer]
localMaxima = l
l (x:r@(y:z:s))
  | y>x && y>z = y : l r
  | 1>0        = l r
l _ = []

{- 
  Maps a descending range starting with the highest count of any number in v into
  a string representing whether the count for each digit is less than that number,
  and then joins the strings and adds the footer.
  c: given n, filter out all numbers not equal to n from v
  h: use c to find the count of all digits in v
  m: find the highest value in h
  g: returns a star only if the count of the given number is >= n
  a: given a row number, create of string with stars for all digits whose count 
        is >= that number
 -}
histogram :: [Integer] -> String
histogram v = (map a [m,m-1..1] >>= id) ++ "==========\n0123456789\n"
  where c n = filter (==n) v
        h   = map (length . c) [0..9]
        m   = foldr max 0 h
        g n x 
          | x < n = ' ' 
          | 1>0 = '*'
        a r = (map (g r) h) ++ "\n"
