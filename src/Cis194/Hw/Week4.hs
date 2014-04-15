module Cis194.Hw.Week4 where

-- EXERCISE 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- the odd values are a no-op, so, first filter them out.
-- then, subtract two from each value in the remaining
-- list and then compute their product (same as foldl (*))
fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' n = product $ map (+(-2)) $ filter (even) n

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- repeatedly iterate over the odd/even function, exiting
-- when we have a value of 1 or less. then, sum all the
-- values in that list that are even
fun2' :: Integer -> Integer
fun2' n = sum . filter (even) . takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n

-- EXERCISE 2
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree list = Leaf

-- EXERCISE 3

-- return True only if the list contains an odd number of
-- True
xor :: [Bool] -> Bool
xor [] = False
xor list = (==1) . (`mod`2) $ foldr (\item acc -> if item then acc + 1 else acc) 0 list

-- implement map using fold
map' :: (a -> b) -> [a] -> [b]
map' f list = foldr (\x l -> f x : l) [] list

-- EXERCISE 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [(i, j) | m <- [1..n], i <- [0..n], j <- [1..i], (i + j + 2 * i * j) > n]
