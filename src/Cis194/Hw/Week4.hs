module Cis194.Hw.Week4 where

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

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree _ = Leaf

xor :: [Bool] -> Bool
xor _ = False

map' :: (a -> b) -> [a] -> [b]
map' f list = foldr (\x l -> f x : l) [] list

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []
