module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (*) 1 [ x-2 | x <- xs, even x ]

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldl (+) 0 . filter even . takeWhile (1/=) . iterate collatz
        where collatz n
                        | even n = div n 2
                        | odd n = n * 3 + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree _ = Leaf

xor :: [Bool] -> Bool
xor _ = False

map' :: (a -> b) -> [a] -> [b]
map' _ _ = []

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []
