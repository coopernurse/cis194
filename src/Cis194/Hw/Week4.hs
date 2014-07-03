module Cis194.Hw.Week4 where

import Debug.Trace

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
fun2' n = sum . filter (even) . takeWhile (>1) $ iterate iterator n
  where iterator x
         | even x = x `div` 2
         | otherwise = 3 * x + 1

-- EXERCISE 2
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- (a -> b -> b) -> b -> [a] -> b
foldTree :: [Char] -> Tree Char
foldTree xs = foldr (insert) Leaf xs

insert :: Char -> Tree Char -> Tree Char
insert item Leaf = Node 0 Leaf item Leaf
insert item (Node h l v r) = case (l, r) of
  (Leaf, Leaf)           -> Node (h+1) (Node h Leaf item Leaf) v r
  ((Node _ _ _ _), Leaf) -> Node h l v (Node (h-1) Leaf item Leaf)
  (Leaf, (Node _ _ _ _)) -> Node h (Node (h-1) Leaf item Leaf) v r
  (_)                    -> case compare (height l) (height r) of
    (GT) -> Node h l v (insert item r)
    (LT) -> Node h (insert item l) v r
    (EQ) -> case ((balanced l), (balanced r)) of
      (True, True)  -> Node (h+1) (insert item l) v r
      (True, False) -> Node h l v (insert item r)
      (_)           -> Node h (insert item l) v r

height :: Tree Char -> Integer
height Leaf = -1
height (Node h _ _ _) = h

balanced :: Tree Char -> Bool
balanced Leaf = True
balanced (Node _ Leaf _ Leaf) = True
balanced (Node _ n1@(Node _ _ _ _) _ n2@(Node _ _ _ _)) = balanced n1 && balanced n2
balanced _ = False

-- EXERCISE 3

-- return True only if the list contains an odd number of
-- True
xor :: [Bool] -> Bool
xor [] = False
xor list = (==1) . (`mod`2) $ foldr (fx) 0 list
  where fx item acc
         | item = (acc :: Integer) + 1
         | otherwise = (acc :: Integer)

-- implement map using fold
map' :: (a -> b) -> [a] -> [b]
map' f list = foldr (\item acc -> f item : acc) [] list

-- EXERCISE 4

-- Start with a list of the integers from 1 to n. From
-- this list, remove all numbers of the form i + j + 2
-- where:
--
-- i, j E N, 1 <= i <= j
-- i + j + 2 * i * j <= n
--
-- The remaining numbers are doubled and incremented by
-- one, giving the list of the odd (you gotta add 2
-- yourself) prime numbers between 1 and 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n
  | n < 2 = []
  | otherwise = 2:[2*z+1 | z <- [1..n], (z `notElem` rejects)]
  where rejects = [i+j+2*i*j | i <- [1..n], j <- [i..n], i <= j, i+j+2*i*j <= n]


