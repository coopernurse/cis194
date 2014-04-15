module Cis194.Hw.Week4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = (*2) . sum . (takeWhile (>0)) . (drop 1) . iterate ((`div`2) . (\x -> if x < 2 || even x then x else 3*x + 1))

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr addNode Leaf
  where addNode x Leaf = Node 0 Leaf x Leaf
        addNode x (Node depth left y right)
          | x > y = rebalance (Node depth left y (addNode x right))
          | otherwise = rebalance (Node depth (addNode x left) y right)
        rebalance Leaf = Leaf
        rebalance (Node _ Leaf x Leaf) = Node 0 Leaf x Leaf
        rebalance (Node _ lt@(Node ld ltl lv ltr) x Leaf)
          | ld > 0 = Node 1 ltl lv (Node 0 ltr x Leaf)
          | otherwise = Node 1 lt x Leaf
        rebalance (Node _ Leaf x rt@(Node rd rtl rv rtr))
          | rd > 0 = Node 1 (Node 0 Leaf x rtl) rv rtr
          | otherwise = Node 1 Leaf x rt
        rebalance (Node d lt@(Node ld ltl lv ltr) x rt@(Node rd rtl rv rtr))
          | ld > rd + 1 = Node (rd + 2) ltl lv (Node (rd+1) ltr x rt)
          | rd > ld + 1 = Node (ld + 2) (Node (ld+1) lt x rtl) rv rtr
          | otherwise = Node ((max ld rd) + 1) lt x rt

xor :: [Bool] -> Bool
xor = foldr (\a b -> (a && not b) || (not a && b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f(x):l) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (2:) . (map ((+1).(*2))) . fst . (foldr sieve ([],witnesses)) $ [1..n]
  where witnesses = (map head) . group . (sortBy (flip compare)) $  [i + j + 2*i*j | i <- [1..n], j <- [1..n], i <= j, i+j+2*i*j <= n]
        sieve x (primes,[]) = (x:primes,[])
        sieve x (primes,w:ws)
          | x > w = (x:primes, w:ws)
          | otherwise = (primes,ws)
