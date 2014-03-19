module Cis194.Hw.Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate next 
  where next x = if even x then x `div` 2 else 3 * x + 1

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

nodeHeight :: Tree a -> Integer
nodeHeight Leaf = -1
nodeHeight (Node _ l _ r) = maximum [(nodeHeight l), (nodeHeight r)] + 1

nodeCount :: Tree a -> Integer
nodeCount Leaf = 0
nodeCount (Node _ l _ r) = (nodeCount l) + (nodeCount r) + 1

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node _ l n r) = newMe
  where leftH  = nodeCount l 
        rightH = nodeCount r 
        child  = if leftH <= rightH then (insertTree x l) 
                 else (insertTree x r)
        newH   = (nodeHeight child) + 1
        newMe  = if leftH <= rightH then Node newH child n r
                 else Node newH l n child

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs 

sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []
