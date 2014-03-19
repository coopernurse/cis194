module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1  = []
  | x < 10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [(x `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse(toDigits(x))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther x  = zipWith (*) (cycle z) x
  where z = if length x `mod` 2 == 1 then [1, 2] else [2, 1]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10    = x + sumDigits(xs)
  | otherwise = sumDigits(toDigits(x)) + sumDigits(xs)

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x)) `mod` 10) == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x source dest spare = hanoi (x-1) source spare dest ++ [(source, dest)] ++ hanoi (x-1) spare dest source

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 _ _ _ _ _ = []
