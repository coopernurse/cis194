module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits x = [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = xs

sumDigits :: [Integer] -> Integer
sumDigits xs = 0

validate :: Integer -> Bool
validate x = False

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x p1 p2 p3 = []
