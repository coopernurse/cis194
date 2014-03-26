module Cis194.Hw.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits n = reverse . toDigitsRev $ n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOtherWithParity :: [Integer] -> (Bool, [Integer])
doubleEveryOtherWithParity [] = (False, [])
doubleEveryOtherWithParity (x:xs) = case (doubleEveryOtherWithParity xs) of
  (True, ys) -> (False, (2*x) : ys)
  (False, ys) -> (True, x : ys)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = snd (doubleEveryOtherWithParity xs)

sumDigits :: [Integer] -> Integer
sumDigits = (foldr (+) 0) . (>>= toDigitsRev)

validate :: Integer -> Bool
validate n = 0 == ((sumDigits (doubleEveryOther (toDigits n) >>= toDigitsRev)) `mod` 10)

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = (hanoi4 (n-threePeg) a d b c) ++ (hanoi threePeg a b c) ++ (hanoi4 (n-threePeg) d b a c)
  where threePeg = optimalThreePeg n

quadratic :: (Floating a) => a -> a -> a -> a
quadratic a b c = ((-b) + sqrt(b*b - 4*a*c)) / (2*a)

optimalThreePeg :: (Integral a) => a -> a
optimalThreePeg = floor . (quadratic 1 1) . fromIntegral . negate . (*2)
