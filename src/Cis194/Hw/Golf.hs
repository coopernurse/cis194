module Cis194.Hw.Golf where

-- steps:
--
-- 1. create range of numbers from 1 to the length of the
--    original list
--
-- 2. map over that list of numbers, using each one seq-
--    uentially to grab the n-th element in the original
--    list (available via a closure) by using an additional
--    zip + foldr
--
-- this implementation is O(3n) and 83 chars
skips :: [a] -> [[a]]
skips xs = map (\n -> foldr (\(m, y) a -> if m `mod` n == 0 then y : a else a) [] $ zip [1..] xs) [1..length xs]

-- steps:
--
-- 1. start recursion by seeding with n=1
--
-- 2. mod n the index of each item in the original list
--    and if 0, keep it
--
-- 3. cons that list on to the return value of a recursive
--    call to skips' with n decremented by 1
--
-- this implementation is O(2n) and 95 chars
skips':: [a] -> [[a]]
skips' xs = s 1 xs
  where s _ []     = []
        s n (z:zs) = (foldr (\(m, y) a -> if m `mod` n == 0 then y : a else a) [] $ zip [1..] xs) : s (n+1) zs


localMaxima :: [Integer] -> [Integer]
localMaxima _ = []

histogram :: [Integer] -> String
histogram _ = ""
