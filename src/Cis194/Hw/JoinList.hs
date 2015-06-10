{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Cis194.Hw.JoinList where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw

import Data.Monoid
import Cis194.Hw.Sized
import Cis194.Hw.Scrabble
import Cis194.Hw.Buffer

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- ** Exercise 1

-- Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived from
-- those of the two arguments.

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 (Empty) = jl1
(+++) (Empty) jl2 = jl2
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

--  snag the annotation of a JoinList
tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag Empty = mempty
tag (Append m jl1 jl2) = m

-- ** Exercise 2

-- safe access into a list by index
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- transmute a JoinList to a list, dumping the monoidal cache
jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty            = mempty
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 <> jlToList l2

-- 1. Implement the function:
--    indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
--    ...which finds the JoinList element at the specified index

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Append m jl1 jl2)
  | n < leftSize = indexJ n jl1
  | otherwise = indexJ (n - leftSize) jl2
  where leftSize = getSize . size $ (tag jl1)
indexJ 0 (Single m a) = Just a
indexJ _ _            = Nothing

-- 2. Implement the function:
-- dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- ...which drops the first n elements from a JoinList.

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append m jl1 jl2)
  | n < leftSize = dropJ n jl1 +++ jl2
  | otherwise = dropJ (n - leftSize) jl2
  where leftSize = getSize . size $ tag jl1
dropJ n jl | n < 1 = jl
dropJ _ _          = Empty

-- 3. Implement the function:
-- takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
-- ...which returns the first n elements from a JoinList.

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append m jl1 jl2)
  | n > leftSize = jl1 +++ (takeJ (n - leftSize) jl2)
  | otherwise = takeJ n jl1
  where leftSize = getSize . size $ tag jl1
takeJ n derp@(Single m a) | n > 0 = derp
takeJ _ _            = Empty


-- ** Exercise 3
--
-- Write the following function:
-- scoreLine :: String -> JoinList Score String

scoreLine :: String -> JoinList Score String
scoreLine s@(x:_) = Single (scoreString s) s
scoreLine _       = Empty


-- ** Exercise 4
--
-- Since we want to track both the size and score of a buffer, you should
-- provide a Buffer instance for the type:
-- JoinList (Score, Size) String
--

-- I stole this from Alex - makes moving around in the editor much faster
balanceJ :: (Sized m, Monoid m) => JoinList m a -> JoinList m a
balanceJ jl@(Append _ _ _) = (balanceJ $ takeJ split jl) +++ (balanceJ $ dropJ split jl)
  where split = (`div` 2) . getSize . size $ tag jl
balanceJ jl = jl

instance Buffer (JoinList (Score, Size) String) where
  fromString s               = balanceJ $ foldr (+++) Empty $ map (\y -> Single (scoreString y, Size 1) y) $ lines s
  line n jl                  = indexJ n jl
  numLines                   = getSize . size . tag
  replaceLine n s jl         = takeJ (n-1) jl +++ fromString s +++ dropJ n jl
  toString                   = unwords . jlToList
  value (Empty)              = 0
  value (Single (sc, _) _)   = getScore sc
  value (Append (sc, _) _ _) = getScore sc
