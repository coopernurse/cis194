module Cis194.Hw.Fibonacci where

import Data.List 

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------

-- Define the infinite list:
--
fibs2 :: [Integer]
--
-- I cheated b/c I couldn't figure it out
-- http://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

--
-- so that it has the same elements as fibs, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--


----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.
-- * Write a function to convert a Stream to an infinite list:
--
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

--
-- * Make your own instance of Show for Stream:
--
instance Show a => Show (Stream a) where
  show s = unwords $ map show $ take 20 $ streamToList s

--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

----------
-- Ex 4 --
----------

-- * Write a function:
--
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

--
-- ...which generates a stream containing infinitely many copies of the
-- given element
--
-- * Write a function:
--
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fx sa = smap list
  where list = streamToList sa
        smap (x:xs) = Stream (fx x) (smap xs)

--
-- ...which applies a function to every element of a Stream
--
-- * Write a function:
--
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fx a = Stream v (streamFromSeed fx v)
  where v = fx a

--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

----------
-- Ex 5 --
----------

-- * Define the stream:
--
nats :: Stream Integer
nats = next [0..]
  where next (x:xs) = Stream x (next xs)

--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--
-- * Define the stream:
--
--ruler :: Stream Integer

pow2 :: Stream Integer
pow2 = streamFromSeed (*2) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a sa) sb = Stream a $ interleaveStreams sb sa

--
-- ...which corresponds to the ruler function:
--
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
--
-- ...where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Hint: define a function interleaveStreams which alternates the
-- elements from two streams. Can you use this function to implement ruler
-- in a clever way that does not have to do any divisibility testing?
