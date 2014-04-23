module Cis194.Hw.Fibonacci where

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
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
-- fibs2 :: [Integer]
--
-- so that it has the same elements as fibs1, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--

-- Since Haskell is lazy, we will only compute items in the fibs2 list
-- when we need them. Computing the third item in the list equates to
-- cons'ing 0 to 1 to the result of a list comprehension. The list
-- comprehension creates tuples out of each item in the CURRENT fibs list
-- list with the item immediately before it, e.g.:
--
-- let x = [1, 2, 3]
-- zip x (tail x) == [(1,2), (2, 3)]
--
-- The sum of the two items in the tuple gets us our fib sequence.
fibs2 :: [Integer]
fibs2 = 0 : 1 : [a + b | (a, b) <- zip fibs2 (tail fibs2)]

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.

data Stream a = Cons a (Stream a)

-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- * Make your own instance of Show for Stream:
--
--   instance Show a => Show (Stream a) where
--     show ...
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

instance Show a => Show (Stream a) where
  show s = unwords . take 20 $ map show $ streamToList s

----------
-- Ex 4 --
----------

-- * Write a function:
--
-- streamRepeat :: a -> Stream a
--
-- ...which generates a stream containing infinitely many copies of the
-- given element

streamRepeat :: a -> Stream a
streamRepeat e = Cons e $ streamRepeat e

-- * Write a function:
--
-- streamMap :: (a -> b) -> Stream a -> Stream b
--
-- ...which applies a function to every element of a Stream

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

-- * Write a function:
--
-- streamFromSeed :: (a -> a) -> a -> Stream a
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f $ f seed

----------
-- Ex 5 --
----------

-- * Define the stream:
--
-- nats :: Stream Integer
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- * Define the stream:
--
-- ruler :: Stream Integer
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

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs
