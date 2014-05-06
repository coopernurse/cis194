import Data.Monoid

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- ** Exercise 1
-- Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived from
-- those of the two arguments.

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

--  You may find it helpful to implement a helper function
--  which gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Empty) = mempty
tag (Append m jl1 jl2) = tag jl1 `mappend` tag jl2



-- ** Exercise 2
-- 1. Implement the function:
--    indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
--    ...which finds the JoinList element at the specified index

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty)              = Nothing
indexJ 0 (Single m a)         = Just a
indexJ n (Single m a) | n > 0 = Nothing
indexJ n _            | n < 0 = Nothing
indexJ n (Append m jl1 jl2)
  | n < leftSize = indexJ n jl1
  | otherwise = indexJ (n - leftSize) jl2
  where leftSize = getSize . size $ (tag jl1)
