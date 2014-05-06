import Data.Monoid

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- ** Exercise 1
-- Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived from
-- those of the two arguments.

{-(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a-}

--  You may find it helpful to implement a helper function
--  which gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Empty) = mempty
tag (Append m jl1 jl2) = tag jl1 `mappend` tag jl2
