module Cis194.Hw.JoinList where

import Data.Monoid
import Cis194.Hw.Sized
import Cis194.Hw.Scrabble

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (mappend (tag l) (tag r)) l r

tag :: Monoid m => JoinList m a -> m
tag (Empty) = mempty 
tag (Single m _) = m
tag (Append m l r) = mappend (tag l) (tag r)

--jlVal :: Monoid m => JoinList m a -> Maybe a 
--jlVal (Empty) = Nothing 
--jlVal (Single _ a) = Just a 
--jlVal (Append m l r) = mappend (jlVal l) (jlVal r)

--
-- why is (Sized b, Monoid b) abstracted here?
-- if the 2nd arg is a JoinList, and JoinLists are basically trees,
-- and the goal is to index into the tree, how are there multiple
-- implementations of Sized+Monoid that provide the required semantics?
--
-- That is, why couldn't we implement this for any JoinList b a where
-- b is unconstrained?
--
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ x jl@(Single b a) = if x == (getSize $ size (tag jl)) then Just a else Nothing
indexJ x (Append m l r) = case indexJ x l of
  Nothing -> indexJ x r
  Just a -> Just a
indexJ _ _ = Nothing

-- indexJ ﬁnds the JoinList element at the speciﬁed index. If the
-- index is out of bounds, the function returns Nothing. By an index
-- in a JoinList we mean the index in the list that it represents. That
-- is, consider a safe list indexing function
--
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)
-- 
-- which returns Just the ith element in a list (starting at zero) if
-- such an element exists, or Nothing otherwise. We also consider
-- an updated function for converting join-lists into lists, just like
-- jlbToList but ignoring the monoidal annotations: 
--   
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
-- 
-- Note: you do not have to include (!!?)
-- and jlToList in your assignment; they
-- are just to help explain how indexJ
-- ought to behave. However, you may
-- certainly use them to help test your
-- implementations if you wish.
-- 
-- We can now specify the desired behavior of indexJ. For any index
-- i and join-list jl, it should be the case that
--
-- (indexJ i jl) == (jlToList jl !!? i)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ x jl@(Single b a) = if x > (getSize $ size b) then jl else Empty
dropJ x (Append m l r) = case l' of
  Empty -> r'
  _ -> case r' of
    Empty -> l'
    _ -> Append m l' r'
  where l' = dropJ x l
        r' = dropJ x r
dropJ _ _ = Empty 

-- The dropJ function drops the ﬁrst n elements from a JoinList.
-- This is analogous to the standard drop function on lists. Formally,
-- dropJ should behave in such a way that
-- 
-- jlToList (dropJ n jl) == drop n (jlToList jl).
-- 
-- Finally, implement the function
 
--takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

-- The takeJ function returns the ﬁrst n elements of a JoinList,
-- dropping all other elements. Again, this function works similarly
-- to the standard library take function; that is, it should be the case that
--
-- jlToList (takeJ n jl) == take n (jlToList jl).
--
-- Ensure that your function deﬁnitions use the size function from
-- the Sized type class to make smart decisions about how to descend
-- into the JoinList tree.


-- *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
-- Append (Score 23)
--   (Single (Score 9) "yay ")
--   (Single (Score 14) "haskell!")

--scoreLine :: String -> JoinList (String -> Score) String
--scoreLine s = mconcat $ map (Single 10) $ words s

