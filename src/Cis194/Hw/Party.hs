module Cis194.Hw.Party where

import Employee
import Data.Monoid
import Data.Tree

-- ** Utility function

startGuestList :: String -> Integer -> GuestList
startGuestList s i
  | i > 0     = GL [Emp { empFun = i, empName = s}] i
  | otherwise = GL [] 0

-- ** Exercise 1
--
-- 1.1
--
-- add a function:
--
-- glCons :: Employee -> GuestList -> GuestList
--
-- which adds an Employee to the GuestList (updating the cached Fun score
-- appropriately). Of course, in general this is impossible: the updated
-- fun score should depend on whether the Employee being added is already
-- in the list, or if any of their direct subordinates are in the list,
-- and so on. For our purposes, though, you may assume that none of these
-- special cases will hold: that is, glCons should simply add the new
-- Employee and add their fun score without doing any kind of checks.

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = x }) (GL l f) = GL (e:l) (f+x)

-- 1.2
--
-- Add a Monoid instance for GuestList (How is the Monoid instance
-- supposed to work, you ask? You figure it out!)

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend gl1@(GL x y) gl2@(GL a b) = GL (x ++ a) (y + b)

-- 1.3
--
-- Create a function:
--
-- moreFun :: GuestList -> GuestList -> GuestList
--
-- which takes two GuestLists and returns whichever one of them is more
-- fun, i.e. has the higher fun score. (If the scores are equal it does
-- not matter which is returned.)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2   = gl1
  | otherwise = gl2

-- ** Exercise 2
--
-- 2.1
--
-- The Data.Tree module from the standard Haskell libraries defines
-- the type of “rose trees”, where each node stores a data element
-- and has any number of children (i.e. a list of subtrees):
--
-- data Tree a = Node {
--   rootLabel :: a,         -- label value
--   subForest :: [Tree a]   -- zero or more child trees
-- }
--
-- Strangely, Data.Tree does not define a fold for this type! Rectify
-- the situation by implementing:
--
-- treeFold :: ... -> Tree a -> b
--
-- See if you can figure out what type(s) should replace the dots in
-- the type of treeFold. If you are stuck, look back at the lecture
-- notes from Week 7, or infer the proper type(s) from the remainder
-- of this assignment.)


-- ** Exercise 3
--
-- Write a function
--
-- nextLevel :: Employee -> [(GuestList, GuestList)]
--                       -> (GuestList, GuestList)
--
-- which takes two arguments. The first is the “boss” of the current
-- subtree (let’s call him Bob). The second argument is a list of the
-- results for each subtree under Bob. Each result is a pair of
-- GuestLists: the first GuestList in the pair is the best possible
-- guest list with the boss of that subtree; the second is the best
-- possible guest list without the boss of that subtree. nextLevel
-- should then compute the overall best guest list that includes Bob,
-- and the overall best guest list that doesn’t include Bob.

-- ** Exercise 4
--
-- Finally, put all of this together to define
--
-- maxFun :: Tree Employee -> GuestList
--
-- which takes a company hierarchy as input and outputs a fun-maximizing
-- guest list. You can test your function on testCompany, provided in
-- Employee.hs.

-- ** Exercise 5
--
-- Implement main :: IO () so that it reads your company’s hierarchy
-- from the file company.txt, and then prints out a formatted guest
-- list, sorted by first name, which looks like:
--
--    Total fun: 23924
--    Adam Debergues
--    Adeline Anselme
