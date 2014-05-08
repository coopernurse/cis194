module Cis194.Hw.Party where

import Employee
import Data.Monoid

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
glCons e@(Emp { empFun = x }) (GL [] _) = GL [e] x

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
