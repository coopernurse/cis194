module Party where

import Data.Monoid
import Data.Tree
import Employee

-- ** Exercise 1
--
-- 1.1

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = f1 }) (GL x f2) = GL (e:x) (f1 + f2)

-- 1.2

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

-- 1.3

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 > f2 then gl1 else gl2

-- ** Exercise 2
--
-- 2.1

{-treeFold :: ... -> Tree a -> b-}
