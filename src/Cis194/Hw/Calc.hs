{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
import qualified Cis194.Hw.StackVM as StackVM

-- pass an expression and get back an integer
-- e.g. Lit 4 == 4
--      Add (Lit 1) (Lit 4) == 5
eval :: ExprT -> Integer
eval e1 = case e1 of
  (Lit n) -> n
  (Add e2 e3) -> eval e2 + eval e3
  (Mul e2 e3) -> eval e2 * eval e3

-- pass a string that can be parsed into an expression and
-- maybe get an integer from its evaluation
-- e.g. evalStr "1+5*2" == 12
evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  (Just e) -> Just (eval e)

-- Create a type class called Expr with three methods: lit,
-- mul, and add which parallel the constructors of ExprT
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Make an instance of Expr for the ExprT type in such a
-- way that:
-- (add (lit 4) (lit 6) :: ExprT) == Add (Lit 4) (Lit 6)
instance Expr ExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

-- Make instances of Expr for each of the following types:
--
-- Integer - works like the original
instance Expr Integer where
  lit n = n
  add n1 n2 = n1 + n2
  mul n1 n2 = n1 * n2

-- Bool - every literal value less than or equal to 0 is
--        interpreted as False and all positive Integers
--        are interpreted as True. Addition is logical or
--        and multiplication is logical and

instance Expr Bool where
  lit x = x > 0
  add y z = y || z
  mul y z = y && z

-- MinMax - addition is taken to be the max function and
--          multiplication is the min function
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax y) (MinMax z) = MinMax (max y z)
  mul (MinMax y) (MinMax z) = MinMax (min y z)

-- Mod7 - all values should be in the range 0..6 and all
--        arithmetic is done modulo 7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 y) (Mod7 z) = Mod7 ((y + z) `mod` 7)
  mul (Mod7 y) (Mod7 z) = Mod7 ((y * z) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Your task is to implement a compiler for artihmetic
-- expressions. Simply create an instance of the Expr type
-- for Program so that arithmetic expressions can be
-- interpreted as compiled programs. For any arithmetic
-- expression
--
-- exp :: Expr a => a
--
-- ... it should be the case that ...
--
-- stackVM exp == Right [IVal exp]
--

-- This Stack Overflow discussion explains why this
-- instance declaration blows up if we don't use the
-- FlexibleInstances thingy:
--
-- http://stackoverflow.com/questions/8633470/illegal-instance-declaration-when-declaring-instance-of-isstring
instance Expr StackVM.Program where
  lit x = (StackVM.PushI x) : []
  add e1 e2 = e1 ++ e2 ++ [StackVM.Add]
  mul e1 e2 = e1 ++ e2 ++ [StackVM.Mul]

-- Finally, create a function:
--
-- compile :: String -> Maybe Program
--
-- ...which takes Strings representing arithmetic
-- expressions and compiles them into Programs that can be
-- run on the custom CPU.
--
compile :: String -> Maybe StackVM.Program
compile s = case (parseExp lit add mul s) of
  Nothing -> Nothing
  (Just e) -> Just e
