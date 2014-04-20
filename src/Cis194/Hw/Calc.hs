module Cis194.Hw.Calc where

import Cis194.Hw.ExprT
import Cis194.Hw.Parser

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

{-Think carefully about what types lit, add, and mul should have. -}
{-It may be helpful to consider the types of the ExprT constructors, -}
{-which you can find out by typing (for example)-}
