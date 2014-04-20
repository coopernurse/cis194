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

-- pass a string that can be parsed into an
-- expression and maybe get an integer from its
-- evaluation
-- e.g. evalStr "1+5*2" == 12
evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  (Just e) -> Just (eval e)
