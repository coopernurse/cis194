module Cis194.Hw.Calc where

import  Cis194.Hw.ExprT

eval :: ExprT -> Integer
eval e1 = case e1 of
  (Lit n) -> n
  (Add e2 e3) -> eval e2 + eval e3
  (Mul e2 e3) -> eval e2 * eval e3

evalStr :: String -> Maybe Integer
evalStr _ = Nothing
