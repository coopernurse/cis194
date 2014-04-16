module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  Just expr -> Just (eval expr)
