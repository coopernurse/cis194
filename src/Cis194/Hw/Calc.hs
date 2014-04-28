{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
import qualified Cis194.Hw.StackVM as Svm
import qualified Data.Map as M

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVar a where
  var :: String -> a

instance Expr ExprT where
  lit x = Lit x
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

instance Expr VarExprT where
  lit x = VLit x
  add e1 e2 = VAdd e1 e2
  mul e1 e2 = VMul e1 e2

instance HasVar VarExprT where
  var s = Var s

instance Expr Integer where
  lit x = x
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit x = x >= 0
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

instance HasVar (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add e1 e2 = \m -> do
    x <- e1 m
    y <- e2 m
    Just (x+y)
  mul e1 e2 = \m -> do
    x <- e1 m
    y <- e2 m
    Just (x*y)

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax e1) (MinMax e2) = MinMax $ max e1 e2
  mul (MinMax e1) (MinMax e2) = MinMax $ min e1 e2

newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 e1) (Mod7 e2) = Mod7 $ (e1 + e2) `mod` 7
  mul (Mod7 e1) (Mod7 e2) = Mod7 $ (e1 * e2) `mod` 7

instance Expr Svm.Program where
  lit x = [Svm.PushI x]
  add e1 e2 = e1 ++ e2 ++ [Svm.Add]
  mul e1 e2 = e1 ++ e2 ++ [Svm.Mul]

compile :: String -> Maybe Svm.Program
compile s = parseExp lit add mul s

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  Just expr -> Just (eval expr)
