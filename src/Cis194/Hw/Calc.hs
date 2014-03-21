-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where 

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
import qualified Cis194.Hw.StackVM as VM

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just ex -> Just (eval ex)
  Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax $ max x y 
  mul (MinMax x) (MinMax y) = MinMax $ min x y 

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile s = case parseExp lit add mul s of 
  Just ex -> Just ex 
  Nothing -> Nothing 

