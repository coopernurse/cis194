{-# LANGUAGE FlexibleInstances #-}
module Cis194.Hw.Calc where

import Cis194.Hw.ExprT
import Cis194.Hw.Parser
import qualified Data.Map as M
import Control.Applicative

-- 1
-- write something that evaluates nested
-- ExprT down to an integer
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- 2
-- write something that takes a string
-- and attempts to evaluate it down to
-- an integer
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

-- 3
-- write an ExprT builder using a new Expr
-- type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- for the lulz
instance Expr String where
  lit = show
  add = (++)
  mul s1 s2 = foldr (\(x, y) acc -> x : y : acc) "" $ zip s1 s2

-- 4
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add (Mod7 i1) (Mod7 i2) = lit $ i1 + i2
  mul (Mod7 i1) (Mod7 i2) = lit $ i1 * i2

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var s = VVar s

type MapToMaybeFn = (M.Map String Integer -> Maybe Integer)

instance HasVars MapToMaybeFn where
  var s = \m -> M.lookup s m

instance Expr MapToMaybeFn where
  lit n = \m -> Just n
  add f1 f2 = \m -> (+) <$> f1 m <*> f2 m
  mul f1 f2 = \m -> (*) <$> f1 m <*> f2 m

withVars :: [(String, Integer)] -> MapToMaybeFn -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
