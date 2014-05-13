module Cis194.Hw.Party where

import Data.Monoid
import Data.Tree
import Data.List
import Employee
import System.Environment

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employee:employees) (fun + empFun employee)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend gl1 (GL emps _) = foldr glCons gl1 emps

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
  | fun1 > fun2 = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node value subForest) = f value $ map (treeFold f) subForest

{-
The first item in each pair is the best guest list with the boss and the second is without
The first item in the result will be the boss : the concatenation of all the second items
The second item in the result will be the concatenation of the greater of the first and second
-}
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss divisionBests = (
  glCons boss $ mconcat . (map snd) $ divisionBests, 
  mconcat $ map (uncurry moreFun) divisionBests )

maxFun :: Tree Employee -> GuestList
maxFun company = uncurry moreFun $ treeFold nextLevel company

sortedEmployeeList :: [Employee] -> String
sortedEmployeeList list = foldr ((++) . (++"\n")) "" $ sort $ map empName list

outputGuestList :: GuestList -> String
outputGuestList (GL list fun) = "Total Fun: " ++ (show fun) ++ "\n" ++ (sortedEmployeeList list)

main :: IO()
main = getArgs >>= readFile . head >>= putStr . outputGuestList . maxFun . read

