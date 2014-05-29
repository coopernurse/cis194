module Cis194.Hw.Party where

import Data.List 
import Data.Ord 
import Data.Monoid
import Data.Tree
import Cis194.Hw.Employee

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend a b = GL ((glEmps a) ++ (glEmps b)) ((glFun a) + (glFun b))

glEmps :: GuestList -> [Employee]
glEmps (GL emps _) = emps

glFun :: GuestList -> Fun
glFun (GL _ f) = f

-- glCons adds an Employee to the GuestList (updating the cached
-- Fun score appropriately).
glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e : (glEmps gl)) ((empFun e) + (glFun gl))

glConcat :: GuestList -> GuestList -> GuestList
glConcat g1 g2 = foldl (\gl e -> glCons e gl) g1 $ glEmps g2

glCombine :: [GuestList] -> GuestList
glCombine [] = GL [] 0
glCombine (gl:xs) = glConcat gl $ glCombine xs 

-- moreFun takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score. (If the scores are equal it
-- does not matter which is returned.)
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 > f2 then gl1 else gl2

treeFold :: (a -> Tree b -> a) -> a -> Tree b -> a
treeFold fx acc t = fx (foldl (\acc' t' -> treeFold fx acc' t') acc $ subForest t) $ t

-- nextLevel takes two parameters a returns two GuestLists as a tuple
--
-- The first argument is the “boss” of the current sub-tree (let’s call him Bob). 
-- The second argument is a list of the results for each subtree under Bob. 
-- Each result is a pair of GuestLists: the
-- first GuestList in the pair is the best possible guest list with the boss
-- of that subtree; the second is the best possible guest list without the
-- boss of that subtree. nextLevel should then compute the overall best
-- guest list that includes Bob, and the overall best guest list that doesn’t
-- include Bob. 
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (withboss, noboss)
  where withboss = glCons emp $ glCombine $ map snd gls
        noboss   = glCombine $ map (\t -> moreFun (fst t) (snd t)) gls

nextLevelRecur :: Tree Employee -> (GuestList, GuestList)
nextLevelRecur t = nextLevel (rootLabel t) $ map nextLevelRecur (subForest t)

-- maxFun takes a company hierarchy as input and outputs a fun-maximizing guest list.
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst best) (snd best)
  where best = nextLevelRecur t

cmpEmpByName :: Employee -> Employee -> Ordering
cmpEmpByName x y = compare (empName x) (empName y)

glPrint :: GuestList -> IO ()
glPrint gl = do
  putStrLn $ "Total fun: " ++ (show $ glFun gl)
  mapM_ (\e -> putStrLn (empName e)) $ sortBy cmpEmpByName $ glEmps gl

-- main reads company hierarchy from company.txt and prints a formatted guest list
-- sorted by first name
main' :: IO () 
main' = do
  tStr <- readFile "data/company.txt"
  glPrint $ maxFun $ read tStr

