module Cis194.Hw.PartySpec (main, spec) where

import Test.Hspec
import Data.Tree
import Cis194.Hw.Party
import Cis194.Hw.Employee

main :: IO ()
main = hspec spec

bigboss = Emp { empName="big boss", empFun=3 }
emp1 = Emp { empName="sam", empFun=1 }
emp2 = Emp { empName="mary", empFun=6 }
emp3 = Emp { empName="bob", empFun=2 }
emp4 = Emp { empName="jane", empFun=5 }

t1  = Node {
  rootLabel=bigboss,
  subForest=[ (makeNode emp1), (makeNode emp2) ]
}

t2 = Node {
  rootLabel=bigboss,
  subForest=[ 
    (makeNode emp1), 
    Node {
      rootLabel=emp2,
      subForest=[ (makeNode emp3), (makeNode emp4) ]
    }
  ]
}

makeNode :: a -> Tree a
makeNode a = Node{rootLabel=a,subForest=[]}

spec :: Spec
spec = do
  describe "Party - treeFold" $ do
    it "should return a single element if there are no subForest elements" $ do
      treeFold (\acc t -> (rootLabel t) : acc) [] (makeNode "bob") `shouldBe` ["bob"]
    it "should recurse subForest elements" $ do
      treeFold (\acc t -> (rootLabel t) : acc) [] (Node{rootLabel="bob",subForest=[makeNode "max", makeNode "sam"]}) `shouldBe` ["bob", "sam", "max"]
    it "should recurse subForest elements recursively" $ do
      treeFold (\acc t -> (rootLabel t) : acc) [] (Node{rootLabel="bob",subForest=[Node{rootLabel="jane",subForest=[makeNode "ed"]}, makeNode "sam"]}) `shouldBe` ["bob", "sam", "jane", "ed"]

  describe "Party - glCons" $ do
    it "should append an employee to an empty GuestList" $ do
      glCons Emp{empName="bob", empFun=3} (GL [] 0) `shouldBe` GL [Emp{empName="bob", empFun=3}] 3

    it "should append an employee to a non-empty GuestList" $ do
      glCons Emp{empName="sam", empFun=13} (GL [Emp{empName="bob", empFun=4}] 4) 
        `shouldBe` GL [Emp{empName="sam", empFun=13}, Emp{empName="bob", empFun=4}] 17

  --describe "Party - moreFun" $ do
  --  it "returns the GuestList with the higher Fun value" $ do
  --    moreFun a b `shouldBe` a
  --    moreFun b a `shouldBe` a
  --    moreFun a c `shouldBe` a
  --    moreFun b c `shouldBe` b
  --    moreFun c b `shouldBe` b1
  --    where
  --      a = GL [Emp{empName="sam", empFun=13}, Emp{empName="bob", empFun=4}] 17
  --      b = GL [Emp{empName="sam", empFun=13}] 13
  --      c = GL [] 0

  describe "Party - nextLevel" $ do
    it "Returns boss if the guest list is empty" $ do
      nextLevel bigboss [ ] `shouldBe` ((GL [bigboss] 3), (GL [] 0))

    it "Returns the boss as the first tuple element" $ do
      nextLevel bigboss [ ((GL [emp1] 1), (GL [emp3] 2)), 
                          ((GL [emp2] 6), (GL [emp4] 5)) ] `shouldBe` ((GL [bigboss,emp4,emp3] 10), (GL [emp2,emp3] 8))
      nextLevel bigboss [ ((GL [emp1] 1), (GL [] 0)), 
                          ((GL [emp2] 6), (GL [] 0)) ] `shouldBe` ((GL [bigboss] 3), (GL [emp2,emp1] 7))

  describe "Party - maxFun" $ do
    it "returns a guest list with the boss if she has no reports" $ do
      maxFun (makeNode bigboss) `shouldBe` (GL [bigboss] 3)
    it "returns a guest list without the boss if her reports will have more fan without her" $ do
      maxFun t1 `shouldBe` (GL [emp2,emp1] 7)
    it "returns a guest list with the boss if her reports' reports will have more fan without them" $ do
      maxFun t2 `shouldBe` (GL [bigboss,emp3,emp4] 10)

