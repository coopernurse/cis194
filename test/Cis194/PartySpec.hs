module Cis194.Hw.PartySpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Party
import Employee

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "glCons" $ do
    it "should return a new GuestList with Employee added and new fun-score" $ do
      let bob = Emp { empFun = 10, empName = "Bobbo" }
      let sue = Emp { empFun = 20, empName = "Suzie" }
      let joe = Emp { empFun = 40, empName = "Joe" }

      let empty = GL [] 0
      let notEmpty = GL [bob, sue] (empFun bob + empFun sue)

      glCons bob empty `shouldBe` GL [bob] 10
      glCons joe notEmpty `shouldBe` GL [joe, bob, sue] 70
