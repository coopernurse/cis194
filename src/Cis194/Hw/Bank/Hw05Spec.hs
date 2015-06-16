module Hw05Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Hw05 (getFlow, upsert, getCriminal)
import Data.Map.Strict as M

import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getFlow" $ do
    let ts1 = [Transaction { from = "Una Nobles", 
                             to = "Leia Klinger", 
                             amount = 100, 
                             tid = "x"}]

    let ts2 = ts1 ++ [Transaction { from = "Una Nobles", 
                                 to = "Derp", 
                                 amount = 50, 
                                 tid = "x"}]

    it "correctly determines who is owed" $ do
      (M.toList $ getFlow ts1) `shouldBe` [("Leia Klinger",100),("Una Nobles",-100)]

    it "combines transactions, summing to the person" $ do
      (M.toList $ getFlow ts2) `shouldBe` [("Derp",50),("Leia Klinger",100),("Una Nobles",-150)]

  describe "upsert" $ do
    let m1 = M.empty
    let m2 = M.fromList [("baz", 10), ("ding", 20)]

    it "creates a new key if one didn't exist" $ do
      M.toList (upsert (+) "foo" 10 m1) `shouldBe` [("foo",10)]

    it "updates key's value if one does" $ do
      M.toList (upsert (+) "baz" 10 m2) `shouldBe` [("baz", 20), ("ding", 20)]
      M.toList (upsert (/) "baz" 10 m2) `shouldBe` [("baz", 1.0), ("ding", 20)]

  describe "getCriminal" $ do
    let m1 = M.empty
    let m2 = M.fromList [("baz", 10), ("ding", 20), ("qux", -25)]

    it "returns the key in a map containing person/value pairs with the highest value" $ do
      getCriminal m1 `shouldBe` "Nobody"
      getCriminal m2 `shouldBe` "ding"
