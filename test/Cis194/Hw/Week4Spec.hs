module Cis194.Hw.Week4Spec (main, spec) where

import Test.Hspec
import Cis194.Hw.Week4
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fun1'" $ do
    it "should output the same results as fun1" $ do
      property $ \x xs -> fun1 (x:xs) == fun1' (x:xs)

  describe "fun2'" $ do
    it "should output the same results as fun2" $ do
      fun2 1 `shouldBe` fun2' 1
      fun2 2 `shouldBe` fun2' 2
      fun2 3 `shouldBe` fun2' 3

  describe "map'" $ do
    it "should behave as map does" $ do
      map (id) [1,2,3] `shouldBe` map' (id) [1,2,3]
      map (`div`3) [1,2,3] `shouldBe` map' (`div`3) [1,2,3]

  describe "xor" $ do
    it "behave as described" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "sieveSundaram" $ do
    it "returns no primes for n < 2" $ do
      sieveSundaram (-1) `shouldBe` []
      sieveSundaram 1 `shouldBe` []

    --it "computes all primes between 2 and 2n+1, inclusive" $ do
    --  sieveSundaram 10 `shouldBe` [2,3,5,7,11,13,17,19]
