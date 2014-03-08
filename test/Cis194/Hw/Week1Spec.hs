module Cis194.Hw.Week1Spec (main, spec) where

import Test.Hspec
import Cis194.Hw.Week1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should split digits of integer into a list" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "should return an empty list for zero" $ do
    	toDigits 0 `shouldBe` []

