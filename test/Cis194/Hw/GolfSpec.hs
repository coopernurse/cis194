module Cis194.Hw.GolfSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Golf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Hopscotch" $ do
    it "should skip every nth letter" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

    it "should handle single element lists" $ do
      skips [1 :: Integer] `shouldBe` [[1]]

    it "should handle lists of booleans" $ do
      skips [True,False] `shouldBe` [[True,False], [False]]

    it "should handle an empty list" $ do
      -- not sure if we can avoid a type annotation here
      skips ([] :: [Integer]) `shouldBe` [] 
