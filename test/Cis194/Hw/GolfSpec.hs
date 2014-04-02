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

  describe "Local Maxima" $ do
    it "should handle an empty list" $ do
      localMaxima ([] :: [Integer]) `shouldBe` []

    it "should return an empty list if given a list of length 2 or 1" $ do
      localMaxima [1, 2] `shouldBe` []
      localMaxima [2] `shouldBe` []

    it "should return an empty list if no maxima exists" $ do
      localMaxima [1, 2, 3] `shouldBe` []

    it "should return a maxima" $ do
      localMaxima [1, 4, 2] `shouldBe` [4]

    it "should handle multiple maximas" $ do
      localMaxima [1, 4, 2, 9, 3] `shouldBe` [4, 9]

  describe "Histogram" $ do
    it "should handle an empty list" $ do
      histogram [] `shouldBe` "==========\n0123456789\n"

    it "should render asterisks representing the freq of each integer" $ do
      histogram [1, 2, 3] `shouldBe` " ***      \n==========\n0123456789\n"
      histogram [1, 2, 1] `shouldBe` " *        \n **       \n==========\n0123456789\n"

